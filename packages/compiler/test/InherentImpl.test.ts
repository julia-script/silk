import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (text: string, target?: string) =>
  Analysis.makeRealized({
    root: SourceFile.make('root', ascii(text)),
    ...(target === undefined ? {} : { target }),
  }).pipe(Effect.provide(SourceResolver.memory(new Map())))

const analyzeModules = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): Effect.Effect<Analysis.Snapshot> => {
  const rootText = entries.find(([name]) => name === rootModule)?.[1]
  if (rootText === undefined) throw new RangeError(`Fixture has no root source ${rootModule}`)
  const imports = new Map(
    entries
      .filter(([name]) => name !== rootModule)
      .map(([name, text]) => [name, ascii(text)] as const),
  )
  return Analysis.makeRealized({ root: SourceFile.make(rootModule, ascii(rootText)) }).pipe(
    Effect.provide(SourceResolver.memory(imports)),
  )
}

const codes = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)

const counter = `pub struct Counter { value: i32 }
impl Counter {
  pub fn zero() -> Self { return Counter { value: 0 } }
  pub fn make(value: i32) -> Self { return Counter { value: value } }
  pub fn read(self: &Self) -> i32 { return self.value }
  pub fn plus(self: Self, amount: i32) -> Self { return Counter { value: self.value + amount } }
}
`

it.effect('classifies inherent receivers and keeps members out of root scope', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${counter}
pub struct Runner {}
impl Runner { pub fn go(self: i32) -> i32 { return self } }
pub fn main() -> i32 { return Runner.go(42) }`)
    assert.deepEqual(codes(self), [])
    const receiver = (name: string): boolean | undefined => {
      const lookup = Analysis.memberByName(self, 'root', name)
      return lookup._tag === 'Resolved' && lookup.declaration._tag === 'FunctionDeclaration'
        ? lookup.declaration.associatedMember?.receiver
        : undefined
    }
    assert.strictEqual(receiver('Counter.read'), true)
    assert.strictEqual(receiver('Counter.plus'), true)
    assert.strictEqual(receiver('Counter.zero'), false)
    assert.strictEqual(receiver('Runner.go'), false)
    assert.strictEqual(Analysis.memberByName(self, 'root', 'read')._tag, 'Missing')
  }),
)

it.effect('resolves imported inherent members through their declaring owner', () =>
  Effect.gen(function* () {
    const self = yield* analyzeModules('app', [
      [
        'widgets',
        `pub struct Gadget { pub size: i32 }
impl Gadget {
  pub fn make(size: i32) -> Self { return Gadget { size: size } }
  fn secret() -> i32 { return 1 }
  pub fn width(self: &Self) -> i32 { return self.size }
}
pub fn helper() -> i32 { return 2 }`,
      ],
      [
        'app',
        `import widgets { Gadget }
pub fn main() -> i32 {
  let gadget = Gadget.make(42)
  return Gadget.width(&gadget)
}`,
      ],
    ])
    assert.deepEqual(codes(self), [])
    for (const member of ['Gadget.make', 'Gadget.width']) {
      const resolved = Analysis.memberByName(self, 'widgets', member)
      assert.strictEqual(resolved._tag, 'Resolved', member)
      if (resolved._tag === 'Resolved') {
        assert.strictEqual(resolved.declaration.canonical._tag, 'Canonical')
        if (resolved.declaration.canonical._tag === 'Canonical')
          assert.strictEqual(resolved.declaration.canonical.id.module, 'widgets')
      }
    }
    assert.strictEqual(Analysis.memberByName(self, 'app', 'make')._tag, 'Missing')
  }),
)

it.effect('prefers an inherent member over a matching module projection', () =>
  Effect.gen(function* () {
    const self = yield* analyzeModules('app', [
      [
        'counter',
        `pub struct Counter { value: i32 }
pub fn make() -> Counter { return Counter { value: 1 } }
impl Counter { pub fn make() -> Self { return Counter { value: 42 } } }
pub fn read(counter: &Counter) -> i32 { return counter.value }`,
      ],
      [
        'app',
        `import counter { Counter, read }
pub fn main() -> i32 { let made = Counter.make() return read(&made) }`,
      ],
    ])
    assert.deepEqual(codes(self), [])
    const member = Analysis.memberByName(self, 'counter', 'Counter.make')
    const free = Analysis.memberByName(self, 'counter', 'make')
    assert.strictEqual(member._tag, 'Resolved')
    assert.strictEqual(free._tag, 'Resolved')
    if (member._tag === 'Resolved' && free._tag === 'Resolved')
      assert.notStrictEqual(member.declaration, free.declaration)
  }),
)

it.effect('rejects invalid heads, duplicate members, collisions, and hooks', () =>
  Effect.gen(function* () {
    const specializedSource = `pub union Option<T> { None, Some { pub value: T } }
impl Option<i32> { pub fn special() -> i32 { return 1 } }
pub fn main() -> i32 { return 0 }`
    const specialized = yield* analyze(specializedSource)
    assert.deepEqual(codes(specialized), ['SEM0194'])
    assert.strictEqual(
      Analysis.diagnostics(specialized).at(0)?.span.start,
      specializedSource.indexOf('Option<i32> {'),
    )
    const bounded = yield* analyze(`interface Display { fn display(value: &Self) -> i32 }
pub union Option<T> { None, Some { pub value: T } }
impl<T: Display> Option<T> {
  pub fn show() -> i32 { return 1 }
  pub fn make(value: T) -> Self { return Option.Some { value: move value } }
}
pub fn main() -> i32 { return 0 }`)
    // The rejected head still closes `Self` for its members: the head's diagnostic is the only one.
    assert.deepEqual(codes(bounded), ['SEM0194'])
    const alias = yield* analyze(`pub union Option<T> { None, Some { pub value: T } }
type Maybe = Option<i32>
impl Maybe { pub fn special() -> i32 { return 1 } }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(alias), ['SEM0194'])
    const foreign = yield* analyzeModules('app', [
      ['widgets', `pub struct Gadget { pub size: i32 }\n`],
      [
        'app',
        `import widgets { Gadget }\nimpl Gadget { pub fn mine() -> i32 { return 1 } }\npub fn main() -> i32 { return 0 }`,
      ],
    ])
    assert.deepEqual(codes(foreign), ['SEM0194'])
    const duplicate = yield* analyze(`${counter}
impl Counter { pub fn read(self: &Self) -> i32 { return 2 } }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(duplicate).filter((code) => code === 'SEM0196').length, 2)
    const collision = yield* analyze(`pub struct Counter { value: i32 }
impl Counter { pub fn value(self: &Self) -> i32 { return self.value } }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(collision), ['SEM0195'])
    const hook = yield* analyze(`pub struct Guard { value: i32 }
impl Guard { fn drop(self: &mut Guard) -> () { return () } }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(hook), ['SEM0195'])
    const mapped = yield* analyze(`pub struct Guard { value: i32 }
fn make() -> Guard { return Guard { value: 1 } }
impl Guard { make: make }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(mapped), ['SEM0195'])
  }),
)
