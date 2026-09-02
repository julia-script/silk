import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Json from './support/Json.js'
import { unreachable } from './support/raise.js'
import * as WasmMain from './support/WasmMain.js'

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

const described = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => `${diagnostic.code} ${diagnostic.message}`)

const evaluated = (self: Analysis.Snapshot): bigint => {
  assert.deepEqual(described(self), [])
  const outcome = Analysis.evaluate(self)
  assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome))
  if (outcome._tag !== 'Completed') return unreachable('completed outcome')
  const value = outcome.result.value
  return typeof value === 'bigint' ? value : unreachable('integer result')
}

const counter = `pub struct Counter { value: i32 }
impl Counter {
  pub fn zero() -> Self { return Counter { value: 0 } }
  pub fn make(value: i32) -> Self { return Counter { value: value } }
  pub fn read(self: &Self) -> i32 { return self.value }
  pub fn plus(self: Self, amount: i32) -> Self { return Counter { value: self.value + amount } }
}
`

it.effect('resolves inherent members through the owner in every call form', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${counter}
pub fn main() -> i32 {
  let base = Counter.make(40)
  let direct = Counter.plus(move base, 1)
  let piped = move direct |> Counter.plus(1)
  let item: fn(&Counter) -> i32 = Counter.read
  let zero = Counter.zero()
  return item(&piped) + Counter.read(&zero)
}`)
    assert.strictEqual(evaluated(self), 42n)
  }),
)

it.effect('carries owner binders ahead of member binders and infers or accepts a prefix', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`pub union Option<T> { None, Some { pub value: T } }
impl<T> Option<T> {
  pub fn none() -> Self { return Option<T>.None }
  pub fn some(value: T) -> Self { return Option.Some { value: move value } }
  pub fn map<U>(self: Self, transform: once fn(T) -> U) -> Option<U> {
    return match move self {
      Option<T>.Some { value } => Option.some<U>(transform(move value))
      Option<T>.None => Option.none<U>()
    }
  }
  pub fn unwrapOr(self: Self, fallback: T) -> T {
    return match move self {
      Option<T>.Some { value } => keep<T>(move value, move fallback)
      Option<T>.None => move fallback
    }
  }
}
fn keep<T>(present: T, unused: T) -> T {
  drop unused
  return move present
}
fn addOne(value: i32) -> i32 { return value + 1 }
fn widen(value: i32) -> i64 { return 40 }
pub fn main() -> i32 {
  let explicit = Option.map<i32, i64>(Option.some(1), widen)
  let section = Option.map(addOne)
  let inferred = section(Option.some(2))
  let absent = Option.none<i32>() |> Option.unwrapOr(-1)
  let wide = Option.unwrapOr<i64>(move explicit, 0)
  let narrow = Option.unwrapOr(move inferred, 0)
  drop wide
  return narrow + absent + 40
}`)
    assert.strictEqual(evaluated(self), 42n)
  }),
)

it.effect('resolves members across modules through a selected import regardless of file name', () =>
  Effect.gen(function* () {
    const self = yield* analyzeModules('app', [
      [
        'widgets',
        `pub struct Gadget { pub size: i32 }\nimpl Gadget {\n  pub fn make(size: i32) -> Self { return Gadget { size: size } }\n  fn secret() -> i32 { return 1 }\n  pub fn width(self: &Self) -> i32 { return self.size }\n}\npub fn helper() -> i32 { return 2 }\n`,
      ],
      [
        'app',
        `import widgets { Gadget }\npub fn main() -> i32 {\n  let gadget = Gadget.make(42)\n  return Gadget.width(&gadget)\n}`,
      ],
    ])
    assert.strictEqual(evaluated(self), 42n)
    const leaking = yield* analyzeModules('app', [
      [
        'widgets',
        `pub struct Gadget { pub size: i32 }\nimpl Gadget { fn secret() -> i32 { return 1 } }\n`,
      ],
      ['app', `import widgets { Gadget }\npub fn main() -> i32 { return Gadget.secret() }`],
    ])
    assert.deepEqual(codes(leaking), ['SEM0015'])
    const notRoot = yield* analyzeModules('app', [
      [
        'widgets',
        `pub struct Gadget { pub size: i32 }\nimpl Gadget { pub fn make(size: i32) -> Self { return Gadget { size: size } } }\n`,
      ],
      ['app', `import widgets { Gadget, make }\npub fn main() -> i32 { return 0 }`],
    ])
    assert.deepEqual(codes(notRoot), ['SEM0197'])
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
impl<T: Display> Option<T> { pub fn show() -> i32 { return 1 } }
pub fn main() -> i32 { return 0 }`)
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

it.effect('keeps root functions free and members away from root scope', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`pub struct Counter { value: i32 }
fn transform(self: Counter) -> Counter { return move self }
impl Counter { pub fn make() -> Self { return Counter { value: 42 } } }
pub fn read(counter: &Counter) -> i32 { return counter.value }
pub fn main() -> i32 {
  let counter = transform(Counter.make())
  return read(&counter)
}`)
    assert.strictEqual(evaluated(self), 42n)
    const notMember = yield* analyze(`pub struct Counter { value: i32 }
fn transform(self: Counter) -> Counter { return move self }
pub fn main() -> i32 {
  let counter = Counter { value: 1 }
  drop Counter.transform(move counter)
  return 0
}`)
    assert.include(codes(notMember), 'SEM0010')
    const notRoot = yield* analyze(`pub struct Counter { value: i32 }
impl Counter { pub fn make() -> Self { return Counter { value: 42 } } }
pub fn main() -> i32 {
  drop make()
  return 0
}`)
    assert.include(codes(notRoot), 'SEM0004')
    const shortHead = yield* analyze(`pub union Option<T> { None, Some { pub value: T } }
impl Option { pub fn none() -> i32 { return 0 } }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(shortHead), ['SEM0194'])
  }),
)

it.effect('binds owner arguments from an applied qualifier ahead of the local prefix', () =>
  Effect.gen(function* () {
    const option = `pub union Option<T> { None, Some { pub value: T } }
impl<T> Option<T> {
  pub fn none() -> Self { return Option<T>.None }
  pub fn some(value: T) -> Self { return Option.Some { value: move value } }
  pub fn map<U>(self: Self, transform: once fn(T) -> U) -> Option<U> {
    return match move self {
      Option<T>.Some { value } => Option.some<U>(transform(move value))
      Option<T>.None => Option.none<U>()
    }
  }
  pub fn unwrapOr(self: Self, fallback: T) -> T {
    return match move self {
      Option<T>.Some { value } => keep<T>(move value, move fallback)
      Option<T>.None => move fallback
    }
  }
}
fn keep<T>(present: T, unused: T) -> T {
  drop unused
  return move present
}
fn widen(value: i32) -> i64 { return 40 }
`
    const self = yield* analyze(`${option}
pub fn main() -> i32 {
  let wide = Option<i32>.map<i64>(Option<i32>.some(2), widen)
  let absent = Option<i32>.none()
  let fallback = Option<i32>.unwrapOr(move absent, 2)
  let value = Option<i64>.unwrapOr(move wide, 0)
  drop value
  let variant = Option<i32>.None
  drop variant
  return fallback + 40
}`)
    assert.strictEqual(evaluated(self), 42n)
    const tooMany = yield* analyze(`${option}
pub fn main() -> i32 {
  drop Option<i32>.map<i64, i64>(Option<i32>.some(2), widen)
  return 0
}`)
    assert.include(codes(tooMany), 'SEM0051')
    const mismatchSource = `${option}
pub fn main() -> i32 {
  drop Option<i64>.unwrapOr(Option<i32>.some(2), 0)
  return 0
}`
    const mismatch = yield* analyze(mismatchSource)
    const appliedArgument = mismatchSource.indexOf('i64>.unwrapOr')
    const disagreement = Analysis.diagnostics(mismatch).find(
      (diagnostic) => diagnostic.code === 'SEM0100',
    )
    assert.isDefined(disagreement)
    assert.strictEqual(disagreement?.span.start, appliedArgument)
    assert.deepEqual(codes(mismatch), ['SEM0100'])
  }),
)

it.effect('treats enum, alias, service, and interface qualifiers through one member path', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`pub enum Status { Ready, Failed }
impl Status { pub fn code(status: Status) -> i32 { return match status { Status.Ready => 40 Status.Failed => 0 } } }
pub union Option<T> { None, Some { pub value: T } }
impl<T> Option<T> { pub fn some(value: T) -> Self { return Option.Some { value: move value } } }
type Maybe = Option<i32>
pub service Logger { fn log(self: &Self, level: i32) -> () }
impl Logger { pub fn quiet() -> i32 { return 1 } }
pub interface Keyed { fn key(value: &Self) -> i32 }
impl Keyed { pub fn describe() -> i32 { return 1 } }
pub fn main() -> i32 {
  let present = Maybe.some(2)
  let carried = match move present {
    Option<i32>.Some { value } => value
    Option<i32>.None => 0
  }
  return Status.code(Status.Ready) + carried + Logger.quiet() + Keyed.describe() - 2
}`)
    assert.strictEqual(evaluated(self), 42n)
    const collision = yield* analyze(`pub service Logger { fn log(self: &Self, level: i32) -> () }
impl Logger { pub fn log() -> i32 { return 1 } }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(collision), ['SEM0195'])
    const enumCollision = yield* analyze(`pub enum Status { Ready, Failed }
impl Status { pub fn Ready() -> i32 { return 1 } }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(enumCollision), ['SEM0195'])
  }),
)

it.effect('classifies receivers by a self parameter typed as the owner', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`${counter}
pub struct Runner {}
impl Runner { pub fn go(self: i32) -> i32 { return self } }
pub fn main() -> i32 { return Runner.go(42) }`)
    assert.strictEqual(evaluated(self), 42n)
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

it.effect('forms sections of associated functions without a receiver', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`pub struct Pair { pub left: i32 pub right: i32 }
impl Pair { pub fn make(left: i32, right: i32) -> Self { return Pair { left: left, right: right } } }
pub fn main() -> i32 {
  let withRight = Pair.make(2)
  let pair = withRight(40)
  return pair.left + pair.right
}`)
    assert.strictEqual(evaluated(self), 42n)
  }),
)

it.effect('lowers member calls and sections identically on wasm', () =>
  Effect.gen(function* () {
    const self = yield* analyze(
      `${counter}
pub fn main() -> i32 {
  let base = Counter.make(40) |> Counter.plus(1)
  let bump = Counter.plus(1)
  let done = bump(move base)
  return Counter.read(&done)
}`,
      'wasm32-unknown-unknown',
    )
    assert.strictEqual(evaluated(self), 42n)
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    assert.strictEqual(yield* WasmMain.invoke(wasm.bytes, 'InherentImpl.counterWasm'), 42)
  }),
)

it.effect('closes Self inside a member bound', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`pub interface Like<A> { fn like(value: &Self, other: &A) -> i32 }
pub struct Counter { value: i32 }
pub struct Twin { value: i32 }
impl Like<Counter> for Twin { fn like(value: &Self, other: &Counter) -> i32 { return value.value + other.value } }
impl Counter {
  pub fn twin<U: Like<Self>>(self: &Self, other: &U) -> i32 { return Like<Counter>.like(other, self) }
}
pub fn main() -> i32 {
  let counter = Counter { value: 40 }
  let twin = Twin { value: 2 }
  return Counter.twin(&counter, &twin)
}`)
    assert.strictEqual(evaluated(self), 42n)
  }),
)

it.effect('prefers a declared member over the module projection of a matching file name', () =>
  Effect.gen(function* () {
    const self = yield* analyzeModules('app', [
      [
        'counter',
        `pub struct Counter { value: i32 }
pub fn make() -> Counter { return Counter { value: 1 } }
impl Counter { pub fn make() -> Self { return Counter { value: 42 } } }
pub fn read(counter: &Counter) -> i32 { return counter.value }
`,
      ],
      [
        'app',
        `import counter { Counter, read }
pub fn main() -> i32 { let made = Counter.make()
  return read(&made) }`,
      ],
    ])
    assert.strictEqual(evaluated(self), 42n)
    const applied = yield* analyze(`pub union Option<T> { None, Some { pub value: T } }
impl<T> Option<T> {
  pub fn owned(self: Option<T>) -> i32 {
    drop self
    return 1
  }
  pub fn shared(self: &Option<T>) -> i32 {
    drop self
    return 2
  }
}
pub fn main() -> i32 { return 0 }`)
    const receiver = (name: string): boolean | undefined => {
      const lookup = Analysis.memberByName(applied, 'root', name)
      return lookup._tag === 'Resolved' && lookup.declaration._tag === 'FunctionDeclaration'
        ? lookup.declaration.associatedMember?.receiver
        : undefined
    }
    assert.strictEqual(receiver('Option.owned'), true)
    assert.strictEqual(receiver('Option.shared'), true)
  }),
)
