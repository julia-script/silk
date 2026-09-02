import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Analysis from '../src/Analysis.js'
import * as SemanticOccurrence from '../src/SemanticOccurrence.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as Json from './support/Json.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (text: string) =>
  Analysis.makeRealized({ root: SourceFile.make('main', ascii(text)) }).pipe(
    Effect.provide(SourceResolver.memory(new Map())),
  )

const analyzeModules = (rootModule: string, entries: ReadonlyArray<readonly [string, string]>) => {
  const sources = new Map(entries.map(([module, text]) => [module, ascii(text)] as const))
  const root = sources.get(rootModule)
  if (root === undefined) throw new Error(`missing root module ${rootModule}`)
  return Analysis.makeRealized({ root: SourceFile.make(rootModule, root) }).pipe(
    Effect.provide(SourceResolver.memory(sources)),
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
  if (outcome._tag !== 'Completed') throw new Error('unreachable')
  return outcome.result.value
}

const identityAt = (
  snapshot: Analysis.Snapshot,
  source: string,
  spelling: string,
  occurrence = 0,
): string | undefined => {
  let offset = -1
  for (let index = 0; index <= occurrence; index += 1) offset = source.indexOf(spelling, offset + 1)
  const found = Analysis.semanticOccurrenceAt(snapshot, 'main', offset)
  return found?.resolution._tag === 'Available'
    ? SemanticOccurrence.identityKey(found.resolution.identity)
    : undefined
}

const option = `pub union Option<T> { None, Some { pub value: T } }
impl<T> Option<T> {
  pub fn some(value: T) -> Self { return Option.Some { value: move value } }
  pub fn map<U>(self: Self, transform: once fn(T) -> U) -> Option<U> {
    return match move self {
      Option<T>.Some { value } => Option.Some { value: transform(move value) }
      Option<T>.None => Option<U>.None
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
`

const counter = `pub struct Counter { value: i32 }
impl Counter {
  pub fn zero() -> Self { return Counter { value: 0 } }
  pub fn read(self: &Self) -> i32 { return self.value }
  pub fn bump(self: &mut Self) -> i32 {
    self.value = self.value + 1
    return self.value
  }
  pub fn add(self: &Self, other: &Self) -> i32 { return self.value + other.value }
  pub fn take(self: Self) -> i32 { return self.value }
}
`

it.effect('resolves the three spellings of one member to one target', () =>
  Effect.gen(function* () {
    // The pipeline spelling captures no callable here: a section capturing a callable argument is
    // a separately tracked lowering gap, unrelated to receiver syntax.
    const source = `${option}
pub fn main() -> i32 {
  let direct = Option.unwrapOr(Option.some<i32>(13), 0)
  let piped = Option.some<i32>(13) |> Option.unwrapOr(0)
  let method = Option.some<i32>(13).unwrapOr(0)
  let value = Option.some<i32>(41)
  let mapped = value.map(addOne)
  return direct + piped + method + mapped.unwrapOr(0) - 39
}`
    const self = yield* analyze(source)
    assert.strictEqual(evaluated(self), 42n)
    // Every `unwrapOr` spelling shares the member's identity, and the receiver stays a value.
    const memberKey = 'declaration:main.Option.unwrapOr'
    assert.strictEqual(identityAt(self, source, 'unwrapOr(Option.some', 0), memberKey)
    assert.strictEqual(identityAt(self, source, 'unwrapOr(0)', 0), memberKey)
    assert.strictEqual(identityAt(self, source, 'unwrapOr(0)', 1), memberKey)
    assert.strictEqual(identityAt(self, source, 'unwrapOr(0)', 2), memberKey)
    assert.strictEqual(identityAt(self, source, 'map(addOne)', 0), 'declaration:main.Option.map')
    const receiverKey = identityAt(self, source, 'value.map', 0)
    assert.isDefined(receiverKey)
    assert.isTrue(receiverKey?.startsWith('binding:'), receiverKey)
  }),
)

it.effect(
  'applies a callable field before consulting members and keeps unknown members unknown',
  () =>
    Effect.gen(function* () {
      const field = yield* analyze(`pub struct Runner<F: fn(i32) -> i32> { pub apply: F }
fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 {
  let runner = Runner { apply: double }
  return runner.apply(21)
}`)
      assert.strictEqual(evaluated(field), 42n)
      const unknown = yield* analyze(`${counter}
pub fn main() -> i32 {
  let value = Counter.zero()
  return value.missing()
}`)
      assert.deepEqual(codes(unknown), ['SEM0027'])
    }),
)

it.effect('gives later arguments their expected types and keeps a shadowing local a value', () =>
  Effect.gen(function* () {
    const expected = yield* analyze(`${counter}
pub fn main() -> i32 {
  let left = Counter { value: 40 }
  let right = Counter { value: 2 }
  return left.add(&right)
}`)
    assert.strictEqual(evaluated(expected), 42n)
    const shadowing = yield* analyze(`${counter}
pub fn main() -> i32 {
  let Counter = Counter { value: 42 }
  return Counter.read()
}`)
    assert.strictEqual(evaluated(shadowing), 42n)
  }),
)

it.effect('adapts the receiver to the declared parameter zero', () =>
  Effect.gen(function* () {
    const shared = yield* analyze(`${counter}
pub fn main() -> i32 {
  let value = Counter { value: 21 }
  let first = value.read()
  return first + value.read()
}`)
    assert.strictEqual(evaluated(shared), 42n)
    const exclusive = yield* analyze(`${counter}
pub fn main() -> i32 {
  let mut value = Counter { value: 40 }
  let bumped = value.bump()
  return bumped + value.bump() - 41
}`)
    assert.strictEqual(evaluated(exclusive), 42n)
    const immutable = yield* analyze(`${counter}
pub fn main() -> i32 {
  let value = Counter { value: 40 }
  return value.bump()
}`)
    assert.deepEqual(codes(immutable), ['SEM0057'])
    const consumed = yield* analyze(`${counter}
pub fn main() -> i32 {
  let value = Counter { value: 42 }
  let taken = value.take()
  return taken + value.read()
}`)
    assert.deepEqual(codes(consumed), ['OWN0001'])
    const rvalue = yield* analyze(`${option}
pub fn main() -> i32 { return Option.some(41).map(addOne).unwrapOr(0) }`)
    assert.strictEqual(evaluated(rvalue), 42n)
    const reference = yield* analyze(`${counter}
fn through(value: &Counter) -> i32 { return value.read() }
pub fn main() -> i32 {
  let value = Counter { value: 42 }
  return through(&value)
}`)
    assert.strictEqual(evaluated(reference), 42n)
    const boxed = yield* analyze(`import silk.box { Box }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
${counter}
effect fn program() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let boxed = run Box.make(Counter { value: 42 })
  return boxed.read()
}
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(boxed), ['SEM0027'])
  }),
)

it.effect('rejects associated functions on values and members used as values', () =>
  Effect.gen(function* () {
    const noReceiver = yield* analyze(`${counter}
pub fn main() -> i32 {
  let value = Counter { value: 1 }
  return value.zero().read()
}`)
    assert.deepEqual(codes(noReceiver), ['SEM0198'])
    assert.include(described(noReceiver).at(0) ?? '', 'Counter.zero')
    const asValue = yield* analyze(`${counter}
pub fn main() -> i32 {
  let value = Counter { value: 1 }
  let reader = value.read
  return 0
}`)
    assert.deepEqual(codes(asValue), ['SEM0199'])
    assert.include(described(asValue).at(0) ?? '', 'Counter.read')
  }),
)

const printable = `interface Printable { fn print(value: &Self) -> i32 }
interface Debug { fn print(value: &Self) -> i32 }
struct Document { size: i32 }
impl Printable for Document { fn print(value: &Self) -> i32 { return value.size } }
`

it.effect('reaches a bound operation through a generic receiver and only there', () =>
  Effect.gen(function* () {
    const bounded = yield* analyze(`${printable}
fn show<T: Printable>(value: &T) -> i32 { return value.print() }
pub fn main() -> i32 {
  let document = Document { size: 42 }
  return show(&document)
}`)
    assert.strictEqual(evaluated(bounded), 42n)
    const explicit = yield* analyze(`${printable}
fn show<T: Printable>(value: &T) -> i32 { return Printable.print(value) }
pub fn main() -> i32 {
  let document = Document { size: 42 }
  return show(&document)
}`)
    assert.strictEqual(evaluated(explicit), 42n)
    const unbounded = yield* analyze(`${printable}
fn show<T>(value: &T) -> i32 { return value.print() }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(unbounded), ['SEM0026'])
    const ambiguous = yield* analyze(`${printable}
impl Debug for Document { fn print(value: &Self) -> i32 { return value.size } }
fn show<T: Printable + Debug>(value: &T) -> i32 { return value.print() }
fn shown<T: Printable + Debug>(value: &T) -> i32 { return Printable.print(value) }
pub fn main() -> i32 {
  let document = Document { size: 42 }
  return shown(&document)
}`)
    assert.deepEqual(codes(ambiguous), ['SEM0200'])
    assert.include(described(ambiguous).at(0) ?? '', 'Printable')
    assert.include(described(ambiguous).at(0) ?? '', 'Debug')
    const concrete = yield* analyze(`${printable}
pub fn main() -> i32 {
  let document = Document { size: 42 }
  return document.print()
}`)
    assert.deepEqual(codes(concrete), ['SEM0027'])
  }),
)

it.effect('keeps loans and reference arguments exactly as the explicit form does', () =>
  Effect.gen(function* () {
    // An exclusive receiver loan conflicts with a live shared loan of the same place.
    const conflicting = yield* analyze(`${counter}
pub fn main() -> i32 {
  let mut value = Counter { value: 40 }
  let shared = &value
  let bumped = value.bump()
  return bumped + shared.value
}`)
    assert.deepEqual(codes(conflicting), ['OWN0010'])
    // A `&fixedArray` written argument keeps its reference type with no second diagnostic.
    const fixedArray = yield* analyze(`pub struct Summer { base: i32 }
impl Summer {
  pub fn sum(self: &Self, values: &[i32]) -> i32 { return self.base + values[0] + values[1] }
}
pub fn main() -> i32 {
  let summer = Summer { base: 30 }
  let values = [5, 7]
  return summer.sum(&values)
}`)
    assert.strictEqual(evaluated(fixedArray), 42n)
  }),
)

it.effect('renames a member through every spelling at once', () =>
  Effect.gen(function* () {
    const source = `${counter}
pub fn main() -> i32 {
  let value = Counter { value: 14 }
  let direct = Counter.read(&value)
  let piped = &value |> Counter.read
  let method = value.read()
  return direct + piped + method
}`
    const self = yield* analyze(source)
    assert.strictEqual(evaluated(self), 42n)
    const key = 'declaration:main.Counter.read'
    const matches: Array<SourceSpan.SourceSpan> = []
    for (const [module, file] of Analysis.sources(self)) {
      const whole = Option.getOrThrow(SourceSpan.make(file, 0, SourceFile.length(file)))
      for (const occurrence of Analysis.semanticOccurrencesInRange(self, module, whole)) {
        if (occurrence.resolution._tag !== 'Available') continue
        if (SemanticOccurrence.identityKey(occurrence.resolution.identity) !== key) continue
        if (Option.getOrUndefined(SourceFile.spelling(file, occurrence.span)) !== 'read') continue
        matches.push(occurrence.span)
      }
    }
    const renamed = [...matches]
      .sort((left, right) => right.start - left.start)
      .reduce((text, span) => `${text.slice(0, span.start)}peek${text.slice(span.end)}`, source)
    assert.strictEqual(
      renamed,
      source
        .replace('pub fn read(self: &Self)', 'pub fn peek(self: &Self)')
        .replace('Counter.read(&value)', 'Counter.peek(&value)')
        .replace('|> Counter.read', '|> Counter.peek')
        .replace('value.read()', 'value.peek()'),
    )
  }),
)

it.effect('completes and hovers the value side with receiver-bound members', () =>
  Effect.gen(function* () {
    const source = `${counter}
pub fn main() -> i32 {
  let value = Counter { value: 42 }
  let read = value.read()
  return value.
}`
    const self = yield* analyze(source)
    const completion = Analysis.completionAt(self, 'main', source.lastIndexOf('value.') + 6)
    assert.deepEqual(completion?.context, { _tag: 'ValueMemberContext', state: 'Available' })
    assert.deepEqual(
      completion?.candidates.map((candidate) => [candidate.label, candidate.kind]),
      [
        ['value', 'Field'],
        ['add', 'Method'],
        ['bump', 'Method'],
        ['read', 'Method'],
        ['take', 'Method'],
      ],
    )
    assert.strictEqual(
      completion?.candidates.find((candidate) => candidate.label === 'add')?.detail?.text,
      'fn(other: &Counter) -> i32',
    )
    // Receiver syntax hovers the receiver-bound contract; the type side keeps the whole contract.
    const method = Analysis.hoverSubjectAt(self, 'main', source.indexOf('value.read()') + 6)
    assert.strictEqual(method?.presentation.text, 'fn() -> i32')
    const explicit = yield* analyze(`${counter}
pub fn main() -> i32 {
  let value = Counter { value: 42 }
  return Counter.read(&value)
}`)
    const typeSide = Analysis.hoverSubjectAt(
      explicit,
      'main',
      `${counter}\npub fn main() -> i32 {\n  let value = Counter { value: 42 }\n  return Counter.`
        .length,
    )
    assert.strictEqual(typeSide?.presentation.text, 'pub fn read(self: &Counter) -> i32')
  }),
)

it.effect('completes bound operations after a generic receiver', () =>
  Effect.gen(function* () {
    const source = `${printable}
fn show<T: Printable>(value: &T) -> i32 { return value. }
pub fn main() -> i32 { return 0 }`
    const self = yield* analyze(source)
    const completion = Analysis.completionAt(self, 'main', source.indexOf('value. }') + 6)
    assert.deepEqual(completion?.context, { _tag: 'ValueMemberContext', state: 'Available' })
    assert.deepEqual(
      completion?.candidates.map((candidate) => [candidate.label, candidate.kind]),
      [['print', 'Method']],
    )
    assert.strictEqual(completion?.candidates.at(0)?.detail?.text, 'fn() -> i32')
  }),
)

it.effect('substitutes the owner binders a receiver fixes in hover and completion', () =>
  Effect.gen(function* () {
    const source = `${option}
pub fn main() -> i32 {
  let value = Option.some<i32>(41)
  let mapped = value.map(addOne)
  let more = value.
  return mapped.unwrapOr(0)
}`
    const self = yield* analyze(source)
    const hover = Analysis.hoverSubjectAt(self, 'main', source.indexOf('value.map(') + 6)
    assert.strictEqual(hover?.presentation.text, 'fn<U>(transform: once fn(i32) -> U) -> Option<U>')
    const completion = Analysis.completionAt(self, 'main', source.indexOf('value.\n') + 6)
    assert.deepEqual(
      completion?.candidates.map((candidate) => [candidate.label, candidate.detail?.text]),
      [
        ['map', 'fn<U>(transform: once fn(i32) -> U) -> Option<U>'],
        ['unwrapOr', 'fn(fallback: i32) -> i32'],
      ],
    )
  }),
)

it.effect('evaluates the method-call fixture covering the specified matrix', () =>
  Effect.gen(function* () {
    const source = readFileSync(
      new URL('./fixtures/method-calls/main.silk', import.meta.url),
      'utf8',
    )
    const self = yield* analyze(source)
    // 1 + 1 + 1 + 5 + 4 + 6 + 1 + 2 + 11 + 2 + 1 + 1 + 2 = 38
    assert.strictEqual(evaluated(self), 38n)
  }),
)

it.effect(
  'accepts explicit borrows, reference receivers, chains, and explicit type arguments',
  () =>
    Effect.gen(function* () {
      // A written borrow of the receiver analyzes as the reference receiver it is; the grouped
      // borrow's lowering is a separately tracked gap shared with `Counter.read((&value))`.
      const explicitBorrow = yield* analyze(`${counter}
pub fn main() -> i32 {
  let value = Counter { value: 42 }
  return (&value).read()
}`)
      assert.deepEqual(described(explicitBorrow), [])
      // A `&mut` reference reborrows for a `&Self` method, inside and outside the owner.
      const reborrow = yield* analyze(`pub struct Counter { value: i32 }
impl Counter {
  pub fn read(self: &Self) -> i32 { return self.value }
  pub fn bumpTwice(self: &mut Self) -> i32 {
    let before = self.read()
    self.value = self.value + 2
    return before + self.read()
  }
}
fn through(value: &mut Counter) -> i32 { return value.read() }
pub fn main() -> i32 {
  let mut value = Counter { value: 19 }
  let doubled = value.bumpTwice()
  return doubled + through(&mut value) - 19
}`)
      assert.strictEqual(evaluated(reborrow), 42n)
      // A chained receiver projects through fields.
      const chained = yield* analyze(`${counter}
pub struct Holder { inner: Counter }
pub fn main() -> i32 {
  let holder = Holder { inner: Counter { value: 42 } }
  return holder.inner.read()
}`)
      assert.strictEqual(evaluated(chained), 42n)
      // Explicit type arguments bind the member's own binders; the receiver fixes the owner's.
      const explicitArguments = yield* analyze(`import silk.i32 as i32
import silk.i64 as i64
${option}
fn widen(value: i32) -> i64 { return i32.toI64(value) }
pub fn main() -> i32 {
  let value = Option.some<i32>(41)
  let widened = value.map<i64>(widen)
  return i64.toI32(widened.unwrapOr(i64.toI64(0))) + 1
}`)
      assert.strictEqual(evaluated(explicitArguments), 42n)
    }),
)

it.effect('keeps visibility, navigation, and own-binder rules across the value side', () =>
  Effect.gen(function* () {
    const hidden = yield* analyzeModules('app', [
      [
        'widgets',
        `pub struct Gadget { pub size: i32 }
impl Gadget { fn secret(self: &Self) -> i32 { return self.size } }
`,
      ],
      [
        'app',
        `import widgets { Gadget }
pub fn main() -> i32 {
  let gadget = Gadget { size: 1 }
  return gadget.secret()
}`,
      ],
    ])
    assert.deepEqual(codes(hidden), ['SEM0015'])
    const source = `${printable}
fn show<T: Printable>(value: &T) -> i32 { return value.print() }
pub fn main() -> i32 {
  let document = Document { size: 42 }
  return show(&document)
}`
    const bounded = yield* analyze(source)
    const key = identityAt(bounded, source, 'print() }', 0)
    assert.isDefined(key)
    assert.isTrue(key?.startsWith('service-operation:'), key)
    assert.strictEqual(identityAt(bounded, source, 'print(value: &Self)', 0), key)
    // An operation with its own type parameters is not a member through either spelling.
    const ownBinders = yield* analyze(`interface Widen { fn widen<U>(value: &Self, seed: U) -> U }
struct Cell { value: i32 }
impl Widen for Cell { fn widen<U>(value: &Self, seed: U) -> U { return move seed } }
fn show<T: Widen>(value: &T) -> i32 { return value.widen(1) }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(codes(ownBinders), ['SEM0026'])
  }),
)

it.effect('hovers the explicit form with the complete contract', () =>
  Effect.gen(function* () {
    const source = `${option}
pub fn main() -> i32 {
  let value = Option.some<i32>(41)
  let mapped = Option.map(move value, addOne)
  return mapped.unwrapOr(0)
}`
    const self = yield* analyze(source)
    const hover = Analysis.hoverSubjectAt(self, 'main', source.indexOf('Option.map(') + 7)
    assert.strictEqual(
      hover?.presentation.text,
      'pub fn map<T, U>(self: Option<T>, transform: once fn(T) -> U) -> main.Option<U>',
    )
  }),
)
