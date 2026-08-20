import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('interface-bounds/main', encoder.encode(source))

const evaluate = (source: string) =>
  Effect.map(snapshot(source), (self) => ({ self, outcome: Analysis.evaluate(self) }))

const messages = (self: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.message)

/** Evaluated scalars can be BigInt, which plain JSON serialization refuses. */
const describe = (outcome: unknown): string =>
  JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value), 2)

/**
 * Two operations on one bound. `combine` calls both — `+` selects `add` and `-` selects
 * `subtract` — over the canonical parameter, once, before any concrete argument exists.
 */
const twoOperations = `pub interface Arith {
  fn add(left: Self, right: Self) -> Self
  fn subtract(left: Self, right: Self) -> Self
}
impl Arith for i32 { add: Intrinsic.i32Add subtract: Intrinsic.i32Subtract }
pub fn combine<T: Arith>(left: T, right: T, offset: T) -> T {
  return (move left + move right) - move offset
}
pub fn main() -> i32 { return combine(40, 44, 40) }`

it.effect('calls every operation of a two-operation bound in one generic body', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(twoOperations)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    // (40 + 44) - 40 selects `subtract`, not a second `add`.
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 44n)
  }),
)

it.effect('specializes one two-operation bound per conforming provider', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`pub interface Arith {
  fn add(left: Self, right: Self) -> Self
  fn subtract(left: Self, right: Self) -> Self
}
impl Arith for i32 { add: Intrinsic.i32Add subtract: Intrinsic.i32Subtract }
impl Arith for u8 { add: Intrinsic.u8Add subtract: Intrinsic.u8Subtract }
pub fn combine<T: Arith>(left: T, right: T, offset: T) -> T {
  return (move left + move right) - move offset
}
pub fn main() -> i32 {
  let narrow = combine<u8>(10, 20, 5)
  return combine<i32>(40, 42, 40)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

/**
 * A bound whose operations do not all return the parameter. `lessThan` is declared over `T` but
 * results in `bool`, so the operator keeps the operation's own declared result.
 */
const comparisonBound = `pub interface Ranked {
  fn lessThan(left: &Self, right: &Self) -> bool
  fn subtract(left: Self, right: Self) -> Self
}
impl Ranked for i32 { lessThan: Intrinsic.i32LessThan subtract: Intrinsic.i32Subtract }
pub fn gap<T: Ranked>(left: T, right: T) -> T {
  if left < right { return move right - move left }
  return move left - move right
}
pub fn main() -> i32 { return gap(2, 44) }`

it.effect('keeps a bound comparison at its declared result type', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(comparisonBound)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('rejects a type argument whose witness omits one bound operation', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub interface Arith {
  fn add(left: Self, right: Self) -> Self
  fn subtract(left: Self, right: Self) -> Self
}
impl Arith for i32 { add: Intrinsic.i32Add }
pub fn combine<T: Arith>(left: T, right: T, offset: T) -> T {
  return (move left + move right) - move offset
}
pub fn main() -> i32 { return combine(40, 44, 40) }`)
    // The specialization names the operation the witness never supplied, not just the interface.
    assert.include(messages(self), 'Invalid conformance: i32 does not implement Arith.subtract')
  }),
)

it.effect('checks a bound against a type argument an explicit prefix wrote', () =>
  Effect.gen(function* () {
    // A substitution seeded from a written prefix is still a substitution: what it binds faces the
    // same conformance check an inferred binding does.
    const declarations = `pub interface Arith {
  fn add(left: Self, right: Self) -> Self
  fn subtract(left: Self, right: Self) -> Self
}
impl Arith for i32 { add: Intrinsic.i32Add subtract: Intrinsic.i32Subtract }
struct Plain { value: i32 }
pub fn combine<T: Arith, U>(left: T, right: T, offset: T, other: U) -> T {
  return (move left + move right) - move offset
}
`
    const { self, outcome } = yield* evaluate(
      `${declarations}pub fn main() -> i32 { return combine<i32>(40, 44, 40, true) }`,
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 44n)

    const violated = yield* snapshot(
      `${declarations}pub fn main() -> i32 { let held = combine<Plain>(Plain { value: 1 }, Plain { value: 2 }, Plain { value: 1 }, true) return 0 }`,
    )
    assert.include(
      messages(violated),
      'Invalid conformance: interface-bounds/main.Plain does not implement Arith',
    )
  }),
)

it.effect('accepts an interface another module declares as a bound', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`import silk.numeric { Integer }
pub fn sum<T: Integer>(left: T, right: T) -> T { return move left + move right }
pub fn main() -> i32 { return sum(20, 22) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('keeps the single-operation Integer bound working unchanged', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`import silk.numeric { add }
pub fn main() -> i32 {
  let narrow = add<u8>(1, 2)
  return add<i32>(40, 2)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('rejects a type argument with no witness for the bound', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub interface Arith {
  fn add(left: &Self, right: &Self) -> Self
  fn subtract(left: &Self, right: &Self) -> Self
}
impl Arith for i32 { add: Intrinsic.i32Add subtract: Intrinsic.i32Subtract }
pub fn combine<T: Arith>(left: T, right: T) -> T { return (left + right) - left }
pub fn main() -> i32 {
  let wide = combine<i64>(40, 44)
  return 0
}`)
    assert.include(messages(self), 'Invalid conformance: i64 does not implement Arith')
  }),
)

it.effect('reports a bound that names no reachable interface', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub struct Holder<T> { value: T }
pub fn combine<T: Holder>(left: T, right: T) -> T { return left + right }
pub fn main() -> i32 { return combine(40, 2) }`)
    assert.include(messages(self), 'Invalid conformance: unknown interface constraint Holder')
  }),
)

/**
 * A bound operation no operator spells. `mix` is reachable only through the bound's own name, and
 * the two witnesses answer it with two unrelated instructions — wrapping for `i32`, saturating for
 * `u8` — so the call must read the witness the specialization selected rather than reuse an
 * operator's width-neutral lowering.
 */
const nonOperatorOperation = `pub interface Mixer {
  fn mix(left: Self, right: Self) -> Self
}
impl Mixer for i32 { mix: Intrinsic.i32WrappingAdd }
impl Mixer for u8 { mix: Intrinsic.u8SaturatingAdd }
pub fn blend<T: Mixer>(left: T, right: T) -> T { return Mixer.mix(move left, move right) }
pub fn main() -> i32 { return blend(40, 2) }`

it.effect('calls a bound operation no operator spells', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(nonOperatorOperation)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('reaches a different witness for each specialized type argument', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`pub interface Mixer {
  fn mix(left: Self, right: Self) -> Self
}
impl Mixer for i32 { mix: Intrinsic.i32WrappingAdd }
impl Mixer for u8 { mix: Intrinsic.u8SaturatingAdd }
pub fn blend<T: Mixer>(left: T, right: T) -> T { return Mixer.mix(move left, move right) }
pub fn main() -> i32 {
  // u8 saturates at 255; i32 wraps from its maximum to its minimum. One shared body, two
  // instructions, and neither is the other's width-neutral form.
  let saturated = blend<u8>(200, 100)
  let wrapped = blend<i32>(2147483647, 1)
  if saturated != 255 { return 1 }
  if wrapped != -2147483648 { return 2 }
  return 42
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('keeps a bound operation at the result its interface declares', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`pub interface Ranked {
  fn ranksBelow(left: &Self, right: &Self) -> bool
}
impl Ranked for i32 { ranksBelow: Intrinsic.i32LessThan }
pub fn ranks<T: Ranked>(left: &T, right: &T) -> bool {
  return Ranked.ranksBelow(left, right)
}
pub fn main() -> i32 {
  let left = 2
  let right = 42
  if ranks<i32>(&left, &right) { return right }
  return left
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('rejects an unrun effectful bound operation at the generic return boundary', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`interface Decoder {
  effect fn decode(value: &Self) -> i32
}
struct Schema {}
effect fn decodeSchema(value: &Schema) -> i32 { return 42 }
impl Decoder for Schema { decode: Schema.decodeSchema }
effect fn decodeWith<T: Decoder>(value: &T) -> i32 {
  return Decoder.decode(value)
}
pub fn main() -> i32 {
  let schema = Schema {}
  return run decodeWith<Schema>(&schema)
}`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => ({
        code: diagnostic.code,
        reason: diagnostic.reason,
      })),
      [
        {
          code: 'SEM0129',
          reason: { _tag: 'ReturnTypeMismatch', expected: 'i32', actual: 'Effect<i32>' },
        },
      ],
    )
    assert.strictEqual(Analysis.mirOf(self)._tag, 'Unavailable')
  }),
)

it.effect('keeps a mapped witness with an invalid body out of target-dependent phases', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`interface Decoder {
  fn decode(value: &Self) -> i32
}
struct Schema {}
fn decodeSchema(value: &Schema) -> i32 { return true }
impl Decoder for Schema { decode: Schema.decodeSchema }
fn decodeWith<T: Decoder>(value: &T) -> i32 { return Decoder.decode(value) }
pub fn main() -> i32 {
  let schema = Schema {}
  return decodeWith<Schema>(&schema)
}`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0129'],
    )
    assert.strictEqual(Analysis.mirOf(self)._tag, 'Unavailable')
  }),
)

/**
 * `Integer.add` names both the bound's operation and a public function of `silk/numeric`. Inside a
 * body bounded by `Integer` the bound takes the spelling; the module function stays reachable
 * everywhere else, including through its own module namespace.
 */
it.effect('prefers the bound operation over a same-named module function', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`import silk.numeric { Integer }
pub fn sum<T: Integer>(left: T, right: T) -> T {
  return Integer.add(move left, move right)
}
pub fn main() -> i32 { return sum(20, 22) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('keeps the module function reachable where no bound claims the name', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`import silk.numeric { Integer }
pub fn main() -> i32 { return Integer.add(40, 2) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('executes an inline conformance operation with Self bound to its provider', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`interface Decoder {
  fn decode(value: &Self) -> i32
}
struct Schema { value: i32 }
impl Decoder for Schema {
  fn decode(value: &Self) -> i32 { return value.value }
}
fn decode<T: Decoder>(value: &T) -> i32 { return Decoder.decode(value) }
pub fn main() -> i32 {
  let schema = Schema { value: 42 }
  return decode(&schema)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('combines inline and mapped operations in one complete conformance', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`interface PairValue {
  fn left(value: &Self) -> i32
  fn right(value: &Self) -> i32
}
struct Pair { left: i32 right: i32 }
fn pairRight(value: &Pair) -> i32 { return value.right }
impl PairValue for Pair {
  fn left(value: &Self) -> i32 { return value.left }
  right: Pair.pairRight
}
fn sum<T: PairValue>(value: &T) -> i32 {
  return PairValue.left(value) + PairValue.right(value)
}
pub fn main() -> i32 {
  let pair = Pair { left: 20, right: 22 }
  return sum(&pair)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('reports a bound operation reachable through two bounded parameters', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub interface Mixer {
  fn mix(left: Self, right: Self) -> Self
}
impl Mixer for i32 { mix: Intrinsic.i32WrappingAdd }
pub fn blend<A: Mixer, B: Mixer>(left: A, right: B) -> A {
  return Mixer.mix(move left, move left)
}
pub fn main() -> i32 { return blend<i32, i32>(40, 2) }`)
    assert.include(messages(self), 'Mixer.mix is ambiguous across bounded type parameters A, B')
  }),
)

it.effect('reports a bound operation the interface never declares', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub interface Mixer {
  fn mix(left: Self, right: Self) -> Self
}
impl Mixer for i32 { mix: Intrinsic.i32WrappingAdd }
pub fn blend<T: Mixer>(left: T, right: T) -> T {
  return Mixer.stir(move left, move right)
}
pub fn main() -> i32 { return blend(40, 2) }`)
    assert.include(messages(self), 'Mixer has no operation stir')
  }),
)

it.effect('records the resolved bound contract on the declaration it belongs to', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(twoOperations)
    const declaration = Analysis.declarationIndex(self)
      .modules.find((module) => module.module === 'interface-bounds/main')
      ?.declarations.find(
        (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === 'combine',
      )
    const bound = declaration?.typeParameters.at(0)?.bound
    assert.strictEqual(bound?._tag, 'ResolvedBound')
    if (bound?._tag !== 'ResolvedBound') return
    assert.strictEqual(bound.spelling, 'Arith')
    assert.deepEqual(bound.application.declaration, {
      _tag: 'CanonicalDeclarationId',
      module: 'interface-bounds/main',
      name: 'Arith',
    })
    assert.deepEqual(
      bound.application.operations.map((operation) =>
        operation.declaration.name._tag === 'Present'
          ? operation.declaration.name.spelling
          : 'Unavailable',
      ),
      ['add', 'subtract'],
    )
  }),
)
