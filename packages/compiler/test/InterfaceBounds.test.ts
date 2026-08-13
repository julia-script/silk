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
const twoOperations = `pub interface Arith<T> {
  fn add(left: T, right: T) -> T
  fn subtract(left: T, right: T) -> T
}
impl Arith<i32> for i32 { add: Intrinsic.i32Add subtract: Intrinsic.i32Subtract }
pub fn combine<T: Arith>(left: T, right: T) -> T { return (left + right) - left }
pub fn main() -> i32 { return combine(40, 44) }`

it.effect('calls every operation of a two-operation bound in one generic body', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(twoOperations)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    // (40 + 44) - 40 selects `subtract`, not a second `add`.
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 44)
  }),
)

it.effect('specializes one two-operation bound per conforming provider', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`pub interface Arith<T> {
  fn add(left: T, right: T) -> T
  fn subtract(left: T, right: T) -> T
}
impl Arith<i32> for i32 { add: Intrinsic.i32Add subtract: Intrinsic.i32Subtract }
impl Arith<u8> for u8 { add: Intrinsic.u8Add subtract: Intrinsic.u8Subtract }
pub fn combine<T: Arith>(left: T, right: T) -> T { return (left + right) - left }
pub fn main() -> i32 {
  let narrow = combine<u8>(200, 55)
  return combine<i32>(40, 42)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

/**
 * A bound whose operations do not all return the parameter. `lessThan` is declared over `T` but
 * results in `bool`, so the operator keeps the operation's own declared result.
 */
const comparisonBound = `pub interface Ranked<T> {
  fn lessThan(left: T, right: T) -> bool
  fn subtract(left: T, right: T) -> T
}
impl Ranked<i32> for i32 { lessThan: Intrinsic.i32LessThan subtract: Intrinsic.i32Subtract }
pub fn gap<T: Ranked>(left: T, right: T) -> T {
  if left < right { return right - left }
  return left - right
}
pub fn main() -> i32 { return gap(2, 44) }`

it.effect('keeps a bound comparison at its declared result type', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(comparisonBound)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('rejects a type argument whose witness omits one bound operation', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub interface Arith<T> {
  fn add(left: T, right: T) -> T
  fn subtract(left: T, right: T) -> T
}
impl Arith<i32> for i32 { add: Intrinsic.i32Add }
pub fn combine<T: Arith>(left: T, right: T) -> T { return (left + right) - left }
pub fn main() -> i32 { return combine(40, 44) }`)
    // The specialization names the operation the witness never supplied, not just the interface.
    assert.include(messages(self), 'Invalid conformance: i32 does not implement Arith.subtract')
  }),
)

it.effect('accepts an interface another module declares as a bound', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`import silk.numeric { Integer }
pub fn twice<T: Integer>(value: T) -> T { return value + value }
pub fn main() -> i32 { return twice(21) }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
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
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('rejects a type argument with no witness for the bound', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub interface Arith<T> {
  fn add(left: T, right: T) -> T
  fn subtract(left: T, right: T) -> T
}
impl Arith<i32> for i32 { add: Intrinsic.i32Add subtract: Intrinsic.i32Subtract }
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
    assert.deepEqual(bound.capability, {
      _tag: 'CanonicalDeclarationId',
      module: 'interface-bounds/main',
      name: 'Arith',
    })
    assert.deepEqual(bound.operations, ['add', 'subtract'])
  }),
)
