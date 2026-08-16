import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import { unreachable } from './support/raise.js'

/**
 * Aggregate receiver access for Effect representations stored in nominal fields.
 *
 * These receiver rules remain the ownership admission boundary after proven nominal realizations
 * cross the layout fence.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const realized = Effect.fnUntraced(function* (name: string, source: string) {
  return yield* Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')
})

const codesOf = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

const declarations = `struct Shared<F: Effect<i32>> { operation: F }
struct Exclusive<F: mut Effect<i32>> { operation: F }
struct Once<F: once Effect<i32>> { operation: F }
`

it.effect('runs a shared stored Effect through a shared aggregate borrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/shared-through-shared',
      `${declarations}fn runShared<F: Effect<i32>>(value: &Shared<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.notInclude(codesOf(snapshot), 'OWN0015')
  }),
)

it.effect('rejects an exclusive stored Effect through a shared aggregate borrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/exclusive-through-shared',
      `${declarations}fn runExclusive<F: mut Effect<i32>>(value: &Exclusive<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )
    const diagnostic =
      Analysis.diagnostics(snapshot).find((candidate) => candidate.code === 'OWN0015') ??
      unreachable('expected a stored-Effect run access rejection')

    assert.strictEqual(diagnostic.reason._tag, 'StoredEffectRunAccess')
    if (diagnostic.reason._tag !== 'StoredEffectRunAccess') return
    assert.strictEqual(diagnostic.reason.receiver, 'Shared')
    assert.strictEqual(diagnostic.reason.required, 'Exclusive')
    assert.strictEqual(diagnostic.reason.field, '#0')
    assert.include(diagnostic.message, 'access to the whole aggregate')
  }),
)

it.effect('runs an exclusive stored Effect through an exclusive aggregate borrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/exclusive-through-exclusive',
      `${declarations}fn runExclusive<F: mut Effect<i32>>(value: &mut Exclusive<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.notInclude(codesOf(snapshot), 'OWN0015')
  }),
)

it.effect('rejects a consuming stored Effect through shared and exclusive aggregate borrows', () =>
  Effect.gen(function* () {
    const shared = yield* realized(
      'stored-effect-ownership/consuming-through-shared',
      `${declarations}fn runOnce<F: once Effect<i32>>(value: &Once<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )
    const exclusive = yield* realized(
      'stored-effect-ownership/consuming-through-exclusive',
      `${declarations}fn runOnce<F: once Effect<i32>>(value: &mut Once<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.deepEqual(
      [shared, exclusive].map((snapshot) => {
        const diagnostic = Analysis.diagnostics(snapshot).find(
          (candidate) => candidate.code === 'OWN0015',
        )
        return diagnostic?.reason._tag === 'StoredEffectRunAccess'
          ? [diagnostic.reason.receiver, diagnostic.reason.required]
          : undefined
      }),
      [
        ['Shared', 'Take'],
        ['Exclusive', 'Take'],
      ],
    )
  }),
)

it.effect('admits a consuming stored Effect only through whole-owner aggregate access', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/consuming-through-owner',
      `${declarations}fn runOnce<F: once Effect<i32>>(value: Once<F>) -> i32 {
  return run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.notInclude(codesOf(snapshot), 'OWN0015')
  }),
)

it.effect('consumes the whole aggregate when its take-once Effect runs', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/whole-owner-move',
      `${declarations}fn runTwice<F: once Effect<i32>>(value: Once<F>) -> i32 {
  let first = run value.operation
  return first + run value.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.include(codesOf(snapshot), 'OWN0001')
    const facts =
      snapshot.ownership.get('stored-effect-ownership/whole-owner-move') ??
      unreachable('expected ownership facts')
    const runTwice = facts.functions.at(0) ?? unreachable('expected runTwice ownership')
    const owner =
      runTwice.bindings.find((binding) => binding.name === 'value') ??
      unreachable('expected value binding')
    assert.notStrictEqual(owner.movedAt, undefined)
  }),
)

it.effect('consumes the outer owner through a nested stored Effect projection', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/nested-owner-move',
      `${declarations}struct Boxed<F: once Effect<i32>> { inner: Once<F> }
fn runTwice<F: once Effect<i32>>(value: Boxed<F>) -> i32 {
  let first = run value.inner.operation
  return first + run value.inner.operation
}
pub fn main() -> i32 { return 0 }`,
    )

    assert.include(codesOf(snapshot), 'OWN0001')
    assert.notInclude(codesOf(snapshot), 'OWN0013')
  }),
)

it.effect('rejects extracting a represented Effect field directly', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stored-effect-ownership/extraction',
      `${declarations}pub fn main() -> i32 {
  let deferred = Once { operation: effect { return 42 } }
  let operation = move deferred.operation
  return 0
}`,
    )
    const diagnostic = Analysis.diagnostics(snapshot).find(
      (candidate) => candidate.code === 'OWN0013',
    )

    assert.strictEqual(diagnostic?.reason._tag, 'RepresentationFieldExtraction')
    assert.include(diagnostic?.message ?? '', 'executable representation')
    assert.notInclude(codesOf(snapshot), 'OWN0002')
  }),
)

it.effect('retains a stored Effect loan until the enclosing aggregate is consumed', () =>
  Effect.gen(function* () {
    const module = 'stored-effect-ownership/retained-loan'
    const source = `struct Deferred<F: Effect<i32>> { operation: F }
effect fn inspect(values: &[i32]) -> i32 { return values[0] }
pub fn main() -> i32 {
  let mut values = [1]
  let recipe = inspect(&values)
  let deferred = Deferred { operation: move recipe }
  values[0] = 2
  drop deferred
  return values[0]
}`
    const snapshot = yield* realized(module, source)
    const facts = snapshot.ownership.get(module) ?? unreachable('expected ownership facts')
    const loan = facts.functions.flatMap((fn) => [...fn.loans]).at(0)

    assert.notStrictEqual(loan, undefined)
    assert.strictEqual(loan?.origin, 'FixedArrayBorrow')
    assert.include(codesOf(snapshot), 'OWN0011')
    assert.strictEqual(
      loan === undefined ? undefined : source.slice(loan.endSpan.start, loan.endSpan.end).trim(),
      'drop deferred',
    )
  }),
)
