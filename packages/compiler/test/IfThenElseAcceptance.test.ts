import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * `Effect.ifThenElse` invokes exactly one of two suspended arms. Because the arms are
 * `once fn()` producing the branch rather than pre-built Effect values, the branch not taken is
 * never *constructed* — a stronger statement than "its effect was not performed", and the one the
 * #98 decision asked these tests to make.
 *
 * The counter service carries the execution half of that: each arm bumps the counter a different
 * number of times and reports a different value, so a single `i32` answer identifies both which
 * arm ran and how many times the service was called. `main` returns `value * 100 + cell.value`,
 * which is why the expected numbers below read as two fields rather than one.
 */
const counter = `service Counter {
  effect fn bump() -> i32 ? &mut Counter
}

struct Cell { value: i32 }

effect fn bump(self: &mut Cell) -> i32 {
  self.value = self.value + 1
  return self.value
}

impl Counter for Cell { bump: Cell.bump }

/// The true arm: one service call, reports 1.
effect fn bumpOnce() -> i32 ? &mut Counter {
  let first = run Counter.bump()
  return 1
}

/// The false arm: ten service calls, reports 2. The count differs from the true arm's by more than
/// one, so a trace where both arms ran is not mistakable for a trace where either ran alone.
effect fn bumpTen() -> i32 ? &mut Counter {
  let a = run Counter.bump()
  let b = run Counter.bump()
  let c = run Counter.bump()
  let d = run Counter.bump()
  let e = run Counter.bump()
  let f = run Counter.bump()
  let g = run Counter.bump()
  let h = run Counter.bump()
  let i = run Counter.bump()
  let j = run Counter.bump()
  return 2
}`

/** Effect fn arms: only the selected arm's Effect is constructed and run. */
const selecting = (condition: string) => `${counter}

pub fn main() -> i32 {
  let mut cell = Cell { value: 0 }
  let chosen = Effect.ifThenElse(${condition}, bumpOnce, bumpTen)
  let value = run Effect.provideMut(move chosen, &mut cell)
  return value * 100 + cell.value
}`

/**
 * The construction half. These arms are ordinary `fn`s, so their bodies execute at *invocation* —
 * before any Effect is run — and each allocates and releases a heap block a different number of
 * times. Every owner here is one heap block, so acquiring and releasing it are both trace events.
 *
 * That makes the assertion about construction rather than execution: an implementation taking
 * pre-built `Effect` arguments would evaluate both arms at the call site and produce both bodies'
 * allocation pairs whatever the condition, even though only one branch's *effect* would then run.
 */
const eager = (condition: string) => `${counter}

struct Clock { storage: Allocation }

effect fn openClock() -> Clock ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  return Clock { storage: move allocation }
}

effect fn release() -> () ! OutOfMemoryError {
  let held = run openClock()
  drop move held
  return ()
}

effect fn ignore(error: OutOfMemoryError) -> () { return () }

/// One acquire/release pair, recovered into an infallible contract so an ordinary \`fn\` can run it.
effect fn touch() -> () { return run Effect.catchAll(release(), ignore) }

/// Construction-time work: one pair, then the branch.
fn buildOnce() -> Effect<i32 ! never ? &mut Counter> {
  let touched = run touch()
  return bumpOnce()
}

/// Construction-time work: two pairs, then the branch. The counts differ, so the trace says which
/// arm was constructed rather than merely that something was.
fn buildTen() -> Effect<i32 ! never ? &mut Counter> {
  let touched = run touch()
  let again = run touch()
  return bumpTen()
}

pub fn main() -> i32 {
  let mut cell = Cell { value: 0 }
  let chosen = Effect.ifThenElse(${condition}, buildOnce, buildTen)
  let value = run Effect.provideMut(move chosen, &mut cell)
  return value * 100 + cell.value
}`

/** Distinct requirement rows on the two arms; the caller discharges both. */
const requirements = (condition: string) => `service Alpha { effect fn alpha() -> i32 ? &Alpha }
service Beta { effect fn beta() -> i32 ? &Beta }

struct Fixed { value: i32 }
effect fn alpha(self: &Fixed) -> i32 { return self.value }
effect fn beta(self: &Fixed) -> i32 { return self.value }
impl Alpha for Fixed { alpha: Fixed.alpha }
impl Beta for Fixed { beta: Fixed.beta }

effect fn fromAlpha() -> i32 ? &Alpha {
  let value = run Alpha.alpha()
  return value * 10
}

effect fn fromBeta() -> i32 ? &Beta {
  let value = run Beta.beta()
  return value * 50
}

pub fn main() -> i32 {
  let alphaProvider = Fixed { value: 3 }
  let betaProvider = Fixed { value: 4 }
  let chosen = Effect.ifThenElse(${condition}, fromAlpha, fromBeta)
  let provided = Effect.provide<Beta>(
    Effect.provide<Alpha>(move chosen, &alphaProvider),
    &betaProvider,
  )
  return run provided
}`

/**
 * A one-sided failure row: one arm can fail and the other cannot, so `! E | F` is a union with an
 * empty side. Selecting the fallible arm hands its failure to the caller's recovery; selecting the
 * infallible one produces no failure to recover.
 */
const failures = (condition: string) => `struct Problem { code: i32 }

effect fn risky() -> i32 ! Problem { fail Problem { code: 6 } }
effect fn safe() -> i32 { return 1 }

effect fn recover(problem: Problem) -> i32 { return problem.code }

pub fn main() -> i32 {
  let chosen = Effect.ifThenElse(${condition}, risky, safe)
  return run Effect.catchAll(move chosen, recover)
}`

/**
 * Runs one program on the evaluator and Wasm and asserts they agree. Native execution parity is
 * carried by the differential corpus in `DriverNativeAcceptance.test.ts`.
 *
 * `expectedEvents` is asserted on the evaluator because it is the only engine that publishes an
 * allocation trace; Wasm is held to the observable result that trace predicts.
 */
const accept = (
  name: string,
  source: string,
  expected: number,
  expectedEvents?: ReadonlyArray<string>,
) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `if-then-else/${name}`,
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [], name)

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      `${name}: ${JSON.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ).slice(0, 400)}`,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, BigInt(expected), `${name} evaluator`)

    if (expectedEvents !== undefined) {
      const events = evaluated.trace.flatMap((event) =>
        event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease'
          ? [event._tag]
          : [],
      )
      assert.deepEqual(events, expectedEvents, `${name} acquire/release trace`)
    }

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)
  })

const acquire = 'AllocationAcquire'
const release = 'AllocationRelease'

it.effect(
  "runs the true arm and performs none of the false arm's effects",
  // 1 from `bumpOnce`, and a counter of 1: the false arm's ten service calls never happened.
  () => accept('selecting-true', selecting('true'), 101),
  180_000,
)

it.effect(
  "runs the false arm and performs none of the true arm's effects",
  // 2 from `bumpTen`, and a counter of 10: the true arm's single service call never happened.
  () => accept('selecting-false', selecting('false'), 210),
  180_000,
)

/**
 * The reworded #98 criterion. The arms do their work at construction, so one acquire/release pair
 * is evidence that `buildOnce` was invoked and `buildTen` was not — the branch not taken was never
 * built, not merely never run.
 */
it.effect(
  'never constructs the false arm when the condition is true',
  () => accept('eager-true', eager('true'), 101, [acquire, release]),
  180_000,
)

it.effect(
  'never constructs the true arm when the condition is false',
  () => accept('eager-false', eager('false'), 210, [acquire, release, acquire, release]),
  180_000,
)

it.effect(
  'unions the requirement rows of both arms',
  () => accept('requirements-true', requirements('true'), 30),
  180_000,
)

it.effect(
  "discharges the unselected arm's requirement even though it never runs",
  () => accept('requirements-false', requirements('false'), 200),
  180_000,
)

it.effect(
  "propagates the selected arm's failure through the unioned row",
  () => accept('failures-true', failures('true'), 6),
  180_000,
)

it.effect(
  'produces no failure when the selected arm is the infallible one',
  () => accept('failures-false', failures('false'), 1),
  180_000,
)

/**
 * The #98 cleanup criterion, as reworded by the 2026-08-13 decision: it is about "an arm that
 * captures a resource and is never invoked". Under zero-arity arms no such arm exists to be
 * written, and this test is what discharges that claim rather than a leak assertion that cannot be
 * expressed.
 *
 * `bootstrap-callable-values` specifies that a section supplying "exactly parameters one through
 * the last" constructs "a unary callable awaiting parameter zero", so section construction always
 * leaves arity 1 and never 0; a named function has no environment at all. `hold(move clock)` is
 * therefore a full application producing an `Effect`, not a zero-arity callable holding the
 * `Clock`, and it is rejected against the arm's declared contract.
 */
it.effect('rejects a capturing value where a zero-arity arm is required', () =>
  Effect.gen(function* () {
    const source = `struct Clock { value: i32 }

effect fn hold(held: Clock) -> i32 { return held.value }

effect fn plain() -> i32 { return 5 }

pub fn main() -> i32 {
  let clock = Clock { value: 9 }
  return run Effect.ifThenElse(false, hold(move clock), plain)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'if-then-else/capturing-arm',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0052'],
    )
  }),
)

/**
 * The arms are `once`, so the combinator's contract holds each to a single invocation. Passing the
 * same reusable named function to both arms is still accepted — the weakening
 * `bootstrap-callable-values` permits — and selects normally.
 */
it.effect('accepts the same reusable function in both arms', () =>
  Effect.gen(function* () {
    const source = `effect fn same() -> i32 { return 8 }

pub fn main() -> i32 { return run Effect.ifThenElse(true, same, same) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'if-then-else/same-arm',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 8n)
  }),
)
