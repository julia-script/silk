import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * `Effect.ensuring` runs a finalizer after the protected Effect completes, whatever the outcome,
 * and hands on the original outcome unchanged.
 *
 * Two things are observable here and both matter. The returned value carries the outcome: on
 * success it is the success value, and on typed failure it is the failure's own `code` payload
 * read back out by the recovery handler, so a replaced or re-wrapped failure would change it. The
 * evaluator's allocation trace carries the ordering: every owner in these programs is one heap
 * block, so acquiring and releasing it are both events, and the position of the finalizer's own
 * events relative to the protected Effect's local cleanup is what pins the LIFO order.
 */
const prelude = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
struct Problem { code: i32 }

struct Clock { storage: Allocation }

effect fn openClock() -> Clock ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  return Clock { storage: move allocation }
}

effect fn exhausted(error: OutOfMemoryError) -> Clock ! Problem { fail Problem { code: 9 } }

/// One owner, acquired inside whichever body runs this and released by that body's cleanup.
effect fn acquireClock() -> Clock ! Problem {
  return run Effect.catchAll(openClock(), exhausted)
}

effect fn release() -> () ! OutOfMemoryError {
  let held = run openClock()
  drop move held
  return ()
}

effect fn ignore(error: OutOfMemoryError) -> () { return () }

/// A fallible release recovered into the finalizer's infallible contract.
///
/// This is the composition the \`! never\` finalizer type is meant to force: the release itself may
/// fail, and the caller decides — here by ignoring it — what a failed release means, before the
/// result is handed to \`ensuring\`. The combinator never sees a second outcome to reconcile with
/// the one it is preserving.
effect fn finalize() -> () {
  return run Effect.catchAll(release(), ignore)
}

/// The same shape holding two owners, so the finalizer's events are a nested pair rather than a
/// second copy of the protected Effect's single pair — that asymmetry is what makes the ordering
/// assertion unambiguous rather than a trace that reads the same in either order.
effect fn releasePair() -> () ! OutOfMemoryError {
  let first = run openClock()
  let second = run openClock()
  drop move second
  drop move first
  return ()
}

effect fn finalizePair() -> () {
  return run Effect.catchAll(releasePair(), ignore)
}

/// Reads the failure's own payload back out, so the asserted value is evidence about *which*
/// failure arrived rather than merely that some failure did.
effect fn recover(problem: Problem) -> i32 { return problem.code }`

/** The finalizer runs after a success, and the success value reaches the caller unchanged. */
const succeeding = `import silk.effect { Effect }
${prelude}

effect fn work() -> i32 ! Problem { return 5 }

pub fn main() -> i32 {
  let guarded = Effect.ensuring(work(), finalize())
  return run Effect.catchAll(move guarded, recover)
}`

/** The finalizer runs after a typed failure, and that same failure reaches the caller. */
const failing = `import silk.effect { Effect }
${prelude}

effect fn work() -> i32 ! Problem { fail Problem { code: 3 } }

pub fn main() -> i32 {
  let guarded = Effect.ensuring(work(), finalize())
  return run Effect.catchAll(move guarded, recover)
}`

/**
 * The protected Effect holds one owner across a failing run and the finalizer holds two, so the
 * trace distinguishes the two bodies: `acquire release` for the protected Effect's local cleanup,
 * then `acquire acquire release release` for the finalizer. The finalizer running first would
 * read `acquire acquire release release acquire release` instead.
 */
const ordering = `import silk.effect { Effect }
${prelude}

effect fn work() -> i32 ! Problem {
  let held = run acquireClock()
  fail Problem { code: 3 }
}

pub fn main() -> i32 {
  let guarded = Effect.ensuring(work(), finalizePair())
  return run Effect.catchAll(move guarded, recover)
}`

/**
 * The #68 shape under `ensuring`: an owner *acquired inside* the protected body, held across a
 * fallible run, and stranded there by the failure unless the propagation exit releases it. #68
 * named this combinator as the one that would reintroduce that leak, so the balance is asserted
 * over both bodies at once — the protected Effect's owner and the finalizer's own.
 */
const acquiringThenFailing = `import silk.effect { Effect }
${prelude}

effect fn work() -> i32 ! Problem {
  let held = run acquireClock()
  fail Problem { code: 3 }
}

pub fn main() -> i32 {
  let guarded = Effect.ensuring(work(), finalize())
  return run Effect.catchAll(move guarded, recover)
}`

/** The same body on the succeeding path, where the release was never in doubt. */
const acquiringThenSucceeding = `import silk.effect { Effect }
${prelude}

effect fn work() -> i32 ! Problem {
  let held = run acquireClock()
  return 5
}

pub fn main() -> i32 {
  let guarded = Effect.ensuring(work(), finalize())
  return run Effect.catchAll(move guarded, recover)
}`

/** A release that genuinely fails, recovered by the caller before it reaches `ensuring`. */
const fallibleRelease = `import silk.effect { Effect }
${prelude}

effect fn work() -> i32 ! Problem { fail Problem { code: 3 } }

effect fn stubborn() -> () ! Problem { fail Problem { code: 8 } }

effect fn swallow(problem: Problem) -> () { return () }

effect fn tolerant() -> () { return run Effect.catchAll(stubborn(), swallow) }

pub fn main() -> i32 {
  let guarded = Effect.ensuring(work(), tolerant())
  return run Effect.catchAll(move guarded, recover)
}`

/**
 * A trap inside the protected Effect. Traps bypass everything — `Effect.catch`, Drop hooks, and
 * now the finalizer — so the run blocks with the trap and the finalizer never starts.
 */
const trapping = `import silk.effect { Effect }
${prelude}

effect fn work(divisor: i32) -> i32 ! Problem {
  let held = run acquireClock()
  return 5 / divisor
}

pub fn main() -> i32 {
  let guarded = Effect.ensuring(work(0), finalize())
  return run Effect.catchAll(move guarded, recover)
}`

const allocationEvents = (
  run: ReturnType<typeof Analysis.evaluate>,
): ReadonlyArray<'AllocationAcquire' | 'AllocationRelease'> =>
  run.trace.flatMap((event) =>
    event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease' ? [event._tag] : [],
  )

const acquire = 'AllocationAcquire'
const release = 'AllocationRelease'

/**
 * The allocation trace is asserted on the evaluator because it is the only engine that publishes
 * one; Wasm is held to the observable result that trace predicts.
 */
const accept = (
  name: string,
  source: string,
  expected: number,
  expectedEvents: ReadonlyArray<string>,
) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `ensuring/${name}`,
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [], name)

    const evaluated = Analysis.evaluate(snapshot)
    // A second release of the same owner blocks with a trap rather than completing, so this
    // assertion is what fails first if the finalizer's exit duplicates the cleanup it follows.
    assert.strictEqual(evaluated._tag, 'Completed', name)
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, BigInt(expected), name)

    const events = allocationEvents(evaluated)
    assert.deepEqual(events, expectedEvents, `${name} acquire/release trace`)
    assert.strictEqual(
      events.filter((event) => event === acquire).length,
      events.filter((event) => event === release).length,
      `${name} acquires equal releases`,
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)
  })

it.effect('runs the finalizer after a success and preserves the success value', () =>
  accept('succeeding', succeeding, 5, [acquire, release]),
)

it.effect('runs the finalizer after a typed failure and preserves that failure', () =>
  accept('failing', failing, 3, [acquire, release]),
)

it.effect("runs the protected Effect's own local cleanup before the finalizer", () =>
  accept('ordering', ordering, 3, [acquire, release, acquire, acquire, release, release]),
)

it.effect('releases an owner acquired inside the protected Effect that then fails', () =>
  accept('acquiring-failing', acquiringThenFailing, 3, [acquire, release, acquire, release]),
)

it.effect('releases an owner acquired inside the protected Effect that then succeeds', () =>
  accept('acquiring-succeeding', acquiringThenSucceeding, 5, [acquire, release, acquire, release]),
)

it.effect('accepts a fallible release the caller has already recovered', () =>
  accept('fallible-release', fallibleRelease, 3, []),
)

/**
 * The acquire and release counts agree once the finalizer has contributed its own, which is the
 * statement #68 is about: the combinator adds a second body's cleanup without stranding the first
 * body's owner or releasing it twice.
 */
it.effect('balances acquires and releases across the protected Effect and the finalizer', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'ensuring/balance',
      ascii(acquiringThenFailing),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    const events = allocationEvents(evaluated)
    const acquired = events.filter((event) => event === acquire).length
    const released = events.filter((event) => event === release).length
    assert.strictEqual(acquired, 2, 'the protected Effect and the finalizer each acquire one')
    assert.strictEqual(released, acquired, 'every acquired owner is released')
  }),
)

/**
 * Requirement 6. The protected Effect acquires its owner and then divides by zero: the run blocks
 * with the trap, and the trace stops at that acquire — no cleanup for the stranded owner and, the
 * point of this test, no finalizer events at all.
 */
it.effect('lets a trap bypass the finalizer', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'ensuring/trapping',
      ascii(trapping),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Trap')
    if (evaluated._tag !== 'Trap') return
    // One acquire and nothing after it: the owner the trap stranded, and no finalizer pair.
    assert.deepEqual(allocationEvents(evaluated), [acquire])
  }),
)
