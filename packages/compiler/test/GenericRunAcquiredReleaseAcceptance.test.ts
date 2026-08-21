import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * The provider owns one heap block, so its acquisition and its release are both visible in the
 * evaluator's allocation trace: an owner stranded by a propagating failure shows up as an acquire
 * with no matching release, and one released twice traps instead of completing.
 */
const provider = `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct Clock { storage: Allocation }

effect fn openClock() -> Clock ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  return Clock { storage: move allocation }
}`

/**
 * A generic effect body that *acquires* an owner and then holds it across a run whose failure
 * channel is a row parameter — the shape `provideEffect` had before #67 restructured it around a
 * reified `Result`.
 *
 * This is the leak issue #68 describes. `fallibleRunSites` used to inspect only concrete failure
 * members, so a generic failure type counted as infallible, no propagation exit was published for
 * it, and `held` was stranded whenever the specialized type let `self` fail.
 *
 * The companion `GenericRunReleaseAcceptance` covers the neighbouring shape where the owner is
 * *passed in* rather than acquired; that one never leaked, and pins that the caller does not also
 * release an owner it moved into the callee.
 */
const generic = `import silk.core { OutOfMemoryError }
${provider}

effect fn acquiring<A, E>(self: once Effect<A ! E>) -> A ! E | OutOfMemoryError {
  let held = run openClock()
  let value = run move self
  drop move held
  return move value
}`

/** The specialized row really can fail, and the failing execution still releases the owner. */
const failingRun = `import silk.core { OutOfMemoryError }
import silk.effect as Effect
${generic}

effect fn failing() -> i32 ! OutOfMemoryError { fail OutOfMemoryError {} }

effect fn work() -> i32 ! OutOfMemoryError { return run acquiring(failing()) }

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(work(), recover) }`

/** The same body on the succeeding path, where the release was never in doubt. */
const succeedingRun = `import silk.core { OutOfMemoryError }
import silk.effect as Effect
${generic}

effect fn fine() -> i32 ! OutOfMemoryError { return 7 }

effect fn work() -> i32 ! OutOfMemoryError { return run acquiring(fine()) }

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(work(), recover) }`

/**
 * Three failing attempts under `Effect.retry`: every attempt acquires its own provider inside the
 * generic body and must release it before the next one acquires, so a per-execution leak shows up
 * as a growing imbalance rather than a single missing release.
 */
const retriedRun = `import silk.core { OutOfMemoryError }
import silk.effect as Effect
${generic}

effect fn failing() -> i32 ! OutOfMemoryError { fail OutOfMemoryError {} }

effect fn work() -> i32 ! OutOfMemoryError {
  let attempt = acquiring(failing())
  let retried = attempt |> Effect.retry(2)
  return run retried
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }

pub fn main() -> i32 { return run Effect.catchAll(work(), recover) }`

const allocationEvents = (
  run: ReturnType<typeof Analysis.evaluate>,
): ReadonlyArray<'AllocationAcquire' | 'AllocationRelease'> =>
  run._tag === 'Completed'
    ? run.trace.flatMap((event) =>
        event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease'
          ? [event._tag]
          : [],
      )
    : []

const accept = (
  name: string,
  source: string,
  expected: number,
  expectedEvents: ReadonlyArray<string>,
) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `generic-run-acquired/${name}`,
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [], name)

    const evaluated = Analysis.evaluate(snapshot)
    // A second release of the same owner blocks with a trap rather than completing, so this
    // assertion is what fails first if run-fallibility starts publishing redundant cleanup.
    assert.strictEqual(evaluated._tag, 'Completed', name)
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, BigInt(expected), name)

    // The interleaving is the stronger statement: each execution had finished with its own
    // provider before the next acquired, rather than the releases merely trailing the acquires.
    const events = allocationEvents(evaluated)
    assert.deepEqual(events, expectedEvents, `${name} acquire/release trace`)
    assert.strictEqual(
      events.filter((event) => event === 'AllocationAcquire').length,
      events.filter((event) => event === 'AllocationRelease').length,
      `${name} acquires equal releases`,
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)
  })

it.effect('releases an owner acquired inside a generic body across a failing run', () =>
  accept('failing', failingRun, 7, ['AllocationAcquire', 'AllocationRelease']),
)

it.effect('releases an owner acquired inside a generic body across a succeeding run', () =>
  accept('succeeding', succeedingRun, 7, ['AllocationAcquire', 'AllocationRelease']),
)

it.effect('gives every retried attempt of a generic body its own released owner', () =>
  accept('retried', retriedRun, 42, [
    'AllocationAcquire',
    'AllocationRelease',
    'AllocationAcquire',
    'AllocationRelease',
    'AllocationAcquire',
    'AllocationRelease',
  ]),
)
