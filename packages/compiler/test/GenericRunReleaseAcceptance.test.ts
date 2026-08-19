import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * The provider owns one heap block, so its acquisition and its release are both visible in the
 * evaluator's allocation trace: an owner that outlived the run holding it shows up as an acquire
 * with no matching release, and one released twice traps instead of completing.
 */
const provider = `struct Clock { storage: Allocation }

effect fn openClock() -> Clock ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  return Clock { storage: move allocation }
}`

/**
 * A generic effect body whose failure channel is a row *parameter*, holding an affine owner across
 * a run that the parameter lets fail. `Ownership.fallibleRunSites` treats such a run as infallible,
 * because it reads the run's own `failures` list, which is empty here while `failureParameters` is
 * not — so no propagation exit is published for it.
 *
 * Issue #68 reads that as a leak in every body of this shape. Measured, this one is not: the owner
 * is released on the failing path as well as the succeeding one, and the counts balance. The two
 * tests below pin that, because the obvious repair — counting an open failure row as fallible —
 * publishes a *second* release for an owner that already had one and turns the failing case into a
 * trap. Nothing else in the suite covers this shape, so without these a double-release change
 * passes 1078 tests untouched.
 */
const generic = `${provider}

effect fn holding<A, !E>(self: once Effect<A ! E>, held: once Clock) -> A ! E {
  let value = run move self
  drop move held
  return move value
}`

/** The specialized row really can fail, and the failing execution still releases the owner. */
const failingRun = `${generic}

effect fn failing() -> i32 ! OutOfMemory { fail OutOfMemory {} }

effect fn work() -> i32 ! OutOfMemory {
  let clock = run openClock()
  return run holding(failing(), move clock)
}

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(work(), recover) }`

/** The same body on the succeeding path, where the release is never in doubt. */
const succeedingRun = `${generic}

effect fn fine() -> i32 ! OutOfMemory { return 7 }

effect fn work() -> i32 ! OutOfMemory {
  let clock = run openClock()
  return run holding(fine(), move clock)
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

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

const accept = (name: string, source: string, expected: number) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `generic-run/${name}`,
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

    const events = allocationEvents(evaluated)
    assert.deepEqual(events, ['AllocationAcquire', 'AllocationRelease'], `${name} trace`)
    assert.strictEqual(
      events.filter((event) => event === 'AllocationAcquire').length,
      events.filter((event) => event === 'AllocationRelease').length,
      `${name} acquires equal releases`,
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)
  })

it.effect('releases an owner held across a failing run in a generic effect body', () =>
  accept('failing', failingRun, 7),
)

it.effect('releases an owner held across a succeeding run in a generic effect body', () =>
  accept('succeeding', succeedingRun, 7),
)
