import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * The provider owns one heap block, so acquiring it and releasing it are both logically visible:
 * the acquisition count is the count of `AllocationAcquire`, and a provider that outlived the
 * execution which acquired it would show up as an acquire without its release.
 */
const provider = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
service Clock { effect fn value() -> i32 ? &mut Clock }
struct FixedClock { storage: Allocation }
effect fn clockValue(self: &mut FixedClock) -> i32 { return 0 }
impl Clock for FixedClock { value: FixedClock.clockValue }

effect fn openClock() -> FixedClock ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorService()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  return FixedClock { storage: move allocation }
}`

/** Two executions of one provided Effect: each must acquire and release its own provider. */
const twoExecutions = `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
${provider}

effect fn read() -> i32 ! OutOfMemoryError ? &mut Clock { return 21 }

effect fn work() -> i32 ! OutOfMemoryError {
  let provided = read() |> Effect.provideEffect(openClock())
  let first = run provided
  let second = run provided
  return first + second
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(work(), recover) }`

/**
 * Three failing attempts under `Effect.retry`. Every attempt fails inside the provided Effect,
 * which is the path that previously carried the acquired provider out of the frame without
 * releasing it, so the retry is also the regression test for the typed-failure release.
 */
const threeAttempts = `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
${provider}

effect fn read() -> i32 ! OutOfMemoryError ? &mut Clock { fail OutOfMemoryError {} }

effect fn work() -> i32 ! OutOfMemoryError {
  let provided = read() |> Effect.provideEffect(openClock())
  let retried = provided |> Effect.retry(2)
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

/**
 * The logical acquire/release trace is asserted on the evaluator because it is the only engine
 * that publishes one; Wasm is held to the observable result that trace predicts.
 */
const acceptAcrossEngines = (
  name: string,
  source: string,
  expected: number,
  expectedEvents: ReadonlyArray<string>,
) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `provide-with/${name}`,
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [], name)

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', name)
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, BigInt(expected), name)
    assert.deepEqual(allocationEvents(evaluated), expectedEvents, `${name} acquire/release trace`)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `${name} wasm`)
  })

it.effect('acquires one provider for each execution of a provideEffect Effect', () =>
  acceptAcrossEngines('two-executions', twoExecutions, 42, [
    'AllocationAcquire',
    'AllocationRelease',
    'AllocationAcquire',
    'AllocationRelease',
  ]),
)

/**
 * The releases interleave with the acquisitions rather than trailing them, which is the stronger
 * statement: each attempt had finished with its own provider before the next attempt acquired.
 */
it.effect('gives every retry attempt its own provider and releases each one in turn', () =>
  acceptAcrossEngines('three-attempts', threeAttempts, 42, [
    'AllocationAcquire',
    'AllocationRelease',
    'AllocationAcquire',
    'AllocationRelease',
    'AllocationAcquire',
    'AllocationRelease',
  ]),
)

/** The acquire count and the release count agree after three attempts, with nothing left over. */
it.effect('balances acquired and released providers after three attempts', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'provide-with/balance',
      ascii(threeAttempts),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    const events = allocationEvents(evaluated)
    const acquired = events.filter((event) => event === 'AllocationAcquire').length
    const released = events.filter((event) => event === 'AllocationRelease').length
    assert.strictEqual(acquired, 3, 'three attempts acquire three providers')
    assert.strictEqual(released, acquired, 'every acquired provider is released')
  }),
)
