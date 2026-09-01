import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** A user-authored allocator that always refuses, exercising the failure half of dispatch. */
const refusing = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
import silk.layout { Layout }
struct ExhaustedAllocator { tag: i32 }

effect fn allocate(self: &mut ExhaustedAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}

impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.allocate }

effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = ExhaustedAllocator { tag: 0 }
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  drop allocation
  return 1
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catchAll(store(), recover)
}`

/** A user-authored allocator that hands out real system blocks, exercising the success half. */
const delegating = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct QuotaAllocator { tag: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  let mut inner = Allocator.systemAllocatorProvider()
  let recipe = Effect.provideMut(Allocator.allocate(move layout), &mut inner)
  let block = run recipe
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { tag: 0 }
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  drop allocation
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catchAll(store(), recover)
}`

it.effect('dispatches provision through user allocator witnesses on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    // Failure half: the witness runs, its OutOfMemoryError reaches the catch, and no block exists.
    const refused = yield* Analysis.ofSourceRealized(
      'owned-allocation-dispatch/refusing',
      ascii(refusing),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(refused), [])
    const refusedRun = Analysis.evaluate(refused)
    assert.strictEqual(refusedRun._tag, 'Completed')
    if (refusedRun._tag !== 'Completed') return
    assert.strictEqual(refusedRun.result.value, 7n)
    assert.isTrue(
      refusedRun.trace.some(
        (event) => event._tag === 'Call' && event.target.name.startsWith('allocate'),
      ),
    )
    assert.deepEqual(Projections.allocationTraceEventsOf(refusedRun), [])

    // Success half: the witness delegates to the system provider and the block still releases
    // exactly once, with the ticket owned independently of either provider.
    const delegated = yield* Analysis.ofSourceRealized(
      'owned-allocation-dispatch/delegating',
      ascii(delegating),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(delegated), [])
    const delegatedRun = Analysis.evaluate(delegated)
    assert.strictEqual(
      delegatedRun._tag,
      'Completed',
      Json.stringify(delegatedRun, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    if (delegatedRun._tag !== 'Completed') return
    assert.strictEqual(delegatedRun.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(delegatedRun).map((event) => event._tag),
      ['AllocationAcquire', 'AllocationRelease'],
    )

    for (const [name, source, expected] of [
      ['refusing', refusing, 7],
      ['delegating', delegating, 42],
    ] as const) {
      const wasm = yield* Analysis.codegenWasm(
        yield* Analysis.ofSourceRealized(
          `owned-allocation-dispatch/${name}`,
          ascii(source),
          'wasm32-unknown-unknown',
        ),
        { mode: 'release' },
      )
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), expected, name)
    }
  }),
)

const ordinalProgram = (
  providers: readonly [string, string],
): string => `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct ExhaustedAllocator { tag: i32 }

effect fn allocate(self: &mut ExhaustedAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}

impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut good = Allocator.systemAllocatorProvider()
  let mut empty = ExhaustedAllocator { tag: 0 }
  let first = Layout.of<[i32; 2]>()
  let recipeA = Allocator.allocate(move first) |> Effect.provideMut(&mut ${providers[0]})
  let a = run recipeA
  let second = Layout.of<[i32; 2]>()
  let recipeB = Allocator.allocate(move second) |> Effect.provideMut(&mut ${providers[1]})
  let b = run recipeB
  drop a
  drop b
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}`

it.effect('sweeps allocation failure ordinals with atomic rejection and unchanged failure', () =>
  Effect.gen(function* () {
    for (const [name, providers, expected, acquiresBeforeFailure] of [
      ['fail-at-0', ['empty', 'good'], 7, 0],
      ['fail-at-1', ['good', 'empty'], 7, 1],
      ['no-failure', ['good', 'good'], 42, 2],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `owned-allocation-ordinals/${name}`,
        ascii(ordinalProgram(providers)),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', name)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, BigInt(expected), name)

      const events = Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag)
      // Atomic rejection: a refused request acquires nothing, and every request before the
      // failing ordinal acquired exactly one block.
      assert.strictEqual(
        events.filter((event) => event === 'AllocationAcquire').length,
        acquiresBeforeFailure,
        name,
      )
      // Release once on every ordinal: earlier live owners release before the failure
      // propagates, and the successful sweep releases exactly what it acquired.
      assert.strictEqual(
        events.filter((event) => event === 'AllocationRelease').length,
        acquiresBeforeFailure,
        name,
      )

      // A failure leaves the process reusable: the same snapshot evaluates identically again.
      const again = Analysis.evaluate(snapshot)
      assert.strictEqual(again._tag, 'Completed', name)
      if (again._tag === 'Completed') assert.strictEqual(again.result.value, BigInt(expected), name)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), expected, name)
    }
  }),
)

const countedQuota = (quota: number): string => `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorProvider()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  let block = run recipe
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: ${quota} }
  let first = Layout.of<[i32; 2]>()
  let recipeA = Allocator.allocate(move first) |> Effect.provideMut(&mut allocator)
  let a = run recipeA
  let second = Layout.of<[i32; 2]>()
  let recipeB = Allocator.allocate(move second) |> Effect.provideMut(&mut allocator)
  let b = run recipeB
  drop a
  drop b
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}`

/**
 * The counted quota allocator is the change's canonical user-authored provider: its state
 * decrements through the exclusive self reference, so exhaustion is a property of the provider
 * value rather than of the call site. Every quota agrees across the evaluator and Wasm.
 */
it.effect('runs a counted quota allocator identically on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    for (const [quota, expected] of [
      [0, 7],
      [1, 7],
      [2, 42],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `owned-allocation-quota/q${quota}`,
        ascii(countedQuota(quota)),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], `q${quota}`)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', `q${quota}`)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, BigInt(expected), `q${quota}`)
      const events = Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag)
      assert.strictEqual(
        events.filter((event) => event === 'AllocationAcquire').length,
        Math.min(quota, 2),
        `q${quota}`,
      )
      assert.strictEqual(
        events.filter((event) => event === 'AllocationRelease').length,
        Math.min(quota, 2),
        `q${quota}`,
      )

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), expected, `q${quota}`)
    }
  }),
)

const forwardedProvider = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct CountingAllocator { hits: i32 }

effect fn allocate(self: &mut CountingAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  self.hits = self.hits + 1
  let mut inner = Allocator.systemAllocatorProvider()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  let block = run pending
  return move block
}

impl Allocator for CountingAllocator { allocate: CountingAllocator.allocate }

effect fn allocateForwarded(layout: Layout) -> Allocation ! OutOfMemoryError ? &mut Allocator {
  let pending = Allocator.allocate(move layout)
  let block = run pending
  return move block
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = CountingAllocator { hits: 0 }
  let layout = Layout.of<[i32; 2]>()
  let pending = allocateForwarded(move layout) |> Effect.provideMut(&mut allocator)
  let block = run pending
  drop block
  return allocator.hits
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}`

it.effect('writes forwarded exclusive provider mutations back on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'owned-allocation-dispatch/forwarded-provider',
      ascii(forwardedProvider),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      Json.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 1n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      ['AllocationAcquire', 'AllocationRelease'],
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 1)
  }),
)
