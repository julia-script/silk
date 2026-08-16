import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** A user-authored allocator that always refuses, exercising the failure half of dispatch. */
const refusing = `struct ExhaustedAllocator { tag: i32 }

effect fn allocate(self: &mut ExhaustedAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  fail OutOfMemory {}
}

impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.allocate }

effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = ExhaustedAllocator { tag: 0 }
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  drop allocation
  return 1
}

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catch(store(), recover)
}`

/** A user-authored allocator that hands out real system blocks, exercising the success half. */
const delegating = `struct QuotaAllocator { tag: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  let mut inner = SystemAllocator.make()
  let recipe = Effect.provideMut(Allocator.allocate(move layout), &mut inner)
  let block = run recipe
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = QuotaAllocator { tag: 0 }
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  drop allocation
  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catch(store(), recover)
}`

it.effect('dispatches provision through user allocator witnesses on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    // Failure half: the witness runs, its OutOfMemory reaches the catch, and no block exists.
    const refused = yield* Analysis.ofSourceRealized(
      'owned-allocation-dispatch/refusing',
      ascii(refusing),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(refused), [])
    const refusedRun = Analysis.evaluate(refused)
    assert.strictEqual(refusedRun._tag, 'Completed')
    if (refusedRun._tag !== 'Completed') return
    assert.strictEqual(refusedRun.result.value, 7)
    assert.isTrue(
      refusedRun.trace.some(
        (event) => event._tag === 'Call' && event.target.name.startsWith('allocate'),
      ),
    )
    assert.deepEqual(Analysis.allocationTraceEventsOf(refusedRun), [])

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
      JSON.stringify(delegatedRun, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    if (delegatedRun._tag !== 'Completed') return
    assert.strictEqual(delegatedRun.result.value, 42)
    assert.deepEqual(
      Analysis.allocationTraceEventsOf(delegatedRun).map((event) => event._tag),
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
): string => `struct ExhaustedAllocator { tag: i32 }

effect fn allocate(self: &mut ExhaustedAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  fail OutOfMemory {}
}

impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.allocate }

effect fn build() -> i32 ! OutOfMemory {
  let mut good = SystemAllocator.make()
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

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catch(build(), recover)
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
      assert.strictEqual(evaluated.result.value, expected, name)

      const events = Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag)
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
      if (again._tag === 'Completed') assert.strictEqual(again.result.value, expected, name)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), expected, name)
    }
  }),
)

const countedQuota = (quota: number): string => `struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  if self.remaining == 0 { fail OutOfMemory {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  let block = run recipe
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn build() -> i32 ! OutOfMemory {
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

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catch(build(), recover)
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
      assert.strictEqual(evaluated.result.value, expected, `q${quota}`)
      const events = Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag)
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

const forwardedProvider = `struct CountingAllocator { hits: i32 }

effect fn allocate(self: &mut CountingAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  self.hits = self.hits + 1
  let mut inner = SystemAllocator.make()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  let block = run pending
  return move block
}

impl Allocator for CountingAllocator { allocate: CountingAllocator.allocate }

effect fn allocateForwarded(layout: Layout) -> Allocation ! OutOfMemory ? &mut Allocator {
  let pending = Allocator.allocate(move layout)
  let block = run pending
  return move block
}

effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = CountingAllocator { hits: 0 }
  let layout = Layout.of<[i32; 2]>()
  let pending = allocateForwarded(move layout) |> Effect.provideMut(&mut allocator)
  let block = run pending
  drop block
  return allocator.hits
}

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catch(build(), recover)
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
      JSON.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 1)
    assert.deepEqual(
      Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      ['AllocationAcquire', 'AllocationRelease'],
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 1)
  }),
)
