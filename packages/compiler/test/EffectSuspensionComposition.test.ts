import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import {
  auditAllocatorSuspension,
  ownedAllocatorSuspensionFailure,
  ownedAllocatorSuspensionSuccess,
} from './support/ownedAllocatorSuspension.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const mapSource = `fn addForty(value: i32) -> i32 { return value + 40 }
effect fn mapped() -> i32 ! OutOfMemory ? &mut Allocator {
  let pending = Effect.suspend(effect { return 2 }) |> Effect.map(addForty)
  return run pending
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(mapped() |> Effect.provideMut(&mut allocator), recover)
}`

const flatMapSource = `effect fn selected(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return value + 40 })
}
effect fn chained() -> i32 ! OutOfMemory ? &mut Allocator {
  let first = Effect.suspend(effect { return 2 })
  return run (move first |> Effect.flatMap(selected))
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(chained() |> Effect.provideMut(&mut allocator), recover)
}`

const recoverySource = `struct Problem { code: i32 }
effect fn failing() -> i32 ! Problem { fail Problem { code: 42 } }
effect fn protected() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  return run Effect.suspend(failing())
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(protected() |> Effect.provideMut(&mut allocator), recover)
}`

const retrySource = `struct Problem { code: i32 }
effect fn attempt() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let observed = run Effect.suspend(effect { return 1 })
  fail Problem { code: observed }
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(
    attempt() |> Effect.retry(2) |> Effect.provideMut(&mut allocator),
    recover
  )
}`

const provisionSource = `service Counter { effect fn get() -> i32 ? &Counter }
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
impl Counter for Fixed { get: Fixed.get }
effect fn provided() -> i32 ! OutOfMemory ? &Counter | &mut Allocator {
  let ready = run Effect.suspend(effect { return 0 })
  if ready == 0 { return run Counter.get() }
  return ready
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let fixed = Fixed { value: 42 }
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(
    provided() |> Effect.provide(&fixed) |> Effect.provideMut(&mut allocator),
    recover
  )
}`

const mutualSource = `effect fn even(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  if value == 0 { return 42 }
  let next = run Effect.suspend(effect { return value - 1 })
  return run odd(next)
}
effect fn odd(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  if value == 0 { return 41 }
  let next = run Effect.suspend(effect { return value - 1 })
  return run even(next)
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(even(20) |> Effect.provideMut(&mut allocator), recover)
}`

const allocatorRefusalSource = `struct ExhaustedAllocator { tag: i32 }
effect fn allocate(self: &mut ExhaustedAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  fail OutOfMemory {}
}
impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.allocate }
effect fn protected() -> i32 ! OutOfMemory ? &mut Allocator {
  let value = run Effect.suspend(effect { return 42 })
  return value
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = ExhaustedAllocator { tag: 0 }
  return run Effect.catchAll(protected() |> Effect.provideMut(&mut allocator), recover)
}`

const laterAllocatorRefusalSource = `struct QuotaAllocator { remaining: i32 }
effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  if self.remaining == 0 { fail OutOfMemory {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  return run Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
}
impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }
struct Guard { storage: Allocation }
impl Drop for Guard { fn drop(self: &mut Guard) -> () { return () } }
effect fn protected() -> i32 ! OutOfMemory ? &mut Allocator {
  let layout = Layout.of<i32>()
  let mut sourceAllocator = SystemAllocator.make()
  let storagePending = Allocator.allocate(move layout) |> Effect.provideMut(&mut sourceAllocator)
  let storage = run storagePending
  let guard = Guard { storage: move storage }
  let value = run Effect.suspend(effect { return 42 })
  return value
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = QuotaAllocator { remaining: 2 }
  return run Effect.catchAll(protected() |> Effect.provideMut(&mut allocator), recover)
}`

const cleanupSource = (failAtBase: boolean): string => `struct Problem { code: i32 }
struct Guard { storage: Allocation }
impl Drop for Guard { fn drop(self: &mut Guard) -> () { return () } }
effect fn descend(value: i32) -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout)
  let guard = Guard { storage: move storage }
  if value == 0 {
    ${failAtBase ? 'fail Problem { code: 7 }' : 'return 0'}
  }
  let next = run Effect.suspend(effect { return value - 1 })
  return 1 + run descend(next)
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(descend(3) |> Effect.provideMut(&mut allocator), recover)
}`

const trapSource = `struct Guard { storage: Allocation }
impl Drop for Guard { fn drop(self: &mut Guard) -> () { return () } }
effect fn protected() -> i32 ! OutOfMemory ? &mut Allocator {
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout)
  let guard = Guard { storage: move storage }
  return run Effect.suspend(effect { return i32.divide(1, 0) })
}
effect fn recover(error: OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(protected() |> Effect.provideMut(&mut allocator), recover)
}`

const runAll = Effect.fnUntraced(function* (name: string, source: string, expected: number) {
  const logical = yield* Analysis.ofSourceRealized(
    `suspension-composition/${name}-logical`,
    ascii(source),
    'aarch64-apple-darwin',
  )
  const wasm = yield* Analysis.ofSourceRealized(
    `suspension-composition/${name}-wasm`,
    ascii(source),
    'wasm32-unknown-unknown',
  )
  assert.deepEqual(Analysis.diagnostics(logical), [], `${name} logical`)
  assert.deepEqual(Analysis.diagnostics(wasm), [], `${name} wasm`)

  if (name.startsWith('owned-allocator')) {
    const catches = Analysis.loweredMir(logical).functions.filter((fn) =>
      fn.instance.contractRow.some(
        (entry) => entry.startsWith('effect-site:') && entry.includes('site:107374'),
      ),
    )
    assert.isAbove(catches.length, 0, `${name}: generated catch runner`)
    assert.isTrue(
      catches.some(
        (fn) =>
          fn.suspension?.classification === 'Suspendable' &&
          fn.suspension.regions.length > 0 &&
          fn.suspension.regions.some((region) =>
            region._tag === 'RunSuspendableEffectRegion'
              ? region.runner.providers.some(
                  (provider) => provider.access === 'Take' && provider.purposes.length === 2,
                )
              : false,
          ),
      ),
      `${name}: Take allocator reaches generated catch suspension`,
    )
  }

  for (const fn of Analysis.loweredMir(logical).functions) {
    for (const region of fn.suspension?.regions ?? []) {
      const runner =
        region._tag === 'RunSuspendableEffectRegion'
          ? region.runner
          : region._tag === 'SuspendEffectRegion'
            ? region.deferred
            : undefined
      for (const provider of runner?.providers ?? []) {
        if (provider.purposes.length !== 2) continue
        assert.strictEqual(provider.capability.module, 'silk/core', `${name}: allocator module`)
        assert.strictEqual(provider.capability.name, 'Allocator', `${name}: allocator capability`)
        assert.strictEqual(provider.role, 'DefaultRole', `${name}: allocator role`)
        assert.strictEqual(provider.requirementAccess, 'Exclusive', `${name}: allocator demand`)
        assert.include(['Exclusive', 'Take'], provider.access, `${name}: allocator strength`)
      }
    }
  }

  if (name === 'audit-allocator-role') {
    const layout = Analysis.layoutOf(logical)
    assert.strictEqual(layout._tag, 'Available', `${name}: layout available`)
    const auditDemands =
      layout._tag === 'Available'
        ? layout.value.effectEnvironments.flatMap((environment) =>
            environment._tag === 'EffectEnvironment'
              ? environment.fields.flatMap((field) =>
                  field.providedRequirement?.role.endsWith('Audit') === true
                    ? [field.providedRequirement]
                    : [],
                )
              : [],
          )
        : []
    assert.isAtLeast(auditDemands.length, 2, `${name}: exact Audit bindings retained`)
    assert.isTrue(
      auditDemands.every((demand) => demand.requirementAccess === 'Shared'),
      `${name}: Audit bindings retain shared stored access`,
    )
    assert.include(
      auditDemands.map((demand) => demand.providerAccess),
      'Shared',
      `${name}: shared Audit provider strength`,
    )
    assert.include(
      auditDemands.map((demand) => demand.providerAccess),
      'Exclusive',
      `${name}: exclusive Audit provider strength`,
    )
    const continuationProviders = Analysis.loweredMir(logical).functions.flatMap((fn) =>
      (fn.suspension?.regions ?? []).flatMap((region) => {
        const runner =
          region._tag === 'RunSuspendableEffectRegion'
            ? region.runner
            : region._tag === 'SuspendEffectRegion'
              ? region.deferred
              : undefined
        return runner?.providers.filter((provider) => provider.purposes.length === 2) ?? []
      }),
    )
    assert.isAbove(
      continuationProviders.length,
      0,
      `${name}: real default continuation Allocator retained`,
    )
    assert.isTrue(
      continuationProviders.some(
        (provider) =>
          provider.providerType.module === 'silk/core' &&
          provider.providerType.name === 'SystemAllocator',
      ),
      `${name}: SystemAllocator supplies continuation storage`,
    )
  }

  const evaluated = Analysis.evaluate(logical, { maxCallDepth: 1_024, maxSteps: 1_000_000 })
  assert.strictEqual(evaluated._tag, 'Completed', `${name}: ${evaluated._tag}`)
  if (evaluated._tag === 'Completed')
    assert.strictEqual(evaluated.result.value, BigInt(expected), name)

  const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(wasmArtifact.bytes.slice()), {})
  const wasmMain = instance.exports.silk_main
  assert.strictEqual(typeof wasmMain, 'function')
  if (typeof wasmMain === 'function') assert.strictEqual(wasmMain(), expected, `${name} wasm`)
})

it.effect('resumes Effect.map after a suspended source', () => runAll('map', mapSource, 42))

it.effect('resumes a suspended Effect selected by flatMap', () =>
  runAll('flat-map', flatMapSource, 42),
)

it.effect('reifies and recovers a suspended typed failure', () =>
  runAll('recovery', recoverySource, 42),
)

it.effect('retries a reusable Effect across suspension', () => runAll('retry', retrySource, 7))

it.effect('retains a provided service across suspension', () =>
  runAll('provision', provisionSource, 42),
)

it.effect('retains and releases a Take-selected affine allocator across suspension', () =>
  Effect.gen(function* () {
    yield* runAll('owned-allocator-success', ownedAllocatorSuspensionSuccess, 42)
    yield* runAll('owned-allocator-failure', ownedAllocatorSuspensionFailure, 7)
  }),
)

it.effect('uses only the exact default exclusive Allocator demand for continuation storage', () =>
  runAll('audit-allocator-role', auditAllocatorSuspension, 42),
)

it.effect('runs suspended mutual recursion on the evaluator and Wasm', () =>
  runAll('mutual', mutualSource, 42),
)

it.effect('preserves selected allocator refusal on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSourceRealized(
      'suspension-composition/allocator-refusal-trace',
      ascii(allocatorRefusalSource),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(logical), [])
    const evaluated = Analysis.evaluate(logical)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') {
      assert.strictEqual(evaluated.result.value, 7n)
      assert.lengthOf(
        evaluated.trace.filter((event) => event._tag === 'ContinuationRequest'),
        1,
      )
      assert.lengthOf(
        evaluated.trace.filter((event) => event._tag === 'ContinuationReject'),
        1,
      )
      assert.lengthOf(
        evaluated.trace.filter((event) => event._tag === 'SuspensionChildStart'),
        0,
      )
    }
    yield* runAll('allocator-refusal', allocatorRefusalSource, 7)
  }),
)

it.effect('rolls back an accepted inner frame when a later allocator request refuses', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSourceRealized(
      'suspension-composition/later-allocator-refusal-trace',
      ascii(laterAllocatorRefusalSource),
      'aarch64-apple-darwin',
    )
    const evaluated = Analysis.evaluate(logical)
    assert.deepEqual(Analysis.diagnostics(logical), [])
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') {
      assert.strictEqual(evaluated.result.value, 7n)
      assert.isAtLeast(
        evaluated.trace.filter((event) => event._tag === 'ContinuationRequest').length,
        2,
        JSON.stringify(
          evaluated.trace.filter((event) =>
            [
              'ContinuationRequest',
              'ContinuationAcquire',
              'ContinuationReject',
              'AllocationAcquire',
              'AllocationRelease',
              'SuspensionChildStart',
            ].includes(event._tag),
          ),
        ),
      )
      assert.lengthOf(
        evaluated.trace.filter((event) => event._tag === 'ContinuationReject'),
        1,
      )
      assert.lengthOf(
        evaluated.trace.filter((event) => event._tag === 'AllocationRelease'),
        evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length,
      )
    }
    yield* runAll('later-allocator-refusal', laterAllocatorRefusalSource, 7)
  }),
)

const assertCleanupTrace = (source: string, expected: number) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `suspension-composition/cleanup-${expected}`,
      ascii(source),
      'aarch64-apple-darwin',
    )
    const evaluated = Analysis.evaluate(snapshot, { maxSteps: 1_000_000 })
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, BigInt(expected))
    const hooks = evaluated.trace
      .map((event, index) => ({ event, index }))
      .filter(({ event }) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'))
    assert.lengthOf(hooks, 4)
    const sourceReclaims = evaluated.trace
      .map((event, index) => ({ event, index }))
      .filter(
        ({ event }) =>
          event._tag === 'ContinuationRelease' && event.function.name.startsWith('descend$effect$'),
      )
    assert.isAtLeast(sourceReclaims.length, 3)
    const hookReleaseTickets = hooks.map(({ index: hookIndex }) => {
      const release = evaluated.trace
        .slice(hookIndex + 1)
        .find((event) => event._tag === 'AllocationRelease')
      assert.isDefined(release)
      return release?._tag === 'AllocationRelease' ? release.ticket : -1
    })
    assert.deepEqual(
      hookReleaseTickets,
      [...hookReleaseTickets].sort((a, b) => b - a),
    )
    let lastHook = -1
    const finalSourceReclaims = sourceReclaims.filter(({ index: reclaimIndex }) => {
      const hookIndex = evaluated.trace.findLastIndex(
        (event, index) =>
          index < reclaimIndex &&
          event._tag === 'Call' &&
          event.target.name.startsWith('drop@impl'),
      )
      if (hookIndex <= lastHook) return false
      lastHook = hookIndex
      return true
    })
    assert.lengthOf(finalSourceReclaims, 3)
    for (const { index: reclaimIndex } of finalSourceReclaims) {
      const hookIndex = evaluated.trace.findLastIndex(
        (event, index) =>
          index < reclaimIndex &&
          event._tag === 'Call' &&
          event.target.name.startsWith('drop@impl'),
      )
      const sourceReleaseIndex = evaluated.trace.findLastIndex(
        (event, index) => index < reclaimIndex && event._tag === 'AllocationRelease',
      )
      assert.isBelow(hookIndex, sourceReleaseIndex)
      assert.isBelow(sourceReleaseIndex, reclaimIndex)
      assert.strictEqual(evaluated.trace.at(reclaimIndex + 1)?._tag, 'AllocationRelease')
    }
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'ContinuationRelease'),
      evaluated.trace.filter((event) => event._tag === 'ContinuationAcquire').length,
    )
    assert.lengthOf(
      evaluated.trace.filter((event) => event._tag === 'AllocationRelease'),
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length,
    )
  })

it.effect('cleans deep suspended success inner-to-outer before continuation reclaim', () =>
  Effect.gen(function* () {
    const source = cleanupSource(false)
    yield* assertCleanupTrace(source, 3)
    yield* runAll('cleanup-success', source, 3)
  }),
)

it.effect('cleans deep suspended typed failure inner-to-outer before continuation reclaim', () =>
  Effect.gen(function* () {
    const source = cleanupSource(true)
    yield* assertCleanupTrace(source, 7)
    yield* runAll('cleanup-failure', source, 7)
  }),
)

it.effect('keeps traps outside source unwind while preserving engine trap behavior', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSourceRealized(
      'suspension-composition/trap-logical',
      ascii(trapSource),
      'aarch64-apple-darwin',
    )
    const evaluated = Analysis.evaluate(logical)
    assert.strictEqual(evaluated._tag, 'Blocked')
    if (evaluated._tag === 'Blocked') {
      assert.strictEqual(evaluated.reason._tag, 'Trap')
      assert.lengthOf(
        evaluated.trace.filter(
          (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
        ),
        0,
      )
      assert.lengthOf(
        evaluated.trace.filter((event) => event._tag === 'ContinuationRelease'),
        0,
      )
    }

    const wasm = yield* Analysis.ofSourceRealized(
      'suspension-composition/trap-wasm',
      ascii(trapSource),
      'wasm32-unknown-unknown',
    )
    const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(
      new WebAssembly.Module(wasmArtifact.bytes.slice()),
      {},
    )
    const wasmMain = instance.exports.silk_main
    assert.strictEqual(typeof wasmMain, 'function')
    if (typeof wasmMain === 'function') assert.throws(() => wasmMain(), WebAssembly.RuntimeError)
  }),
)
