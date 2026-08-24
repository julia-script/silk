import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import type * as CleanupPlan from '../src/CleanupPlan.js'
import * as CoroutineFrame from '../src/CoroutineFrame.js'
import * as Layout from '../src/Layout.js'
import * as Lower from '../src/Lower.js'
import * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirNormalization from '../src/MirNormalization.js'
import * as MirVerification from '../src/MirVerification.js'
import * as OpaqueRealization from '../src/OpaqueRealization.js'
import type * as Ownership from '../src/Ownership.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as SuspensionMir from '../src/SuspensionMir.js'
import * as SuspensionOwnership from '../src/SuspensionOwnership.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const lowerStored = Effect.fnUntraced(function* (name: string, source: string) {
  const snapshot = yield* Analysis.ofSourceRealized(
    name,
    ascii(source),
    Target.wasm32UnknownUnknown.id,
  )
  const catalog = Layout.catalog(Target.wasm32UnknownUnknown, snapshot.index, snapshot.instances)
  const layout = Layout.plan(catalog, snapshot.instances, snapshot.index)
  const ownership = Analysis.ownershipOf(snapshot, name) ?? unreachable('expected module ownership')
  const module = Lower.lowerProgram(
    snapshot.instances,
    new Map<string, Ownership.ModuleOwnership>([[name, ownership]]),
    layout,
    snapshot.index,
    OpaqueRealization.catalogOf(snapshot),
  )
  return Object.freeze({ snapshot, layout, module })
})

const finalizeSuspension = (
  snapshot: Analysis.Snapshot,
  module: Mir.Module,
  layout: Layout.Plan,
): Mir.Module => {
  const provisional = ProvisionalMir.build(snapshot.instances, layout, snapshot.index)
  const normalized = MirNormalization.normalize(module, provisional)
  return CoroutineFrame.apply(
    SuspensionMir.finalize(
      normalized,
      provisional,
      SuspensionOwnership.plan(normalized, provisional, snapshot.index),
      snapshot.index,
    ),
  )
}

const replaceDrop = (
  module: Mir.Module,
  target: Extract<Mir.Operation, { readonly _tag: 'Drop' }>,
  replacement: Extract<Mir.Operation, { readonly _tag: 'Drop' }>,
): Mir.Module =>
  Object.freeze({
    ...module,
    functions: Object.freeze(
      module.functions.map((fn) =>
        Object.freeze({
          ...fn,
          regions: Object.freeze(
            fn.regions.map((region) =>
              region._tag === 'OperationRegion'
                ? Object.freeze({
                    ...region,
                    operations: Object.freeze(
                      region.operations.map((operation) =>
                        operation === target ? replacement : operation,
                      ),
                    ),
                  })
                : region._tag === 'CleanupRegion'
                  ? Object.freeze({
                      ...region,
                      releases: Object.freeze(
                        region.releases.map((operation) =>
                          operation === target ? replacement : operation,
                        ),
                      ),
                    })
                  : region,
            ),
          ),
        }),
      ),
    ),
  })

const replaceOperation = (
  module: Mir.Module,
  target: Mir.Operation,
  replacement: Mir.Operation,
): Mir.Module =>
  Object.freeze({
    ...module,
    functions: Object.freeze(
      module.functions.map((fn) =>
        Object.freeze({
          ...fn,
          regions: Object.freeze(
            fn.regions.map((region) =>
              region._tag === 'OperationRegion'
                ? Object.freeze({
                    ...region,
                    operations: Object.freeze(
                      region.operations.map((operation) =>
                        operation === target ? replacement : operation,
                      ),
                    ),
                  })
                : region,
            ),
          ),
        }),
      ),
    ),
  })

it.effect('carries lazy construction and exact stored Effect execution through MIR', () =>
  Effect.gen(function* () {
    const name = 'stored-effect-mir/lifecycle'
    const { snapshot, module } = yield* lowerStored(
      name,
      `struct Deferred<F: Effect<i32>> { operation: F }
pub fn main() -> i32 {
  let other = effect { return 0 }
  let ignored = run other
  let deferred = Deferred { operation: effect { return 42 } }
  return run deferred.operation
}`,
    )
    const operations = module.functions.flatMap(MirVerification.operations)
    const make = operations.find((operation) => operation._tag === 'MakeEffect')
    const construct = operations.find((operation) => operation._tag === 'Construct')
    const run = module.functions
      .flatMap((fn) =>
        MirVerification.operations(fn).map((operation) => Object.freeze({ fn, operation })),
      )
      .find(({ fn, operation }) => {
        if (operation._tag !== 'RunEffectValue') return false
        const type = fn.localTypes.at(operation.effect.ordinal)
        return type?._tag === 'EffectValue' && type.storage !== undefined
      })?.operation
    const stored = construct?._tag === 'Construct' ? construct.fields.at(0)?.stored : undefined
    const projected = module.functions
      .flatMap((fn) => fn.localTypes)
      .find((type) => type._tag === 'EffectValue' && type.storage !== undefined)

    assert.notInclude(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0107',
    )
    assert.strictEqual(snapshot.layoutCatalog._tag, 'Available')
    assert.strictEqual(snapshot.layout._tag, 'Available')
    assert.strictEqual(snapshot.mir._tag, 'Available')
    assert.strictEqual(make?._tag, 'MakeEffect')
    assert.strictEqual(stored?._tag, 'StoredEffectField')
    assert.strictEqual(projected?._tag, 'EffectValue')
    assert.strictEqual(
      projected?._tag === 'EffectValue' ? projected.storage?._tag : undefined,
      'StoredEffectField',
    )
    assert.strictEqual(run?._tag, 'RunEffectValue')
    if (run?._tag !== 'RunEffectValue' || stored?._tag !== 'StoredEffectField') return
    assert.deepEqual(run.runner, stored.realization.runner)
    assert.deepEqual(run.runnerTypeArguments, stored.realization.runnerArguments)
    assert.deepEqual(Type.failureMembers(run.outcomeType.type), stored.realization.rows.failures)
    assert.deepEqual(
      Type.requirementMembers(run.outcomeType.type),
      stored.realization.rows.requirements,
    )
    assert.deepEqual(MirVerification.verify(module), [])
    const alternate = operations.find(
      (operation) => operation._tag === 'RunEffectValue' && operation !== run,
    )
    assert.strictEqual(alternate?._tag, 'RunEffectValue')
    if (alternate?._tag !== 'RunEffectValue') return
    const forged = Object.freeze({
      ...run,
      runner: alternate.runner,
      runnerTypeArguments: alternate.runnerTypeArguments,
      arguments: Object.freeze([run.effect]),
    })
    assert.include(
      MirVerification.verify(replaceOperation(module, run, forged)).map(
        (violation) => violation.rule,
      ),
      'InvalidEffectOperation',
    )
  }),
)

it.effect('binds provider-specialized runs to their exact generated runner and witness', () =>
  Effect.gen(function* () {
    const { module } = yield* lowerStored(
      'stored-effect-mir/provided-runner',
      `import silk.effect as Effect
service Counter { effect fn get() -> i32 ? &Counter }
service Meter { effect fn read() -> i32 ? &Meter }
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
effect fn read(self: &Fixed) -> i32 { return self.value }
impl Counter for Fixed { get: Fixed.get }
impl Meter for Fixed { read: Fixed.read }
effect fn count() -> i32 ? &Counter { return run Counter.get() }
effect fn measure() -> i32 ? &Meter { return run Meter.read() }
pub fn main() -> i32 {
  let fixed = Fixed { value: 42 }
  let ignored = run (count() |> Effect.provide(&fixed))
  let alsoIgnored = run (measure() |> Effect.provide(&fixed))
  return run (count() |> Effect.provide(&fixed))
}`,
    )
    const providedRuns = module.functions.flatMap((fn) =>
      MirVerification.operations(fn).flatMap((operation) => {
        if (operation._tag !== 'RunEffectValue' || operation.providers.length !== 1) return []
        return [operation]
      }),
    )
    const counter = providedRuns.find(
      (operation) => operation.providers.at(0)?.capability.name === 'Counter',
    )
    const meter = providedRuns.find(
      (operation) => operation.providers.at(0)?.capability.name === 'Meter',
    )

    assert.strictEqual(counter?._tag, 'RunEffectValue')
    assert.strictEqual(meter?._tag, 'RunEffectValue')
    assert.deepEqual(MirVerification.verify(module), [])
    if (counter?._tag !== 'RunEffectValue' || meter?._tag !== 'RunEffectValue') return
    const wrongWrapper = Object.freeze({
      ...counter,
      runner: meter.runner,
      runnerTypeArguments: meter.runnerTypeArguments,
    })
    assert.include(
      MirVerification.verify(replaceOperation(module, counter, wrongWrapper)).map(
        (violation) => violation.rule,
      ),
      'InvalidEffectOperation',
    )
    const counterProvider = counter.providers.at(0)
    const meterProvider = meter.providers.at(0)
    if (counterProvider === undefined || meterProvider === undefined) return
    const wrongWitness = Object.freeze({
      ...counter,
      providers: Object.freeze([
        Object.freeze({ ...counterProvider, witness: meterProvider.witness }),
      ]),
    })
    assert.include(
      MirVerification.verify(replaceOperation(module, counter, wrongWitness)).map(
        (violation) => violation.rule,
      ),
      'InvalidEffectOperation',
    )
  }),
)

it.effect('resolves unrun stored Effect cleanup before MIR', () =>
  Effect.gen(function* () {
    const { module } = yield* lowerStored(
      'stored-effect-mir/unrun-cleanup',
      `struct Token { value: i32 }
struct Deferred<F: once Effect<i32>> { operation: F }
fn consume(token: Token) -> i32 { return token.value }
pub fn main() -> i32 {
  let token = Token { value: 1 }
  let deferred = Deferred { operation: effect { return consume(move token) } }
  return 0
}`,
    )
    const cleanups = module.functions
      .flatMap(MirVerification.operations)
      .flatMap((operation) => (operation._tag === 'Drop' ? [operation.cleanup] : []))

    assert.notInclude(
      cleanups.map((cleanup) => cleanup._tag),
      'RepresentedEffectCleanup',
    )
    assert.include(
      cleanups.flatMap((cleanup) =>
        cleanup._tag === 'StructCleanup'
          ? cleanup.fields.map((field) => field.cleanup._tag)
          : [cleanup._tag],
      ),
      'EffectCleanup',
    )
    assert.deepEqual(MirVerification.verify(module), [])

    const drop = module.functions
      .flatMap(MirVerification.operations)
      .find(
        (operation): operation is Extract<Mir.Operation, { readonly _tag: 'Drop' }> =>
          operation._tag === 'Drop' &&
          operation.cleanup._tag === 'StructCleanup' &&
          operation.cleanup.fields.some((field) => field.cleanup._tag === 'EffectCleanup'),
      )
    assert.isDefined(drop)
    if (drop === undefined || drop.cleanup._tag !== 'StructCleanup') return
    const structCleanup = drop.cleanup
    const field = structCleanup.fields.find(
      (candidate) => candidate.cleanup._tag === 'EffectCleanup',
    )
    assert.strictEqual(field?.cleanup._tag, 'EffectCleanup')
    if (field?.cleanup._tag !== 'EffectCleanup') return
    const slot = field.cleanup.slots.at(0) ?? unreachable('expected owned stored Effect slot')
    const withEffectCleanup = (
      cleanup: Extract<CleanupPlan.CleanupPlan, { readonly _tag: 'EffectCleanup' }>,
    ): Extract<Mir.Operation, { readonly _tag: 'Drop' }> =>
      Object.freeze({
        ...drop,
        cleanup: Object.freeze({
          ...structCleanup,
          fields: Object.freeze(
            structCleanup.fields.map((candidate) =>
              candidate === field ? Object.freeze({ ...candidate, cleanup }) : candidate,
            ),
          ),
        }),
      })
    const malformed = [
      Object.freeze({ ...field.cleanup, slots: Object.freeze([]) }),
      Object.freeze({
        ...field.cleanup,
        slots: Object.freeze([Object.freeze({ ...slot, laneOffset: slot.laneOffset + 1 })]),
      }),
      Object.freeze({
        ...field.cleanup,
        slots: Object.freeze([Object.freeze({ ...slot, laneCount: slot.laneCount + 1 })]),
      }),
      Object.freeze({
        ...field.cleanup,
        slots: Object.freeze([
          Object.freeze({
            ...slot,
            cleanup: Object.freeze({ _tag: 'NoCleanup' as const, type: slot.cleanup.type }),
          }),
        ]),
      }),
    ]
    for (const cleanup of malformed) {
      assert.include(
        MirVerification.verify(replaceDrop(module, drop, withEffectCleanup(cleanup))).map(
          (violation) => violation.rule,
        ),
        'InvalidAggregateOperation',
      )
    }
  }),
)

it.effect('retains stored runners across suspension and resume planning', () =>
  Effect.gen(function* () {
    const lowered = yield* lowerStored(
      'stored-effect-mir/suspending',
      `import silk.effect as Effect
struct Deferred<F: Effect<i32>> { operation: F }
effect fn delayed() -> i32 {
  return run Effect.suspend(effect { return 42 })
}
pub fn main() -> i32 {
  let deferred = Deferred { operation: effect { return run delayed() } }
  return run deferred.operation
}`,
    )
    const module = finalizeSuspension(lowered.snapshot, lowered.module, lowered.layout)
    const run = module.functions
      .flatMap(MirVerification.operations)
      .find((operation) => operation._tag === 'RunEffectValue')
    const suspension = module.functions.flatMap((fn) => fn.suspension?.regions ?? [])

    assert.strictEqual(run?._tag, 'RunEffectValue')
    assert.include(
      suspension.map((region) => region._tag),
      'SuspendEffectRegion',
    )
    assert.include(
      suspension.map((region) => region._tag),
      'RunSuspendableEffectRegion',
    )
    const storedRunner =
      module.functions.find(
        (fn) => fn.id.module === 'stored-effect-mir/suspending' && fn.id.name === 'main$effect$0',
      ) ?? unreachable('expected stored Effect runner')
    const storedRelay =
      storedRunner.suspension?.regions.find(
        (region) => region._tag === 'RunSuspendableEffectRegion',
      ) ?? unreachable('expected stored Effect suspension relay')
    const relayTarget =
      storedRelay.runner.declaration ?? unreachable('expected stored Effect relay target')
    assert.isTrue(
      module.functions.some((fn) =>
        Mir.matchesInstance(fn, relayTarget, storedRelay.runner.typeArguments),
      ),
      'the stored Effect relay must target a realized runner in the origin-reachable chain',
    )
    assert.isTrue(
      module.functions.some((fn) =>
        fn.suspension?.regions.some((region) => region._tag === 'SuspendEffectRegion'),
      ),
      'the reachable chain must terminate in an explicit suspension origin',
    )
    assert.deepEqual(MirVerification.verify(module), [])
    assert.include(MirEncoding.encode(module), 'stored-effect-mir/suspending.main$effect$0')
  }),
)

it.effect('keeps typed-failure releases on stored Effect propagation paths', () =>
  Effect.gen(function* () {
    const { module } = yield* lowerStored(
      'stored-effect-mir/failure-cleanup',
      `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct Token { value: i32 }
struct Deferred<F: once Effect<i32>> { operation: F }
fn consume(token: Token) -> i32 { return token.value }
effect fn build() -> i32 ! OutOfMemoryError {
  let token = Token { value: 1 }
  let deferred = Deferred { operation: effect { return consume(move token) } }
  let mut allocator = Allocator.systemAllocatorService()
  let allocation = run Allocator.allocate(Layout.of<[i32; 2]>()) |> Effect.provideMut(&mut allocator)
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    )
    const propagating = module.functions
      .flatMap(MirVerification.operations)
      .flatMap((operation) =>
        (operation._tag === 'RunEffect' || operation._tag === 'RunEffectValue') &&
        operation.releases !== undefined
          ? [operation]
          : [],
      )
    const cleanupTags = propagating.flatMap((operation) =>
      (operation.releases ?? []).flatMap((release) =>
        release.cleanup._tag === 'StructCleanup'
          ? release.cleanup.fields.map((field) => field.cleanup._tag)
          : [release.cleanup._tag],
      ),
    )

    assert.include(cleanupTags, 'EffectCleanup')
    assert.deepEqual(MirVerification.verify(module), [])
    assert.isFalse(
      module.layout.entries.some(
        (entry) => Type.isEffect(entry.type) && entry.representation._tag !== 'Aggregate',
      ),
    )
  }),
)
