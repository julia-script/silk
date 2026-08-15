import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ContinuationLayout from '../src/ContinuationLayout.js'
import * as Layout from '../src/Layout.js'
import * as Lower from '../src/Lower.js'
import * as Mir from '../src/Mir.js'
import * as MirNormalization from '../src/MirNormalization.js'
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
  const layout = Layout.plan(catalog, snapshot.instances)
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
  return ContinuationLayout.apply(
    SuspensionMir.finalize(
      normalized,
      provisional,
      SuspensionOwnership.plan(normalized, provisional, snapshot.index),
      snapshot.index,
    ),
  )
}

it.effect('carries lazy construction and exact stored Effect execution through MIR', () =>
  Effect.gen(function* () {
    const name = 'stored-effect-mir/lifecycle'
    const { snapshot, module } = yield* lowerStored(
      name,
      `struct Deferred<F: Effect<i32>> { operation: F }
pub fn main() -> i32 {
  let deferred = Deferred { operation: effect { return 42 } }
  return run deferred.operation
}`,
    )
    const operations = module.functions.flatMap(Mir.operations)
    const make = operations.find((operation) => operation._tag === 'MakeEffect')
    const construct = operations.find((operation) => operation._tag === 'Construct')
    const run = operations.find((operation) => operation._tag === 'RunEffectValue')
    const stored = construct?._tag === 'Construct' ? construct.fields.at(0)?.stored : undefined
    const projected = module.functions
      .flatMap((fn) => fn.localTypes)
      .find((type) => type._tag === 'EffectValue' && type.storage !== undefined)

    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0107',
    )
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
    assert.deepEqual(run.outcomeType.type.failures, stored.realization.rows.failures)
    assert.deepEqual(run.outcomeType.type.requirements, stored.realization.rows.requirements)
    assert.deepEqual(Mir.verify(module), [])
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
      .flatMap(Mir.operations)
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
    assert.deepEqual(Mir.verify(module), [])
  }),
)

it.effect('retains stored runners across suspension and resume planning', () =>
  Effect.gen(function* () {
    const lowered = yield* lowerStored(
      'stored-effect-mir/suspending',
      `struct Deferred<F: Effect<i32>> { operation: F }
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
effect fn delayed() -> i32 {
  let mut allocator = SystemAllocator.make()
  let provided = Effect.suspend(effect { return 42 }) |> Effect.provideMut(&mut allocator)
  return run Effect.catch(move provided, recover)
}
pub fn main() -> i32 {
  let deferred = Deferred { operation: effect { return run delayed() } }
  return run deferred.operation
}`,
    )
    const module = finalizeSuspension(lowered.snapshot, lowered.module, lowered.layout)
    const run = module.functions
      .flatMap(Mir.operations)
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
    const violations = Mir.verify(module)
    assert.notInclude(
      violations.map((violation) => violation.rule),
      'InvalidEffectOperation',
    )
    assert.notInclude(
      violations.map((violation) => violation.rule),
      'InvalidSuspension',
    )
    assert.deepEqual(
      violations.map((violation) => violation.rule),
      ['OrphanSuspensionMachinery'],
    )
    assert.include(Mir.encode(module), 'stored-effect-mir/suspending.main$effect$0')
  }),
)

it.effect('keeps typed-failure releases on stored Effect propagation paths', () =>
  Effect.gen(function* () {
    const { module } = yield* lowerStored(
      'stored-effect-mir/failure-cleanup',
      `struct Token { value: i32 }
struct Deferred<F: once Effect<i32>> { operation: F }
fn consume(token: Token) -> i32 { return token.value }
effect fn build() -> i32 ! OutOfMemory {
  let token = Token { value: 1 }
  let deferred = Deferred { operation: effect { return consume(move token) } }
  let mut allocator = SystemAllocator.make()
  let allocation = run Allocator.allocate(Layout.of<[i32; 2]>()) |> Effect.provideMut(&mut allocator)
  return 42
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catch(build(), recover) }`,
    )
    const propagating = module.functions
      .flatMap(Mir.operations)
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
    assert.deepEqual(Mir.verify(module), [])
    assert.isFalse(
      module.layout.entries.some(
        (entry) => Type.isEffect(entry.type) && entry.representation._tag !== 'Aggregate',
      ),
    )
  }),
)
