import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
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
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const lowerStored = Effect.fnUntraced(function* (name: string, source: string) {
  const snapshot = yield* Analysis.ofSourceRealized(
    name,
    ascii(source),
    Target.wasm32UnknownUnknown.id,
  )
  const diagnostics = Analysis.diagnostics(snapshot)
  assert.isTrue(diagnostics.length > 0)
  assert.isTrue(
    diagnostics.every((diagnostic) => diagnostic.code === 'SEM0107'),
    JSON.stringify(diagnostics.map(({ code, message }) => ({ code, message }))),
  )
  const catalog = Layout.catalog(Target.wasm32UnknownUnknown, snapshot.index, snapshot.instances)
  const layout = Layout.plan(catalog, snapshot.instances)
  const ownership =
    Analysis.ownershipOf(snapshot, name) ?? unreachable('expected module ownership facts')
  const lowered = Lower.lowerProgram(
    snapshot.instances,
    new Map<string, Ownership.ModuleOwnership>([[name, ownership]]),
    layout,
    snapshot.index,
    OpaqueRealization.catalogOf(snapshot),
  )
  const provisional = ProvisionalMir.build(snapshot.instances, layout, snapshot.index)
  const normalized = MirNormalization.normalize(lowered, provisional)
  const module = ContinuationLayout.apply(
    SuspensionMir.finalize(
      normalized,
      provisional,
      SuspensionOwnership.plan(normalized, provisional, snapshot.index),
      snapshot.index,
    ),
  )
  return Object.freeze({ snapshot, module })
})

const completedValue = (outcome: BootstrapEvaluation.Outcome): number => {
  assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
  if (outcome._tag !== 'Completed') return unreachable('expected completed evaluation')
  return outcome.result.value
}

const storedRun = (module: Mir.Module, label = 'stored Effect') => {
  const realizations = module.layout.entries.flatMap((entry) =>
    entry.representation._tag === 'StoredEffectEnvironment'
      ? [entry.representation.realization]
      : [],
  )
  for (const realization of realizations) {
    for (const operation of module.functions.flatMap(Mir.operations)) {
      if (operation._tag !== 'RunEffectValue') continue
      const runner = operation.runnerBase?.declaration ?? operation.runner
      if (runner.module === realization.runner.module && runner.name === realization.runner.name)
        return Object.freeze({ operation, realization })
    }
  }
  const expected = realizations.map((realization) => realization.runner.name).join(', ')
  const actual = module.functions
    .flatMap(Mir.operations)
    .flatMap((operation) =>
      operation._tag === 'RunEffectValue'
        ? [operation.runnerBase?.declaration.name ?? operation.runner.name]
        : [],
    )
    .join(', ')
  return unreachable(`expected one ${label} run (${expected || 'none'}; saw ${actual || 'none'})`)
}

const assertRunnerAndRows = (module: Mir.Module, outcome: BootstrapEvaluation.Outcome): void => {
  const { operation, realization } = storedRun(module)
  assert.deepEqual(operation.runnerBase?.declaration ?? operation.runner, realization.runner)
  assert.deepEqual(
    operation.runnerBase?.typeArguments ?? operation.runnerTypeArguments,
    realization.runnerArguments,
  )
  assert.deepEqual(operation.outcomeType.type.failures, realization.rows.failures)
  assert.deepEqual(operation.outcomeType.type.requirements, realization.rows.requirements)
  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.isTrue(
    outcome.trace.some(
      (event) =>
        event._tag === 'Call' &&
        event.target.module === operation.runner.module &&
        event.target.name === operation.runner.name,
    ),
  )
}

const assertProvidedSpecialization = (
  module: Mir.Module,
  outcome: BootstrapEvaluation.Outcome,
): void => {
  const operation = module.functions
    .flatMap(Mir.operations)
    .find(
      (candidate) =>
        candidate._tag === 'RunEffectValue' &&
        candidate.runnerBase?.declaration.name === 'count$effect$-1',
    )
  assert.isDefined(operation)
  if (operation?._tag !== 'RunEffectValue') return
  assert.match(operation.runner.name, /^count\$effect\$-1\$provided\$/)
  assert.lengthOf(operation.providers, 1)
  assert.deepEqual(operation.outcomeType.type.failures, [])
  assert.lengthOf(operation.outcomeType.type.requirements, 1)
  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.isTrue(
    outcome.trace.some(
      (event) =>
        event._tag === 'Call' &&
        event.target.module === operation.runner.module &&
        event.target.name === operation.runner.name,
    ),
  )
}

const assertOwnershipFacts = (
  realization: ReturnType<typeof storedRun>['realization'],
  name: (typeof runtimeMatrix)[number]['name'],
): void => {
  assert.isTrue('environment' in realization && 'cleanup' in realization, name)
  if (!('environment' in realization) || !('cleanup' in realization)) return
  if (name === 'shared' || name === 'exclusive') {
    assert.isTrue(
      name === 'shared'
        ? realization.environment.every((slot) => !slot.owned)
        : realization.environment.some((slot) => slot.borrowed),
      name,
    )
    assert.lengthOf(realization.cleanup.unrunLanes, 0, name)
    assert.isFalse(realization.cleanup.consumedByRun, name)
    return
  }
  assert.isTrue(
    realization.environment.some((slot) => slot.owned),
    name,
  )
  assert.isAbove(realization.cleanup.unrunLanes.length, 0, name)
  assert.isTrue(realization.cleanup.consumedByRun, name)
}

const shared = `struct Deferred<F: Effect<i32>> { operation: F }
pub fn main() -> i32 {
  let base = 21
  let deferred = Deferred { operation: effect { return base } }
  return (run deferred.operation) + (run deferred.operation)
}`

const exclusive = `struct Deferred<F: mut Effect<i32>> { operation: F }
pub fn main() -> i32 {
  let mut counter = 20
  let mut deferred = Deferred { operation: effect {
    counter = counter + 1
    return counter
  } }
  return (run deferred.operation) + (run deferred.operation)
}`

const consuming = `struct Token { value: i32 storage: Allocation }
impl Drop for Token { fn drop(self: &mut Token) -> () { return () } }
struct Deferred<F: once Effect<i32>> { operation: F }
fn consume(token: Token) -> i32 { return token.value }
effect fn build() -> i32 ! OutOfMemory ? &mut Allocator {
  let storage = run Allocator.allocate(Layout.of<i32>())
  let token = Token { value: 42, storage: move storage }
  let deferred = Deferred { operation: effect { return consume(move token) } }
  return run deferred.operation
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(build() |> Effect.provideMut(&mut allocator), recover)
}`

const provided = `service Counter { effect fn get() -> i32 ? &Counter }
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
impl Counter for Fixed { get: Fixed.get }
effect fn count() -> i32 ? &Counter { return run Counter.get() }
struct Deferred<F: once Effect<i32>> { operation: F }
pub fn main() -> i32 {
  let fixed = Fixed { value: 42 }
  let deferred = Deferred { operation: count() |> Effect.provide(&fixed) }
  return run deferred.operation
}`

const runtimeMatrix = [
  { name: 'shared', source: shared, result: 42, access: 'Shared' },
  { name: 'exclusive', source: exclusive, result: 43, access: 'Exclusive' },
  { name: 'consuming', source: consuming, result: 42, access: 'Take' },
  { name: 'provided', source: provided, result: 42, access: 'Take' },
] as const

it.effect('executes stored Effects with exact runner, rows, access, and ownership transport', () =>
  Effect.gen(function* () {
    for (const testCase of runtimeMatrix) {
      const { snapshot, module } = yield* lowerStored(
        `stored-effect-runtime/${testCase.name}`,
        testCase.source,
      )
      assert.deepEqual(Mir.verify(module), [], testCase.name)
      const { realization } = storedRun(module, testCase.name)
      assert.strictEqual(realization.access, testCase.access, testCase.name)
      assertOwnershipFacts(realization, testCase.name)
      const outcome = BootstrapEvaluation.evaluate(snapshot.instances, module)
      assert.strictEqual(completedValue(outcome), testCase.result, testCase.name)
      assertRunnerAndRows(module, outcome)
      if (testCase.name === 'consuming') {
        assert.strictEqual(
          outcome._tag === 'Completed'
            ? outcome.trace.filter(
                (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
              ).length
            : 0,
          1,
        )
        assert.strictEqual(
          outcome._tag === 'Completed'
            ? outcome.trace.filter((event) => event._tag === 'AllocationRelease').length
            : 0,
          1,
        )
      }
      if (testCase.name === 'provided') assertProvidedSpecialization(module, outcome)
    }
  }),
)

type CleanupExit = 'unrun' | 'failure'

const cleanupProgram = (exit: CleanupExit): string => `struct Problem { code: i32 }
struct Guard { tag: i32 storage: Allocation }
impl Drop for Guard { fn drop(self: &mut Guard) -> () { return () } }
struct Deferred<A, !E, ?R, F: once Effect<A ! E ? R>> { operation: F }
fn defer<A, !E, ?R, F: once Effect<A ! E ? R>>(
  operation: F
) -> Deferred<A, E, R, F> {
  return Deferred<A, E, R> { operation: move operation }
}
effect fn failing(guard: Guard) -> i32 ! Problem {
  let result = guard.tag
  if result == 0 { return 0 }
  fail Problem { code: result }
}
effect fn build() -> i32 ! Problem | OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let storage = run Allocator.allocate(Layout.of<i32>()) |> Effect.provideMut(&mut allocator)
  let guard = Guard { tag: 42, storage: move storage }
  let deferred = defer(failing(move guard))
  ${exit === 'failure' ? 'return run deferred.operation' : 'return 42'}
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

it.effect('cleans unrun and failing stored Effect environments exactly once', () =>
  Effect.gen(function* () {
    for (const exit of ['unrun', 'failure'] as const) {
      const { snapshot, module } = yield* lowerStored(
        `stored-effect-runtime/cleanup-${exit}`,
        cleanupProgram(exit),
      )
      assert.deepEqual(Mir.verify(module), [], exit)
      const outcome = BootstrapEvaluation.evaluate(snapshot.instances, module)
      assert.strictEqual(completedValue(outcome), 42, exit)
      assert.strictEqual(
        outcome.trace.filter(
          (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
        ).length,
        1,
        `${exit} Drop hook`,
      )
      assert.strictEqual(
        outcome.trace.filter((event) => event._tag === 'AllocationAcquire').length,
        1,
        `${exit} allocation acquire`,
      )
      assert.strictEqual(
        outcome.trace.filter((event) => event._tag === 'AllocationRelease').length,
        1,
        `${exit} allocation release`,
      )
      if (exit === 'failure') assertRunnerAndRows(module, outcome)
    }
  }),
)

const suspending = `struct Guard { tag: i32 storage: Allocation }
impl Drop for Guard { fn drop(self: &mut Guard) -> () { return () } }
struct Deferred<A, !E, ?R, F: once Effect<A ! E ? R>> { operation: F }
fn defer<A, !E, ?R, F: once Effect<A ! E ? R>>(
  operation: F
) -> Deferred<A, E, R, F> {
  return Deferred<A, E, R> { operation: move operation }
}
effect fn delayed(guard: Guard) -> i32 ! OutOfMemory ? &mut Allocator {
  let base = run Effect.suspend(effect { return 40 })
  return base + guard.tag
}
effect fn build() -> i32 ! OutOfMemory ? &mut Allocator {
  let storage = run Allocator.allocate(Layout.of<i32>())
  let guard = Guard { tag: 2, storage: move storage }
  let deferred = defer(delayed(move guard))
  return run deferred.operation
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(build() |> Effect.provideMut(&mut allocator), recover)
}`

it.effect('resumes a suspending stored Effect and cleans its environment exactly once', () =>
  Effect.gen(function* () {
    const { snapshot, module } = yield* lowerStored('stored-effect-runtime/suspending', suspending)
    assert.deepEqual(Mir.verify(module), [])
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, module)
    assert.strictEqual(completedValue(outcome), 42)
    assertRunnerAndRows(module, outcome)
    assert.strictEqual(
      outcome.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
      ).length,
      1,
    )
    assert.isAbove(outcome.trace.filter((event) => event._tag === 'SuspensionOrigin').length, 0)
    assert.strictEqual(
      outcome.trace.filter((event) => event._tag === 'ContinuationAcquire').length,
      outcome.trace.filter((event) => event._tag === 'ContinuationRelease').length,
    )
  }),
)
