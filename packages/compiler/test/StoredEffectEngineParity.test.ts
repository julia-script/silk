import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
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
import * as WasmBackend from '../src/WasmBackend.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * Lowers one stored-Effect program to MIR for a chosen target.
 *
 * `SEM0107` still fences every stored Effect out of `Analysis.realize`, so this mirrors the merged
 * evaluator harness and drives the phases directly instead of reading `snapshot.mir`. The fence
 * assertion is deliberate: task 4.4 narrows it only for shapes these engines prove first.
 */
const lowerStored = Effect.fnUntraced(function* (
  name: string,
  source: string,
  target: Target.Target,
) {
  const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), target.id)
  const diagnostics = Analysis.diagnostics(snapshot)
  assert.isTrue(diagnostics.length > 0, name)
  assert.isTrue(
    diagnostics.every((diagnostic) => diagnostic.code === 'SEM0107'),
    JSON.stringify(diagnostics.map(({ code, message }) => ({ code, message }))),
  )
  const catalog = Layout.catalog(target, snapshot.index, snapshot.instances)
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
  assert.deepEqual(Mir.verify(module), [], name)
  return Object.freeze({ snapshot, module })
})

const runWasm = Effect.fnUntraced(function* (bytes: Uint8Array) {
  const result = yield* Effect.try(() => {
    const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') return unreachable('expected Wasm main')
    return main()
  })
  assert.strictEqual(typeof result, 'number')
  return typeof result === 'number' ? result : unreachable('expected numeric Wasm result')
})

const completedValue = (outcome: BootstrapEvaluation.Outcome): number => {
  assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
  if (outcome._tag !== 'Completed') return unreachable('expected completed evaluation')
  return outcome.result.value
}

const storedRealizations = (module: Mir.Module) =>
  module.layout.entries.flatMap((entry) =>
    entry.representation._tag === 'StoredEffectEnvironment'
      ? [entry.representation.realization]
      : [],
  )

/** The static runner every engine must agree on for one stored Effect. */
const storedRunner = (module: Mir.Module, label: string) => {
  const realizations = storedRealizations(module)
  for (const realization of realizations) {
    for (const operation of module.functions.flatMap(Mir.operations)) {
      if (operation._tag !== 'RunEffectValue') continue
      const runner = operation.runnerBase?.declaration ?? operation.runner
      if (runner.module === realization.runner.module && runner.name === realization.runner.name)
        return Object.freeze({ operation, realization })
    }
  }
  return unreachable(`expected one ${label} stored Effect run`)
}

/** The single stored realization, for shapes such as `unrun` that never reach a run operation. */
const storedRealization = (module: Mir.Module, label: string) => {
  const realizations = storedRealizations(module)
  return realizations.length === 1
    ? (realizations.at(0) ?? unreachable(`expected one ${label} realization`))
    : unreachable(`expected one ${label} realization (${realizations.length} found)`)
}

/** Mirrors the LLVM symbol sanitizer so IR assertions name the emitted runner, not the MIR spelling. */
const sanitize = (name: string): string => name.replace(/[^A-Za-z0-9_]/g, '_')

const dropCalls = (outcome: BootstrapEvaluation.Outcome): number =>
  outcome._tag === 'Completed'
    ? outcome.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
      ).length
    : 0

const traceCount = (outcome: BootstrapEvaluation.Outcome, tag: string): number =>
  outcome._tag === 'Completed' ? outcome.trace.filter((event) => event._tag === tag).length : 0

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

it.effect(
  'executes stored Effects with identical results and runner identity in every engine',
  () =>
    Effect.gen(function* () {
      const host = yield* Target.host()
      for (const testCase of runtimeMatrix) {
        // One module name for both targets: the runner identity must not depend on the target.
        const moduleName = `stored-effect-parity/${testCase.name}`
        const wasmLowering = yield* lowerStored(
          moduleName,
          testCase.source,
          Target.wasm32UnknownUnknown,
        )
        const { realization } = storedRunner(wasmLowering.module, testCase.name)
        assert.strictEqual(realization.access, testCase.access, testCase.name)

        const evaluated = BootstrapEvaluation.evaluate(
          wasmLowering.snapshot.instances,
          wasmLowering.module,
        )
        assert.strictEqual(completedValue(evaluated), testCase.result, testCase.name)

        const wasm = yield* Backend.emit(WasmBackend.WasmBackend, wasmLowering.module, {
          mode: 'release',
        })
        assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact', testCase.name)
        if (wasm._tag !== 'WebAssemblyModuleArtifact') return
        assert.strictEqual(yield* runWasm(wasm.bytes), testCase.result, `${testCase.name} Wasm`)
        // Stored Effects stay statically dispatched: no runtime dictionary, no indirect call.
        assert.notInclude(wasm.wat, 'call_indirect', testCase.name)

        const nativeLowering = yield* lowerStored(moduleName, testCase.source, host)
        const nativeRunner = storedRunner(nativeLowering.module, testCase.name)
        const llvm = yield* Backend.emit(Backend.LlvmBackend, nativeLowering.module, {
          mode: 'release',
        })
        assert.strictEqual(llvm._tag, 'LlvmBitcodeArtifact', testCase.name)
        if (llvm._tag !== 'LlvmBitcodeArtifact') return
        assert.include(llvm.ir, 'define i32 @silk_main', testCase.name)
        // The same static runner the evaluator called is the one LLVM emitted.
        assert.include(llvm.ir, sanitize(nativeRunner.realization.runner.name), testCase.name)
        assert.deepEqual(
          nativeRunner.realization.runner,
          realization.runner,
          `${testCase.name} runner identity across targets`,
        )
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
  let guard = Guard { tag: 7, storage: move storage }
  let deferred = defer(failing(move guard))
  ${exit === 'failure' ? 'return run deferred.operation' : 'return 42'}
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

it.effect('cleans unrun and failing stored Effect environments exactly once in every engine', () =>
  Effect.gen(function* () {
    const host = yield* Target.host()
    for (const exit of ['unrun', 'failure'] as const) {
      const moduleName = `stored-effect-parity/cleanup-${exit}`
      const { snapshot, module } = yield* lowerStored(
        moduleName,
        cleanupProgram(exit),
        Target.wasm32UnknownUnknown,
      )
      const outcome = BootstrapEvaluation.evaluate(snapshot.instances, module)
      assert.strictEqual(completedValue(outcome), 42, exit)
      // The evaluator's cleanup trace is the contract the backends must match.
      assert.strictEqual(dropCalls(outcome), 1, `${exit} Drop hook`)
      assert.strictEqual(traceCount(outcome, 'AllocationAcquire'), 1, `${exit} acquire`)
      assert.strictEqual(traceCount(outcome, 'AllocationRelease'), 1, `${exit} release`)

      const wasm = yield* Backend.emit(WasmBackend.WasmBackend, module, { mode: 'release' })
      assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact', exit)
      if (wasm._tag !== 'WebAssemblyModuleArtifact') return
      assert.strictEqual(yield* runWasm(wasm.bytes), 42, `${exit} Wasm`)
      assert.notInclude(wasm.wat, 'call_indirect', exit)

      const native = yield* lowerStored(moduleName, cleanupProgram(exit), host)
      const llvm = yield* Backend.emit(Backend.LlvmBackend, native.module, { mode: 'release' })
      assert.strictEqual(llvm._tag, 'LlvmBitcodeArtifact', exit)
      if (llvm._tag !== 'LlvmBitcodeArtifact') return
      // The Drop hook that cleans the stored environment survives into native code.
      assert.include(llvm.ir, sanitize('drop@impl'), exit)

      // `unrun` never reaches a run operation, so the realization is the only runner fact.
      const runner = storedRealization(module, exit).runner
      const runnerCalls =
        outcome._tag === 'Completed'
          ? outcome.trace.filter(
              (event) =>
                event._tag === 'Call' &&
                event.target.module === runner.module &&
                event.target.name === runner.name,
            ).length
          : -1
      assert.strictEqual(runnerCalls, exit === 'failure' ? 1 : 0, `${exit} stored runner calls`)
      if (exit === 'failure')
        assert.isAbove(traceCount(outcome, 'EffectFailure'), 0, 'typed failure trace')
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

it.effect('resumes a suspending stored Effect with matching cleanup in every engine', () =>
  Effect.gen(function* () {
    const host = yield* Target.host()
    const moduleName = 'stored-effect-parity/suspending'
    const { snapshot, module } = yield* lowerStored(
      moduleName,
      suspending,
      Target.wasm32UnknownUnknown,
    )
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, module)
    assert.strictEqual(completedValue(outcome), 42)
    assert.strictEqual(dropCalls(outcome), 1)
    assert.isAbove(traceCount(outcome, 'SuspensionOrigin'), 0)

    const wasm = yield* Backend.emit(WasmBackend.WasmBackend, module, { mode: 'release' })
    assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact')
    if (wasm._tag !== 'WebAssemblyModuleArtifact') return
    assert.strictEqual(yield* runWasm(wasm.bytes), 42)
    assert.notInclude(wasm.wat, 'call_indirect')

    const native = yield* lowerStored(moduleName, suspending, host)
    const llvm = yield* Backend.emit(Backend.LlvmBackend, native.module, { mode: 'release' })
    assert.strictEqual(llvm._tag, 'LlvmBitcodeArtifact')
    if (llvm._tag !== 'LlvmBitcodeArtifact') return
    assert.include(llvm.ir, 'define i32 @silk_main')
    assert.include(
      llvm.ir,
      sanitize(storedRunner(native.module, 'suspending').realization.runner.name),
    )
  }),
)
