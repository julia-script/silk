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
import * as StandardStreams from '../src/StandardStreams.js'
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

/**
 * Asserts the run operation carries exactly the realization's contract rows.
 *
 * Rows stay compile-time facts, so this is the parity check that both backends consume the same
 * exact success, failure, and requirement rows the evaluator proved — without any runtime lane.
 */
const assertExactRows = (module: Mir.Module, label: string): void => {
  const { operation, realization } = storedRunner(module, label)
  assert.deepEqual(
    operation.runnerBase?.declaration ?? operation.runner,
    realization.runner,
    `${label} runner`,
  )
  assert.deepEqual(
    operation.runnerBase?.typeArguments ?? operation.runnerTypeArguments,
    realization.runnerArguments,
    `${label} runner arguments`,
  )
  assert.deepEqual(
    operation.outcomeType.type.failures,
    realization.rows.failures,
    `${label} failure rows`,
  )
  assert.deepEqual(
    operation.outcomeType.type.requirements,
    realization.rows.requirements,
    `${label} requirement rows`,
  )
}

/** The single stored realization, for shapes such as `unrun` that never reach a run operation. */
const storedRealization = (module: Mir.Module, label: string) => {
  const realizations = storedRealizations(module)
  return realizations.length === 1
    ? (realizations.at(0) ?? unreachable(`expected one ${label} realization`))
    : unreachable(`expected one ${label} realization (${realizations.length} found)`)
}

/**
 * The exact symbol the LLVM backend emits for one runner.
 *
 * Asserting the full mangled symbol — rather than a sanitized name fragment — pins the runner's
 * concrete instance key too, so a correct spelling paired with the wrong specialization fails.
 */
const emittedSymbol = (module: Mir.Module, runner: { module: string; name: string }): string => {
  const candidates = module.functions.filter((candidate) => candidate.id.module === runner.module)
  // Provider specialization renames a runner in place (`f$effect$-1` -> `f$effect$-1$provided$N`),
  // so accept the exact runner or its single specialization, and nothing else.
  const fn =
    candidates.find((candidate) => candidate.id.name === runner.name) ??
    candidates.find((candidate) => candidate.id.name.startsWith(`${runner.name}$provided$`)) ??
    unreachable(`expected an emitted function for ${runner.module}.${runner.name}`)
  return Backend.symbolFor(fn, Mir.machineEntry(module))
}

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
        assertExactRows(wasmLowering.module, testCase.name)

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
        // The native lowering carries the same exact rows the Wasm lowering and evaluator proved.
        assertExactRows(nativeLowering.module, testCase.name)
        const llvm = yield* Backend.emit(Backend.LlvmBackend, nativeLowering.module, {
          mode: 'release',
        })
        assert.strictEqual(llvm._tag, 'LlvmBitcodeArtifact', testCase.name)
        if (llvm._tag !== 'LlvmBitcodeArtifact') return
        assert.include(llvm.ir, 'define i32 @silk_main', testCase.name)
        // The same static runner the evaluator called is the one LLVM emitted.
        assert.include(
          llvm.ir,
          emittedSymbol(nativeLowering.module, nativeRunner.realization.runner),
          testCase.name,
        )
        assert.deepEqual(
          nativeRunner.realization.runner,
          realization.runner,
          `${testCase.name} runner identity across targets`,
        )
        if (testCase.name === 'provided') {
          // Service provision resolves statically: the specialized runner reaches the backend, and
          // the provider travels in the environment rather than a runtime dictionary.
          const specialized = nativeLowering.module.functions
            .flatMap(Mir.operations)
            .find(
              (candidate) =>
                candidate._tag === 'RunEffectValue' &&
                candidate.runnerBase?.declaration.name === 'count$effect$-1',
            )
          assert.isDefined(specialized, testCase.name)
          if (specialized?._tag !== 'RunEffectValue') return
          assert.match(specialized.runner.name, /^count\$effect\$-1\$provided\$/)
          assert.lengthOf(specialized.providers, 1, testCase.name)
          assert.include(
            llvm.ir,
            emittedSymbol(nativeLowering.module, specialized.runner),
            testCase.name,
          )
        }
      }
    }),
)

/**
 * Repeats one stored-Effect construct-and-clean cycle `count` times.
 *
 * Returning 42 does not distinguish a released capture from a leaked one, so the cleanup parity
 * check runs the cycle in a loop and measures the Wasm heap: an environment the backend fails to
 * release once per iteration grows memory, while exactly-once cleanup keeps it flat.
 */
const cleanupCycles = (exit: CleanupExit, count: number): string => `struct Problem { code: i32 }
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
effect fn cycle() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let storage = run Allocator.allocate(Layout.of<i32>())
  let guard = Guard { tag: 7, storage: move storage }
  let deferred = defer(failing(move guard))
  ${exit === 'failure' ? 'return run deferred.operation' : 'return 42'}
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 42 }
effect fn drive() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let mut index = 0
  let mut total = 0
  while index < ${count} {
    total = total + run cycle()
    index = index + 1
  }
  return total
}
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let total = run Effect.catch(drive() |> Effect.provideMut(&mut allocator), recover)
  if total == ${exit === 'failure' ? 42 : count * 42} { return 42 }
  return 1
}`

const pagesOf = (instance: WebAssembly.Instance): number => {
  const memory = instance.exports[StandardStreams.wasmMemoryExport]
  assert.instanceOf(memory, WebAssembly.Memory)
  return memory instanceof WebAssembly.Memory
    ? memory.buffer.byteLength / 65536
    : Number.POSITIVE_INFINITY
}

/** Runs a Wasm artifact and reports both its result and the heap it needed. */
const runWasmMeasured = Effect.fnUntraced(function* (bytes: Uint8Array) {
  return yield* Effect.try(() => {
    const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') return unreachable('expected Wasm main')
    const value = main()
    return Object.freeze({ value, pages: pagesOf(instance) })
  })
})

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
      const dropFn =
        native.module.functions.find((candidate) => candidate.id.name.startsWith('drop@impl')) ??
        unreachable(`expected an emitted Drop hook for ${exit}`)
      assert.include(llvm.ir, Backend.symbolFor(dropFn, Mir.machineEntry(native.module)), exit)

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
      if (exit === 'failure') {
        assert.isAbove(traceCount(outcome, 'EffectFailure'), 0, 'typed failure trace')
        // A typed failure keeps its exact failure rows through both lowerings.
        assertExactRows(module, exit)
        assertExactRows(native.module, `${exit} native`)
      }
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
    assertExactRows(module, 'suspending')

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
      emittedSymbol(native.module, storedRunner(native.module, 'suspending').realization.runner),
    )
  }),
)

it.effect('keeps the Wasm heap flat across repeated stored Effect cleanup cycles', () =>
  Effect.gen(function* () {
    // Only the unrun exit repeats: a typed failure aborts the loop on its first cycle, so its
    // iteration count would not vary and the heap comparison would prove nothing.
    for (const exit of ['unrun'] as const) {
      const short = yield* lowerStored(
        `stored-effect-parity/cycles-${exit}-short`,
        cleanupCycles(exit, 8),
        Target.wasm32UnknownUnknown,
      )
      const long = yield* lowerStored(
        `stored-effect-parity/cycles-${exit}-long`,
        cleanupCycles(exit, 80),
        Target.wasm32UnknownUnknown,
      )
      const shortWasm = yield* Backend.emit(WasmBackend.WasmBackend, short.module, {
        mode: 'release',
      })
      const longWasm = yield* Backend.emit(WasmBackend.WasmBackend, long.module, {
        mode: 'release',
      })
      assert.strictEqual(shortWasm._tag, 'WebAssemblyModuleArtifact', exit)
      assert.strictEqual(longWasm._tag, 'WebAssemblyModuleArtifact', exit)
      if (shortWasm._tag !== 'WebAssemblyModuleArtifact') return
      if (longWasm._tag !== 'WebAssemblyModuleArtifact') return
      const shortRun = yield* runWasmMeasured(shortWasm.bytes)
      const longRun = yield* runWasmMeasured(longWasm.bytes)
      assert.strictEqual(shortRun.value, 42, `${exit} short cycles`)
      assert.strictEqual(longRun.value, 42, `${exit} long cycles`)
      // Ten times the cycles must not cost more heap: a capture leaked once per cycle would grow it.
      assert.strictEqual(longRun.pages, shortRun.pages, `${exit} heap growth across cycles`)
    }
  }),
)
