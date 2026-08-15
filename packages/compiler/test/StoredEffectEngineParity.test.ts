import { NodeServices } from '@effect/platform-node'
import { assert, layer } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as ContinuationLayout from '../src/ContinuationLayout.js'
import * as Layout from '../src/Layout.js'
import * as Lower from '../src/Lower.js'
import * as Mir from '../src/Mir.js'
import * as MirNormalization from '../src/MirNormalization.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as OpaqueRealization from '../src/OpaqueRealization.js'
import type * as Ownership from '../src/Ownership.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as StandardStreams from '../src/StandardStreams.js'
import * as SuspensionMir from '../src/SuspensionMir.js'
import * as SuspensionOwnership from '../src/SuspensionOwnership.js'
import * as Target from '../src/Target.js'
import * as WasmBackend from '../src/WasmBackend.js'
import * as Process from './support/Process.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const escapeRegExp = (value: string): string => value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')

const clangPath = Effect.fnUntraced(function* () {
  const configured = yield* Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(''))
  if (configured.length > 0) return configured
  const fileSystem = yield* FileSystem.FileSystem
  for (const candidate of ['/opt/homebrew/opt/llvm/bin/clang', '/usr/local/opt/llvm/bin/clang'])
    if (yield* fileSystem.exists(candidate)) return candidate
  return 'clang'
})

/**
 * Takes one already-lowered LLVM artifact through object emission, shim compilation, linking, and
 * execution. `SEM0107` still fences `Driver.compile`, so this is the native path task 4.2 can use.
 */
const runNative = Effect.fnUntraced(function* (
  toolchain: NativeToolchain.Toolchain,
  artifact: Backend.LlvmBitcodeArtifact,
  target: Target.Target,
) {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const destination = path.join(yield* fileSystem.makeTempDirectoryScoped(), 'program')
  const linked = NativeToolchain.withBuildScope('stored-effect-parity', (scope) => {
    const object = NativeToolchain.emitObject(toolchain, scope, artifact, target, 'release')
    if (object._tag !== 'ObjectArtifact') return object
    const shim = NativeToolchain.compileShim(
      toolchain,
      scope,
      target,
      artifact.termination,
      artifact.nativeRuntimeSymbols,
    )
    if (shim._tag !== 'ObjectArtifact') return shim
    return NativeToolchain.ClangLinker.link(
      toolchain,
      target,
      [object.artifact, shim.artifact],
      [],
      destination,
    )
  })
  assert.strictEqual(
    linked._tag,
    'Executable',
    linked._tag === 'ToolchainFailure' ? linked.output : linked._tag,
  )
  if (linked._tag !== 'Executable') return unreachable('expected a native executable')
  return yield* Process.run(linked.path, [])
})

const makeToolchain = Effect.fnUntraced(function* () {
  return Object.freeze({
    _tag: 'Toolchain' as const,
    clang: yield* clangPath(),
    shimCache: NativeToolchain.makeShimCache(),
  })
})

const lowerSource = Effect.fnUntraced(function* (
  name: string,
  source: string,
  target: Target.Target,
  fence: 'stored' | 'open',
) {
  const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), target.id)
  const diagnostics = Analysis.diagnostics(snapshot)
  if (fence === 'stored') {
    assert.isTrue(diagnostics.length > 0, name)
    assert.isTrue(
      diagnostics.every((diagnostic) => diagnostic.code === 'SEM0107'),
      JSON.stringify(diagnostics.map(({ code, message }) => ({ code, message }))),
    )
  } else {
    assert.deepEqual(
      diagnostics,
      [],
      JSON.stringify(diagnostics.map(({ code, message }) => ({ code, message }))),
    )
  }
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
  return yield* lowerSource(name, source, target, 'stored')
})

const emitLlvm = Effect.fnUntraced(function* (module: Mir.Module, label: string) {
  const llvm = yield* Backend.emit(Backend.LlvmBackend, module, { mode: 'release' })
  assert.strictEqual(llvm._tag, 'LlvmBitcodeArtifact', label)
  if (llvm._tag !== 'LlvmBitcodeArtifact') return unreachable('expected LLVM artifact')
  return llvm
})

const assertLlvmDropCall = (ir: string, module: Mir.Module, label: string): void => {
  const dropFn =
    module.functions.find((candidate) => candidate.id.name.startsWith('drop@impl')) ??
    unreachable(`expected an emitted Drop hook for ${label}`)
  const dropSymbol = Backend.symbolFor(dropFn, Mir.machineEntry(module))
  assert.match(
    ir,
    new RegExp(`\\bcall\\b[^\\n]*@${escapeRegExp(dropSymbol)}\\(`),
    `${label} LLVM Drop call`,
  )
}

/**
 * A Drop that divides by zero on first entry. Completing with 42 means the hook never ran; a trap
 * or any other exit is the ≥1 signal. Pair with the poison program (exit 42) to pin exactly once.
 */
const assertNativeWitnessTraps = Effect.fnUntraced(function* (
  toolchain: NativeToolchain.Toolchain,
  module: Mir.Module,
  host: Target.Target,
  label: string,
) {
  const llvm = yield* emitLlvm(module, `${label} witness`)
  const outcome = yield* Effect.exit(runNative(toolchain, llvm, host))
  assert.isFalse(
    outcome._tag === 'Success' && Number(outcome.value.exitCode) === 42,
    `${label} native witness must trap rather than return 42`,
  )
})

/** Matches `heapTableBase` in WasmBackend: free-list heads live at the first Wasm heap page. */
const wasmPageBytes = 65536
const wasmHeapTableBase = wasmPageBytes

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
const emittedFunction = (module: Mir.Module, runner: { module: string; name: string }) => {
  const candidates = module.functions.filter((candidate) => candidate.id.module === runner.module)
  // Provider specialization renames a runner in place (`f$effect$-1` -> `f$effect$-1$provided$N`),
  // so accept the exact runner or its single specialization, and nothing else.
  return (
    candidates.find((candidate) => candidate.id.name === runner.name) ??
    candidates.find((candidate) => candidate.id.name.startsWith(`${runner.name}$provided$`)) ??
    unreachable(`expected an emitted function for ${runner.module}.${runner.name}`)
  )
}

/** The exact debug-WAT call target for one runner, including the suspend-step suffix when needed. */
const emittedWasmCallTarget = (
  module: Mir.Module,
  runner: { module: string; name: string },
): string => {
  const fn = emittedFunction(module, runner)
  const symbol = Backend.symbolFor(fn, Mir.machineEntry(module))
  return (fn.suspension?.classification ?? 'Synchronous') !== 'Synchronous'
    ? `${symbol}$suspend_step`
    : symbol
}

const assertDirectWasmRunner = (
  wat: string,
  module: Mir.Module,
  runner: { module: string; name: string },
  label: string,
): void => {
  const target = emittedWasmCallTarget(module, runner)
  assert.include(wat, `call $${target}`, `${label} direct call $${target}`)
  assert.notInclude(wat, 'call_indirect', label)
}

/**
 * Pins the LLVM call instruction, not merely that the runner symbol is defined.
 *
 * A `define ... @symbol` line would satisfy a substring check; `call ... @symbol(` is the
 * static dispatch the stored run must emit, including `$suspend_step` when the runner suspends.
 */
const assertDirectLlvmRunner = (
  ir: string,
  module: Mir.Module,
  runner: { module: string; name: string },
  label: string,
): void => {
  const target = emittedWasmCallTarget(module, runner)
  assert.match(
    ir,
    new RegExp(`\\bcall\\b[^\\n]*@${escapeRegExp(target)}\\(`),
    `${label} LLVM call @${target}`,
  )
}

const dropCalls = (outcome: BootstrapEvaluation.Outcome): number =>
  outcome._tag === 'Completed'
    ? outcome.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
      ).length
    : 0

const traceCount = (
  outcome: BootstrapEvaluation.Outcome,
  tag: BootstrapEvaluation.TraceEvent['_tag'],
): number =>
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

layer(NodeServices.layer)('stored Effect engine parity', (it) => {
  it.effect(
    'executes stored Effects with identical results and runner identity in every engine',
    () =>
      Effect.gen(function* () {
        const host = yield* Target.host()
        const toolchain = yield* makeToolchain()
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

          // Debug WAT names the exact call target; release would only give a numeric index.
          const wasm = yield* Backend.emit(WasmBackend.WasmBackend, wasmLowering.module, {
            mode: 'debug',
          })
          assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact', testCase.name)
          if (wasm._tag !== 'WebAssemblyModuleArtifact') return
          assert.strictEqual(yield* runWasm(wasm.bytes), testCase.result, `${testCase.name} Wasm`)
          assertDirectWasmRunner(wasm.wat, wasmLowering.module, realization.runner, testCase.name)

          const nativeLowering = yield* lowerStored(moduleName, testCase.source, host)
          const nativeRunner = storedRunner(nativeLowering.module, testCase.name)
          // The native lowering carries the same exact rows the Wasm lowering and evaluator proved.
          assertExactRows(nativeLowering.module, testCase.name)
          const llvm = yield* emitLlvm(nativeLowering.module, testCase.name)
          assert.include(llvm.ir, 'define i32 @silk_main', testCase.name)
          assertDirectLlvmRunner(
            llvm.ir,
            nativeLowering.module,
            nativeRunner.realization.runner,
            testCase.name,
          )
          assert.deepEqual(
            nativeRunner.realization.runner,
            realization.runner,
            `${testCase.name} runner identity across targets`,
          )
          const nativeRun = yield* runNative(toolchain, llvm, host)
          assert.strictEqual(
            Number(nativeRun.exitCode),
            testCase.result,
            `${testCase.name} native: ${nativeRun.stderr}`,
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
            assertDirectLlvmRunner(
              llvm.ir,
              nativeLowering.module,
              specialized.runner,
              `${testCase.name} specialized`,
            )
            assertDirectWasmRunner(
              wasm.wat,
              wasmLowering.module,
              specialized.runner,
              `${testCase.name} specialized`,
            )
          }
        }
      }).pipe(Effect.scoped),
    180_000,
  )

  type CleanupExit = 'success' | 'unrun' | 'failure'
  type DropKind = 'poison' | 'witness'

  /**
   * Shared stored-Effect cleanup surface.
   *
   * Poison: first Drop zeros `tag`, a second Drop traps, so a duplicate cannot return 42.
   * Witness: the first Drop divides by zero, so a missing Drop returns 42 and a real Drop traps.
   * Together those two native processes pass only for exactly one Drop. A mut-borrow ticket cannot
   * be read after a stored Effect is dropped (OWN0011 / InvalidLoan), so native cannot count in
   * process; Wasm still uses a non-zero class-0 free-list head at `wasmHeapTableBase` as the
   * missing-cleanup signal for the poison program.
   */
  const cleanupSurface = (kind: DropKind = 'poison'): string => `struct Problem { code: i32 }
struct Guard { tag: i32 storage: Allocation }
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    ${
      kind === 'witness'
        ? `let boom = self.tag / 0
    return ()`
        : `if self.tag == 0 {
      let boom = 1 / 0
      return ()
    }
    self.tag = 0
    return ()`
    }
  }
}
struct Deferred<A, !E, ?R, F: once Effect<A ! E ? R>> { operation: F }
fn defer<A, !E, ?R, F: once Effect<A ! E ? R>>(
  operation: F
) -> Deferred<A, E, R, F> {
  return Deferred<A, E, R> { operation: move operation }
}
effect fn succeeding(guard: Guard) -> i32 {
  return 40 + guard.tag
}
effect fn failing(guard: Guard) -> i32 ! Problem {
  let result = guard.tag
  if result == 0 { return 0 }
  fail Problem { code: result }
}`

  /**
   * Repeats one stored-Effect construct-and-clean cycle `count` times.
   *
   * Failure recovers inside each iteration so the loop can actually repeat. Each capture is 2KiB
   * (`[i64; 256]`) so 80 leaked cycles must grow the Wasm heap past a 64KiB page; an `i32` leak
   * would stay inside the first page and the comparison would pass. A double Drop traps.
   */
  const cleanupCycles = (
    exit: Exclude<CleanupExit, 'success'>,
    count: number,
  ): string => `${cleanupSurface()}
effect fn cycle() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let storage = run Allocator.allocate(Layout.of<[i64; 256]>())
  let guard = Guard { tag: 7, storage: move storage }
  let deferred = defer(failing(move guard))
  ${exit === 'failure' ? 'return run deferred.operation' : 'return 42'}
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 42 }
effect fn drive() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let mut index = 0
  let mut total = 0
  while index < ${count} {
    total = total + run Effect.catch(cycle(), recover)
    index = index + 1
  }
  return total
}
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let total = run Effect.catch(drive() |> Effect.provideMut(&mut allocator), recover)
  if total == ${count * 42} { return 42 }
  return 1
}`

  const pagesOf = (instance: WebAssembly.Instance): number => {
    const memory = instance.exports[StandardStreams.wasmMemoryExport]
    assert.instanceOf(memory, WebAssembly.Memory)
    return memory instanceof WebAssembly.Memory
      ? memory.buffer.byteLength / wasmPageBytes
      : Number.POSITIVE_INFINITY
  }

  /** Runs a Wasm artifact and reports both its result and the heap it needed. */
  const runWasmMeasured = Effect.fnUntraced(function* (bytes: Uint8Array) {
    return yield* Effect.try(() => {
      const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
      const main = instance.exports.silk_main
      if (typeof main !== 'function') return unreachable('expected Wasm main')
      const value = main()
      const memory = instance.exports[StandardStreams.wasmMemoryExport]
      const class0 =
        memory instanceof WebAssembly.Memory
          ? new DataView(memory.buffer).getInt32(wasmHeapTableBase, true)
          : 0
      return Object.freeze({ value, pages: pagesOf(instance), class0 })
    })
  })

  const cleanupProgram = (exit: CleanupExit, kind: DropKind = 'poison'): string => {
    const tag = exit === 'success' ? 2 : 7
    const failures = exit === 'success' ? 'OutOfMemory' : 'Problem | OutOfMemory'
    const recoverValue = exit === 'success' ? 0 : 42
    const construct = exit === 'success' ? 'succeeding(move guard)' : 'failing(move guard)'
    const runLine = exit === 'unrun' ? 'return 42' : 'return run deferred.operation'
    return `${cleanupSurface(kind)}
effect fn recover(error: ${failures}) -> i32 { return ${recoverValue} }
effect fn build() -> i32 ! ${failures} {
  let mut allocator = SystemAllocator.make()
  let storage = run Allocator.allocate(Layout.of<i32>()) |> Effect.provideMut(&mut allocator)
  let guard = Guard { tag: ${tag}, storage: move storage }
  let deferred = defer(${construct})
  ${runLine}
}
pub fn main() -> i32 { return run Effect.catch(build(), recover) }`
  }

  it.effect(
    'cleans successful, unrun, and failing stored Effect environments exactly once in every engine',
    () =>
      Effect.gen(function* () {
        const host = yield* Target.host()
        const toolchain = yield* makeToolchain()
        for (const exit of ['success', 'unrun', 'failure'] as const) {
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

          const wasm = yield* Backend.emit(WasmBackend.WasmBackend, module, { mode: 'debug' })
          assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact', exit)
          if (wasm._tag !== 'WebAssemblyModuleArtifact') return
          const wasmRun = yield* runWasmMeasured(wasm.bytes)
          // 42 is not enough: poison rejects 2, a non-zero class-0 free-list head rejects 0.
          assert.strictEqual(wasmRun.value, 42, `${exit} Wasm`)
          assert.notStrictEqual(wasmRun.class0, 0, `${exit} Wasm capture returned to class 0`)
          assert.notInclude(wasm.wat, 'call_indirect', exit)
          if (exit !== 'unrun') {
            assertDirectWasmRunner(
              wasm.wat,
              module,
              storedRunner(module, exit).realization.runner,
              exit,
            )
          }

          const native = yield* lowerStored(moduleName, cleanupProgram(exit), host)
          const llvm = yield* emitLlvm(native.module, exit)
          assertLlvmDropCall(llvm.ir, native.module, exit)
          if (exit !== 'unrun') {
            assertDirectLlvmRunner(
              llvm.ir,
              native.module,
              storedRunner(native.module, exit).realization.runner,
              `${exit} native`,
            )
          }
          const nativeRun = yield* runNative(toolchain, llvm, host)
          assert.strictEqual(Number(nativeRun.exitCode), 42, `${exit} native: ${nativeRun.stderr}`)

          const witness = yield* lowerStored(
            `${moduleName}-witness`,
            cleanupProgram(exit, 'witness'),
            host,
          )
          yield* assertNativeWitnessTraps(toolchain, witness.module, host, exit)

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
          assert.strictEqual(runnerCalls, exit === 'unrun' ? 0 : 1, `${exit} stored runner calls`)
          if (exit !== 'unrun') {
            assertExactRows(module, exit)
            assertExactRows(native.module, `${exit} native`)
          }
          if (exit === 'failure') {
            assert.isAbove(traceCount(outcome, 'EffectFailure'), 0, 'typed failure trace')
          }
        }
      }).pipe(Effect.scoped),
    240_000,
  )

  const suspendingProgram = (kind: DropKind = 'poison'): string => `${cleanupSurface(kind)}
effect fn delayed(guard: Guard) -> i32 ! OutOfMemory ? &mut Allocator {
  let base = run Effect.suspend(effect { return 40 })
  return base + guard.tag
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
effect fn build() -> i32 ! OutOfMemory ? &mut Allocator {
  let storage = run Allocator.allocate(Layout.of<i32>())
  let guard = Guard { tag: 2, storage: move storage }
  let deferred = defer(delayed(move guard))
  return run deferred.operation
}
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(build() |> Effect.provideMut(&mut allocator), recover)
}`

  it.effect(
    'resumes a suspending stored Effect with matching cleanup in every engine',
    () =>
      Effect.gen(function* () {
        const host = yield* Target.host()
        const toolchain = yield* makeToolchain()
        const moduleName = 'stored-effect-parity/suspending'
        const { snapshot, module } = yield* lowerStored(
          moduleName,
          suspendingProgram(),
          Target.wasm32UnknownUnknown,
        )
        const outcome = BootstrapEvaluation.evaluate(snapshot.instances, module)
        assert.strictEqual(completedValue(outcome), 42)
        assert.strictEqual(dropCalls(outcome), 1)
        assert.isAbove(traceCount(outcome, 'SuspensionOrigin'), 0)
        assertExactRows(module, 'suspending')

        const wasm = yield* Backend.emit(WasmBackend.WasmBackend, module, { mode: 'debug' })
        assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact')
        if (wasm._tag !== 'WebAssemblyModuleArtifact') return
        const wasmRun = yield* runWasmMeasured(wasm.bytes)
        assert.strictEqual(wasmRun.value, 42)
        assert.notStrictEqual(wasmRun.class0, 0, 'suspending Wasm capture returned to class 0')
        assertDirectWasmRunner(
          wasm.wat,
          module,
          storedRunner(module, 'suspending').realization.runner,
          'suspending',
        )

        const native = yield* lowerStored(moduleName, suspendingProgram(), host)
        const llvm = yield* emitLlvm(native.module, 'suspending')
        assert.include(llvm.ir, 'define i32 @silk_main')
        assertLlvmDropCall(llvm.ir, native.module, 'suspending')
        assertDirectLlvmRunner(
          llvm.ir,
          native.module,
          storedRunner(native.module, 'suspending').realization.runner,
          'suspending',
        )
        const nativeRun = yield* runNative(toolchain, llvm, host)
        assert.strictEqual(Number(nativeRun.exitCode), 42, `suspending native: ${nativeRun.stderr}`)

        const witness = yield* lowerStored(
          `${moduleName}-witness`,
          suspendingProgram('witness'),
          host,
        )
        yield* assertNativeWitnessTraps(toolchain, witness.module, host, 'suspending')
      }).pipe(Effect.scoped),
    240_000,
  )

  /**
   * Keeps `count` 2KiB allocations live until `main` returns. Wasm does not shrink pages on free, so
   * this is the missing-cleanup control: 80 holds must use more pages than 8, or the flat-heap
   * comparison below cannot see a leaked capture.
   */
  const leakHolds = (
    count: number,
  ): string => `effect fn recover(error: OutOfMemory) -> i32 { return 0 }
effect fn drive() -> i32 ! OutOfMemory ? &mut Allocator {
  ${Array.from({ length: count }, (_, index) => `let hold${index} = run Allocator.allocate(Layout.of<[i64; 256]>())`).join('\n  ')}
  return 42
}
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catch(drive() |> Effect.provideMut(&mut allocator), recover)
}`

  const suspendCycles = (count: number): string => `${cleanupSurface()}
effect fn delayed(guard: Guard) -> i32 ! OutOfMemory ? &mut Allocator {
  let base = run Effect.suspend(effect { return 40 })
  return base + guard.tag
}
effect fn cycle() -> i32 ! OutOfMemory ? &mut Allocator {
  let storage = run Allocator.allocate(Layout.of<[i64; 256]>())
  let guard = Guard { tag: 2, storage: move storage }
  let deferred = defer(delayed(move guard))
  return run deferred.operation
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
effect fn drive() -> i32 ! OutOfMemory ? &mut Allocator {
  ${Array.from({ length: count }, (_, index) => `let value${index} = run cycle()`).join('\n  ')}
  return ${Array.from({ length: count }, (_, index) => `value${index}`).join(' + ')}
}
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let total = run Effect.catch(drive() |> Effect.provideMut(&mut allocator), recover)
  if total == ${count * 42} { return 42 }
  return 1
}`

  it.effect('keeps the Wasm heap flat across repeated stored Effect cleanup cycles', () =>
    Effect.gen(function* () {
      const leakShort = yield* lowerSource(
        'stored-effect-parity/cycles-leak-short',
        leakHolds(8),
        Target.wasm32UnknownUnknown,
        'open',
      )
      const leakLong = yield* lowerSource(
        'stored-effect-parity/cycles-leak-long',
        leakHolds(80),
        Target.wasm32UnknownUnknown,
        'open',
      )
      const leakShortWasm = yield* Backend.emit(WasmBackend.WasmBackend, leakShort.module, {
        mode: 'release',
      })
      const leakLongWasm = yield* Backend.emit(WasmBackend.WasmBackend, leakLong.module, {
        mode: 'release',
      })
      assert.strictEqual(leakShortWasm._tag, 'WebAssemblyModuleArtifact', 'leak short')
      assert.strictEqual(leakLongWasm._tag, 'WebAssemblyModuleArtifact', 'leak long')
      if (leakShortWasm._tag !== 'WebAssemblyModuleArtifact') return
      if (leakLongWasm._tag !== 'WebAssemblyModuleArtifact') return
      const leakShortRun = yield* runWasmMeasured(leakShortWasm.bytes)
      const leakLongRun = yield* runWasmMeasured(leakLongWasm.bytes)
      assert.strictEqual(leakShortRun.value, 42, 'leak short')
      assert.strictEqual(leakLongRun.value, 42, 'leak long')
      // Sensitivity: 80 live 2KiB holds must grow past the 8-hold heap, or a missed Drop is invisible.
      assert.isAbove(leakLongRun.pages, leakShortRun.pages, 'leak control must grow Wasm pages')

      const programs = [
        {
          name: 'unrun',
          short: 8,
          long: 80,
          source: (count: number) => cleanupCycles('unrun', count),
        },
        {
          name: 'failure',
          short: 8,
          long: 80,
          source: (count: number) => cleanupCycles('failure', count),
        },
        { name: 'suspending', short: 8, long: 80, source: suspendCycles },
      ] as const
      for (const testCase of programs) {
        const short = yield* lowerStored(
          `stored-effect-parity/cycles-${testCase.name}-short`,
          testCase.source(testCase.short),
          Target.wasm32UnknownUnknown,
        )
        const long = yield* lowerStored(
          `stored-effect-parity/cycles-${testCase.name}-long`,
          testCase.source(testCase.long),
          Target.wasm32UnknownUnknown,
        )
        const shortWasm = yield* Backend.emit(WasmBackend.WasmBackend, short.module, {
          mode: 'release',
        })
        const longWasm = yield* Backend.emit(WasmBackend.WasmBackend, long.module, {
          mode: 'release',
        })
        assert.strictEqual(shortWasm._tag, 'WebAssemblyModuleArtifact', testCase.name)
        assert.strictEqual(longWasm._tag, 'WebAssemblyModuleArtifact', testCase.name)
        if (shortWasm._tag !== 'WebAssemblyModuleArtifact') return
        if (longWasm._tag !== 'WebAssemblyModuleArtifact') return
        const shortRun = yield* runWasmMeasured(shortWasm.bytes)
        const longRun = yield* runWasmMeasured(longWasm.bytes)
        assert.strictEqual(shortRun.value, 42, `${testCase.name} short cycles`)
        assert.strictEqual(longRun.value, 42, `${testCase.name} long cycles`)
        // Same counts as the leak control: cleanup must reuse, so pages stay flat.
        assert.strictEqual(
          longRun.pages,
          shortRun.pages,
          `${testCase.name} heap growth across cycles`,
        )
      }
    }),
  )
})
