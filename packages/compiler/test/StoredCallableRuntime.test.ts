import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as Mir from '../src/Mir.js'
import * as Ownership from '../src/Ownership.js'
import * as Target from '../src/Target.js'
import * as WasmBackend from '../src/WasmBackend.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const lowerStored = Effect.fnUntraced(function* (
  name: string,
  source: string,
  target: Target.Target,
) {
  const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), target.id)
  assert.notInclude(
    Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
    'SEM0103',
  )
  return { snapshot, module: Analysis.loweredMir(snapshot) }
})

const completedValue = (outcome: BootstrapEvaluation.Outcome): number => {
  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return unreachable('expected completed evaluation')
  assert.strictEqual(outcome.result._tag, 'IntegerValue')
  if (outcome.result._tag !== 'IntegerValue') return unreachable('expected i32 result')
  return Number(outcome.result.value)
}

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

const named = `struct Parser<F: fn(i32) -> i32> { parse: F }
fn decode(value: i32) -> i32 { return value + 2 }
pub fn main() -> i32 {
  let parser = Parser { parse: decode }
  return parser.parse(40)
}`

const copiedCapture = `struct Parser<F: fn(i32) -> i32> { parse: F }
pub fn main() -> i32 {
  let parser = Parser { parse: i32.add(2) }
  return parser.parse(40)
}`

const sharedReuse = `struct Parser<F: fn(i32) -> i32> { parse: F }
pub fn main() -> i32 {
  let parser = Parser { parse: i32.add(1) }
  return parser.parse(20) + parser.parse(20)
}`

const nested = `struct Parser<F: fn(i32) -> i32> { parse: F }
struct Boxed<F: fn(i32) -> i32> { inner: Parser<F> }
fn box<F: fn(i32) -> i32>(inner: Parser<F>) -> Boxed<F> {
  return Boxed<F> { inner: move inner }
}
pub fn main() -> i32 {
  let parser = Parser { parse: i32.add(2) }
  let boxed = box(move parser)
  return boxed.inner.parse(40)
}`

const takeDeclarations = `struct Token { value: i32 }
struct Holder<F: once fn(i32) -> i32> { step: F }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
`

const uncalled = `${takeDeclarations}pub fn main() -> i32 {
  let token = Token { value: 2 }
  let holder = Holder { step: consume(move token) }
  return 42
}`

const called = `${takeDeclarations}pub fn main() -> i32 {
  let token = Token { value: 2 }
  let holder = Holder { step: consume(move token) }
  return holder.step(40)
}`

const moved = `${takeDeclarations}fn keep<F: once fn(i32) -> i32>(holder: Holder<F>) -> i32 {
  return 42
}
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let holder = Holder { step: consume(move token) }
  return keep(move holder)
}`

const scopedBorrow = `struct Holder<F: mut fn(i32) -> i32> { step: F }
fn write(value: i32, values: &mut [i32]) -> i32 {
  values[0] = value
  return values[0]
}
pub fn main() -> i32 {
  let mut values = [0]
  let mut holder = Holder { step: write(&mut values) }
  let result = holder.step(42)
  drop holder
  return values[0] + result - 42
}`

const specializedCapture = `struct Token<T> { value: T }
struct Holder<F: once fn(i32) -> i32> { step: F }
fn consume<T>(value: i32, token: Token<T>) -> i32 { return value }
fn apply<T>(token: Token<T>, value: i32) -> i32 {
  let holder = Holder { step: consume<T>(move token) }
  return holder.step(value)
}
pub fn main() -> i32 {
  return apply<i32>(Token<i32> { value: 1 }, 20) + apply<bool>(Token<bool> { value: true }, 22)
}`

const equalShapeSpecializations = `struct Holder<F: fn(i32) -> i32> { step: F }
fn apply<T>(marker: T, value: i32) -> i32 {
  let holder = Holder { step: i32.add(1) }
  return holder.step(value)
}
pub fn main() -> i32 { return apply<i32>(0, 20) + apply<bool>(true, 20) }`

const borrowedCallableLoopExits = `struct Cell { value: i32 }
fn read(value: i32, cell: &Cell) -> i32 { return value + cell.value }
fn returnInside(flag: bool, cell: &Cell) -> i32 {
  let step = read(cell)
  while flag { return step(0) }
  return 0
}
fn exitLoop(flag: bool, cell: &Cell) -> i32 {
  let step = read(cell)
  while flag { break }
  return step(0)
}
fn continueLoop(cell: &Cell) -> i32 {
  let step = read(cell)
  let mut count = 0
  while count < 1 {
    count = count + 1
    continue
  }
  return step(0)
}
fn repeatLoop(cell: &Cell) -> i32 {
  let step = read(cell)
  let mut count = 0
  while count < 1 { count = count + 1 }
  return step(0)
}
pub fn main() -> i32 {
  let cell = Cell { value: 42 }
  return returnInside(true, &cell) + returnInside(false, &cell)
    + exitLoop(true, &cell) + exitLoop(false, &cell)
    + continueLoop(&cell) + repeatLoop(&cell)
}`

type CleanupExit = 'uncalled' | 'consuming' | 'moved' | 'typed-failure'

const cleanupProgram = (dropBody: string, exit: CleanupExit) => `struct Guard {
  tag: i32
  storage: Allocation
}
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    ${dropBody}
  }
}
struct Holder<F: once fn(i32) -> i32> { step: F }
fn consume(value: i32, guard: Guard) -> i32 { return value + guard.tag }
fn keep<F: once fn(i32) -> i32>(holder: Holder<F>) -> i32 { return 42 }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let guard = Guard { tag: 2, storage: move allocation }
  let holder = Holder { step: consume(move guard) }
  ${
    exit === 'consuming'
      ? 'return holder.step(40)'
      : exit === 'moved'
        ? 'return keep(move holder)'
        : exit === 'typed-failure'
          ? 'fail OutOfMemoryError {}'
          : 'return 42'
  }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

const hookOnlyCleanupProgram = (exit: CleanupExit) => `struct Guard<F: once fn(i32) -> i32> {
  tag: i32
  marker: F
}
impl<F: once fn(i32) -> i32> Drop for Guard<F> {
  fn drop(self: &mut Guard<F>) -> () {
    let observable = self.tag / 0
    return ()
  }
}
fn marker(value: i32) -> i32 { return value }
struct Holder<F: once fn(i32) -> i32> { step: F }
fn consume<F: once fn(i32) -> i32>(value: i32, guard: Guard<F>) -> i32 {
  return value + guard.tag
}
fn keep<F: once fn(i32) -> i32>(holder: Holder<F>) -> i32 { return 42 }
effect fn build() -> i32 ! OutOfMemoryError {
  let guard = Guard { tag: 2, marker: marker }
  let holder = Holder { step: consume(move guard) }
  ${
    exit === 'consuming'
      ? 'return holder.step(40)'
      : exit === 'moved'
        ? 'return keep(move holder)'
        : exit === 'typed-failure'
          ? 'fail OutOfMemoryError {}'
          : 'return 42'
  }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

const cleanupExits = ['uncalled', 'consuming', 'moved', 'typed-failure'] as const

const typedFailure = `${takeDeclarations}effect fn build() -> i32 ! OutOfMemoryError {
  let token = Token { value: 2 }
  let holder = Holder { step: consume(move token) }
  fail OutOfMemoryError {}
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

const runtimeMatrix = [
  { source: named, target: 'decode' },
  { source: copiedCapture, target: 'silk_i32_add' },
  { source: sharedReuse, target: 'silk_i32_add' },
  { source: nested, target: 'silk_i32_add' },
  { source: uncalled, target: 'consume' },
  { source: called, target: 'consume' },
  { source: moved, target: 'consume' },
  { source: scopedBorrow, target: 'write' },
  { source: specializedCapture, target: 'consume' },
  { source: equalShapeSpecializations, target: 'silk_i32_add' },
] as const

it.effect('executes the stored-callable matrix in evaluator and direct Wasm', () =>
  Effect.gen(function* () {
    for (const [ordinal, testCase] of runtimeMatrix.entries()) {
      const { snapshot, module } = yield* lowerStored(
        `stored-callable-runtime/matrix-${ordinal}`,
        testCase.source,
        Target.wasm32UnknownUnknown,
      )
      const evaluated = BootstrapEvaluation.evaluate(snapshot.instances, module)
      assert.strictEqual(completedValue(evaluated), 42)

      const artifact = yield* Backend.emit(WasmBackend.WasmBackend, module, { mode: 'release' })
      assert.strictEqual(artifact._tag, 'WebAssemblyModuleArtifact')
      if (artifact._tag !== 'WebAssemblyModuleArtifact') return
      assert.strictEqual(yield* runWasm(artifact.bytes), 42, `runtime matrix case ${ordinal}`)
      assert.notInclude(artifact.wat, 'call_indirect')
      assert.notInclude(artifact.wat, '(table ')
    }
  }),
)

it.effect('lowers the same stored-callable matrix through static native LLVM targets', () =>
  Effect.gen(function* () {
    const target = yield* Target.host()
    for (const [ordinal, testCase] of runtimeMatrix.entries()) {
      const { module } = yield* lowerStored(
        `stored-callable-runtime/native-${ordinal}`,
        testCase.source,
        target,
      )
      const artifact = yield* Backend.emit(Backend.LlvmBackend, module, { mode: 'release' })
      assert.strictEqual(artifact._tag, 'LlvmBitcodeArtifact')
      if (artifact._tag !== 'LlvmBitcodeArtifact') return
      assert.include(artifact.ir, 'define i32 @silk_main')
      assert.include(artifact.ir, testCase.target)
    }
  }),
)

it.effect('executes stored-callable cleanup and scoped-borrow traces exactly once', () =>
  Effect.gen(function* () {
    const cases = [
      { source: uncalled, applies: 0, cleanups: 1 },
      { source: copiedCapture, applies: 1, cleanups: 0 },
      { source: called, applies: 1, cleanups: 0 },
      { source: moved, applies: 0, cleanups: 1 },
      { source: scopedBorrow, applies: 1, cleanups: 0 },
    ] as const
    for (const [ordinal, testCase] of cases.entries()) {
      const { snapshot, module } = yield* lowerStored(
        `stored-callable-runtime/cleanup-${ordinal}`,
        testCase.source,
        Target.wasm32UnknownUnknown,
      )
      const outcome = BootstrapEvaluation.evaluate(snapshot.instances, module)
      assert.strictEqual(completedValue(outcome), 42)
      const callableEvents = outcome.trace.filter(
        (event): event is BootstrapEvaluation.CallableTraceEvent =>
          event._tag === 'CallableConstruct' ||
          event._tag === 'CallableApply' ||
          event._tag === 'CallableCleanup' ||
          event._tag === 'CallableRejected',
      )
      assert.strictEqual(
        callableEvents.filter((event) => event._tag === 'CallableConstruct').length,
        1,
      )
      assert.strictEqual(
        callableEvents.filter((event) => event._tag === 'CallableApply').length,
        testCase.applies,
      )
      assert.strictEqual(
        callableEvents.filter((event) => event._tag === 'CallableCleanup').length,
        testCase.cleanups,
      )
      assert.notInclude(
        callableEvents.map((event) => event._tag),
        'CallableRejected',
      )
    }
  }),
)

it.effect('ends a borrowed callable loan on every terminal and loop-exit path', () =>
  Effect.gen(function* () {
    const { snapshot, module } = yield* lowerStored(
      'stored-callable-runtime/borrowed-loop-exits',
      borrowedCallableLoopExits,
      Target.wasm32UnknownUnknown,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Mir.verify(module), [])
    assert.strictEqual(
      completedValue(BootstrapEvaluation.evaluate(snapshot.instances, module)),
      210,
    )

    const wasm = yield* Backend.emit(WasmBackend.WasmBackend, module, { mode: 'release' })
    assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact')
    if (wasm._tag !== 'WebAssemblyModuleArtifact') return
    assert.strictEqual(yield* runWasm(wasm.bytes), 210)
  }),
)

it.effect('cleans an uncalled stored callable exactly once on a typed-failure exit', () =>
  Effect.gen(function* () {
    const { snapshot, module } = yield* lowerStored(
      'stored-callable-runtime/typed-failure',
      typedFailure,
      Target.wasm32UnknownUnknown,
    )
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, module)
    assert.strictEqual(completedValue(outcome), 42)
    const callableEvents = outcome.trace.filter(
      (event): event is BootstrapEvaluation.CallableTraceEvent =>
        event._tag === 'CallableConstruct' ||
        event._tag === 'CallableApply' ||
        event._tag === 'CallableCleanup' ||
        event._tag === 'CallableRejected',
    )
    const cleanup = callableEvents.filter((event) => event._tag === 'CallableCleanup')
    const cleanedTicket = cleanup.at(0)?.ticket
    assert.strictEqual(cleanup.length, 1)
    assert.notStrictEqual(cleanedTicket, undefined)
    assert.include(
      callableEvents
        .filter((event) => event._tag === 'CallableConstruct')
        .map((event) => event.ticket),
      cleanedTicket,
    )
    assert.notInclude(
      callableEvents.filter((event) => event._tag === 'CallableApply').map((event) => event.ticket),
      cleanedTicket,
    )

    const wasm = yield* Backend.emit(WasmBackend.WasmBackend, module, { mode: 'release' })
    assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact')
    if (wasm._tag !== 'WebAssemblyModuleArtifact') return
    assert.strictEqual(yield* runWasm(wasm.bytes), 42)
    assert.notInclude(wasm.wat, 'call_indirect')

    const host = yield* Target.host()
    const native = yield* lowerStored(
      'stored-callable-runtime/typed-failure-native',
      typedFailure,
      host,
    )
    const llvm = yield* Backend.emit(Backend.LlvmBackend, native.module, { mode: 'release' })
    assert.strictEqual(llvm._tag, 'LlvmBitcodeArtifact')
    if (llvm._tag !== 'LlvmBitcodeArtifact') return
    assert.include(llvm.ir, 'consume')
  }),
)

it.effect(
  'executes owned callable capture hooks and resource cleanup on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const host = yield* Target.host()
      for (const exit of cleanupExits) {
        const resourceCleanup = cleanupProgram('return ()', exit)
        const wasm = yield* Analysis.ofSourceRealized(
          `stored-callable-runtime/resource-cleanup-${exit}-wasm`,
          ascii(resourceCleanup),
          Target.wasm32UnknownUnknown.id,
        )
        assert.deepEqual(Analysis.diagnostics(wasm), [], exit)
        const evaluated = Analysis.evaluate(wasm)
        assert.strictEqual(completedValue(evaluated), 42, exit)
        assert.strictEqual(
          evaluated.trace.filter(
            (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
          ).length,
          1,
          `${exit} evaluator Drop hook`,
        )
        assert.strictEqual(
          evaluated.trace.filter((event) => event._tag === 'AllocationAcquire').length,
          1,
          `${exit} evaluator allocation acquire`,
        )
        assert.strictEqual(
          evaluated.trace.filter((event) => event._tag === 'AllocationRelease').length,
          1,
          `${exit} evaluator allocation release`,
        )

        const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
        assert.strictEqual(yield* runWasm(wasmArtifact.bytes), 42, `${exit} Wasm`)
        assert.include(wasmArtifact.wat, 'drop_impl')
        assert.notInclude(wasmArtifact.wat, 'call_indirect')
        assert.notInclude(wasmArtifact.wat, '(table ')

        const native = yield* Analysis.ofSourceRealized(
          `stored-callable-runtime/resource-cleanup-${exit}-native`,
          ascii(resourceCleanup),
          host.id,
        )
        assert.deepEqual(Analysis.diagnostics(native), [], exit)
        const llvm = yield* Analysis.codegen(native, { mode: 'release' })
        assert.include(llvm.ir, 'drop_impl_0')
        assert.include(llvm.ir, 'call void @free')

        const observableDrop = cleanupProgram('let boom = self.tag / 0 return ()', exit)
        const trappingWasm = yield* Analysis.ofSourceRealized(
          `stored-callable-runtime/observable-drop-${exit}-wasm`,
          ascii(observableDrop),
          Target.wasm32UnknownUnknown.id,
        )
        assert.deepEqual(Analysis.diagnostics(trappingWasm), [], exit)
        const trappingEvaluation = Analysis.evaluate(trappingWasm)
        assert.strictEqual(trappingEvaluation._tag, 'Trap', exit)
        assert.strictEqual(
          trappingEvaluation.trace.filter(
            (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
          ).length,
          1,
          `${exit} trapping evaluator Drop hook`,
        )
        const trappingWasmArtifact = yield* Analysis.codegenWasm(trappingWasm, {
          mode: 'release',
        })
        const wasmExit = yield* Effect.exit(runWasm(trappingWasmArtifact.bytes))
        assert.strictEqual(wasmExit._tag, 'Failure', `${exit} trapping Wasm`)

        const hookOnlyDrop = hookOnlyCleanupProgram(exit)
        const hookOnlyNative = yield* Analysis.ofSourceRealized(
          `stored-callable-runtime/hook-only-${exit}-native`,
          ascii(hookOnlyDrop),
          host.id,
        )
        assert.deepEqual(Analysis.diagnostics(hookOnlyNative), [], `${exit} hook-only native`)
        const hookOnlyMir = Analysis.loweredMir(hookOnlyNative)
        const hookOnlyOperations = hookOnlyMir.functions.flatMap(Mir.operations)
        assert.isFalse(
          hookOnlyOperations.some(
            (operation) => operation._tag === 'Allocate' || operation._tag.startsWith('RawBuffer'),
          ),
          `${exit} hook-only MIR has no allocation operations`,
        )
        const hookOnlyCleanups = hookOnlyOperations.flatMap((operation) =>
          operation._tag === 'Drop' || operation._tag === 'SlotDrop' ? [operation.cleanup] : [],
        )
        assert.isTrue(
          hookOnlyCleanups.some(
            (cleanup) => Ownership.cleanupHasHook(cleanup) && !Ownership.cleanupReclaims(cleanup),
          ),
          `${exit} MIR retains a hook-only cleanup plan`,
        )
        const hookOnlyArtifact = yield* Analysis.codegen(hookOnlyNative, { mode: 'release' })
        assert.match(hookOnlyArtifact.ir, /call void @silk_[^(\n]*drop_impl_0/)
        assert.notInclude(hookOnlyArtifact.ir, 'call void @free')
      }
    }),
  300_000,
)
