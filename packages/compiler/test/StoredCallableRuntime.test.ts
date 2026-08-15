import { spawnSync } from 'node:child_process'
import { existsSync } from 'node:fs'
import { join } from 'node:path'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as Target from '../src/Target.js'
import * as WasmBackend from '../src/WasmBackend.js'
import { unreachable } from './support/raise.js'

const clang =
  process.env.SILK_TEST_CLANG ??
  (existsSync('/opt/homebrew/opt/llvm/bin/clang')
    ? '/opt/homebrew/opt/llvm/bin/clang'
    : existsSync('/usr/local/opt/llvm/bin/clang')
      ? '/usr/local/opt/llvm/bin/clang'
      : 'clang')
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  shimCache: NativeToolchain.makeShimCache(),
})

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const lowerStored = (name: string, source: string, target: Target.Target) =>
  Effect.gen(function* () {
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
  assert.strictEqual(outcome.result._tag, 'I32Value')
  if (outcome.result._tag !== 'I32Value') return unreachable('expected i32 result')
  return outcome.result.value
}

const runWasm = (bytes: Uint8Array): number => {
  const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
  const main = instance.exports.silk_main
  assert.isFunction(main)
  return typeof main === 'function' ? (main() as number) : unreachable('expected Wasm main')
}

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

const typedFailure = `${takeDeclarations}effect fn build() -> i32 ! OutOfMemory {
  let token = Token { value: 2 }
  let holder = Holder { step: consume(move token) }
  fail OutOfMemory {}
}
effect fn recover(error: OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

const runtimeMatrix = [
  { source: named, target: 'decode' },
  { source: copiedCapture, target: 'silk_i32_add' },
  { source: sharedReuse, target: 'silk_i32_add' },
  { source: nested, target: 'silk_i32_add' },
  { source: uncalled, target: 'consume' },
  { source: called, target: 'consume' },
  { source: moved, target: 'consume' },
  { source: scopedBorrow, target: 'write' },
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
      assert.strictEqual(runWasm(artifact.bytes), 42, `runtime matrix case ${ordinal}`)
      assert.notInclude(artifact.wat, 'call_indirect')
      assert.notInclude(artifact.wat, '(table ')
    }
  }),
)

it.effect(
  'executes the same stored-callable matrix through static native LLVM targets',
  () =>
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
        NativeToolchain.withBuildScope(`stored-callable-${ordinal}`, (scope) => {
          const object = NativeToolchain.emitObject(toolchain, scope, artifact, target, 'release')
          const shim = NativeToolchain.compileShim(toolchain, scope, target, artifact.termination)
          assert.strictEqual(object._tag, 'ObjectArtifact')
          assert.strictEqual(shim._tag, 'ObjectArtifact')
          if (object._tag !== 'ObjectArtifact' || shim._tag !== 'ObjectArtifact') return
          const executable = NativeToolchain.ClangLinker.link(
            toolchain,
            target,
            [object.artifact, shim.artifact],
            [],
            join(scope.root, 'program'),
          )
          assert.strictEqual(executable._tag, 'Executable')
          if (executable._tag !== 'Executable') return
          const run = spawnSync(executable.path, [], { encoding: 'utf8' })
          assert.strictEqual(run.signal, null)
          assert.strictEqual(run.stderr, '')
          assert.strictEqual(run.status, 42)
        })
      }
    }),
  300_000,
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

it.effect(
  'cleans an uncalled stored callable exactly once on a typed-failure exit',
  () =>
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
        callableEvents
          .filter((event) => event._tag === 'CallableApply')
          .map((event) => event.ticket),
        cleanedTicket,
      )

      const wasm = yield* Backend.emit(WasmBackend.WasmBackend, module, { mode: 'release' })
      assert.strictEqual(wasm._tag, 'WebAssemblyModuleArtifact')
      if (wasm._tag !== 'WebAssemblyModuleArtifact') return
      assert.strictEqual(runWasm(wasm.bytes), 42)
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
      NativeToolchain.withBuildScope('stored-callable-typed-failure', (scope) => {
        const object = NativeToolchain.emitObject(toolchain, scope, llvm, host, 'release')
        const shim = NativeToolchain.compileShim(toolchain, scope, host, llvm.termination)
        assert.strictEqual(object._tag, 'ObjectArtifact')
        assert.strictEqual(shim._tag, 'ObjectArtifact')
        if (object._tag !== 'ObjectArtifact' || shim._tag !== 'ObjectArtifact') return
        const executable = NativeToolchain.ClangLinker.link(
          toolchain,
          host,
          [object.artifact, shim.artifact],
          [],
          join(scope.root, 'program'),
        )
        assert.strictEqual(executable._tag, 'Executable')
        if (executable._tag !== 'Executable') return
        const run = spawnSync(executable.path, [], { encoding: 'utf8' })
        assert.strictEqual(run.signal, null)
        assert.strictEqual(run.stderr, '')
        assert.strictEqual(run.status, 42)
      })
    }),
  60_000,
)
