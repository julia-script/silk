import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Process from './support/Process.js'

const encoder = new TextEncoder()
const module = 'opaque/engine-parity'
const nativeTarget = 'aarch64-apple-darwin'
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang: '/usr/bin/clang',
  shimCache: NativeToolchain.makeShimCache(),
})

const programs = Object.freeze([
  Object.freeze({
    name: 'exact-callable',
    source: `fn add(left: i32, right: i32) -> i32 { return left + right }
fn selected() -> typeof(add) { return add }
pub fn main() -> i32 { let operation = selected() return operation(40, 2) }`,
  }),
  Object.freeze({
    name: 'opaque-callable',
    source: `fn add(left: i32, right: i32) -> i32 { return left + right }
fn make(value: i32) -> some<F: fn(i32) -> i32> F { return add(value) }
pub fn main() -> i32 {
  let first = make(40)
  let second = make(1)
  return first(1) + second(0)
}`,
  }),
  Object.freeze({
    name: 'opaque-effect',
    source: `fn make(value: i32) -> some<F: Effect<i32>> F {
  return effect { return value }
}
pub fn main() -> i32 { return run make(42) }`,
  }),
])

const snapshot = (source: string, target: string) =>
  Analysis.ofSourceRealized(module, encoder.encode(source), target)

const assertStaticMir = (self: Analysis.Snapshot, name: string): void => {
  assert.strictEqual(self.mir._tag, 'Available', name)
  if (self.mir._tag !== 'Available') return
  const encoded = Mir.encode(self.mir.value)
  assert.notInclude(encoded, 'OpaqueRepresentationArgument', name)
  assert.notInclude(encoded, 'unavailable body', name)
  assert.notInclude(encoded, 'unavailable contract type', name)
  assert.strictEqual(
    self.mir.value.functions
      .flatMap(Mir.operations)
      .some((operation) => operation._tag === 'Allocate'),
    false,
    name,
  )
}

const nativeIndirectCalls = (ir: string): ReadonlyArray<string> =>
  ir.split('\n').filter((line) => /\b(?:call|invoke)\b[^(]*%[-.\w"]+\s*\(/.test(line))

const executeWasm = Effect.fnUntraced(function* (bytes: Uint8Array) {
  return yield* Effect.try(() => {
    const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
    const main = instance.exports.silk_main
    return typeof main === 'function'
      ? Object.freeze({ _tag: 'Completed', value: main() })
      : Object.freeze({ _tag: 'MissingMain' })
  })
})

it.effect(
  'executes exact and opaque representations identically on bootstrap, Wasm, and native engines',
  () =>
    Effect.gen(function* () {
      const fileSystem = yield* FileSystem.FileSystem
      const path = yield* Path.Path
      const destinationRoot = yield* fileSystem.makeTempDirectoryScoped()
      for (const program of programs) {
        const wasmSnapshot = yield* snapshot(program.source, 'wasm32-unknown-unknown')
        assert.deepEqual(
          wasmSnapshot.diagnostics.map((diagnostic) => diagnostic.code),
          [],
          program.name,
        )
        assertStaticMir(wasmSnapshot, program.name)
        const interpreted = Analysis.evaluate(wasmSnapshot)
        assert.strictEqual(
          interpreted._tag,
          'Completed',
          `${program.name}: ${interpreted._tag === 'Blocked' ? JSON.stringify(interpreted.reason) : interpreted._tag}`,
        )
        if (interpreted._tag !== 'Completed') continue
        assert.strictEqual(interpreted.result.value, 42, program.name)

        const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
        const wasmExecution = yield* executeWasm(wasm.bytes)
        assert.strictEqual(wasmExecution._tag, 'Completed', program.name)
        if (wasmExecution._tag !== 'Completed') continue
        assert.strictEqual(wasmExecution.value, 42, program.name)
        assert.notInclude(wasm.wat, 'call_indirect', program.name)
        assert.notInclude(wasm.wat, 'memory.grow', program.name)

        const nativeSnapshot = yield* snapshot(program.source, nativeTarget)
        assertStaticMir(nativeSnapshot, program.name)
        const nativeArtifact = yield* Analysis.codegen(nativeSnapshot, { mode: 'release' })
        assert.notInclude(nativeArtifact.ir, 'OpaqueRepresentationArgument', program.name)
        assert.notInclude(nativeArtifact.ir, '@malloc', program.name)
        assert.notInclude(nativeArtifact.ir, '@calloc', program.name)
        assert.deepEqual(
          nativeIndirectCalls(nativeArtifact.ir),
          [],
          `${program.name}: native indirect call`,
        )

        const compiled = yield* Driver.compile({
          compilation: { root: SourceFile.make(module, encoder.encode(program.source)) },
          toolchain,
          profile: 'release',
          destination: path.join(destinationRoot, program.name),
        }).pipe(Effect.provide(SourceResolver.empty))
        assert.strictEqual(compiled._tag, 'Compiled', program.name)
        if (compiled._tag !== 'Compiled') continue
        const native = yield* Process.run(compiled.path, [])
        assert.strictEqual(native.exitCode, 42, `${program.name}: ${native.stderr}`)
      }
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  180_000,
)
