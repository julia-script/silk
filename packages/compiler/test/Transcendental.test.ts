import { spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Schema from 'effect/Schema'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as FloatingPoint from '../src/FloatingPoint.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Transcendental from '../src/Transcendental.js'

const Vector = Schema.Struct({
  width: Schema.Literals([32, 64]),
  inputBits: Schema.String,
  operation: Schema.Literals(['Sin', 'Cos']),
  referenceBits: Schema.String,
})

const Fixture = Schema.Struct({
  generator: Schema.String,
  vectors: Schema.Array(Vector),
})

const fixture = Schema.decodeUnknownSync(Fixture)(
  JSON.parse(
    readFileSync(
      fileURLToPath(new URL('./fixtures/transcendental-vectors.json', import.meta.url)),
      'utf8',
    ),
  ),
)

const ulpDistance = (width: 32 | 64, left: bigint, right: bigint): bigint => {
  const leftKey = FloatingPoint.totalOrderKey({ width, bits: left })
  const rightKey = FloatingPoint.totalOrderKey({ width, bits: right })
  return leftKey < rightKey ? rightKey - leftKey : leftKey - rightKey
}

it('stays within four ulp of independently generated high-precision vectors', () => {
  assert.include(fixture.generator, 'bc -l at scale 1200')
  for (const vector of fixture.vectors) {
    const actual = Transcendental.evaluate(vector.operation, {
      width: vector.width,
      bits: BigInt(vector.inputBits),
    })
    assert.isAtMost(
      Number(ulpDistance(vector.width, actual.bits, BigInt(vector.referenceBits))),
      4,
      `${vector.width} ${vector.operation}(${vector.inputBits})`,
    )
  }
})

const executionVectors = [
  ...fixture.vectors,
  { width: 32 as const, inputBits: '0x00000000', operation: 'Sin' as const },
  { width: 32 as const, inputBits: '0x80000000', operation: 'Sin' as const },
  { width: 32 as const, inputBits: '0x00000000', operation: 'Cos' as const },
  { width: 32 as const, inputBits: '0x7f800000', operation: 'Sin' as const },
  { width: 32 as const, inputBits: '0xff800000', operation: 'Cos' as const },
  { width: 32 as const, inputBits: '0x7fc12345', operation: 'Sin' as const },
  { width: 64 as const, inputBits: '0x0000000000000000', operation: 'Sin' as const },
  { width: 64 as const, inputBits: '0x8000000000000000', operation: 'Sin' as const },
  { width: 64 as const, inputBits: '0x0000000000000000', operation: 'Cos' as const },
  { width: 64 as const, inputBits: '0x7ff0000000000000', operation: 'Sin' as const },
  { width: 64 as const, inputBits: '0xfff0000000000000', operation: 'Cos' as const },
  { width: 64 as const, inputBits: '0x7ff8123456789abc', operation: 'Sin' as const },
].map((vector) => {
  const inputBits = BigInt(vector.inputBits)
  return Object.freeze({
    width: vector.width,
    inputBits,
    operation: vector.operation,
    expectedBits: Transcendental.evaluate(vector.operation, {
      width: vector.width,
      bits: inputBits,
    }).bits,
  })
})

const source = `pub fn main() -> i32 {
${executionVectors
  .map(
    (vector, index) =>
      `  if f${vector.width}.toBits(f${vector.width}.${vector.operation.toLowerCase()}(f${vector.width}.fromBits(${vector.inputBits.toString()}))) != ${vector.expectedBits.toString()} { return ${index + 1} }`,
  )
  .join('\n')}
  return 42
}`

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-transcendental-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang: '/usr/bin/clang',
})

it.effect(
  'returns identical canonical bits through evaluation, native, and direct Wasm',
  () =>
    Effect.gen(function* () {
      const bytes = new TextEncoder().encode(source)
      const native = yield* Analysis.ofSource(
        'transcendental/native',
        bytes,
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(native), [])
      assert.deepEqual(Mir.verify(Analysis.loweredMir(native)), [])
      const encodedHir = Hir.encode(Analysis.rootAnalysis(native).hir)
      assert.include(encodedHir, 'builtin f32.Sin : f32')
      assert.include(encodedHir, 'builtin f64.Cos : f64')
      const encodedMir = Mir.encode(Analysis.loweredMir(native))
      assert.include(encodedMir, 'float-sin')
      assert.include(encodedMir, ': f32')
      assert.include(encodedMir, 'float-cos')
      assert.include(encodedMir, ': f64')
      const operations = Analysis.loweredMir(native).functions.flatMap(Mir.operations)
      assert.strictEqual(
        operations.filter((operation) => operation._tag === 'FloatTranscendental').length,
        executionVectors.length,
      )
      const first = Analysis.evaluate(native)
      const second = Analysis.evaluate(native)
      assert.deepEqual(first, second)
      assert.strictEqual(first._tag, 'Completed')
      if (first._tag === 'Completed') assert.strictEqual(first.result.value, 42)

      const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
      assert.notInclude(nativeArtifact.ir, 'llvm.sin')
      assert.notInclude(nativeArtifact.ir, 'llvm.cos')
      assert.notInclude(nativeArtifact.ir, ' fast ')

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make('transcendental/process', bytes) },
        toolchain,
        profile: 'release',
        destination: join(destinationRoot, 'program'),
      }).pipe(Effect.provide(SourceResolver.memory(new Map())))
      assert.strictEqual(compiled._tag, 'Compiled', JSON.stringify(compiled))
      if (compiled._tag === 'Compiled') {
        const process = spawnSync(compiled.path, [], { encoding: 'utf8' })
        assert.strictEqual(process.status, 42, process.stderr)
      }

      const wasm = yield* Analysis.ofSource('transcendental/wasm', bytes, 'wasm32-unknown-unknown')
      assert.deepEqual(Analysis.diagnostics(wasm), [])
      const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
      assert.notInclude(wasmArtifact.wat, '(import')
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasmArtifact.bytes.slice()))
      const main = instance.exports.silk_main
      assert.strictEqual(typeof main, 'function')
      if (typeof main === 'function') assert.strictEqual(main(), 42)
    }),
  30_000,
)

it.effect('rejects a mismatched transcendental MIR result before execution', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'transcendental/malformed',
      new TextEncoder().encode('pub fn main() -> i32 { return f64.toI32(f64.cos(1.0)) }'),
      'aarch64-apple-darwin',
    )
    const mir = Analysis.loweredMir(snapshot)
    const transcendental = mir.functions
      .flatMap(Mir.operations)
      .find((operation) => operation._tag === 'FloatTranscendental')
    assert.strictEqual(transcendental?._tag, 'FloatTranscendental')
    if (transcendental?._tag !== 'FloatTranscendental') return
    const malformed: Mir.Module = {
      ...mir,
      functions: mir.functions.map(
        (fn): Mir.MirFunction =>
          Mir.operations(fn).includes(transcendental)
            ? {
                ...fn,
                localTypes: fn.localTypes.map((type, ordinal) =>
                  ordinal === transcendental.destination.ordinal ? { _tag: 'f32' } : type,
                ),
              }
            : fn,
      ),
    }
    assert.include(
      Mir.verify(malformed).map((violation) => violation.rule),
      'InvalidIntegerOperation',
    )
  }),
)
