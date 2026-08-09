import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Schema from 'effect/Schema'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const Blocker = Schema.Struct({
  phase: Schema.String,
  code: Schema.String,
  message: Schema.String,
})

const Manifest = Schema.Struct({
  schemaVersion: Schema.Literal(1),
  id: Schema.String,
  title: Schema.String,
  status: Schema.Literals(['executable', 'frontier']),
  source: Schema.String,
  input: Schema.String,
  expected: Schema.Struct({
    entryResult: Schema.Number,
    summary: Schema.String,
  }),
  capabilities: Schema.Array(Schema.String),
  targets: Schema.Array(Schema.Literals(['evaluation', 'native', 'wasm'])),
  blockers: Schema.Array(Blocker),
})

const examplesRoot = fileURLToPath(new URL('../../../examples/algorithms/', import.meta.url))
const exampleIds = readdirSync(examplesRoot)
  .filter((name) => statSync(join(examplesRoot, name)).isDirectory())
  .sort()

const examples = exampleIds.map((id) => {
  const root = join(examplesRoot, id)
  const manifest = Schema.decodeUnknownSync(Manifest)(
    JSON.parse(readFileSync(join(root, 'example.json'), 'utf8')),
  )
  return Object.freeze({
    root,
    manifest,
    bytes: new Uint8Array(readFileSync(join(root, manifest.source))),
    readme: readFileSync(join(root, 'README.md'), 'utf8'),
  })
})

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-algorithm-examples-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang: '/usr/bin/clang',
})

const sourceId = (id: string, target: string): string => `examples/algorithms/${id}/${target}`

const evidence = (self: Analysis.Snapshot) => {
  const diagnostics = Analysis.diagnostics(self)
  if (diagnostics.length > 0) {
    return diagnostics.map(({ phase, code, message }) => Object.freeze({ phase, code, message }))
  }
  const outcome = Analysis.evaluate(self)
  if (outcome._tag !== 'Blocked') return []
  if (outcome.reason._tag === 'RecursiveCycle') {
    return [
      Object.freeze({
        phase: 'evaluation',
        code: outcome.reason._tag,
        message: outcome.reason.cycle.map((instance) => instance.declaration.name).join(' -> '),
      }),
    ]
  }
  return [
    Object.freeze({
      phase: 'evaluation',
      code: outcome.reason._tag,
      message: outcome.reason._tag,
    }),
  ]
}

it('keeps six complete, readable programs and explicit status contracts', () => {
  assert.deepEqual(exampleIds, [
    'crc-32',
    'fft',
    'game-of-life',
    'matrix-multiplication',
    'quicksort',
    'sieve',
  ])
  assert.deepEqual(
    examples
      .filter(({ manifest }) => manifest.status === 'executable')
      .map(({ manifest }) => manifest.id),
    ['crc-32', 'game-of-life', 'matrix-multiplication', 'sieve'],
  )
  for (const { root, manifest, bytes, readme } of examples) {
    assert.strictEqual(manifest.id, root.slice(root.lastIndexOf('/') + 1))
    assert.strictEqual(existsSync(join(root, manifest.source)), true)
    assert.isAbove(bytes.length, 100)
    assert.isAbove(readme.length, 100)
    assert.isAbove(manifest.input.length, 10)
    assert.isAbove(manifest.expected.summary.length, 20)
    assert.isAbove(manifest.capabilities.length, 2)
    assert.deepEqual(manifest.targets, ['evaluation', 'native', 'wasm'])
    assert.strictEqual(Number.isInteger(manifest.expected.entryResult), true)
    assert.strictEqual(manifest.blockers.length === 0, manifest.status === 'executable')
  }
})

it.effect('keeps frontier evidence normalized and deterministic on both targets', () =>
  Effect.gen(function* () {
    for (const { manifest, bytes } of examples) {
      if (manifest.status !== 'frontier') continue
      for (const target of ['aarch64-apple-darwin', 'wasm32-unknown-unknown'] as const) {
        const first = yield* Analysis.ofSource(sourceId(manifest.id, target), bytes, target)
        const second = yield* Analysis.ofSource(sourceId(manifest.id, target), bytes, target)
        assert.deepEqual(evidence(first), manifest.blockers, `${manifest.id} ${target}`)
        assert.deepEqual(evidence(second), manifest.blockers, `${manifest.id} ${target}`)
        assert.deepEqual(evidence(first), evidence(second), `${manifest.id} changed evidence`)
      }
    }
  }),
)

it.effect(
  'executes the baseline through evaluation, native, and direct WebAssembly with exact parity',
  () =>
    Effect.gen(function* () {
      for (const { manifest, bytes } of examples) {
        if (manifest.status !== 'executable') continue
        const native = yield* Analysis.ofSource(
          sourceId(manifest.id, 'native'),
          bytes,
          'aarch64-apple-darwin',
        )
        assert.deepEqual(Analysis.diagnostics(native), [], manifest.id)
        const evaluated = Analysis.evaluate(native)
        assert.strictEqual(evaluated._tag, 'Completed', manifest.id)
        if (evaluated._tag !== 'Completed') continue
        assert.strictEqual(evaluated.result._tag, 'I32Value', manifest.id)
        if (evaluated.result._tag !== 'I32Value') continue
        assert.strictEqual(evaluated.result.value, manifest.expected.entryResult, manifest.id)

        const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
        assert.isAbove(nativeArtifact.bitcode.length, 0, manifest.id)

        const wasm = yield* Analysis.ofSource(
          sourceId(manifest.id, 'wasm'),
          bytes,
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(Analysis.diagnostics(wasm), [], manifest.id)
        const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
        const instance = new WebAssembly.Instance(
          new WebAssembly.Module(wasmArtifact.bytes.slice()),
          {},
        )
        const wasmMain = instance.exports.silk_main
        assert.strictEqual(typeof wasmMain, 'function', manifest.id)
        if (typeof wasmMain !== 'function') continue
        assert.strictEqual(wasmMain(), manifest.expected.entryResult, manifest.id)

        const compiled = yield* Driver.compile({
          compilation: {
            root: SourceFile.make(sourceId(manifest.id, 'native-process'), bytes),
          },
          toolchain,
          profile: 'release',
          destination: join(destinationRoot, manifest.id),
        }).pipe(Effect.provide(SourceResolver.memory(new Map())))
        assert.strictEqual(compiled._tag, 'Compiled', manifest.id)
        if (compiled._tag !== 'Compiled') continue
        const process = spawnSync(compiled.path, [], { encoding: 'utf8' })
        assert.strictEqual(process.status, manifest.expected.entryResult, process.stderr)
      }
    }),
  20_000,
)
