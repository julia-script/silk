import { spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const bytes = new Uint8Array(
  readFileSync(new URL('./fixtures/runtime-slice-exclusive.silk', import.meta.url)),
)
const moduleName = 'runtime-slice-acceptance/main'
const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-runtime-slice-acceptance-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect(
  'keeps exclusive move-only replacement and cleanup in parity across all three engines',
  () =>
    Effect.gen(function* () {
      const native = yield* Analysis.ofSource(moduleName, bytes, 'aarch64-apple-darwin')
      const wasm = yield* Analysis.ofSource(moduleName, bytes, 'wasm32-unknown-unknown')
      assert.deepEqual(Analysis.diagnostics(native), [])
      assert.deepEqual(Analysis.diagnostics(wasm), [])

      const evaluated = Analysis.evaluate(native)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)
      assert.strictEqual(
        evaluated.trace.filter((event) => event._tag === 'ReplacementCleanup').length,
        1,
      )
      assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'Replacement').length, 1)

      const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
      const wasmInstance = new WebAssembly.Instance(
        new WebAssembly.Module(wasmArtifact.bitcode.slice()),
        {},
      )
      const wasmMain = wasmInstance.exports.silk_main as () => number
      assert.strictEqual(wasmMain(), 42)

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make(moduleName, bytes) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, 'exclusive'),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') return
      const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42, run.stderr)
    }),
)
