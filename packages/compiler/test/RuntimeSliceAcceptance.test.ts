import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const bytes = new Uint8Array(
  readFileSync(new URL('./fixtures/runtime-slice-exclusive.silk', import.meta.url)),
)
const moduleName = 'runtime-slice-acceptance/main'

it.effect(
  'keeps exclusive move-only replacement and cleanup in parity across the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const native = yield* Analysis.ofSourceRealized(moduleName, bytes, 'aarch64-apple-darwin')
      const wasm = yield* Analysis.ofSourceRealized(moduleName, bytes, 'wasm32-unknown-unknown')
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
        new WebAssembly.Module(wasmArtifact.bytes.slice()),
        {},
      )
      const wasmMain = wasmInstance.exports.silk_main as () => number
      assert.strictEqual(wasmMain(), 42)
    }),
)
