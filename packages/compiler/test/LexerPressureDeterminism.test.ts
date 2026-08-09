import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps lexer pressure phases and artifacts byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(
    new URL('./fixtures/lexer-pressure-determinism.mjs', import.meta.url),
  )
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly native: {
      readonly diagnostics: ReadonlyArray<unknown>
      readonly modules: ReadonlyArray<string>
      readonly outcome: string
      readonly allocations: ReadonlyArray<string>
    }
    readonly wasm: {
      readonly diagnostics: ReadonlyArray<unknown>
      readonly modules: ReadonlyArray<string>
      readonly outcome: string
      readonly allocations: ReadonlyArray<string>
    }
    readonly nativeBytes: string
    readonly wasmBytes: string
  }
  assert.deepEqual(encoded.native.diagnostics, [])
  assert.deepEqual(encoded.wasm.diagnostics, [])
  assert.include(encoded.native.modules, 'silk/vector')
  assert.include(encoded.wasm.modules, 'silk/vector')
  assert.strictEqual(encoded.native.outcome, 'Completed')
  assert.strictEqual(encoded.wasm.outcome, 'Completed')
  assert.deepEqual(encoded.wasm.allocations, encoded.native.allocations)
  assert.strictEqual(encoded.nativeBytes.length, 64)
  assert.strictEqual(encoded.wasmBytes.length, 64)
}, 45_000)
