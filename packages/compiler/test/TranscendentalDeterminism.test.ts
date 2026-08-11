import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps transcendental traces and artifacts byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(
    new URL('./fixtures/transcendental-determinism.mjs', import.meta.url),
  )
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly result: number
    readonly native: string
    readonly wasm: string
    readonly nativeIr: string
    readonly wasmIr: string
  }
  assert.strictEqual(encoded.result, 42)
  assert.strictEqual(encoded.native.length, 64)
  assert.strictEqual(encoded.wasm.length, 64)
  assert.notInclude(encoded.nativeIr, 'llvm.sin')
  assert.notInclude(encoded.nativeIr, 'llvm.cos')
  assert.notInclude(encoded.wasmIr, '(import')
}, 90_000)
