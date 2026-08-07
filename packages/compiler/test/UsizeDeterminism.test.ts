import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps exact Usize phases byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(new URL('./fixtures/usize-determinism.mjs', import.meta.url))
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly exact: string
    readonly native: string
    readonly wasm: string
  }
  assert.include(encoded.exact, '9007199254740993')
  assert.strictEqual(encoded.native.length, 64)
  assert.strictEqual(encoded.wasm.length, 64)
})
