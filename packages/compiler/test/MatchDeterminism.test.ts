import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps every match phase byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(new URL('./fixtures/match-determinism.mjs', import.meta.url))
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly semantic: ReadonlyArray<unknown>
    readonly native: string
    readonly wasm: string
  }
  assert.strictEqual(encoded.semantic.length, 1)
  assert.strictEqual(encoded.native.length, 64)
  assert.strictEqual(encoded.wasm.length, 64)
})
