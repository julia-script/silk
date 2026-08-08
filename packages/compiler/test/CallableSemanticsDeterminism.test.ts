import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps callable semantic identities byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(
    new URL('./fixtures/callable-semantics-determinism.mjs', import.meta.url),
  )
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly functions: ReadonlyArray<unknown>
    readonly diagnostics: ReadonlyArray<string>
  }
  assert.strictEqual(encoded.functions.length, 3)
  assert.deepEqual(encoded.diagnostics, [])
})
