import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps intrinsic closure inventories and availability diagnostics stable across fresh processes', () => {
  const fixture = fileURLToPath(
    new URL('./fixtures/intrinsic-availability-determinism.mjs', import.meta.url),
  )
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly closure: string
    readonly diagnostic: ReadonlyArray<{ readonly code: string }>
    readonly hostImports: ReadonlyArray<unknown>
  }
  assert.include(encoded.closure, 'Intrinsic.i32Add')
  assert.deepEqual(
    encoded.diagnostic.map((entry) => entry.code),
    ['SEM0093'],
  )
  assert.deepEqual(encoded.hostImports, [])
})
