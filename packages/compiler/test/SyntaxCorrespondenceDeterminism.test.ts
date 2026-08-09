import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps syntax correspondence evidence byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(
    new URL('./fixtures/syntax-correspondence-determinism.mjs', import.meta.url),
  )
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()
  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  assert.isAbove(JSON.parse(first.stdout).counts.correspondingElements, 0)
})
