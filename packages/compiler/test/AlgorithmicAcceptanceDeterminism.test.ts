import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps the composed algorithm byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(
    new URL('./fixtures/algorithmic-acceptance-determinism.mjs', import.meta.url),
  )
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly closure: ReadonlyArray<string>
    readonly nativeBytes: string
    readonly wasmBytes: string
    readonly nativeText: string
    readonly wasmText: string
  }
  assert.deepEqual(encoded.closure, [
    'app/Main',
    'compiler/Coverage',
    'compiler/Member',
    'silk/option',
    'silk/usize',
  ])
  assert.strictEqual(encoded.nativeBytes.length, 64)
  assert.strictEqual(encoded.wasmBytes.length, 64)
  assert.strictEqual(encoded.nativeText.length, 64)
  assert.strictEqual(encoded.wasmText.length, 64)
}, 90_000)
