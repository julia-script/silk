import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps the composed algorithm byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(
    new URL('./fixtures/algorithmic-acceptance-determinism.mjs', import.meta.url),
  )
  // The encoded snapshot covers every module in the closure, and the closure now reaches the
  // formatting stack through `usize`, so the dump is several megabytes. The default buffer would
  // kill the child and report a null status rather than a difference.
  const run = () =>
    spawnSync(process.execPath, [fixture], { encoding: 'utf8', maxBuffer: 64 * 1024 * 1024 })
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
    'silk/bytes',
    'silk/core',
    'silk/format',
    'silk/i32',
    'silk/i64',
    'silk/layout',
    'silk/option',
    'silk/raw-buffer',
    'silk/result',
    'silk/slot',
    'silk/string',
    'silk/u32',
    'silk/u64',
    'silk/u8',
    'silk/usize',
    'silk/vector',
  ])
  assert.strictEqual(encoded.nativeBytes.length, 64)
  assert.strictEqual(encoded.wasmBytes.length, 64)
  assert.strictEqual(encoded.nativeText.length, 64)
  assert.strictEqual(encoded.wasmText.length, 64)
}, 90_000)
