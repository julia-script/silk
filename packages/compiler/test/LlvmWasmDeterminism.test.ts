import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps LLVM wasm IR and bitcode byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(new URL('./fixtures/llvm-wasm-determinism.mjs', import.meta.url))
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()
  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly tag: string
    readonly ir: string
    readonly bitcode: string
    readonly symbols: ReadonlyArray<string>
  }
  assert.strictEqual(encoded.tag, 'LlvmBitcodeArtifact')
  assert.strictEqual(encoded.ir.length, 64)
  assert.strictEqual(encoded.bitcode.length, 64)
  assert.include(encoded.symbols, 'silk_main')
}, 20_000)
