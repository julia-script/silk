import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps scanner phases, stdlib imports, and artifacts byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(new URL('./fixtures/scanner-determinism.mjs', import.meta.url))
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly toolchainIdentity: string
    readonly native: {
      readonly diagnostics: ReadonlyArray<unknown>
      readonly modules: ReadonlyArray<string>
      readonly hir: string
      readonly ownership: string
      readonly instances: string
      readonly layout: string
      readonly mir: string
    }
    readonly wasm: {
      readonly diagnostics: ReadonlyArray<unknown>
      readonly modules: ReadonlyArray<string>
      readonly hir: string
      readonly ownership: string
      readonly instances: string
      readonly layout: string
      readonly mir: string
    }
    readonly nativeBytes: string
  }
  assert.strictEqual(encoded.toolchainIdentity.length, 64)
  assert.deepEqual(encoded.native.diagnostics, [])
  assert.deepEqual(encoded.wasm.diagnostics, [])
  assert.include(encoded.native.modules, 'silk/vector')
  assert.include(encoded.wasm.modules, 'silk/vector')
  for (const snapshot of [encoded.native, encoded.wasm]) {
    assert.strictEqual(snapshot.hir.length, 64)
    assert.strictEqual(snapshot.ownership.length, 64)
    assert.strictEqual(snapshot.instances.length, 64)
    assert.strictEqual(snapshot.layout.length, 64)
    assert.strictEqual(snapshot.mir.length, 64)
  }
  assert.strictEqual(encoded.nativeBytes.length, 64)
}, 120_000)
