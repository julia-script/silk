import { spawnSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps logging phases and artifacts byte-identical across fresh processes', () => {
  const fixture = fileURLToPath(new URL('./fixtures/logging-determinism.mjs', import.meta.url))
  const run = () => spawnSync(process.execPath, [fixture], { encoding: 'utf8' })
  const first = run()
  const second = run()

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly diagnostics: ReadonlyArray<unknown>
    readonly modules: ReadonlyArray<string>
    readonly hir: string
    readonly nativeOutcome: { readonly _tag: string }
    readonly wasmOutcome: { readonly _tag: string }
    readonly native: string
    readonly wasm: string
    readonly hostImports: ReadonlyArray<unknown>
  }
  assert.deepEqual(encoded.diagnostics, [])
  assert.include(encoded.modules, 'silk/logging')
  assert.include(encoded.hir, 'service-call silk/logging.Logger.log')
  assert.strictEqual(encoded.nativeOutcome._tag, 'Completed')
  assert.strictEqual(encoded.wasmOutcome._tag, 'Completed')
  assert.deepEqual(encoded.hostImports, [])
  assert.strictEqual(encoded.native.length, 64)
  assert.strictEqual(encoded.wasm.length, 64)
}, 180_000)
