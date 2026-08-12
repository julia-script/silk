import { execFile } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

it('keeps stack VM pressure phases and artifacts byte-identical across fresh processes', async () => {
  const fixture = fileURLToPath(
    new URL('./fixtures/stack-vm-pressure-determinism.mjs', import.meta.url),
  )
  const run = () =>
    new Promise<{ status: number; stdout: string; stderr: string }>((resolve) => {
      execFile(process.execPath, [fixture], { encoding: 'utf8' }, (error, stdout, stderr) =>
        resolve({ status: error === null ? 0 : 1, stdout, stderr }),
      )
    })
  const [first, second] = await Promise.all([run(), run()])

  assert.strictEqual(first.status, 0, first.stderr)
  assert.strictEqual(second.status, 0, second.stderr)
  assert.strictEqual(first.stdout, second.stdout)
  const encoded = JSON.parse(first.stdout) as {
    readonly native: {
      readonly diagnostics: ReadonlyArray<unknown>
      readonly modules: ReadonlyArray<string>
      readonly outcome: string
      readonly allocations: ReadonlyArray<string>
    }
    readonly wasm: {
      readonly diagnostics: ReadonlyArray<unknown>
      readonly modules: ReadonlyArray<string>
      readonly outcome: string
      readonly allocations: ReadonlyArray<string>
    }
    readonly nativeBytes: string
    readonly wasmBytes: string
    readonly separate: {
      readonly native: {
        readonly diagnostics: ReadonlyArray<unknown>
        readonly outcome: string
        readonly allocations: ReadonlyArray<string>
      }
      readonly wasm: {
        readonly diagnostics: ReadonlyArray<unknown>
        readonly outcome: string
        readonly allocations: ReadonlyArray<string>
      }
      readonly nativeText: string
      readonly wasmText: string
      readonly nativeBytes: string
      readonly wasmBytes: string
    }
  }
  assert.deepEqual(encoded.native.diagnostics, [])
  assert.deepEqual(encoded.wasm.diagnostics, [])
  assert.include(encoded.native.modules, 'silk/vector')
  assert.include(encoded.wasm.modules, 'silk/vector')
  assert.strictEqual(encoded.native.outcome, 'Completed')
  assert.strictEqual(encoded.wasm.outcome, 'Completed')
  assert.deepEqual(encoded.wasm.allocations, encoded.native.allocations)
  assert.strictEqual(encoded.nativeBytes.length, 64)
  assert.strictEqual(encoded.wasmBytes.length, 64)
  assert.deepEqual(encoded.separate.native.diagnostics, [])
  assert.deepEqual(encoded.separate.wasm.diagnostics, [])
  assert.strictEqual(encoded.separate.native.outcome, 'Completed')
  assert.strictEqual(encoded.separate.wasm.outcome, 'Completed')
  assert.deepEqual(encoded.separate.wasm.allocations, encoded.separate.native.allocations)
  assert.strictEqual(encoded.separate.nativeText.length, 64)
  assert.strictEqual(encoded.separate.wasmText.length, 64)
  assert.strictEqual(encoded.separate.nativeBytes.length, 64)
  assert.strictEqual(encoded.separate.wasmBytes.length, 64)
}, 240_000)
