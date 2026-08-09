import { assert, it } from '@effect/vitest'
import * as Result from 'effect/Result'
import * as Backend from '../src/Backend.js'
import * as BackendRegistry from '../src/BackendRegistry.js'
import * as Target from '../src/Target.js'

it('resolves stable backend ids independently from targets', () => {
  const llvm = BackendRegistry.resolve('llvm')
  const wasm = BackendRegistry.resolve('wasm')
  assert.strictEqual(Result.isSuccess(llvm), true)
  assert.strictEqual(Result.isSuccess(wasm), true)
  if (Result.isSuccess(llvm)) assert.strictEqual(llvm.success, Backend.LlvmBackend)
  if (Result.isSuccess(wasm)) assert.strictEqual(wasm.success.id, 'wasm')
})

it('rejects unknown ids and incompatible pairs as typed data', () => {
  const unknown = BackendRegistry.resolve('native')
  assert.strictEqual(Result.isFailure(unknown), true)
  if (Result.isFailure(unknown)) assert.strictEqual(unknown.failure.reason._tag, 'UnknownBackend')

  const wasm = BackendRegistry.resolve('wasm')
  assert.strictEqual(Result.isSuccess(wasm), true)
  if (Result.isFailure(wasm)) return
  const incompatible = BackendRegistry.requireTarget(wasm.success, Target.aarch64AppleDarwin)
  assert.strictEqual(Result.isFailure(incompatible), true)
  if (Result.isFailure(incompatible)) {
    assert.strictEqual(incompatible.failure.reason._tag, 'IncompatibleTarget')
  }
})

it('supports both LLVM and direct Wasm for the canonical WebAssembly target', () => {
  for (const id of ['llvm', 'wasm'] as const) {
    const resolved = BackendRegistry.resolve(id)
    assert.strictEqual(Result.isSuccess(resolved), true)
    if (Result.isFailure(resolved)) continue
    const compatible = BackendRegistry.requireTarget(resolved.success, Target.wasm32UnknownUnknown)
    assert.strictEqual(Result.isSuccess(compatible), true)
  }
})
