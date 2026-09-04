import { assert, it } from '@effect/vitest'
import * as Result from 'effect/Result'
import * as BackendRegistry from '../src/BackendRegistry.js'
import * as LlvmBackend from '../src/LlvmBackend.js'
import * as Target from '../src/Target.js'

it('resolves stable backend ids independently from targets', () => {
  const llvm = BackendRegistry.resolve('llvm')
  assert.strictEqual(Result.isSuccess(llvm), true)
  if (Result.isSuccess(llvm)) assert.strictEqual(llvm.success, LlvmBackend.LlvmBackend)
})

it('rejects unknown ids as typed data', () => {
  const unknown = BackendRegistry.resolve('not-a-backend')
  assert.strictEqual(Result.isFailure(unknown), true)
  if (Result.isFailure(unknown)) assert.strictEqual(unknown.failure.reason._tag, 'UnknownBackend')
})

it('uses LLVM for the canonical WebAssembly target', () => {
  const resolved = BackendRegistry.resolve('llvm')
  assert.strictEqual(Result.isSuccess(resolved), true)
  if (Result.isFailure(resolved)) return
  const compatible = BackendRegistry.requireTarget(resolved.success, Target.wasm32UnknownUnknown)
  assert.strictEqual(Result.isSuccess(compatible), true)
})
