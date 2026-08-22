import { assert, it } from '@effect/vitest'
import * as LlvmBackend from '@silk-effect/compiler/LlvmBackend'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as Target from '@silk-effect/compiler/Target'
import * as WasmBackend from '@silk-effect/compiler/WasmBackend'
import * as Result from 'effect/Result'
import * as BuildPlan from '../src/BuildPlan.js'
import type * as Project from '../src/Project.js'
import * as TargetSelector from '../src/TargetSelector.js'

const project = (name = 'hello', outputDirectory = '/workspace/build'): Project.Project =>
  Object.freeze({
    _tag: 'Project',
    name,
    version: '0.1.0',
    manifestPath: '/workspace/silk.toml',
    directory: '/workspace',
    entry: Object.freeze({
      _tag: 'SourceEntry',
      module: 'Main',
      path: '/workspace/src/Main.silk',
      sourceRoot: '/workspace/src',
      bytes: SourceFile.toUint8Array(SourceFile.make('Main', new Uint8Array())),
    }),
    build: Object.freeze({ backend: 'llvm', targets: ['host'] as const, outputDirectory }),
  })

it('plans deterministic backend/target/profile/package destinations', () => {
  const first = BuildPlan.make(project(), {
    backend: LlvmBackend.LlvmBackend,
    target: Target.aarch64AppleDarwin,
    profile: 'debug',
  })
  const second = BuildPlan.make(project(), {
    backend: LlvmBackend.LlvmBackend,
    target: Target.aarch64AppleDarwin,
    profile: 'debug',
  })
  assert.deepStrictEqual(first, second)
  assert.strictEqual(Result.isSuccess(first), true)
  if (Result.isSuccess(first)) {
    assert.strictEqual(
      first.success.destination,
      '/workspace/build/llvm/aarch64-apple-darwin/debug/hello',
    )
  }
})

it('selects the wasm extension and prevents backend collisions', () => {
  const llvm = BuildPlan.make(project(), {
    backend: LlvmBackend.LlvmBackend,
    target: Target.wasm32UnknownUnknown,
    profile: 'release',
  })
  const wasm = BuildPlan.make(project('hello', '/workspace/artifacts'), {
    backend: WasmBackend.WasmBackend,
    target: Target.wasm32UnknownUnknown,
    profile: 'release',
  })
  assert.strictEqual(Result.isSuccess(llvm), true)
  assert.strictEqual(Result.isSuccess(wasm), true)
  if (Result.isSuccess(llvm)) {
    assert.strictEqual(
      llvm.success.destination,
      '/workspace/build/llvm/wasm32-unknown-unknown/release/hello.wasm',
    )
  }
  if (Result.isSuccess(wasm)) {
    assert.strictEqual(
      wasm.success.destination,
      '/workspace/artifacts/wasm/wasm32-unknown-unknown/release/hello.wasm',
    )
  }
})

it('rejects incompatible backend-target pairs during planning', () => {
  const planned = BuildPlan.make(project(), {
    backend: WasmBackend.WasmBackend,
    target: Target.aarch64AppleDarwin,
    profile: 'debug',
  })
  assert.strictEqual(Result.isFailure(planned), true)
  if (Result.isFailure(planned)) {
    assert.strictEqual(planned.failure.reason._tag, 'IncompatibleBackendTarget')
  }
})

it('keeps run host-only and requires the LLVM native backend', () => {
  const host = TargetSelector.resolve('host')
  assert.strictEqual(Result.isSuccess(host), true)
  if (Result.isFailure(host)) return
  const accepted = BuildPlan.make(project(), {
    backend: LlvmBackend.LlvmBackend,
    target: host.success,
    profile: 'debug',
    purpose: 'run',
  })
  assert.strictEqual(Result.isSuccess(accepted), true)
})

it('guards the plan against a non-portable project value', () => {
  const planned = BuildPlan.make(project('Not Portable'), {
    backend: LlvmBackend.LlvmBackend,
    target: Target.aarch64AppleDarwin,
    profile: 'debug',
  })
  assert.strictEqual(Result.isFailure(planned), true)
  if (Result.isFailure(planned))
    assert.strictEqual(planned.failure.reason._tag, 'InvalidPackageName')
})
