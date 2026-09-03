import { assert, it } from '@effect/vitest'
import * as LlvmBackend from '@silklang/compiler/LlvmBackend'
import * as NativeToolchain from '@silklang/compiler/NativeToolchain'
import type * as Project from '@silklang/compiler/Project'
import * as SourceFile from '@silklang/compiler/SourceFile'
import * as Target from '@silklang/compiler/Target'
import * as TargetSelector from '@silklang/compiler/TargetSelector'
import * as WasmBackend from '@silklang/compiler/WasmBackend'
import * as Result from 'effect/Result'
import * as BuildPlan from '../src/BuildPlan.js'

const project = (
  name = 'hello',
  outputDirectory = '/workspace/build',
  artifact: Project.Project['build']['artifact'] = 'NativeExecutable',
): Project.Project =>
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
    build: Object.freeze({
      backend: 'llvm',
      targets: ['host'] as const,
      outputDirectory,
      artifact,
      nativeLinkInputs: [],
    }),
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

it('uses platform library filenames and rejects library plans for wasm or run', () => {
  const shared = BuildPlan.make(project('answer', '/workspace/build', 'NativeSharedLibrary'), {
    backend: LlvmBackend.LlvmBackend,
    target: Target.aarch64AppleDarwin,
    profile: 'release',
  })
  const archive = BuildPlan.make(project('answer', '/workspace/build', 'NativeStaticLibrary'), {
    backend: LlvmBackend.LlvmBackend,
    target: Target.x8664UnknownLinuxGnu,
    profile: 'release',
  })
  assert.strictEqual(Result.isSuccess(shared), true)
  assert.strictEqual(Result.isSuccess(archive), true)
  if (Result.isSuccess(shared)) assert.match(shared.success.destination, /libanswer\.dylib$/)
  if (Result.isSuccess(archive)) assert.match(archive.success.destination, /libanswer\.a$/)

  const wasm = BuildPlan.make(project('answer', '/workspace/build', 'NativeSharedLibrary'), {
    backend: LlvmBackend.LlvmBackend,
    target: Target.wasm32UnknownUnknown,
    profile: 'release',
  })
  assert.strictEqual(Result.isFailure(wasm), true)
  if (Result.isFailure(wasm))
    assert.strictEqual(wasm.failure.reason._tag, 'IncompatibleArtifactTarget')

  const host = TargetSelector.resolve('host', NativeToolchain.hostSelection())
  assert.strictEqual(Result.isSuccess(host), true)
  if (Result.isFailure(host)) return
  const run = BuildPlan.make(project('answer', '/workspace/build', 'NativeStaticLibrary'), {
    backend: LlvmBackend.LlvmBackend,
    target: host.success,
    profile: 'release',
    purpose: 'run',
  })
  assert.strictEqual(Result.isFailure(run), true)
  if (Result.isFailure(run)) assert.strictEqual(run.failure.reason._tag, 'NonExecutableRunArtifact')
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
  const host = TargetSelector.resolve('host', NativeToolchain.hostSelection())
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
