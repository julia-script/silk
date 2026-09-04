import { assert, it } from '@effect/vitest'
import * as NativeToolchain from '@silklang/compiler/NativeToolchain'
import type * as Project from '@silklang/compiler/Project'
import * as SourceFile from '@silklang/compiler/SourceFile'
import * as Target from '@silklang/compiler/Target'
import * as TargetSelector from '@silklang/compiler/TargetSelector'
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
      targets: ['host'] as const,
      outputDirectory,
      artifact,
      nativeLinkInputs: [],
    }),
  })

it('plans deterministic backend/target/profile/package destinations', () => {
  const first = BuildPlan.make(project(), {
    target: Target.aarch64AppleDarwin,
    profile: 'debug',
  })
  const second = BuildPlan.make(project(), {
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

it('uses platform library filenames and rejects library plans for wasm or run', () => {
  const shared = BuildPlan.make(project('answer', '/workspace/build', 'NativeSharedLibrary'), {
    target: Target.aarch64AppleDarwin,
    profile: 'release',
  })
  const archive = BuildPlan.make(project('answer', '/workspace/build', 'NativeStaticLibrary'), {
    target: Target.x8664UnknownLinuxGnu,
    profile: 'release',
  })
  assert.strictEqual(Result.isSuccess(shared), true)
  assert.strictEqual(Result.isSuccess(archive), true)
  if (Result.isSuccess(shared)) assert.match(shared.success.destination, /libanswer\.dylib$/)
  if (Result.isSuccess(archive)) assert.match(archive.success.destination, /libanswer\.a$/)

  const wasm = BuildPlan.make(project('answer', '/workspace/build', 'NativeSharedLibrary'), {
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
    target: host.success,
    profile: 'release',
    purpose: 'run',
  })
  assert.strictEqual(Result.isFailure(run), true)
  if (Result.isFailure(run)) assert.strictEqual(run.failure.reason._tag, 'NonExecutableRunArtifact')
})

it('keeps run host-only', () => {
  const host = TargetSelector.resolve('host', NativeToolchain.hostSelection())
  assert.strictEqual(Result.isSuccess(host), true)
  if (Result.isFailure(host)) return
  const accepted = BuildPlan.make(project(), {
    target: host.success,
    profile: 'debug',
    purpose: 'run',
  })
  assert.strictEqual(Result.isSuccess(accepted), true)
})

it('guards the plan against a non-portable project value', () => {
  const planned = BuildPlan.make(project('Not Portable'), {
    target: Target.aarch64AppleDarwin,
    profile: 'debug',
  })
  assert.strictEqual(Result.isFailure(planned), true)
  if (Result.isFailure(planned))
    assert.strictEqual(planned.failure.reason._tag, 'InvalidPackageName')
})
