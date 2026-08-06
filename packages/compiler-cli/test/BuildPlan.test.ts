import { assert, it } from '@effect/vitest'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as Target from '@silk-effect/compiler/Target'
import * as Result from 'effect/Result'
import * as BuildPlan from '../src/BuildPlan.js'
import type * as Project from '../src/Project.js'

const project = (name = 'hello'): Project.Project =>
  Object.freeze({
    _tag: 'Project',
    name,
    manifestPath: '/workspace/silk.toml',
    directory: '/workspace',
    entry: Object.freeze({
      _tag: 'SourceEntry',
      module: 'Main',
      path: '/workspace/src/Main.silk',
      sourceRoot: '/workspace/src',
      bytes: SourceFile.toUint8Array(SourceFile.make('Main', new Uint8Array())),
    }),
  })

it('plans a deterministic target/profile/package destination', () => {
  const first = BuildPlan.make(project(), {
    target: 'aarch64-apple-darwin',
    profile: 'debug',
  })
  const second = BuildPlan.make(project(), {
    target: 'aarch64-apple-darwin',
    profile: 'debug',
  })

  assert.strictEqual(Result.isSuccess(first), true)
  assert.deepStrictEqual(first, second)
  if (Result.isSuccess(first)) {
    assert.strictEqual(
      first.success.destination,
      '/workspace/.silk/build/aarch64-apple-darwin/debug/hello',
    )
    assert.strictEqual(first.success.toolchain.clang, 'clang')
  }
})

it('changes only the profile segment for release planning', () => {
  const planned = BuildPlan.make(project(), {
    target: 'aarch64-apple-darwin',
    profile: 'release',
    clang: '/toolchain/clang',
  })
  assert.strictEqual(Result.isSuccess(planned), true)
  if (Result.isSuccess(planned)) {
    assert.strictEqual(
      planned.success.destination,
      '/workspace/.silk/build/aarch64-apple-darwin/release/hello',
    )
    assert.strictEqual(planned.success.toolchain.clang, '/toolchain/clang')
  }
})

it('rejects unavailable and non-native targets', () => {
  const unknown = BuildPlan.make(project(), { target: 'unknown-target', profile: 'debug' })
  assert.strictEqual(Result.isFailure(unknown), true)
  if (Result.isFailure(unknown))
    assert.strictEqual(unknown.failure.reason._tag, 'TargetUnavailable')

  const wasm = BuildPlan.make(project(), {
    target: Target.wasm32UnknownUnknown.id,
    profile: 'debug',
  })
  assert.strictEqual(Result.isFailure(wasm), true)
  if (Result.isFailure(wasm)) assert.strictEqual(wasm.failure.reason._tag, 'NonNativeTarget')
})

it('rejects a foreign target for run while accepting the host target', () => {
  const host = Target.select(undefined)
  assert.strictEqual(host._tag, 'Resolved')
  if (host._tag !== 'Resolved') return
  const accepted = BuildPlan.make(project(), {
    target: host.target.id,
    profile: 'debug',
    purpose: 'run',
  })
  assert.strictEqual(Result.isSuccess(accepted), true)

  const foreign = Target.native.find((candidate) => candidate.id !== host.target.id)
  if (foreign === undefined) return
  const rejected = BuildPlan.make(project(), {
    target: foreign.id,
    profile: 'debug',
    purpose: 'run',
  })
  assert.strictEqual(Result.isFailure(rejected), true)
  if (Result.isFailure(rejected)) {
    assert.strictEqual(rejected.failure.reason._tag, 'ForeignRunTarget')
  }
})

it('guards the plan against a non-portable project value', () => {
  const planned = BuildPlan.make(project('Not Portable'), {
    target: 'aarch64-apple-darwin',
    profile: 'debug',
  })
  assert.strictEqual(Result.isFailure(planned), true)
  if (Result.isFailure(planned)) {
    assert.strictEqual(planned.failure.reason._tag, 'InvalidPackageName')
  }
})
