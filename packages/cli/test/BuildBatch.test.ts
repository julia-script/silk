import { assert, it } from '@effect/vitest'
import * as NativeLinkInput from '@silklang/compiler/NativeLinkInput'
import * as NativeToolchain from '@silklang/compiler/NativeToolchain'
import type * as Project from '@silklang/compiler/Project'
import * as SourceFile from '@silklang/compiler/SourceFile'
import * as TargetSelector from '@silklang/compiler/TargetSelector'
import * as Result from 'effect/Result'
import * as BuildBatch from '../src/BuildBatch.js'

const project = (
  nativeLinkInputs: Project.Project['build']['nativeLinkInputs'] = [],
  artifact: Project.Project['build']['artifact'] = 'NativeExecutable',
): Project.Project =>
  Object.freeze({
    _tag: 'Project',
    name: 'hello',
    version: '0.1.0',
    manifestPath: '/workspace/silk.toml',
    directory: '/workspace',
    entry: Object.freeze({
      _tag: 'SourceEntry',
      module: 'main',
      path: '/workspace/src/main.silk',
      sourceRoot: '/workspace/src',
      bytes: SourceFile.toUint8Array(SourceFile.make('main', new Uint8Array())),
    }),
    build: Object.freeze({
      backend: 'llvm',
      targets: ['host', 'wasm32-unknown-unknown'] as const,
      outputDirectory: '/workspace/build',
      artifact,
      nativeLinkInputs,
    }),
  })

it('preflights an ordered deduplicated multi-target LLVM batch', () => {
  const host = TargetSelector.resolve('host', NativeToolchain.hostSelection())
  assert.strictEqual(Result.isSuccess(host), true)
  if (Result.isFailure(host)) return
  const batch = BuildBatch.make(project(), {
    targets: ['host', host.success.id, 'wasm32-unknown-unknown'],
    profile: 'debug',
  })
  assert.strictEqual(Result.isSuccess(batch), true)
  if (Result.isFailure(batch)) return
  assert.deepStrictEqual(
    batch.success.plans.map((plan) => plan.target.id),
    [host.success.id, 'wasm32-unknown-unknown'],
  )
  assert.deepStrictEqual(
    batch.success.plans.map((plan) => plan.nativeLinkInputs),
    [[], []],
  )
})

it('forwards structured inputs to a native plan and rejects them for a wasm batch', () => {
  const linked = project([
    NativeLinkInput.library('c', 'Dynamic'),
    NativeLinkInput.library('m', 'Dynamic'),
  ])
  const native = BuildBatch.make(linked, { targets: ['host'], profile: 'debug' })
  assert.strictEqual(Result.isSuccess(native), true)
  if (Result.isSuccess(native))
    assert.deepStrictEqual(native.success.plans[0].nativeLinkInputs, linked.build.nativeLinkInputs)

  const wasm = BuildBatch.make(linked, {
    targets: ['wasm32-unknown-unknown'],
    profile: 'debug',
  })
  assert.strictEqual(Result.isFailure(wasm), true)
  if (!Result.isFailure(wasm)) return
  assert.strictEqual(wasm.failure.reason._tag, 'Plan')
  if (wasm.failure.reason._tag !== 'Plan') return
  assert.strictEqual(wasm.failure.reason.error.reason._tag, 'NativeInputsForWebAssembly')
  if (wasm.failure.reason.error.reason._tag === 'NativeInputsForWebAssembly')
    assert.strictEqual(wasm.failure.reason.error.reason.target, 'wasm32-unknown-unknown')
})

it('rejects every target- or artifact-incompatible native input during batch preflight', () => {
  const cases = [
    {
      project: project([NativeLinkInput.framework('CoreFoundation')]),
      target: 'x86_64-unknown-linux-gnu',
      reason: 'FrameworkTarget',
    },
    {
      project: project([NativeLinkInput.library('answer', 'Static')], 'NativeSharedLibrary'),
      target: 'aarch64-apple-darwin',
      reason: 'StaticLibraryTarget',
    },
    {
      project: project([NativeLinkInput.searchPath('/sdk/lib')], 'NativeStaticLibrary'),
      target: 'aarch64-apple-darwin',
      reason: 'StaticArchiveInput',
    },
  ] as const
  for (const example of cases) {
    const batch = BuildBatch.make(example.project, {
      targets: [example.target],
      profile: 'debug',
    })
    assert.strictEqual(Result.isFailure(batch), true)
    if (!Result.isFailure(batch)) continue
    assert.strictEqual(batch.failure.reason._tag, 'Plan')
    if (batch.failure.reason._tag !== 'Plan') continue
    assert.strictEqual(batch.failure.reason.error.reason._tag, 'UnsupportedNativePlan')
    if (batch.failure.reason.error.reason._tag !== 'UnsupportedNativePlan') continue
    assert.strictEqual(batch.failure.reason.error.reason.plan.reason, example.reason)
    assert.strictEqual(batch.failure.reason.error.reason.plan.target.id, example.target)
  }
})

it('rejects an unknown backend before returning any plans', () => {
  const batch = BuildBatch.make(project(), { backend: 'not-a-backend', profile: 'debug' })
  assert.strictEqual(Result.isFailure(batch), true)
  if (Result.isFailure(batch)) assert.strictEqual(batch.failure.reason._tag, 'Backend')
})
