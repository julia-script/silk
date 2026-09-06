import * as ConfigurationOrigin from '@silklang/compiler/ConfigurationOrigin'
import * as ProjectProfile from '@silklang/compiler/ProjectProfile'
import * as Effect from 'effect/Effect'
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
      targets: ['host', 'wasm32-unknown-unknown'] as const,
      outputDirectory: '/workspace/build',
      artifact,
      nativeLinkInputs,
    }),
  })

it.effect('preflights an ordered deduplicated multi-target LLVM batch', () =>
  Effect.gen(function* () {
    const host = TargetSelector.resolve('host', NativeToolchain.hostSelection())
    assert.strictEqual(Result.isSuccess(host), true)
    if (Result.isFailure(host)) return
    const batch = yield* Effect.result(
      BuildBatch.make(project(), {
        targets: ['host', host.success.id, 'wasm32-unknown-unknown'],
        optimization: 'debug',
      }),
    )
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
  }),
)

it.effect('forwards structured inputs to a native plan and rejects them for a wasm batch', () =>
  Effect.gen(function* () {
    const linked = project([
      NativeLinkInput.library('c', 'Dynamic'),
      NativeLinkInput.library('m', 'Dynamic'),
    ])
    const native = yield* Effect.result(
      BuildBatch.make(linked, { targets: ['host'], optimization: 'debug' }),
    )
    assert.strictEqual(Result.isSuccess(native), true)
    if (Result.isSuccess(native))
      assert.deepStrictEqual(
        native.success.plans[0].nativeLinkInputs,
        linked.build.nativeLinkInputs,
      )

    const wasm = yield* Effect.result(
      BuildBatch.make(linked, {
        targets: ['wasm32-unknown-unknown'],
        optimization: 'debug',
      }),
    )
    assert.strictEqual(Result.isFailure(wasm), true)
    if (!Result.isFailure(wasm)) return
    assert.strictEqual(wasm.failure.reason._tag, 'Plan')
    if (wasm.failure.reason._tag !== 'Plan') return
    assert.strictEqual(wasm.failure.reason.error.reason._tag, 'NativeInputsForWebAssembly')
    if (wasm.failure.reason.error.reason._tag === 'NativeInputsForWebAssembly')
      assert.strictEqual(wasm.failure.reason.error.reason.target, 'wasm32-unknown-unknown')
  }),
)

it.effect(
  'rejects every target- or artifact-incompatible native input during batch preflight',
  () =>
    Effect.gen(function* () {
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
        const batch = yield* Effect.result(
          BuildBatch.make(example.project, {
            targets: [example.target],
            optimization: 'debug',
          }),
        )
        assert.strictEqual(Result.isFailure(batch), true)
        if (!Result.isFailure(batch)) continue
        assert.strictEqual(batch.failure.reason._tag, 'Plan')
        if (batch.failure.reason._tag !== 'Plan') continue
        assert.strictEqual(batch.failure.reason.error.reason._tag, 'UnsupportedNativePlan')
        if (batch.failure.reason.error.reason._tag !== 'UnsupportedNativePlan') continue
        assert.strictEqual(batch.failure.reason.error.reason.plan.reason, example.reason)
        assert.strictEqual(batch.failure.reason.error.reason.plan.target.id, example.target)
      }
    }),
)

it.effect('selects default and explicit complete profiles before host fallback', () =>
  Effect.gen(function* () {
    const origin = ConfigurationOrigin.literal('/workspace/silk.toml')
    const profiles = yield* ProjectProfile.catalog(
      {
        library: {
          target: 'x86_64-unknown-linux-gnu',
          artifact: 'static-archive',
          optimization: 'speed',
          debug: false,
        },
        wasm: { target: 'wasm32-unknown-unknown' },
      },
      'library',
      undefined,
      origin,
    )
    const configured = { ...project(), profiles }
    const runnable = yield* BuildBatch.make(project(), { purpose: 'run' })
    assert.strictEqual(runnable.plans.length, 1)
    assert.strictEqual(runnable.plans[0].artifactKind, 'NativeExecutable')
    const batch = yield* BuildBatch.make(configured, {})
    assert.strictEqual(batch.plans[0].target.id, 'x86_64-unknown-linux-gnu')
    assert.strictEqual(batch.plans[0].artifactKind, 'NativeStaticLibrary')
    assert.strictEqual(batch.plans[0].optimization, 'release')
    const named = yield* BuildBatch.make(configured, { profile: 'wasm' })
    assert.strictEqual(named.plans[0].artifactKind, 'WebAssemblyModule')
    const override = yield* BuildBatch.make(configured, {
      profileInput: '{"target":"aarch64-unknown-linux-gnu","artifact":"executable"}',
    })
    assert.strictEqual(override.plans[0].target.id, 'aarch64-unknown-linux-gnu')
    for (const options of [
      { profile: 'missing' },
      { profile: 'wasm', targets: ['host'] },
      { profile: 'wasm', profileInput: '{"target":"wasm32-unknown-unknown"}' },
    ]) {
      const result = yield* Effect.result(BuildBatch.make(configured, options))
      assert.strictEqual(Result.isFailure(result), true)
    }
  }),
)
