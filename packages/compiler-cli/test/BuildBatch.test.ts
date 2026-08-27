import { assert, it } from '@effect/vitest'
import * as SourceFile from '@silk-lang/compiler/SourceFile'
import * as Result from 'effect/Result'
import * as BuildBatch from '../src/BuildBatch.js'
import type * as Project from '../src/Project.js'
import * as TargetSelector from '../src/TargetSelector.js'

const project = (): Project.Project =>
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
    }),
  })

it('preflights an ordered deduplicated multi-target LLVM batch', () => {
  const host = TargetSelector.resolve('host')
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
})

it('rejects one incompatible pair before returning any plans', () => {
  const batch = BuildBatch.make(project(), { backend: 'wasm', profile: 'debug' })
  assert.strictEqual(Result.isFailure(batch), true)
  if (Result.isFailure(batch)) assert.strictEqual(batch.failure.reason._tag, 'Plan')
})
