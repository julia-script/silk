import { assert, it } from '@effect/vitest'
import * as Result from 'effect/Result'
import * as TargetSelector from '../src/TargetSelector.js'

it('resolves host and canonical ids while preserving first-seen canonical order', () => {
  const host = TargetSelector.resolve('host')
  assert.strictEqual(Result.isSuccess(host), true)
  if (Result.isFailure(host)) return
  const resolved = TargetSelector.resolveAll([
    'host',
    host.success.id,
    'wasm32-unknown-unknown',
    'host',
  ])
  assert.strictEqual(Result.isSuccess(resolved), true)
  if (Result.isFailure(resolved)) return
  assert.deepStrictEqual(
    resolved.success.map((target) => target.id),
    [host.success.id, 'wasm32-unknown-unknown'],
  )
})

it('rejects selectors outside the canonical target set', () => {
  const resolved = TargetSelector.resolveAll(['unknown-target'])
  assert.strictEqual(Result.isFailure(resolved), true)
  if (Result.isFailure(resolved)) {
    assert.strictEqual(resolved.failure.reason._tag, 'UnavailableTarget')
  }
})
