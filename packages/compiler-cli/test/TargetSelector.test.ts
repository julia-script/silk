import { assert, it } from '@effect/vitest'
import * as Target from '@silk-effect/compiler/Target'
import * as Result from 'effect/Result'
import * as TargetSelector from '../src/TargetSelector.js'

it('resolves host and canonical ids while preserving first-seen canonical order', () => {
  const host = Target.select(undefined)
  assert.strictEqual(host._tag, 'Resolved')
  if (host._tag !== 'Resolved') return
  const resolved = TargetSelector.resolveAll([
    'host',
    host.target.id,
    'wasm32-unknown-unknown',
    'host',
  ])
  assert.strictEqual(Result.isSuccess(resolved), true)
  if (Result.isFailure(resolved)) return
  assert.deepStrictEqual(
    resolved.success.map((target) => target.id),
    [host.target.id, 'wasm32-unknown-unknown'],
  )
})

it('rejects selectors outside the canonical target set', () => {
  const resolved = TargetSelector.resolveAll(['unknown-target'])
  assert.strictEqual(Result.isFailure(resolved), true)
  if (Result.isFailure(resolved)) {
    assert.strictEqual(resolved.failure.reason._tag, 'UnavailableTarget')
  }
})
