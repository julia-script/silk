import { assert, it } from '@effect/vitest'
import * as WebContainerError from '../src/WebContainerError.js'

it('models invalid input without causal ancestry', () => {
  const error = WebContainerError.invalidInput(
    'WebContainerProcess.resize',
    'Terminal dimensions must be positive',
    'rows must be positive',
    { command: 'node' },
  )

  assert.strictEqual(error._tag, 'WebContainerError')
  assert.strictEqual(error.reason._tag, 'InvalidInput')
  assert.strictEqual(error.operation, 'WebContainerProcess.resize')
  assert.strictEqual(error.command, 'node')
  assert.strictEqual(Object.hasOwn(error, 'cause'), false)
})

it('models invalid state without causal ancestry', () => {
  const error = WebContainerError.invalidState(
    'WebContainer.spawn',
    'Cannot spawn after teardown',
    'the runtime scope is closed',
  )

  assert.strictEqual(error.reason._tag, 'InvalidState')
  assert.strictEqual(Object.hasOwn(error, 'cause'), false)
})

it('preserves wrapped failure ancestry', () => {
  const cause = new Error('boot failed')
  const error = WebContainerError.wrappedFailure(
    'WebContainer.boot',
    'Cannot boot WebContainer',
    cause,
  )

  assert.strictEqual(error.reason._tag, 'WrappedFailure')
  assert.strictEqual(error.cause, cause)
  assert.strictEqual(error.reason._tag === 'WrappedFailure' ? error.reason.cause : undefined, cause)
})

it('reads only guarded string messages and codes', () => {
  assert.strictEqual(WebContainerError.messageOf({ message: 'missing' }), 'missing')
  assert.strictEqual(WebContainerError.messageOf({ message: 1 }), undefined)
  assert.strictEqual(WebContainerError.codeOf({ code: 'ENOENT' }), 'ENOENT')
  assert.strictEqual(WebContainerError.codeOf(null), undefined)
})
