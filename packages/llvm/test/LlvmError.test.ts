import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import { invalidInput, invalidState, LlvmError, wrappedFailure } from '../src/LlvmError.js'

it.effect('is yieldable and recoverable by its LLVM-specific tag', () =>
  Effect.gen(function* () {
    const failure = invalidInput({
      operation: 'Type.integer',
      message: 'LLVM integer widths must be positive',
      input: 0,
    })
    const caught = yield* failure.pipe(
      Effect.catchTag('LlvmError', (error) => Effect.succeed(error)),
    )

    assert.instanceOf(caught, LlvmError)
    assert.strictEqual(caught._tag, 'LlvmError')
    assert.strictEqual(caught.operation, 'Type.integer')
    assert.deepEqual(caught.reason, { _tag: 'InvalidInput', input: 0 })
    assert.isUndefined(caught.cause)
  }),
)

it('distinguishes state diagnostics from genuine JavaScript causal ancestry', () => {
  const state = invalidState({
    operation: 'Builder.validateHandle',
    message: 'The handle belongs to a different LLVM builder',
    state: { expected: 'first', actual: 'second' },
  })
  const cause = new RangeError('encoder overflow')
  const wrapped = wrappedFailure({
    operation: 'Bitcode.encode',
    message: 'LLVM bitcode encoding failed',
    cause,
  })

  assert.strictEqual(state.reason._tag, 'InvalidState')
  assert.isUndefined(state.cause)
  assert.strictEqual(wrapped.reason._tag, 'WrappedFailure')
  assert.strictEqual(wrapped.cause, cause)
})
