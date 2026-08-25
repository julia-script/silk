import { assert, it } from '@effect/vitest'
import * as ExecutionTransition from '../src/ExecutionTransition.js'

const edge = (result: ExecutionTransition.Result): ExecutionTransition.Edge => {
  assert.strictEqual(result._tag, 'ExecutionTransitionEdge')
  if (result._tag !== 'ExecutionTransitionEdge') throw new RangeError('expected transition edge')
  assert.deepEqual(ExecutionTransition.verifyEdge(result), [])
  return result
}

it('verifies and deterministically inspects a complete external park generation', () => {
  const initial = ExecutionTransition.initialize(2, 7, true)
  const running = edge(ExecutionTransition.drive(initial)).after
  const registering = edge(ExecutionTransition.register(running)).after
  const dormant = edge(
    ExecutionTransition.relinquish(edge(ExecutionTransition.retainGuard(registering)).after),
  ).after
  const notifying = edge(ExecutionTransition.wake(dormant)).after
  const eligible = edge(ExecutionTransition.notificationReturned(notifying)).after
  const resumed = edge(ExecutionTransition.drive(eligible))
  const completed = edge(ExecutionTransition.complete(resumed.after))

  assert.strictEqual(resumed.event, 'Resume')
  assert.deepEqual(resumed.cleanup, ['Guard'])
  assert.strictEqual(completed.after.execution, 'Completed')
  assert.strictEqual(ExecutionTransition.encode(completed), ExecutionTransition.encode(completed))
  assert.notInclude(ExecutionTransition.encode(completed), 'offset')
})

it('publishes initial readiness exactly once before the first drive', () => {
  const initial = ExecutionTransition.initialize(3, 9, true)
  const ready = edge(ExecutionTransition.notifyInitial(initial)).after

  assert.strictEqual(ready.execution, 'InitialReady')
  assert.strictEqual(ExecutionTransition.notifyInitial(ready)._tag, 'ExecutionTransitionViolation')
  assert.strictEqual(edge(ExecutionTransition.drive(ready)).after.execution, 'Running')
})

it('latches wake during registration and notifies only after relinquishment', () => {
  const running = edge(ExecutionTransition.drive(ExecutionTransition.initialize(0, 1, true))).after
  const registering = edge(ExecutionTransition.register(running)).after
  const latched = edge(ExecutionTransition.wake(registering)).after
  const guarded = edge(ExecutionTransition.retainGuard(latched)).after
  const notifying = edge(ExecutionTransition.relinquish(guarded)).after

  assert.strictEqual(latched.wake?.phase, 'Latched')
  assert.strictEqual(notifying.execution, 'Notifying')
})

it('rejects illegal drive, duplicate Wake, premature reuse, and early endpoint cleanup', () => {
  const running = edge(ExecutionTransition.drive(ExecutionTransition.initialize(0, 1, true))).after
  const registering = edge(ExecutionTransition.register(running)).after
  const dormant = edge(
    ExecutionTransition.relinquish(edge(ExecutionTransition.retainGuard(registering)).after),
  ).after
  assert.strictEqual(ExecutionTransition.drive(dormant)._tag, 'FatalExecutionTrap')

  const notifying = edge(ExecutionTransition.wake(dormant)).after
  assert.strictEqual(ExecutionTransition.wake(notifying)._tag, 'ExecutionTransitionViolation')
  assert.strictEqual(ExecutionTransition.register(notifying)._tag, 'ExecutionTransitionViolation')

  const pending = edge(ExecutionTransition.cancel(notifying))
  assert.strictEqual(pending.after.execution, 'DestroyPending')
  assert.deepEqual(pending.cleanup, [])
  assert.deepEqual(ExecutionTransition.verifyEdge(pending), [])
  const released = edge(ExecutionTransition.notificationReturned(pending.after))
  assert.strictEqual(released.after.execution, 'Released')
  assert.include(released.cleanup, 'Endpoint')
})

it('retains allocation until a late cancelled Wake consumes final authority', () => {
  const running = edge(ExecutionTransition.drive(ExecutionTransition.initialize(0, 1, true))).after
  const registering = edge(ExecutionTransition.register(running)).after
  const dormant = edge(
    ExecutionTransition.relinquish(edge(ExecutionTransition.retainGuard(registering)).after),
  ).after
  const cancelled = edge(ExecutionTransition.cancel(dormant)).after
  const released = edge(ExecutionTransition.wake(cancelled)).after

  assert.strictEqual(cancelled.execution, 'Destroyed')
  assert.strictEqual(cancelled.wake?.allocation, 'Retained')
  assert.strictEqual(released.wake?.allocation, 'Released')
})
