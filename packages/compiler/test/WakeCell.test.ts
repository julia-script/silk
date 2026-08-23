import { assert, it } from '@effect/vitest'
import * as WakeCell from '../src/WakeCell.js'

const transition = (result: WakeCell.Transition): WakeCell.State => {
  assert.strictEqual(result._tag, 'Transition')
  return result.state
}

const registered = (): WakeCell.State =>
  transition(WakeCell.retainGuard(transition(WakeCell.beginRegistration(WakeCell.initial()))))

it('latches wake during registration until the complete suspension handoff returns', () => {
  const registering = transition(WakeCell.beginRegistration(WakeCell.initial()))
  const latched = transition(WakeCell.consumeWake(registering))
  assert.strictEqual(latched.phase, 'Latched')
  assert.isFalse(latched.wakeAuthority)
  assert.isTrue(latched.registeringAuthority)

  const retained = transition(WakeCell.retainGuard(latched))
  assert.strictEqual(retained.phase, 'Latched')
  const notifying = transition(WakeCell.suspensionReturned(retained))
  assert.strictEqual(notifying.phase, 'Notifying')
  assert.isTrue(notifying.invocationAuthority)
  assert.deepEqual(WakeCell.verify(notifying), [])

  const eligible = transition(WakeCell.notificationReturned(notifying))
  assert.strictEqual(eligible.phase, 'Eligible')
  assert.isFalse(eligible.invocationAuthority)
  assert.deepEqual(WakeCell.verify(eligible), [])
})

it('suppresses a latched notification when onSuspend destroys the execution', () => {
  const registering = transition(WakeCell.beginRegistration(WakeCell.initial()))
  const latched = transition(WakeCell.consumeWake(registering))
  const retained = transition(WakeCell.retainGuard(latched))
  const cancelled = transition(WakeCell.destroyExecution(retained))
  assert.strictEqual(cancelled.phase, 'Cancelled')
  assert.strictEqual(cancelled.values, 'Cleaned')
  assert.strictEqual(cancelled.allocation, 'Retained')

  const released = transition(WakeCell.suspensionReturned(cancelled))
  assert.strictEqual(released.phase, 'Released')
  assert.strictEqual(released.allocation, 'Released')
  assert.deepEqual(WakeCell.verify(released), [])
})

it('notifies at most once after dormancy and traps every duplicate authority use', () => {
  const dormant = transition(WakeCell.suspensionReturned(registered()))
  const notifying = transition(WakeCell.consumeWake(dormant))
  assert.strictEqual(notifying.phase, 'Notifying')
  assert.strictEqual(WakeCell.consumeWake(notifying)._tag, 'WakeCellViolation')
  assert.strictEqual(WakeCell.dropWake(notifying)._tag, 'WakeCellViolation')

  const eligible = transition(WakeCell.notificationReturned(notifying))
  const resumed = transition(WakeCell.resume(eligible))
  assert.strictEqual(resumed.phase, 'Idle')
  assert.strictEqual(resumed.guard, 'Cleaned')
  assert.deepEqual(WakeCell.verify(resumed), [])
})

it('traps indirect drive throughout notification without mutating endpoint authority', () => {
  const dormant = transition(WakeCell.suspensionReturned(registered()))
  assert.strictEqual(WakeCell.admitDrive(dormant)._tag, 'FatalDriveTrap')
  const notifying = transition(WakeCell.consumeWake(dormant))
  const admission = WakeCell.admitDrive(notifying)
  assert.strictEqual(admission._tag, 'FatalDriveTrap')
  assert.strictEqual(admission.state, notifying)
  assert.isTrue(admission.state.notificationAuthority)
  assert.isTrue(admission.state.invocationAuthority)
  assert.strictEqual(admission.state.phase, 'Notifying')
})

it('defers reentrant destruction until the endpoint invocation borrow ends', () => {
  const dormant = transition(WakeCell.suspensionReturned(registered()))
  const notifying = transition(WakeCell.consumeWake(dormant))
  const pending = transition(WakeCell.destroyExecution(notifying))
  assert.strictEqual(pending.phase, 'DestroyPending')
  assert.strictEqual(pending.values, 'Live')
  assert.strictEqual(pending.allocation, 'Retained')
  assert.isTrue(pending.invocationAuthority)

  const released = transition(WakeCell.notificationReturned(pending))
  assert.strictEqual(released.phase, 'Released')
  assert.strictEqual(released.values, 'Cleaned')
  assert.strictEqual(released.guard, 'Cleaned')
  assert.deepEqual(WakeCell.verify(released), [])
})

it('keeps the indivisible allocation inert until a late cancelled Wake is discharged', () => {
  const dormant = transition(WakeCell.suspensionReturned(registered()))
  const cancelled = transition(WakeCell.destroyExecution(dormant))
  assert.strictEqual(cancelled.phase, 'Cancelled')
  assert.strictEqual(cancelled.values, 'Cleaned')
  assert.isTrue(cancelled.wakeAuthority)
  assert.strictEqual(cancelled.allocation, 'Retained')

  const noOp = WakeCell.consumeWake(cancelled)
  assert.strictEqual(noOp._tag, 'Transition')
  if (noOp._tag !== 'Transition') return
  assert.strictEqual(noOp.action, 'CancelledNoop')
  assert.strictEqual(noOp.state.phase, 'Released')
  assert.strictEqual(noOp.state.allocation, 'Released')
  assert.deepEqual(WakeCell.verify(noOp.state), [])
})

it('reuses one stable cell only after every prior generation authority ends', () => {
  const firstDormant = transition(WakeCell.suspensionReturned(registered()))
  assert.strictEqual(WakeCell.beginRegistration(firstDormant)._tag, 'WakeCellViolation')
  const firstEligible = transition(
    WakeCell.notificationReturned(transition(WakeCell.consumeWake(firstDormant))),
  )
  const firstResumed = transition(WakeCell.resume(firstEligible))
  const secondRegistering = transition(WakeCell.beginRegistration(firstResumed))
  assert.strictEqual(secondRegistering.generation, 2)
  assert.isTrue(secondRegistering.wakeAuthority)
  assert.deepEqual(WakeCell.verify(secondRegistering), [])
  assert.strictEqual(WakeCell.encode(secondRegistering), WakeCell.encode(secondRegistering))
})

it('dropping an eligible execution cleans without repeating notification', () => {
  const dormant = transition(WakeCell.suspensionReturned(registered()))
  const eligible = transition(
    WakeCell.notificationReturned(transition(WakeCell.consumeWake(dormant))),
  )
  const destroyed = transition(WakeCell.destroyExecution(eligible))
  assert.strictEqual(destroyed.phase, 'Released')
  assert.strictEqual(destroyed.values, 'Cleaned')
  assert.deepEqual(WakeCell.verify(destroyed), [])
})

it('rejects malformed authority combinations and encodes equal states identically', () => {
  const malformed: WakeCell.State = Object.freeze({
    ...WakeCell.initial(),
    phase: 'Notifying',
    wakeAuthority: true,
    notificationAuthority: true,
    invocationAuthority: true,
  })
  assert.deepEqual(WakeCell.verify(malformed), ['DuplicateWakeAuthority'])
  assert.strictEqual(WakeCell.encode(WakeCell.initial()), WakeCell.encode(WakeCell.initial()))
})
