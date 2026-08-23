import * as ExecutionLifecycle from './ExecutionLifecycle.js'
import * as WakeCell from './WakeCell.js'

/** Stable target-neutral identity for one independently owned execution package. */
export interface Identity {
  readonly _tag: 'ExecutionIdentity'
  readonly package: number
  readonly root: number
}

/** Complete logical state from which every evaluator and backend transition is derived. */
export interface State {
  readonly _tag: 'ExecutionTransitionState'
  readonly identity: Identity
  readonly execution: ExecutionLifecycle.State | 'DestroyPending' | 'Released'
  readonly wake?: WakeCell.State
}

export type Event =
  | 'Initialize'
  | 'Drive'
  | 'Register'
  | 'RetainGuard'
  | 'Relinquish'
  | 'Wake'
  | 'Notify'
  | 'Eligible'
  | 'Resume'
  | 'Complete'
  | 'Cancel'
  | 'Release'

export interface Edge {
  readonly _tag: 'ExecutionTransitionEdge'
  readonly event: Event
  readonly before: State
  readonly after: State
  readonly cleanup: ReadonlyArray<'Guard' | 'Body' | 'Endpoint' | 'Callback' | 'Allocation'>
}

export type Result =
  | Edge
  | {
      readonly _tag: 'ExecutionTransitionViolation'
      readonly event: Event
      readonly state: State
      readonly reason:
        | 'IllegalPredecessor'
        | 'WakeAuthority'
        | 'PackageProvenance'
        | 'CompletionLoan'
        | 'EndpointBorrow'
    }
  | {
      readonly _tag: 'FatalExecutionTrap'
      readonly event: 'Drive'
      readonly state: State
      readonly reason: 'DormantOrNotifying'
    }

const state = (identity: Identity, execution: State['execution'], wake?: WakeCell.State): State =>
  Object.freeze({
    _tag: 'ExecutionTransitionState',
    identity,
    execution,
    ...(wake === undefined ? {} : { wake }),
  })

const edge = (
  event: Event,
  before: State,
  after: State,
  cleanup: Edge['cleanup'] = Object.freeze([]),
): Edge => Object.freeze({ _tag: 'ExecutionTransitionEdge', event, before, after, cleanup })

const violation = (
  state_: State,
  event: Event,
  reason: Extract<Result, { readonly _tag: 'ExecutionTransitionViolation' }>['reason'],
): Result => Object.freeze({ _tag: 'ExecutionTransitionViolation', event, state: state_, reason })

const transitionedWake = (
  self: State,
  event: Event,
  transition: WakeCell.Transition,
  execution: State['execution'] = self.execution,
  cleanup: Edge['cleanup'] = Object.freeze([]),
): Result =>
  transition._tag === 'WakeCellViolation'
    ? violation(self, event, 'WakeAuthority')
    : edge(event, self, state(self.identity, execution, transition.state), cleanup)

/** Creates one deterministic package/root pair before source body execution. */
export const initialize = (packageIdentity: number, root: number, readiness: boolean): State =>
  state(
    Object.freeze({ _tag: 'ExecutionIdentity', package: packageIdentity, root }),
    'Initial',
    readiness ? WakeCell.initial() : undefined,
  )

/** Enters only Initial or Eligible and fatally rejects owner drive while progress is external. */
export const drive = (self: State): Result => {
  const logical = ExecutionLifecycle.transition(
    self.execution === 'DestroyPending' || self.execution === 'Released'
      ? 'Destroyed'
      : self.execution,
    'Drive',
  )
  if (logical._tag === 'FatalIntrinsicStateTrap')
    return Object.freeze({
      _tag: 'FatalExecutionTrap',
      event: 'Drive',
      state: self,
      reason: 'DormantOrNotifying',
    })
  if (logical._tag !== 'Transition') return violation(self, 'Drive', 'IllegalPredecessor')
  if (self.execution === 'Eligible' && self.wake !== undefined) {
    const resumed = WakeCell.resume(self.wake)
    if (resumed._tag === 'WakeCellViolation') return violation(self, 'Drive', 'WakeAuthority')
    return edge(
      'Resume',
      self,
      state(self.identity, 'Running', resumed.state),
      Object.freeze(['Guard']),
    )
  }
  return edge('Drive', self, state(self.identity, 'Running', self.wake))
}

/** Begins one generation and publishes its sole affine Wake authority. */
export const register = (self: State): Result =>
  self.execution !== 'Running' || self.wake === undefined
    ? violation(self, 'Register', 'IllegalPredecessor')
    : transitionedWake(self, 'Register', WakeCell.beginRegistration(self.wake))

/** Retains the ordinary registration guard before the execution relinquishes. */
export const retainGuard = (self: State): Result =>
  self.wake === undefined
    ? violation(self, 'RetainGuard', 'IllegalPredecessor')
    : transitionedWake(self, 'RetainGuard', WakeCell.retainGuard(self.wake))

/** Completes the suspension callback boundary before allowing notification. */
export const relinquish = (self: State): Result => {
  if (self.execution !== 'Running' || self.wake === undefined)
    return violation(self, 'Relinquish', 'IllegalPredecessor')
  const returned = WakeCell.suspensionReturned(self.wake)
  if (returned._tag === 'WakeCellViolation') return violation(self, 'Relinquish', 'WakeAuthority')
  const execution = returned.state.phase === 'Notifying' ? 'Notifying' : 'Dormant'
  return edge(
    returned.state.phase === 'Notifying' ? 'Notify' : 'Relinquish',
    self,
    state(self.identity, execution, returned.state),
  )
}

/** Consumes the generation's Wake as readiness only. */
export const wake = (self: State): Result =>
  self.wake === undefined
    ? violation(self, 'Wake', 'IllegalPredecessor')
    : transitionedWake(
        self,
        'Wake',
        WakeCell.consumeWake(self.wake),
        self.wake.phase === 'Dormant' ? 'Notifying' : self.execution,
      )

/** Ends endpoint invocation; reentrant destruction never publishes Eligible. */
export const notificationReturned = (self: State): Result => {
  if (self.wake === undefined) return violation(self, 'Eligible', 'IllegalPredecessor')
  const returned = WakeCell.notificationReturned(self.wake)
  if (returned._tag === 'WakeCellViolation') return violation(self, 'Eligible', 'EndpointBorrow')
  const released = returned.state.phase === 'Released'
  return edge(
    released ? 'Release' : 'Eligible',
    self,
    state(self.identity, released ? 'Released' : 'Eligible', returned.state),
    released
      ? Object.freeze(['Guard', 'Body', 'Endpoint', 'Callback', 'Allocation'])
      : Object.freeze([]),
  )
}

/** Completes a running body and its terminal package cleanup. */
export const complete = (self: State): Result =>
  self.execution !== 'Running'
    ? violation(self, 'Complete', 'IllegalPredecessor')
    : edge(
        'Complete',
        self,
        state(self.identity, 'Completed', self.wake),
        Object.freeze(['Endpoint', 'Callback', 'Allocation']),
      )

/** Cancels an ordinary owner state or defers cleanup across an active endpoint borrow. */
export const cancel = (self: State): Result => {
  if (self.wake === undefined) {
    if (
      self.execution !== 'Initial' &&
      self.execution !== 'Dormant' &&
      self.execution !== 'Eligible'
    )
      return violation(self, 'Cancel', 'IllegalPredecessor')
    return edge(
      'Release',
      self,
      state(self.identity, 'Released'),
      Object.freeze(['Body', 'Endpoint', 'Callback', 'Allocation']),
    )
  }
  const destroyed = WakeCell.destroyExecution(self.wake)
  if (destroyed._tag === 'WakeCellViolation') return violation(self, 'Cancel', 'WakeAuthority')
  const pending = destroyed.state.phase === 'DestroyPending'
  const released = destroyed.state.phase === 'Released'
  return edge(
    pending ? 'Cancel' : released ? 'Release' : 'Cancel',
    self,
    state(
      self.identity,
      pending ? 'DestroyPending' : released ? 'Released' : 'Destroyed',
      destroyed.state,
    ),
    pending
      ? Object.freeze([])
      : released
        ? Object.freeze(['Guard', 'Body', 'Endpoint', 'Callback', 'Allocation'])
        : Object.freeze(['Guard', 'Body', 'Endpoint', 'Callback']),
  )
}

/** Validates one edge independently of a backend's fused physical tags. */
export const verifyEdge = (self: Edge): ReadonlyArray<string> => {
  const violations: Array<string> = []
  if (self.before.identity.package !== self.after.identity.package)
    violations.push('PackageProvenance')
  if (self.before.identity.root !== self.after.identity.root) violations.push('LogicalRoot')
  if (self.after.wake !== undefined) violations.push(...WakeCell.verify(self.after.wake))
  if (self.after.execution === 'DestroyPending' && self.cleanup.length > 0)
    violations.push('EndpointCleanupBeforeInvocationReturn')
  return Object.freeze([...new Set(violations)])
}

/** Compact private tag selected by native and Wasm only after MIR validation. */
export const tag = (self: State): number => {
  switch (self.execution) {
    case 'Initial':
      return 0
    case 'Running':
      return 1
    case 'Dormant':
      return 2
    case 'Notifying':
      return 3
    case 'Eligible':
      return 4
    case 'Completed':
      return 5
    case 'Destroyed':
      return 6
    case 'DestroyPending':
      return 7
    case 'Released':
      return 8
  }
}

/** Representation-free deterministic inspection of one state or transition edge. */
export const encode = (self: State | Edge): string => {
  if (self._tag === 'ExecutionTransitionState')
    return `execution id=e${self.identity.package} root=x${self.identity.root} state=${self.execution.toLowerCase()}${self.wake === undefined ? ' generation=none' : ` generation=${self.wake.generation} ${WakeCell.encode(self.wake)}`}`
  return `${encode(self.before)} --${self.event.toLowerCase()} cleanup=${self.cleanup.map((item) => item.toLowerCase()).join(',') || 'none'}--> ${encode(self.after)}`
}
