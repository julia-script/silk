/** One target-neutral stable wake-control cell shared by an Execution and one Wake generation. */
export type Phase =
  | 'Idle'
  | 'Registering'
  | 'Latched'
  | 'Dormant'
  | 'Notifying'
  | 'Eligible'
  | 'Cancelled'
  | 'DestroyPending'
  | 'Released'

export interface State {
  readonly _tag: 'WakeCellState'
  readonly generation: number
  readonly phase: Phase
  readonly executionAuthority: boolean
  readonly wakeAuthority: boolean
  readonly registeringAuthority: boolean
  readonly notificationAuthority: boolean
  readonly invocationAuthority: boolean
  readonly values: 'Live' | 'Cleaned'
  readonly guard: 'Absent' | 'Owned' | 'Cleaned'
  readonly allocation: 'Retained' | 'Released'
}

export type Violation =
  | 'IllegalPredecessor'
  | 'DuplicateWakeAuthority'
  | 'MissingWakeAuthority'
  | 'PrematureGenerationReuse'
  | 'AuthorityInvariant'

export type Transition =
  | {
      readonly _tag: 'Transition'
      readonly state: State
      readonly action:
        | 'Register'
        | 'Dormant'
        | 'Latched'
        | 'Notify'
        | 'Eligible'
        | 'CancelledNoop'
        | 'Dropped'
        | 'Cancelled'
        | 'DestroyPending'
        | 'Released'
        | 'Resume'
    }
  | { readonly _tag: 'WakeCellViolation'; readonly reason: Violation; readonly state: State }

const state = (value: Omit<State, '_tag'>): State =>
  Object.freeze({ _tag: 'WakeCellState', ...value })

const transitioned = (
  self: State,
  action: Extract<Transition, { readonly _tag: 'Transition' }>['action'],
): Transition => Object.freeze({ _tag: 'Transition', state: self, action })

const violated = (self: State, reason: Violation): Transition =>
  Object.freeze({ _tag: 'WakeCellViolation', reason, state: self })

/** Initializes one reusable cell before the Execution's first park generation. */
export const initial = (): State =>
  state({
    generation: 0,
    phase: 'Idle',
    executionAuthority: true,
    wakeAuthority: false,
    registeringAuthority: false,
    notificationAuthority: false,
    invocationAuthority: false,
    values: 'Live',
    guard: 'Absent',
    allocation: 'Retained',
  })

const releasable = (self: State): boolean =>
  !self.executionAuthority &&
  !self.wakeAuthority &&
  !self.registeringAuthority &&
  !self.notificationAuthority &&
  !self.invocationAuthority

const releaseIfFinal = (self: State): State =>
  releasable(self) ? state({ ...self, phase: 'Released', allocation: 'Released' }) : self

/** Creates the generation's sole Wake and takes a registration transient. */
export const beginRegistration = (self: State): Transition => {
  if (self.phase !== 'Idle')
    return violated(
      self,
      self.wakeAuthority || self.registeringAuthority || self.notificationAuthority
        ? 'PrematureGenerationReuse'
        : 'IllegalPredecessor',
    )
  if (
    !self.executionAuthority ||
    self.wakeAuthority ||
    self.registeringAuthority ||
    self.notificationAuthority ||
    self.invocationAuthority ||
    self.values !== 'Live' ||
    (self.guard !== 'Cleaned' && self.guard !== 'Absent') ||
    self.allocation !== 'Retained'
  )
    return violated(self, 'AuthorityInvariant')
  return transitioned(
    state({
      ...self,
      generation: self.generation + 1,
      phase: 'Registering',
      wakeAuthority: true,
      registeringAuthority: true,
      guard: 'Absent',
    }),
    'Register',
  )
}

/** Retains the ordinary registration guard before suspension ownership is transferred. */
export const retainGuard = (self: State): Transition => {
  if (
    (self.phase !== 'Registering' && self.phase !== 'Latched') ||
    !self.registeringAuthority ||
    self.guard !== 'Absent'
  )
    return violated(self, 'IllegalPredecessor')
  return transitioned(
    state({ ...self, phase: self.phase === 'Latched' ? 'Latched' : 'Dormant', guard: 'Owned' }),
    self.phase === 'Latched' ? 'Latched' : 'Dormant',
  )
}

/** Consumes the sole Wake, latching during registration or beginning live notification. */
export const consumeWake = (self: State): Transition => {
  if (!self.wakeAuthority) return violated(self, 'MissingWakeAuthority')
  if (self.phase === 'Registering' || self.phase === 'Latched')
    return transitioned(state({ ...self, phase: 'Latched', wakeAuthority: false }), 'Latched')
  if (self.phase === 'Dormant' && !self.registeringAuthority)
    return transitioned(
      state({
        ...self,
        phase: 'Notifying',
        wakeAuthority: false,
        notificationAuthority: true,
        invocationAuthority: true,
      }),
      'Notify',
    )
  if (self.phase === 'Cancelled')
    return transitioned(releaseIfFinal(state({ ...self, wakeAuthority: false })), 'CancelledNoop')
  return violated(self, 'IllegalPredecessor')
}

/** Drops readiness authority without signaling; cancelled storage may become reclaimable. */
export const dropWake = (self: State): Transition => {
  if (!self.wakeAuthority) return violated(self, 'MissingWakeAuthority')
  return transitioned(releaseIfFinal(state({ ...self, wakeAuthority: false })), 'Dropped')
}

/** Ends the registration transient only after the complete suspension callback returns. */
export const suspensionReturned = (self: State): Transition => {
  if (
    !self.registeringAuthority ||
    (self.guard !== 'Owned' && !(self.phase === 'Cancelled' && self.guard === 'Cleaned'))
  )
    return violated(self, 'IllegalPredecessor')
  if (self.phase === 'Dormant')
    return transitioned(state({ ...self, registeringAuthority: false }), 'Dormant')
  if (self.phase === 'Latched')
    return transitioned(
      state({
        ...self,
        phase: 'Notifying',
        registeringAuthority: false,
        notificationAuthority: true,
        invocationAuthority: true,
      }),
      'Notify',
    )
  if (self.phase === 'Cancelled')
    return transitioned(
      releaseIfFinal(state({ ...self, registeringAuthority: false })),
      'Cancelled',
    )
  return violated(self, 'IllegalPredecessor')
}

/** Completes endpoint invocation, making a live execution eligible or finishing deferred destroy. */
export const notificationReturned = (self: State): Transition => {
  if (!self.notificationAuthority || !self.invocationAuthority)
    return violated(self, 'IllegalPredecessor')
  if (self.phase === 'Notifying')
    return transitioned(
      state({
        ...self,
        phase: 'Eligible',
        notificationAuthority: false,
        invocationAuthority: false,
      }),
      'Eligible',
    )
  if (self.phase === 'DestroyPending') {
    const released = releaseIfFinal(
      state({
        ...self,
        phase: 'Cancelled',
        notificationAuthority: false,
        invocationAuthority: false,
        values: 'Cleaned',
        guard: self.guard === 'Owned' ? 'Cleaned' : self.guard,
      }),
    )
    return transitioned(released, released.phase === 'Released' ? 'Released' : 'Cancelled')
  }
  return violated(self, 'IllegalPredecessor')
}

/** Cancels before cleanup; endpoint borrows delay value cleanup while notification is active. */
export const destroyExecution = (self: State): Transition => {
  if (!self.executionAuthority) return violated(self, 'IllegalPredecessor')
  if (self.phase === 'Notifying')
    return transitioned(
      state({ ...self, phase: 'DestroyPending', executionAuthority: false }),
      'DestroyPending',
    )
  if (
    self.phase !== 'Idle' &&
    self.phase !== 'Registering' &&
    self.phase !== 'Latched' &&
    self.phase !== 'Dormant' &&
    self.phase !== 'Eligible'
  )
    return violated(self, 'IllegalPredecessor')
  const cancelled = releaseIfFinal(
    state({
      ...self,
      phase: 'Cancelled',
      executionAuthority: false,
      values: 'Cleaned',
      guard: self.guard === 'Owned' ? 'Cleaned' : self.guard,
    }),
  )
  return transitioned(cancelled, cancelled.phase === 'Released' ? 'Released' : 'Cancelled')
}

/** Drops the retained guard immediately before source continues and resets only safe cell state. */
export const resume = (self: State): Transition => {
  if (
    self.phase !== 'Eligible' ||
    !self.executionAuthority ||
    self.wakeAuthority ||
    self.registeringAuthority ||
    self.notificationAuthority ||
    self.invocationAuthority ||
    self.guard !== 'Owned'
  )
    return violated(self, 'PrematureGenerationReuse')
  return transitioned(state({ ...self, phase: 'Idle', guard: 'Cleaned' }), 'Resume')
}

/** Verifies the authority invariants backends must preserve when fusing physical state tags. */
export const verify = (self: State): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = []
  if ((self.notificationAuthority || self.invocationAuthority) && self.wakeAuthority)
    violations.push('DuplicateWakeAuthority')
  if (self.notificationAuthority !== self.invocationAuthority) violations.push('AuthorityInvariant')
  if (
    (self.phase === 'Notifying' || self.phase === 'DestroyPending') !== self.notificationAuthority
  )
    violations.push('AuthorityInvariant')
  if (self.phase === 'Released' && (self.allocation !== 'Released' || !releasable(self)))
    violations.push('AuthorityInvariant')
  if (self.allocation === 'Released' && self.phase !== 'Released')
    violations.push('AuthorityInvariant')
  if ((self.phase === 'Cancelled' || self.phase === 'Released') && self.values !== 'Cleaned')
    violations.push('AuthorityInvariant')
  if (self.phase === 'DestroyPending' && self.values !== 'Live')
    violations.push('AuthorityInvariant')
  return Object.freeze([...new Set(violations)])
}

/** Stable representation-free inspection used by MIR and deterministic generation tests. */
export const encode = (self: State): string =>
  `wake-cell generation=${self.generation} phase=${self.phase.toLowerCase()} authority=execution:${self.executionAuthority ? 1 : 0},wake:${self.wakeAuthority ? 1 : 0},registering:${self.registeringAuthority ? 1 : 0},notification:${self.notificationAuthority ? 1 : 0},invocation:${self.invocationAuthority ? 1 : 0} values=${self.values.toLowerCase()} guard=${self.guard.toLowerCase()} allocation=${self.allocation.toLowerCase()}`
