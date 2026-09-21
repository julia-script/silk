import * as Effect from 'effect/Effect'

/** A provider-owned semantic operation with a reconstructible canonical address. */
export interface Descriptor {
  readonly _tag: 'SemanticQueryDescriptor'
  readonly family: string
  readonly schema: number
  readonly address: string
  readonly reuse: 'Revision' | 'Session'
}

/** One typed current input that can be read again while validating a prior record. */
export interface InputAddress {
  readonly _tag: 'SemanticInputAddress'
  readonly family: string
  readonly schema: number
  readonly address: string
}

export interface QueryRead {
  readonly _tag: 'QueryRead'
  readonly descriptor: Descriptor
  readonly fingerprint: string
}

export interface InputRead {
  readonly _tag: 'InputRead'
  readonly input: InputAddress
  readonly fingerprint: string
}

export type Observation = QueryRead | InputRead
export type Observe = (input: InputAddress) => void

/** The single dispatcher for one semantic environment. */
export interface Provider {
  readonly execute: (descriptor: Descriptor, observe: Observe) => unknown
  readonly fingerprint: (descriptor: Descriptor, answer: unknown) => string
  readonly read: (input: InputAddress) => string | undefined
}

export interface Completed<A> {
  readonly descriptor: Descriptor
  readonly key: string
  readonly answer: A
  readonly fingerprint: string
  readonly observations: ReadonlyArray<Observation>
}

/** A bounded transfer object that does not retain its producing session. */
export interface Snapshot {
  readonly _tag: 'SemanticQuerySnapshot'
  readonly records: ReadonlyMap<string, Completed<unknown>>
}

export interface Cycle {
  readonly _tag: 'SemanticQueryCycle'
  readonly key: string
  readonly path: ReadonlyArray<string>
}

export type Result<A> =
  | { readonly _tag: 'Completed'; readonly completed: Completed<A>; readonly reused: boolean }
  | { readonly _tag: 'Cycle'; readonly cycle: Cycle }

export interface Counters {
  readonly _tag: 'SemanticQueryCounters'
  readonly validations: number
  readonly executions: number
  readonly reuses: number
}

export interface Session {
  readonly _tag: 'SemanticQuerySession'
  readonly epoch: string
}

interface Active {
  readonly key: string
  readonly observations: Array<Observation>
}

interface State {
  readonly provider: Provider
  readonly previous: ReadonlyMap<string, Completed<unknown>>
  readonly current: Map<string, Completed<unknown>>
  readonly active: Array<Active>
  readonly executions: Map<string, number>
  readonly counters: { validations: number; executions: number; reuses: number }
  freshDepth: number
}

const states = new WeakMap<Session, State>()
const missingFingerprint = '\u0000missing'

const stateOf = (self: Session): State => {
  const state = states.get(self)
  if (state === undefined) throw new RangeError('Unknown semantic query session')
  return state
}

export const keyOf = (descriptor: Descriptor): string =>
  descriptor.family +
  ':' +
  descriptor.schema +
  ':' +
  descriptor.address.length +
  ':' +
  descriptor.address

const inputKey = (input: InputAddress): string =>
  input.family + ':' + input.schema + ':' + input.address.length + ':' + input.address

const readFingerprint = (state: State, input: InputAddress): string =>
  state.provider.read(input) ?? missingFingerprint

export const make = (epoch: string, provider: Provider, previous?: Snapshot): Session => {
  const session = Object.freeze({ _tag: 'SemanticQuerySession' as const, epoch })
  states.set(session, {
    provider,
    previous: previous?.records ?? new Map(),
    current: new Map(),
    active: [],
    executions: new Map(),
    counters: { validations: 0, executions: 0, reuses: 0 },
    freshDepth: 0,
  })
  return session
}

const observationKey = (observation: Observation): string =>
  observation._tag === 'QueryRead'
    ? observation._tag + ':' + keyOf(observation.descriptor) + ':' + observation.fingerprint
    : observation._tag + ':' + inputKey(observation.input) + ':' + observation.fingerprint

const observeInto = (active: Active, observation: Observation): void => {
  const encoded = observationKey(observation)
  if (active.observations.some((candidate) => observationKey(candidate) === encoded)) return
  active.observations.push(Object.freeze(observation))
}

const observeParent = (state: State, descriptor: Descriptor, fingerprint: string): void => {
  const parent = state.active.at(-1)
  if (parent !== undefined)
    observeInto(parent, Object.freeze({ _tag: 'QueryRead', descriptor, fingerprint }))
}

const cycle = (state: State, key: string): Result<never> => {
  const ordinal = state.active.findIndex((active) => active.key === key)
  return Object.freeze({
    _tag: 'Cycle',
    cycle: Object.freeze({
      _tag: 'SemanticQueryCycle',
      key,
      path: Object.freeze([
        ...state.active.slice(Math.max(0, ordinal)).map((active) => active.key),
        key,
      ]),
    }),
  })
}

const storedAs = <A>(stored: Completed<unknown>): Completed<A> =>
  // Family and schema are part of the key and determine the provider answer type.
  stored as Completed<A>

const validateObservation = (self: Session, observation: Observation): boolean => {
  const state = stateOf(self)
  if (observation._tag === 'InputRead')
    return readFingerprint(state, observation.input) === observation.fingerprint
  const result = execute<unknown>(self, observation.descriptor, true)
  return result._tag === 'Completed' && result.completed.fingerprint === observation.fingerprint
}

const validate = (self: Session, completed: Completed<unknown>): boolean => {
  const state = stateOf(self)
  state.counters.validations += 1
  for (const observation of completed.observations)
    if (!validateObservation(self, observation)) return false
  return true
}

const executeProvider = <A>(self: Session, descriptor: Descriptor, publish: boolean): Result<A> => {
  const state = stateOf(self)
  const key = keyOf(descriptor)
  const active: Active = { key, observations: [] }
  state.active.push(active)
  try {
    state.counters.executions += 1
    state.executions.set(key, (state.executions.get(key) ?? 0) + 1)
    const answer = state.provider.execute(descriptor, (input) =>
      observeInto(
        active,
        Object.freeze({
          _tag: 'InputRead',
          input,
          fingerprint: readFingerprint(state, input),
        }),
      ),
    ) as A
    const completed = Object.freeze({
      descriptor,
      key,
      answer,
      fingerprint: state.provider.fingerprint(descriptor, answer),
      observations: Object.freeze([...active.observations]),
    })
    if (publish) state.current.set(key, completed)
    const removed = state.active.pop()
    if (removed !== active) throw new RangeError('Semantic query reservation stack is corrupted')
    observeParent(state, descriptor, completed.fingerprint)
    return Object.freeze({ _tag: 'Completed', completed, reused: false })
  } catch (cause) {
    if (state.active.at(-1) === active) state.active.pop()
    throw cause
  }
}

const execute = <A>(self: Session, descriptor: Descriptor, allowReuse: boolean): Result<A> => {
  const state = stateOf(self)
  const key = keyOf(descriptor)
  const publish = state.freshDepth === 0
  if (state.active.some((active) => active.key === key)) return cycle(state, key)
  if (allowReuse && publish) {
    const current = state.current.get(key)
    if (current !== undefined) {
      state.counters.reuses += 1
      observeParent(state, descriptor, current.fingerprint)
      return Object.freeze({ _tag: 'Completed', completed: storedAs<A>(current), reused: true })
    }
    const previous = descriptor.reuse === 'Revision' ? state.previous.get(key) : undefined
    if (previous !== undefined) {
      const reservation: Active = { key, observations: [] }
      state.active.push(reservation)
      let valid = false
      try {
        valid = validate(self, previous)
      } finally {
        const removed = state.active.pop()
        if (removed !== reservation)
          throw new RangeError('Semantic query validation stack is corrupted')
      }
      if (valid) {
        state.current.set(key, previous)
        state.counters.reuses += 1
        observeParent(state, descriptor, previous.fingerprint)
        return Object.freeze({
          _tag: 'Completed',
          completed: storedAs<A>(previous),
          reused: true,
        })
      }
    }
  }
  return executeProvider(self, descriptor, publish)
}

export const query = <A>(self: Session, descriptor: Descriptor): Result<A> =>
  execute(self, descriptor, true)

/** Bypasses both current and previous reuse for this root and every nested provider. */
export const fresh = <A>(self: Session, descriptor: Descriptor): Result<A> => {
  const state = stateOf(self)
  state.freshDepth += 1
  try {
    return execute(self, descriptor, false)
  } finally {
    state.freshDepth -= 1
  }
}

export const queryEffect = Effect.fn('SemanticQuery.query')(function* <A>(
  self: Session,
  descriptor: Descriptor,
): Effect.fn.Return<Completed<A>, Cycle> {
  yield* Effect.yieldNow
  const result = query<A>(self, descriptor)
  return result._tag === 'Cycle' ? yield* Effect.fail(result.cycle) : result.completed
})

export const completed = <A>(self: Session, descriptor: Descriptor): Completed<A> | undefined => {
  const stored = stateOf(self).current.get(keyOf(descriptor))
  return stored === undefined ? undefined : storedAs<A>(stored)
}

export const snapshot = (self: Session): Snapshot =>
  Object.freeze({
    _tag: 'SemanticQuerySnapshot',
    records: new Map(
      [...stateOf(self).current].filter(
        ([, completed]) => completed.descriptor.reuse === 'Revision',
      ),
    ),
  })

export const executionCount = (self: Session, descriptor: Descriptor): number =>
  stateOf(self).executions.get(keyOf(descriptor)) ?? 0

export const counters = (self: Session): Counters => {
  const counters = stateOf(self).counters
  return Object.freeze({ _tag: 'SemanticQueryCounters', ...counters })
}

export const isActive = (self: Session, descriptor: Descriptor): boolean =>
  stateOf(self).active.some((active) => active.key === keyOf(descriptor))

export const observe = (self: Session, input: InputAddress): void => {
  const state = stateOf(self)
  const active = state.active.at(-1)
  if (active !== undefined)
    observeInto(
      active,
      Object.freeze({
        _tag: 'InputRead',
        input,
        fingerprint: readFingerprint(state, input),
      }),
    )
}
