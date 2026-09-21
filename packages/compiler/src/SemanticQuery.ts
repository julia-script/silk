import * as Effect from 'effect/Effect'

/** One immutable dependency observed while a semantic query executes. */
export type Observation =
  | { readonly _tag: 'Query'; readonly key: string }
  | {
      readonly _tag: 'Namespace'
      readonly module: string
      readonly spelling: string
      readonly candidates: ReadonlyArray<string>
    }
  | { readonly _tag: 'Header'; readonly declaration: string }
  | { readonly _tag: 'ImportSelection'; readonly module: string; readonly binding: string }
  | { readonly _tag: 'Alias'; readonly declaration: string }
  | { readonly _tag: 'Bound'; readonly declaration: string }
  | { readonly _tag: 'Conformance'; readonly key: string }
  | { readonly _tag: 'Configuration'; readonly key: string; readonly value: string }

/** Records a dependency at the exact semantic read which determined an answer. */
export type Observe = (observation: Observation) => void

/** One typed request executed by the session's authoritative provider for that request family. */
export interface Request<A> {
  readonly _tag: 'SemanticQueryRequest'
  readonly key: string
  readonly execute: (observe: Observe) => A
}

/** A completed immutable answer and the dependencies observed while producing it. */
export interface Completed<A> {
  readonly key: string
  readonly answer: A
  readonly observations: ReadonlyArray<Observation>
}

/** Explicit active-query recursion; providers decide whether it is semantically available. */
export interface Cycle {
  readonly _tag: 'SemanticQueryCycle'
  readonly key: string
  readonly path: ReadonlyArray<string>
}

export type Result<A> =
  | { readonly _tag: 'Completed'; readonly completed: Completed<A>; readonly reused: boolean }
  | { readonly _tag: 'Cycle'; readonly cycle: Cycle }

/** The public immutable identity of one fresh semantic environment. */
export interface Session {
  readonly _tag: 'SemanticQuerySession'
  readonly epoch: string
}

interface Active {
  readonly key: string
  readonly observations: Array<Observation>
}

interface State {
  readonly completed: Map<string, Completed<unknown>>
  readonly active: Array<Active>
  readonly executions: Map<string, number>
}

const states = new WeakMap<Session, State>()

const stateOf = (self: Session): State => {
  const state = states.get(self)
  if (state === undefined) throw new RangeError('Unknown semantic query session')
  return state
}

/** Creates an empty answer store for exactly one immutable semantic environment. */
export const make = (epoch: string): Session => {
  const session = Object.freeze({ _tag: 'SemanticQuerySession' as const, epoch })
  states.set(session, { completed: new Map(), active: [], executions: new Map() })
  return session
}

const observeInto = (active: Active, observation: Observation): void => {
  const encoded = JSON.stringify(observation)
  if (active.observations.some((candidate) => JSON.stringify(candidate) === encoded)) return
  active.observations.push(Object.freeze(observation))
}

const observeParent = (state: State, key: string): void => {
  const parent = state.active.at(-1)
  if (parent !== undefined) observeInto(parent, Object.freeze({ _tag: 'Query', key }))
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

const execute = <A>(self: Session, request: Request<A>, publish: boolean): Result<A> => {
  const state = stateOf(self)
  observeParent(state, request.key)
  if (publish) {
    const stored = state.completed.get(request.key)
    if (stored !== undefined)
      // The store is populated only by the same keyed request family. This cast is the local
      // variance bridge which keeps unknown out of every public answer channel.
      return Object.freeze({
        _tag: 'Completed',
        completed: stored as Completed<A>,
        reused: true,
      })
  }
  if (state.active.some((active) => active.key === request.key)) return cycle(state, request.key)
  const active: Active = { key: request.key, observations: [] }
  state.active.push(active)
  try {
    state.executions.set(request.key, (state.executions.get(request.key) ?? 0) + 1)
    const answer = request.execute((observation) => observeInto(active, observation))
    const completed = Object.freeze({
      key: request.key,
      answer,
      observations: Object.freeze([...active.observations]),
    })
    if (publish) state.completed.set(request.key, completed)
    return Object.freeze({ _tag: 'Completed', completed, reused: false })
  } finally {
    const removed = state.active.pop()
    if (removed !== active) throw new RangeError('Semantic query reservation stack is corrupted')
  }
}

/** Executes or reuses a request in this session. Defects never publish partial answers. */
export const query = <A>(self: Session, request: Request<A>): Result<A> =>
  execute(self, request, true)

/** Executes the same provider without reading or publishing the completed-answer store. */
export const fresh = <A>(self: Session, request: Request<A>): Result<A> =>
  execute(self, request, false)

/**
 * Effect boundary for cancellable callers. The yield is the supported cancellation boundary;
 * synchronous providers remain deliberately non-preemptive.
 */
export const queryEffect = Effect.fn('SemanticQuery.query')(function* <A>(
  self: Session,
  request: Request<A>,
): Effect.fn.Return<Completed<A>, Cycle> {
  yield* Effect.yieldNow
  const result = query(self, request)
  return result._tag === 'Cycle' ? yield* Effect.fail(result.cycle) : result.completed
})

/** Reads a completed result for inspection without executing its provider. */
export const completed = <A>(self: Session, key: string): Completed<A> | undefined =>
  // See execute(): request keys uniquely determine their answer family.
  stateOf(self).completed.get(key) as Completed<A> | undefined

/** Structural counter used by focused query-boundary fixtures. */
export const executionCount = (self: Session, key: string): number =>
  stateOf(self).executions.get(key) ?? 0

/** True only while a provider currently owns the request reservation. */
export const isActive = (self: Session, key: string): boolean =>
  stateOf(self).active.some((active) => active.key === key)
