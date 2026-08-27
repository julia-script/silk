import * as Data from 'effect/Data'
import type * as Duration from 'effect/Duration'
import * as Effect from 'effect/Effect'
import * as Queue from 'effect/Queue'
import * as Result from 'effect/Result'
import * as Semaphore from 'effect/Semaphore'

export interface DocumentVersion {
  readonly uri: string
  readonly version: number
}

export type SourceSynchronization =
  | { readonly _tag: 'Open'; readonly uri: string; readonly version: number }
  | { readonly _tag: 'Change'; readonly uri: string; readonly version: number }
  | { readonly _tag: 'Close'; readonly uri: string }

export interface Disposable {
  readonly dispose: () => void
}

export interface EditorClient {
  readonly start: () => Promise<void>
  readonly stop: () => Promise<void>
  readonly sendRequest: <A>(method: string, parameters?: unknown) => Promise<A>
  readonly onSourceSynchronization: (listener: (event: SourceSynchronization) => void) => Disposable
  readonly onInvalidation: (listener: () => void) => Disposable
  readonly deleteDiagnostics: (uri: string) => void
}

export interface OwnedProcess {
  readonly awaitExit: Effect.Effect<unknown>
  readonly terminate: Effect.Effect<void, EditorSessionError>
}

export interface ClientGeneration {
  readonly client: EditorClient
  readonly process: OwnedProcess
}

export interface DiagnosticGate {
  readonly isCurrent: (uri: string, version: number | undefined) => boolean
}

export class EditorSessionError extends Data.TaggedError('EditorSessionError')<{
  readonly operation: 'construct' | 'start' | 'synchronize' | 'request' | 'retire'
  readonly message: string
  readonly cause?: unknown
}> {}

export interface Options {
  readonly construct: (
    generation: number,
    gate: DiagnosticGate,
  ) => Effect.Effect<ClientGeneration, EditorSessionError>
  readonly currentDocuments: () => ReadonlyArray<DocumentVersion>
  readonly retirementDeadline?: Duration.Input
  readonly synchronizationDeadline?: Duration.Input
  readonly acceptanceRequest?: string
}

type Status =
  | { readonly _tag: 'Absent' }
  | { readonly _tag: 'Starting'; readonly generation: ActiveGeneration }
  | { readonly _tag: 'Ready'; readonly generation: ActiveGeneration }
  | { readonly _tag: 'Retiring'; readonly generation: ActiveGeneration }
  | { readonly _tag: 'Unavailable'; readonly error: EditorSessionError }

interface ActiveGeneration extends ClientGeneration {
  readonly id: number
  readonly bindings: Array<Disposable>
  readonly signal: Queue.Queue<void>
}

interface State {
  status: Status
  nextGeneration: number
  readonly semaphore: Semaphore.Semaphore
  readonly options: Required<
    Pick<Options, 'retirementDeadline' | 'synchronizationDeadline' | 'acceptanceRequest'>
  > &
    Omit<Options, 'retirementDeadline' | 'synchronizationDeadline' | 'acceptanceRequest'>
  readonly editorVersions: Map<string, number>
  readonly invalidationSubscribers: Set<() => void>
}

const TypeId: unique symbol = Symbol.for('@silk-lang/vscode/EditorSession')

export interface EditorSession {
  readonly [TypeId]: State
}

const defaults = Object.freeze({
  retirementDeadline: 2_000,
  synchronizationDeadline: 2_000,
  acceptanceRequest: 'silk/acceptedSourceVersions',
})

/** Creates the stable extension-lifetime editor session without exposing a concrete client. */
export const make = (options: Options): EditorSession => ({
  [TypeId]: {
    status: { _tag: 'Absent' },
    nextGeneration: 0,
    semaphore: Semaphore.makeUnsafe(1),
    options: { ...defaults, ...options },
    editorVersions: new Map(options.currentDocuments().map((entry) => [entry.uri, entry.version])),
    invalidationSubscribers: new Set(),
  },
})

const currentGeneration = (state: State): ActiveGeneration | undefined => {
  switch (state.status._tag) {
    case 'Starting':
    case 'Ready':
    case 'Retiring':
      return state.status.generation
    case 'Absent':
    case 'Unavailable':
      return undefined
  }
}

const isCurrent = (
  state: State,
  generation: number,
  uri: string,
  version: number | undefined,
): boolean => {
  if (
    (state.status._tag !== 'Starting' && state.status._tag !== 'Ready') ||
    state.status.generation.id !== generation
  )
    return false
  return version === undefined || state.editorVersions.get(uri) === version
}

const startClient = (client: EditorClient) =>
  Effect.tryPromise({
    try: () => client.start(),
    catch: (cause) =>
      new EditorSessionError({
        operation: 'start',
        message: 'Could not start the Silk language client',
        cause,
      }),
  })

const stopClient = (client: EditorClient) =>
  Effect.tryPromise({
    try: () => client.stop(),
    catch: (cause) =>
      new EditorSessionError({
        operation: 'retire',
        message: 'Could not stop the Silk language client',
        cause,
      }),
  })

const disposeBindings = (generation: ActiveGeneration): void => {
  for (const binding of generation.bindings.splice(0)) binding.dispose()
}

const retire = Effect.fnUntraced(function* (state: State, generation: ActiveGeneration) {
  state.status = { _tag: 'Retiring', generation }
  disposeBindings(generation)
  const exited = yield* Effect.race(
    stopClient(generation.client).pipe(
      Effect.ignore,
      Effect.andThen(generation.process.awaitExit),
      Effect.as(true),
    ),
    Effect.sleep(state.options.retirementDeadline).pipe(Effect.as(false)),
  )
  if (!exited) yield* generation.process.terminate
})

const synchronize = Effect.fnUntraced(function* (state: State, generation: ActiveGeneration) {
  const wait = Effect.gen(function* () {
    while (true) {
      const expected = state.options.currentDocuments()
      state.editorVersions.clear()
      for (const document of expected) state.editorVersions.set(document.uri, document.version)
      const response = yield* Effect.tryPromise({
        try: () =>
          generation.client.sendRequest<{ readonly accepted: boolean }>(
            state.options.acceptanceRequest,
            { entries: expected },
          ),
        catch: (cause) =>
          new EditorSessionError({
            operation: 'synchronize',
            message: 'The language server did not acknowledge synchronized documents',
            cause,
          }),
      })
      const current = state.options.currentDocuments()
      const unchanged =
        current.length === expected.length &&
        current.every((document) =>
          expected.some(
            (entry) => entry.uri === document.uri && entry.version === document.version,
          ),
        )
      if (response.accepted && unchanged) return
      if (!unchanged) continue
      yield* Queue.take(generation.signal)
    }
  })
  yield* Effect.raceFirst(
    wait,
    Effect.sleep(state.options.synchronizationDeadline).pipe(
      Effect.flatMap(
        () =>
          new EditorSessionError({
            operation: 'synchronize',
            message: 'Timed out waiting for language-server source acceptance',
          }),
      ),
    ),
  )
})

const startUnlocked = Effect.fnUntraced(function* (state: State) {
  if (state.status._tag === 'Starting' || state.status._tag === 'Ready') return
  state.nextGeneration += 1
  const id = state.nextGeneration
  const gate: DiagnosticGate = {
    isCurrent: (uri, version) => isCurrent(state, id, uri, version),
  }
  const constructed = yield* state.options.construct(id, gate)
  const signal = yield* Queue.sliding<void>(1)
  const generation: ActiveGeneration = {
    ...constructed,
    id,
    bindings: [],
    signal,
  }
  state.status = { _tag: 'Starting', generation }
  generation.bindings.push(
    generation.client.onSourceSynchronization(() => {
      Queue.offerUnsafe(generation.signal, undefined)
    }),
    generation.client.onInvalidation(() => {
      if (!isCurrent(state, id, '', undefined)) return
      for (const subscriber of state.invalidationSubscribers) subscriber()
    }),
  )
  const started = yield* Effect.result(
    startClient(generation.client).pipe(Effect.andThen(synchronize(state, generation))),
  )
  if (Result.isFailure(started)) {
    const failure = started.failure
    yield* retire(state, generation).pipe(Effect.ignore)
    state.status = { _tag: 'Unavailable', error: failure }
    return yield* failure
  }
  state.status = { _tag: 'Ready', generation }
})

/** Starts the session and becomes ready only after built-in synchronization is acknowledged. */
export const start = Effect.fn('EditorSession.start')(function* (self: EditorSession) {
  const state = self[TypeId]
  yield* state.semaphore.withPermit(startUnlocked(state))
})

/** Retires the acknowledged old process before constructing a fresh client generation. */
export const restart = Effect.fn('EditorSession.restart')(function* (self: EditorSession) {
  const state = self[TypeId]
  yield* state.semaphore.withPermit(
    Effect.gen(function* () {
      const generation = currentGeneration(state)
      if (generation !== undefined) yield* retire(state, generation)
      state.status = { _tag: 'Absent' }
      yield* startUnlocked(state)
    }),
  )
})

/** Stops the session idempotently and acknowledges process death. */
export const stop = Effect.fn('EditorSession.stop')(function* (self: EditorSession) {
  const state = self[TypeId]
  yield* state.semaphore.withPermit(
    Effect.gen(function* () {
      const generation = currentGeneration(state)
      if (generation !== undefined) yield* retire(state, generation)
      state.status = { _tag: 'Absent' }
    }),
  )
})

/** Runs a stable request without exposing the replaceable concrete language client. */
export const request = Effect.fn('EditorSession.request')(function* <A>(
  self: EditorSession,
  method: string,
  parameters?: unknown,
): Effect.fn.Return<A, EditorSessionError> {
  const state = self[TypeId]
  if (state.status._tag !== 'Ready')
    return yield* new EditorSessionError({
      operation: 'request',
      message: 'The Silk editor session is not ready',
    })
  const generation = state.status.generation
  return yield* Effect.tryPromise({
    try: () => generation.client.sendRequest<A>(method, parameters),
    catch: (cause) =>
      new EditorSessionError({
        operation: 'request',
        message: `Silk language request ${method} failed`,
        cause,
      }),
  })
})

/** Immediately retires diagnostics that describe text older than the editor model. */
export const documentChanged = (self: EditorSession, document: DocumentVersion): void => {
  const state = self[TypeId]
  state.editorVersions.set(document.uri, document.version)
  currentGeneration(state)?.client.deleteDiagnostics(document.uri)
}

/** Removes diagnostics and version authority when an editor document closes. */
export const documentClosed = (self: EditorSession, uri: string): void => {
  const state = self[TypeId]
  state.editorVersions.delete(uri)
  currentGeneration(state)?.client.deleteDiagnostics(uri)
}

/** Subscribes once for the session lifetime and survives every concrete client replacement. */
export const onInvalidation = (self: EditorSession, listener: () => void): Disposable => {
  const subscribers = self[TypeId].invalidationSubscribers
  subscribers.add(listener)
  return { dispose: () => subscribers.delete(listener) }
}

export const status = (self: EditorSession): Status['_tag'] => self[TypeId].status._tag

export const runStart = (self: EditorSession): Promise<void> => Effect.runPromise(start(self))
export const runRestart = (self: EditorSession): Promise<void> => Effect.runPromise(restart(self))
export const runStop = (self: EditorSession): Promise<void> => Effect.runPromise(stop(self))
export const runRequest = <A,>(
  self: EditorSession,
  method: string,
  parameters?: unknown,
): Promise<A> => Effect.runPromise(request<A>(self, method, parameters))
