import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Semaphore from 'effect/Semaphore'

/** The lifecycle surface owned from each concrete vscode-languageclient instance. */
export interface LanguageClient {
  readonly start: () => Promise<void>
  readonly stop: () => Promise<void>
}

export class LanguageClientLifecycleError extends Data.TaggedError('LanguageClientLifecycleError')<{
  readonly operation: 'construct' | 'start' | 'stop'
  readonly cause: unknown
}> {}

type State<Client extends LanguageClient> =
  | { readonly _tag: 'Absent' }
  | { readonly _tag: 'Running'; readonly client: Client }

const absent = <Client extends LanguageClient>(): State<Client> => Object.freeze({ _tag: 'Absent' })

const LanguageClientLifecycleTypeId: unique symbol = Symbol.for(
  '@silk-effect/vscode/LanguageClientLifecycle',
)

interface LifecycleState<Client extends LanguageClient> {
  state: State<Client>
  readonly semaphore: Semaphore.Semaphore
  readonly construct: () => Client
}

/** Serialized ownership of the extension's replaceable language client. */
export interface LanguageClientLifecycle<Client extends LanguageClient> {
  readonly [LanguageClientLifecycleTypeId]: LifecycleState<Client>
}

/** Creates an absent client lifecycle with injectable construction. */
export const make = <Client extends LanguageClient>(
  construct: () => Client,
): LanguageClientLifecycle<Client> => ({
  [LanguageClientLifecycleTypeId]: {
    state: absent(),
    semaphore: Semaphore.makeUnsafe(1),
    construct,
  },
})

const construct = <Client extends LanguageClient>(state: LifecycleState<Client>) =>
  Effect.try({
    try: state.construct,
    catch: (cause) => new LanguageClientLifecycleError({ operation: 'construct', cause }),
  })

const startClient = (client: LanguageClient) =>
  Effect.tryPromise({
    try: () => client.start(),
    catch: (cause) => new LanguageClientLifecycleError({ operation: 'start', cause }),
  })

const stopClient = (client: LanguageClient) =>
  Effect.tryPromise({
    try: () => client.stop(),
    catch: (cause) => new LanguageClientLifecycleError({ operation: 'stop', cause }),
  })

const startUnlocked = Effect.fnUntraced(function* <Client extends LanguageClient>(
  state: LifecycleState<Client>,
) {
  if (state.state._tag === 'Running') return
  const client = yield* construct(state)
  yield* startClient(client)
  state.state = Object.freeze({ _tag: 'Running', client })
})

const stopUnlocked = Effect.fnUntraced(function* <Client extends LanguageClient>(
  state: LifecycleState<Client>,
) {
  const current = state.state
  state.state = absent()
  if (current._tag === 'Running') yield* stopClient(current.client)
})

/** Starts a client only when the lifecycle is absent. */
export const start = Effect.fn('LanguageClientLifecycle.start')(function* <
  Client extends LanguageClient,
>(self: LanguageClientLifecycle<Client>) {
  const state = self[LanguageClientLifecycleTypeId]
  yield* state.semaphore.withPermit(startUnlocked(state))
})

/** Retires the current client and leaves the lifecycle absent even when stop fails. */
export const stop = Effect.fn('LanguageClientLifecycle.stop')(function* <
  Client extends LanguageClient,
>(self: LanguageClientLifecycle<Client>) {
  const state = self[LanguageClientLifecycleTypeId]
  yield* state.semaphore.withPermit(stopUnlocked(state))
})

/** Retires the old client and always constructs a fresh replacement after its stop exits. */
export const restart = Effect.fn('LanguageClientLifecycle.restart')(function* <
  Client extends LanguageClient,
>(self: LanguageClientLifecycle<Client>) {
  const state = self[LanguageClientLifecycleTypeId]
  yield* state.semaphore.withPermit(
    Effect.gen(function* () {
      yield* Effect.exit(stopUnlocked(state))
      yield* startUnlocked(state)
    }),
  )
})

/** Returns the currently running client for inspector notification registration. */
export const current = <Client extends LanguageClient>(
  self: LanguageClientLifecycle<Client>,
): Client | undefined =>
  self[LanguageClientLifecycleTypeId].state._tag === 'Running'
    ? self[LanguageClientLifecycleTypeId].state.client
    : undefined

/** Observes whether the lifecycle currently owns a running client. */
export const isRunning = <Client extends LanguageClient>(
  self: LanguageClientLifecycle<Client>,
): boolean => self[LanguageClientLifecycleTypeId].state._tag === 'Running'

/** Runs activation at the CommonJS extension boundary. */
export const runStart = <Client extends LanguageClient>(
  self: LanguageClientLifecycle<Client>,
): Promise<void> => Effect.runPromise(start(self))

/** Runs fresh-client replacement at the CommonJS extension boundary. */
export const runRestart = <Client extends LanguageClient>(
  self: LanguageClientLifecycle<Client>,
): Promise<void> => Effect.runPromise(restart(self))

/** Runs retirement at the CommonJS extension boundary. */
export const runStop = <Client extends LanguageClient>(
  self: LanguageClientLifecycle<Client>,
): Promise<void> => Effect.runPromise(stop(self))
