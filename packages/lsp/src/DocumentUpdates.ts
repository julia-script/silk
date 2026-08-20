import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'

interface Update {
  readonly version: number
  readonly done: Deferred.Deferred<void>
}

const DocumentUpdatesTypeId: unique symbol = Symbol.for('@silk-effect/lsp/DocumentUpdates')

/** Per-document protocol ordering without a server-global update barrier. */
export interface DocumentUpdates {
  readonly [DocumentUpdatesTypeId]: Map<string, Update>
}

/** Creates an empty collection of independent document synchronization lanes. */
export const make = (): DocumentUpdates => ({ [DocumentUpdatesTypeId]: new Map() })

/** Runs one update after the previously accepted update for the same URI. */
export const enqueue = Effect.fn('DocumentUpdates.enqueue')(function* <A, E, R>(
  self: DocumentUpdates,
  uri: string,
  version: number,
  effect: Effect.Effect<A, E, R>,
): Effect.fn.Return<A, E, R> {
  const updates = self[DocumentUpdatesTypeId]
  const previous = updates.get(uri)
  const current: Update = Object.freeze({ version, done: Deferred.makeUnsafe<void>() })
  updates.set(uri, current)
  return yield* Effect.gen(function* () {
    if (previous !== undefined) yield* Deferred.await(previous.done)
    return yield* effect
  }).pipe(
    Effect.ensuring(
      Effect.gen(function* () {
        if (updates.get(uri) === current) updates.delete(uri)
        yield* Deferred.succeed(current.done, undefined)
      }),
    ),
  )
})

/** Waits only for the update that was current for this URI when the operation began. */
export const awaitCurrent = Effect.fn('DocumentUpdates.awaitCurrent')(function* (
  self: DocumentUpdates,
  uri: string,
) {
  const current = self[DocumentUpdatesTypeId].get(uri)
  if (current !== undefined) yield* Deferred.await(current.done)
})

/** Waits for every URI lane that was current when draining began. */
export const drain = Effect.fn('DocumentUpdates.drain')(function* (self: DocumentUpdates) {
  const current = [...self[DocumentUpdatesTypeId].values()]
  for (const update of current) yield* Deferred.await(update.done)
})
