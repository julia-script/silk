import { assert, it } from '@effect/vitest'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as DocumentUpdates from '../src/DocumentUpdates.js'

it.effect('orders one URI while unrelated document lanes remain live', () =>
  Effect.gen(function* () {
    const updates = DocumentUpdates.make()
    const release = yield* Deferred.make<void>()
    const events: Array<string> = []
    const slow = yield* Effect.forkChild(
      DocumentUpdates.enqueue(
        updates,
        'file:///slow/Main.silk',
        1,
        Effect.gen(function* () {
          events.push('slow:start')
          yield* Deferred.await(release)
          events.push('slow:end')
        }),
      ),
      { startImmediately: true },
    )
    const next = yield* Effect.forkChild(
      DocumentUpdates.enqueue(
        updates,
        'file:///slow/Main.silk',
        2,
        Effect.sync(() => events.push('slow:next')),
      ),
      { startImmediately: true },
    )
    yield* DocumentUpdates.enqueue(
      updates,
      'file:///ready/Main.silk',
      1,
      Effect.sync(() => events.push('ready')),
    )
    yield* DocumentUpdates.awaitCurrent(updates, 'file:///ready/Main.silk')

    assert.deepEqual(events, ['slow:start', 'ready'])
    yield* Deferred.succeed(release, undefined)
    yield* Fiber.join(slow)
    yield* Fiber.join(next)
    yield* DocumentUpdates.drain(updates)
    assert.deepEqual(events, ['slow:start', 'ready', 'slow:end', 'slow:next'])
  }),
)
