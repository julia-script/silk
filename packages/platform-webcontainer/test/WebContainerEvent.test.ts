import { assert, it } from '@effect/vitest'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as Stream from 'effect/Stream'
import * as WebContainerEvent from '../src/WebContainerEvent.js'

it.effect('preserves event fields and callback order', () =>
  Effect.gen(function* () {
    const ready = yield* Deferred.make<void>()
    let listener: ((event: WebContainerEvent.Port) => void) | undefined
    let unsubscribes = 0
    const events = WebContainerEvent.stream<WebContainerEvent.Port>((registered) =>
      Effect.gen(function* () {
        listener = registered
        yield* Deferred.succeed(ready, undefined)
        return {
          unsubscribe: Effect.sync(() => {
            unsubscribes += 1
          }),
        }
      }),
    )
    const fiber = yield* events.pipe(Stream.take(2), Stream.runCollect, Effect.forkChild)
    yield* Deferred.await(ready)
    const installed = listener
    if (installed === undefined) {
      return yield* Effect.die(new Error('listener was not installed'))
    }
    installed(WebContainerEvent.port(3000, 'open', 'https://one.test'))
    installed(WebContainerEvent.port(3000, 'close', 'https://one.test'))
    const values = yield* Fiber.join(fiber)
    assert.deepStrictEqual(
      values.map((event) => event.status),
      ['open', 'close'],
    )
    assert.strictEqual(values[0]?.url, 'https://one.test')
    assert.strictEqual(unsubscribes, 1)
  }),
)

it.effect('uses independent subscriptions and cleans up interrupted consumers', () =>
  Effect.gen(function* () {
    const listeners = new Set<(event: WebContainerEvent.ServerReady) => void>()
    let unsubscribes = 0
    const subscribed = yield* Deferred.make<void>()
    const events = WebContainerEvent.stream<WebContainerEvent.ServerReady>((listener) =>
      Effect.gen(function* () {
        listeners.add(listener)
        if (listeners.size === 2) {
          yield* Deferred.succeed(subscribed, undefined)
        }
        return {
          unsubscribe: Effect.sync(() => {
            listeners.delete(listener)
            unsubscribes += 1
          }),
        }
      }),
    )
    const first = yield* events.pipe(Stream.runDrain, Effect.forkChild)
    const second = yield* events.pipe(Stream.take(1), Stream.runCollect, Effect.forkChild)
    yield* Deferred.await(subscribed)
    yield* Fiber.interrupt(first)
    assert.strictEqual(listeners.size, 1)
    for (const listener of listeners) {
      listener(WebContainerEvent.serverReady(4173, 'https://ready.test'))
    }
    const values = yield* Fiber.join(second)
    assert.strictEqual(values[0]?.port, 4173)
    assert.strictEqual(unsubscribes, 2)
    assert.strictEqual(listeners.size, 0)
  }),
)

it('constructs every preview and runtime event variant without losing fields', () => {
  assert.deepStrictEqual(WebContainerEvent.internalError('broken'), {
    _tag: 'InternalError',
    message: 'broken',
  })
  assert.deepStrictEqual(WebContainerEvent.serverReady(80, 'https://example.test'), {
    _tag: 'ServerReady',
    port: 80,
    url: 'https://example.test',
  })
})
