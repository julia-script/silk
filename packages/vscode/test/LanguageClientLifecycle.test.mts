import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as LanguageClientLifecycle from '../src/LanguageClientLifecycle.mjs'

class FakeClient implements LanguageClientLifecycle.LanguageClient {
  constructor(
    readonly name: string,
    private readonly events: Array<string>,
    private readonly startResult: () => Promise<void> = () => Promise.resolve(),
    private readonly stopResult: () => Promise<void> = () => Promise.resolve(),
  ) {}

  start(): Promise<void> {
    this.events.push(`${this.name}:start`)
    return this.startResult()
  }

  stop(): Promise<void> {
    this.events.push(`${this.name}:stop`)
    return this.stopResult()
  }
}

const nextClient = (clients: Array<FakeClient>): FakeClient => {
  const client = clients.shift()
  if (client === undefined) throw new Error('missing fake client')
  return client
}

it.effect('owns activation and normal stop/start replacement explicitly', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    const clients = [new FakeClient('first', events), new FakeClient('second', events)]
    const lifecycle = LanguageClientLifecycle.make(() => nextClient(clients))

    yield* LanguageClientLifecycle.start(lifecycle)
    yield* LanguageClientLifecycle.start(lifecycle)
    assert.strictEqual(LanguageClientLifecycle.current(lifecycle)?.name, 'first')
    assert.deepEqual(events, ['first:start'])

    yield* LanguageClientLifecycle.restart(lifecycle)
    assert.strictEqual(LanguageClientLifecycle.current(lifecycle)?.name, 'second')
    assert.deepEqual(events, ['first:start', 'first:stop', 'second:start'])

    yield* LanguageClientLifecycle.stop(lifecycle)
    assert.isFalse(LanguageClientLifecycle.isRunning(lifecycle))
    assert.deepEqual(events, ['first:start', 'first:stop', 'second:start', 'second:stop'])
  }),
)

it.effect('starts a fresh client after the retired client stop times out', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    const clients = [
      new FakeClient('timed-out', events, undefined, () =>
        Promise.reject(new Error('Stopping the server timed out')),
      ),
      new FakeClient('replacement', events),
    ]
    const lifecycle = LanguageClientLifecycle.make(() => nextClient(clients))

    yield* LanguageClientLifecycle.start(lifecycle)
    yield* LanguageClientLifecycle.restart(lifecycle)

    assert.strictEqual(LanguageClientLifecycle.current(lifecycle)?.name, 'replacement')
    assert.deepEqual(events, ['timed-out:start', 'timed-out:stop', 'replacement:start'])
  }),
)

it.effect('leaves no client marked running when replacement startup fails', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    const clients = [
      new FakeClient('first', events),
      new FakeClient('broken', events, () => Promise.reject(new Error('replacement failed'))),
    ]
    const lifecycle = LanguageClientLifecycle.make(() => nextClient(clients))
    yield* LanguageClientLifecycle.start(lifecycle)

    const restarted = yield* Effect.exit(LanguageClientLifecycle.restart(lifecycle))

    assert.isTrue(Exit.isFailure(restarted))
    assert.isFalse(LanguageClientLifecycle.isRunning(lifecycle))
    assert.isUndefined(LanguageClientLifecycle.current(lifecycle))
    assert.deepEqual(events, ['first:start', 'first:stop', 'broken:start'])
  }),
)

it.effect('serializes concurrent restart commands', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    let releaseStop: (() => void) | undefined
    const controlledStop = new Promise<void>((resolve) => {
      releaseStop = resolve
    })
    const clients = [
      new FakeClient('first', events, undefined, () => controlledStop),
      new FakeClient('second', events),
      new FakeClient('third', events),
    ]
    const lifecycle = LanguageClientLifecycle.make(() => nextClient(clients))
    yield* LanguageClientLifecycle.start(lifecycle)
    const firstRestart = yield* Effect.forkChild(LanguageClientLifecycle.restart(lifecycle), {
      startImmediately: true,
    })
    const secondRestart = yield* Effect.forkChild(LanguageClientLifecycle.restart(lifecycle), {
      startImmediately: true,
    })
    yield* Effect.yieldNow

    assert.deepEqual(events, ['first:start', 'first:stop'])
    if (releaseStop === undefined) throw new Error('controlled stop did not start')
    releaseStop()
    yield* Fiber.join(firstRestart)
    yield* Fiber.join(secondRestart)

    assert.strictEqual(LanguageClientLifecycle.current(lifecycle)?.name, 'third')
    assert.deepEqual(events, [
      'first:start',
      'first:stop',
      'second:start',
      'second:stop',
      'third:start',
    ])
  }),
)
