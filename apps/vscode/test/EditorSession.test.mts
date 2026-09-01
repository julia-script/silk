import { assert, it } from '@effect/vitest'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as TestClock from 'effect/testing/TestClock'
import * as EditorSession from '../src/EditorSession.mjs'

const neverPromise = (): Promise<void> => Effect.runPromise(Effect.never)

const uri = 'file:///workspace/Main.silk'

class FakeProcess implements EditorSession.OwnedProcess {
  private readonly exited = Deferred.makeUnsafe<void>()
  readonly awaitExit = Deferred.await(this.exited)
  readonly terminate: Effect.Effect<void, EditorSession.EditorSessionError>

  constructor(
    private readonly name: string,
    private readonly events: Array<string>,
  ) {
    this.terminate = Effect.fnUntraced(function* (this: FakeProcess) {
      this.events.push(`${this.name}:terminate`)
      yield* Deferred.succeed(this.exited, undefined)
    }).call(this)
  }

  exit(): void {
    this.events.push(`${this.name}:exit`)
    Deferred.doneUnsafe(this.exited, Effect.void)
  }
}

class FakeClient implements EditorSession.EditorClient {
  private readonly sourceListeners = new Set<(event: EditorSession.SourceSynchronization) => void>()
  private readonly invalidationListeners = new Set<() => void>()
  readonly deleted: Array<string> = []

  constructor(
    readonly name: string,
    private readonly events: Array<string>,
    process: FakeProcess,
    private readonly document: () => EditorSession.DocumentVersion | undefined,
    private readonly startResult: () => Promise<void> = () => Promise.resolve(),
    private readonly stopResult: () => Promise<void> = () => {
      process.exit()
      return Promise.resolve()
    },
    private readonly acceptanceResult: () => boolean = () => true,
  ) {}

  async start(): Promise<void> {
    this.events.push(`${this.name}:start`)
    await this.startResult()
    const document = this.document()
    if (document !== undefined)
      for (const listener of this.sourceListeners)
        listener({ _tag: 'Open', uri: document.uri, version: document.version })
  }

  stop(): Promise<void> {
    this.events.push(`${this.name}:stop`)
    return this.stopResult()
  }

  sendRequest<A>(method: string, parameters?: unknown): Promise<A> {
    this.events.push(`${this.name}:request:${method}`)
    const value =
      method === 'silk/acceptedSourceVersions' ? { accepted: this.acceptanceResult() } : parameters
    return Promise.resolve(value as A)
  }

  onSourceSynchronization(
    listener: (event: EditorSession.SourceSynchronization) => void,
  ): EditorSession.Disposable {
    this.sourceListeners.add(listener)
    return { dispose: () => this.sourceListeners.delete(listener) }
  }

  onInvalidation(listener: () => void): EditorSession.Disposable {
    this.invalidationListeners.add(listener)
    return { dispose: () => this.invalidationListeners.delete(listener) }
  }

  deleteDiagnostics(target: string): void {
    this.deleted.push(target)
  }

  invalidate(): void {
    for (const listener of this.invalidationListeners) listener()
  }
}

interface Prepared {
  readonly client: FakeClient
  readonly process: FakeProcess
  gate: EditorSession.DiagnosticGate | undefined
}

const next = (prepared: Array<Prepared>): Prepared => {
  const value = prepared.shift()
  if (value === undefined) throw new Error('missing prepared generation')
  return value
}

it.effect('acknowledges synchronization and preserves stable consumers across replacement', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    let current = { uri, version: 1 }
    const prepared: Array<Prepared> = []
    const prepare = (name: string): Prepared => {
      const process = new FakeProcess(name, events)
      const client = new FakeClient(name, events, process, () => current)
      const value = {
        client,
        process,
        gate: undefined,
      }
      prepared.push(value)
      return value
    }
    const first = prepare('first')
    const second = prepare('second')
    const session = EditorSession.make({
      currentDocuments: () => [current],
      construct: (_generation, gate) =>
        Effect.sync(() => {
          const value = next(prepared)
          value.gate = gate
          events.push(`${value.client.name}:construct`)
          return { client: value.client, process: value.process }
        }),
    })
    let invalidations = 0
    const subscription = EditorSession.onInvalidation(session, () => {
      invalidations += 1
    })

    yield* EditorSession.start(session)
    assert.strictEqual(EditorSession.status(session), 'Ready')
    assert.isTrue(first.gate?.isCurrent(uri, 1) ?? false)
    first.client.invalidate()
    assert.strictEqual(invalidations, 1)
    assert.deepEqual(yield* EditorSession.request(session, 'silk/example', { value: 1 }), {
      value: 1,
    })

    current = { uri, version: 2 }
    EditorSession.documentChanged(session, current)
    assert.deepEqual(first.client.deleted, [uri])
    assert.isFalse(first.gate?.isCurrent(uri, 1) ?? true)
    yield* EditorSession.restart(session)
    assert.strictEqual(EditorSession.status(session), 'Ready')
    assert.isFalse(first.gate?.isCurrent(uri, 2) ?? true)
    assert.isTrue(second.gate?.isCurrent(uri, 2) ?? false)
    second.client.invalidate()
    assert.strictEqual(invalidations, 2)
    assert.deepEqual(events, [
      'first:construct',
      'first:start',
      'first:request:silk/acceptedSourceVersions',
      'first:request:silk/example',
      'first:stop',
      'first:exit',
      'second:construct',
      'second:start',
      'second:request:silk/acceptedSourceVersions',
    ])

    EditorSession.documentClosed(session, uri)
    assert.deepEqual(second.client.deleted, [uri])
    subscription.dispose()
    yield* EditorSession.stop(session)
    yield* EditorSession.stop(session)
    assert.strictEqual(EditorSession.status(session), 'Absent')
  }),
)

it.effect('forces acknowledged process retirement before starting a replacement', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    const document = { uri, version: 1 }
    const firstProcess = new FakeProcess('first', events)
    const first = new FakeClient(
      'first',
      events,
      firstProcess,
      () => document,
      undefined,
      neverPromise,
    )
    const secondProcess = new FakeProcess('second', events)
    const second = new FakeClient('second', events, secondProcess, () => document)
    const queue: Array<EditorSession.ClientGeneration> = [
      { client: first, process: firstProcess },
      { client: second, process: secondProcess },
    ]
    const owned = EditorSession.make({
      currentDocuments: () => [document],
      retirementDeadline: 10,
      construct: () =>
        Effect.sync(() => {
          const value = queue.shift()
          if (value === undefined) throw new Error('missing generation')
          return value
        }),
    })
    yield* EditorSession.start(owned)
    const replacement = yield* Effect.forkChild(EditorSession.restart(owned), {
      startImmediately: true,
    })
    while (!events.includes('first:stop')) yield* Effect.yieldNow
    yield* Effect.promise(() => Promise.resolve())
    yield* Effect.yieldNow
    assert.deepEqual(events.slice(-1), ['first:stop'])
    yield* TestClock.adjust(10)
    yield* Fiber.join(replacement)
    assert.deepEqual(events.slice(-3), [
      'first:terminate',
      'second:start',
      'second:request:silk/acceptedSourceVersions',
    ])
    yield* EditorSession.stop(owned)
  }),
)

it.effect('becomes explicitly unavailable when synchronization or replacement startup fails', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    const document = { uri, version: 1 }
    const firstProcess = new FakeProcess('first', events)
    const first = new FakeClient('first', events, firstProcess, () => document)
    const brokenProcess = new FakeProcess('broken', events)
    const broken = new FakeClient(
      'broken',
      events,
      brokenProcess,
      () => document,
      () => Promise.reject(new Error('controlled start failure')),
    )
    const queue: Array<EditorSession.ClientGeneration> = [
      { client: first, process: firstProcess },
      { client: broken, process: brokenProcess },
    ]
    const session = EditorSession.make({
      currentDocuments: () => [document],
      construct: () =>
        Effect.sync(() => {
          const value = queue.shift()
          if (value === undefined) throw new Error('missing generation')
          return value
        }),
    })
    yield* EditorSession.start(session)
    const restarted = yield* Effect.exit(EditorSession.restart(session))
    assert.isTrue(Exit.isFailure(restarted))
    assert.strictEqual(EditorSession.status(session), 'Unavailable')
    assert.deepEqual(events.slice(-4), ['first:exit', 'broken:start', 'broken:stop', 'broken:exit'])
  }),
)

it.effect('starts when the server accepted an already-open document missed by the observer', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    const document = { uri, version: 1 }
    const process = new FakeProcess('silent', events)
    const client = new FakeClient('silent', events, process, () => undefined)
    const session = EditorSession.make({
      currentDocuments: () => [document],
      synchronizationDeadline: 10,
      construct: () => Effect.succeed({ client, process }),
    })
    const started = yield* Effect.forkChild(Effect.exit(EditorSession.start(session)), {
      startImmediately: true,
    })
    while (!events.includes('silent:start')) yield* Effect.yieldNow
    yield* Effect.promise(() => Promise.resolve())
    yield* Effect.yieldNow
    yield* TestClock.adjust(10)
    yield* Effect.yieldNow
    yield* TestClock.adjust(10)
    assert.isTrue(Exit.isSuccess(yield* Fiber.join(started)))
    assert.strictEqual(EditorSession.status(session), 'Ready')
    yield* EditorSession.stop(session)
  }),
)

it.effect('times out when the server has not accepted an already-open document', () =>
  Effect.gen(function* () {
    const events: Array<string> = []
    const document = { uri, version: 1 }
    const process = new FakeProcess('unaccepted', events)
    const client = new FakeClient(
      'unaccepted',
      events,
      process,
      () => undefined,
      undefined,
      undefined,
      () => false,
    )
    const session = EditorSession.make({
      currentDocuments: () => [document],
      synchronizationDeadline: 10,
      construct: () => Effect.succeed({ client, process }),
    })
    const started = yield* Effect.forkChild(Effect.exit(EditorSession.start(session)), {
      startImmediately: true,
    })
    while (!events.includes('unaccepted:request:silk/acceptedSourceVersions'))
      yield* Effect.yieldNow
    yield* TestClock.adjust(10)
    assert.isTrue(Exit.isFailure(yield* Fiber.join(started)))
    assert.strictEqual(EditorSession.status(session), 'Unavailable')
  }),
)
