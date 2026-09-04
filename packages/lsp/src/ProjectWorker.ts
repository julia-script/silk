import { Worker } from 'node:worker_threads'
import * as Data from 'effect/Data'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Queue from 'effect/Queue'
import * as Result from 'effect/Result'
import type * as Scope from 'effect/Scope'
import type * as Document from './Document.js'
import type * as ProjectSnapshot from './ProjectSnapshot.js'
import * as ProjectWorkerRuntime from './ProjectWorkerRuntime.js'
import type * as WorkerEpoch from './WorkerEpoch.js'
import * as WorkerProtocol from './WorkerProtocol.js'

export class ProjectWorkerError extends Data.TaggedError('ProjectWorkerError')<{
  readonly operation: 'spawn' | 'send' | 'receive' | 'terminate'
  readonly message: string
  readonly cause?: unknown
}> {}

/** Replaceable processing context for one project's compiler-analysis queries. */
export interface ProjectWorker {
  readonly _tag: 'ProjectWorker'
  readonly epoch: WorkerEpoch.WorkerEpoch
  readonly send: (message: WorkerProtocol.HostMessage) => Effect.Effect<void, ProjectWorkerError>
  readonly take: Effect.Effect<WorkerProtocol.WorkerMessage, ProjectWorkerError>
  readonly shutdown: Effect.Effect<void, ProjectWorkerError>
  readonly terminate: Effect.Effect<void, ProjectWorkerError>
  readonly awaitExit: Effect.Effect<number>
}

export interface Factory {
  readonly spawn: (
    epoch: WorkerEpoch.WorkerEpoch,
  ) => Effect.Effect<ProjectWorker, ProjectWorkerError, Scope.Scope>
}

type Event =
  | { readonly _tag: 'Message'; readonly message: WorkerProtocol.WorkerMessage }
  | { readonly _tag: 'Failure'; readonly error: ProjectWorkerError }

const receive = Effect.fnUntraced(function* (events: Queue.Dequeue<Event>) {
  const event = yield* Queue.take(events)
  return event._tag === 'Message' ? event.message : yield* event.error
})

/** Controlled in-process adapter used by deterministic scheduler tests. */
export const makeInProcess = Effect.fn('ProjectWorker.makeInProcess')(function* (options: {
  readonly epoch: WorkerEpoch.WorkerEpoch
  readonly analyze: (
    documents: ReadonlyArray<Document.Document>,
    previous: ReadonlyMap<string, ProjectSnapshot.DocumentSnapshot>,
    invalidation: {
      readonly priorityUri?: string
      readonly dirtyPaths: ReadonlyArray<string>
      readonly rediscover: boolean
    },
  ) => Effect.Effect<ReadonlyMap<string, ProjectSnapshot.DocumentSnapshot>>
  readonly beforeQuery?: (query: import('./EditorQuery.js').EditorQuery) => Effect.Effect<void>
}): Effect.fn.Return<ProjectWorker> {
  const events = yield* Queue.unbounded<Event>()
  const exited = Deferred.makeUnsafe<number>()
  let stopped = false
  const runtime = yield* ProjectWorkerRuntime.make({
    epoch: options.epoch,
    analyze: options.analyze,
    ...(options.beforeQuery === undefined ? {} : { beforeQuery: options.beforeQuery }),
    emit: Effect.fnUntraced(function* (message) {
      yield* Queue.offer(events, { _tag: 'Message', message })
      if (message._tag === 'Stopped') {
        stopped = true
        yield* Deferred.succeed(exited, 0)
      }
    }),
  })
  const send = Effect.fn('ProjectWorker.send')(function* (message: WorkerProtocol.HostMessage) {
    if (stopped)
      return yield* new ProjectWorkerError({
        operation: 'send',
        message: 'Project worker is stopped',
      })
    yield* runtime.accept(message)
  })
  const shutdown = send({
    protocolVersion: WorkerProtocol.version,
    _tag: 'Shutdown',
    epoch: options.epoch,
  })
  const terminate = Effect.fn('ProjectWorker.terminate')(function* () {
    if (stopped) return
    stopped = true
    yield* runtime.shutdown
    yield* Deferred.succeed(exited, 1)
  })
  return Object.freeze({
    _tag: 'ProjectWorker',
    epoch: options.epoch,
    send,
    take: receive(events),
    shutdown,
    terminate: terminate(),
    awaitExit: Deferred.await(exited),
  })
})

const spawnNode = Effect.fnUntraced(function* (
  epoch: WorkerEpoch.WorkerEpoch,
  entry: URL,
): Effect.fn.Return<ProjectWorker, ProjectWorkerError> {
  const events = yield* Queue.unbounded<Event>()
  const exited = Deferred.makeUnsafe<number>()
  const worker = yield* Effect.try({
    try: () =>
      new Worker(entry, {
        workerData: { epoch },
      }),
    catch: (cause) =>
      new ProjectWorkerError({
        operation: 'spawn',
        message: 'Could not start project worker',
        cause,
      }),
  })
  let stopped = false
  worker.on('message', (input: unknown) => {
    const decoded = WorkerProtocol.decodeWorker(input)
    Queue.offerUnsafe(
      events,
      Result.isSuccess(decoded)
        ? { _tag: 'Message', message: decoded.success }
        : {
            _tag: 'Failure',
            error: new ProjectWorkerError({
              operation: 'receive',
              message: decoded.failure.message,
              cause: decoded.failure,
            }),
          },
    )
  })
  worker.on('error', (cause) => {
    Queue.offerUnsafe(events, {
      _tag: 'Failure',
      error: new ProjectWorkerError({
        operation: 'receive',
        message: 'Project worker emitted an error',
        cause,
      }),
    })
  })
  worker.on('exit', (code) => {
    stopped = true
    Deferred.doneUnsafe(exited, Effect.succeed(code))
  })

  const send = Effect.fn('ProjectWorker.send')(function* (message: WorkerProtocol.HostMessage) {
    if (stopped)
      return yield* new ProjectWorkerError({
        operation: 'send',
        message: 'Project worker is stopped',
      })
    yield* Effect.try({
      try: () => worker.postMessage(message),
      catch: (cause) =>
        new ProjectWorkerError({
          operation: 'send',
          message: 'Could not send project worker message',
          cause,
        }),
    })
  })
  const shutdown = send({
    protocolVersion: WorkerProtocol.version,
    _tag: 'Shutdown',
    epoch,
  })
  const terminate = Effect.fn('ProjectWorker.terminate')(function* () {
    if (stopped) return
    yield* Effect.tryPromise({
      try: () => worker.terminate(),
      catch: (cause) =>
        new ProjectWorkerError({
          operation: 'terminate',
          message: 'Could not terminate project worker',
          cause,
        }),
    })
    stopped = true
  })
  return Object.freeze({
    _tag: 'ProjectWorker',
    epoch,
    send,
    take: receive(events),
    shutdown,
    terminate: terminate(),
    awaitExit: Deferred.await(exited),
  })
})

/** Scoped production worker-thread adapter. */
export const makeNode = (
  epoch: WorkerEpoch.WorkerEpoch,
  entry: URL = new URL('./ProjectWorkerMain.js', import.meta.url),
) =>
  Effect.acquireRelease(spawnNode(epoch, entry), (worker) => worker.terminate.pipe(Effect.ignore))

export const nodeFactory: Factory = Object.freeze({ spawn: makeNode })
