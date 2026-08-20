import type * as ProjectAnalysis from '@silk-effect/compiler/ProjectAnalysis'
import type * as WorkspaceInventory from '@silk-effect/compiler/WorkspaceInventory'
import type * as Cause from 'effect/Cause'
import * as Deferred from 'effect/Deferred'
import * as Duration from 'effect/Duration'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as Option from 'effect/Option'
import * as Queue from 'effect/Queue'
import * as Scope from 'effect/Scope'
import type * as Document from './Document.js'

/** One coherent document and analysis snapshot committed at a project revision. */
export interface AnalyzedDocument {
  readonly document: Document.Document
  readonly project: ProjectAnalysis.ProjectAnalysis
  readonly snapshot: ProjectAnalysis.View
  readonly moduleUris: ReadonlyMap<string, string>
  readonly inventory: WorkspaceInventory.WorkspaceInventory
}

/** Document priority and filesystem hints coalesced into one project revision. */
export interface Invalidation {
  readonly priorityUri?: string
  readonly dirtyPaths?: ReadonlyArray<string>
  readonly rediscover?: boolean
}

export interface AcceptedInvalidation {
  readonly priorityUri?: string
  readonly dirtyPaths: ReadonlyArray<string>
  readonly rediscover: boolean
}

/** An analysis or publication failure for one document in the current revision. */
export interface RevisionFailure {
  readonly revision: number
  readonly document: Document.Document
  readonly cause: Cause.Cause<unknown>
}

export interface Options<R> {
  readonly workspace: string
  readonly sourceRoot: string
  readonly debounce?: Duration.Input
  readonly analyze: (
    documents: ReadonlyArray<Document.Document>,
    previous: ReadonlyMap<string, AnalyzedDocument>,
    invalidation: AcceptedInvalidation,
  ) => Effect.Effect<ReadonlyMap<string, AnalyzedDocument>, never, R>
  readonly publish: (session: AnalyzedDocument) => Effect.Effect<void, never, R>
  readonly failed?: (failure: RevisionFailure) => Effect.Effect<void, never, R>
}

interface Waiter {
  readonly uri: string
  readonly version: number
  readonly deferred: Deferred.Deferred<Option.Option<AnalyzedDocument>>
}

interface Pending {
  readonly revision: number
  readonly invalidation: AcceptedInvalidation
  readonly documents: ReadonlyArray<Document.Document>
}

/** One latest-wins scheduler and atomic commit boundary for a discovered project. */
export interface ProjectSession {
  readonly _tag: 'ProjectSession'
  readonly workspace: string
  readonly sourceRoot: string
  readonly documents: () => ReadonlyArray<Document.Document>
  readonly open: (document: Document.Document) => Effect.Effect<void>
  readonly close: (uri: string) => Effect.Effect<void>
  readonly invalidate: (invalidation?: Invalidation) => Effect.Effect<void>
  readonly acquire: (uri: string, version: number) => Effect.Effect<Option.Option<AnalyzedDocument>>
  readonly shutdown: () => Effect.Effect<void>
}

/** Creates an isolated scoped scheduler and starts its supervisor fiber. */
export const make = Effect.fn('ProjectSession.make')(function* <R>(
  options: Options<R>,
): Effect.fn.Return<ProjectSession, never, R> {
  const synchronized = new Map<string, Document.Document>()
  let revision = 0
  let committedRevision = -1
  let committed = new Map<string, AnalyzedDocument>()
  let pending: Pending | undefined
  let active: Fiber.Fiber<Exit.Exit<void>> | undefined
  let closed = false
  let waiters: Array<Waiter> = []
  const debounce = options.debounce ?? Duration.millis(25)
  const signal = yield* Queue.sliding<number>(1)
  const projectScope = yield* Scope.make()

  const emptyInvalidation = (): AcceptedInvalidation =>
    Object.freeze({ dirtyPaths: Object.freeze([]), rediscover: false })
  let accumulated = emptyInvalidation()

  const mergeInvalidation = (
    left: AcceptedInvalidation,
    right: Invalidation,
  ): AcceptedInvalidation =>
    Object.freeze({
      ...(right.priorityUri === undefined
        ? left.priorityUri === undefined
          ? {}
          : { priorityUri: left.priorityUri }
        : { priorityUri: right.priorityUri }),
      dirtyPaths: Object.freeze(
        [...new Set([...left.dirtyPaths, ...(right.dirtyPaths ?? [])])].sort(),
      ),
      rediscover: left.rediscover || right.rediscover === true,
    })

  const freezePending = (): Pending =>
    Object.freeze({
      revision,
      invalidation: accumulated,
      documents: Object.freeze([...synchronized.values()]),
    })

  const completeWaiters = Effect.fnUntraced(function* (
    select: (waiter: Waiter) => Option.Option<AnalyzedDocument> | undefined,
  ) {
    const remaining: Array<Waiter> = []
    const completed: Array<readonly [Waiter, Option.Option<AnalyzedDocument>]> = []
    for (const waiter of waiters) {
      const result = select(waiter)
      if (result === undefined) remaining.push(waiter)
      else completed.push([waiter, result])
    }
    waiters = remaining
    for (const [waiter, result] of completed) yield* Deferred.succeed(waiter.deferred, result)
  })

  const analyzePending = Effect.fnUntraced(function* (work: Pending) {
    const ordered =
      work.invalidation.priorityUri === undefined
        ? work.documents
        : Object.freeze([
            ...work.documents.filter((document) => document.uri === work.invalidation.priorityUri),
            ...work.documents.filter((document) => document.uri !== work.invalidation.priorityUri),
          ])
    const analyzed =
      ordered.length === 0
        ? new Map<string, AnalyzedDocument>()
        : yield* options.analyze(ordered, new Map(committed), work.invalidation)
    if (closed || work.revision !== revision) return
    for (const session of analyzed.values()) {
      if (closed || work.revision !== revision) return
      yield* options.publish(session)
    }
    if (closed || work.revision !== revision) return
    committed = new Map(analyzed)
    committedRevision = work.revision
    accumulated = emptyInvalidation()
    yield* completeWaiters((waiter) => {
      const document = synchronized.get(waiter.uri)
      if (document === undefined || document.version !== waiter.version) return Option.none()
      const session = committed.get(waiter.uri)
      return session?.document.version === waiter.version ? Option.some(session) : Option.none()
    })
  })

  const failCurrent = Effect.fnUntraced(function* (work: Pending, cause: Cause.Cause<unknown>) {
    if (closed || work.revision !== revision) return
    if (options.failed !== undefined) {
      for (const document of work.documents) {
        const current = synchronized.get(document.uri)
        if (current?.version !== document.version) continue
        yield* options
          .failed(Object.freeze({ revision: work.revision, document, cause }))
          .pipe(Effect.ignoreCause)
      }
    }
    yield* completeWaiters(() => Option.none())
  })

  const awaitDebounce = Effect.fnUntraced(function* () {
    while (!closed) {
      const outcome = yield* Effect.race(
        Effect.sleep(debounce).pipe(Effect.as('Debounced' as const)),
        Queue.take(signal).pipe(Effect.as('Signaled' as const)),
      )
      if (outcome === 'Debounced') return
    }
  })

  const supervisor = Effect.fnUntraced(function* () {
    while (!closed) {
      yield* Queue.take(signal)
      yield* awaitDebounce()
      if (closed) return
      const work = pending
      pending = undefined
      if (work === undefined) continue
      const fiber = yield* Effect.forkIn(Effect.exit(analyzePending(work)), projectScope, {
        startImmediately: true,
      })
      active = fiber
      const fiberExit = yield* Fiber.await(fiber)
      if (active === fiber) active = undefined
      if (closed) return
      if (Exit.isFailure(fiberExit)) {
        yield* failCurrent(work, fiberExit.cause)
        continue
      }
      const workExit = fiberExit.value
      if (Exit.isFailure(workExit)) yield* failCurrent(work, workExit.cause)
    }
  })

  yield* Effect.forkIn(supervisor(), projectScope, { startImmediately: true })

  const schedule = Effect.fnUntraced(function* (invalidation: Invalidation = {}) {
    if (closed) return
    revision += 1
    accumulated = mergeInvalidation(accumulated, invalidation)
    pending = freezePending()
    const running = active
    if (running !== undefined) {
      yield* Effect.forkIn(Fiber.interrupt(running), projectScope, { startImmediately: true })
    }
    yield* Queue.offer(signal, revision)
  })

  const open = Effect.fn('ProjectSession.open')(function* (document: Document.Document) {
    yield* Effect.uninterruptible(
      Effect.gen(function* () {
        if (closed) return
        const previous = synchronized.get(document.uri)
        synchronized.set(document.uri, document)
        if (previous !== undefined && previous.version !== document.version) {
          yield* completeWaiters((waiter) =>
            waiter.uri === document.uri && waiter.version !== document.version
              ? Option.none()
              : undefined,
          )
        }
        yield* schedule({ priorityUri: document.uri })
      }),
    )
  })

  const close = Effect.fn('ProjectSession.close')(function* (uri: string) {
    yield* Effect.uninterruptible(
      Effect.gen(function* () {
        if (closed || !synchronized.delete(uri)) return
        committed.delete(uri)
        yield* completeWaiters((waiter) => (waiter.uri === uri ? Option.none() : undefined))
        yield* schedule({ rediscover: true })
      }),
    )
  })

  const invalidate = Effect.fn('ProjectSession.invalidate')(function* (
    invalidation: Invalidation = {},
  ) {
    yield* Effect.uninterruptible(schedule(invalidation))
  })

  const acquire = Effect.fn('ProjectSession.acquire')(function* (uri: string, version: number) {
    const document = synchronized.get(uri)
    if (closed || document === undefined || document.version !== version) return Option.none()
    const session = committed.get(uri)
    if (
      committedRevision === revision &&
      session !== undefined &&
      session.document.version === version
    )
      return Option.some(session)
    const deferred = Deferred.makeUnsafe<Option.Option<AnalyzedDocument>>()
    waiters.push(Object.freeze({ uri, version, deferred }))
    return yield* Deferred.await(deferred)
  })

  const shutdown = Effect.fn('ProjectSession.shutdown')(function* () {
    yield* Effect.uninterruptible(
      Effect.gen(function* () {
        if (closed) return
        closed = true
        revision += 1
        pending = undefined
        synchronized.clear()
        committed.clear()
        yield* completeWaiters(() => Option.none())
        yield* Scope.close(projectScope, Exit.succeed(undefined))
      }),
    )
  })

  return Object.freeze({
    _tag: 'ProjectSession',
    workspace: options.workspace,
    sourceRoot: options.sourceRoot,
    documents: () => Object.freeze([...synchronized.values()]),
    open,
    close,
    invalidate,
    acquire,
    shutdown,
  })
})
