import type * as Analysis from '@silk-effect/compiler/Analysis'
import * as Deferred from 'effect/Deferred'
import * as Duration from 'effect/Duration'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import type * as Document from './Document.js'

/** One coherent document and analysis snapshot committed at a project revision. */
export interface AnalyzedDocument {
  readonly document: Document.Document
  readonly snapshot: Analysis.Snapshot
  readonly moduleUris: ReadonlyMap<string, string>
}

export interface Options<R> {
  readonly workspace: string
  readonly sourceRoot: string
  readonly debounce?: Duration.Input
  readonly analyze: (
    document: Document.Document,
    documents: ReadonlyArray<Document.Document>,
  ) => Effect.Effect<AnalyzedDocument, never, R>
  readonly publish: (session: AnalyzedDocument) => Effect.Effect<void, never, R>
}

interface Waiter {
  readonly uri: string
  readonly version: number
  readonly deferred: Deferred.Deferred<Option.Option<AnalyzedDocument>>
}

interface Pending {
  readonly revision: number
  readonly priority?: string
  readonly documents: ReadonlyArray<Document.Document>
}

/** One latest-wins scheduler and atomic commit boundary for a discovered project. */
export interface ProjectSession<R> {
  readonly _tag: 'ProjectSession'
  readonly workspace: string
  readonly sourceRoot: string
  readonly documents: () => ReadonlyArray<Document.Document>
  readonly open: (document: Document.Document) => Effect.Effect<void, never, R>
  readonly close: (uri: string) => Effect.Effect<void, never, R>
  readonly invalidate: (priority?: string) => Effect.Effect<void, never, R>
  readonly acquire: (uri: string, version: number) => Effect.Effect<Option.Option<AnalyzedDocument>>
  readonly shutdown: () => Effect.Effect<void>
}

/** Creates an isolated project scheduler. Mutable state remains private to this actor. */
export const make = <R>(options: Options<R>): ProjectSession<R> => {
  const synchronized = new Map<string, Document.Document>()
  let revision = 0
  let committedRevision = -1
  let committed = new Map<string, AnalyzedDocument>()
  let pending: Pending | undefined
  let active = false
  let closed = false
  let waiters: Array<Waiter> = []
  let idle: Deferred.Deferred<void> | undefined
  const debounce = options.debounce ?? Duration.millis(25)

  const freezePending = (priority?: string): Pending =>
    Object.freeze({
      revision,
      ...(priority === undefined ? {} : { priority }),
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
      work.priority === undefined
        ? work.documents
        : Object.freeze([
            ...work.documents.filter((document) => document.uri === work.priority),
            ...work.documents.filter((document) => document.uri !== work.priority),
          ])
    const analyzed = new Map<string, AnalyzedDocument>()
    for (const document of ordered) {
      analyzed.set(document.uri, yield* options.analyze(document, work.documents))
    }
    if (closed || work.revision !== revision) return
    committed = analyzed
    committedRevision = work.revision
    for (const session of analyzed.values()) yield* options.publish(session)
    yield* completeWaiters((waiter) => {
      const document = synchronized.get(waiter.uri)
      if (document === undefined || document.version !== waiter.version) return Option.none()
      const session = analyzed.get(waiter.uri)
      return session?.document.version === waiter.version ? Option.some(session) : Option.none()
    })
  })

  const worker = Effect.fnUntraced(function* () {
    while (!closed) {
      yield* Effect.sleep(debounce)
      const work = pending
      pending = undefined
      if (work === undefined) break
      yield* analyzePending(work)
      if (pending === undefined) break
    }
    active = false
    const completedIdle = idle
    idle = undefined
    if (completedIdle !== undefined) yield* Deferred.succeed(completedIdle, undefined)
  })

  const schedule = Effect.fnUntraced(function* (priority?: string) {
    if (closed) return
    revision += 1
    pending = freezePending(priority)
    if (active) return
    active = true
    yield* worker()
  })

  const open = Effect.fn('ProjectSession.open')(function* (document: Document.Document) {
    const previous = synchronized.get(document.uri)
    synchronized.set(document.uri, document)
    if (previous !== undefined && previous.version !== document.version) {
      yield* completeWaiters((waiter) =>
        waiter.uri === document.uri && waiter.version !== document.version
          ? Option.none()
          : undefined,
      )
    }
    yield* schedule(document.uri)
  })

  const close = Effect.fn('ProjectSession.close')(function* (uri: string) {
    if (!synchronized.delete(uri)) return
    committed.delete(uri)
    yield* completeWaiters((waiter) => (waiter.uri === uri ? Option.none() : undefined))
    yield* schedule()
  })

  const invalidate = Effect.fn('ProjectSession.invalidate')(function* (priority?: string) {
    yield* schedule(priority)
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
    closed = true
    revision += 1
    pending = undefined
    synchronized.clear()
    committed.clear()
    yield* completeWaiters(() => Option.none())
    if (!active) return
    idle = Deferred.makeUnsafe<void>()
    yield* Deferred.await(idle)
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
}
