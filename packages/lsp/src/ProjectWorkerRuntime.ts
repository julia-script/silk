import * as Cause from 'effect/Cause'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as Scope from 'effect/Scope'
import * as Document from './Document.js'
import type * as EditorQuery from './EditorQuery.js'
import * as IncidentId from './IncidentId.js'
import * as Inspection from './Inspection.js'
import type * as ProjectGeneration from './ProjectGeneration.js'
import type * as ProjectSnapshot from './ProjectSnapshot.js'
import type * as RequestId from './RequestId.js'
import type * as WorkerEpoch from './WorkerEpoch.js'
import * as WorkerProtocol from './WorkerProtocol.js'

export interface Options<R> {
  readonly epoch: WorkerEpoch.WorkerEpoch
  readonly analyze: (
    documents: ReadonlyArray<Document.Document>,
    previous: ReadonlyMap<string, ProjectSnapshot.DocumentSnapshot>,
    invalidation: {
      readonly priorityUri?: string
      readonly dirtyPaths: ReadonlyArray<string>
      readonly rediscover: boolean
    },
    onProgress: (phase: string) => Effect.Effect<void>,
  ) => Effect.Effect<ReadonlyMap<string, ProjectSnapshot.DocumentSnapshot>, never, R>
  readonly emit: (message: WorkerProtocol.WorkerMessage) => Effect.Effect<void>
  readonly beforeQuery?: (query: EditorQuery.EditorQuery) => Effect.Effect<void, never, R>
}

interface ActiveAnalysis {
  readonly generation: ProjectGeneration.ProjectGeneration
  readonly fiber: Fiber.Fiber<void>
}

interface ActiveQuery {
  readonly generation: ProjectGeneration.ProjectGeneration
  readonly fiber: Fiber.Fiber<void>
}

/** Worker-owned compiler state and message interpreter for one project epoch. */
export interface ProjectWorkerRuntime<R> {
  readonly _tag: 'ProjectWorkerRuntime'
  readonly accept: (message: WorkerProtocol.HostMessage) => Effect.Effect<void, never, R>
  readonly shutdown: Effect.Effect<void>
}

const requestKey = (requestId: RequestId.RequestId): number => requestId.value

const execute = Effect.fnUntraced(function* (
  session: ProjectSnapshot.DocumentSnapshot,
  query: EditorQuery.EditorQuery,
  generation: ProjectGeneration.ProjectGeneration,
  epoch: WorkerEpoch.WorkerEpoch,
): Effect.fn.Return<unknown> {
  const uriOf = (module: string): string | undefined => session.moduleUris.get(module)
  switch (query._tag) {
    case 'Diagnostics':
      return Object.freeze({
        resultId: `${epoch.value}:${generation.value}:${session.document.version}`,
        items: Document.diagnostics(session.document, session.snapshot, uriOf),
      })
    case 'Hover':
      return Document.hover(session.document, session.snapshot, query.parameters)
    case 'Completion':
      return Document.completion(
        session.document,
        session.snapshot,
        query.parameters,
        session.inventory,
      )
    case 'SignatureHelp':
      return Document.signatureHelp(session.document, session.snapshot, query.parameters)
    case 'CodeActions':
      return Document.codeActions(
        session.document,
        session.snapshot,
        query.parameters,
        uriOf,
        session.inventory,
      )
    case 'ResolveCodeAction':
      return Document.resolveCodeAction(
        session.document,
        session.snapshot,
        session.inventory,
        query.parameters,
        uriOf,
      )
    case 'Definition':
      return Document.definition(session.document, session.snapshot, query.parameters, uriOf)
    case 'References':
      return Document.references(
        session.document,
        session.snapshot,
        query.parameters.position,
        query.parameters.includeDeclaration,
        uriOf,
      )
    case 'PrepareRename':
      return Document.prepareRename(session.document, session.snapshot, query.parameters)
    case 'Rename':
      return Document.rename(
        session.document,
        session.snapshot,
        query.parameters.position,
        query.parameters.newName,
        uriOf,
      )
    case 'InlayHints':
      return Document.inlayHints(session.document, session.snapshot, query.parameters)
    case 'Symbols':
      return Document.symbols(session.document, session.snapshot)
    case 'Formatting':
      return yield* Document.format(session.document, session.snapshot)
    case 'SemanticTokens':
      return Document.semanticTokens(session.document, session.snapshot)
    case 'FoldingRanges':
      return Document.foldingRanges(session.document, session.snapshot)
    case 'PrepareCallHierarchy':
      return Document.prepareCallHierarchy(
        session.document,
        session.snapshot,
        query.parameters,
        uriOf,
      )
    case 'IncomingCalls':
      return Document.incomingCalls(session.document, session.snapshot, query.parameters, uriOf)
    case 'OutgoingCalls':
      return Document.outgoingCalls(session.document, session.snapshot, query.parameters, uriOf)
    case 'Inspection':
      return Inspection.project(session, query.parameters)
  }
})

const causeMessage = (cause: Cause.Cause<unknown>): string => Cause.pretty(cause)

type WorkerPayload<M = WorkerProtocol.WorkerMessage> = M extends WorkerProtocol.WorkerMessage
  ? Omit<M, 'protocolVersion' | 'epoch'>
  : never

const envelope = (
  epoch: WorkerEpoch.WorkerEpoch,
  message: WorkerPayload,
): WorkerProtocol.WorkerMessage => {
  switch (message._tag) {
    case 'Ready':
      return { ...message, protocolVersion: WorkerProtocol.version, epoch }
    case 'Progress':
      return { ...message, protocolVersion: WorkerProtocol.version, epoch }
    case 'Superseded':
      return { ...message, protocolVersion: WorkerProtocol.version, epoch }
    case 'Commit':
      return { ...message, protocolVersion: WorkerProtocol.version, epoch }
    case 'Failure':
      return { ...message, protocolVersion: WorkerProtocol.version, epoch }
    case 'Result':
      return { ...message, protocolVersion: WorkerProtocol.version, epoch }
    case 'Stopped':
      return { ...message, protocolVersion: WorkerProtocol.version, epoch }
  }
}

export const make = Effect.fn('ProjectWorkerRuntime.make')(function* <R>(
  options: Options<R>,
): Effect.fn.Return<ProjectWorkerRuntime<R>, never, R> {
  const scope = yield* Scope.make()
  let committedGeneration: ProjectGeneration.ProjectGeneration | undefined
  let committed = new Map<string, ProjectSnapshot.DocumentSnapshot>()
  let activeAnalysis: ActiveAnalysis | undefined
  const queries = new Map<number, ActiveQuery>()
  let nextIncident = IncidentId.initial
  let closed = false

  const emit = (message: WorkerPayload) => options.emit(envelope(options.epoch, message))

  const cancelQuery = Effect.fnUntraced(function* (requestId: RequestId.RequestId) {
    const key = requestKey(requestId)
    const active = queries.get(key)
    if (active === undefined) return
    queries.delete(key)
    yield* Fiber.interrupt(active.fiber)
  })

  const startQuery = Effect.fnUntraced(function* (
    message: Extract<WorkerProtocol.HostMessage, { readonly _tag: 'Query' }>,
  ) {
    const key = requestKey(message.requestId)
    yield* cancelQuery(message.requestId)
    const session =
      committedGeneration?.value === message.generation.value
        ? committed.get(message.query.uri)
        : undefined
    if (session === undefined) {
      yield* emit({
        _tag: 'Result',
        generation: message.generation,
        requestId: message.requestId,
        result: { _tag: 'Unavailable' },
      })
      return
    }
    const work = execute(session, message.query, message.generation, options.epoch).pipe(
      Effect.flatMap((value) =>
        emit({
          _tag: 'Result',
          generation: message.generation,
          requestId: message.requestId,
          result: { _tag: 'Ready', value },
        }),
      ),
      Effect.ensuring(
        Effect.sync(() => {
          queries.delete(key)
        }),
      ),
    )
    const queryWork =
      options.beforeQuery === undefined
        ? work
        : options.beforeQuery(message.query).pipe(Effect.andThen(work))
    const fiber = yield* Effect.forkIn(queryWork, scope)
    queries.set(key, Object.freeze({ generation: message.generation, fiber }))
  })

  const supersede = Effect.fnUntraced(function* (generation: ProjectGeneration.ProjectGeneration) {
    const active = activeAnalysis
    if (active?.generation.value === generation.value) {
      activeAnalysis = undefined
      yield* Fiber.interrupt(active.fiber)
    }
    for (const [key, query] of queries) {
      if (query.generation.value !== generation.value) continue
      queries.delete(key)
      yield* Fiber.interrupt(query.fiber)
    }
    yield* emit({ _tag: 'Superseded', generation })
  })

  const startAnalysis = Effect.fnUntraced(function* (
    message: Extract<WorkerProtocol.HostMessage, { readonly _tag: 'Analyze' }>,
  ) {
    const prior = activeAnalysis
    if (prior !== undefined) yield* supersede(prior.generation)
    const documents = message.sources.map((source) =>
      Document.make({
        uri: source.uri,
        version: source.version.value,
        workspace: source.workspace,
        sourceRoot: source.sourceRoot,
        module: source.module,
        bytes: source.bytes,
        configuration: source.configuration,
      }),
    )
    const work = Effect.gen(function* () {
      yield* emit({ _tag: 'Progress', generation: message.generation, phase: 'AnalysisStarted' })
      const result = yield* Effect.exit(
        options.analyze(
          documents,
          new Map(committed),
          message.invalidation,
          Effect.fnUntraced(function* (phase: string) {
            yield* emit({ _tag: 'Progress', generation: message.generation, phase })
          }),
        ),
      )
      if (closed || activeAnalysis?.generation.value !== message.generation.value) return
      activeAnalysis = undefined
      if (Exit.isFailure(result)) {
        nextIncident = IncidentId.next(nextIncident)
        yield* emit({
          _tag: 'Failure',
          generation: message.generation,
          incident: nextIncident,
          message: causeMessage(result.cause),
        })
        return
      }
      committed = new Map(result.value)
      committedGeneration = message.generation
      yield* emit({
        _tag: 'Commit',
        generation: message.generation,
        uris: [...committed.keys()],
      })
    })
    const fiber = yield* Effect.forkIn(work, scope)
    activeAnalysis = Object.freeze({ generation: message.generation, fiber })
  })

  const shutdown = Effect.fn('ProjectWorkerRuntime.shutdown')(function* () {
    if (closed) return
    closed = true
    activeAnalysis = undefined
    queries.clear()
    yield* Scope.close(scope, Exit.succeed(undefined))
    yield* emit({ _tag: 'Stopped' })
  })

  const accept = Effect.fn('ProjectWorkerRuntime.accept')(function* (
    message: WorkerProtocol.HostMessage,
  ) {
    if (closed || message.epoch.value !== options.epoch.value) return
    switch (message._tag) {
      case 'Initialize':
        yield* emit({ _tag: 'Ready' })
        return
      case 'Analyze':
        yield* startAnalysis(message)
        return
      case 'SupersedeAnalysis':
        yield* supersede(message.generation)
        return
      case 'Query':
        yield* startQuery(message)
        return
      case 'CancelQuery':
        yield* cancelQuery(message.requestId)
        return
      case 'Shutdown':
        yield* shutdown()
    }
  })

  return Object.freeze({ _tag: 'ProjectWorkerRuntime', accept, shutdown: shutdown() })
})
