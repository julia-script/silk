import * as Deferred from 'effect/Deferred'
import type * as Duration from 'effect/Duration'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as Queue from 'effect/Queue'
import * as Result from 'effect/Result'
import * as Scope from 'effect/Scope'
import * as Document from './Document.js'
import * as DocumentVersion from './DocumentVersion.js'
import type * as EditorQuery from './EditorQuery.js'
import * as GenerationState from './GenerationState.js'
import * as IncidentId from './IncidentId.js'
import * as ProjectGeneration from './ProjectGeneration.js'
import type * as ProjectWorker from './ProjectWorker.js'
import * as QueryOutcome from './QueryOutcome.js'
import * as RequestId from './RequestId.js'
import type * as SourceEvent from './SourceEvent.js'
import * as SourceLedger from './SourceLedger.js'
import * as WorkerEpoch from './WorkerEpoch.js'
import * as WorkerProtocol from './WorkerProtocol.js'

export interface Policy {
  readonly debounce: Duration.Input
  readonly queryDeadline: Duration.Input
  readonly diagnosticDeadline: Duration.Input
  readonly supersededLease: Duration.Input
  readonly noProgressLease: Duration.Input
  /** Bounds cold worker loading and the Ready handshake independently of teardown. */
  readonly startupDeadline: Duration.Input
  readonly retirementDeadline: Duration.Input
  readonly failureLimit: number
}

export const defaultPolicy: Policy = Object.freeze({
  debounce: 25,
  queryDeadline: 2_000,
  diagnosticDeadline: 5_000,
  supersededLease: 500,
  noProgressLease: 10_000,
  startupDeadline: 10_000,
  retirementDeadline: 2_000,
  failureLimit: 3,
})

export type Event =
  | {
      readonly _tag: 'Committed'
      readonly workspace: string
      readonly generation: ProjectGeneration.ProjectGeneration
      readonly uris: ReadonlyArray<string>
      readonly documents: ReadonlyArray<AcceptanceBarrierEntry>
      readonly refreshDiagnostics: boolean
    }
  | {
      readonly _tag: 'Failed'
      readonly workspace: string
      readonly generation: ProjectGeneration.ProjectGeneration
      readonly incident: IncidentId.IncidentId
    }
  | {
      readonly _tag: 'WorkerRetired'
      readonly workspace: string
      readonly epoch: WorkerEpoch.WorkerEpoch
      readonly reason: RetirementReason
    }

type RetirementReason = 'Failure' | 'NoProgress' | 'Superseded' | 'Shutdown'

export interface AcceptanceBarrierEntry {
  readonly uri: string
  readonly version: number
}

export interface WorkspaceEngine {
  readonly _tag: 'WorkspaceEngine'
  readonly accept: (
    event: SourceEvent.SourceEvent,
  ) => Result.Result<SourceLedger.Acceptance, SourceLedger.ProtocolViolation>
  readonly request: <K extends EditorQuery.Tag>(
    query: EditorQuery.EditorQuery<K>,
  ) => Effect.Effect<QueryOutcome.QueryOutcome<EditorQuery.Result<K>>>
  readonly events: Queue.Dequeue<Event>
  readonly accepted: (entries: ReadonlyArray<AcceptanceBarrierEntry>) => boolean
  readonly shutdown: Effect.Effect<void>
}

export interface Options<R> {
  readonly discover: (entry: SourceLedger.Entry) => Effect.Effect<Document.Document, never, R>
  readonly workerFactory: ProjectWorker.Factory
  readonly policy?: Partial<Policy>
}

interface Association {
  readonly workspace: string
  readonly sourceRoot: string
  readonly module: string
  readonly configuration?: unknown
}

interface Waiter {
  readonly requestId: RequestId.RequestId
  readonly uri: string
  readonly version: DocumentVersion.DocumentVersion
  readonly generation: ProjectGeneration.ProjectGeneration
  readonly deferred: Deferred.Deferred<WaiterSignal>
}

type WaiterSignal =
  | { readonly _tag: 'Committed' }
  | QueryOutcome.Superseded
  | QueryOutcome.Canceled
  | QueryOutcome.DeadlineExceeded
  | QueryOutcome.AnalysisFailed
  | QueryOutcome.Unavailable
  | QueryOutcome.Closed

interface WorkerQuery {
  readonly uri: string
  readonly generation: ProjectGeneration.ProjectGeneration
  readonly epoch: WorkerEpoch.WorkerEpoch
  readonly deferred: Deferred.Deferred<QueryOutcome.QueryOutcome<unknown>>
}

interface WorkerSlot {
  readonly worker: ProjectWorker.ProjectWorker
  readonly scope: Scope.Closeable
  readonly ready: Deferred.Deferred<void, ProjectWorker.ProjectWorkerError>
}

interface ProjectContext {
  readonly workspace: string
  sourceRoot: string
  readonly documents: Map<string, Document.Document>
  generation: ProjectGeneration.ProjectGeneration
  state: GenerationState.GenerationState
  pending: boolean
  activeGeneration: ProjectGeneration.ProjectGeneration | undefined
  supersedeRequested: ProjectGeneration.ProjectGeneration | undefined
  awaitingSupersede: ProjectGeneration.ProjectGeneration | undefined
  workerEpoch: WorkerEpoch.WorkerEpoch
  worker: WorkerSlot | undefined
  watchdog: Fiber.Fiber<void> | undefined
  supersedeWatchdog: Fiber.Fiber<void> | undefined
  progressToken: number
  readonly waiters: Map<number, Waiter>
  readonly queries: Map<number, WorkerQuery>
  readonly dirtyPaths: Set<string>
  rediscover: boolean
  failureCount: number
  circuit: 'Closed' | 'Open' | 'HalfOpen'
  refreshDiagnostics: boolean
}

const outcomeRecord = (
  input: unknown,
): input is { readonly _tag: string; readonly value?: unknown } =>
  typeof input === 'object' && input !== null && '_tag' in input && typeof input._tag === 'string'

const workerOutcome = (input: unknown): QueryOutcome.QueryOutcome<unknown> =>
  outcomeRecord(input) && input._tag === 'Ready'
    ? QueryOutcome.ready(input.value)
    : QueryOutcome.unavailable

const requestKey = (id: RequestId.RequestId): number => id.value

const stateOf = (project: ProjectContext): GenerationState.GenerationState => project.state

/** Creates the single owner of source truth, project generations, workers, and query settlement. */
export const make = Effect.fn('WorkspaceEngine.make')(function* <R>(
  options: Options<R>,
): Effect.fn.Return<WorkspaceEngine, never, R> {
  const policy: Policy = Object.freeze({ ...defaultPolicy, ...options.policy })
  const ledger = SourceLedger.make()
  const signal = yield* Queue.sliding<void>(1)
  const events = yield* Queue.sliding<Event>(256)
  const scope = yield* Scope.make()
  const associations = new Map<string, Association>()
  const projects = new Map<string, ProjectContext>()
  const discovery = new Set<string>()
  const discoveryWaiters = new Map<string, Map<number, Waiter>>()
  const retirements = new Set<ProjectContext>()
  let nextRequest = RequestId.initial
  let nextIncident = IncidentId.initial
  let closed = false

  const complete = (waiter: Waiter, value: WaiterSignal): void => {
    Deferred.doneUnsafe(waiter.deferred, Effect.succeed(value))
  }

  const settleProject = (project: ProjectContext, value: WaiterSignal): void => {
    for (const [key, waiter] of project.waiters) {
      if (waiter.generation.value !== project.generation.value && value._tag === 'Committed')
        continue
      project.waiters.delete(key)
      complete(waiter, value)
    }
  }

  const settleUri = (project: ProjectContext, uri: string, value: WaiterSignal): void => {
    for (const [key, waiter] of project.waiters) {
      if (waiter.uri !== uri) continue
      project.waiters.delete(key)
      complete(waiter, value)
    }
  }

  const settleDiscovery = (uri: string, value: WaiterSignal): void => {
    const waiters = discoveryWaiters.get(uri)
    if (waiters === undefined) return
    discoveryWaiters.delete(uri)
    for (const waiter of waiters.values()) complete(waiter, value)
  }

  const cancelQueries = (
    project: ProjectContext,
    value: QueryOutcome.QueryOutcome<unknown>,
  ): void => {
    for (const [key, query] of project.queries) {
      project.queries.delete(key)
      Deferred.doneUnsafe(query.deferred, Effect.succeed(value))
    }
  }

  const scheduleProject = (project: ProjectContext): void => {
    if (project.state._tag === 'Closed') return
    if (project.circuit === 'Open') project.circuit = 'HalfOpen'
    settleProject(project, QueryOutcome.superseded)
    cancelQueries(project, QueryOutcome.superseded)
    if (project.activeGeneration !== undefined)
      project.supersedeRequested = project.activeGeneration
    project.generation = ProjectGeneration.next(project.generation)
    project.state = GenerationState.pending(project.generation)
    project.pending = true
  }

  const projectOf = (association: Association): ProjectContext => {
    const current = projects.get(association.workspace)
    if (current !== undefined) {
      current.sourceRoot = association.sourceRoot
      return current
    }
    const generation = ProjectGeneration.initial
    const created: ProjectContext = {
      workspace: association.workspace,
      sourceRoot: association.sourceRoot,
      documents: new Map(),
      generation,
      state: GenerationState.pending(generation),
      pending: false,
      activeGeneration: undefined,
      supersedeRequested: undefined,
      awaitingSupersede: undefined,
      workerEpoch: WorkerEpoch.initial,
      worker: undefined,
      watchdog: undefined,
      supersedeWatchdog: undefined,
      progressToken: 0,
      waiters: new Map(),
      queries: new Map(),
      dirtyPaths: new Set(),
      rediscover: false,
      failureCount: 0,
      circuit: 'Closed',
      refreshDiagnostics: false,
    }
    projects.set(association.workspace, created)
    return created
  }

  const detach = (uri: string, value: WaiterSignal): void => {
    const association = associations.get(uri)
    if (association === undefined) return
    associations.delete(uri)
    const project = projects.get(association.workspace)
    if (project === undefined) return
    project.documents.delete(uri)
    settleUri(project, uri, value)
    if (project.documents.size === 0) {
      project.state = GenerationState.closed(project.generation)
      settleProject(project, QueryOutcome.closed)
      cancelQueries(project, QueryOutcome.closed)
      projects.delete(project.workspace)
      retirements.add(project)
    } else scheduleProject(project)
  }

  let discoveryRevision = 0
  const applyAccepted = (acceptance: SourceLedger.Acceptance): void => {
    if (acceptance._tag === 'Ignored') return
    const event = acceptance.event
    switch (event._tag) {
      case 'Open':
        discovery.add(event.uri)
        settleDiscovery(event.uri, QueryOutcome.superseded)
        break
      case 'Change': {
        settleDiscovery(event.uri, QueryOutcome.superseded)
        const association = associations.get(event.uri)
        const project = association === undefined ? undefined : projects.get(association.workspace)
        if (association === undefined || project === undefined) {
          discovery.add(event.uri)
          break
        }
        project.documents.set(
          event.uri,
          Document.make({
            uri: event.uri,
            version: event.version.value,
            bytes: event.bytes,
            workspace: association.workspace,
            sourceRoot: association.sourceRoot,
            module: association.module,
            configuration: association.configuration,
          }),
        )
        if (project.documents.size > 1) project.refreshDiagnostics = true
        scheduleProject(project)
        break
      }
      case 'Close':
        discovery.delete(event.uri)
        settleDiscovery(event.uri, QueryOutcome.closed)
        detach(event.uri, QueryOutcome.closed)
        break
      case 'Invalidate':
        if (event.rediscover) discoveryRevision += 1
        for (const project of projects.values()) {
          for (const path of event.paths)
            if (path.startsWith(project.sourceRoot)) project.dirtyPaths.add(path)
          if (event.rediscover) project.rediscover = true
          if (project.dirtyPaths.size > 0 || event.rediscover) {
            project.refreshDiagnostics = true
            scheduleProject(project)
          }
        }
        if (event.rediscover)
          for (const entry of SourceLedger.entries(ledger)) discovery.add(entry.uri)
        break
    }
  }

  const accept = (
    event: SourceEvent.SourceEvent,
  ): Result.Result<SourceLedger.Acceptance, SourceLedger.ProtocolViolation> => {
    if (closed)
      return Result.fail(
        new SourceLedger.ProtocolViolation({
          uri: 'uri' in event ? event.uri : '',
          message: 'Workspace engine is closed',
        }),
      )
    const accepted = SourceLedger.accept(ledger, event)
    if (Result.isFailure(accepted)) return accepted
    applyAccepted(accepted.success)
    Queue.offerUnsafe(signal, undefined)
    return accepted
  }

  const retire = Effect.fnUntraced(function* (project: ProjectContext, reason: RetirementReason) {
    const slot = project.worker
    project.worker = undefined
    project.activeGeneration = undefined
    project.awaitingSupersede = undefined
    if (project.watchdog !== undefined) {
      yield* Fiber.interrupt(project.watchdog)
      project.watchdog = undefined
    }
    if (project.supersedeWatchdog !== undefined) {
      yield* Fiber.interrupt(project.supersedeWatchdog)
      project.supersedeWatchdog = undefined
    }
    if (slot === undefined) return
    const graceful = yield* Effect.race(
      slot.worker.shutdown.pipe(
        Effect.ignore,
        Effect.andThen(slot.worker.awaitExit),
        Effect.as(true),
      ),
      Effect.sleep(policy.retirementDeadline).pipe(Effect.as(false)),
    )
    if (!graceful) {
      yield* slot.worker.terminate.pipe(Effect.ignore)
      yield* slot.worker.awaitExit.pipe(Effect.ignore)
    }
    yield* Scope.close(slot.scope, Exit.succeed(undefined))
    Queue.offerUnsafe(events, {
      _tag: 'WorkerRetired',
      workspace: project.workspace,
      epoch: slot.worker.epoch,
      reason,
    })
  })

  const failProject = Effect.fnUntraced(function* (
    project: ProjectContext,
    incident: IncidentId.IncidentId,
    reason: 'Failure' | 'NoProgress',
  ) {
    const failedGeneration = project.generation
    project.state = GenerationState.failed(failedGeneration, incident)
    settleProject(project, QueryOutcome.analysisFailed(incident))
    cancelQueries(project, QueryOutcome.analysisFailed(incident))
    Queue.offerUnsafe(events, {
      _tag: 'Failed',
      workspace: project.workspace,
      generation: failedGeneration,
      incident,
    })
    const halfOpen = project.circuit === 'HalfOpen'
    project.failureCount += 1
    yield* retire(project, reason)
    if (closed || project.documents.size === 0) return
    if (halfOpen || project.failureCount >= policy.failureLimit) {
      project.circuit = 'Open'
      return
    }
    project.generation = ProjectGeneration.next(project.generation)
    project.state = GenerationState.pending(project.generation)
    project.pending = true
    Queue.offerUnsafe(signal, undefined)
  })

  const resetWatchdog = Effect.fnUntraced(function* (project: ProjectContext) {
    project.progressToken += 1
    const token = project.progressToken
    if (project.watchdog !== undefined) yield* Fiber.interrupt(project.watchdog)
    project.watchdog = yield* Effect.forkIn(
      Effect.sleep(policy.noProgressLease).pipe(
        Effect.flatMap(() => {
          if (closed || project.progressToken !== token || project.activeGeneration === undefined)
            return Effect.void
          project.watchdog = undefined
          nextIncident = IncidentId.next(nextIncident)
          return failProject(project, nextIncident, 'NoProgress')
        }),
      ),
      scope,
    )
  })

  const handleWorkerMessage = Effect.fnUntraced(function* (
    project: ProjectContext,
    epoch: WorkerEpoch.WorkerEpoch,
    message: WorkerProtocol.WorkerMessage,
  ) {
    if (project.worker?.worker.epoch.value !== epoch.value || message.epoch.value !== epoch.value)
      return
    switch (message._tag) {
      case 'Ready':
        yield* Deferred.succeed(project.worker.ready, undefined)
        return
      case 'Progress':
        if (project.activeGeneration?.value === message.generation.value)
          yield* resetWatchdog(project)
        return
      case 'Superseded':
        if (project.awaitingSupersede?.value !== message.generation.value) return
        project.awaitingSupersede = undefined
        project.activeGeneration = undefined
        if (project.supersedeWatchdog !== undefined) {
          yield* Fiber.interrupt(project.supersedeWatchdog)
          project.supersedeWatchdog = undefined
        }
        Queue.offerUnsafe(signal, undefined)
        return
      case 'Commit':
        if (
          project.generation.value !== message.generation.value ||
          project.activeGeneration?.value !== message.generation.value
        )
          return
        project.activeGeneration = undefined
        project.state = GenerationState.committed(message.generation, epoch, message.uris)
        project.failureCount = 0
        project.circuit = 'Closed'
        settleProject(project, { _tag: 'Committed' })
        Queue.offerUnsafe(events, {
          _tag: 'Committed',
          workspace: project.workspace,
          generation: message.generation,
          uris: message.uris,
          documents: message.uris.flatMap((uri) => {
            const document = project.documents.get(uri)
            return document === undefined ? [] : [{ uri, version: document.version }]
          }),
          refreshDiagnostics: project.refreshDiagnostics,
        })
        project.refreshDiagnostics = false
        return
      case 'Failure':
        if (project.generation.value !== message.generation.value) return
        yield* failProject(project, message.incident, 'Failure')
        return
      case 'Result': {
        const key = requestKey(message.requestId)
        const query = project.queries.get(key)
        if (
          query === undefined ||
          query.generation.value !== message.generation.value ||
          query.epoch.value !== epoch.value
        )
          return
        project.queries.delete(key)
        yield* Deferred.succeed(query.deferred, workerOutcome(message.result))
        return
      }
      case 'Stopped':
        return
    }
  })

  const listen = Effect.fnUntraced(function* (project: ProjectContext, slot: WorkerSlot) {
    while (!closed && project.worker === slot) {
      const taken = yield* Effect.result(slot.worker.take)
      if (Result.isFailure(taken)) {
        yield* Deferred.fail(slot.ready, taken.failure)
        nextIncident = IncidentId.next(nextIncident)
        yield* failProject(project, nextIncident, 'Failure')
        return
      }
      yield* handleWorkerMessage(project, slot.worker.epoch, taken.success)
      if (taken.success._tag === 'Stopped') return
    }
  })

  const ensureWorker = Effect.fnUntraced(function* (
    project: ProjectContext,
  ): Effect.fn.Return<ProjectWorker.ProjectWorker | undefined> {
    if (project.worker !== undefined) return project.worker.worker
    if (project.circuit === 'Open') return undefined
    project.workerEpoch = WorkerEpoch.next(project.workerEpoch)
    const workerScope = yield* Scope.make()
    const spawned = yield* Effect.result(
      options.workerFactory
        .spawn(project.workerEpoch)
        .pipe(Effect.provideService(Scope.Scope, workerScope)),
    )
    if (Result.isFailure(spawned)) {
      yield* Scope.close(workerScope, Exit.succeed(undefined))
      nextIncident = IncidentId.next(nextIncident)
      yield* failProject(project, nextIncident, 'Failure')
      return undefined
    }
    const ready = Deferred.makeUnsafe<void, ProjectWorker.ProjectWorkerError>()
    const slot: WorkerSlot = { worker: spawned.success, scope: workerScope, ready }
    project.worker = slot
    yield* Effect.forkIn(listen(project, slot), scope)
    const initialized = yield* Effect.result(
      slot.worker.send({
        protocolVersion: WorkerProtocol.version,
        _tag: 'Initialize',
        epoch: slot.worker.epoch,
        workspace: project.workspace,
        sourceRoot: project.sourceRoot,
      }),
    )
    if (Result.isFailure(initialized)) {
      nextIncident = IncidentId.next(nextIncident)
      yield* failProject(project, nextIncident, 'Failure')
      return undefined
    }
    const readiness = yield* Effect.result(
      Deferred.await(ready).pipe(Effect.timeout(policy.startupDeadline)),
    )
    if (Result.isFailure(readiness)) {
      nextIncident = IncidentId.next(nextIncident)
      yield* failProject(project, nextIncident, 'Failure')
      return undefined
    }
    return slot.worker
  })

  const processRetirements = Effect.fnUntraced(function* () {
    const pending = [...retirements]
    retirements.clear()
    yield* Effect.forEach(pending, (project) => retire(project, 'Shutdown'), {
      concurrency: 'unbounded',
      discard: true,
    })
  })

  const processSupersedes = Effect.fnUntraced(function* () {
    for (const project of projects.values()) {
      const generation = project.supersedeRequested
      const worker = project.worker?.worker
      if (generation === undefined || worker === undefined) continue
      project.supersedeRequested = undefined
      project.awaitingSupersede = generation
      const sent = yield* Effect.result(
        worker.send({
          protocolVersion: WorkerProtocol.version,
          _tag: 'SupersedeAnalysis',
          epoch: worker.epoch,
          generation,
        }),
      )
      if (Result.isFailure(sent)) {
        yield* retire(project, 'Superseded')
        Queue.offerUnsafe(signal, undefined)
        continue
      }
      project.supersedeWatchdog = yield* Effect.forkIn(
        Effect.sleep(policy.supersededLease).pipe(
          Effect.flatMap(() => {
            if (project.awaitingSupersede?.value !== generation.value) return Effect.void
            project.supersedeWatchdog = undefined
            return retire(project, 'Superseded').pipe(
              Effect.andThen(
                Effect.sync(() => {
                  project.awaitingSupersede = undefined
                  Queue.offerUnsafe(signal, undefined)
                }),
              ),
            )
          }),
        ),
        scope,
      )
    }
  })

  const processDiscovery = Effect.fnUntraced(function* () {
    const revision = discoveryRevision
    const requested = [...discovery]
    discovery.clear()
    const discovered: Array<Document.Document> = []
    for (const uri of requested) {
      const entry = SourceLedger.get(ledger, uri)
      if (entry === undefined) continue
      const document = yield* options.discover(entry)
      const current = SourceLedger.get(ledger, uri)
      if (current?.version.value !== entry.version.value || revision !== discoveryRevision) {
        if (current !== undefined) discovery.add(uri)
        continue
      }
      discovered.push(document)
    }
    if (revision !== discoveryRevision) {
      for (const document of discovered) discovery.add(document.uri)
      return
    }
    const touched = new Set<ProjectContext>()
    for (const document of discovered) {
      const prior = associations.get(document.uri)
      const association: Association = Object.freeze({
        workspace: document.workspace,
        sourceRoot: document.sourceRoot,
        module: document.module,
        configuration: document.configuration,
      })
      const reassigned =
        prior !== undefined &&
        (prior.workspace !== association.workspace ||
          prior.sourceRoot !== association.sourceRoot ||
          prior.module !== association.module)
      if (reassigned) detach(document.uri, QueryOutcome.superseded)
      associations.set(document.uri, association)
      const project = projectOf(association)
      if (reassigned) project.refreshDiagnostics = true
      if (project.documents.size > 0 && !project.documents.has(document.uri))
        project.refreshDiagnostics = true
      project.documents.set(document.uri, document)
      project.rediscover = false
      touched.add(project)
      settleDiscovery(document.uri, QueryOutcome.superseded)
    }
    for (const project of touched) scheduleProject(project)
  })

  const startPending = Effect.fnUntraced(function* () {
    for (const project of projects.values()) {
      if (
        !project.pending ||
        project.activeGeneration !== undefined ||
        project.awaitingSupersede !== undefined ||
        project.circuit === 'Open'
      )
        continue
      const worker = yield* ensureWorker(project)
      if (worker === undefined || project.state._tag !== 'Pending') continue
      const generation = project.generation
      project.pending = false
      project.activeGeneration = generation
      const message: WorkerProtocol.HostMessage = {
        protocolVersion: WorkerProtocol.version,
        _tag: 'Analyze',
        epoch: worker.epoch,
        generation,
        sources: [...project.documents.values()].map((document) => ({
          uri: document.uri,
          version: DocumentVersion.make(document.version),
          workspace: document.workspace,
          sourceRoot: document.sourceRoot,
          module: document.module,
          bytes: document.bytes,
          configuration: document.configuration,
        })),
        invalidation: {
          dirtyPaths: [...project.dirtyPaths].sort(),
          rediscover: project.rediscover,
        },
      }
      project.dirtyPaths.clear()
      project.rediscover = false
      const sent = yield* Effect.result(worker.send(message))
      if (Result.isFailure(sent)) {
        nextIncident = IncidentId.next(nextIncident)
        yield* failProject(project, nextIncident, 'Failure')
        continue
      }
      yield* resetWatchdog(project)
    }
  })

  const awaitDebounce = Effect.fnUntraced(function* () {
    while (!closed) {
      const outcome = yield* Effect.race(
        Effect.sleep(policy.debounce).pipe(Effect.as('Debounced' as const)),
        Queue.take(signal).pipe(Effect.as('Signaled' as const)),
      )
      if (outcome === 'Debounced') return
      yield* processRetirements()
      yield* processSupersedes()
      yield* processDiscovery()
    }
  })

  const supervisor = Effect.fnUntraced(function* () {
    while (!closed) {
      yield* Queue.take(signal)
      yield* processRetirements()
      yield* processSupersedes()
      yield* processDiscovery()
      yield* awaitDebounce()
      if (!closed) yield* startPending()
    }
  })
  yield* Effect.forkIn(supervisor(), scope, { startImmediately: true })

  const registerWaiter = Effect.fnUntraced(function* (
    uri: string,
    version: DocumentVersion.DocumentVersion,
    project: ProjectContext | undefined,
  ) {
    nextRequest = RequestId.next(nextRequest)
    const deferred = Deferred.makeUnsafe<WaiterSignal>()
    const waiter: Waiter = {
      requestId: nextRequest,
      uri,
      version,
      generation: project?.generation ?? ProjectGeneration.initial,
      deferred,
    }
    const waiters =
      project === undefined
        ? (discoveryWaiters.get(uri) ?? new Map<number, Waiter>())
        : project.waiters
    if (project === undefined) discoveryWaiters.set(uri, waiters)
    waiters.set(requestKey(waiter.requestId), waiter)
    return yield* Deferred.await(deferred).pipe(
      Effect.ensuring(
        Effect.sync(() => {
          waiters.delete(requestKey(waiter.requestId))
          if (project === undefined && waiters.size === 0) discoveryWaiters.delete(uri)
        }),
      ),
    )
  })

  const dispatch = Effect.fnUntraced(function* <K extends EditorQuery.Tag>(
    project: ProjectContext,
    query: EditorQuery.EditorQuery<K>,
  ): Effect.fn.Return<QueryOutcome.QueryOutcome<EditorQuery.Result<K>>> {
    const slot = project.worker
    if (slot === undefined || project.state._tag !== 'Committed') return QueryOutcome.unavailable
    nextRequest = RequestId.next(nextRequest)
    const requestId = nextRequest
    const generation = project.state.generation
    const epoch = project.state.epoch
    const deferred = Deferred.makeUnsafe<QueryOutcome.QueryOutcome<unknown>>()
    const key = requestKey(requestId)
    project.queries.set(key, { uri: query.uri, generation, epoch, deferred })
    const sent = yield* Effect.result(
      slot.worker.send({
        protocolVersion: WorkerProtocol.version,
        _tag: 'Query',
        epoch,
        generation,
        requestId,
        query,
      }),
    )
    if (Result.isFailure(sent)) {
      project.queries.delete(key)
      return QueryOutcome.unavailable
    }
    const outcome = yield* Deferred.await(deferred).pipe(
      Effect.ensuring(
        Effect.gen(function* () {
          if (!project.queries.delete(key)) return
          yield* slot.worker
            .send({
              protocolVersion: WorkerProtocol.version,
              _tag: 'CancelQuery',
              epoch,
              requestId,
            })
            .pipe(Effect.ignore)
        }),
      ),
    )
    // The request id records the query tag at this boundary; TypeScript cannot express that map
    // correlation after structured clone, so the single cast is confined to the protocol seam.
    return outcome as QueryOutcome.QueryOutcome<EditorQuery.Result<K>>
  })

  const requestCurrent = Effect.fnUntraced(function* <K extends EditorQuery.Tag>(
    query: EditorQuery.EditorQuery<K>,
  ): Effect.fn.Return<QueryOutcome.QueryOutcome<EditorQuery.Result<K>>> {
    const entry = SourceLedger.get(ledger, query.uri)
    if (closed || entry === undefined) return QueryOutcome.closed
    const association = associations.get(query.uri)
    const project = association === undefined ? undefined : projects.get(association.workspace)
    if (project === undefined) {
      const signal = yield* registerWaiter(query.uri, entry.version, undefined)
      return signal._tag === 'Committed' ? QueryOutcome.unavailable : signal
    }
    switch (project.state._tag) {
      case 'Committed':
        return project.state.uris.has(query.uri)
          ? yield* dispatch(project, query)
          : QueryOutcome.unavailable
      case 'Failed':
        return QueryOutcome.analysisFailed(project.state.incident)
      case 'Superseded':
        return QueryOutcome.superseded
      case 'Closed':
        return QueryOutcome.closed
      case 'Pending': {
        const waiting = yield* registerWaiter(query.uri, entry.version, project)
        if (waiting._tag !== 'Committed') return waiting
        const current = SourceLedger.get(ledger, query.uri)
        const state = stateOf(project)
        if (
          current?.version.value !== entry.version.value ||
          state._tag !== 'Committed' ||
          !state.uris.has(query.uri)
        )
          return QueryOutcome.superseded
        return yield* dispatch(project, query)
      }
    }
  })

  const request = <K extends EditorQuery.Tag>(query: EditorQuery.EditorQuery<K>) =>
    Effect.race(
      requestCurrent(query),
      Effect.sleep(
        query._tag === 'Diagnostics' ? policy.diagnosticDeadline : policy.queryDeadline,
      ).pipe(Effect.as(QueryOutcome.deadlineExceeded)),
    )

  const accepted = (expected: ReadonlyArray<AcceptanceBarrierEntry>): boolean =>
    expected.every((item) => SourceLedger.get(ledger, item.uri)?.version.value === item.version)

  const shutdown = Effect.fn('WorkspaceEngine.shutdown')(function* () {
    if (closed) return
    closed = true
    SourceLedger.clear(ledger)
    for (const [uri] of discoveryWaiters) settleDiscovery(uri, QueryOutcome.closed)
    for (const project of projects.values()) {
      project.state = GenerationState.closed(project.generation)
      settleProject(project, QueryOutcome.closed)
      cancelQueries(project, QueryOutcome.closed)
    }
    yield* Effect.forEach(projects.values(), (project) => retire(project, 'Shutdown'), {
      concurrency: 'unbounded',
      discard: true,
    })
    projects.clear()
    associations.clear()
    yield* Scope.close(scope, Exit.succeed(undefined))
    yield* Queue.shutdown(events)
  })

  return Object.freeze({
    _tag: 'WorkspaceEngine',
    accept,
    request,
    events,
    accepted,
    shutdown: shutdown(),
  })
})
