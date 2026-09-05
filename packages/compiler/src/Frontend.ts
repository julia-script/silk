import * as Effect from 'effect/Effect'
import * as BodyQuery from './BodyQuery.js'
import * as DeclarationCollection from './DeclarationCollection.js'
import * as DeclarationCompletion from './DeclarationCompletion.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as IncrementalReuse from './IncrementalReuse.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as ModuleSemantics from './ModuleSemantics.js'
import * as ModuleSurface from './ModuleSurface.js'
import * as NameResolution from './NameResolution.js'
import * as ResolutionWork from './ResolutionWork.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import * as Ownership from './Ownership.js'
import * as PhaseReport from './PhaseReport.js'
import type * as SemanticInvalidation from './SemanticInvalidation.js'
import type * as SourceResolver from './SourceResolver.js'

/** Optional environment-specific observations attached to compiler phase reports. */
export interface Options {
  readonly heapBytes?: () => number
  /** Internal differential-test escape hatch; production paths normalize shared MIR. */
  readonly normalizeMir?: boolean
}

interface FrontendFacts {
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly surfaces: ReadonlyMap<string, ModuleSurface.ModuleSurface>
  readonly semantics: ReadonlyMap<string, ModuleSemantics.ModuleSemantics>
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly ownership: ReadonlyMap<string, Ownership.ModuleOwnership>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
}

/** Immutable compiler frontend facts shared by Analysis and Driver. */
export interface Frontend extends FrontendFacts {
  readonly closure: ModuleClosure.Closure
  readonly requestedTarget?: string
  readonly configuration?: ModuleClosure.CompilationRequest['configuration']
}

/** Immutable multi-root frontend facts computed once for one project revision. */
export interface ProjectFrontend extends FrontendFacts {
  readonly closure: ModuleClosure.ProjectClosure
  readonly semanticInvalidation: SemanticInvalidation.SemanticInvalidation
}

interface HeaderFacts {
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly surfaces: ReadonlyMap<string, ModuleSurface.ModuleSurface>
}

const analyzeHeaders = Effect.fnUntraced(function* (
  closure: ModuleClosure.Facts,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
): Effect.fn.Return<HeaderFacts> {
  const collected = PhaseReport.measureInto(
    report,
    'declaration-collection',
    closure.modules.length,
    () => DeclarationCollection.collect(closure),
    (value) => value.modules.reduce((sum, module) => sum + module.members.length, 0),
    (value) => value.diagnostics.length,
    options,
  )
  yield* Effect.yieldNow
  const index = PhaseReport.measureInto(
    report,
    'declaration-index',
    collected.modules.length,
    () => {
      const preliminary = NameResolution.resolve(closure, collected)
      const resolvers = NameResolution.makeResolvers(preliminary, collected)
      const completed = DeclarationCompletion.complete(collected, resolvers)
      ResolutionWork.share(completed, collected)
      return completed
    },
    (value) => value.modules.reduce((sum, module) => sum + module.members.length, 0),
    (value) => value.diagnostics.length,
    options,
  )
  yield* Effect.yieldNow
  const resolution = PhaseReport.measureInto(
    report,
    'name-resolution',
    index.modules.length,
    () => NameResolution.resolve(closure, index),
    (value) => value.modules.reduce((sum, module) => sum + module.bindings.length, 0),
    (value) => value.diagnostics.length,
    options,
  )
  yield* Effect.yieldNow
  const surfaces = PhaseReport.measureInto(
    report,
    'module-surface',
    index.modules.length,
    () => ModuleSurface.fromIndex(index),
    (value) => value.size,
    () => 0,
    options,
  )
  return Object.freeze({ index, resolution, surfaces })
})

interface ElaboratedModules {
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly computed: ReadonlyMap<string, Elaboration.Result>
}

const elaborateModules = Effect.fnUntraced(function* (
  closure: ModuleClosure.Facts,
  headers: HeaderFacts,
  retained: ReadonlyMap<string, Elaboration.Result> = new Map(),
  precomputed: ReadonlyMap<string, Elaboration.Result> = new Map(),
  bodyQuery?: BodyQuery.BodyQuery,
): Effect.fn.Return<ElaboratedModules> {
  const results = new Map<string, Elaboration.Result>()
  const computed = new Map<string, Elaboration.Result>()
  for (const [ordinal, module] of closure.modules.entries()) {
    yield* IncrementalReuse.checkpointModuleBatch(ordinal)
    const reused = retained.get(module.name) ?? precomputed.get(module.name)
    if (reused !== undefined) {
      results.set(module.name, reused)
      continue
    }
    const moduleHeaders = headers.index.modules.find(
      (candidate) => candidate.module === module.name,
    )
    const scope = NameResolution.scopeOf(headers.resolution, module.name)
    if (moduleHeaders === undefined || scope === undefined)
      throw new RangeError(`Pipeline lost module facts for ${module.name}`)
    const result = Elaboration.elaborateModule({
      syntax: module.syntax,
      headers: moduleHeaders,
      scope,
      index: headers.index,
      ...(bodyQuery === undefined ? {} : { bodyQuery }),
    })
    const published = bodyQuery === undefined ? result : BodyQuery.publish(bodyQuery, result)
    results.set(module.name, published)
    computed.set(module.name, published)
  }
  return Object.freeze({ results, computed })
})

const analyzeSemantics = Effect.fnUntraced(function* (
  closure: ModuleClosure.Facts,
  headers: HeaderFacts,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
  reuse?: {
    readonly previous: IncrementalReuse.ProjectReuseBasis
    readonly invalidation: SemanticInvalidation.SemanticInvalidation
  },
  precomputed?: {
    readonly results: ReadonlyMap<string, Elaboration.Result>
    readonly opaqueRealizations?: OpaqueRealization.Catalog
    readonly bodyQueries?: BodyQuery.BodyQuery
  },
): Effect.fn.Return<Omit<FrontendFacts, 'resolution' | 'surfaces' | 'report'>> {
  const candidates =
    reuse === undefined
      ? new Map<string, ModuleSemantics.ModuleSemantics>()
      : yield* IncrementalReuse.retainedSemantics(closure, reuse.previous, reuse.invalidation)
  const retained =
    precomputed?.bodyQueries === undefined
      ? candidates
      : new Map(
          [...candidates].filter(
            ([name, semantics]) => precomputed.results.get(name) === semantics.elaboration,
          ),
        )
  const retainedElaborations = IncrementalReuse.retainedElaborations(retained)
  const results = yield* PhaseReport.measureEffectInto(
    report,
    'elaboration',
    closure.modules.length - retained.size,
    elaborateModules(closure, headers, retainedElaborations, precomputed?.results).pipe(
      Effect.map((elaborated) => elaborated.results),
    ),
    (value) => [...value.values()].reduce((sum, module) => sum + module.functions.length, 0),
    (value) => [...value.values()].reduce((sum, module) => sum + module.diagnostics.length, 0),
    {
      ...options,
      counters: () =>
        Object.freeze({
          _tag: 'ModuleReuseCounters' as const,
          reused: retained.size,
          recomputed: closure.modules.length - retained.size,
        }),
    },
  )
  const generatedAggregates = new Map<string, DeclarationFacts.StructFact>()
  for (const result of results.values())
    for (const aggregate of result.generatedAggregates) {
      if (aggregate.canonical._tag !== 'Canonical') continue
      generatedAggregates.set(
        `${aggregate.canonical.id.module}:${aggregate.canonical.id.name}`,
        aggregate,
      )
    }
  const index = DeclarationIndex.make(
    headers.index.stage,
    headers.index.modules,
    headers.index.diagnostics,
    generatedAggregates,
  )
  const retainedOwnership = new Set<string>()
  const ownership = yield* PhaseReport.measureEffectInto(
    report,
    'ownership',
    results.size,
    Effect.gen(function* () {
      const ownership = new Map<string, Ownership.ModuleOwnership>()
      const localSharedAccessBoundaries = Ownership.localSharedAccessBoundaryPlan(results)
      let ordinal = 0
      for (const [name, result] of results) {
        yield* IncrementalReuse.checkpointModuleBatch(ordinal)
        const checked = Ownership.checkModule(
          result,
          index,
          localSharedAccessBoundaries,
          precomputed?.bodyQueries,
        )
        const previous = retained.get(name)?.ownership
        const unchanged =
          previous !== undefined &&
          previous.functions.length === checked.functions.length &&
          previous.functions.every((fn, ordinal) => fn === checked.functions[ordinal]) &&
          previous.diagnostics.length === checked.diagnostics.length &&
          previous.diagnostics.every(
            (diagnostic, ordinal) => diagnostic === checked.diagnostics[ordinal],
          )
        if (unchanged) retainedOwnership.add(name)
        ownership.set(name, unchanged ? previous : checked)
        ordinal += 1
      }
      return ownership
    }),
    (value) => [...value.values()].reduce((sum, module) => sum + module.functions.length, 0),
    (value) => [...value.values()].reduce((sum, module) => sum + module.diagnostics.length, 0),
    {
      ...options,
      counters: () =>
        Object.freeze({
          _tag: 'ModuleReuseCounters' as const,
          reused: retainedOwnership.size,
          recomputed: results.size - retainedOwnership.size,
        }),
    },
  )
  const opaqueRealizations =
    precomputed?.opaqueRealizations ??
    PhaseReport.measureInto(
      report,
      'opaque-realization',
      results.size,
      () => OpaqueRealization.analyze(results),
      (value) => value.definitions.size,
      (value) => value.diagnostics.length,
      options,
    )
  const semantics = new Map<string, ModuleSemantics.ModuleSemantics>()
  let semanticOrdinal = 0
  for (const [module, elaboration] of results) {
    yield* IncrementalReuse.checkpointModuleBatch(semanticOrdinal)
    const reused = retained.get(module)
    if (reused !== undefined && reused.ownership === ownership.get(module))
      semantics.set(module, reused)
    else {
      const moduleOwnership = ownership.get(module)
      if (moduleOwnership === undefined)
        throw new RangeError(`Pipeline lost ownership facts for ${module}`)
      semantics.set(module, ModuleSemantics.make(module, elaboration, moduleOwnership))
    }
    semanticOrdinal += 1
  }
  const diagnostics = Diagnostic.merge(
    ...closure.modules.map((module) => module.syntax.lexicalDiagnostics),
    ...closure.modules.map((module) => module.syntax.parserDiagnostics),
    closure.diagnostics,
    headers.resolution.diagnostics,
    ...[...results.values()].map((result) => result.diagnostics),
    opaqueRealizations.diagnostics,
    ...[...ownership.values()].map((facts) => facts.diagnostics),
  )
  return OpaqueRealization.withCatalog(
    Object.freeze({
      index,
      semantics,
      results,
      ownership,
      diagnostics,
    }),
    opaqueRealizations,
  )
})

const analyzeFrontend = Effect.fnUntraced(function* (
  closure: ModuleClosure.Facts,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
): Effect.fn.Return<FrontendFacts> {
  const headers = yield* analyzeHeaders(closure, report, options)
  const semantics = yield* analyzeSemantics(closure, headers, report, options)
  return OpaqueRealization.withCatalog(
    Object.freeze({ ...headers, ...semantics, report: Object.freeze([...report]) }),
    OpaqueRealization.catalogOf(semantics),
  )
})

/** Constructs the complete recoverable compiler frontend for one compilation request. */
export const frontend = Effect.fn('Frontend.frontend')(function* (
  request: ModuleClosure.CompilationRequest,
  options: Options = {},
): Effect.fn.Return<Frontend, never, SourceResolver.SourceResolver> {
  const report: Array<PhaseReport.PhaseReport> = []
  const closure = yield* PhaseReport.measureEffectInto(
    report,
    'closure',
    1,
    ModuleClosure.load(request),
    (value) => value.modules.length,
    (value) => value.diagnostics.length,
    options,
  )
  yield* Effect.yieldNow
  const facts = yield* analyzeFrontend(closure, report, options)
  return OpaqueRealization.withCatalog(
    Object.freeze({
      closure,
      ...facts,
      ...(request.configuration === undefined ? {} : {configuration: request.configuration}),
      ...(request.target === undefined ? {} : { requestedTarget: request.target }),
    }),
    OpaqueRealization.catalogOf(facts),
  )
})

/** Constructs one complete compiler frontend for the union closure of project roots. */
export const frontendProject = Effect.fn('Frontend.frontendProject')(function* (
  request: ModuleClosure.ProjectRequest,
  options: Options = {},
  previous?: IncrementalReuse.ProjectReuseBasis,
): Effect.fn.Return<ProjectFrontend, never, SourceResolver.SourceResolver> {
  const report: Array<PhaseReport.PhaseReport> = []
  const closure = yield* PhaseReport.measureEffectInto(
    report,
    'closure',
    request.roots.length,
    ModuleClosure.loadProject(request),
    (value) => value.modules.length,
    (value) => value.diagnostics.length,
    options,
  )
  yield* Effect.yieldNow
  const headers = yield* analyzeHeaders(closure, report, options)
  const bodyQueries = BodyQuery.make(
    headers.index,
    [...(previous?.semantics.values() ?? [])].map((module) => module.elaboration),
  )
  const currentElaboration = yield* PhaseReport.measureEffectInto(
    report,
    'body-queries',
    headers.index.modules.reduce((sum, module) => sum + module.declarations.length, 0),
    elaborateModules(closure, headers, new Map(), new Map(), bodyQueries),
    (value) =>
      [...value.results.values()].reduce((sum, result) => sum + result.functions.length, 0),
    (value) =>
      [...value.results.values()].reduce((sum, result) => sum + result.diagnostics.length, 0),
    { ...options, counters: () => BodyQuery.counters(bodyQueries) },
  )
  const currentResults = currentElaboration.results
  const currentOpaqueRealizations = OpaqueRealization.analyze(currentResults)
  yield* Effect.yieldNow
  const invalidation = PhaseReport.measure(
    'semantic-invalidation',
    closure.modules.length,
    () =>
      IncrementalReuse.invalidate({
        closure,
        surfaces: headers.surfaces,
        opaqueRealizations: currentOpaqueRealizations,
        ...(previous === undefined ? {} : { previous }),
      }),
    (value) => value.totals.recomputed,
    () => 0,
    {
      ...options,
      counters: (value) => {
        const totals = value.totals
        return Object.freeze({
          _tag: 'SemanticInvalidationCounters' as const,
          reusable: totals.reusable,
          recomputed: totals.recomputed,
          fresh: totals.reasons.Fresh,
          localChange: totals.reasons.LocalChange,
          opaqueBodyChange: totals.reasons.OpaqueBodyChange,
          opaqueTargetChange: totals.reasons.OpaqueTargetChange,
          opaqueLayoutChange: totals.reasons.OpaqueLayoutChange,
          dependencySurfaceChange: totals.reasons.DependencySurfaceChange,
          cyclicPeerChange: totals.reasons.CyclicPeerChange,
          environmentChange: totals.reasons.EnvironmentChange,
          surfaceChange: totals.reasons.SurfaceChange,
        })
      },
    },
  )
  report.push(invalidation.report)
  yield* Effect.yieldNow
  const semantics = yield* analyzeSemantics(
    closure,
    headers,
    report,
    options,
    previous === undefined ? undefined : { previous, invalidation: invalidation.value },
    Object.freeze({
      results: currentElaboration.computed,
      bodyQueries,
      ...(previous === undefined ? { opaqueRealizations: currentOpaqueRealizations } : {}),
    }),
  )
  const queryReport = report.findIndex((phase) => phase.phase === 'body-queries')
  const measuredQuery = report[queryReport]
  if (measuredQuery !== undefined)
    report[queryReport] = PhaseReport.make({
      ...measuredQuery,
      counters: BodyQuery.counters(bodyQueries),
    })
  return OpaqueRealization.withCatalog(
    Object.freeze({
      closure,
      ...headers,
      ...semantics,
      semanticInvalidation: invalidation.value,
      report: Object.freeze([...report]),
    }),
    OpaqueRealization.catalogOf(semantics),
  )
})
