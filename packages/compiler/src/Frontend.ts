import * as Effect from 'effect/Effect'
import * as DeclarationCollection from './DeclarationCollection.js'
import * as DeclarationCompletion from './DeclarationCompletion.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as IncrementalReuse from './IncrementalReuse.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as ModuleSemantics from './ModuleSemantics.js'
import * as ModuleSurface from './ModuleSurface.js'
import * as NameResolution from './NameResolution.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import * as Ownership from './Ownership.js'
import * as PhaseReport from './PhaseReport.js'
import * as ResolutionSeams from './ResolutionSeams.js'
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
      const resolvers = ResolutionSeams.make(
        (module: string, path: DeclarationFacts.TypePathFact) =>
          NameResolution.resolveType(preliminary, collected, module, path),
        (module: string, path: DeclarationFacts.TypePathFact) =>
          NameResolution.resolveItem(preliminary, collected, module, path),
      )
      return DeclarationCompletion.complete(collected, resolvers)
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
    })
    results.set(module.name, result)
    computed.set(module.name, result)
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
  },
): Effect.fn.Return<Omit<FrontendFacts, keyof HeaderFacts | 'report'>> {
  const retained =
    reuse === undefined
      ? new Map<string, ModuleSemantics.ModuleSemantics>()
      : yield* IncrementalReuse.retainedSemantics(closure, reuse.previous, reuse.invalidation)
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
  const ownership = yield* PhaseReport.measureEffectInto(
    report,
    'ownership',
    results.size - retained.size,
    Effect.gen(function* () {
      const ownership = new Map<string, Ownership.ModuleOwnership>()
      let ordinal = 0
      for (const [name, result] of results) {
        yield* IncrementalReuse.checkpointModuleBatch(ordinal)
        ownership.set(
          name,
          retained.get(name)?.ownership ?? Ownership.checkModule(result, headers.index),
        )
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
          reused: retained.size,
          recomputed: results.size - retained.size,
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
    if (reused !== undefined) semantics.set(module, reused)
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
  const syntaxRetained = IncrementalReuse.syntaxRetained(closure, previous)
  const currentElaboration = yield* elaborateModules(closure, headers, syntaxRetained)
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
      ...(previous === undefined ? { opaqueRealizations: currentOpaqueRealizations } : {}),
    }),
  )
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
