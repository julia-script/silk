import * as ConfigurationValue from './ConfigurationValue.js'
import * as ConfigurationOrigin from './ConfigurationOrigin.js'
import type * as PackageConfiguration from './PackageConfiguration.js'
import type * as ProfileBootstrap from './ProfileBootstrap.js'
import * as CompilationProfile from './CompilationProfile.js'
import * as Result from 'effect/Result'
import * as ConfigurationError from './ConfigurationError.js'
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
import * as ModuleSelection from './ModuleSelection.js'
import * as Canonical from './internal/Canonical.js'
import * as Realization from './Realization.js'
import * as ModuleSemantics from './ModuleSemantics.js'
import * as ModuleSurface from './ModuleSurface.js'
import * as NameResolution from './NameResolution.js'
import * as ResolutionWork from './ResolutionWork.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import * as Ownership from './Ownership.js'
import * as PhaseReport from './PhaseReport.js'
import * as SemanticInvalidation from './SemanticInvalidation.js'
import * as SourceResolver from './SourceResolver.js'
import * as SourceFile from './SourceFile.js'
import * as Stdlib from './Stdlib.js'
import * as Option from 'effect/Option'
import * as ArtifactComposition from './ArtifactComposition.js'

/** Optional environment-specific observations attached to compiler phase reports. */
export interface Options {
  readonly heapBytes?: () => number
  /** Internal differential-test escape hatch; production paths normalize shared MIR. */
  readonly normalizeMir?: boolean
}

interface FrontendFacts {
  readonly profile?: CompilationProfile.CompilationProfile
  readonly selection?: ModuleSelection.ModuleSelection
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
  readonly composition?: ArtifactComposition.Resolved
  readonly closure: ModuleClosure.Closure
  readonly initialProfile?: CompilationProfile.Initial
  readonly configurationError?: ConfigurationError.ConfigurationError
  readonly requestedTarget?: string
  readonly configuration?: ModuleClosure.CompilationRequest['configuration']
}

/** Immutable multi-root frontend facts computed once for one project revision. */
export interface ProjectFrontend extends FrontendFacts {
  readonly semanticEnvironment: string
  readonly closure: ModuleClosure.ProjectClosure
  readonly semanticInvalidation: SemanticInvalidation.SemanticInvalidation
}

interface HeaderFacts {
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly surfaces: ReadonlyMap<string, ModuleSurface.ModuleSurface>
}

const analyzeHeaders = Effect.fn('Frontend.analyzeHeaders')(function* (
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

const elaborateModules = Effect.fn('Frontend.elaborateModules')(function* (
  closure: ModuleClosure.Facts,
  headers: HeaderFacts,
  retained: ReadonlyMap<string, Elaboration.Result> = new Map(),
  precomputed: ReadonlyMap<string, Elaboration.Result> = new Map(),
  bodyQuery?: BodyQuery.BodyQuery,
): Effect.fn.Return<ElaboratedModules> {
  const results = new Map<string, Elaboration.Result>()
  const computed = new Map<string, Elaboration.Result>()
  for (const [ordinal, module] of closure.modules.entries()) {
    yield* Effect.gen(function* () {
      yield* Effect.annotateCurrentSpan({
        module: module.name,
      })
      yield* IncrementalReuse.checkpointModuleBatch(ordinal)
      const reused = retained.get(module.name) ?? precomputed.get(module.name)
      if (reused !== undefined) {
        results.set(module.name, reused)
        return
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
    }).pipe(Effect.withSpan('Frontend.elaborateModules:module'))
  }
  return Object.freeze({ results, computed })
})

const analyzeSemantics = Effect.fn('Frontend.analyzeSemantics')(function* (
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

const analyzeFrontend = Effect.fn('Frontend.analyzeFrontend')(function* (
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

/** Supplies lazy static helpers with headers and source, without checking executable bodies. */
const bootstrapFacts = Effect.fn('Frontend.bootstrapFacts')(function* (
  closure: ModuleClosure.Facts,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
): Effect.fn.Return<FrontendFacts> {
  const headers = yield* analyzeHeaders(closure, report, options)
  const results = new Map<string, Elaboration.Result>()
  for (const module of closure.modules) {
    const moduleHeaders = headers.index.modules.find(
      (candidate) => candidate.module === module.name,
    )
    const scope = NameResolution.scopeOf(headers.resolution, module.name)
    if (moduleHeaders === undefined || scope === undefined)
      throw new RangeError('Bootstrap lost module headers')
    results.set(
      module.name,
      Elaboration.elaborateModule({
        syntax: module.syntax,
        headers: { ...moduleHeaders, declarations: [], constants: [] },
        scope,
        index: headers.index,
      }),
    )
  }
  return OpaqueRealization.withCatalog(
    Object.freeze({
      ...headers,
      results,
      semantics: new Map<string, ModuleSemantics.ModuleSemantics>(),
      ownership: new Map<string, Ownership.ModuleOwnership>(),
      diagnostics: Diagnostic.merge(
        closure.diagnostics,
        ...closure.modules.map((module) => module.syntax.lexicalDiagnostics),
        ...closure.modules.map((module) => module.syntax.parserDiagnostics),
      ),
      report: Object.freeze([...report]),
    }),
    OpaqueRealization.analyze(results),
  )
})

type ProfileSnapshot =
  | Result.Result<CompilationProfile.Initial, ConfigurationError.ConfigurationError>
  | undefined
type BindingSnapshot = Result.Result<
  ReadonlyArray<PackageConfiguration.Binding>,
  ConfigurationError.ConfigurationError
>
type CompositionSnapshot =
  | Result.Result<ArtifactComposition.Resolved, ConfigurationError.ConfigurationError>
  | undefined

const normalizeProfile = Effect.fn('Frontend.normalizeProfile')(function* (
  request: ModuleClosure.CompilationRequest,
) {
  const input =
    request.configuration?.profile ??
    (request.target === undefined ? undefined : { target: request.target })
  return input === undefined ? undefined : yield* Effect.result(CompilationProfile.normalize(input))
})

const decodeBindings = Effect.fn('Frontend.decodeBindings')(function* (
  configuration: ModuleClosure.CompilationRequest['configuration'],
) {
  const bindings: Array<PackageConfiguration.Binding> = []
  for (const binding of configuration?.bindings ?? []) {
    const origin = ConfigurationOrigin.snapshot(binding.origin)
    const value = yield* ConfigurationValue.decode(binding.value, origin)
    bindings.push(Object.freeze({ ...binding, origin, value }))
  }
  return Object.freeze(bindings)
})

const snapshotConfiguration = Effect.fn('Frontend.snapshotConfiguration')(
  (
    configuration: ModuleClosure.CompilationRequest['configuration'],
    initial: ProfileSnapshot,
    bindings: BindingSnapshot,
  ) =>
    Effect.sync(() => {
      if (configuration === undefined) return undefined
      const profile =
        initial !== undefined && Result.isSuccess(initial)
          ? CompilationProfile.input(initial.success)
          : configuration.profile
      return Object.freeze({
        ...configuration,
        profile,
        bindings: Result.isSuccess(bindings) ? bindings.success : Object.freeze([]),
        ...(configuration.modules === undefined
          ? {}
          : {
              modules: Object.freeze(
                configuration.modules.map((module) => Object.freeze({ ...module })),
              ),
            }),
      })
    }),
)

const resolveComposition = Effect.fn('Frontend.resolveComposition')(function* (
  root: string,
  configuration: ModuleClosure.CompilationRequest['configuration'],
  initial: CompilationProfile.Initial,
) {
  const catalog = yield* ArtifactComposition.decode(
    configuration?.composition ?? ArtifactComposition.defaults(initial),
    configuration?.compositionOrigin,
  )
  return yield* ArtifactComposition.resolve(catalog, root, initial)
})

interface AdditionalRoots {
  readonly sources: ReadonlyArray<SourceFile.SourceFile>
  readonly failures: ReadonlyArray<SourceResolver.SourceResolverError>
  readonly error: ConfigurationError.ConfigurationError | undefined
}

/** Load composition roots while retaining missing sources and operational failures separately. */
const loadAdditionalRoots = Effect.fn('Frontend.loadAdditionalRoots')(function* (
  root: string,
  composition: CompositionSnapshot,
  componentModules: ReadonlyArray<string>,
): Effect.fn.Return<AdditionalRoots, never, SourceResolver.SourceResolver> {
  const sources: Array<SourceFile.SourceFile> = []
  const failures: Array<SourceResolver.SourceResolverError> = []
  let error: ConfigurationError.ConfigurationError | undefined
  if (composition !== undefined && Result.isSuccess(composition)) {
    const missing: Array<string> = []
    for (const module of new Set([...composition.success.modules, ...componentModules])) {
      if (module === root) continue
      const resolved = yield* Effect.result(
        Stdlib.isReserved(module)
          ? SourceResolver.resolveStandardLibrary(module)
          : SourceResolver.resolve(module),
      )
      if (Result.isFailure(resolved)) failures.push(resolved.failure)
      else if (Option.isNone(resolved.success)) missing.push(module)
      else
        sources.push(
          SourceFile.make(module, resolved.success.value.bytes, resolved.success.value.origin),
        )
    }
    if (missing.length > 0)
      error = ConfigurationError.make(
        'ArtifactComposition.roots',
        'MissingParameter',
        'artifact source roots',
        [
          composition.success.runtime?.origin ??
            ConfigurationOrigin.literal('artifact composition'),
          ...composition.success.retention
            .filter((root) => missing.includes(root.module))
            .map((root) => root.origin),
        ],
        missing,
      )
  }
  return { sources, failures, error }
})

const loadClosure = Effect.fn('Frontend.loadClosure')(function* (
  request: ModuleClosure.CompilationRequest,
  roots: AdditionalRoots,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
) {
  const closure = yield* PhaseReport.measureEffectInto(
    report,
    'closure',
    1,
    ModuleClosure.load(request, roots.sources),
    (value) => value.modules.length,
    (value) => value.diagnostics.length,
    options,
  )
  return roots.failures.length === 0
    ? closure
    : Object.freeze({
        ...closure,
        resolutionFailures: Object.freeze([...closure.resolutionFailures, ...roots.failures]),
      })
})

/** Preserve configuration-error precedence and the semantic facts' opaque realization catalog. */
const assembleSnapshot = Effect.fn('Frontend.assembleSnapshot')(
  (
    request: ModuleClosure.CompilationRequest,
    closure: ModuleClosure.Closure,
    facts: FrontendFacts,
    initial: ProfileSnapshot,
    composition: CompositionSnapshot,
    roots: AdditionalRoots,
    configuration: ModuleClosure.CompilationRequest['configuration'],
    bindings: BindingSnapshot,
  ) =>
    Effect.sync((): Frontend => {
      let initialFacts: Pick<Frontend, 'initialProfile' | 'configurationError'> = {}
      if (initial !== undefined)
        initialFacts = Result.isSuccess(initial)
          ? { initialProfile: initial.success }
          : { configurationError: initial.failure }
      let compositionFacts: Pick<Frontend, 'composition' | 'configurationError'> = {}
      if (composition !== undefined)
        compositionFacts = Result.isSuccess(composition)
          ? { composition: composition.success }
          : { configurationError: composition.failure }
      return OpaqueRealization.withCatalog(
        Object.freeze({
          closure,
          ...facts,
          ...initialFacts,
          ...compositionFacts,
          ...(roots.error === undefined ? {} : { configurationError: roots.error }),
          ...(configuration === undefined ? {} : { configuration }),
          ...(Result.isFailure(bindings) ? { configurationError: bindings.failure } : {}),
          ...(request.target === undefined ? {} : { requestedTarget: request.target }),
        }),
        OpaqueRealization.catalogOf(facts),
      )
    }),
)

const diagnoseIncompleteProfile = Effect.fn('Frontend.diagnoseIncompleteProfile')(
  (self: Frontend, closure: ModuleClosure.Closure, target: string | undefined) =>
    Effect.sync((): Frontend => {
      const span = closure.modules.find((module) => module.name === closure.rootModule)?.syntax.root
        .span
      return span === undefined
        ? self
        : OpaqueRealization.withCatalog(
            {
              ...self,
              diagnostics: Diagnostic.merge(self.diagnostics, [
                Diagnostic.staticPhaseViolation(
                  'ModuleSelection.profile',
                  target ?? '<unavailable>',
                  [],
                  span,
                ),
              ]),
            },
            OpaqueRealization.catalogOf(self),
          )
    }),
)

const selectModules = Effect.fn('Frontend.selectModules')(function* (
  request: ModuleClosure.CompilationRequest,
  closure: ModuleClosure.Closure,
  roots: AdditionalRoots,
  completion: ProfileBootstrap.Completion,
) {
  const selected = yield* ModuleSelection.select(
    { roots: [request.root, ...roots.sources], application: request.root.id },
    {
      ...closure,
      _tag: 'ProjectModuleClosure',
      rootModules: [request.root.id, ...roots.sources.map((root) => root.id)],
    },
    completion,
  )
  const selectedClosure = ModuleClosure.view(
    {
      ...selected.closure,
      resolutionFailures: Object.freeze([
        ...selected.closure.resolutionFailures,
        ...roots.failures,
      ]),
    },
    closure.rootModule,
  )
  if (selectedClosure === undefined) throw new RangeError('Module selection lost its root')
  return { closure: selectedClosure, selection: selected.selection }
})

const finalizeSelection = Effect.fn('Frontend.finalizeSelection')(
  (
    self: Frontend,
    facts: FrontendFacts,
    closure: ModuleClosure.Closure,
    selection: ModuleSelection.ModuleSelection,
  ) =>
    Effect.sync((): Frontend =>
      OpaqueRealization.withCatalog(
        Object.freeze({ ...self, ...facts, closure, selection, profile: selection.profile }),
        OpaqueRealization.catalogOf(facts),
      ),
    ),
)

/** Constructs the complete recoverable compiler frontend for one compilation request. */
export const frontend = Effect.fn('Frontend.frontend')(function* (
  request: ModuleClosure.CompilationRequest,
  options: Options = {},
  componentModules: ReadonlyArray<string> = [],
): Effect.fn.Return<Frontend, never, SourceResolver.SourceResolver> {
  yield* Effect.annotateCurrentSpan('frontend.root', request.root.id)
  const initial = yield* normalizeProfile(request)
  const bindings = yield* Effect.result(decodeBindings(request.configuration))
  const configuration = yield* snapshotConfiguration(request.configuration, initial, bindings)
  const composition =
    initial !== undefined && Result.isSuccess(initial)
      ? yield* Effect.result(resolveComposition(request.root.id, configuration, initial.success))
      : undefined
  const roots = yield* loadAdditionalRoots(request.root.id, composition, componentModules)
  const report: Array<PhaseReport.PhaseReport> = []
  const closure = yield* loadClosure(request, roots, report, options)
  yield* Effect.yieldNow
  const requiresSelection = ModuleSelection.required(closure)
  yield* Effect.annotateCurrentSpan('frontend.requiresSelection', requiresSelection)
  const facts = yield* requiresSelection
    ? bootstrapFacts(closure, report, options)
    : analyzeFrontend(closure, report, options)
  const unselected = yield* assembleSnapshot(
    request,
    closure,
    facts,
    initial,
    composition,
    roots,
    configuration,
    bindings,
  )
  if (!requiresSelection) return unselected

  const configured = yield* Realization.configure(unselected, request.target)
  if (configured.completion === undefined)
    return yield* diagnoseIncompleteProfile(configured.frontend, closure, request.target)
  const selected = yield* selectModules(request, closure, roots, configured.completion)
  const selectedFacts = yield* analyzeFrontend(selected.closure, report, options)
  return yield* finalizeSelection(unselected, selectedFacts, selected.closure, selected.selection)
})

/** Selected module closure and headers shared by compilation and source catalogs. */
export interface SelectedProject {
  readonly closure: ModuleClosure.ProjectClosure
  readonly profile?: CompilationProfile.CompilationProfile
  readonly selection?: ModuleSelection.ModuleSelection
  readonly headers: HeaderFacts
}

const resolveProjectComposition = Effect.fn('Frontend.resolveProjectComposition')(function* (
  application: string,
  configuration: NonNullable<ModuleClosure.ProjectRequest['configuration']>,
) {
  const profile = yield* CompilationProfile.decode(configuration.profile)
  return yield* resolveComposition(application, configuration, profile)
})

interface ExpandedProject {
  readonly request: ModuleClosure.ProjectRequest
  readonly failures: ReadonlyArray<SourceResolver.SourceResolverError>
  readonly missing: ReadonlyArray<string>
}

const loadProjectRoots = Effect.fn('Frontend.loadProjectRoots')(function* (
  request: ModuleClosure.ProjectRequest,
): Effect.fn.Return<ExpandedProject, never, SourceResolver.SourceResolver> {
  const roots = [...request.roots]
  const application = request.application ?? roots[0]?.id
  const requestedModules = new Set(application === undefined ? [] : [application])
  const failures: Array<SourceResolver.SourceResolverError> = []
  const missing: Array<string> = []
  if (request.configuration !== undefined && application !== undefined) {
    const selectedRoots = yield* Effect.result(
      resolveProjectComposition(application, request.configuration),
    )
    if (Result.isSuccess(selectedRoots))
      for (const module of selectedRoots.success.modules) requestedModules.add(module)
  }
  for (const module of requestedModules) {
    if (roots.some((root) => root.id === module)) continue
    const resolved = yield* Effect.result(
      Stdlib.isReserved(module)
        ? SourceResolver.resolveStandardLibrary(module)
        : SourceResolver.resolve(module),
    )
    if (Result.isFailure(resolved)) failures.push(resolved.failure)
    else if (Option.isNone(resolved.success)) missing.push(module)
    else
      roots.push(
        SourceFile.make(module, resolved.success.value.bytes, resolved.success.value.origin),
      )
  }
  return {
    request: { ...request, roots, ...(application === undefined ? {} : { application }) },
    failures,
    missing,
  }
})

const loadProjectClosure = Effect.fn('Frontend.loadProjectClosure')(function* (
  expanded: ExpandedProject,
  rootCount: number,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
) {
  const closure = yield* PhaseReport.measureEffectInto(
    report,
    'closure',
    rootCount,
    ModuleClosure.loadProject(expanded.request),
    (value) => value.modules.length,
    (value) => value.diagnostics.length,
    options,
  )
  return Object.freeze({
    ...closure,
    resolutionFailures: Object.freeze([...closure.resolutionFailures, ...expanded.failures]),
  })
})

const diagnoseProjectProfile = Effect.fn('Frontend.diagnoseProjectProfile')(
  (closure: ModuleClosure.ProjectClosure, frontend: Frontend, first: string | undefined) =>
    Effect.sync(() => {
      const span = closure.modules.find((module) => module.name === first)?.syntax.root.span
      return Object.freeze({
        ...closure,
        diagnostics: Diagnostic.merge(
          closure.diagnostics,
          frontend.diagnostics.filter(
            (diagnostic) => diagnostic.code === Diagnostic.invalidConfigurationCode,
          ),
          span === undefined || !ModuleSelection.required(closure)
            ? []
            : [
                Diagnostic.staticPhaseViolation(
                  'ModuleSelection.profile',
                  '<unavailable>',
                  [],
                  span,
                ),
              ],
        ),
      })
    }),
)

const configureProjectSelection = Effect.fn('Frontend.configureProjectSelection')(function* (
  request: ModuleClosure.ProjectRequest,
  closure: ModuleClosure.ProjectClosure,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
) {
  const application = request.application
  let selection: ModuleSelection.ModuleSelection | undefined
  let profile: CompilationProfile.CompilationProfile | undefined
  let bootstrapHeaders: HeaderFacts | undefined
  if (request.configuration !== undefined || ModuleSelection.required(closure)) {
    const first =
      application !== undefined && closure.sources.has(application)
        ? application
        : closure.rootModules[0]
    const view = first === undefined ? undefined : ModuleClosure.view(closure, first)
    if (view === undefined) throw new RangeError('Project selection lost its root')
    const base = yield* bootstrapFacts(closure, report, options)
    if (!ModuleSelection.required(closure)) bootstrapHeaders = base
    const configured = yield* Realization.configure(
      OpaqueRealization.withCatalog(
        {
          ...base,
          closure: view,
          ...(request.configuration === undefined ? {} : { configuration: request.configuration }),
        },
        OpaqueRealization.catalogOf(base),
      ),
      undefined,
    )
    profile = configured.completion?.profile
    if (configured.completion !== undefined && ModuleSelection.required(closure)) {
      const selected = yield* ModuleSelection.select(request, closure, configured.completion)
      closure = selected.closure
      selection = selected.selection
    } else if (configured.completion === undefined) {
      closure = yield* diagnoseProjectProfile(closure, configured.frontend, first)
    }
  }
  return { closure, profile, selection, bootstrapHeaders }
})

const diagnoseMissingProjectRoots = Effect.fn('Frontend.diagnoseMissingProjectRoots')(
  (
    request: ModuleClosure.ProjectRequest,
    closure: ModuleClosure.ProjectClosure,
    missing: ReadonlyArray<string>,
  ) =>
    Effect.sync(() => {
      const span =
        request.roots[0] === undefined
          ? undefined
          : closure.modules.find((module) => module.name === request.roots[0]?.id)?.syntax.root.span
      if (missing.length > 0 && span !== undefined)
        closure = {
          ...closure,
          diagnostics: Diagnostic.merge(closure.diagnostics, [
            Diagnostic.invalidConfiguration(
              ConfigurationError.make(
                'Frontend.selectProject',
                'MissingParameter',
                'artifact source roots',
                [
                  request.configuration?.compositionOrigin ??
                    ConfigurationOrigin.literal('application'),
                ],
                missing,
              ),
              span,
            ),
          ]),
        }

      return closure
    }),
)

/** Runs canonical profile/bootstrap/selection without elaborating unrelated executable bodies. */
export const selectProject = Effect.fn('Frontend.selectProject')(function* (
  request: ModuleClosure.ProjectRequest,
  report: Array<PhaseReport.PhaseReport> = [],
  options: Options = {},
): Effect.fn.Return<SelectedProject, never, SourceResolver.SourceResolver> {
  yield* Effect.annotateCurrentSpan(
    'frontend.roots',
    request.roots.map((root) => root.id),
  )
  const expanded = yield* loadProjectRoots(request)
  const loaded = yield* loadProjectClosure(expanded, request.roots.length, report, options)
  yield* Effect.yieldNow
  const selected = yield* configureProjectSelection(expanded.request, loaded, report, options)
  const headers =
    selected.bootstrapHeaders ?? (yield* analyzeHeaders(selected.closure, report, options))
  const closure = yield* diagnoseMissingProjectRoots(
    expanded.request,
    selected.closure,
    expanded.missing,
  )
  return Object.freeze({
    closure,
    headers,
    ...(selected.profile === undefined ? {} : { profile: selected.profile }),
    ...(selected.selection === undefined ? {} : { selection: selected.selection }),
  })
})

/** Constructs one complete compiler frontend for the union closure of project roots. */
export const frontendProject = Effect.fn('Frontend.frontendProject')(function* (
  request: ModuleClosure.ProjectRequest,
  options: Options = {},
  previous?: IncrementalReuse.ProjectReuseBasis,
): Effect.fn.Return<ProjectFrontend, never, SourceResolver.SourceResolver> {
  const report: Array<PhaseReport.PhaseReport> = []
  const { closure, profile, selection, headers } = yield* selectProject(request, report, options)
  const semanticEnvironment = Canonical.record('SelectedFrontend', [
    SemanticInvalidation.environment,
    profile?.identity ?? '',
    selection?.dependencies ?? '',
  ])
  const compatiblePrevious = previous?.environment === semanticEnvironment ? previous : undefined
  const bodyQueries = BodyQuery.make(
    headers.index,
    [...(compatiblePrevious?.semantics.values() ?? [])].map((module) => module.elaboration),
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
        environment: semanticEnvironment,
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
      semanticEnvironment,
      ...(selection === undefined ? {} : { selection }),
      ...(profile === undefined ? {} : { profile }),
      report: Object.freeze([...report]),
    }),
    OpaqueRealization.catalogOf(semantics),
  )
})

/** Extends a proven source snapshot with demanded component modules through the explicit resolver. */
export const withComponents = Effect.fn('Frontend.withComponents')(function* (
  self: Frontend,
  profile: CompilationProfile.CompilationProfile,
  modules: ReadonlyArray<string>,
  options: Options = {},
): Effect.fn.Return<Frontend, never, SourceResolver.SourceResolver> {
  if (modules.every((module) => self.closure.sources.has(module))) return self
  const root = self.closure.sources.get(self.closure.rootModule)
  if (root === undefined) throw new RangeError('Component activation lost application source')
  const resolver = yield* SourceResolver.SourceResolver
  const existing = (module: string) => {
    const source = self.closure.sources.get(module)
    return source === undefined
      ? undefined
      : SourceResolver.resolved(SourceFile.toUint8Array(source), source.origin)
  }
  const resolve = Effect.fnUntraced(function* (module: string, standard: boolean) {
    const source = existing(module)
    return source === undefined
      ? yield* standard ? resolver.resolveStandardLibrary(module) : resolver.resolve(module)
      : Option.some(source)
  })
  return yield* frontend(
    {
      root,
      configuration: { ...self.configuration, profile: CompilationProfile.input(profile) },
    },
    options,
    modules,
  ).pipe(
    Effect.provideService(SourceResolver.SourceResolver, {
      ...resolver,
      resolve: (module) => resolve(module, false),
      resolveStandardLibrary: (module) => resolve(module, true),
    }),
  )
})
