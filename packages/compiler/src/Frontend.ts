import * as ConfigurationValue from './ConfigurationValue.js'
import * as ConfigurationOrigin from './ConfigurationOrigin.js'
import type * as PackageConfiguration from './PackageConfiguration.js'
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

/** Supplies lazy static helpers with headers and source, without checking executable bodies. */
const bootstrapFacts = Effect.fnUntraced(function* (
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

/** Constructs the complete recoverable compiler frontend for one compilation request. */
export const frontend = Effect.fn('Frontend.frontend')(function* (
  request: ModuleClosure.CompilationRequest,
  options: Options = {},
): Effect.fn.Return<Frontend, never, SourceResolver.SourceResolver> {
  const initialInput =
    request.configuration?.profile ??
    (request.target === undefined ? undefined : { target: request.target })
  const initial =
    initialInput === undefined
      ? undefined
      : yield* Effect.result(CompilationProfile.normalize(initialInput))
  const bindingSnapshot = yield* Effect.result(
    Effect.gen(function* () {
      const bindings: Array<PackageConfiguration.Binding> = []
      for (const binding of request.configuration?.bindings ?? []) {
        const origin = ConfigurationOrigin.snapshot(binding.origin)
        const value = yield* ConfigurationValue.decode(binding.value, origin)
        bindings.push(Object.freeze({ ...binding, origin, value }))
      }
      return Object.freeze(bindings)
    }),
  )
  let configuration = request.configuration
  if (configuration !== undefined) {
    const profile =
      initial !== undefined && Result.isSuccess(initial)
        ? CompilationProfile.input(initial.success)
        : configuration.profile
    configuration = Object.freeze({
      ...configuration,
      profile,
      bindings: Result.isSuccess(bindingSnapshot) ? bindingSnapshot.success : Object.freeze([]),
      ...(configuration.modules === undefined
        ? {}
        : {
            modules: Object.freeze(
              configuration.modules.map((module) => Object.freeze({ ...module })),
            ),
          }),
    })
  }
  const composition =
    initial !== undefined && Result.isSuccess(initial)
      ? yield* Effect.result(
          Effect.gen(function* () {
            const catalog = yield* ArtifactComposition.decode(
              configuration?.composition ??
                ArtifactComposition.defaults(request.root.id, initial.success),
              configuration?.compositionOrigin,
            )
            return yield* ArtifactComposition.resolve(catalog, request.root.id, initial.success)
          }),
        )
      : undefined
  const additionalRoots: Array<SourceFile.SourceFile> = []
  const rootFailures: Array<SourceResolver.SourceResolverError> = []
  let rootError: ConfigurationError.ConfigurationError | undefined
  if (composition !== undefined && Result.isSuccess(composition)) {
    const missing: Array<string> = []
    for (const module of composition.success.modules) {
      if (module === request.root.id) continue
      const resolved = yield* Effect.result(
        Stdlib.isReserved(module)
          ? SourceResolver.resolveStandardLibrary(module)
          : SourceResolver.resolve(module),
      )
      if (Result.isFailure(resolved)) rootFailures.push(resolved.failure)
      else if (Option.isNone(resolved.success)) missing.push(module)
      else
        additionalRoots.push(
          SourceFile.make(module, resolved.success.value.bytes, resolved.success.value.origin),
        )
    }
    if (missing.length > 0)
      rootError = ConfigurationError.make(
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
  const report: Array<PhaseReport.PhaseReport> = []
  const loadedClosure = yield* PhaseReport.measureEffectInto(
    report,
    'closure',
    1,
    ModuleClosure.load(request, additionalRoots),
    (value) => value.modules.length,
    (value) => value.diagnostics.length,
    options,
  )
  const closure =
    rootFailures.length === 0
      ? loadedClosure
      : Object.freeze({
          ...loadedClosure,
          resolutionFailures: Object.freeze([...loadedClosure.resolutionFailures, ...rootFailures]),
        })
  yield* Effect.yieldNow
  const facts = yield* ModuleSelection.required(closure)
    ? bootstrapFacts(closure, report, options)
    : analyzeFrontend(closure, report, options)
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
  const unselected: Frontend = OpaqueRealization.withCatalog(
    Object.freeze({
      closure,
      ...facts,
      ...initialFacts,
      ...compositionFacts,
      ...(rootError === undefined ? {} : { configurationError: rootError }),
      ...(configuration === undefined ? {} : { configuration }),
      ...(Result.isFailure(bindingSnapshot) ? { configurationError: bindingSnapshot.failure } : {}),
      ...(request.target === undefined ? {} : { requestedTarget: request.target }),
    }),
    OpaqueRealization.catalogOf(facts),
  )
  if (!ModuleSelection.required(closure)) return unselected
  const configured = yield* Realization.configure(unselected, request.target)
  if (configured.completion === undefined) {
    const span = closure.modules.find((module) => module.name === closure.rootModule)?.syntax.root
      .span
    return span === undefined
      ? configured.frontend
      : OpaqueRealization.withCatalog(
          {
            ...configured.frontend,
            diagnostics: Diagnostic.merge(configured.frontend.diagnostics, [
              Diagnostic.staticPhaseViolation(
                'ModuleSelection.profile',
                request.target ?? '<unavailable>',
                [],
                span,
              ),
            ]),
          },
          OpaqueRealization.catalogOf(configured.frontend),
        )
  }
  const selected = yield* ModuleSelection.select(
    { roots: [request.root, ...additionalRoots], application: request.root.id },
    {
      ...closure,
      _tag: 'ProjectModuleClosure',
      rootModules: [request.root.id, ...additionalRoots.map((root) => root.id)],
    },
    configured.completion,
  )
  const selectedClosure = ModuleClosure.view(
    {
      ...selected.closure,
      resolutionFailures: Object.freeze([...selected.closure.resolutionFailures, ...rootFailures]),
    },
    closure.rootModule,
  )
  if (selectedClosure === undefined) throw new RangeError('Module selection lost its root')
  const selectedFacts = yield* analyzeFrontend(selectedClosure, report, options)
  return OpaqueRealization.withCatalog(
    Object.freeze({
      ...unselected,
      ...selectedFacts,
      closure: selectedClosure,
      selection: selected.selection,
      profile: selected.selection.profile,
    }),
    OpaqueRealization.catalogOf(selectedFacts),
  )
})

/** Selected module closure and headers shared by compilation and source catalogs. */
export interface SelectedProject {
  readonly closure: ModuleClosure.ProjectClosure
  readonly profile?: CompilationProfile.CompilationProfile
  readonly selection?: ModuleSelection.ModuleSelection
  readonly headers: HeaderFacts
}

/** Runs canonical profile/bootstrap/selection without elaborating unrelated executable bodies. */
export const selectProject = Effect.fn('Frontend.selectProject')(function* (
  request: ModuleClosure.ProjectRequest,
  report: Array<PhaseReport.PhaseReport> = [],
  options: Options = {},
): Effect.fn.Return<SelectedProject, never, SourceResolver.SourceResolver> {
  const roots = [...request.roots]
  const application = request.application ?? roots[0]?.id
  const requestedModules = new Set(application === undefined ? [] : [application])
  const failures: Array<SourceResolver.SourceResolverError> = []
  const missing: Array<string> = []
  if (request.configuration?.composition !== undefined && application !== undefined) {
    const selectedRoots = yield* Effect.result(
      Effect.gen(function* () {
        const profile = yield* CompilationProfile.decode(request.configuration?.profile)
        const catalog = yield* ArtifactComposition.decode(
          request.configuration?.composition,
          request.configuration?.compositionOrigin,
        )
        return yield* ArtifactComposition.resolve(catalog, application, profile)
      }),
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
  const expanded = { ...request, roots }
  let closure = yield* PhaseReport.measureEffectInto(
    report,
    'closure',
    request.roots.length,
    ModuleClosure.loadProject(expanded),
    (value) => value.modules.length,
    (value) => value.diagnostics.length,
    options,
  )
  closure = Object.freeze({
    ...closure,
    resolutionFailures: Object.freeze([...closure.resolutionFailures, ...failures]),
  })
  yield* Effect.yieldNow
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
      const selected = yield* ModuleSelection.select(expanded, closure, configured.completion)
      closure = selected.closure
      selection = selected.selection
    } else if (configured.completion === undefined) {
      const span = closure.modules.find((module) => module.name === first)?.syntax.root.span
      closure = Object.freeze({
        ...closure,
        diagnostics: Diagnostic.merge(
          closure.diagnostics,
          configured.frontend.diagnostics.filter(
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
    }
  }
  const headers = bootstrapHeaders ?? (yield* analyzeHeaders(closure, report, options))
  const span =
    roots[0] === undefined
      ? undefined
      : closure.modules.find((module) => module.name === roots[0]?.id)?.syntax.root.span
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

  return Object.freeze({
    closure,
    headers,
    ...(profile === undefined ? {} : { profile }),
    ...(selection === undefined ? {} : { selection }),
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
