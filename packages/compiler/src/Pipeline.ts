import * as Effect from 'effect/Effect'
import { AnalysisUnavailable } from './AnalysisUnavailable.js'
import * as Backend from './Backend.js'
import * as BackendRegistry from './BackendRegistry.js'
import * as CoroutineFrame from './CoroutineFrame.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Instances from './Instances.js'
import * as IntrinsicAvailability from './IntrinsicAvailability.js'
import * as Layout from './Layout.js'
import * as Lower from './Lower.js'
import type * as Mir from './Mir.js'
import * as MirNormalization from './MirNormalization.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as ModuleSemantics from './ModuleSemantics.js'
import * as ModuleSurface from './ModuleSurface.js'
import * as NameResolution from './NameResolution.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import * as Ownership from './Ownership.js'
import * as PhaseReport from './PhaseReport.js'
import * as ProvisionalMir from './ProvisionalMir.js'
import * as ResolutionSeams from './ResolutionSeams.js'
import * as SemanticInvalidation from './SemanticInvalidation.js'
import type * as SourceResolver from './SourceResolver.js'
import * as SuspensionMir from './SuspensionMir.js'
import * as SuspensionOwnership from './SuspensionOwnership.js'
import * as Target from './Target.js'

/** Optional environment-specific observations attached to compiler phase reports. */
export interface Options {
  readonly heapBytes?: () => number
  /** Internal differential-test escape hatch; production paths normalize shared MIR. */
  readonly normalizeMir?: boolean
}

const normalizeMir = (
  program: Mir.Module,
  provisional: ProvisionalMir.Module,
  options: Options,
): Mir.Module =>
  options.normalizeMir === false ? program : MirNormalization.normalize(program, provisional)

const finalizeMir = (
  program: Mir.Module,
  provisional: ProvisionalMir.Module,
  index: DeclarationIndex.Index,
  options: Options,
): Mir.Module => {
  const normalized = normalizeMir(program, provisional, options)
  return options.normalizeMir === false
    ? normalized
    : CoroutineFrame.apply(
        SuspensionMir.finalize(
          normalized,
          provisional,
          SuspensionOwnership.plan(normalized, provisional, index),
          index,
        ),
      )
}

/** An available target-owned artifact or the reason realization could not construct it. */
export type Targeted<A> =
  | { readonly _tag: 'Available'; readonly value: A }
  | {
      readonly _tag: 'Unavailable'
      readonly error: Target.TargetError | AnalysisUnavailable
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

/** Prior immutable project facts permitted to seed module semantic structural sharing. */
export interface ProjectReuseBasis {
  readonly closure: ModuleClosure.ProjectClosure
  readonly surfaces: ReadonlyMap<string, ModuleSurface.ModuleSurface>
  readonly semantics: ReadonlyMap<string, ModuleSemantics.ModuleSemantics>
  readonly opaqueRealizations: OpaqueRealization.Catalog
  readonly environment: string
}

/** Immutable target/runtime facts derived from exactly one Frontend value. */
export interface Realization {
  readonly instances: Instances.Discovery
  readonly target: Target.Selection
  readonly layoutCatalog: Targeted<Layout.Catalog>
  readonly layout: Targeted<Layout.Plan>
  readonly mir: Targeted<Mir.Module>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
}

/** Driver-facing runtime preparation that preserves artifact-production gates. */
export type Preparation =
  | {
      readonly _tag: 'Rejected'
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
      readonly report: ReadonlyArray<PhaseReport.PhaseReport>
    }
  | {
      readonly _tag: 'NoEntry'
      readonly reason: Extract<Instances.Entry, { readonly _tag: 'Unavailable' }>['reason']
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
      readonly report: ReadonlyArray<PhaseReport.PhaseReport>
    }
  | {
      readonly _tag: 'TargetFailed'
      readonly error: Target.TargetError
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
      readonly report: ReadonlyArray<PhaseReport.PhaseReport>
    }
  | {
      readonly _tag: 'BackendFailed'
      readonly error: Backend.BackendError
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
      readonly report: ReadonlyArray<PhaseReport.PhaseReport>
    }
  | {
      readonly _tag: 'Prepared'
      readonly target: Target.Target
      readonly program: Mir.Module
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
      readonly report: ReadonlyArray<PhaseReport.PhaseReport>
    }

const measured = <A>(
  report: Array<PhaseReport.PhaseReport>,
  phase: string,
  inputs: number,
  run: () => A,
  outputs: (value: A) => number,
  diagnostics: (value: A) => number = () => 0,
  options: Options = {},
): A => {
  const result = PhaseReport.measure(phase, inputs, run, outputs, diagnostics, options.heapBytes)
  report.push(result.report)
  return result.value
}

const measuredModuleWork = <A>(
  report: Array<PhaseReport.PhaseReport>,
  phase: string,
  modules: number,
  reused: number,
  run: () => A,
  outputs: (value: A) => number,
  diagnostics: (value: A) => number,
  options: Options,
): A => {
  const result = PhaseReport.measure(
    phase,
    modules - reused,
    run,
    outputs,
    diagnostics,
    options.heapBytes,
  )
  report.push(
    PhaseReport.make({
      ...result.report,
      counters: Object.freeze({
        _tag: 'ModuleReuseCounters',
        reused,
        recomputed: modules - reused,
      }),
    }),
  )
  return result.value
}

const hasInvalidGenericBody = (
  index: DeclarationIndex.Index,
  diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
): boolean =>
  index.modules.some((module) =>
    module.members.some(
      (member) =>
        member.typeParameters.length > 0 &&
        diagnostics.some(
          (diagnostic) =>
            diagnostic.span.sourceId === member.syntax.span.sourceId &&
            diagnostic.span.start >= member.syntax.span.start &&
            diagnostic.span.end <= member.syntax.span.end,
        ),
    ),
  )

interface HeaderFacts {
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly surfaces: ReadonlyMap<string, ModuleSurface.ModuleSurface>
}

const analyzeHeaders = (
  closure: ModuleClosure.Facts,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
): HeaderFacts => {
  const collected = measured(
    report,
    'declaration-collection',
    closure.modules.length,
    () => DeclarationIndex.collect(closure),
    (value) => value.modules.reduce((sum, module) => sum + module.members.length, 0),
    (value) => value.diagnostics.length,
    options,
  )
  const index = measured(
    report,
    'declaration-index',
    collected.modules.length,
    () => {
      const preliminary = NameResolution.resolve(closure, collected)
      const resolvers = ResolutionSeams.make(
        (module: string, path: DeclarationIndex.TypePathFact) =>
          NameResolution.resolveType(preliminary, collected, module, path),
        (module: string, path: DeclarationIndex.TypePathFact) =>
          NameResolution.resolveItem(preliminary, collected, module, path),
      )
      return DeclarationIndex.complete(collected, resolvers)
    },
    (value) => value.modules.reduce((sum, module) => sum + module.members.length, 0),
    (value) => value.diagnostics.length,
    options,
  )
  const resolution = measured(
    report,
    'name-resolution',
    index.modules.length,
    () => NameResolution.resolve(closure, index),
    (value) => value.modules.reduce((sum, module) => sum + module.bindings.length, 0),
    (value) => value.diagnostics.length,
    options,
  )
  const surfaces = measured(
    report,
    'module-surface',
    index.modules.length,
    () => ModuleSurface.fromIndex(index),
    (value) => value.size,
    () => 0,
    options,
  )
  return Object.freeze({ index, resolution, surfaces })
}

interface ElaboratedModules {
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly computed: ReadonlyMap<string, Elaboration.Result>
}

const elaborateModules = (
  closure: ModuleClosure.Facts,
  headers: HeaderFacts,
  retained: ReadonlyMap<string, Elaboration.Result> = new Map(),
  precomputed: ReadonlyMap<string, Elaboration.Result> = new Map(),
): ElaboratedModules => {
  const results = new Map<string, Elaboration.Result>()
  const computed = new Map<string, Elaboration.Result>()
  for (const module of closure.modules) {
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
}

const analyzeSemantics = (
  closure: ModuleClosure.Facts,
  headers: HeaderFacts,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
  reuse?: {
    readonly previous: ProjectReuseBasis
    readonly invalidation: SemanticInvalidation.SemanticInvalidation
  },
  precomputed?: {
    readonly results: ReadonlyMap<string, Elaboration.Result>
    readonly opaqueRealizations?: OpaqueRealization.Catalog
  },
): Omit<FrontendFacts, keyof HeaderFacts | 'report'> => {
  const reusable = new Set(
    reuse?.invalidation.observations.flatMap((observation) =>
      observation._tag === 'Reusable' ? [observation.module] : [],
    ) ?? [],
  )
  const retained = new Map<string, ModuleSemantics.ModuleSemantics>()
  for (const module of closure.modules) {
    if (!reusable.has(module.name)) continue
    const artifact = reuse?.previous.semantics.get(module.name)
    if (
      artifact !== undefined &&
      artifact.module === module.name &&
      artifact.elaboration.syntax === module.syntax
    )
      retained.set(module.name, artifact)
  }
  const retainedElaborations = new Map(
    [...retained].map(([module, semantics]) => [module, semantics.elaboration]),
  )
  const results = measuredModuleWork(
    report,
    'elaboration',
    closure.modules.length,
    retained.size,
    () => elaborateModules(closure, headers, retainedElaborations, precomputed?.results).results,
    (value) => [...value.values()].reduce((sum, module) => sum + module.functions.length, 0),
    (value) => [...value.values()].reduce((sum, module) => sum + module.diagnostics.length, 0),
    options,
  )
  const ownership = measuredModuleWork(
    report,
    'ownership',
    results.size,
    retained.size,
    () =>
      new Map(
        [...results.entries()].map(([name, result]) => [
          name,
          retained.get(name)?.ownership ?? Ownership.checkModule(result, headers.index),
        ]),
      ),
    (value) => [...value.values()].reduce((sum, module) => sum + module.functions.length, 0),
    (value) => [...value.values()].reduce((sum, module) => sum + module.diagnostics.length, 0),
    options,
  )
  const opaqueRealizations =
    precomputed?.opaqueRealizations ??
    measured(
      report,
      'opaque-realization',
      results.size,
      () => OpaqueRealization.analyze(results),
      (value) => value.definitions.size,
      (value) => value.diagnostics.length,
      options,
    )
  const semantics = new Map(
    [...results.entries()].map(([module, elaboration]) => {
      const reused = retained.get(module)
      if (reused !== undefined) return [module, reused] as const
      const moduleOwnership = ownership.get(module)
      if (moduleOwnership === undefined)
        throw new RangeError(`Pipeline lost ownership facts for ${module}`)
      return [module, ModuleSemantics.make(module, elaboration, moduleOwnership)] as const
    }),
  )
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
}

const analyzeFrontend = (
  closure: ModuleClosure.Facts,
  report: Array<PhaseReport.PhaseReport>,
  options: Options,
): FrontendFacts => {
  const headers = analyzeHeaders(closure, report, options)
  const semantics = analyzeSemantics(closure, headers, report, options)
  return OpaqueRealization.withCatalog(
    Object.freeze({ ...headers, ...semantics, report: Object.freeze([...report]) }),
    OpaqueRealization.catalogOf(semantics),
  )
}

/** Constructs the complete recoverable compiler frontend for one compilation request. */
export const frontend = Effect.fn('Pipeline.frontend')(function* (
  request: ModuleClosure.CompilationRequest,
  options: Options = {},
): Effect.fn.Return<Frontend, never, SourceResolver.SourceResolver> {
  const report: Array<PhaseReport.PhaseReport> = []
  const closureStartedAt = performance.now()
  const closure = yield* ModuleClosure.load(request)
  const closureHeap = options.heapBytes?.()
  report.push(
    PhaseReport.make({
      phase: 'closure',
      elapsedMs: performance.now() - closureStartedAt,
      inputs: 1,
      outputs: closure.modules.length,
      diagnostics: closure.diagnostics.length,
      ...(closureHeap === undefined ? {} : { heapBytes: closureHeap }),
    }),
  )
  const facts = analyzeFrontend(closure, report, options)
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
export const frontendProject = Effect.fn('Pipeline.frontendProject')(function* (
  request: ModuleClosure.ProjectRequest,
  options: Options = {},
  previous?: ProjectReuseBasis,
): Effect.fn.Return<ProjectFrontend, never, SourceResolver.SourceResolver> {
  const report: Array<PhaseReport.PhaseReport> = []
  const closureStartedAt = performance.now()
  const closure = yield* ModuleClosure.loadProject(request)
  const closureHeap = options.heapBytes?.()
  report.push(
    PhaseReport.make({
      phase: 'closure',
      elapsedMs: performance.now() - closureStartedAt,
      inputs: closure.rootModules.length,
      outputs: closure.modules.length,
      diagnostics: closure.diagnostics.length,
      ...(closureHeap === undefined ? {} : { heapBytes: closureHeap }),
    }),
  )
  const headers = analyzeHeaders(closure, report, options)
  const syntaxRetained = new Map(
    closure.modules.flatMap((module) => {
      const artifact = previous?.semantics.get(module.name)
      return artifact?.elaboration.syntax === module.syntax
        ? ([[module.name, artifact.elaboration]] as const)
        : []
    }),
  )
  const currentElaboration = elaborateModules(closure, headers, syntaxRetained)
  const currentResults = currentElaboration.results
  const currentOpaqueRealizations = OpaqueRealization.analyze(currentResults)
  const previousSyntax = new Map(
    previous?.closure.modules.map((module) => [module.name, module.syntax]),
  )
  const revisions = new Map(
    closure.modules.map((module) => [
      module.name,
      Object.freeze({
        _tag:
          previousSyntax.get(module.name) === undefined
            ? ('Fresh' as const)
            : previousSyntax.get(module.name) === module.syntax
              ? ('Reused' as const)
              : ('Changed' as const),
      }),
    ]),
  )
  const invalidation = PhaseReport.measure(
    'semantic-invalidation',
    closure.modules.length,
    () =>
      SemanticInvalidation.make({
        current: Object.freeze({
          closure,
          surfaces: headers.surfaces,
          opaqueRealizations: currentOpaqueRealizations,
          environment: SemanticInvalidation.environment,
        }),
        revisions,
        ...(previous === undefined
          ? {}
          : {
              previous: Object.freeze({
                closure: previous.closure,
                surfaces: previous.surfaces,
                opaqueRealizations: previous.opaqueRealizations,
                environment: previous.environment,
              }),
            }),
      }),
    (value) => value.totals.recomputed,
  )
  const totals = invalidation.value.totals
  report.push(
    PhaseReport.make({
      ...invalidation.report,
      counters: Object.freeze({
        _tag: 'SemanticInvalidationCounters',
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
      }),
    }),
  )
  const semantics = analyzeSemantics(
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

/**
 * Every diagnostic family judged against reachable concrete instances, collected once so that
 * `realize` and `prepare` cannot drift apart on which checks a specialized program must pass.
 */
const instanceViolationDiagnostics = (
  self: Frontend,
  discovery: Instances.Discovery,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Diagnostic.merge(
    Instances.violationDiagnostics(discovery),
    Instances.copyDropViolations(discovery, self.index),
    Instances.requirementBindingViolations(discovery, self.index),
    Instances.unlowerableWitnessViolations(discovery, self.index),
    Instances.storedCallableViolations(discovery, self.index),
    Instances.storedEffectViolations(discovery, self.index),
  )

/** Derives immutable target/runtime facts from one completed frontend. */
export const realize = (
  self: Frontend,
  targetId: string | undefined = self.requestedTarget,
  options: Options = {},
): Realization => {
  const report = [...self.report]
  const specializationInvalid =
    Diagnostic.hasGenericSpecializationErrors(self.diagnostics) ||
    hasInvalidGenericBody(self.index, self.diagnostics)
  const instances = measured(
    report,
    'instance-discovery',
    self.results.size,
    () =>
      specializationInvalid
        ? Instances.invalid(self.closure.rootModule)
        : Instances.discover(self.closure.rootModule, self.results, self.ownership, self.index),
    (value) => value.instances.length,
    (value) => value.violations.length,
    options,
  )
  const baseDiagnostics = Diagnostic.merge(
    self.diagnostics,
    instanceViolationDiagnostics(self, instances),
  )
  const specializationError =
    specializationInvalid || Diagnostic.hasGenericSpecializationErrors(baseDiagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.realize',
          message: 'Target-dependent phases are unavailable for invalid source specialization',
        })
      : undefined
  const returnContractError =
    specializationError === undefined && Diagnostic.hasReturnContractErrors(baseDiagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.realize',
          message: 'Target-dependent phases are unavailable for an invalid return contract',
        })
      : undefined
  // Storage fences (SEM0103/SEM0107) name programs the layout planner cannot serve, so realization
  // stops at the source diagnostic instead of producing an InvalidMir echo of it.
  const instanceFenceError =
    specializationError === undefined && Diagnostic.hasInstanceFenceErrors(baseDiagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.realize',
          message:
            'Target-dependent phases are unavailable while a reachable construction stores an unsupported executable representation',
        })
      : undefined
  const realizationError = specializationError ?? returnContractError ?? instanceFenceError
  const targetLayout = measured(
    report,
    'target-layout',
    instances.instances.length,
    () => {
      const target = Target.select(targetId)
      const layoutCatalog: Targeted<Layout.Catalog> =
        realizationError !== undefined
          ? Object.freeze({ _tag: 'Unavailable', error: realizationError })
          : target._tag === 'Resolved'
            ? Object.freeze({
                _tag: 'Available',
                value: Layout.catalog(target.target, self.index, instances),
              })
            : Object.freeze({ _tag: 'Unavailable', error: target.error })
      const layout: Targeted<Layout.Plan> =
        layoutCatalog._tag === 'Available'
          ? Object.freeze({ _tag: 'Available', value: Layout.plan(layoutCatalog.value, instances) })
          : layoutCatalog
      return Object.freeze({ target, layoutCatalog, layout })
    },
    (value) => (value.layout._tag === 'Available' ? value.layout.value.entries.length : 0),
    (value) => (value.layout._tag === 'Available' ? value.layout.value.diagnostics.length : 0),
    options,
  )
  const diagnostics = Diagnostic.merge(
    baseDiagnostics,
    ...(targetLayout.layout._tag === 'Available' ? [targetLayout.layout.value.diagnostics] : []),
  )
  const targetLiteralError =
    targetLayout.layout._tag === 'Available' &&
    Diagnostic.hasErrors(targetLayout.layout.value.diagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.realize',
          message: 'MIR is unavailable because a usize literal exceeds the selected target',
        })
      : undefined
  const mir: Targeted<Mir.Module> = (() => {
    if (targetLiteralError !== undefined)
      return Object.freeze({ _tag: 'Unavailable', error: targetLiteralError })
    if (targetLayout.layout._tag === 'Unavailable') return targetLayout.layout
    const availableLayout = targetLayout.layout.value
    return Object.freeze({
      _tag: 'Available',
      value: measured(
        report,
        'mir-lowering',
        instances.instances.length,
        () =>
          finalizeMir(
            Lower.lowerProgram(
              instances,
              self.ownership,
              availableLayout,
              self.index,
              OpaqueRealization.catalogOf(self),
            ),
            ProvisionalMir.build(instances, availableLayout, self.index),
            self.index,
            options,
          ),
        (value) => value.functions.length,
        () => 0,
        options,
      ),
    })
  })()
  return Object.freeze({
    instances,
    target: targetLayout.target,
    layoutCatalog: targetLayout.layoutCatalog,
    layout: targetLayout.layout,
    mir,
    diagnostics,
    report: Object.freeze([...report]),
  })
}

/** Prepares valid runtime facts for Driver while stopping at each artifact-production gate. */
export const prepare = (
  self: Frontend,
  backend: Backend.Backend,
  targetId: string | undefined = self.requestedTarget,
  options: Options = {},
): Preparation => {
  const report = [...self.report]
  if (Diagnostic.hasErrors(self.diagnostics))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: self.diagnostics,
      report: Object.freeze(report),
    })
  const discovery = measured(
    report,
    'instance-discovery',
    self.results.size,
    () => Instances.discover(self.closure.rootModule, self.results, self.ownership, self.index),
    (value) => value.instances.length,
    (value) => value.violations.length,
    options,
  )
  const diagnostics = Diagnostic.merge(
    self.diagnostics,
    instanceViolationDiagnostics(self, discovery),
  )
  if (Diagnostic.hasErrors(diagnostics))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics,
      report: Object.freeze(report),
    })
  if (discovery.entry._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'NoEntry',
      reason: discovery.entry.reason,
      diagnostics,
      report: Object.freeze(report),
    })
  const targetAndLayout = measured(
    report,
    'target-layout',
    discovery.instances.length,
    () => {
      const selection = Target.select(targetId)
      if (selection._tag === 'Unavailable') return selection
      const compatible = BackendRegistry.requireTarget(backend, selection.target)
      if (compatible._tag === 'Failure')
        return Object.freeze({
          _tag: 'BackendUnavailable' as const,
          error: new Backend.BackendError({
            operation: 'Backend.emit',
            backend: backend.id,
            message: compatible.failure.message,
            reason: { _tag: 'UnsupportedTarget', target: selection.target.id },
          }),
        })
      const availability = IntrinsicAvailability.select(
        discovery.intrinsics,
        IntrinsicAvailability.backendTarget(backend.id),
      )
      if (availability._tag === 'Unavailable')
        return Object.freeze({
          _tag: 'IntrinsicUnavailable' as const,
          diagnostics: availability.diagnostics,
        })
      const catalog = Layout.catalog(selection.target, self.index, discovery)
      return Object.freeze({
        _tag: 'Available' as const,
        target: selection.target,
        layout: Layout.plan(catalog, discovery),
      })
    },
    (value) => (value._tag === 'Available' ? value.layout.entries.length : 0),
    (value) => (value._tag === 'Available' ? value.layout.diagnostics.length : 0),
    options,
  )
  if (targetAndLayout._tag === 'BackendUnavailable')
    return Object.freeze({
      _tag: 'BackendFailed',
      error: targetAndLayout.error,
      diagnostics,
      report: Object.freeze(report),
    })
  if (targetAndLayout._tag === 'IntrinsicUnavailable')
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: Diagnostic.merge(diagnostics, targetAndLayout.diagnostics),
      report: Object.freeze(report),
    })
  if (targetAndLayout._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: targetAndLayout.error,
      diagnostics,
      report: Object.freeze(report),
    })
  if (Diagnostic.hasErrors(targetAndLayout.layout.diagnostics))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: Diagnostic.merge(diagnostics, targetAndLayout.layout.diagnostics),
      report: Object.freeze(report),
    })
  const program = measured(
    report,
    'mir-lowering',
    discovery.instances.length,
    () =>
      finalizeMir(
        Lower.lowerProgram(
          discovery,
          self.ownership,
          targetAndLayout.layout,
          self.index,
          OpaqueRealization.catalogOf(self),
        ),
        ProvisionalMir.build(discovery, targetAndLayout.layout, self.index),
        self.index,
        options,
      ),
    (value) => value.functions.length,
    () => 0,
    options,
  )
  return Object.freeze({
    _tag: 'Prepared',
    target: targetAndLayout.target,
    program,
    diagnostics,
    report: Object.freeze(report),
  })
}
