import * as Effect from 'effect/Effect'
import { AnalysisUnavailable } from './AnalysisUnavailable.js'
import * as Backend from './Backend.js'
import * as BackendRegistry from './BackendRegistry.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import * as Lower from './Lower.js'
import type * as Mir from './Mir.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as NameResolution from './NameResolution.js'
import * as Ownership from './Ownership.js'
import * as PhaseReport from './PhaseReport.js'
import type * as SourceResolver from './SourceResolver.js'
import * as Target from './Target.js'

/** Optional environment-specific observations attached to compiler phase reports. */
export interface Options {
  readonly heapBytes?: () => number
}

/** An available target-owned artifact or the reason realization could not construct it. */
export type Targeted<A> =
  | { readonly _tag: 'Available'; readonly value: A }
  | {
      readonly _tag: 'Unavailable'
      readonly error: Target.TargetError | AnalysisUnavailable
    }

/** Immutable compiler frontend facts shared by Analysis and Driver. */
export interface Frontend {
  readonly closure: ModuleClosure.Closure
  readonly index: DeclarationIndex.Index
  readonly resolution: NameResolution.Resolution
  readonly results: ReadonlyMap<string, Elaboration.Result>
  readonly ownership: ReadonlyMap<string, Ownership.ModuleOwnership>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport.PhaseReport>
  readonly requestedTarget?: string
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
      return DeclarationIndex.complete(collected, (module, path) =>
        NameResolution.resolveType(preliminary, collected, module, path),
      )
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
  const results = measured(
    report,
    'elaboration',
    index.modules.length,
    () =>
      new Map(
        closure.modules.map((module) => {
          const headers = index.modules.find((candidate) => candidate.module === module.name)
          const scope = NameResolution.scopeOf(resolution, module.name)
          if (headers === undefined || scope === undefined)
            throw new RangeError(`Pipeline lost module facts for ${module.name}`)
          return [
            module.name,
            Elaboration.elaborateModule({ syntax: module.syntax, headers, scope, index }),
          ]
        }),
      ),
    (value) => [...value.values()].reduce((sum, module) => sum + module.functions.length, 0),
    (value) => [...value.values()].reduce((sum, module) => sum + module.diagnostics.length, 0),
    options,
  )
  const ownership = measured(
    report,
    'ownership',
    results.size,
    () =>
      new Map(
        [...results.entries()].map(([name, result]) => [name, Ownership.checkModule(result)]),
      ),
    (value) => [...value.values()].reduce((sum, module) => sum + module.functions.length, 0),
    (value) => [...value.values()].reduce((sum, module) => sum + module.diagnostics.length, 0),
    options,
  )
  const diagnostics = Diagnostic.merge(
    ...closure.modules.map((module) => module.syntax.lexicalDiagnostics),
    ...closure.modules.map((module) => module.syntax.parserDiagnostics),
    closure.diagnostics,
    resolution.diagnostics,
    ...[...results.values()].map((result) => result.diagnostics),
    ...[...ownership.values()].map((facts) => facts.diagnostics),
  )
  return Object.freeze({
    closure,
    index,
    resolution,
    results,
    ownership,
    diagnostics,
    report: Object.freeze([...report]),
    ...(request.target === undefined ? {} : { requestedTarget: request.target }),
  })
})

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
    Instances.violationDiagnostics(instances),
    Instances.copyDropViolations(instances, self.index),
  )
  const specializationError =
    specializationInvalid || Diagnostic.hasGenericSpecializationErrors(baseDiagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.realize',
          message: 'Target-dependent phases are unavailable for invalid source specialization',
        })
      : undefined
  const targetLayout = measured(
    report,
    'target-layout',
    instances.instances.length,
    () => {
      const target = Target.select(targetId)
      const layoutCatalog: Targeted<Layout.Catalog> =
        specializationError !== undefined
          ? Object.freeze({ _tag: 'Unavailable', error: specializationError })
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
        () => Lower.lowerProgram(instances, self.ownership, availableLayout, self.index),
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
    Instances.violationDiagnostics(discovery),
    Instances.copyDropViolations(discovery, self.index),
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
    () => Lower.lowerProgram(discovery, self.ownership, targetAndLayout.layout, self.index),
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
