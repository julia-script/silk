import type * as DeclarationIndex from './DeclarationIndex.js'

/**
 * Every diagnostic family judged against reachable concrete instances, collected once so that
 * `realize` and `prepare` cannot drift apart on which checks a specialized program must pass.
 */
const instanceViolationDiagnostics = (
  self: Frontend,
  discovery: Instances.Discovery,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  Diagnostic.merge(
    InstanceDiagnostics.violationDiagnostics(discovery),
    InstanceDiagnostics.copyDropViolations(discovery, self.index),
    InstanceDiagnostics.requirementBindingViolations(discovery, self.index),
    InstanceDiagnostics.unlowerableWitnessViolations(discovery, self.index),
    InstanceDiagnostics.storedCallableViolations(discovery, self.index),
    InstanceDiagnostics.storedEffectViolations(discovery, self.index),
    ExecutableProperty.violationDiagnostics(discovery, self.index),
  )

function discoverAndLower(
  self: Frontend,
  targetId: string | undefined,
  options: Options,
): Realization
function discoverAndLower(
  self: Frontend,
  targetId: string | undefined,
  options: Options,
  backend: Backend.Backend,
): Preparation
function discoverAndLower(
  self: Frontend,
  targetId: string | undefined,
  options: Options,
  backend?: Backend.Backend,
): Realization | Preparation {
  const report = [...self.report]
  if (backend !== undefined && Diagnostic.hasErrors(self.diagnostics))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: self.diagnostics,
      report: Object.freeze(report),
    })

  const specializationInvalid =
    Diagnostic.hasGenericSpecializationErrors(self.diagnostics) ||
    hasInvalidGenericBody(self.index, self.diagnostics)
  const instances = PhaseReport.measureInto(
    report,
    'instance-discovery',
    self.results.size,
    () =>
      backend === undefined && specializationInvalid
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
  if (backend !== undefined && Diagnostic.hasErrors(baseDiagnostics))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })
  if (backend !== undefined && instances.entry._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'NoEntry',
      reason: instances.entry.reason,
      ...(instances.entry.requirements === undefined
        ? {}
        : { requirements: instances.entry.requirements }),
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })

  const analysisUnavailable = (() => {
    if (backend !== undefined) return undefined
    if (specializationInvalid || Diagnostic.hasGenericSpecializationErrors(baseDiagnostics))
      return new AnalysisUnavailable({
        operation: 'Analysis.realize',
        message: 'Target-dependent phases are unavailable for invalid source specialization',
      })
    if (Diagnostic.hasReturnContractErrors(baseDiagnostics))
      return new AnalysisUnavailable({
        operation: 'Analysis.realize',
        message: 'Target-dependent phases are unavailable for an invalid return contract',
      })
    if (Diagnostic.hasInstanceFenceErrors(baseDiagnostics))
      return new AnalysisUnavailable({
        operation: 'Analysis.realize',
        message:
          'Target-dependent phases are unavailable while a reachable construction stores an unsupported executable representation',
      })
    return undefined
  })()

  const targetLayout = PhaseReport.measureInto(
    report,
    'target-layout',
    instances.instances.length,
    () => {
      const selection = Target.select(targetId)
      if (selection._tag === 'Unavailable')
        return Object.freeze({ _tag: 'Unavailable' as const, selection, error: selection.error })
      if (backend !== undefined) {
        const compatible = BackendRegistry.requireTarget(backend, selection.target)
        if (compatible._tag === 'Failure')
          return Object.freeze({
            _tag: 'BackendUnavailable' as const,
            selection,
            error: new Backend.BackendError({
              operation: 'Backend.emit',
              backend: backend.id,
              message: compatible.failure.message,
              reason: { _tag: 'UnsupportedTarget', target: selection.target.id },
            }),
          })
        const availability = IntrinsicAvailability.select(
          instances.intrinsics,
          IntrinsicAvailability.backendTarget(backend.id),
        )
        if (availability._tag === 'Unavailable')
          return Object.freeze({
            _tag: 'IntrinsicUnavailable' as const,
            selection,
            error: Target.unavailableInventory(selection.target, availability.operations),
          })
      }
      if (analysisUnavailable !== undefined)
        return Object.freeze({
          _tag: 'AnalysisUnavailable' as const,
          selection,
          error: analysisUnavailable,
        })
      const catalog = Layout.catalog(
        selection.target,
        self.index,
        instances,
        OpaqueRealization.catalogOf(self),
      )
      return Object.freeze({
        _tag: 'Available' as const,
        selection,
        target: selection.target,
        catalog,
        layout: Layout.plan(catalog, instances, self.index),
      })
    },
    (value) => (value._tag === 'Available' ? value.layout.entries.length : 0),
    (value) => (value._tag === 'Available' ? value.layout.diagnostics.length : 0),
    options,
  )

  if (targetLayout._tag === 'BackendUnavailable')
    return Object.freeze({
      _tag: 'BackendFailed',
      error: targetLayout.error,
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })
  if (targetLayout._tag === 'IntrinsicUnavailable')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: targetLayout.error,
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })
  if (backend !== undefined && targetLayout._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: targetLayout.error,
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })

  const diagnostics = Diagnostic.merge(
    baseDiagnostics,
    ...(targetLayout._tag === 'Available' ? [targetLayout.layout.diagnostics] : []),
  )
  if (
    backend !== undefined &&
    targetLayout._tag === 'Available' &&
    Diagnostic.hasErrors(targetLayout.layout.diagnostics)
  )
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics,
      report: Object.freeze(report),
    })

  const targetLiteralError =
    backend === undefined &&
    targetLayout._tag === 'Available' &&
    Diagnostic.hasErrors(targetLayout.layout.diagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.realize',
          message: 'MIR is unavailable because a usize literal exceeds the selected target',
        })
      : undefined
  const program =
    targetLayout._tag === 'Available' && targetLiteralError === undefined
      ? PhaseReport.measureInto(
          report,
          'mir-lowering',
          instances.instances.length,
          () =>
            finalizeMir(
              Lower.lowerProgram(
                instances,
                self.ownership,
                targetLayout.layout,
                self.index,
                OpaqueRealization.catalogOf(self),
              ),
              ProvisionalMir.build(instances, targetLayout.layout, self.index),
              self.index,
              options,
            ),
          (value) => value.functions.length,
          () => 0,
          options,
        )
      : undefined

  if (backend !== undefined) {
    if (targetLayout._tag !== 'Available' || program === undefined)
      throw new RangeError('Driver lowering reached an unavailable target after its gates')
    return Object.freeze({
      _tag: 'Prepared',
      target: targetLayout.target,
      program,
      diagnostics,
      report: Object.freeze(report),
    })
  }

  const unavailable =
    targetLayout._tag === 'Unavailable' || targetLayout._tag === 'AnalysisUnavailable'
      ? targetLayout.error
      : undefined
  return Object.freeze({
    instances,
    target: targetLayout.selection,
    layoutCatalog:
      targetLayout._tag === 'Available'
        ? Object.freeze({ _tag: 'Available', value: targetLayout.catalog })
        : Object.freeze({ _tag: 'Unavailable', error: targetLayout.error }),
    layout:
      targetLayout._tag === 'Available'
        ? Object.freeze({ _tag: 'Available', value: targetLayout.layout })
        : Object.freeze({ _tag: 'Unavailable', error: targetLayout.error }),
    mir:
      program !== undefined
        ? Object.freeze({ _tag: 'Available', value: program })
        : Object.freeze({
            _tag: 'Unavailable',
            error:
              targetLiteralError ??
              unavailable ??
              new AnalysisUnavailable({
                operation: 'Analysis.realize',
                message: 'MIR is unavailable',
              }),
          }),
    diagnostics,
    report: Object.freeze([...report]),
  })
}

/** Derives immutable target/runtime facts from one completed frontend. */
export const realize = (
  self: Frontend,
  targetId: string | undefined = self.requestedTarget,
  options: Options = {},
): Realization => discoverAndLower(self, targetId, options)

/** Prepares valid runtime facts for Driver while stopping at each artifact-production gate. */
export const prepare = (
  self: Frontend,
  backend: Backend.Backend,
  targetId: string | undefined = self.requestedTarget,
  options: Options = {},
): Preparation => discoverAndLower(self, targetId, options, backend)

import { AnalysisUnavailable } from './AnalysisUnavailable.js'
import * as Backend from './Backend.js'
import * as BackendRegistry from './BackendRegistry.js'
import * as CoroutineFrame from './CoroutineFrame.js'
import * as Diagnostic from './Diagnostic.js'
import * as ExecutableProperty from './ExecutableProperty.js'
import type { Frontend, Options } from './Frontend.js'
import * as InstanceDiagnostics from './InstanceDiagnostics.js'
import * as Instances from './Instances.js'
import * as IntrinsicAvailability from './IntrinsicAvailability.js'
import * as Layout from './Layout.js'
import * as Lower from './Lower.js'
import type * as Mir from './Mir.js'
import * as MirNormalization from './MirNormalization.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import * as PhaseReport from './PhaseReport.js'
import * as ProvisionalMir from './ProvisionalMir.js'
import * as SuspensionMir from './SuspensionMir.js'
import * as SuspensionOwnership from './SuspensionOwnership.js'
import * as Target from './Target.js'
import type * as Type from './Type.js'

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
      readonly requirements?: ReadonlyArray<Type.Requirement>
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
