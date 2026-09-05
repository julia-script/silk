import type * as DeclarationIndex from './DeclarationIndex.js'

/**
 * Every diagnostic family judged against reachable concrete instances, collected once so that
 * `realize` and `prepare` cannot drift apart on which checks a specialized program must pass.
 */
const instanceViolationDiagnostics = (
  self: Frontend,
  discovery: Instances.Discovery,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const entryKey = discovery.entry._tag === 'Resolved' ? discovery.entry.key : undefined
  const entryInstance =
    entryKey !== undefined
      ? discovery.instances.find(
          (instance) => Instances.keyText(instance.key) === Instances.keyText(entryKey),
        )
      : undefined
  const entryDiagnostic =
    entryKey !== undefined && entryInstance !== undefined
      ? ExecutionBoundary.entryDiagnostic(
          Instances.suspensionOf(discovery, entryKey),
          false,
          entryInstance.function.declaration.syntax.span,
        )
      : undefined
  return Diagnostic.merge(
    InstanceDiagnostics.violationDiagnostics(discovery),
    InstanceDiagnostics.copyDropViolations(discovery, self.index),
    InstanceDiagnostics.requirementBindingViolations(discovery, self.index),
    InstanceDiagnostics.unlowerableWitnessViolations(discovery, self.index),
    InstanceDiagnostics.storedCallableViolations(discovery, self.index),
    InstanceDiagnostics.storedEffectViolations(discovery, self.index),
    ExecutableProperty.violationDiagnostics(discovery, self.index),
    ...(entryDiagnostic === undefined ? [] : [[entryDiagnostic]]),
    entryShapeDiagnostics(self, discovery),
  )
}

/**
 * ENTRY-001/002/004/005: a root `main` whose declared shape cannot be an entry is reported at its
 * declaration. An absent or untyped `main` stays a discovery reason: the first has no declaration
 * to point at and the second already carries the ordinary missing-result diagnostic.
 */
const entryShapeDiagnostics = (
  self: Frontend,
  discovery: Instances.Discovery,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  if (discovery.entry._tag !== 'Unavailable') return Object.freeze([])
  const root = self.results.get(discovery.rootModule)
  if (root === undefined) return Object.freeze([])
  const lookup = Elaboration.declarationByName(root, 'main')
  if (lookup._tag === 'Missing') return Object.freeze([])
  const declarations = lookup._tag === 'Resolved' ? [lookup.declaration] : lookup.declarations
  const detail = entryShapeDetail(discovery.entry)
  if (detail === undefined) return Object.freeze([])
  return Object.freeze(
    declarations.map((declaration) =>
      Diagnostic.invalidEntryShape(detail, declaration.syntax.span),
    ),
  )
}

const entryShapeDetail = (
  entry: Extract<Instances.Entry, { readonly _tag: 'Unavailable' }>,
): string | undefined => {
  switch (entry.reason) {
    case 'AmbiguousEntry':
      return 'is declared more than once in the root module'
    case 'StaticEntry':
      return 'must be a runtime function'
    case 'GenericEntry':
      return 'must not declare type parameters'
    case 'ParameterizedEntry':
      return 'must take no parameters'
    case 'PrivateEntry':
      return 'must be declared `pub`'
    case 'InvalidOrdinaryEntryResult':
      return 'must explicitly return `()` or `i32` when it is an ordinary function'
    case 'InvalidEffectEntryResult':
      return 'must succeed with `()` when it is an effect function'
    case 'EffectEntryRequirements':
      return `has unresolved requirements: ${(entry.requirements ?? []).map((requirement) => Type.encodeRequirement(requirement)).join(', ')}`
    case 'MissingEntry':
    case 'UntypedEntry':
    case 'UnavailableEntryBody':
    case 'MissingLibraryExport':
    case 'InvalidSource':
      return undefined
  }
}

/** Rejects a pointer-sized exported static that cannot be represented on the selected target. */
const foreignStaticTargetDiagnostics = (
  index: DeclarationIndex.Index,
  target: Target.Target,
): ReadonlyArray<Diagnostic.Diagnostic> =>
  index.modules.flatMap((module) =>
    module.members.flatMap((member): ReadonlyArray<Diagnostic.Diagnostic> => {
      if (
        member._tag !== 'ForeignStaticDeclaration' ||
        member.direction !== 'Export' ||
        member.declaredType._tag !== 'Resolved' ||
        member.literal?._tag !== 'IntegerLiteral'
      )
        return []
      const scalar =
        typeof member.declaredType.type === 'string'
          ? Scalar.find(member.declaredType.type)
          : undefined
      if (scalar?.spelling !== 'usize' && scalar?.spelling !== 'isize') return []
      const range = Scalar.range(scalar, target.pointerSize === 4 ? 32 : 64)
      if (member.literal.value >= range.minimum && member.literal.value <= range.maximum) return []
      return [
        Diagnostic.invalidConstant(
          `the exported C static initializer is outside ${scalar.spelling} on ${target.id}`,
          member.initializer?.span ?? member.syntax.span,
        ),
      ]
    }),
  )

function discoverAndLower(
  self: Frontend,
  targetId: string | undefined,
  options: Options & { readonly artifactKind?: ArtifactKind.ArtifactKind },
): Realization
function discoverAndLower(
  self: Frontend,
  targetId: string | undefined,
  options: Options & { readonly artifactKind?: ArtifactKind.ArtifactKind },
  prepareForEmission: true,
): Preparation
function discoverAndLower(
  self: Frontend,
  targetId: string | undefined,
  options: Options & { readonly artifactKind?: ArtifactKind.ArtifactKind },
  prepareForEmission = false,
): Realization | Preparation {
  const report = [...self.report]
  if (prepareForEmission && Diagnostic.hasErrors(self.diagnostics))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: self.diagnostics,
      report: Object.freeze(report),
    })

  const specializationInvalid =
    Diagnostic.hasGenericSpecializationErrors(self.diagnostics) ||
    hasInvalidGenericBody(self.index, self.diagnostics)
  // Static specialization is target-relative. Resolve the closed target before constructing any
  // executable worklist so no candidate body can observe a missing or host-inferred target.
  const targetSelection = Target.select(targetId)
  const foreignStaticDiagnostics =
    targetSelection._tag === 'Resolved'
      ? foreignStaticTargetDiagnostics(self.index, targetSelection.target)
      : Object.freeze([])
  const instances = PhaseReport.measureInto(
    report,
    'instance-discovery',
    self.results.size,
    () =>
      targetSelection._tag === 'Unavailable' || (!prepareForEmission && specializationInvalid)
        ? Instances.invalid(self.closure.rootModule)
        : Instances.discover(
            self.closure.rootModule,
            self.results,
            self.index,
            targetSelection.target,
            self.resolution,
            ArtifactKind.isLibrary(options.artifactKind ?? ArtifactKind.nativeExecutable)
              ? 'Library'
              : 'Executable',
          ),
    (value) => value.instances.length,
    (value) => value.violations.length,
    { ...options, counters: (value) => value.counters },
  )
  const declarationDiagnosticKeys = new Set(
    self.diagnostics.map(
      (diagnostic) =>
        `${diagnostic.phase}\u0000${diagnostic.code}\u0000${diagnostic.span.sourceId}\u0000${diagnostic.span.start}\u0000${diagnostic.span.end}`,
    ),
  )
  const residualizationDiagnostics = instances.residualizationDiagnostics.filter(
    (diagnostic) =>
      !declarationDiagnosticKeys.has(
        `${diagnostic.phase}\u0000${diagnostic.code}\u0000${diagnostic.span.sourceId}\u0000${diagnostic.span.start}\u0000${diagnostic.span.end}`,
      ),
  )
  const baseDiagnostics = Diagnostic.merge(
    self.diagnostics,
    residualizationDiagnostics,
    instanceViolationDiagnostics(self, instances),
    foreignStaticDiagnostics,
  )
  if (prepareForEmission && Diagnostic.hasErrors(baseDiagnostics))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })
  const foreignDiagnostics =
    !prepareForEmission || targetSelection._tag === 'Unavailable'
      ? Object.freeze([])
      : ForeignAvailability.select(instances.foreignCalls, targetSelection.target)
  if (foreignDiagnostics.length > 0)
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: Diagnostic.merge(baseDiagnostics, foreignDiagnostics),
      report: Object.freeze(report),
    })
  const analysisUnavailable = (() => {
    if (prepareForEmission) return undefined
    if (Diagnostic.hasErrors(foreignStaticDiagnostics))
      return new AnalysisUnavailable({
        operation: 'Analysis.realize',
        message:
          'Target-dependent phases are unavailable because an exported C static initializer exceeds the selected target',
      })
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
      const selection = targetSelection
      if (selection._tag === 'Unavailable')
        return Object.freeze({ _tag: 'Unavailable' as const, selection, error: selection.error })
      if (prepareForEmission) {
        const availability = IntrinsicAvailability.select(instances.intrinsics, selection.target)
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

  if (targetLayout._tag === 'IntrinsicUnavailable')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: targetLayout.error,
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })
  if (prepareForEmission && targetLayout._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'TargetFailed',
      error: targetLayout.error,
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })
  if (prepareForEmission && instances.entry._tag === 'Unavailable')
    return Object.freeze({
      _tag: 'NoEntry',
      reason: instances.entry.reason,
      ...(instances.entry.requirements === undefined
        ? {}
        : { requirements: instances.entry.requirements }),
      diagnostics: baseDiagnostics,
      report: Object.freeze(report),
    })

  const diagnostics = Diagnostic.merge(
    baseDiagnostics,
    ...(targetLayout._tag === 'Available' ? [targetLayout.layout.diagnostics] : []),
  )
  if (
    prepareForEmission &&
    targetLayout._tag === 'Available' &&
    Diagnostic.hasErrors(targetLayout.layout.diagnostics)
  )
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics,
      report: Object.freeze(report),
    })

  const targetLiteralError =
    !prepareForEmission &&
    targetLayout._tag === 'Available' &&
    Diagnostic.hasErrors(targetLayout.layout.diagnostics)
      ? new AnalysisUnavailable({
          operation: 'Analysis.realize',
          message: 'MIR is unavailable because a usize literal exceeds the selected target',
        })
      : undefined
  const residualizationError = Diagnostic.hasErrors(instances.residualizationDiagnostics)
    ? new AnalysisUnavailable({
        operation: 'Analysis.realize',
        message: 'MIR is unavailable after failed source residualization',
      })
    : undefined
  const finalized =
    targetLayout._tag === 'Available' &&
    targetLiteralError === undefined &&
    residualizationError === undefined
      ? PhaseReport.measureInto(
          report,
          'mir-lowering',
          instances.instances.length,
          () =>
            finalizeMir(
              Lower.lowerProgram(
                instances,
                targetLayout.layout,
                self.index,
                OpaqueRealization.catalogOf(self),
              ),
              ProvisionalMir.build(instances, targetLayout.layout, self.index),
              self.index,
              options,
            ),
          (value) => value.program?.functions.length ?? 0,
          (value) => value.diagnostics.length,
          options,
        )
      : undefined
  const program = finalized?.program
  const finalizedDiagnostics = Diagnostic.merge(diagnostics, finalized?.diagnostics ?? [])

  if (prepareForEmission) {
    if (Diagnostic.hasErrors(finalizedDiagnostics))
      return Object.freeze({
        _tag: 'Rejected',
        diagnostics: finalizedDiagnostics,
        report: Object.freeze(report),
      })
    if (targetLayout._tag !== 'Available' || program === undefined)
      throw new RangeError('Driver lowering reached an unavailable target after its gates')
    const planning = ForeignPlanning.check(program, targetLayout.target)
    if (planning.length > 0)
      return Object.freeze({
        _tag: 'Rejected',
        diagnostics: Diagnostic.merge(finalizedDiagnostics, planning),
        report: Object.freeze(report),
      })
    return Object.freeze({
      _tag: 'Prepared',
      target: targetLayout.target,
      program,
      diagnostics: finalizedDiagnostics,
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
              residualizationError ??
              unavailable ??
              new AnalysisUnavailable({
                operation: 'Analysis.realize',
                message: 'MIR is unavailable',
              }),
          }),
    diagnostics: finalizedDiagnostics,
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
  targetId: string | undefined = self.requestedTarget,
  options: Options & { readonly artifactKind?: ArtifactKind.ArtifactKind } = {},
): Preparation => discoverAndLower(self, targetId, options, true)

import { AnalysisUnavailable } from './AnalysisUnavailable.js'
import * as ArtifactKind from './ArtifactKind.js'
import * as CoroutineFrame from './CoroutineFrame.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as ExecutableProperty from './ExecutableProperty.js'
import * as ExecutionBoundary from './ExecutionBoundary.js'
import * as ForeignAvailability from './ForeignAvailability.js'
import * as ForeignPlanning from './ForeignPlanning.js'
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
import * as Scalar from './Scalar.js'
import * as SuspensionMir from './SuspensionMir.js'
import * as SuspensionOwnership from './SuspensionOwnership.js'
import * as Target from './Target.js'
import * as Type from './Type.js'

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
): {
  readonly program: Mir.Module | undefined
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const normalized = normalizeMir(program, provisional, options)
  const ownership = SuspensionOwnership.plan(normalized, provisional, index)
  const diagnostics = ownership.violations.map((violation) =>
    Diagnostic.invalidSuspensionOwnership(violation.detail, violation.span),
  )
  if (diagnostics.length > 0) return { program: undefined, diagnostics }
  if (options.normalizeMir === false) return { program: normalized, diagnostics }
  return {
    program: CoroutineFrame.apply(
      SuspensionMir.finalize(normalized, provisional, ownership, index),
    ),
    diagnostics,
  }
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
