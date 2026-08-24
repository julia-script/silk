import * as ConformanceProof from './ConformanceProof.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Instances from './Instances.js'
import * as Mir from './Mir.js'
import * as ProvisionalMir from './ProvisionalMir.js'
import type * as SuspensionOwnership from './SuspensionOwnership.js'
import * as Type from './Type.js'

/** Finalizes provisional complete-or-relay control into target-neutral executable MIR facts. */

const samePoint = (left: ProvisionalMir.ControlId, right: ProvisionalMir.ControlId): boolean =>
  left.execution === right.execution &&
  left.sourceId === right.sourceId &&
  left.functionOrdinal === right.functionOrdinal &&
  left.spanStart === right.spanStart &&
  left.spanEnd === right.spanEnd &&
  left.ordinal === right.ordinal &&
  left.port === right.port

const pointOf = (
  owner: Instances.InstanceKey,
  point: ProvisionalMir.ControlId,
): Mir.SuspensionPointId =>
  Object.freeze({
    _tag: 'SuspensionPointId',
    owner,
    sourceId: point.sourceId,
    spanStart: point.spanStart,
    spanEnd: point.spanEnd,
    ordinal: point.ordinal,
  })

const resumeOf = (
  point: Mir.SuspensionPointId,
  path: Mir.ResumePointId['path'],
): Mir.ResumePointId => Object.freeze({ _tag: 'ResumePointId', point, path })

const runnerOf = (
  runner: ProvisionalMir.Runner,
  index: DeclarationIndex.Index,
  operation?: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' | 'ExecutionPark' }
  >,
  functions: ReadonlyArray<Mir.MirFunction> = [],
): Mir.SuspensionRunner => {
  const arguments_ = operation?._tag === 'ExecutionPark' ? [] : (operation?.arguments ?? [])
  const operationProviders = operation?._tag === 'RunEffectValue' ? operation.providers : []
  // One selection identity for both the dedup and the argument lookup below — two predicates
  // here previously let a provider count as "already selected" yet miss its runtime argument.
  const sameSelection = (
    left: {
      readonly role: string
      readonly requirementAccess: string
      readonly capability: Type.Type
    },
    right: {
      readonly role: string
      readonly requirementAccess: string
      readonly capability: Type.Type
    },
  ): boolean =>
    left.role === right.role &&
    left.requirementAccess === right.requirementAccess &&
    Type.equals(left.capability, right.capability)
  const providersWithRuntimeSelections = Object.freeze([
    ...operationProviders,
    ...runner.providers.filter(
      (provider) => !operationProviders.some((selected) => sameSelection(provider, selected)),
    ),
  ])
  const runtimeProviders = providersWithRuntimeSelections.filter(
    (provider) => provider.witness?._tag === 'SourceConformanceWitness',
  )
  const argumentOffset = Math.max(0, arguments_.length - runtimeProviders.length)
  let runtimeOrdinal = 0
  const providers: ReadonlyArray<Mir.SuspensionProviderArgument> =
    providersWithRuntimeSelections.map((provider) => {
      const hasRuntimeArgument = provider.witness?._tag === 'SourceConformanceWitness'
      const selected =
        operation?._tag === 'RunEffectValue'
          ? operation.providers.find((candidate) => sameSelection(candidate, provider))
          : undefined
      const argument = hasRuntimeArgument
        ? operation?._tag === 'RunEffectValue'
          ? selected?.argument
          : arguments_.at(argumentOffset + runtimeOrdinal)
        : undefined
      if (hasRuntimeArgument) runtimeOrdinal += 1
      const witness =
        provider.witness ??
        ConformanceProof.witness(index, provider.providerType, provider.capability)
      return Object.freeze({
        ...provider,
        ...(witness === undefined ? {} : { witness }),
        ...(argument === undefined ? {} : { argument }),
        purposes: ['ChildRequirement'] as const,
      })
    })
  const declaration =
    operation === undefined || operation._tag === 'ExecutionPark'
      ? runner.declaration
      : operation._tag === 'RunEffect'
        ? operation.target
        : operation.runner
  const typeArguments =
    operation === undefined || operation._tag === 'ExecutionPark'
      ? runner.typeArguments
      : operation._tag === 'RunEffect'
        ? operation.typeArguments
        : operation.runnerTypeArguments
  const exact =
    declaration === undefined
      ? undefined
      : functions.find((fn) => Mir.matchesInstance(fn, declaration, typeArguments))
  return Object.freeze({
    classification: runner.classification,
    ...(declaration === undefined ? {} : { declaration }),
    ...(exact?.instance === undefined
      ? runner.instance === undefined ||
        declaration === undefined ||
        runner.instance.declaration.module !== declaration.module ||
        runner.instance.declaration.name !== declaration.name
        ? {}
        : { instance: runner.instance }
      : { instance: exact.instance }),
    ...(runner.effectIdentity === undefined ? {} : { effectIdentity: runner.effectIdentity }),
    typeArguments,
    outcome:
      operation === undefined || operation._tag === 'ExecutionPark'
        ? runner.outcome
        : operation.outcomeType.type,
    captures: runner.captures,
    providers: Object.freeze(providers),
  })
}

const completionOf = (completion: ProvisionalMir.CompletionPolicy): Mir.SuspensionCompletion =>
  Object.freeze({ ...completion })

const pathPlanOf = (plan: SuspensionOwnership.ResumePlan): Mir.CoroutineFramePathPlan =>
  Object.freeze({
    restores: plan.restores,
    loanEnds: plan.loanEnds,
    releases: plan.releases,
  })

const descriptorOf = (
  point: Mir.SuspensionPointId,
  runner: Mir.SuspensionRunner,
  plan: SuspensionOwnership.Plan,
): Mir.CoroutineFrameState =>
  Object.freeze({
    _tag: 'CoroutineFrameState',
    point,
    runner,
    outcome: runner.outcome,
    slots: plan.slots,
    success: Object.freeze({
      ...pathPlanOf(plan.success),
      resume: resumeOf(point, 'Success'),
    }),
    failure: Object.freeze({
      ...pathPlanOf(plan.failure),
      resume: resumeOf(point, 'Failure'),
    }),
  })

interface LocatedOperation {
  readonly region: Mir.RegionId
  readonly operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' | 'ExecutionPark' }
  >
}

const operationsOf = (fn: Mir.MirFunction): ReadonlyArray<LocatedOperation> =>
  fn.regions.flatMap((region) => {
    const operations =
      region._tag === 'OperationRegion'
        ? region.operations
        : region._tag === 'CleanupRegion'
          ? region.releases
          : []
    return operations
      .flatMap(Mir.operationTree)
      .flatMap((operation) =>
        operation._tag === 'RunEffect' ||
        operation._tag === 'RunEffectValue' ||
        operation._tag === 'ReifyEffect' ||
        operation._tag === 'ExecutionPark'
          ? [Object.freeze({ region: region.id, operation })]
          : [],
      )
  })

const sameSpan = (
  operation: Mir.Operation,
  control: Extract<ProvisionalMir.Outcome, { readonly _tag: 'RunSuspendableEffect' }>,
): boolean =>
  operation.provenance.span.sourceId === control.span.sourceId &&
  operation.provenance.span.start === control.span.start &&
  operation.provenance.span.end === control.span.end

const regionsOf = (
  program: Mir.Module,
  fn: Mir.MirFunction,
  execution: ProvisionalMir.Execution | undefined,
  ownership: SuspensionOwnership.Module,
  index: DeclarationIndex.Index,
): ReadonlyArray<Mir.SuspensionRegion> => {
  if (execution === undefined) return Object.freeze([])
  const located = operationsOf(fn)
  return Object.freeze(
    execution.regions.flatMap((region): ReadonlyArray<Mir.SuspensionRegion> => {
      if (region.outcome._tag === 'Complete') return []
      const point = pointOf(fn.instance, region.id)
      if (region.outcome._tag === 'SuspendEffect') {
        const outcome = region.outcome
        const candidate = located.find(
          (entry) =>
            entry.operation.provenance.span.sourceId === outcome.span.sourceId &&
            entry.operation.provenance.span.start === outcome.span.start &&
            entry.operation.provenance.span.end === outcome.span.end,
        )
        if (candidate === undefined || candidate.operation._tag === 'ExecutionPark') return []
        const deferred = runnerOf(outcome.deferred, index, candidate.operation, program.functions)
        return [
          Object.freeze({
            _tag: 'SuspendEffectRegion',
            point,
            ownerRegion: candidate.region,
            operation: candidate.operation,
            deferred,
            transfer: Object.freeze({ _tag: 'OriginateTransfer' as const }),
            provenance: Object.freeze({ span: outcome.span, generated: false }),
          }),
        ]
      }
      const outcome = region.outcome
      const candidate = located.find(
        (entry) =>
          sameSpan(entry.operation, outcome) &&
          (outcome.completion._tag === 'Reify'
            ? entry.operation._tag === 'ReifyEffect'
            : entry.operation._tag !== 'ReifyEffect') &&
          (entry.operation._tag === 'ExecutionPark'
            ? Type.equals(outcome.runner.outcome.success, Type.unit)
            : true),
      )
      if (candidate === undefined) return []
      const plan = ownership.plans.find(
        (candidatePlan) =>
          Instances.keyText(candidatePlan.function) === Instances.keyText(fn.instance) &&
          samePoint(candidatePlan.point, region.id),
      )
      const runner = runnerOf(outcome.runner, index, candidate.operation, program.functions)
      return [
        Object.freeze({
          _tag: 'RunSuspendableEffectRegion',
          point,
          ownerRegion: candidate.region,
          operation: candidate.operation,
          runner,
          completion: completionOf(outcome.completion),
          liveLocals: plan?.slots.map((slot) => slot.local) ?? Object.freeze([]),
          complete: Object.freeze({ _tag: 'CompleteInCurrentActivation' }),
          relay: Object.freeze({
            _tag: 'RelayExistingTransfer',
            preserves: outcome.relay.preserves,
            frame: plan?.frame ?? 'MissingOwnershipPlan',
            ...(plan?.frame === 'StatefulRelay'
              ? { state: descriptorOf(point, runner, plan) }
              : {}),
          }),
          provenance: candidate.operation.provenance,
        }),
      ]
    }),
  )
}

/** Produces final MIR. Provisional control and ownership facts are consumed, never embedded. */
export const finalize = (
  program: Mir.Module,
  provisional: ProvisionalMir.Module,
  ownership: SuspensionOwnership.Module,
  index: DeclarationIndex.Index,
): Mir.Module => {
  const functions = Object.freeze(
    program.functions.map((fn) =>
      (() => {
        const execution = ProvisionalMir.executionOf(provisional, fn.instance)
        const classification = ProvisionalMir.classificationOfExecution(provisional, fn.instance)
        const regions = regionsOf(program, fn, execution, ownership, index)
        const states = Object.freeze(
          regions
            .flatMap((region) =>
              region._tag === 'RunSuspendableEffectRegion' && region.relay.state !== undefined
                ? [region.relay.state]
                : [],
            )
            .sort(
              (left, right) =>
                left.point.sourceId.localeCompare(right.point.sourceId) ||
                left.point.spanStart - right.point.spanStart ||
                left.point.ordinal - right.point.ordinal,
            ),
        )
        return regions.length === 0 && (classification === 'Synchronous' || execution === undefined)
          ? fn
          : Object.freeze({
              ...fn,
              suspension: Object.freeze({
                classification,
                regions,
                ...(states.length === 0
                  ? {}
                  : {
                      frame: Object.freeze({
                        _tag: 'CoroutineFrameDescriptor' as const,
                        function: fn.instance,
                        states,
                      }),
                    }),
              }),
            })
      })(),
    ),
  )
  const hasRetainedOrigin = functions.some((fn) =>
    fn.suspension?.regions.some(
      (region) =>
        region._tag === 'SuspendEffectRegion' ||
        (region._tag === 'RunSuspendableEffectRegion' && region.operation._tag === 'ExecutionPark'),
    ),
  )
  if (!hasRetainedOrigin) return program
  return Object.freeze({
    ...program,
    functions,
  })
}

/** Stable inspection summary for focused finalization tests. */
export const summary = (program: Mir.Module): string =>
  program.functions
    .flatMap((fn) =>
      (fn.suspension?.regions ?? []).map(
        (region) =>
          `${Instances.keyText(fn.instance)}:${region._tag}:${region.point.sourceId}:${region.point.spanStart}:${region.point.ordinal}:${region._tag === 'RunSuspendableEffectRegion' ? (region.relay.state?.slots.map((slot) => `${slot.ordinal}:${slot.local.ordinal}:${Type.encode(Mir.semanticType(slot.type))}`).join(',') ?? 'tail') : 'origin'}`,
      ),
    )
    .join('\n')
