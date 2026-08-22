import {
  authored,
  cleanupForLocal,
  concreteCleanup,
  generated,
  propagationLoanEnds,
  propagationReleases,
} from './CleanupEmission.js'
import * as ConformanceProof from './ConformanceProof.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type {} from './EntryAssembly.js'
import type {} from './Forwarding.js'
import { effectRecipe, inlineForwardedRequirement } from './Forwarding.js'
import type { FunctionLowering } from './FunctionLowering.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import type { ProvidedRequirement } from './Lower.js'
import { borrowKey, specializeProvider } from './Lower.js'
import type {} from './LowerExpression.js'
import { lowerExpression } from './LowerExpression.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'
import {
  baseRunnerKey,
  ensureProvidedRunner,
  instanceText,
  providerBindings,
  requirementsFor,
  runtimeRequirementArguments,
} from './ValueType.js'

export const lowerCatchEffectValue = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>,
): LoweredExpression | undefined => {
  const protected_ = lowerExpression(fn, expression.protected)
  const protectedType =
    protected_ === undefined ? undefined : fn.localTypes.at(protected_.result.ordinal)
  const handler = lowerExpression(fn, expression.handler)
  const handlerType = handler === undefined ? undefined : fn.localTypes.at(handler.result.ordinal)
  if (
    protected_ === undefined ||
    protectedType?._tag !== 'EffectValue' ||
    handler === undefined ||
    handlerType?._tag !== 'CallableValue'
  )
    return undefined

  const site = Hir.effectCatchSite(
    fn.owner.function.declaration.id,
    fn.owner.key.declaration,
    expression.span,
  )
  const environment = fn.layout.effectEnvironments.find(
    (
      candidate,
    ): candidate is Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
      candidate._tag === 'EffectEnvironment' &&
      Instances.keyText(candidate.instance) === Instances.keyText(fn.owner.key) &&
      Hir.sameExecutableSite(candidate.site, site),
  )
  if (environment === undefined || environment.fields.length !== 2) return undefined
  const type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }> = Object.freeze({
    _tag: 'EffectValue',
    type: environment.effect,
    site,
    environment,
  })
  const runner = Hir.effectRunnerId(fn.owner.key.declaration, site)
  const specializationKey = baseRunnerKey(fn.owner.key, site)
  const destination = fn.alloc(type)
  fn.emit(
    Object.freeze({
      _tag: 'MakeEffect',
      destination,
      runner,
      runnerTypeArguments: fn.owner.key.typeArguments,
      captures: Object.freeze([
        Object.freeze({
          source: protected_.result,
          access: environment.fields[0]?.access ?? 'Take',
        }),
        Object.freeze({ source: handler.result, access: environment.fields[1]?.access ?? 'Take' }),
      ]),
      type,
      provenance: authored(expression.span),
    }),
  )
  if (!fn.generatedRunners.some((candidate) => candidate.specializationKey === specializationKey))
    fn.generatedRunners.push(
      Object.freeze({
        _tag: 'CatchEffectRunner',
        id: runner,
        owner: fn.owner,
        expression,
        type,
        protectedType,
        handlerType,
        specializationKey,
        providedRequirements: Object.freeze([]),
      }),
    )
  return Object.freeze({ result: destination })
}

export interface LoweredExpression {
  readonly result: Mir.LocalId
}

export interface LoweredPlace {
  readonly root: Mir.LocalId
  readonly selectors: ReadonlyArray<Mir.PlaceSelector>
}

export const lowerRunEffectValue = (
  fn: FunctionLowering,
  effect: Mir.LocalId,
  effectType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  success: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): LoweredExpression | undefined => {
  const successType = fn.type(success)
  if (
    successType === undefined ||
    successType._tag === 'EffectOutcome' ||
    successType._tag === 'EffectValue'
  )
    return undefined
  const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
    _tag: 'EffectOutcome',
    type: effectType.type,
  })
  const outcome = fn.alloc(outcomeType)
  const destination = fn.alloc(successType)
  const propagationType =
    Type.failureMembers(effectType.type).length === 0 || fn.effectOutcome === undefined
      ? undefined
      : fn.type(fn.effectOutcome)
  if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome') return undefined
  const tagMappings = Type.failureMembers(effectType.type).flatMap((failure, sourceOrdinal) => {
    const target =
      propagationType === undefined
        ? undefined
        : Type.failureMembers(propagationType.type).findIndex((candidate) =>
            Type.equals(candidate, failure),
          )
    return target === undefined || target < 0
      ? []
      : [Object.freeze({ source: sourceOrdinal + 1, target: target + 1 })]
  })
  if (tagMappings.length !== Type.failureMembers(effectType.type).length) return undefined
  const propagationShape =
    propagationType === undefined ? undefined : Layout.callingShape(fn.layout, propagationType.type)
  const provided = requirementsFor(availableRequirements, effectType.type)
  const providedRunner =
    provided === undefined || provided.length === 0
      ? undefined
      : ensureProvidedRunner(fn, effectType, provided)
  if (provided !== undefined && provided.length > 0 && providedRunner === undefined)
    return undefined
  const baseRunner =
    effectType.storage?.realization.runner ??
    Hir.effectRunnerId(effectType.environment.instance.declaration, effectType.site)
  const baseRunnerTypeArguments =
    effectType.storage?.realization.runnerArguments ?? effectType.environment.instance.typeArguments
  const failureEnds = propagationLoanEnds(fn, span)
  const releases = propagationReleases(fn, span)
  fn.emit(
    Object.freeze({
      _tag: 'RunEffectValue',
      destination,
      outcome,
      effect,
      runner: providedRunner ?? baseRunner,
      runnerTypeArguments: baseRunnerTypeArguments,
      ...(providedRunner === undefined
        ? {}
        : {
            runnerBase: Object.freeze({
              declaration: baseRunner,
              typeArguments: baseRunnerTypeArguments,
            }),
          }),
      providers: providerBindings(provided),
      arguments: runtimeRequirementArguments(provided),
      outcomeType,
      ...(propagationType === undefined ? {} : { propagationType }),
      tagMappings: Object.freeze(tagMappings),
      propagationLaneCount: propagationShape?.laneCount ?? 0,
      ...(Type.failureMembers(effectType.type).length === 0 || failureEnds.length === 0
        ? {}
        : { failureLoanEnds: failureEnds }),
      ...(propagationType === undefined || releases.length === 0 ? {} : { releases }),
      type: successType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({ result: destination })
}

export const lowerRunEffectComposite = (
  fn: FunctionLowering,
  effect: Mir.LocalId,
  effectType: Extract<Mir.Type, { readonly _tag: 'EffectComposite' }>,
  success: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): LoweredExpression | undefined => {
  const successType = fn.type(success)
  if (
    successType === undefined ||
    successType._tag === 'EffectOutcome' ||
    successType._tag === 'EffectValue' ||
    successType._tag === 'EffectComposite'
  )
    return undefined
  const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
    _tag: 'EffectOutcome',
    type: effectType.contract,
  })
  const outcome = fn.alloc(outcomeType)
  const destination = fn.alloc(successType)
  const propagationType =
    Type.failureMembers(effectType.contract).length === 0 || fn.effectOutcome === undefined
      ? undefined
      : fn.type(fn.effectOutcome)
  if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome') return undefined
  const alternatives = effectType.alternatives.flatMap((alternative) => {
    const provided = requirementsFor(availableRequirements, alternative.type)
    if (provided === undefined) return []
    const providedRunner =
      provided.length === 0 ? undefined : ensureProvidedRunner(fn, alternative, provided)
    if (provided.length > 0 && providedRunner === undefined) return []
    const runner =
      providedRunner ??
      alternative.storage?.realization.runner ??
      Hir.effectRunnerId(alternative.environment.instance.declaration, alternative.site)
    const runnerTypeArguments =
      alternative.storage?.realization.runnerArguments ??
      alternative.environment.instance.typeArguments
    const tagMappings = Type.failureMembers(alternative.type).flatMap((failure, sourceOrdinal) => {
      const target = Type.failureMembers(effectType.contract).findIndex((candidate) =>
        Type.equals(candidate, failure),
      )
      return target < 0 ? [] : [Object.freeze({ source: sourceOrdinal + 1, target: target + 1 })]
    })
    return tagMappings.length !== Type.failureMembers(alternative.type).length
      ? []
      : [
          Object.freeze({
            type: alternative,
            runner,
            runnerTypeArguments,
            tagMappings,
            arguments: runtimeRequirementArguments(provided),
          }),
        ]
  })
  if (alternatives.length !== effectType.alternatives.length) return undefined
  const outerMappings = Type.failureMembers(effectType.contract).flatMap(
    (failure, sourceOrdinal) => {
      const target =
        propagationType === undefined
          ? undefined
          : Type.failureMembers(propagationType.type).findIndex((candidate) =>
              Type.equals(candidate, failure),
            )
      return target === undefined || target < 0
        ? []
        : [Object.freeze({ source: sourceOrdinal + 1, target: target + 1 })]
    },
  )
  if (outerMappings.length !== Type.failureMembers(effectType.contract).length) return undefined
  const propagationShape =
    propagationType === undefined ? undefined : Layout.callingShape(fn.layout, propagationType.type)
  const arguments_ = Object.freeze(
    [
      ...new Map(
        alternatives
          .flatMap((alternative) => alternative.arguments)
          .map((argument) => [argument.ordinal, argument] as const),
      ).values(),
    ].sort((left, right) => left.ordinal - right.ordinal),
  )
  fn.emit(
    Object.freeze({
      _tag: 'RunEffectComposite',
      destination,
      outcome,
      effect,
      alternatives: Object.freeze(alternatives),
      arguments: arguments_,
      outcomeType,
      ...(propagationType === undefined ? {} : { propagationType }),
      tagMappings: Object.freeze(outerMappings),
      propagationLaneCount: propagationShape?.laneCount ?? 0,
      ...(Type.failureMembers(effectType.contract).length === 0
        ? {}
        : { failureLoanEnds: propagationLoanEnds(fn, span) }),
      ...(propagationType === undefined ? {} : { releases: propagationReleases(fn, span) }),
      type: successType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({ result: destination })
}

export interface ReifiedEffect {
  readonly result: Mir.LocalId
  readonly resultType: Extract<Mir.Type, { readonly _tag: 'Nominal' }>
  readonly resultField: DeclarationFacts.FieldId
  readonly resultUnion: Type.StructuralUnion
  readonly successType: Type.Nominal
  readonly successField: DeclarationFacts.FieldId
  readonly failureType: Type.Nominal
  readonly failureField: DeclarationFacts.FieldId
  readonly failureValueType: Type.Type
}

export const reifyEffectValue = (
  fn: FunctionLowering,
  effect: Mir.LocalId,
  effectType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  result: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): ReifiedEffect | undefined => {
  const provided = requirementsFor(availableRequirements, effectType.type)
  if (provided === undefined) return undefined
  const runner =
    provided.length === 0
      ? (effectType.storage?.realization.runner ??
        Hir.effectRunnerId(effectType.environment.instance.declaration, effectType.site))
      : ensureProvidedRunner(fn, effectType, provided)
  if (runner === undefined) return undefined
  const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
    _tag: 'EffectOutcome',
    type: effectType.type,
  })
  const resultType = fn.type(result)
  const failureValueType = Type.failureValue(Type.failureMembers(effectType.type))
  const successType = Type.resultSuccess(effectType.type.success)
  const failureType = Type.resultFailure(failureValueType)
  const resultUnionNormalization = Type.union([successType, failureType])
  const resultUnion =
    resultUnionNormalization._tag === 'Normalized' && Type.isUnion(resultUnionNormalization.type)
      ? resultUnionNormalization.type
      : undefined
  const resultEntry =
    resultType?._tag === 'Nominal' ? Layout.entry(fn.layout, resultType.type) : undefined
  const successEntry = Layout.entry(fn.layout, successType)
  const failureEntry = Layout.entry(fn.layout, failureType)
  const resultField =
    resultEntry?.representation._tag === 'Aggregate'
      ? resultEntry.representation.fields.at(0)?.id
      : undefined
  const successField =
    successEntry?.representation._tag === 'Aggregate'
      ? successEntry.representation.fields.at(0)?.id
      : undefined
  const failureField =
    failureEntry?.representation._tag === 'Aggregate'
      ? failureEntry.representation.fields.at(0)?.id
      : undefined
  const resultShape =
    resultType?._tag === 'Nominal' ? Layout.callingShape(fn.layout, resultType.type) : undefined
  const outcomeShape = Layout.callingShape(fn.layout, effectType.type)
  const failureValueShape = Layout.callingShape(fn.layout, failureValueType)
  const successTag = resultUnion?.members.findIndex((member) => Type.equals(member, successType))
  const failureTag = resultUnion?.members.findIndex((member) => Type.equals(member, failureType))
  if (
    resultType?._tag !== 'Nominal' ||
    resultUnion === undefined ||
    resultField === undefined ||
    successField === undefined ||
    failureField === undefined ||
    resultShape === undefined ||
    outcomeShape === undefined ||
    failureValueShape === undefined ||
    successTag === undefined ||
    successTag < 0 ||
    failureTag === undefined ||
    failureTag < 0
  )
    return undefined
  const outcome = fn.alloc(outcomeType)
  const destination = fn.alloc(resultType)
  fn.emit(
    Object.freeze({
      _tag: 'ReifyEffect' as const,
      destination,
      outcome,
      effect,
      runner,
      runnerTypeArguments:
        effectType.storage?.realization.runnerArguments ??
        effectType.environment.instance.typeArguments,
      arguments: runtimeRequirementArguments(provided),
      outcomeType,
      resultType,
      resultField,
      resultUnion,
      successType,
      successField,
      successTag,
      failureType,
      failureField,
      failureTag,
      failureValueType,
      resultShape,
      outcomeShape,
      failureValueShape,
      type: resultType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({
    result: destination,
    resultType,
    resultField,
    resultUnion,
    successType,
    successField,
    failureType,
    failureField,
    failureValueType,
  })
}

export const callableEffectResult = (
  fn: FunctionLowering,
  callable: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  if (callable.target._tag !== 'DeclarationCallableTarget') return undefined
  const typeArguments =
    callable.environment?.callable.typeArguments ??
    callable.storage?.realization.targetArguments ??
    Object.freeze([])
  const result = fn.effectResults.get(instanceText(callable.target.declaration, typeArguments))
  return result?._tag === 'EffectValue' ? result : undefined
}

export const lowerEffectCatch = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>,
  runSpan: SourceSpan.SourceSpan,
  captured?: {
    readonly protected: Mir.LocalId
    readonly protectedType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
    readonly handler: Mir.LocalId
    readonly handlerType: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>
  },
): LoweredExpression | undefined => {
  if (expression.protected._tag === 'Unavailable') return undefined
  const protected_ =
    captured === undefined
      ? expression.protected._tag === 'ServiceEffectConstruct'
        ? lowerServiceEffectValue(fn, expression.protected, fn.providedRequirements)
        : lowerExpression(fn, expression.protected)
      : Object.freeze({ result: captured.protected })
  const protectedType =
    captured?.protectedType ??
    (protected_ === undefined ? undefined : fn.localTypes.at(protected_.result.ordinal))
  if (protected_ === undefined || protectedType?._tag !== 'EffectValue') return undefined

  // Both operands are formed before the protected Effect starts, matching ordinary call
  // evaluation even though the handler is invoked only on the selected failure path.
  const handler =
    captured === undefined
      ? lowerExpression(fn, expression.handler)
      : Object.freeze({ result: captured.handler })
  const handlerType =
    captured?.handlerType ??
    (handler === undefined ? undefined : fn.localTypes.at(handler.result.ordinal))
  const handlerEffectType =
    handlerType?._tag === 'CallableValue' ? callableEffectResult(fn, handlerType) : undefined
  if (
    handler === undefined ||
    handlerType?._tag !== 'CallableValue' ||
    handlerEffectType === undefined
  )
    return undefined
  const unusedHandlerDrop = (): ReadonlyArray<Mir.DropOperation> => {
    const cleanup = cleanupForLocal(
      fn,
      concreteCleanup(fn, Mir.semanticType(handlerType)),
      handlerType,
    )
    return cleanup._tag === 'NoCleanup'
      ? Object.freeze([])
      : Object.freeze([
          Object.freeze({
            _tag: 'Drop' as const,
            local: handler.result,
            cleanup,
            provenance: generated(runSpan),
          }),
        ])
  }

  const selected = fn.semantic(expression.selected)
  const protectedEffect = fn.semantic(expression.protected.type)
  const resultEffect = fn.semantic(expression.type)
  if (Type.isNever(selected) || !Type.isEffect(protectedEffect) || !Type.isEffect(resultEffect))
    return undefined
  const protectedFailures = Type.failureMembers(protectedEffect)
  const selectedMembers: ReadonlyArray<Type.Type> = Type.isUnion(selected)
    ? selected.members
    : Object.freeze([selected])
  if (
    selectedMembers.some(
      (member) => !protectedFailures.some((failure) => Type.equals(failure, member)),
    )
  )
    return undefined

  const reified = reifyEffectValue(
    fn,
    protected_.result,
    protectedType,
    Type.result(protectedEffect.success, Type.failureValue(protectedFailures)),
    expression.span,
  )
  if (reified === undefined) return undefined
  const resultUnionType: Extract<Mir.Type, { readonly _tag: 'Union' }> = Object.freeze({
    _tag: 'Union',
    type: reified.resultUnion,
  })
  const resultUnion = fn.alloc(resultUnionType)
  fn.emit(
    Object.freeze({
      _tag: 'Project' as const,
      destination: resultUnion,
      source: reified.result,
      field: reified.resultField,
      type: resultUnionType,
      provenance: generated(expression.span),
    }),
  )

  const successType = fn.type(resultEffect.success)
  const resultUnionShape = Layout.callingShape(fn.layout, reified.resultUnion)
  const successShape = Layout.callingShape(fn.layout, resultEffect.success)
  const failureValueMir = fn.type(reified.failureValueType)
  const propagationEffect = fn.effectOutcome
  const propagationType = propagationEffect === undefined ? undefined : fn.type(propagationEffect)
  const propagationShape =
    propagationEffect === undefined ? undefined : Layout.callingShape(fn.layout, propagationEffect)
  if (
    successType === undefined ||
    successType._tag === 'EffectOutcome' ||
    resultUnionShape === undefined ||
    successShape === undefined ||
    (failureValueMir?._tag !== 'Nominal' && failureValueMir?._tag !== 'Union') ||
    propagationEffect === undefined ||
    propagationType?._tag !== 'EffectOutcome' ||
    propagationShape === undefined
  )
    return undefined

  const declaration = fn.owner.function.declaration.id
  const outerMatch: Match.MatchId = Object.freeze({
    _tag: 'MatchId',
    function: declaration,
    span: expression.span,
  })
  const successArm: Match.ArmId = Object.freeze({
    _tag: 'MatchArmId',
    match: outerMatch,
    ordinal: 0,
  })
  const failureArm: Match.ArmId = Object.freeze({
    _tag: 'MatchArmId',
    match: outerMatch,
    ordinal: 1,
  })
  const successBinding: Match.BindingId = Object.freeze({
    _tag: 'PatternBindingId',
    arm: successArm,
    ordinal: 0,
  })
  const failureBinding: Match.BindingId = Object.freeze({
    _tag: 'PatternBindingId',
    arm: failureArm,
    ordinal: 0,
  })
  const success = fn.alloc(successType)
  const failure = fn.alloc(failureValueMir)

  const failureMembers =
    failureValueMir._tag === 'Nominal'
      ? Object.freeze([failureValueMir.type])
      : failureValueMir.type.members
  const innerSpan =
    SourceSpan.fromOffsets(
      expression.span.sourceId,
      expression.span.start,
      expression.span.start,
    ) ?? expression.span
  const innerMatch: Match.MatchId = Object.freeze({
    _tag: 'MatchId',
    function: declaration,
    span: innerSpan,
  })
  const innerArms: Array<Mir.MatchArm> = []
  for (const [ordinal, member] of failureMembers.entries()) {
    const armId: Match.ArmId = Object.freeze({
      _tag: 'MatchArmId',
      match: innerMatch,
      ordinal,
    })
    const bindingId: Match.BindingId = Object.freeze({
      _tag: 'PatternBindingId',
      arm: armId,
      ordinal: 0,
    })
    const memberType = fn.type(member)
    if (memberType === undefined || memberType._tag === 'EffectOutcome') return undefined
    const bound = fn.alloc(memberType)
    const [selectedResult, selectedOperations] = fn.capture(() => {
      if (selectedMembers.some((candidate) => Type.equals(candidate, member))) {
        let handlerArgument = bound
        if (!Type.equals(member, selected)) {
          const selectedType = fn.type(selected)
          const sourceShape = Layout.callingShape(fn.layout, member)
          const targetShape = Layout.callingShape(fn.layout, selected)
          const conversion = TypeCompatibility.check(member, selected)
          if (
            selectedType?._tag !== 'Union' ||
            sourceShape === undefined ||
            targetShape === undefined ||
            conversion._tag !== 'Inject'
          )
            return undefined
          handlerArgument = fn.alloc(selectedType)
          fn.emit(
            Object.freeze({
              _tag: 'ConvertUnion' as const,
              destination: handlerArgument,
              source: bound,
              sourceType: memberType,
              targetType: selectedType,
              conversion: 'Inject' as const,
              mappings: conversion.mappings,
              sourceShape,
              targetShape,
              access: 'Owned' as const,
              provenance: generated(expression.span),
            }),
          )
        }
        const applied = fn.alloc(handlerEffectType)
        fn.emit(
          Object.freeze({
            _tag: 'ApplyCallable' as const,
            destination: applied,
            callable: handler.result,
            typeArguments:
              handlerType.environment?.callable.typeArguments ??
              handlerType.storage?.realization.targetArguments ??
              Object.freeze([]),
            captures: Object.freeze([]),
            arguments: Object.freeze([handlerArgument]),
            callableType: handlerType.type,
            access: handlerType.type.mode,
            evaluation: 'CalleeThenArguments' as const,
            realization: 'Environment' as const,
            type: handlerEffectType,
            provenance: generated(expression.span),
          }),
        )
        return lowerRunEffectValue(fn, applied, handlerEffectType, resultEffect.success, runSpan)
      }
      const target = Type.failureMembers(propagationEffect).findIndex((candidate) =>
        Type.equals(candidate, member),
      )
      const bottom = fn.type('never')
      if (target < 0 || bottom?._tag !== 'Bottom') return undefined
      const destination = fn.alloc(bottom)
      for (const drop of unusedHandlerDrop()) fn.emit(drop)
      const releases = propagationReleases(fn, runSpan)
      fn.emit(
        Object.freeze({
          _tag: 'PropagateEffectFailure' as const,
          source: bound,
          sourceType: memberType,
          propagationType,
          tagMappings: Object.freeze([Object.freeze({ source: 0, target: target + 1 })]),
          propagationLaneCount: propagationShape.laneCount,
          ...(releases.length === 0 ? {} : { releases }),
          type: bottom,
          provenance: generated(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    })
    if (selectedResult === undefined) return undefined
    innerArms.push(
      Object.freeze({
        id: armId,
        member,
        universal: false,
        before: Object.freeze(failureMembers.slice(ordinal)),
        after: Object.freeze(failureMembers.slice(ordinal + 1)),
        bindings: Object.freeze([
          Object.freeze({
            id: bindingId,
            destination: bound,
            path: Object.freeze([]),
            type: memberType,
            access: 'Move' as const,
            provenance: generated(expression.span),
          }),
        ]),
        selected: Object.freeze({
          access: 'Move' as const,
          operations: selectedOperations,
          result: selectedResult.result,
          cleanup: Object.freeze([]),
          endBorrow: false,
        }),
        provenance: generated(expression.span),
      }),
    )
  }
  const innerResult = fn.alloc(successType)
  const innerOperation: Mir.MatchOperation = Object.freeze({
    _tag: 'Match',
    id: innerMatch,
    destination: innerResult,
    scrutinee: failure,
    scrutineeType: failureValueMir,
    scrutineeShape: Layout.callingShape(fn.layout, reified.failureValueType) ?? resultUnionShape,
    access: 'Move',
    retainsBindings: false,
    members: failureMembers,
    decisions: Object.freeze(
      failureMembers.map((member, ordinal) =>
        Object.freeze({
          member,
          candidates: Object.freeze([innerArms.at(ordinal)?.id].flatMap((id) => id ?? [])),
        }),
      ),
    ),
    arms: Object.freeze(innerArms),
    type: successType,
    resultShape: successShape,
    provenance: generated(expression.span),
  })
  const destination = fn.alloc(successType)
  fn.emit(
    Object.freeze({
      _tag: 'Match' as const,
      id: outerMatch,
      destination,
      scrutinee: resultUnion,
      scrutineeType: resultUnionType,
      scrutineeShape: resultUnionShape,
      access: 'Move' as const,
      retainsBindings: false,
      members: reified.resultUnion.members,
      decisions: Object.freeze(
        reified.resultUnion.members.map((member) =>
          Object.freeze({
            member,
            candidates: Object.freeze([
              Type.equals(member, reified.successType) ? successArm : failureArm,
            ]),
          }),
        ),
      ),
      arms: Object.freeze([
        Object.freeze({
          id: successArm,
          member: reified.successType,
          universal: false,
          before: reified.resultUnion.members,
          after: Object.freeze([reified.failureType]),
          bindings: Object.freeze([
            Object.freeze({
              id: successBinding,
              destination: success,
              path: Object.freeze([reified.successField]),
              type: successType,
              access: 'Move' as const,
              provenance: generated(expression.span),
            }),
          ]),
          selected: Object.freeze({
            access: 'Move' as const,
            operations: unusedHandlerDrop(),
            result: success,
            cleanup: Object.freeze([]),
            endBorrow: false,
          }),
          provenance: generated(expression.span),
        }),
        Object.freeze({
          id: failureArm,
          member: reified.failureType,
          universal: false,
          before: Object.freeze([reified.failureType]),
          after: Object.freeze([]),
          bindings: Object.freeze([
            Object.freeze({
              id: failureBinding,
              destination: failure,
              path: Object.freeze([reified.failureField]),
              type: failureValueMir,
              access: 'Move' as const,
              provenance: generated(expression.span),
            }),
          ]),
          selected: Object.freeze({
            access: 'Move' as const,
            operations: Object.freeze([innerOperation]),
            result: innerResult,
            cleanup: Object.freeze([]),
            endBorrow: false,
          }),
          provenance: generated(expression.span),
        }),
      ]),
      type: successType,
      resultShape: successShape,
      provenance: generated(expression.span),
    }),
  )
  endRunLoans(fn, runSpan)
  return Object.freeze({ result: destination })
}

export const lowerPlacePath = (
  fn: FunctionLowering,
  expression: Hir.Expression,
): LoweredPlace | undefined => {
  if (expression._tag === 'Project') {
    const subject = lowerPlacePath(fn, expression.subject)
    if (subject === undefined) return undefined
    return Object.freeze({
      root: subject.root,
      selectors: Object.freeze([
        ...subject.selectors,
        Object.freeze({
          _tag: 'FieldSelector' as const,
          field: expression.field,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      ]),
    })
  }
  if (expression._tag === 'IndexPlace') {
    const subject = lowerPlacePath(fn, expression.subject)
    if (subject === undefined) return undefined
    const index:
      | Extract<Mir.PlaceSelector, { readonly _tag: 'ElementSelector' }>['index']
      | undefined =
      expression.bounds._tag === 'Proven'
        ? Object.freeze({ _tag: 'Proven', value: expression.bounds.index })
        : (() => {
            const lowered = lowerExpression(fn, expression.index)
            return lowered === undefined
              ? undefined
              : Object.freeze({ _tag: 'Runtime' as const, local: lowered.result })
          })()
    if (index === undefined) return undefined
    return Object.freeze({
      root: subject.root,
      selectors: Object.freeze([
        ...subject.selectors,
        Object.freeze({
          _tag: 'ElementSelector' as const,
          length: expression.array.length,
          index,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      ]),
    })
  }
  if (expression._tag === 'SliceIndexPlace') {
    const subject = lowerPlacePath(fn, expression.slice)
    const index = lowerExpression(fn, expression.index)
    if (subject === undefined || index === undefined) return undefined
    return Object.freeze({
      root: subject.root,
      selectors: Object.freeze([
        ...subject.selectors,
        Object.freeze({
          _tag: 'SliceElementSelector',
          index: index.result,
          access: expression.access,
          provenance: authored(expression.span),
        }),
      ]),
    })
  }
  const root = lowerExpression(fn, expression)
  return root === undefined
    ? undefined
    : Object.freeze({ root: root.result, selectors: Object.freeze([]) })
}

export const lowerPlace = (
  fn: FunctionLowering,
  expression: Extract<
    Hir.Expression,
    { readonly _tag: 'Project' | 'IndexPlace' | 'SliceIndexPlace' }
  >,
): LoweredExpression | undefined => {
  const place = lowerPlacePath(fn, expression)
  const type = fn.type(expression.type)
  if (place === undefined || type === undefined) return undefined
  const destination = fn.alloc(type)
  fn.emit(
    Object.freeze({
      _tag: 'ReadPlace',
      destination,
      root: place.root,
      selectors: place.selectors,
      type,
      ...((type._tag === 'CallableValue' &&
        type.storage !== undefined &&
        type.type.mode === 'Take') ||
      (type._tag === 'EffectValue' && type.storage !== undefined && type.type.access === 'Take')
        ? { consume: true as const }
        : {}),
      provenance: Object.freeze({ span: expression.span, generated: false }),
    }),
  )
  return Object.freeze({ result: destination })
}

export const endLoans = (
  fn: FunctionLowering,
  loans: ReadonlyArray<Hir.BorrowId>,
  span: SourceSpan.SourceSpan,
): void => {
  for (const authored of loans) {
    const borrow = fn.recipeBorrow(authored)
    endLoan(fn, borrow, span)
  }
}

/** Ends one authored or synthetic loan exactly once. */
export const endLoan = (
  fn: FunctionLowering,
  borrow: Hir.BorrowId,
  span: SourceSpan.SourceSpan,
): void => {
  const key = borrowKey(borrow)
  const held = fn.loanLocals.get(key)
  if (held === undefined) return
  fn.emit(
    Object.freeze({
      _tag: 'EndLoan',
      borrow,
      slice: held,
      provenance: generated(span),
    }),
  )
  fn.loanLocals.delete(key)
}

export const endRunLoans = (fn: FunctionLowering, span: SourceSpan.SourceSpan): void => {
  // A constructed effect holds its argument borrows until the run that consumes it: ownership
  // records that end site, so every lowering path for run must release them here.
  for (const loan of fn.ownership?.loans ?? []) {
    if (
      loan.origin !== 'EffectCapture' &&
      loan.origin !== 'CallableCapture' &&
      loan.origin !== 'ValueBorrow' &&
      loan.origin !== 'InterfaceOperand'
    )
      continue
    if (loan.endSpan.sourceId !== span.sourceId || loan.endSpan.end > span.end) {
      continue
    }
    endLoans(fn, [loan.id], span)
  }
}

export const dropOwnedProvider = (
  fn: FunctionLowering,
  local_: Mir.LocalId,
  type: Type.Nominal,
  span: SourceSpan.SourceSpan,
): void => {
  const cleanup = concreteCleanup(fn, type)
  if (cleanup._tag === 'NoCleanup') return
  fn.emit(
    Object.freeze({
      _tag: 'Drop',
      local: local_,
      cleanup,
      provenance: generated(span),
    }),
  )
}

export const endReturnedViewLoans = (fn: FunctionLowering, span: SourceSpan.SourceSpan): void => {
  for (const loan of fn.ownership?.loans ?? []) {
    if (loan.origin !== 'ReturnedView') continue
    if (
      loan.endSpan.sourceId !== span.sourceId ||
      loan.endSpan.start < span.start ||
      loan.endSpan.end > span.end
    ) {
      continue
    }
    endLoans(fn, [loan.id], span)
  }
}

export const retainedEffectLoans = (
  fn: FunctionLowering,
  expression: Hir.Expression,
): ReadonlyArray<Hir.BorrowId> => {
  const retained = new Map<string, Hir.BorrowId>()
  for (const child of Hir.expressionTree(expression)) {
    if (child._tag === 'BindingReference') {
      for (const borrow of fn.effectLoanEnds.get(child.binding.ordinal) ?? [])
        retained.set(borrowKey(borrow), borrow)
    }
    if (child._tag === 'SliceBorrow' || child._tag === 'ValueBorrow') {
      retained.set(borrowKey(child.borrow), child.borrow)
      continue
    }
    if (
      (child._tag === 'BuiltinCall' || child._tag === 'BoundOperationCall') &&
      child.witnessEffectSite !== undefined
    ) {
      for (const loan of fn.ownership?.loans ?? []) {
        if (
          loan.origin === 'InterfaceOperand' &&
          loan.id.callSpan.sourceId === child.span.sourceId &&
          loan.id.callSpan.start === child.span.start &&
          loan.id.callSpan.end === child.span.end
        )
          retained.set(borrowKey(loan.id), loan.id)
      }
    }
    if (child._tag !== 'CallableSection') continue
    for (const capture of child.captures) {
      if (capture.value._tag !== 'SliceBorrow' && capture.value._tag !== 'ValueBorrow') continue
      retained.set(borrowKey(capture.value.borrow), capture.value.borrow)
    }
  }
  return Object.freeze([...retained.values()])
}

export const borrowedWriteRoot = (
  fn: FunctionLowering,
  root: Hir.BorrowedWritePlace['root'],
): Mir.LocalId | undefined =>
  root._tag === 'ParameterSliceRoot'
    ? fn.parameterLocals.get(root.parameter.ordinal)
    : fn.bindingLocals.get(root.binding.ordinal)

export const lowerServiceEffectValue = (
  fn: FunctionLowering,
  subject: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>,
  availableRequirements: ReadonlyArray<ProvidedRequirement>,
): LoweredExpression | undefined => {
  const provided = availableRequirements.find(
    (requirement) =>
      requirement.role === subject.role &&
      Type.equals(requirement.capability, subject.service) &&
      (subject.access === 'Shared' ||
        requirement.access === 'Exclusive' ||
        requirement.access === 'Take'),
  )
  if (provided?.witness._tag !== 'SourceConformanceWitness' || provided.local === undefined)
    return undefined
  const target = ConformanceProof.witnessOperation(provided.witness, subject.operation)
  if (target === undefined) return undefined
  const loweredArguments = subject.arguments.map((argument) => lowerExpression(fn, argument))
  if (loweredArguments.some((argument) => argument === undefined)) return undefined
  const typeArguments = provided.witness.typeArguments
  const effectResult = fn.effectResults.get(instanceText(target, typeArguments))
  if (effectResult === undefined) return undefined
  const effect = fn.alloc(effectResult)
  fn.emit(
    Object.freeze({
      _tag: 'Call',
      destination: effect,
      target,
      typeArguments,
      arguments: Object.freeze([
        provided.local,
        ...loweredArguments.flatMap((argument) =>
          argument === undefined ? [] : [argument.result],
        ),
      ]),
      type: effectResult,
      provenance: authored(subject.span),
    }),
  )
  return Object.freeze({ result: effect })
}

interface LoweredProvidedEffect {
  readonly requirement: ProvidedRequirement
  readonly ownedProvider?: Mir.LocalId
  readonly loan?: Hir.BorrowId
}

const prepareProvidedEffect = (
  fn: FunctionLowering,
  providerFact: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>['provider'],
): LoweredProvidedEffect | undefined => {
  const selected = specializeProvider(fn, providerFact)
  if (selected === undefined) return undefined
  const provider =
    providerFact.binding !== undefined
      ? fn.bindingLocals.get(providerFact.binding.ordinal)
      : providerFact.parameter !== undefined
        ? fn.parameterLocals.get(providerFact.parameter.ordinal)
        : undefined
  const ownedProvider = providerFact.selectionAccess === 'Take' ? provider : undefined
  if (selected.witness._tag !== 'SourceConformanceWitness')
    return Object.freeze({
      requirement: selected,
      ...(ownedProvider === undefined ? {} : { ownedProvider }),
    })

  const access =
    providerFact.selectionAccess === 'Take' ? ('Exclusive' as const) : providerFact.selectionAccess
  const forwardedType =
    providerFact.parameter === undefined || provider === undefined
      ? undefined
      : fn.localTypes.at(provider.ordinal)
  if (
    provider !== undefined &&
    forwardedType?._tag === 'Reference' &&
    forwardedType.type.access === access &&
    Type.equals(forwardedType.type.target, selected.providerType)
  )
    return Object.freeze({
      requirement: Object.freeze({ ...selected, local: provider }),
      ...(ownedProvider === undefined ? {} : { ownedProvider }),
    })

  const providerType = fn.type(selected.providerType)
  const referenceType = fn.type(Type.reference(access, selected.providerType))
  const authoredLoan = fn.ownership?.loans.find(
    (candidate) =>
      (candidate.origin === 'EffectCapture' ||
        candidate.origin === 'CallableCapture' ||
        candidate.origin === 'ValueBorrow') &&
      candidate.access === access &&
      candidate.startSpan.start === providerFact.span.start &&
      candidate.startSpan.end === providerFact.span.end,
  )
  const borrow = fn.beginRecipeBorrow(
    authoredLoan?.id ?? fn.freshSyntheticBorrow(providerFact.span),
  )
  if (
    provider === undefined ||
    providerType?._tag !== 'Nominal' ||
    referenceType?._tag !== 'Reference' ||
    borrow === undefined
  )
    return undefined
  const reference = fn.alloc(referenceType)
  fn.emit(
    Object.freeze({
      _tag: 'BeginLoan',
      borrow,
      destination: reference,
      root: provider,
      selectors: Object.freeze([]),
      sourceType: providerType,
      type: referenceType,
      access,
      reborrow: false,
      suspendsParent: false,
      provenance: authored(providerFact.span),
    }),
  )
  fn.loanLocals.set(borrowKey(borrow), reference)
  return Object.freeze({
    requirement: Object.freeze({ ...selected, local: reference }),
    loan: borrow,
    ...(ownedProvider === undefined ? {} : { ownedProvider }),
  })
}

/**
 * Brackets one provided requirement for reification or immediate execution. The actor that begins
 * a provider loan also ends it, removes its tracking entry, and drops a taken provider after the
 * protected lowering has finished.
 */
export const lowerProvidedEffect = <A>(
  fn: FunctionLowering,
  providerFact: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>['provider'],
  use: (requirement: ProvidedRequirement) => A | undefined,
): A | undefined => {
  const provided = prepareProvidedEffect(fn, providerFact)
  if (provided === undefined) return undefined
  const result = use(provided.requirement)
  if (result === undefined) return undefined
  if (provided.loan !== undefined) endLoans(fn, [provided.loan], providerFact.span)
  if (provided.ownedProvider !== undefined)
    dropOwnedProvider(
      fn,
      provided.ownedProvider,
      provided.requirement.providerType,
      providerFact.span,
    )
  return result
}

export const lowerReifiedEffectRecipe = (
  fn: FunctionLowering,
  subject: Hir.Expression,
  resultType: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): ReifiedEffect | undefined => {
  const recipe = effectRecipe(fn, subject)
  const forwarded = inlineForwardedRequirement(fn, recipe)
  if (forwarded !== undefined) {
    const provider = lowerExpression(fn, forwarded.provider)
    if (provider === undefined) return undefined
    const providerBorrow =
      forwarded.provider._tag === 'ValueBorrow'
        ? fn.recipeBorrow(forwarded.provider.borrow)
        : undefined
    let runtimeProvider = provider.result
    let ownedLoan: { readonly borrow: Hir.BorrowId; readonly slice: Mir.LocalId } | undefined
    if (
      forwarded.selection.access === 'Take' &&
      forwarded.selection.witness._tag === 'SourceConformanceWitness'
    ) {
      const providerType = fn.type(forwarded.selection.providerType)
      const referenceType = fn.type(Type.reference('Exclusive', forwarded.selection.providerType))
      if (providerType?._tag !== 'Nominal' || referenceType?._tag !== 'Reference') return undefined
      const borrow = fn.freshSyntheticBorrow(forwarded.provider.span)
      const reference = fn.alloc(referenceType)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow,
          destination: reference,
          root: provider.result,
          selectors: Object.freeze([]),
          sourceType: providerType,
          type: referenceType,
          access: 'Exclusive',
          reborrow: false,
          suspendsParent: false,
          provenance: authored(forwarded.provider.span),
        }),
      )
      fn.loanLocals.set(borrowKey(borrow), reference)
      runtimeProvider = reference
      ownedLoan = Object.freeze({ borrow, slice: reference })
    }
    const reified = lowerReifiedEffectRecipe(
      fn,
      forwarded.binding.protected,
      resultType,
      span,
      Object.freeze([
        ...availableRequirements,
        Object.freeze({ ...forwarded.selection, local: runtimeProvider }),
      ]),
    )
    if (reified === undefined) return undefined
    endRunLoans(fn, span)
    if (providerBorrow !== undefined) endLoans(fn, [providerBorrow], span)
    if (ownedLoan !== undefined) endLoan(fn, ownedLoan.borrow, forwarded.provider.span)
    if (forwarded.selection.access === 'Take')
      dropOwnedProvider(
        fn,
        provider.result,
        forwarded.selection.providerType,
        forwarded.provider.span,
      )
    return reified
  }

  if (recipe._tag === 'EffectBindRequirement') {
    return lowerProvidedEffect(fn, recipe.provider, (requirement) => {
      const reified = lowerReifiedEffectRecipe(
        fn,
        recipe.protected,
        resultType,
        span,
        Object.freeze([...availableRequirements, requirement]),
      )
      if (reified === undefined) return undefined
      endRunLoans(fn, span)
      return reified
    })
  }
  const lowered =
    recipe._tag === 'ServiceEffectConstruct'
      ? lowerServiceEffectValue(fn, recipe, availableRequirements)
      : lowerExpression(fn, recipe)
  const effectType = lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
  if (lowered === undefined || effectType?._tag !== 'EffectValue') return undefined
  const reified = reifyEffectValue(
    fn,
    lowered.result,
    effectType,
    resultType,
    span,
    availableRequirements,
  )
  if (reified === undefined) return undefined
  endRunLoans(fn, span)
  if (recipe._tag === 'EffectConstruct' || recipe._tag === 'ServiceEffectConstruct')
    endLoans(fn, recipe.loanEnds, span)
  return reified
}

export const lowerEffectExecution = (
  fn: FunctionLowering,
  subject: Hir.Expression,
  success: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): LoweredExpression | undefined => {
  if (subject._tag === 'Match') {
    return lowerExpression(
      fn,
      Object.freeze({
        ...subject,
        arms: Object.freeze(
          subject.arms.map((arm) =>
            Object.freeze({
              ...arm,
              result: Object.freeze({
                _tag: 'Run' as const,
                subject: arm.result,
                type: success,
                span: arm.result.span,
              }),
            }),
          ),
        ),
        type: success,
      }),
    )
  }
  if (subject._tag === 'BindingReference') {
    const storedRecipe = fn.effectRecipes.get(subject.binding.ordinal)
    if (storedRecipe !== undefined)
      return lowerEffectExecution(fn, storedRecipe, success, span, availableRequirements)
  }
  if (subject._tag === 'Move' && subject.subject._tag === 'BindingReference') {
    const storedRecipe = fn.effectRecipes.get(subject.subject.binding.ordinal)
    if (storedRecipe !== undefined)
      return lowerEffectExecution(fn, storedRecipe, success, span, availableRequirements)
  }

  const forwarded = inlineForwardedRequirement(fn, subject)
  if (forwarded !== undefined) {
    const provider = lowerExpression(fn, forwarded.provider)
    if (provider === undefined) return undefined
    const providerBorrow =
      forwarded.provider._tag === 'ValueBorrow'
        ? fn.recipeBorrow(forwarded.provider.borrow)
        : undefined
    let runtimeProvider = provider.result
    let ownedLoan: { readonly borrow: Hir.BorrowId; readonly slice: Mir.LocalId } | undefined
    if (
      forwarded.selection.access === 'Take' &&
      forwarded.selection.witness._tag === 'SourceConformanceWitness'
    ) {
      const providerType = fn.type(forwarded.selection.providerType)
      const referenceType = fn.type(Type.reference('Exclusive', forwarded.selection.providerType))
      if (providerType?._tag !== 'Nominal' || referenceType?._tag !== 'Reference') return undefined
      const borrow = fn.freshSyntheticBorrow(forwarded.provider.span)
      const reference = fn.alloc(referenceType)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow,
          destination: reference,
          root: provider.result,
          selectors: Object.freeze([]),
          sourceType: providerType,
          type: referenceType,
          access: 'Exclusive',
          reborrow: false,
          suspendsParent: false,
          provenance: authored(forwarded.provider.span),
        }),
      )
      fn.loanLocals.set(borrowKey(borrow), reference)
      runtimeProvider = reference
      ownedLoan = Object.freeze({ borrow, slice: reference })
    }
    const result = lowerEffectExecution(
      fn,
      forwarded.binding.protected,
      success,
      span,
      Object.freeze([
        ...availableRequirements,
        Object.freeze({ ...forwarded.selection, local: runtimeProvider }),
      ]),
    )
    if (result === undefined) return undefined
    endRunLoans(fn, span)
    if (providerBorrow !== undefined) endLoans(fn, [providerBorrow], span)
    if (ownedLoan !== undefined) endLoan(fn, ownedLoan.borrow, forwarded.provider.span)
    if (forwarded.selection.access === 'Take')
      dropOwnedProvider(
        fn,
        provider.result,
        forwarded.selection.providerType,
        forwarded.provider.span,
      )
    if (
      forwarded.binding.protected._tag === 'EffectConstruct' ||
      forwarded.binding.protected._tag === 'ServiceEffectConstruct'
    )
      endLoans(fn, forwarded.binding.protected.loanEnds, span)
    return result
  }

  if (subject._tag === 'EffectBindRequirement') {
    return lowerProvidedEffect(fn, subject.provider, (requirement) => {
      const result = lowerEffectExecution(
        fn,
        subject.protected,
        success,
        span,
        Object.freeze([...availableRequirements, requirement]),
      )
      if (result === undefined) return undefined
      endRunLoans(fn, span)
      if (
        subject.protected._tag === 'EffectConstruct' ||
        subject.protected._tag === 'ServiceEffectConstruct'
      )
        endLoans(fn, subject.protected.loanEnds, span)
      return result
    })
  }
  if (subject._tag === 'ServiceEffectConstruct') {
    const lowered = lowerServiceEffectValue(fn, subject, availableRequirements)
    const effectResult =
      lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
    if (lowered === undefined || effectResult?._tag !== 'EffectValue') return undefined
    const result = lowerRunEffectValue(
      fn,
      lowered.result,
      effectResult,
      success,
      span,
      availableRequirements,
    )
    if (result !== undefined) {
      endRunLoans(fn, span)
      endLoans(fn, subject.loanEnds, span)
    }
    return result
  }

  if (subject._tag === 'BuiltinCall' && Type.isEffect(subject.type)) {
    const run = Object.freeze({ _tag: 'Run' as const, subject, type: success, span })
    return lowerExpression(fn, run)
  }

  const lowered = lowerExpression(fn, subject)
  const loweredType = lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
  if (lowered === undefined || loweredType === undefined) return undefined
  if (loweredType._tag === 'EffectComposite')
    return lowerRunEffectComposite(
      fn,
      lowered.result,
      loweredType,
      success,
      span,
      availableRequirements,
    )
  if (loweredType._tag !== 'EffectValue') return undefined
  return lowerRunEffectValue(fn, lowered.result, loweredType, success, span, availableRequirements)
}

/**
 * Redirects one bound operator to the provider's own function when specialization lands on a type
 * whose conformance maps the operation to source rather than to a sealed intrinsic.
 *
 * Operator syntax lowers values through the compiler-known scalar shape, while the bound records
 * the ordinary interface contract the operator spells. A source witness therefore receives each
 * operand exactly as that contract declares it: borrowed operands create call-scoped loans, while
 * value operands remain values. This is contract-directed lowering, not a witness-side adapter.
 */
