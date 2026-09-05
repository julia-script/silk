import {
  authored,
  cleanupForLocal,
  concreteCleanup,
  generated,
  ownershipLocal,
  lowerOwnershipPath,
  propagationLoanEnds,
  propagationReleases,
} from './CleanupEmission.js'
import * as ConformanceProof from './ConformanceProof.js'
import type {} from './EntryAssembly.js'
import type {} from './Forwarding.js'
import { inlineForwardedRequirement } from './Forwarding.js'
import type { FunctionLowering } from './FunctionLowering.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import * as Lifetime from './Lifetime.js'
import type { ProvidedRequirement } from './Lower.js'
import { borrowKey, patternKey, specializeProvider } from './Lower.js'
import type {} from './LowerExpression.js'
import { lowerExpression, lowerExecution } from './LowerExpression.js'
import * as Match from './Match.js'
import * as Ownership from './Ownership.js'
import * as Mir from './Mir.js'
import * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'
import {
  baseRunnerKey,
  effectValueByIdentity,
  ensureProvidedRunner,
  instanceText,
  providerBindings,
  requirementsFor,
  runtimeRequirementArguments,
} from './ValueType.js'

export const lowerCatchEffectValue = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.activeRequirements ??
    fn.providedRequirements,
): LoweredExpression | undefined => {
  const protected_ = lowerExpression(fn, expression.protected, availableRequirements)
  if (protected_ === 'Transferred') return protected_
  const protectedType =
    protected_ === undefined ? undefined : fn.localTypes.at(protected_.result.ordinal)
  const handler = lowerExpression(fn, expression.handler, availableRequirements)
  if (handler === 'Transferred') return handler
  const handlerType = handler === undefined ? undefined : fn.localTypes.at(handler.result.ordinal)
  if (
    protected_ === undefined ||
    protectedType?._tag !== 'EffectValue' ||
    handler === undefined ||
    handlerType?._tag !== 'CallableValue'
  ) {
    return undefined
  }

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

export interface LoweredValue {
  readonly result: Mir.LocalId
}

/** A successful eager expression either supplies a normal value or leaves its execution context. */
export type LoweredExpression = LoweredValue | 'Transferred'

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
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.activeRequirements ??
    fn.providedRequirements,
): LoweredExpression | undefined => {
  // A success that is itself an Effect is the value the environment's success identity names.
  const successType = Type.isEffect(fn.semantic(success))
    ? effectValueByIdentity(fn.layout, effectType.environment.successEffectIdentity ?? '')
    : fn.type(success)
  if (successType === undefined || successType._tag === 'EffectOutcome') return undefined
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
  const runnerInstance =
    effectType.storage?.realization.runnerInstance ?? effectType.environment.instance
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
      ...(runnerInstance.staticArguments.length === 0
        ? {}
        : { runnerStaticArguments: runnerInstance.staticArguments }),
      ...(providedRunner === undefined
        ? {}
        : {
            runnerBase: Object.freeze({
              declaration: baseRunner,
              typeArguments: baseRunnerTypeArguments,
              ...(runnerInstance.staticArguments.length === 0
                ? {}
                : { staticArguments: runnerInstance.staticArguments }),
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
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.activeRequirements ??
    fn.providedRequirements,
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

export interface CaughtEffect {
  readonly valid: Mir.LocalId
  readonly success: Mir.LocalId
  readonly failure: Mir.LocalId
  readonly failureValueType: Type.Type
}

export const runCaughtEffectValue = (
  fn: FunctionLowering,
  effect: Mir.LocalId,
  effectType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.activeRequirements ??
    fn.providedRequirements,
): CaughtEffect | undefined => {
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
  const failureValueType = Type.failureValue(Type.failureMembers(effectType.type))
  const boolType = fn.type('bool')
  const successType = fn.type(effectType.type.success)
  const failureType = fn.type(failureValueType)
  const outcomeShape = Layout.callingShape(fn.layout, effectType.type)
  const successShape = Layout.callingShape(fn.layout, effectType.type.success)
  const failureValueShape = Layout.callingShape(fn.layout, failureValueType)
  if (
    boolType?._tag !== 'bool' ||
    successType === undefined ||
    successType._tag === 'EffectOutcome' ||
    failureType === undefined ||
    failureType._tag === 'EffectOutcome' ||
    outcomeShape === undefined ||
    successShape === undefined ||
    failureValueShape === undefined ||
    Type.failureMembers(effectType.type).length === 0
  )
    return undefined
  const outcome = fn.alloc(outcomeType)
  const valid = fn.alloc(boolType)
  const success = fn.alloc(successType)
  const failure = fn.alloc(failureType)
  fn.emit(
    Object.freeze({
      _tag: 'CatchEffect' as const,
      destination: valid,
      outcome,
      successValue: success,
      failureValue: failure,
      effect,
      runner,
      runnerTypeArguments:
        effectType.storage?.realization.runnerArguments ??
        effectType.environment.instance.typeArguments,
      arguments: runtimeRequirementArguments(provided),
      outcomeType,
      failureValueType,
      successShape,
      outcomeShape,
      failureValueShape,
      type: boolType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({
    valid,
    success,
    failure,
    failureValueType,
  })
}

export const callableEffectValue = (
  fn: FunctionLowering,
  callable: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  if (callable.target._tag !== 'DeclarationCallableTarget') return undefined
  const typeArguments =
    callable.environment?.callable.typeArguments ??
    callable.storage?.realization.targetArguments ??
    callable.typeArguments ??
    Object.freeze([])
  const result = fn.effectResults.get(instanceText(callable.target.declaration, typeArguments))
  return result?._tag === 'EffectValue' ? result : undefined
}

/**
 * Injects a run result into the recovered success type. A `never` source diverged before producing
 * a value, so it flows through unchanged; a member of the `A | B` union is injected or widened.
 */
const injectSuccess = (
  fn: FunctionLowering,
  source: Mir.LocalId,
  from: Type.Type,
  to: Type.Type,
  span: SourceSpan.SourceSpan,
): Mir.LocalId | undefined => {
  if (Type.equals(from, to) || Type.isNever(from)) return source
  const conversion = TypeCompatibility.check(from, to)
  const sourceType = fn.type(from)
  const targetType = fn.type(to)
  const sourceShape = Layout.callingShape(fn.layout, from)
  const targetShape = Layout.callingShape(fn.layout, to)
  if (
    (conversion._tag !== 'Inject' && conversion._tag !== 'Widen') ||
    sourceType === undefined ||
    sourceType._tag === 'EffectOutcome' ||
    targetType?._tag !== 'Union' ||
    sourceShape === undefined ||
    targetShape === undefined
  )
    return undefined
  const destination = fn.alloc(targetType)
  fn.emit(
    Object.freeze({
      _tag: 'ConvertUnion' as const,
      destination,
      source,
      sourceType,
      targetType,
      conversion: conversion._tag,
      mappings: conversion.mappings,
      sourceShape,
      targetShape,
      access: 'Owned' as const,
      provenance: generated(span),
    }),
  )
  return destination
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
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.activeRequirements ??
    fn.providedRequirements,
): LoweredExpression | undefined => {
  if (expression.protected._tag === 'Unavailable') return undefined
  let protected_: LoweredExpression | undefined
  if (captured === undefined) {
    if (expression.protected._tag === 'ServiceEffectConstruct') {
      protected_ = lowerServiceEffectValue(fn, expression.protected, availableRequirements)
    } else {
      protected_ = lowerExpression(fn, expression.protected, availableRequirements)
    }
  } else {
    protected_ = Object.freeze({ result: captured.protected })
  }
  if (protected_ === 'Transferred') return protected_
  const protectedType =
    captured?.protectedType ??
    (protected_ === undefined ? undefined : fn.localTypes.at(protected_.result.ordinal))
  if (protected_ === undefined || protectedType?._tag !== 'EffectValue') return undefined

  // Both operands are formed before the protected Effect starts, matching ordinary call
  // evaluation even though the handler is invoked only on the selected failure path.
  const handler =
    captured === undefined
      ? lowerExpression(fn, expression.handler, availableRequirements)
      : Object.freeze({ result: captured.handler })
  if (handler === 'Transferred') return handler
  const handlerType =
    captured?.handlerType ??
    (handler === undefined ? undefined : fn.localTypes.at(handler.result.ordinal))
  const handlerEffectType =
    handlerType?._tag === 'CallableValue' ? callableEffectValue(fn, handlerType) : undefined
  if (
    handler === undefined ||
    handlerType?._tag !== 'CallableValue' ||
    handlerEffectType === undefined
  )
    { if (fn.owner.key.declaration.name === 'Effect.catchAll') console.log('TEMP catch-stop', 47, JSON.stringify({ handler: handlerType, keys: [...fn.effectResults.keys()].filter((key) => key.includes('recover')) })); return undefined }
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
  if (!Type.isEffect(protectedEffect) || !Type.isEffect(resultEffect)) return undefined
  if (Type.isNever(selected)) {
    const succeeded = lowerRunEffectValue(
      fn,
      protected_.result,
      protectedType,
      protectedEffect.success,
      runSpan,
      availableRequirements,
    )
    if (succeeded === 'Transferred') return succeeded
    if (succeeded === undefined) return undefined
    for (const drop of unusedHandlerDrop()) fn.emit(drop)
    const destination = injectSuccess(
      fn,
      succeeded.result,
      protectedEffect.success,
      resultEffect.success,
      expression.span,
    )
    if (destination === undefined) return undefined
    endRunLoans(fn, runSpan)
    return Object.freeze({ result: destination })
  }
  const protectedFailures = Type.failureMembers(protectedEffect)
  const selectedMembers: ReadonlyArray<Type.Type> = Type.isUnion(selected)
    ? selected.members
    : Object.freeze([selected])
  if (
    selectedMembers.some(
      (member) => !protectedFailures.some((failure) => Type.equals(failure, member)),
    )
  )
    { if (fn.owner.key.declaration.name === 'Effect.catchAll') console.log('TEMP catch-stop', 102); return undefined }

  const caught = runCaughtEffectValue(
    fn,
    protected_.result,
    protectedType,
    expression.span,
    availableRequirements,
  )
  if (caught === undefined) return undefined
  // The handler runs with its own success type; `never` diverges and a member of `A | B` is
  // injected so both paths meet the recovered union (FAIL-004).
  const runHandler = (applied: Mir.LocalId): LoweredExpression | undefined => {
    const handled = lowerRunEffectValue(
      fn,
      applied,
      handlerEffectType,
      handlerEffectType.type.success,
      runSpan,
      availableRequirements,
    )
    if (handled === 'Transferred') return handled
    if (handled === undefined) return undefined
    const result = injectSuccess(
      fn,
      handled.result,
      handlerEffectType.type.success,
      resultEffect.success,
      expression.span,
    )
    return result === undefined ? undefined : Object.freeze({ result })
  }
  const [takenResult, takenOperations] = fn.capture(() => {
    for (const drop of unusedHandlerDrop()) fn.emit(drop)
    return injectSuccess(
      fn,
      caught.success,
      protectedEffect.success,
      resultEffect.success,
      expression.span,
    )
  })
  if (takenResult === undefined) return undefined
  const taken = lowerExecution(fn, expression.span, () => {
    for (const operation of takenOperations) fn.emit(operation)
    return Object.freeze({ result: takenResult })
  })
  if (taken === undefined) return undefined

  const successType = fn.type(resultEffect.success)
  const successShape = Layout.callingShape(fn.layout, resultEffect.success)
  const failureValueMir = fn.type(caught.failureValueType)
  const propagationEffect = fn.effectOutcome
  const propagationType = propagationEffect === undefined ? undefined : fn.type(propagationEffect)
  const propagationShape =
    propagationEffect === undefined ? undefined : Layout.callingShape(fn.layout, propagationEffect)
  if (
    successType === undefined ||
    successType._tag === 'EffectOutcome' ||
    successShape === undefined ||
    failureValueMir === undefined ||
    failureValueMir._tag === 'EffectOutcome' ||
    propagationEffect === undefined ||
    propagationType?._tag !== 'EffectOutcome' ||
    propagationShape === undefined
  )
    { if (fn.owner.key.declaration.name === 'Effect.catchAll') console.log('TEMP catch-stop', 168); return undefined }

  if (failureValueMir._tag !== 'Nominal' && failureValueMir._tag !== 'Union') {
    const onlyFailure = protectedFailures.at(0)
    if (
      protectedFailures.length !== 1 ||
      onlyFailure === undefined ||
      !selectedMembers.some((candidate) => Type.equals(candidate, onlyFailure))
    )
      { if (fn.owner.key.declaration.name === 'Effect.catchAll') console.log('TEMP catch-stop', 177); return undefined }
    const otherwise = lowerExecution(fn, expression.span, () => {
      const applied = fn.alloc(handlerEffectType)
      fn.emit(
        Object.freeze({
          _tag: 'ApplyCallable' as const,
          destination: applied,
          callable: handler.result,
          typeArguments:
            handlerType.environment?.callable.typeArguments ??
            handlerType.storage?.realization.targetArguments ??
            handlerType.typeArguments ??
            Object.freeze([]),
          captures: Object.freeze([]),
          arguments: Object.freeze([caught.failure]),
          callableType: handlerType.type,
          access: handlerType.type.mode,
          evaluation: 'CalleeThenArguments' as const,
          realization: 'Environment' as const,
          type: handlerEffectType,
          provenance: generated(expression.span),
        }),
      )
      return runHandler(applied)
    })
    if (otherwise === undefined) return undefined
    const destination = fn.alloc(successType)
    fn.emit(
      Object.freeze({
        _tag: 'Conditional' as const,
        destination,
        condition: caught.valid,
        taken,
        otherwise,
        type: successType,
        resultShape: successShape,
        provenance: generated(expression.span),
      }),
    )
    endRunLoans(fn, runSpan)
    return Object.freeze({ result: destination })
  }

  const declaration = fn.owner.function.declaration.id
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
  const failureCoverage = Object.freeze(failureMembers.map(Match.structuralMember))
  const innerArms: Array<Mir.MatchArm> = []
  for (const [ordinal, member] of failureMembers.entries()) {
    const memberCoverage = Match.structuralMember(member)
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
    const selectedExecution = lowerExecution(fn, expression.span, () => {
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
            { if (fn.owner.key.declaration.name === 'Effect.catchAll') console.log('TEMP catch-stop', 267); return undefined }
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
              handlerType.typeArguments ??
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
        return runHandler(applied)
      }
      const target = Type.failureMembers(propagationEffect).findIndex((candidate) =>
        Type.equals(candidate, member),
      )
      const bottom = fn.type('never')
      if (target < 0 || bottom?._tag !== 'Bottom') return undefined
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
      return 'Transferred'
    })
    if (selectedExecution === undefined) return undefined
    innerArms.push(
      Object.freeze({
        id: armId,
        member: memberCoverage,
        universal: false,
        before: Object.freeze(failureCoverage.slice(ordinal)),
        after: Object.freeze(failureCoverage.slice(ordinal + 1)),
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
        cleanupBindings: Object.freeze([]),
        selected: Object.freeze({
          access: 'Move' as const,
          execution: selectedExecution,
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
    scrutinee: caught.failure,
    scrutineeType: failureValueMir,
    scrutineeShape: Layout.callingShape(fn.layout, caught.failureValueType) ?? successShape,
    access: 'Move',
    retainsBindings: false,
    members: failureCoverage,
    decisions: Object.freeze(
      failureCoverage.map((member, ordinal) =>
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
  const otherwise = lowerExecution(fn, expression.span, () => {
    fn.emit(innerOperation)
    return Object.freeze({ result: innerResult })
  })
  if (otherwise === undefined) return undefined
  const destination = fn.alloc(successType)
  fn.emit(
    Object.freeze({
      _tag: 'Conditional' as const,
      destination,
      condition: caught.valid,
      taken,
      otherwise,
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
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.activeRequirements ??
    fn.providedRequirements,
): LoweredPlace | 'Transferred' | undefined => {
  if (expression._tag === 'ReferentPlace') {
    const root = lowerExpression(fn, expression.subject, availableRequirements)
    if (root === 'Transferred') return root
    return root === undefined
      ? undefined
      : Object.freeze({ root: root.result, selectors: Object.freeze([]) })
  }
  if (expression._tag === 'Project') {
    const subject = lowerPlacePath(fn, expression.subject, availableRequirements)
    if (subject === 'Transferred') return subject
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
    const subject = lowerPlacePath(fn, expression.subject, availableRequirements)
    if (subject === 'Transferred') return subject
    if (subject === undefined) return undefined
    const index:
      | Extract<Mir.PlaceSelector, { readonly _tag: 'ElementSelector' }>['index']
      | 'Transferred'
      | undefined =
      expression.bounds._tag === 'Proven'
        ? Object.freeze({ _tag: 'Proven', value: expression.bounds.index })
        : (() => {
            const lowered = lowerExpression(fn, expression.index, availableRequirements)
            if (lowered === 'Transferred') return lowered
            return lowered === undefined
              ? undefined
              : Object.freeze({ _tag: 'Runtime' as const, local: lowered.result })
          })()
    if (index === 'Transferred') return index
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
    const subject = lowerPlacePath(fn, expression.slice, availableRequirements)
    if (subject === 'Transferred') return subject
    const index = lowerExpression(fn, expression.index, availableRequirements)
    if (index === 'Transferred') return index
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
  const root = lowerExpression(fn, expression, availableRequirements)
  if (root === 'Transferred') return root
  return root === undefined
    ? undefined
    : Object.freeze({ root: root.result, selectors: Object.freeze([]) })
}

export const lowerPlace = (
  fn: FunctionLowering,
  expression: Extract<
    Hir.Expression,
    { readonly _tag: 'ReferentPlace' | 'Project' | 'IndexPlace' | 'SliceIndexPlace' }
  >,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.activeRequirements ??
    fn.providedRequirements,
): LoweredExpression | undefined => {
  const place = lowerPlacePath(fn, expression, availableRequirements)
  if (place === 'Transferred') return place
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
      (type._tag === 'EffectValue' && type.storage !== undefined && type.type.access === 'Take') ||
      ('access' in expression && expression.access === 'ConsumeRequested')
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
      loan.origin !== 'ReturnedCallableCapture' &&
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
    if (loan.origin !== 'ReturnedView' && loan.origin !== 'ReturnedCallableCapture') continue
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
      (child._tag === 'BuiltinCall' || child._tag === 'InterfaceOperationCall') &&
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

/** Resolves a discriminant-only pattern alias to its original owned storage. */
export const patternPlace = (
  fn: FunctionLowering,
  binding: Match.BindingId,
  span: SourceSpan.SourceSpan,
):
  | { readonly root: Mir.LocalId; readonly selectors: ReadonlyArray<Mir.PlaceSelector> }
  | undefined => {
  const place = Ownership.allBindings(fn.ownership).find(
    (candidate) =>
      candidate.site._tag === 'Pattern' &&
      patternKey(candidate.site.binding) === patternKey(binding),
  )?.place
  if (place === undefined) return undefined
  const root = ownershipLocal(fn, place.root)
  if (root === undefined) return undefined
  const selectors = lowerOwnershipPath(fn, root, place.path, span)
  return selectors === undefined ? undefined : { root, selectors }
}

export const ownedWriteRoot = (
  fn: FunctionLowering,
  root: Hir.OwnedWriteRoot,
): Mir.LocalId | undefined => {
  switch (root._tag) {
    case 'ParameterWriteRoot':
      return fn.parameterLocals.get(root.parameter.ordinal)
    case 'BindingWriteRoot':
      return fn.bindingLocals.get(root.binding.ordinal)
    case 'PatternWriteRoot': {
      const place = Ownership.allBindings(fn.ownership).find(
        (candidate) =>
          candidate.site._tag === 'Pattern' &&
          patternKey(candidate.site.binding) === patternKey(root.binding),
      )?.place
      return place === undefined
        ? fn.patternLocals.get(patternKey(root.binding))
        : ownershipLocal(fn, place.root)
    }
  }
}

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
  const loweredArguments: Array<Mir.LocalId> = []
  for (const argument of subject.arguments) {
    const lowered = lowerExpression(fn, argument, availableRequirements)
    if (lowered === 'Transferred' || lowered === undefined) return lowered
    loweredArguments.push(lowered.result)
  }
  const call = fn.call(subject.span, target)
  if (
    call === undefined ||
    call.target.declaration.module !== target.module ||
    call.target.declaration.name !== target.name
  )
    return undefined
  const typeArguments = call?.target.typeArguments ?? provided.witness.typeArguments
  const effectValue =
    (call?.resultEffect === undefined
      ? undefined
      : effectValueByIdentity(fn.layout, call.resultEffect)) ??
    fn.effectResults.get(instanceText(target, typeArguments))
  if (effectValue === undefined) return undefined
  const effect = fn.alloc(effectValue)
  fn.emit(
    Object.freeze({
      _tag: 'Call',
      destination: effect,
      target,
      typeArguments,
      arguments: Object.freeze([provided.local, ...loweredArguments]),
      type: effectValue,
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
  let provider: Mir.LocalId | undefined
  if (providerFact.binding !== undefined) {
    provider = fn.bindingLocals.get(providerFact.binding.ordinal)
  } else if (providerFact.parameter !== undefined) {
    provider = fn.parameterLocals.get(providerFact.parameter.ordinal)
  } else {
    provider = undefined
  }
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

  if (provider === undefined) return undefined
  const providerType = fn.type(selected.providerType)
  const referenceType = fn.type(
    Type.reference(
      access,
      selected.providerType,
      forwardedType?._tag === 'Reference'
        ? forwardedType.type.lifetime
        : Lifetime.local(fn.owner.key.declaration, 'retained-provider', provider.ordinal),
    ),
  )
  const authoredLoan = fn.ownership?.loans.find(
    (candidate) =>
      (candidate.origin === 'EffectCapture' ||
        candidate.origin === 'CallableCapture' ||
        candidate.origin === 'ReturnedCallableCapture' ||
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
 * Brackets one provided requirement for catch handling or immediate execution. The actor that begins
 * a provider loan also ends it, removes its tracking entry, and drops a taken provider after the
 * protected lowering has finished.
 */
export const lowerProvidedEffect = <A>(
  fn: FunctionLowering,
  providerFact: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>['provider'],
  use: (requirement: ProvidedRequirement) => A | undefined,
): A | 'Transferred' | undefined => {
  const provided = prepareProvidedEffect(fn, providerFact)
  if (provided === undefined) return undefined
  const result = use(provided.requirement)
  if (result === 'Transferred') return result
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

type ForwardedRequirement = Exclude<ReturnType<typeof inlineForwardedRequirement>, undefined>

/** Brackets the provider introduced by an inlined forwarding wrapper. */
const lowerForwardedProvider = <A>(
  fn: FunctionLowering,
  forwarded: ForwardedRequirement,
  loanEndSpan: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement>,
  use: (requirement: ProvidedRequirement) => A | undefined,
): A | 'Transferred' | undefined => {
  const provider = lowerExpression(fn, forwarded.provider, availableRequirements)
  if (provider === 'Transferred') return provider
  if (provider === undefined) return undefined
  const providerBorrow =
    forwarded.provider._tag === 'ValueBorrow'
      ? fn.recipeBorrow(forwarded.provider.borrow)
      : undefined
  let runtimeProvider = provider.result
  let ownedLoan: Hir.BorrowId | undefined
  if (
    forwarded.selection.access === 'Take' &&
    forwarded.selection.witness._tag === 'SourceConformanceWitness'
  ) {
    const providerType = fn.type(forwarded.selection.providerType)
    const referenceType = fn.type(
      Type.reference(
        'Exclusive',
        forwarded.selection.providerType,
        Lifetime.local(fn.owner.key.declaration, 'owned-provider', provider.result.ordinal),
      ),
    )
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
    ownedLoan = borrow
  }
  const result = use(Object.freeze({ ...forwarded.selection, local: runtimeProvider }))
  if (result === 'Transferred') return result
  if (result === undefined) return undefined
  if (providerBorrow !== undefined) endLoans(fn, [providerBorrow], loanEndSpan)
  if (ownedLoan !== undefined) endLoans(fn, [ownedLoan], forwarded.provider.span)
  if (forwarded.selection.access === 'Take')
    dropOwnedProvider(
      fn,
      provider.result,
      forwarded.selection.providerType,
      forwarded.provider.span,
    )
  return result
}

export const lowerEffectExecution = (
  fn: FunctionLowering,
  subject: Hir.Expression,
  success: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.activeRequirements ??
    fn.providedRequirements,
): LoweredExpression | undefined => {
  if (subject._tag === 'Match') {
    return lowerExpression(
      fn,
      Object.freeze({
        ...subject,
        arms: Object.freeze(
          subject.arms.map((arm) =>
            arm.body._tag === 'Block'
              ? arm
              : Object.freeze({
                  ...arm,
                  body: Object.freeze({
                    ...arm.body,
                    type: success,
                    expression: Object.freeze({
                      _tag: 'Run' as const,
                      subject: arm.body.expression,
                      type: success,
                      span: arm.body.span,
                    }),
                  }),
                }),
          ),
        ),
        type: success,
      }),
      availableRequirements,
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
    return lowerForwardedProvider(fn, forwarded, span, availableRequirements, (requirement) => {
      const result = lowerEffectExecution(
        fn,
        forwarded.binding.protected,
        success,
        span,
        Object.freeze([...availableRequirements, requirement]),
      )
      if (result === 'Transferred') return result
      if (result === undefined) return undefined
      endRunLoans(fn, span)
      if (
        forwarded.binding.protected._tag === 'EffectConstruct' ||
        forwarded.binding.protected._tag === 'ServiceEffectConstruct'
      )
        endLoans(fn, forwarded.binding.protected.loanEnds, span)
      return result
    })
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
      if (result === 'Transferred') return result
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
    if (lowered === 'Transferred') return lowered
    const effectValue = lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
    if (lowered === undefined || effectValue?._tag !== 'EffectValue') return undefined
    const result = lowerRunEffectValue(
      fn,
      lowered.result,
      effectValue,
      success,
      span,
      availableRequirements,
    )
    if (result === 'Transferred') return result
    if (result !== undefined) {
      endRunLoans(fn, span)
      endLoans(fn, subject.loanEnds, span)
    }
    return result
  }

  if (subject._tag === 'BuiltinCall' && Type.isEffect(subject.type)) {
    const run = Object.freeze({ _tag: 'Run' as const, subject, type: success, span })
    return lowerExpression(fn, run, availableRequirements)
  }

  const lowered = lowerExpression(fn, subject, availableRequirements)
  if (lowered === 'Transferred') return lowered
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
