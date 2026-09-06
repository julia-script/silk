import * as ConformanceProof from './ConformanceProof.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as FieldRealization from './FieldRealization.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as Suspension from './Suspension.js'
import * as SuspensionMode from './SuspensionMode.js'
import * as Type from './Type.js'

/**
 * Monomorphic suspension control before normalization and MIR-local liveness. This is deliberately
 * not a `Mir.Module`: verification and LLVM lowering cannot consume provisional control.
 */

export type Classification = Suspension.SuspensionClassification

export type ExecutionKey =
  | {
      readonly _tag: 'InstanceExecution'
      readonly instance: Instances.InstanceKey
      readonly functionOrdinal: number
      readonly identity: string
    }
  | {
      readonly _tag:
        | 'EffectRunnerExecution'
        | 'WitnessEffectRunnerExecution'
        | 'BuiltinEffectRunnerExecution'
      readonly owner: Instances.InstanceKey
      readonly site: Hir.EffectSiteId
      readonly identity: string
      readonly runner: DeclarationFacts.CanonicalId
    }
  | {
      readonly _tag: 'ProvidedEffectRunnerExecution'
      readonly owner: Instances.InstanceKey
      readonly site: Hir.EffectSiteId
      readonly identity: string
      readonly effectIdentity: string
      readonly runner: DeclarationFacts.CanonicalId
      readonly providers: ReadonlyArray<Provider>
    }

export interface ControlId {
  readonly _tag: 'ProvisionalControlId'
  readonly execution: string
  readonly sourceId: string
  readonly functionOrdinal: number
  readonly spanStart: number
  readonly spanEnd: number
  readonly ordinal: number
  readonly port: 'Origin' | 'Invoke' | 'Complete'
}

export interface Capture extends Suspension.Capture {}

export interface Provider extends Suspension.Provider {}

export interface Runner extends Suspension.RunnerBase<Capture, Provider> {
  readonly execution:
    | ExecutionKey
    | { readonly _tag: 'UnknownExecution'; readonly identity: string }
  readonly providedIdentity?: string
}

export type CompletionPolicy = Suspension.SuspensionCompletion

export type Outcome =
  | {
      readonly _tag: 'SuspendEffect'
      readonly deferred: Runner
      /** An unpublished transfer is created; this does not start the deferred runner. */
      readonly transfer: {
        readonly _tag: 'OriginateTransfer'
      }
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'RunSuspendableEffect'
      readonly runner: Runner
      readonly completion: CompletionPolicy
      readonly complete: ControlId
      readonly relay: {
        readonly _tag: 'RelayExistingTransfer'
        readonly preserves: readonly ['Child', 'Origin', 'TypedOutcome']
      }
      readonly span: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'Complete'; readonly policy: CompletionPolicy }

/** One node in the provisional control graph. Transfer relay is a terminal outcome, not an edge. */
export interface Region {
  readonly _tag: 'ProvisionalRegion'
  readonly id: ControlId
  readonly outcome: Outcome
}

export interface Execution {
  readonly _tag: 'ProvisionalExecution'
  readonly key: ExecutionKey
  readonly classification: Classification
  readonly regions: ReadonlyArray<Region>
}

export interface Module {
  readonly _tag: 'ProvisionalMirModule'
  readonly module: string
  readonly executions: ReadonlyArray<Execution>
}

const executionIdentity = (key: ExecutionKey): string => key.identity

const executionInstanceCache = new WeakMap<ExecutionKey, Instances.InstanceKey>()

const executionInstance = (key: ExecutionKey): Instances.InstanceKey => {
  if (key._tag === 'InstanceExecution') {
    return key.instance
  }
  let cached = executionInstanceCache.get(key)
  if (cached === undefined) {
    let siteKind = 'effect-site'
    if (key._tag === 'WitnessEffectRunnerExecution') siteKind = 'witness-effect-site'
    else if (key._tag === 'BuiltinEffectRunnerExecution') siteKind = 'builtin-effect-site'
    cached = Object.freeze({
      _tag: 'InstanceKey',
      declaration: key.runner,
      typeArguments: key.owner.typeArguments,
      evidence: key.owner.evidence,
      staticArguments: key.owner.staticArguments,
      contractRow: Object.freeze([
        ...key.owner.contractRow,
        `${siteKind}:${Hir.executableSiteKey(key.site)}`,
        ...(key._tag === 'ProvidedEffectRunnerExecution'
          ? key.providers.map(providedContractEntry)
          : []),
      ]),
    })
    executionInstanceCache.set(key, cached)
  }
  return cached
}

const sameSpan = (left: SourceSpan.SourceSpan, right: SourceSpan.SourceSpan): boolean =>
  left.sourceId === right.sourceId && left.start === right.start && left.end === right.end

const providedBaseName = (name: string): string | undefined => {
  const marker = name.lastIndexOf('$provided$')
  return marker < 0 ? undefined : name.slice(0, marker)
}

const executionIndexCache = new WeakMap<ReadonlyArray<Execution>, Map<string, Execution>>()

const executionIndex = (executions: ReadonlyArray<Execution>): Map<string, Execution> => {
  let index = executionIndexCache.get(executions)
  if (index === undefined) {
    index = new Map()
    for (const execution of executions) {
      const key = Instances.keyText(executionInstance(execution.key))
      if (!index.has(key)) index.set(key, execution)
    }
    executionIndexCache.set(executions, index)
  }
  return index
}

const executionForInstance = (
  self: Module,
  instance: Instances.InstanceKey,
): Execution | undefined => {
  const exact = executionIndex(self.executions).get(Instances.keyText(instance))
  if (exact !== undefined) return exact
  const baseName = providedBaseName(instance.declaration.name)
  if (baseName === undefined) return undefined
  const candidates = self.executions.filter((execution) => {
    if (execution.key._tag === 'InstanceExecution') return false
    const executionKey = executionInstance(execution.key)
    return (
      executionKey.declaration.module === instance.declaration.module &&
      executionKey.declaration.name === baseName
    )
  })
  const specialized = candidates.find((execution) => {
    const executionKey = executionInstance(execution.key)
    return (
      Instances.keyText(Object.freeze({ ...executionKey, declaration: instance.declaration })) ===
      Instances.keyText(instance)
    )
  })
  if (specialized !== undefined) return specialized
  const openInstance: Instances.InstanceKey = Object.freeze({
    ...instance,
    contractRow: Object.freeze(
      instance.contractRow.filter((entry) => !entry.startsWith('provided:')),
    ),
  })
  return candidates.find((execution) => {
    const executionKey = executionInstance(execution.key)
    return (
      (execution.key._tag === 'EffectRunnerExecution' ||
        execution.key._tag === 'WitnessEffectRunnerExecution' ||
        execution.key._tag === 'BuiltinEffectRunnerExecution') &&
      Instances.keyText(Object.freeze({ ...executionKey, declaration: instance.declaration })) ===
        Instances.keyText(openInstance)
    )
  })
}

/** Resolves the provisional execution corresponding to one exact lowered MIR function. */
export const executionOf = (self: Module, instance: Instances.InstanceKey): Execution | undefined =>
  executionForInstance(self, instance)

const mergeClassifications = (classifications: ReadonlyArray<Classification>): Classification => {
  if (classifications.includes('Suspendable')) {
    return 'Suspendable'
  }
  if (classifications.includes('Unknown')) {
    return 'Unknown'
  }
  if (classifications.includes('Synchronous')) {
    return 'Synchronous'
  }
  return 'Unknown'
}

const providedClassification = (
  self: Module,
  instance: Instances.InstanceKey,
): Classification | undefined => {
  const baseName = providedBaseName(instance.declaration.name)
  if (baseName === undefined) return undefined
  const classifications = self.executions.flatMap((execution) =>
    execution.regions.flatMap((region) => {
      const outcome = region.outcome
      if (
        outcome._tag !== 'RunSuspendableEffect' ||
        outcome.runner.execution._tag !== 'ProvidedEffectRunnerExecution'
      )
        return []
      // Distinct providers may give the same base runner different suspension behavior.
      const key = executionInstance(outcome.runner.execution)
      return key.declaration.module === instance.declaration.module &&
        key.declaration.name === baseName &&
        Instances.keyText(Object.freeze({ ...key, declaration: instance.declaration })) ===
          Instances.keyText(instance)
        ? [outcome.runner.classification]
        : []
    }),
  )
  if (classifications.length > 0) return mergeClassifications(classifications)

  return undefined
}

const controlId = (
  execution: ExecutionKey,
  span: SourceSpan.SourceSpan,
  ordinal: number,
  port: ControlId['port'],
): ControlId =>
  Object.freeze({
    _tag: 'ProvisionalControlId',
    execution: executionIdentity(execution),
    sourceId: span.sourceId,
    functionOrdinal:
      execution._tag === 'InstanceExecution'
        ? execution.functionOrdinal
        : execution.site.function.ordinal,
    spanStart: span.start,
    spanEnd: span.end,
    ordinal,
    port,
  })

const classificationOfEffect = (
  discovery: Instances.Discovery,
  identity: string | undefined,
): Classification => {
  if (identity === undefined) {
    return 'Unknown'
  }
  if (suspendableSummary(Instances.effectSuspensionOf(discovery, identity))) {
    return 'Suspendable'
  }
  return 'Synchronous'
}

const suspendableSummary = (summary: SuspensionMode.Summary): boolean =>
  SuspensionMode.has(summary, 'NestedTransfer') || SuspensionMode.has(summary, 'ExternalPark')

const sameDeclaration = (
  left: DeclarationFacts.CanonicalId,
  right: DeclarationFacts.CanonicalId,
): boolean => left.module === right.module && left.name === right.name

const providerKey = (provider: Provider): string =>
  `${Type.key(provider.capability)}@${provider.role}:${provider.requirementAccess}:${provider.access}:${Type.key(provider.providerType)}:${
    provider.witness?._tag ?? 'unknown'
  }`

const providedContractEntry = (provider: Provider): string => `provided:${providerKey(provider)}`

const bindingsOfStatements = (
  statements: ReadonlyArray<Hir.Statement>,
): ReadonlyMap<number, Hir.Expression> =>
  new Map(
    statements.flatMap((statement) => {
      if (statement._tag === 'Bind') {
        return [[statement.binding.ordinal, statement.initializer] as const]
      }
      if (statement._tag === 'If') {
        return [...statement.taken, ...statement.otherwise].flatMap((nested) =>
          nested._tag === 'Bind' ? [[nested.binding.ordinal, nested.initializer] as const] : [],
        )
      }
      return []
    }),
  )

const bindingsOf = (fn: Hir.HirFunction): ReadonlyMap<number, Hir.Expression> =>
  bindingsOfStatements(fn.statements)

interface BuildContext {
  readonly discovery: Instances.Discovery
  readonly layout: Layout.Plan
  readonly index: DeclarationIndex.Index
  readonly instance: Instances.Instance
  readonly bindings: ReadonlyMap<number, Hir.Expression>
  readonly effectClassifications: ReadonlyMap<string, Classification>
  readonly ambientProviders: ReadonlyArray<Provider>
}

const storedEffectRealizationOf = (
  expression: Hir.Expression,
  context: BuildContext,
): FieldRealization.EffectRealization | undefined => {
  if (expression._tag === 'BindingReference') {
    const initializer = context.bindings.get(expression.binding.ordinal)
    return initializer === undefined ? undefined : storedEffectRealizationOf(initializer, context)
  }
  if (expression._tag === 'Move') return storedEffectRealizationOf(expression.subject, context)
  if (expression._tag === 'UnionConvert')
    return storedEffectRealizationOf(expression.source, context)
  if (expression._tag === 'EffectBindRequirement')
    return storedEffectRealizationOf(expression.protected, context)
  if (expression._tag !== 'Project') return undefined
  const represented = Type.substitute(
    expression.type,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  const planned = Layout.entry(context.layout, represented)?.representation
  if (planned?._tag === 'StoredEffectEnvironment') return planned.realization
  return undefined
}

const serviceResultEffectOf = (
  expression: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>,
  context: BuildContext,
): string | undefined => {
  const capability = Type.substitute(
    expression.service,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  if (!Type.isNominal(capability)) return undefined
  const provider = context.ambientProviders.find(
    (candidate) =>
      candidate.role === expression.role &&
      Type.equals(candidate.capability, capability) &&
      (expression.access === 'Shared' ||
        candidate.access === 'Exclusive' ||
        candidate.access === 'Take'),
  )
  const implementation =
    provider?.witness?._tag === 'SourceConformanceWitness'
      ? ConformanceProof.witnessOperation(provider.witness, expression.operation)
      : undefined
  if (implementation === undefined || provider?.witness?._tag !== 'SourceConformanceWitness')
    return undefined
  const selectedCalls = context.discovery.calls.filter(
    (call) =>
      Instances.keyText(call.owner) === Instances.keyText(context.instance.key) &&
      call.target.declaration.module === implementation.module &&
      call.target.declaration.name === implementation.name &&
      call.span.sourceId === expression.span.sourceId &&
      call.span.start === expression.span.start &&
      call.span.end === expression.span.end,
  )
  const selectedCall = selectedCalls.length === 1 ? selectedCalls.at(0) : undefined
  return selectedCall?.resultEffect
}

const effectIdentityOf = (
  expression: Hir.Expression,
  context: BuildContext,
): string | undefined => {
  if (expression._tag === 'BindingReference') {
    const initializer = context.bindings.get(expression.binding.ordinal)
    return initializer === undefined ? undefined : effectIdentityOf(initializer, context)
  }
  if (expression._tag === 'Move') return effectIdentityOf(expression.subject, context)
  if (expression._tag === 'UnionConvert') return effectIdentityOf(expression.source, context)
  if (expression._tag === 'EffectBindRequirement')
    return effectIdentityOf(expression.protected, {
      ...context,
      ambientProviders: providersOf(expression, context),
    })
  if (expression._tag === 'Project') {
    const realization = storedEffectRealizationOf(expression, context)
    const represented = Type.substitute(
      expression.type,
      context.instance.substitution,
      context.instance.specialization.compatibility,
    )
    const retainedIdentity =
      Type.isRepresented(represented) &&
      Type.isEffect(represented.contract) &&
      Type.isExactRepresentationArgument(represented.representation.argument) &&
      Type.isEffectIdentityArgument(represented.representation.argument.identity)
        ? represented.representation.argument.identity.identity
        : undefined
    return realization?.runnerIdentity ?? retainedIdentity
  }
  if (
    (expression._tag === 'InterfaceOperationCall' || expression._tag === 'BuiltinCall') &&
    expression.witnessEffectSite !== undefined
  )
    return Instances.effectIdentity(context.instance.key, expression.witnessEffectSite)
  if (
    expression._tag === 'BuiltinCall' &&
    Type.isEffect(
      Type.substitute(
        expression.type,
        context.instance.substitution,
        context.instance.specialization.compatibility,
      ),
    )
  )
    return Instances.effectIdentity(
      context.instance.key,
      Hir.builtinEffectSite(
        context.instance.function.declaration.id,
        context.instance.key.declaration,
        expression.span,
      ),
    )
  if (expression._tag === 'EffectBlock')
    return Instances.effectIdentity(context.instance.key, expression.site)
  if (expression._tag === 'EffectCatch')
    return Instances.effectIdentity(
      context.instance.key,
      Hir.effectCatchSite(
        context.instance.function.declaration.id,
        context.instance.key.declaration,
        expression.span,
      ),
    )
  if (expression._tag === 'ParameterReference')
    return Instances.parameterEffectIdentity(
      context.instance.function,
      context.instance.key,
      expression.parameter.ordinal,
    )
  // An erased section applied in place (`effect |> Effect.provideMut(&mut provider)`) is one
  // recorded call of the section's declaration, so its result Effect is identified exactly as a
  // direct call's. Without this, a provided execution that owns such a run has no runner identity
  // for it and its classification stays unknown forever.
  // The piped call is recorded at the same span, so the section's declaration is matched too;
  // together they select at most one record.
  if (
    expression._tag === 'CallableApply' &&
    expression.realization === 'DirectErasedSection' &&
    expression.callee._tag === 'CallableSection' &&
    expression.callee.target._tag === 'DeclarationCallableTarget'
  ) {
    const target = expression.callee.target.declaration
    return context.discovery.calls.find(
      (call) =>
        Instances.keyText(call.owner) === Instances.keyText(context.instance.key) &&
        call.target.declaration.module === target.module &&
        call.target.declaration.name === target.name &&
        call.span.sourceId === expression.span.sourceId &&
        call.span.start === expression.span.start &&
        call.span.end === expression.span.end,
    )?.resultEffect
  }
  if (expression._tag === 'Call' || expression._tag === 'EffectConstruct') {
    return context.discovery.calls.find(
      (call) =>
        Instances.keyText(call.owner) === Instances.keyText(context.instance.key) &&
        call.span.sourceId === expression.span.sourceId &&
        call.span.start === expression.span.start &&
        call.span.end === expression.span.end,
    )?.resultEffect
  }
  if (expression._tag === 'ServiceEffectConstruct')
    return serviceResultEffectOf(expression, context)
  return undefined
}

const providersOf = (
  expression: Hir.Expression,
  context: BuildContext,
): ReadonlyArray<Provider> => {
  if (expression._tag === 'BindingReference') {
    const initializer = context.bindings.get(expression.binding.ordinal)
    return initializer === undefined ? [] : providersOf(initializer, context)
  }
  if (expression._tag === 'Move') return providersOf(expression.subject, context)
  if (expression._tag === 'UnionConvert') return providersOf(expression.source, context)
  if (expression._tag !== 'EffectBindRequirement') return context.ambientProviders
  const proof = Instances.requirementSelection(context.instance, expression.provider)
  if (proof === undefined) return providersOf(expression.protected, context)
  const selected = proof.selected
  const capability = selected.capability
  const providerType = proof.provider
  if (capability === undefined || !Type.isNominal(capability) || !Type.isNominal(providerType))
    return providersOf(expression.protected, context)
  const witness =
    expression.provider.witness ?? ConformanceProof.witness(context.index, providerType, capability)
  return Object.freeze([
    ...providersOf(expression.protected, context),
    Object.freeze({
      capability,
      providerType,
      role: selected.role,
      requirementAccess: selected.access,
      access: expression.provider.selectionAccess,
      ...(witness === undefined ? {} : { witness }),
    }),
  ])
}

const witnessExpressionAt = (
  instance: Instances.Instance,
  site: Hir.EffectSiteId,
):
  | Extract<Hir.Expression, { readonly _tag: 'InterfaceOperationCall' | 'BuiltinCall' }>
  | undefined =>
  instance.function.statements
    .flatMap(Hir.statementExpressions)
    .flatMap(Hir.expressionTree)
    .find(
      (
        expression,
      ): expression is Extract<
        Hir.Expression,
        { readonly _tag: 'InterfaceOperationCall' | 'BuiltinCall' }
      > =>
        (expression._tag === 'InterfaceOperationCall' || expression._tag === 'BuiltinCall') &&
        expression.witnessEffectSite !== undefined &&
        Hir.sameExecutableSite(expression.witnessEffectSite, site),
    )

const builtinExpressionAt = (
  instance: Instances.Instance,
  site: Hir.EffectSiteId,
): Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' }> | undefined =>
  instance.function.statements
    .flatMap(Hir.statementExpressions)
    .flatMap(Hir.expressionTree)
    .find(
      (expression): expression is Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' }> =>
        expression._tag === 'BuiltinCall' &&
        expression.witnessEffectSite === undefined &&
        Hir.sameExecutableSite(
          Hir.builtinEffectSite(
            instance.function.declaration.id,
            instance.key.declaration,
            expression.span,
          ),
          site,
        ),
    )

const runnerOf = (
  expression: Hir.Expression,
  context: BuildContext,
  resolved?: {
    readonly identity: string
    readonly effect: Type.Effect
    readonly providers: ReadonlyArray<Provider>
  },
): Runner => {
  const stored = resolved === undefined ? storedEffectRealizationOf(expression, context) : undefined
  const identity = resolved?.identity ?? effectIdentityOf(expression, context)
  const environment = context.layout.effectEnvironments.find(
    (candidate) =>
      candidate._tag === 'EffectEnvironment' &&
      identity !== undefined &&
      Instances.effectIdentity(candidate.instance, candidate.site) === identity,
  )
  const expressionType =
    'type' in expression
      ? Type.substitute(
          expression.type,
          context.instance.substitution,
          context.instance.specialization.compatibility,
        )
      : undefined
  const expressionContract =
    expressionType !== undefined && Type.isRepresented(expressionType)
      ? expressionType.contract
      : expressionType
  const effect =
    resolved?.effect ??
    stored?.contract ??
    (environment?._tag === 'EffectEnvironment' ? environment.effect : undefined) ??
    (expressionContract !== undefined && Type.isEffect(expressionContract)
      ? expressionContract
      : undefined)
  if (effect === undefined)
    throw new RangeError('Provisional runner requires a resolved Effect contract')
  const availableProviders = resolved?.providers ?? providersOf(expression, context)
  const providers = Object.freeze(
    Type.requirementMembers(effect).flatMap((requirement) => {
      const selected = availableProviders.find(
        (provider) =>
          provider.role === requirement.role &&
          Type.equals(provider.capability, requirement.capability) &&
          (requirement.access === 'Shared' ||
            provider.access === 'Exclusive' ||
            provider.access === 'Take'),
      )
      return selected === undefined
        ? []
        : [Object.freeze({ ...selected, requirementAccess: requirement.access })]
    }),
  )
  if (environment?._tag !== 'EffectEnvironment' || identity === undefined) {
    return Object.freeze({
      execution: Object.freeze({
        _tag: 'UnknownExecution',
        identity: `${context.instance.key.declaration.module}:${expression.span.start}`,
      }),
      classification: 'Unknown',
      typeArguments: Object.freeze([]),
      outcome: effect,
      captures: Object.freeze([]),
      providers,
    })
  }
  const environmentOwner = context.discovery.instances.find(
    (candidate) => Instances.keyText(candidate.key) === Instances.keyText(environment.instance),
  )
  const witness =
    environmentOwner === undefined
      ? undefined
      : witnessExpressionAt(environmentOwner, environment.site)
  let executionKind:
    | 'EffectRunnerExecution'
    | 'WitnessEffectRunnerExecution'
    | 'BuiltinEffectRunnerExecution' = 'EffectRunnerExecution'
  if (witness !== undefined) executionKind = 'WitnessEffectRunnerExecution'
  else if (
    environmentOwner !== undefined &&
    builtinExpressionAt(environmentOwner, environment.site) !== undefined
  )
    executionKind = 'BuiltinEffectRunnerExecution'
  const baseExecution: ExecutionKey = Object.freeze({
    _tag: executionKind,
    owner: stored?.runnerInstance ?? environment.instance,
    site: stored?.site ?? environment.site,
    identity,
    runner:
      stored?.runner ?? Hir.effectRunnerId(environment.instance.declaration, environment.site),
  })
  const providedIdentity =
    providers.length === 0
      ? undefined
      : `${identity}\u0006${providers.map(providerKey).join('\u0000')}`
  const execution: ExecutionKey =
    providedIdentity === undefined
      ? baseExecution
      : Object.freeze({
          _tag: 'ProvidedEffectRunnerExecution',
          owner: environment.instance,
          site: environment.site,
          identity: providedIdentity,
          effectIdentity: identity,
          runner: baseExecution.runner,
          providers,
        })
  let baseClassification: Classification
  if (stored === undefined) {
    baseClassification =
      context.effectClassifications.get(identity) ??
      classificationOfEffect(context.discovery, identity)
  } else if (stored.suspendable) {
    baseClassification = 'Suspendable'
  } else {
    baseClassification = 'Synchronous'
  }
  const providedClassification = (): Classification => {
    const fixedProvided =
      providedIdentity === undefined
        ? undefined
        : context.effectClassifications.get(providedIdentity)
    if (fixedProvided !== undefined) return fixedProvided
    if (providedIdentity === undefined || baseClassification === 'Suspendable')
      return baseClassification
    const owner = context.discovery.instances.find(
      (candidate) => Instances.keyText(candidate.key) === Instances.keyText(environment.instance),
    )
    if (owner === undefined) return 'Unknown'
    const block = owner.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .find(
        (candidate) =>
          candidate._tag === 'EffectBlock' &&
          Hir.sameExecutableSite(candidate.site, environment.site),
      )
    // Provider substitution can only change suspendability through service constructions in an
    // explicit effect block. Bound-operation and forwarded environments have no such local
    // constructions; their discovered effect identity already carries the exact witness result.
    if (block?._tag !== 'EffectBlock') return baseClassification
    const specializedContext: BuildContext = {
      ...context,
      instance: owner,
      bindings: new Map([...bindingsOf(owner.function), ...bindingsOfStatements(block.statements)]),
      ambientProviders: providers,
    }
    if (
      block.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .some(
          (candidate) =>
            candidate._tag === 'Run' && isSuspendOrigin(candidate.subject, specializedContext),
        )
    )
      return 'Suspendable'
    for (const service of block.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .filter(
        (
          candidate,
        ): candidate is Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }> =>
          candidate._tag === 'ServiceEffectConstruct',
      )) {
      const resultEffect = serviceResultEffectOf(service, specializedContext)
      if (
        resultEffect !== undefined &&
        (context.effectClassifications.get(resultEffect) === 'Suspendable' ||
          suspendableSummary(Instances.effectSuspensionOf(context.discovery, resultEffect)))
      )
        return 'Suspendable'
      const capability = Type.substitute(
        service.service,
        owner.substitution,
        owner.specialization.compatibility,
      )
      const selected = providers.find(
        (provider) =>
          Type.equals(provider.capability, capability) && provider.role === service.role,
      )
      if (selected?.witness?._tag !== 'SourceConformanceWitness') return 'Unknown'
      const implementation = ConformanceProof.witnessOperation(selected.witness, service.operation)
      if (implementation === undefined) return 'Unknown'
      const candidates = Instances.matchingSpecialization(context.discovery, {
        declaration: implementation,
        typeArguments: selected.witness.typeArguments,
      })
      if (candidates.length === 0) return 'Unknown'
      if (
        candidates.some(
          (candidate) =>
            suspendableSummary(Instances.suspensionOf(context.discovery, candidate.key)) ||
            (candidate.resultEffect !== undefined &&
              suspendableSummary(
                Instances.effectSuspensionOf(context.discovery, candidate.resultEffect),
              )),
        )
      )
        return 'Suspendable'
    }
    return 'Synchronous'
  }
  return Object.freeze({
    execution,
    classification: providedClassification(),
    declaration: execution.runner,
    instance: stored?.runnerInstance ?? environment.instance,
    effectIdentity: identity,
    ...(providedIdentity === undefined ? {} : { providedIdentity }),
    typeArguments: stored?.runnerArguments ?? environment.instance.typeArguments,
    outcome: stored?.contract ?? environment.effect,
    captures: Object.freeze(
      environment.fields.map((field, ordinal) =>
        Object.freeze({
          ordinal,
          source: field.source,
          sourceOrdinal: field.ordinal,
          access: field.access,
          type: field.type,
        }),
      ),
    ),
    providers,
  })
}

const reifyPolicy = (
  outcome: Type.Effect,
  context: BuildContext,
): Extract<CompletionPolicy, { readonly _tag: 'Reify' }> | undefined => {
  const failureValueType = Type.failureValue(Type.failureMembers(outcome))
  const successType = outcome.success
  const outcomeShape = Layout.callingShape(context.layout, outcome)
  const successShape = Layout.callingShape(context.layout, successType)
  const failureValueShape = Layout.callingShape(context.layout, failureValueType)
  if (outcomeShape === undefined || successShape === undefined || failureValueShape === undefined)
    return undefined
  return Object.freeze({
    _tag: 'Reify',
    outcome,
    successType,
    failureValueType,
    successShape,
    outcomeShape,
    failureValueShape,
  })
}

const isSuspendOrigin = (expression: Hir.Expression, context: BuildContext): boolean => {
  if (expression._tag === 'BindingReference') {
    const initializer = context.bindings.get(expression.binding.ordinal)
    return initializer !== undefined && isSuspendOrigin(initializer, context)
  }
  if (expression._tag === 'Move') return isSuspendOrigin(expression.subject, context)
  if (expression._tag === 'UnionConvert') return isSuspendOrigin(expression.source, context)
  if (expression._tag === 'EffectBindRequirement')
    return isSuspendOrigin(expression.protected, context)
  return expression._tag === 'BuiltinCall' && expression.operation === 'EffectSuspend'
}

const deferredOf = (
  expression: Hir.Expression,
  context: BuildContext,
): Hir.Expression | undefined => {
  if (expression._tag === 'BindingReference') {
    const initializer = context.bindings.get(expression.binding.ordinal)
    return initializer === undefined ? undefined : deferredOf(initializer, context)
  }
  if (expression._tag === 'Move') return deferredOf(expression.subject, context)
  if (expression._tag === 'UnionConvert') return deferredOf(expression.source, context)
  if (expression._tag === 'EffectBindRequirement') return deferredOf(expression.protected, context)
  return expression._tag === 'BuiltinCall' && expression.operation === 'EffectSuspend'
    ? expression.arguments.at(0)
    : undefined
}

const effectCatchOf = (
  expression: Hir.Expression,
  context: BuildContext,
  resolving: ReadonlySet<number> = new Set(),
): Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }> | undefined => {
  if (expression._tag === 'BindingReference') {
    const ordinal = expression.binding.ordinal
    if (resolving.has(ordinal)) return undefined
    const initializer = context.bindings.get(ordinal)
    return initializer === undefined
      ? undefined
      : effectCatchOf(initializer, context, new Set(resolving).add(ordinal))
  }
  if (expression._tag === 'Move') return effectCatchOf(expression.subject, context, resolving)
  return expression._tag === 'EffectCatch' ? expression : undefined
}

const runSpanOfCatch = (
  statements: ReadonlyArray<Hir.Statement>,
  target: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>,
  context: BuildContext,
): SourceSpan.SourceSpan =>
  statements
    .flatMap(Hir.statementExpressions)
    .flatMap(Hir.expressionTree)
    .find(
      (candidate) =>
        candidate._tag === 'Run' && effectCatchOf(candidate.subject, context) === target,
    )?.span ?? target.span

const catchHandlerRunner = (
  expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>,
  context: BuildContext,
): Runner | undefined => {
  if (expression.handler._tag === 'Unavailable') return undefined
  const catchIdentity = effectIdentityOf(expression, context)
  const catchEffect = context.discovery.effects.find(
    (candidate) => candidate.identity === catchIdentity,
  )
  // The catch environment always stores [protected effect, handler callable], but the handler is
  // selected by its callable identity rather than by position so a capture-layout change cannot
  // silently misclassify the handler as synchronous.
  const callableIdentity = catchEffect?.captures.find(
    (capture) => capture.callableIdentity !== undefined,
  )?.callableIdentity
  const handlerType = Type.substitute(
    expression.handler.type,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  if (
    callableIdentity?.target._tag !== 'Declaration' ||
    !Type.isCallable(handlerType) ||
    !Type.isEffect(handlerType.result)
  )
    return undefined
  const declaration: DeclarationFacts.CanonicalId = Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: callableIdentity.target.module,
    name: callableIdentity.target.name,
  })
  const identities = [
    ...new Set(
      Instances.matchingSpecialization(context.discovery, {
        declaration,
        typeArguments: callableIdentity.typeArguments,
      }).flatMap((candidate) =>
        candidate.resultEffect === undefined ? [] : [candidate.resultEffect],
      ),
    ),
  ]
  const identity = identities.length === 1 ? identities.at(0) : undefined
  return identity === undefined
    ? undefined
    : runnerOf(expression.handler, context, {
        identity,
        effect: handlerType.result,
        providers: context.ambientProviders,
      })
}

const controlsOfCatch = (
  expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>,
  execution: ExecutionKey,
  context: BuildContext,
  ordinalOffset = 0,
  runSpan = expression.span,
): ReadonlyArray<Region> => {
  const regions: Array<Region> = []
  if (expression.protected._tag === 'Unavailable') return Object.freeze(regions)
  const protectedEffect = Type.substitute(
    expression.protected.type,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  const resultEffect = Type.substitute(
    expression.type,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  if (!Type.isEffect(protectedEffect) || !Type.isEffect(resultEffect)) return Object.freeze([])

  const protectedRunner = runnerOf(expression.protected, context)
  const selected = Type.substitute(
    expression.selected,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  if (Type.isNever(selected)) {
    if (protectedRunner.classification === 'Synchronous') return Object.freeze(regions)
    const policy: Extract<CompletionPolicy, { readonly _tag: 'Propagate' }> = Object.freeze({
      _tag: 'Propagate',
      outcome: protectedRunner.outcome,
      failureMappings: Object.freeze(
        Type.failureMembers(protectedRunner.outcome).map((_failure, source) =>
          Object.freeze({ source: source + 1, target: source + 1 }),
        ),
      ),
    })
    const id = controlId(execution, runSpan, ordinalOffset, 'Invoke')
    const complete = controlId(execution, runSpan, ordinalOffset, 'Complete')
    return Object.freeze([
      Object.freeze({
        _tag: 'ProvisionalRegion',
        id,
        outcome: Object.freeze({
          _tag: 'RunSuspendableEffect',
          runner: protectedRunner,
          completion: policy,
          complete,
          relay: Object.freeze({
            _tag: 'RelayExistingTransfer',
            preserves: ['Child', 'Origin', 'TypedOutcome'] as const,
          }),
          span: runSpan,
        }),
      }),
      Object.freeze({
        _tag: 'ProvisionalRegion',
        id: complete,
        outcome: Object.freeze({ _tag: 'Complete', policy }),
      }),
    ])
  }
  const protectedPolicy = reifyPolicy(protectedRunner.outcome, context)
  if (protectedRunner.classification !== 'Synchronous' && protectedPolicy !== undefined) {
    const id = controlId(execution, expression.span, ordinalOffset, 'Invoke')
    const complete = controlId(execution, expression.span, ordinalOffset, 'Complete')
    regions.push(
      Object.freeze({
        _tag: 'ProvisionalRegion',
        id,
        outcome: Object.freeze({
          _tag: 'RunSuspendableEffect',
          runner: protectedRunner,
          completion: protectedPolicy,
          complete,
          relay: Object.freeze({
            _tag: 'RelayExistingTransfer',
            preserves: ['Child', 'Origin', 'TypedOutcome'] as const,
          }),
          span: expression.span,
        }),
      }),
      Object.freeze({
        _tag: 'ProvisionalRegion',
        id: complete,
        outcome: Object.freeze({ _tag: 'Complete', policy: protectedPolicy }),
      }),
    )
  }

  const handlerRunner = catchHandlerRunner(expression, context)
  if (
    handlerRunner?.classification !== 'Suspendable' &&
    handlerRunner?.classification !== 'Unknown'
  )
    return Object.freeze(regions)
  const mappings = Type.failureMembers(handlerRunner.outcome).flatMap((failure, source) => {
    const target = Type.failureMembers(resultEffect).findIndex((candidate) =>
      Type.equals(candidate, failure),
    )
    return target < 0 ? [] : [Object.freeze({ source: source + 1, target: target + 1 })]
  })
  if (mappings.length !== Type.failureMembers(handlerRunner.outcome).length)
    return Object.freeze(regions)
  const policy: Extract<CompletionPolicy, { readonly _tag: 'Propagate' }> = Object.freeze({
    _tag: 'Propagate',
    outcome: handlerRunner.outcome,
    failureMappings: Object.freeze(mappings),
  })
  const id = controlId(execution, expression.span, ordinalOffset + 1, 'Invoke')
  const complete = controlId(execution, expression.span, ordinalOffset + 1, 'Complete')
  regions.push(
    Object.freeze({
      _tag: 'ProvisionalRegion',
      id,
      outcome: Object.freeze({
        _tag: 'RunSuspendableEffect',
        runner: handlerRunner,
        completion: policy,
        complete,
        relay: Object.freeze({
          _tag: 'RelayExistingTransfer',
          preserves: ['Child', 'Origin', 'TypedOutcome'] as const,
        }),
        span: expression.span,
      }),
    }),
    Object.freeze({
      _tag: 'ProvisionalRegion',
      id: complete,
      outcome: Object.freeze({ _tag: 'Complete', policy }),
    }),
  )
  return Object.freeze(regions)
}

const controlsOfExpressions = (
  expressions: ReadonlyArray<Hir.Expression>,
  execution: ExecutionKey,
  executionClassification: Classification,
  context: BuildContext,
): ReadonlyArray<Region> => {
  const regions: Array<Region> = []
  let ordinal = 0
  const maySpecializeProviders =
    execution._tag === 'ProvidedEffectRunnerExecution' ||
    expressions
      .flatMap(Hir.expressionTree)
      .some((candidate) => candidate._tag === 'EffectBindRequirement')
  const visit = (expression: Hir.Expression): void => {
    if (expression._tag === 'EffectBlock') return
    if (expression._tag === 'Run') {
      const idOrdinal = ordinal
      ordinal += 1
      const caught = effectCatchOf(expression.subject, context)
      if (caught !== undefined) {
        regions.push(...controlsOfCatch(caught, execution, context, idOrdinal, expression.span))
        ordinal += 1
        return
      }
      if (isSuspendOrigin(expression.subject, context)) {
        const deferred = deferredOf(expression.subject, context)
        if (deferred !== undefined) {
          const id = controlId(execution, expression.span, idOrdinal, 'Origin')
          regions.push(
            Object.freeze({
              _tag: 'ProvisionalRegion',
              id,
              outcome: Object.freeze({
                _tag: 'SuspendEffect',
                deferred: runnerOf(deferred, context),
                transfer: Object.freeze({ _tag: 'OriginateTransfer' }),
                span: expression.span,
              }),
            }),
          )
        }
      } else {
        const protected_ = expression.subject
        const storedSuspendable =
          storedEffectRealizationOf(protected_, context)?.suspendable === true
        const runner = runnerOf(protected_, context)
        const fixedClassification = context.effectClassifications.get(
          effectIdentityOf(protected_, context) ?? '',
        )
        // The concrete fixed point proves that no call or run in this execution can transfer.
        // Stored Effects add their realization's suspendability dependency after the ordinary
        // execution fixed point, so that exact runner may still require relay control here.
        if (
          executionClassification === 'Synchronous' &&
          !maySpecializeProviders &&
          !storedSuspendable &&
          (fixedClassification === undefined || fixedClassification === 'Synchronous')
        ) {
          for (const child of Hir.expressionChildren(expression)) visit(child)
          return
        }
        if (runner.classification !== 'Synchronous') {
          const policy = Object.freeze({
            _tag: 'Propagate' as const,
            outcome: runner.outcome,
            failureMappings: Object.freeze(
              Type.failureMembers(runner.outcome).map((_failure, source) =>
                Object.freeze({ source: source + 1, target: source + 1 }),
              ),
            ),
          })
          if (policy !== undefined) {
            const id = controlId(execution, expression.span, idOrdinal, 'Invoke')
            const complete = controlId(execution, expression.span, idOrdinal, 'Complete')
            regions.push(
              Object.freeze({
                _tag: 'ProvisionalRegion',
                id,
                outcome: Object.freeze({
                  _tag: 'RunSuspendableEffect',
                  runner,
                  completion: policy,
                  complete,
                  relay: Object.freeze({
                    _tag: 'RelayExistingTransfer',
                    preserves: ['Child', 'Origin', 'TypedOutcome'] as const,
                  }),
                  span: expression.span,
                }),
              }),
              Object.freeze({
                _tag: 'ProvisionalRegion',
                id: complete,
                outcome: Object.freeze({ _tag: 'Complete', policy }),
              }),
            )
          }
        }
      }
    }
    for (const child of Hir.expressionChildren(expression)) visit(child)
  }
  for (const expression of expressions) visit(expression)
  return Object.freeze(regions)
}

const controlsOf = (
  statements: ReadonlyArray<Hir.Statement>,
  execution: ExecutionKey,
  classification: Classification,
  context: BuildContext,
): ReadonlyArray<Region> =>
  controlsOfExpressions(
    statements.flatMap(Hir.statementExpressions),
    execution,
    classification,
    context,
  )

const builtinExecution = (
  expression: Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' }>,
  context: BuildContext,
): Execution | undefined => {
  const effect = Type.substitute(
    expression.type,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  if (!Type.isEffect(effect)) return undefined
  const site = Hir.builtinEffectSite(
    context.instance.function.declaration.id,
    context.instance.key.declaration,
    expression.span,
  )
  const identity = Instances.effectIdentity(context.instance.key, site)
  const key: ExecutionKey = Object.freeze({
    _tag: 'BuiltinEffectRunnerExecution',
    owner: context.instance.key,
    site,
    identity,
    runner: Hir.effectRunnerId(context.instance.key.declaration, site),
  })
  const classification = classificationOfEffect(context.discovery, identity)
  const regions = controlsOfExpressions(
    [
      Object.freeze({
        _tag: 'Run',
        subject: expression,
        type: effect.success,
        span: expression.span,
      }),
    ],
    key,
    classification,
    context,
  )
  return Object.freeze({
    _tag: 'ProvisionalExecution',
    key,
    classification: classificationWithRegions(classification, regions),
    regions,
  })
}

const providedRunnersOf = (
  statements: ReadonlyArray<Hir.Statement>,
  context: BuildContext,
): ReadonlyArray<Runner> => {
  const runners: Array<Runner> = []
  const visit = (expression: Hir.Expression): void => {
    if (expression._tag === 'EffectBlock') return
    if (expression._tag === 'Run') {
      const protected_ = expression.subject
      const runner = runnerOf(protected_, context)
      if (runner.execution._tag === 'ProvidedEffectRunnerExecution') runners.push(runner)
    }
    for (const child of Hir.expressionChildren(expression)) visit(child)
  }
  for (const statement of statements)
    for (const expression of Hir.statementExpressions(statement)) visit(expression)
  return Object.freeze(runners)
}

/** The adapter itself runs the selected implementation's returned Effect. */
const witnessExecution = (
  expression: Extract<Hir.Expression, { readonly _tag: 'InterfaceOperationCall' | 'BuiltinCall' }>,
  context: BuildContext,
): Execution | undefined => {
  const site = expression.witnessEffectSite
  const bound =
    expression._tag === 'InterfaceOperationCall' ? expression : expression.interfaceOperation
  if (site === undefined || bound === undefined) return undefined
  const capability = Type.substitute(
    bound.capability,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  const provider = Type.substitute(
    bound.provider,
    context.instance.substitution,
    context.instance.specialization.compatibility,
  )
  if (!Type.isNominal(capability)) return undefined
  const selected = ConformanceProof.interfaceWitnessTarget(
    context.index,
    provider,
    capability,
    bound.operation,
  )
  const key: ExecutionKey = Object.freeze({
    _tag: 'WitnessEffectRunnerExecution',
    owner: context.instance.key,
    site,
    identity: Instances.effectIdentity(context.instance.key, site),
    runner: Hir.effectRunnerId(context.instance.key.declaration, site),
  })
  const regions: Array<Region> = []
  let classification: Classification = 'Unknown'
  if (selected === undefined) {
    const intrinsic = ConformanceProof.interfaceOperationIntrinsic(
      context.index,
      provider,
      capability,
      bound.operation,
    )
    if (intrinsic?.rule._tag !== 'BuiltinRule') return undefined
    classification = classificationOfEffect(context.discovery, key.identity)
  } else {
    const candidates = Instances.matchingSpecialization(context.discovery, {
      declaration: selected.implementation,
      typeArguments: selected.typeArguments,
    })
    const target = candidates.length === 1 ? candidates.at(0) : undefined
    if (target === undefined) return undefined
    if (target.function.declaration.functionKind === 'Ordinary') {
      classification = suspendableSummary(
        Instances.executionSuspensionOf(context.discovery, target.key),
      )
        ? 'Suspendable'
        : 'Synchronous'
    } else if (target.resultEffect !== undefined) {
      const result = target.specialization.result
      const effect = Type.isRepresented(result) ? result.contract : result
      if (!Type.isEffect(effect)) return undefined
      const runner = runnerOf(expression, context, {
        identity: target.resultEffect,
        effect,
        providers: context.ambientProviders,
      })
      if (runner.execution._tag === 'UnknownExecution') return undefined
      classification = runner.classification
      if (classification !== 'Synchronous') {
        const policy: CompletionPolicy = Object.freeze({
          _tag: 'Propagate',
          outcome: runner.outcome,
          failureMappings: Object.freeze(
            Type.failureMembers(runner.outcome).map((_failure, ordinal) =>
              Object.freeze({ source: ordinal + 1, target: ordinal + 1 }),
            ),
          ),
        })
        const complete = controlId(key, expression.span, 0, 'Complete')
        regions.push(
          Object.freeze({
            _tag: 'ProvisionalRegion',
            id: controlId(key, expression.span, 0, 'Invoke'),
            outcome: Object.freeze({
              _tag: 'RunSuspendableEffect',
              runner,
              completion: policy,
              complete,
              relay: Object.freeze({
                _tag: 'RelayExistingTransfer',
                preserves: ['Child', 'Origin', 'TypedOutcome'] as const,
              }),
              span: expression.span,
            }),
          }),
          Object.freeze({
            _tag: 'ProvisionalRegion',
            id: complete,
            outcome: Object.freeze({ _tag: 'Complete', policy }),
          }),
        )
      }
    }
  }
  return Object.freeze({
    _tag: 'ProvisionalExecution',
    key,
    classification,
    regions: Object.freeze(regions),
  })
}

const classificationWithRegions = (
  base: Classification,
  regions: ReadonlyArray<Region>,
): Classification =>
  regions.reduce<Classification>((classification, region) => {
    if (region.outcome._tag !== 'RunSuspendableEffect') return classification
    if (region.outcome.runner.classification === 'Suspendable') return 'Suspendable'
    return classification === 'Synchronous' ? 'Unknown' : classification
  }, base)

/** Builds deterministic provisional control without producing executable final MIR. */
export const build = (
  discovery: Instances.Discovery,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
): Module => {
  const buildPass = (effectClassifications: ReadonlyMap<string, Classification>): Module => {
    const executions: Array<Execution> = []
    const observedProvided: Array<Runner> = []
    for (const instance of discovery.instances) {
      const context: BuildContext = {
        discovery,
        layout,
        index,
        instance,
        bindings: bindingsOf(instance.function),
        effectClassifications,
        ambientProviders: Object.freeze([]),
      }
      const instanceKey: ExecutionKey = Object.freeze({
        _tag: 'InstanceExecution',
        instance: instance.key,
        functionOrdinal: instance.function.declaration.id.ordinal,
        identity: Instances.keyText(instance.key),
      })
      const instanceClassification: Classification = suspendableSummary(
        Instances.executionSuspensionOf(discovery, instance.key),
      )
        ? 'Suspendable'
        : 'Synchronous'
      const instanceRegions = controlsOf(
        instance.function.statements,
        instanceKey,
        instanceClassification,
        context,
      )
      observedProvided.push(...providedRunnersOf(instance.function.statements, context))
      executions.push(
        Object.freeze({
          _tag: 'ProvisionalExecution',
          key: instanceKey,
          classification: classificationWithRegions(instanceClassification, instanceRegions),
          regions: instanceRegions,
        }),
      )
      const expressions = instance.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
      // A directly run intrinsic is executed in its caller. Only first-class builtin
      // values acquire the separate generated runner and its own control authority.
      const directlyRunBuiltins = new Set(
        expressions.flatMap((expression) =>
          expression._tag === 'Run' && expression.subject._tag === 'BuiltinCall'
            ? [expression.subject]
            : [],
        ),
      )
      for (const expression of expressions) {
        if (
          (expression._tag === 'InterfaceOperationCall' || expression._tag === 'BuiltinCall') &&
          expression.witnessEffectSite !== undefined
        ) {
          const execution = witnessExecution(expression, context)
          if (execution !== undefined) executions.push(execution)
          continue
        }
        if (expression._tag === 'BuiltinCall') {
          if (directlyRunBuiltins.has(expression)) continue
          const execution = builtinExecution(expression, context)
          if (execution !== undefined) executions.push(execution)
          continue
        }
        if (expression._tag !== 'EffectBlock' && expression._tag !== 'EffectCatch') continue
        const site =
          expression._tag === 'EffectBlock'
            ? expression.site
            : Hir.effectCatchSite(
                instance.function.declaration.id,
                instance.key.declaration,
                expression.span,
              )
        const identity = Instances.effectIdentity(instance.key, site)
        const key: ExecutionKey = Object.freeze({
          _tag: 'EffectRunnerExecution',
          owner: instance.key,
          site,
          identity,
          runner: Hir.effectRunnerId(instance.key.declaration, site),
        })
        const runnerClassification =
          effectClassifications.get(identity) ?? classificationOfEffect(discovery, identity)
        const runnerContext: BuildContext = {
          ...context,
          bindings:
            expression._tag === 'EffectBlock'
              ? new Map([...context.bindings, ...bindingsOfStatements(expression.statements)])
              : context.bindings,
        }
        const regions =
          expression._tag === 'EffectBlock'
            ? controlsOf(expression.statements, key, runnerClassification, runnerContext)
            : controlsOfCatch(
                expression,
                key,
                runnerContext,
                0,
                runSpanOfCatch(instance.function.statements, expression, runnerContext),
              )
        if (expression._tag === 'EffectBlock')
          observedProvided.push(...providedRunnersOf(expression.statements, runnerContext))
        else
          observedProvided.push(
            ...regions.flatMap((region) =>
              region.outcome._tag === 'RunSuspendableEffect' &&
              region.outcome.runner.execution._tag === 'ProvidedEffectRunnerExecution'
                ? [region.outcome.runner]
                : [],
            ),
          )
        const specializedClassification = classificationWithRegions(runnerClassification, regions)
        executions.push(
          Object.freeze({
            _tag: 'ProvisionalExecution',
            key,
            classification: specializedClassification,
            regions,
          }),
        )
      }
    }

    // Provider-specialized runners reuse the authored Effect block, but they are distinct
    // executable instances: provider dispatch can make a source-synchronous service call
    // suspendable. Materialize provisional control for every represented provided runner reached
    // by the pass so final MIR never borrows the open base runner's (potentially empty) regions.
    const pendingProvided = [...observedProvided]
    const represented = new Set(executions.map((execution) => executionIdentity(execution.key)))
    const visitedProvided = new Set<string>()
    for (let ordinal = 0; ordinal < pendingProvided.length; ordinal += 1) {
      const runner = pendingProvided.at(ordinal)
      if (runner?.execution._tag !== 'ProvidedEffectRunnerExecution') continue
      const key = runner.execution
      if (visitedProvided.has(key.identity)) continue
      visitedProvided.add(key.identity)
      if (represented.has(key.identity)) continue
      const owner = discovery.instances.find(
        (candidate) => Instances.keyText(candidate.key) === Instances.keyText(key.owner),
      )
      const body = owner?.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .find((candidate) => {
          if (candidate._tag === 'EffectBlock')
            return Hir.sameExecutableSite(candidate.site, key.site)
          if (candidate._tag !== 'EffectCatch') return false
          return Hir.sameExecutableSite(
            Hir.effectCatchSite(
              owner.function.declaration.id,
              owner.key.declaration,
              candidate.span,
            ),
            key.site,
          )
        })
      if (owner === undefined || (body?._tag !== 'EffectBlock' && body?._tag !== 'EffectCatch'))
        continue
      const specializesProviderDispatch =
        body._tag === 'EffectCatch' ||
        body.statements
          .flatMap(Hir.statementExpressions)
          .flatMap(Hir.expressionTree)
          .some(
            (expression) =>
              expression._tag === 'ServiceEffectConstruct' ||
              expression._tag === 'EffectBindRequirement',
          )
      const context: BuildContext = {
        discovery,
        layout,
        index,
        instance: owner,
        bindings:
          body._tag === 'EffectBlock'
            ? new Map([...bindingsOf(owner.function), ...bindingsOfStatements(body.statements)])
            : bindingsOf(owner.function),
        effectClassifications,
        ambientProviders: key.providers,
      }
      const regions =
        body._tag === 'EffectBlock'
          ? controlsOf(body.statements, key, runner.classification, context)
          : controlsOfCatch(
              body,
              key,
              context,
              0,
              runSpanOfCatch(owner.function.statements, body, context),
            )
      if (body._tag === 'EffectBlock')
        pendingProvided.push(...providedRunnersOf(body.statements, context))
      const relaysProvidedRunner = regions.some(
        (region) =>
          region.outcome._tag === 'RunSuspendableEffect' &&
          region.outcome.runner.execution._tag === 'ProvidedEffectRunnerExecution',
      )
      // A provider can affect this block either at a local service/binding operation or through a
      // nested provided runner. Always scan transparent blocks above, but reuse their open
      // execution when the provider changes neither local dispatch nor relay control. In
      // particular, this avoids duplicating direct suspension origins whose allocator provider is
      // consumed only by the suspension primitive itself.
      if (!specializesProviderDispatch && !relaysProvidedRunner) continue
      represented.add(key.identity)
      const execution: Execution = Object.freeze({
        _tag: 'ProvisionalExecution',
        key,
        classification: classificationWithRegions(runner.classification, regions),
        regions,
      })
      executions.push(execution)
      for (const region of regions) {
        const outcome = region.outcome
        if (
          outcome._tag === 'RunSuspendableEffect' &&
          outcome.runner.execution._tag === 'ProvidedEffectRunnerExecution'
        )
          pendingProvided.push(outcome.runner)
      }
    }
    return Object.freeze({
      _tag: 'ProvisionalMirModule',
      module: discovery.rootModule,
      executions: Object.freeze(executions),
    })
  }
  const classificationsOf = (self: Module): ReadonlyMap<string, Classification> => {
    const candidates = new Map<string, Array<Classification>>()
    const record = (identity: string, classification: Classification): void => {
      const current = candidates.get(identity) ?? []
      current.push(classification)
      candidates.set(identity, current)
    }
    for (const execution of self.executions) {
      if (execution.key._tag !== 'InstanceExecution')
        record(execution.key.identity, execution.classification)
      for (const region of execution.regions)
        if (region.outcome._tag === 'RunSuspendableEffect')
          record(region.outcome.runner.execution.identity, region.outcome.runner.classification)
    }
    return new Map(
      [...candidates].map(([identity, classifications]) => [
        identity,
        mergeClassifications(classifications),
      ]),
    )
  }
  const sameClassifications = (
    left: ReadonlyMap<string, Classification>,
    right: ReadonlyMap<string, Classification>,
  ): boolean =>
    left.size === right.size &&
    [...left].every(([identity, classification]) => right.get(identity) === classification)

  const joinClassifications = (
    current: ReadonlyMap<string, Classification>,
    observed: ReadonlyMap<string, Classification>,
  ): ReadonlyMap<string, Classification> => {
    const joined = new Map(current)
    for (const [identity, classification] of observed) {
      const previous = joined.get(identity)
      joined.set(
        identity,
        previous === undefined ? classification : mergeClassifications([previous, classification]),
      )
    }
    return joined
  }

  let classifications: ReadonlyMap<string, Classification> = new Map()
  let result = buildPass(classifications)
  const transitions = new Map<string, number>()
  // Each pass exhausts its finite provider-specialization worklist. Across passes, newly observed
  // identities come only from the finite specialized HIR/provider facts, while the joined safety
  // classification for each actual identity is monotone:
  // absent -> synchronous -> unknown -> suspendable.
  for (;;) {
    const next = joinClassifications(classifications, classificationsOf(result))
    if (sameClassifications(classifications, next)) return result
    for (const [identity, classification] of next) {
      if (classifications.get(identity) === classification) continue
      const count = (transitions.get(identity) ?? 0) + 1
      if (count > 3)
        throw new RangeError(`Provisional MIR classification exceeded its lattice for ${identity}`)
      transitions.set(identity, count)
    }
    classifications = next
    result = buildPass(classifications)
  }
}

/** Returns the exact provisional execution classification for one lowered MIR function. */
export const classificationOfExecution = (
  self: Module,
  instance: Instances.InstanceKey,
): Classification =>
  executionForInstance(self, instance)?.classification ??
  providedClassification(self, instance) ??
  'Unknown'

/**
 * Returns the selected runner classification at one source run. The lookup is deliberately scoped
 * by the exact monomorphic execution before comparing spans, so generic specializations and hidden
 * Effect runners at the same source site cannot share a normalization decision.
 */
export const classificationOfRun = (
  self: Module,
  instance: Instances.InstanceKey,
  span: SourceSpan.SourceSpan,
): Classification => {
  const execution = executionForInstance(self, instance)
  const control = execution?.regions.find(
    (region) =>
      region.outcome._tag === 'RunSuspendableEffect' && sameSpan(region.outcome.span, span),
  )
  let selected: Classification
  if (control?.outcome._tag === 'RunSuspendableEffect') {
    selected = control.outcome.runner.classification
  } else if (execution === undefined) {
    selected = 'Unknown'
  } else if (
    execution.classification === 'Synchronous' &&
    providedClassification(self, instance) === 'Suspendable'
  ) {
    selected = 'Suspendable'
  } else {
    // Control construction records every unknown or suspendable run in this execution.
    // An unknown sibling can widen the execution summary without changing this run.
    selected = 'Synchronous'
  }
  return selected
}

/** Explicit transfer origins cannot be normalized into synchronous direct calls. */
export const isOriginOfRun = (
  self: Module,
  instance: Instances.InstanceKey,
  span: SourceSpan.SourceSpan,
): boolean =>
  executionForInstance(self, instance)?.regions.some(
    (region) => region.outcome._tag === 'SuspendEffect' && sameSpan(region.outcome.span, span),
  ) ?? false

export interface RunControl {
  readonly id: ControlId
  readonly outcome: Extract<Outcome, { readonly _tag: 'RunSuspendableEffect' }>
}

/** Returns the exact provisional complete-or-relay control associated with one lowered source run. */
export const controlOfRun = (
  self: Module,
  instance: Instances.InstanceKey,
  span: SourceSpan.SourceSpan,
): RunControl | undefined => {
  const execution = executionForInstance(self, instance)
  const region = execution?.regions.find(
    (candidate) =>
      candidate.outcome._tag === 'RunSuspendableEffect' && sameSpan(candidate.outcome.span, span),
  )
  return region?.outcome._tag === 'RunSuspendableEffect'
    ? Object.freeze({ id: region.id, outcome: region.outcome })
    : undefined
}

/** Returns the classification of an exact generated runner identity, if provisional facts name it. */
export const classificationOfRunner = (
  self: Module,
  runner: DeclarationFacts.CanonicalId,
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): Classification => {
  const argumentKey = typeArguments.map(Type.genericArgumentKey).join('\u0000')
  const candidates = self.executions.flatMap((execution) => {
    if (
      execution.key._tag !== 'InstanceExecution' &&
      sameDeclaration(execution.key.runner, runner) &&
      execution.key.owner.typeArguments.map(Type.genericArgumentKey).join('\u0000') === argumentKey
    )
      return [execution.classification]
    return execution.regions.flatMap((region) => {
      const outcome = region.outcome
      return outcome._tag === 'RunSuspendableEffect' &&
        outcome.runner.declaration !== undefined &&
        sameDeclaration(outcome.runner.declaration, runner) &&
        outcome.runner.typeArguments.map(Type.genericArgumentKey).join('\u0000') === argumentKey
        ? [outcome.runner.classification]
        : []
    })
  })
  return mergeClassifications(candidates)
}

const controlIdText = (id: ControlId): string =>
  `${id.execution}@${id.sourceId}:${id.functionOrdinal}:${id.spanStart}:${id.spanEnd}#${id.ordinal}.${id.port}`

/** Deterministic inspection encoding with no compatibility promise. */
export const encode = (self: Module): string =>
  [
    `provisional-mir ${self.module}`,
    ...self.executions.flatMap((execution) => [
      `execution ${executionIdentity(execution.key)} ${execution.classification.toLowerCase()}`,
      ...execution.regions.map((region) => {
        const outcome = region.outcome
        if (outcome._tag === 'SuspendEffect')
          return `  ${controlIdText(region.id)} suspend child=${outcome.deferred.effectIdentity ?? 'unknown'} transfer=unpublished`
        if (outcome._tag === 'RunSuspendableEffect')
          return `  ${controlIdText(region.id)} run runner=${outcome.runner.effectIdentity ?? 'unknown'} completion=${outcome.completion._tag.toLowerCase()} complete=${controlIdText(outcome.complete)} relay=existing(${outcome.relay.preserves.join(',')})`
        return `  ${controlIdText(region.id)} complete policy=${outcome.policy._tag.toLowerCase()}`
      }),
    ]),
    '',
  ].join('\n')

export interface Violation {
  readonly _tag: 'ProvisionalMirViolation'
  readonly execution: string
  readonly detail: string
}

/** Verifies the provisional graph without making it consumable by final-MIR backends. */
export const verify = (self: Module): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = []
  for (const execution of self.executions) {
    const identity = executionIdentity(execution.key)
    const regions = new Map<string, Region>()
    for (const region of execution.regions) {
      const key = controlIdText(region.id)
      if (regions.has(key)) {
        violations.push(
          Object.freeze({
            _tag: 'ProvisionalMirViolation',
            execution: identity,
            detail: `duplicate provisional region ${key}`,
          }),
        )
      }
      regions.set(key, region)
    }
    for (const region of execution.regions) {
      const outcome = region.outcome
      if (outcome._tag !== 'RunSuspendableEffect') continue
      const target = regions.get(controlIdText(outcome.complete))
      if (target?.outcome._tag !== 'Complete') {
        violations.push(
          Object.freeze({
            _tag: 'ProvisionalMirViolation',
            execution: identity,
            detail: `suspendable run ${controlIdText(region.id)} has a dangling Complete target`,
          }),
        )
      } else if (target.outcome.policy._tag !== outcome.completion._tag) {
        violations.push(
          Object.freeze({
            _tag: 'ProvisionalMirViolation',
            execution: identity,
            detail: `suspendable run ${controlIdText(region.id)} changes its Complete policy`,
          }),
        )
      }
      if (outcome.runner.classification === 'Synchronous') {
        violations.push(
          Object.freeze({
            _tag: 'ProvisionalMirViolation',
            execution: identity,
            detail: `synchronous runner appears in suspendable control ${controlIdText(region.id)}`,
          }),
        )
      }
    }
  }
  return Object.freeze(violations)
}
