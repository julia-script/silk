import * as DeclarationIndex from './DeclarationIndex.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/**
 * Monomorphic suspension control before normalization and MIR-local liveness. This is deliberately
 * not a `Mir.Module`: verification, evaluation, and backends cannot consume provisional control.
 */

export type Classification = 'Synchronous' | 'Suspendable' | 'Unknown'

export type ExecutionKey =
  | {
      readonly _tag: 'InstanceExecution'
      readonly instance: Instances.InstanceKey
      readonly functionOrdinal: number
      readonly identity: string
    }
  | {
      readonly _tag: 'EffectRunnerExecution'
      readonly owner: Instances.InstanceKey
      readonly site: Hir.EffectSiteId
      readonly identity: string
      readonly runner: DeclarationIndex.CanonicalId
    }
  | {
      readonly _tag: 'ProvidedEffectRunnerExecution'
      readonly owner: Instances.InstanceKey
      readonly site: Hir.EffectSiteId
      readonly identity: string
      readonly effectIdentity: string
      readonly runner: DeclarationIndex.CanonicalId
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

export interface Capture {
  readonly ordinal: number
  readonly source: 'Binding' | 'Parameter'
  readonly sourceOrdinal: number
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
  readonly type: DeclarationIndex.SemanticType
}

export interface Provider {
  readonly capability: Type.Nominal
  readonly providerType: Type.Nominal
  readonly role: string
  readonly access: 'Shared' | 'Exclusive' | 'Take'
  readonly witness?: DeclarationIndex.ConformanceWitness
}

export interface Runner {
  readonly execution:
    | ExecutionKey
    | { readonly _tag: 'UnknownExecution'; readonly identity: string }
  readonly classification: Classification
  readonly declaration?: DeclarationIndex.CanonicalId
  readonly instance?: Instances.InstanceKey
  readonly effectIdentity?: string
  readonly providedIdentity?: string
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly outcome: Type.Effect
  readonly captures: ReadonlyArray<Capture>
  readonly providers: ReadonlyArray<Provider>
}

export type CompletionPolicy =
  | {
      readonly _tag: 'Propagate'
      readonly outcome: Type.Effect
      readonly failureMappings: ReadonlyArray<{ readonly source: number; readonly target: number }>
    }
  | {
      readonly _tag: 'Reify'
      readonly outcome: Type.Effect
      readonly resultType: Type.Nominal
      readonly resultField: DeclarationIndex.FieldId
      readonly resultUnion: Type.StructuralUnion
      readonly successType: Type.Nominal
      readonly successField: DeclarationIndex.FieldId
      readonly successTag: number
      readonly failureType: Type.Nominal
      readonly failureField: DeclarationIndex.FieldId
      readonly failureTag: number
      readonly failureValueType: Type.Type
      readonly resultShape: Layout.CallingShape
      readonly outcomeShape: Layout.CallingShape
      readonly failureValueShape: Layout.CallingShape
    }

export type Outcome =
  | {
      readonly _tag: 'SuspendEffect'
      readonly deferred: Runner
      /** An unpublished transfer is created; this does not start the deferred runner. */
      readonly transfer: {
        readonly _tag: 'OriginateUnpublishedTransfer'
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

const executionInstance = (key: ExecutionKey): Instances.InstanceKey =>
  key._tag === 'InstanceExecution'
    ? key.instance
    : Object.freeze({
        _tag: 'InstanceKey',
        declaration: key.runner,
        typeArguments: key.owner.typeArguments,
        contractRow: Object.freeze([
          ...key.owner.contractRow,
          `effect-site:${Hir.executableSiteKey(key.site)}`,
        ]),
      })

const sameInstance = (left: Instances.InstanceKey, right: Instances.InstanceKey): boolean =>
  Instances.keyText(left) === Instances.keyText(right)

const sameSpan = (left: SourceSpan.SourceSpan, right: SourceSpan.SourceSpan): boolean =>
  left.sourceId === right.sourceId && left.start === right.start && left.end === right.end

const providedBaseName = (name: string): string | undefined => {
  const marker = name.lastIndexOf('$provided$')
  return marker < 0 ? undefined : name.slice(0, marker)
}

const executionForInstance = (
  self: Module,
  instance: Instances.InstanceKey,
): Execution | undefined => {
  const exact = self.executions.find((execution) =>
    sameInstance(executionInstance(execution.key), instance),
  )
  if (exact !== undefined) return exact
  const baseName = providedBaseName(instance.declaration.name)
  if (baseName === undefined) return undefined
  return self.executions.find((execution) => {
    if (execution.key._tag === 'InstanceExecution') return false
    const executionKey = executionInstance(execution.key)
    return (
      executionKey.declaration.module === instance.declaration.module &&
      executionKey.declaration.name === baseName &&
      Instances.keyText(Object.freeze({ ...executionKey, declaration: instance.declaration })) ===
        Instances.keyText(instance)
    )
  })
}

/** Resolves the provisional execution corresponding to one exact lowered MIR function. */
export const executionOf = (self: Module, instance: Instances.InstanceKey): Execution | undefined =>
  executionForInstance(self, instance)

const mergeClassifications = (classifications: ReadonlyArray<Classification>): Classification =>
  classifications.includes('Suspendable')
    ? 'Suspendable'
    : classifications.includes('Unknown')
      ? 'Unknown'
      : classifications.includes('Synchronous')
        ? 'Synchronous'
        : 'Unknown'

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
      const key = outcome.runner.execution
      return key.runner.module === instance.declaration.module &&
        key.runner.name === baseName &&
        key.owner.typeArguments.map(Type.genericArgumentKey).join('\u0000') ===
          instance.typeArguments.map(Type.genericArgumentKey).join('\u0000')
        ? [outcome.runner.classification]
        : []
    }),
  )
  return classifications.length === 0 ? undefined : mergeClassifications(classifications)
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
): Classification =>
  identity === undefined
    ? 'Unknown'
    : Instances.isEffectSuspendable(discovery, identity)
      ? 'Suspendable'
      : 'Synchronous'

const sameDeclaration = (
  left: DeclarationIndex.CanonicalId,
  right: DeclarationIndex.CanonicalId,
): boolean => left.module === right.module && left.name === right.name

const providerKey = (provider: Provider): string =>
  `${Type.key(provider.capability)}@${provider.role}:${provider.access}:${Type.key(provider.providerType)}:${
    provider.witness?._tag ?? 'unknown'
  }`

const bindingsOfStatements = (
  statements: ReadonlyArray<Hir.Statement>,
): ReadonlyMap<number, Hir.Expression> =>
  new Map(
    statements.flatMap((statement) =>
      statement._tag === 'Bind'
        ? [[statement.binding.ordinal, statement.initializer] as const]
        : statement._tag === 'If'
          ? [...statement.taken, ...statement.otherwise].flatMap((nested) =>
              nested._tag === 'Bind' ? [[nested.binding.ordinal, nested.initializer] as const] : [],
            )
          : [],
    ),
  )

const bindingsOf = (fn: Hir.HirFunction): ReadonlyMap<number, Hir.Expression> =>
  bindingsOfStatements(fn.statements)

interface BuildContext {
  readonly discovery: Instances.Discovery
  readonly layout: Layout.Plan
  readonly index: DeclarationIndex.Index
  readonly instance: Instances.Instance
  readonly bindings: ReadonlyMap<number, Hir.Expression>
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
    return effectIdentityOf(expression.protected, context)
  if (expression._tag === 'EffectResult') return effectIdentityOf(expression.protected, context)
  if (expression._tag === 'EffectBlock')
    return Instances.effectIdentity(context.instance.key, expression.site)
  if (expression._tag === 'ParameterReference')
    return Instances.parameterEffectIdentity(
      context.instance.function,
      context.instance.key,
      expression.parameter.ordinal,
    )
  if (expression._tag === 'EffectConstruct') {
    return context.discovery.calls.find(
      (call) =>
        Instances.keyText(call.owner) === Instances.keyText(context.instance.key) &&
        call.span.sourceId === expression.span.sourceId &&
        call.span.start === expression.span.start &&
        call.span.end === expression.span.end,
    )?.resultEffect
  }
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
  if (expression._tag === 'EffectResult') return providersOf(expression.protected, context)
  if (expression._tag !== 'EffectBindRequirement') return []
  const capability = Type.substitute(expression.provider.capability, context.instance.substitution)
  const providerType = Type.substitute(
    expression.provider.providerType,
    context.instance.substitution,
  )
  if (!Type.isNominal(capability) || !Type.isNominal(providerType))
    return providersOf(expression.protected, context)
  const witness =
    expression.provider.witness ?? DeclarationIndex.witness(context.index, providerType, capability)
  return Object.freeze([
    ...providersOf(expression.protected, context),
    Object.freeze({
      capability,
      providerType,
      role: expression.provider.role,
      access: expression.provider.access,
      ...(witness === undefined ? {} : { witness }),
    }),
  ])
}

const runnerOf = (expression: Hir.Expression, context: BuildContext): Runner => {
  const identity = effectIdentityOf(expression, context)
  const environment = context.layout.effectEnvironments.find(
    (candidate) =>
      candidate._tag === 'EffectEnvironment' &&
      identity !== undefined &&
      Instances.effectIdentity(candidate.instance, candidate.site) === identity,
  )
  const expressionType =
    'type' in expression
      ? Type.substitute(expression.type, context.instance.substitution)
      : Type.effect('never', [])
  const effect = Type.isEffect(expressionType) ? expressionType : Type.effect(expressionType, [])
  const providers = providersOf(expression, context)
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
  const baseExecution: ExecutionKey = Object.freeze({
    _tag: 'EffectRunnerExecution',
    owner: environment.instance,
    site: environment.site,
    identity,
    runner: Hir.effectRunnerId(environment.instance.declaration, environment.site),
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
  const baseClassification = classificationOfEffect(context.discovery, identity)
  const providedClassification = (): Classification => {
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
    if (block?._tag !== 'EffectBlock') return 'Unknown'
    for (const service of block.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .filter(
        (
          candidate,
        ): candidate is Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }> =>
          candidate._tag === 'ServiceEffectConstruct',
      )) {
      const capability = Type.substitute(service.service, owner.substitution)
      const selected = providers.find(
        (provider) =>
          Type.equals(provider.capability, capability) && provider.role === service.role,
      )
      if (selected?.witness?._tag !== 'SourceConformanceWitness') return 'Unknown'
      const implementation = DeclarationIndex.witnessOperation(selected.witness, service.operation)
      if (implementation === undefined) return 'Unknown'
      const candidates = context.discovery.instances.filter((candidate) =>
        sameDeclaration(candidate.key.declaration, implementation),
      )
      if (candidates.length === 0) return 'Unknown'
      if (candidates.some((candidate) => Instances.isSuspendable(context.discovery, candidate.key)))
        return 'Suspendable'
    }
    return 'Synchronous'
  }
  return Object.freeze({
    execution,
    classification: providedClassification(),
    declaration: execution.runner,
    instance: environment.instance,
    effectIdentity: identity,
    ...(providedIdentity === undefined ? {} : { providedIdentity }),
    typeArguments: environment.instance.typeArguments,
    outcome: environment.effect,
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
  result: Type.Type,
  context: BuildContext,
): Extract<CompletionPolicy, { readonly _tag: 'Reify' }> | undefined => {
  const failureValueType = Type.failureValue(outcome.failures)
  const successType = Type.resultSuccess(outcome.success)
  const failureType = Type.resultFailure(failureValueType)
  const union = Type.union([successType, failureType])
  if (union._tag !== 'Normalized' || !Type.isUnion(union.type)) return undefined
  if (!Type.isNominal(result)) return undefined
  const resultType = result
  const resultEntry = Layout.entry(context.layout, resultType)
  const successEntry = Layout.entry(context.layout, successType)
  const failureEntry = Layout.entry(context.layout, failureType)
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
  const resultShape = Layout.callingShape(context.layout, resultType)
  const outcomeShape = Layout.callingShape(context.layout, outcome)
  const failureValueShape = Layout.callingShape(context.layout, failureValueType)
  const successTag = union.type.members.findIndex((member) => Type.equals(member, successType))
  const failureTag = union.type.members.findIndex((member) => Type.equals(member, failureType))
  if (
    resultField === undefined ||
    successField === undefined ||
    failureField === undefined ||
    resultShape === undefined ||
    outcomeShape === undefined ||
    failureValueShape === undefined ||
    successTag < 0 ||
    failureTag < 0
  )
    return undefined
  return Object.freeze({
    _tag: 'Reify',
    outcome,
    resultType,
    resultField,
    resultUnion: union.type,
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

const controlsOf = (
  statements: ReadonlyArray<Hir.Statement>,
  execution: ExecutionKey,
  executionClassification: Classification,
  context: BuildContext,
): ReadonlyArray<Region> => {
  const regions: Array<Region> = []
  let ordinal = 0
  const maySpecializeProviders = statements.some((statement) =>
    Hir.statementExpressions(statement)
      .flatMap(Hir.expressionTree)
      .some((candidate) => candidate._tag === 'EffectBindRequirement'),
  )
  const visit = (expression: Hir.Expression): void => {
    if (expression._tag === 'EffectBlock') return
    if (expression._tag === 'Run') {
      const idOrdinal = ordinal
      ordinal += 1
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
                transfer: Object.freeze({ _tag: 'OriginateUnpublishedTransfer' }),
                span: expression.span,
              }),
            }),
          )
        }
      } else {
        // The concrete fixed point proves that no call or run in this execution can transfer.
        // Unknown local origin recovery must not manufacture suspension control in that case.
        if (executionClassification === 'Synchronous' && !maySpecializeProviders) {
          for (const child of Hir.expressionChildren(expression)) visit(child)
          return
        }
        const protected_ =
          expression.subject._tag === 'EffectResult'
            ? expression.subject.protected
            : expression.subject
        const runner = runnerOf(protected_, context)
        if (runner.classification !== 'Synchronous') {
          const policy =
            expression.subject._tag === 'EffectResult'
              ? reifyPolicy(
                  runner.outcome,
                  Type.substitute(expression.type, context.instance.substitution),
                  context,
                )
              : Object.freeze({
                  _tag: 'Propagate' as const,
                  outcome: runner.outcome,
                  failureMappings: Object.freeze(
                    runner.outcome.failures.map((_failure, source) =>
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
  for (const statement of statements)
    for (const expression of Hir.statementExpressions(statement)) visit(expression)
  return Object.freeze(regions)
}

/** Builds deterministic provisional control without producing executable final MIR. */
export const build = (
  discovery: Instances.Discovery,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
): Module => {
  const executions: Array<Execution> = []
  for (const instance of discovery.instances) {
    const context: BuildContext = {
      discovery,
      layout,
      index,
      instance,
      bindings: bindingsOf(instance.function),
    }
    const instanceKey: ExecutionKey = Object.freeze({
      _tag: 'InstanceExecution',
      instance: instance.key,
      functionOrdinal: instance.function.declaration.id.ordinal,
      identity: Instances.keyText(instance.key),
    })
    const instanceClassification: Classification = Instances.isExecutionSuspendable(
      discovery,
      instance.key,
    )
      ? 'Suspendable'
      : 'Synchronous'
    executions.push(
      Object.freeze({
        _tag: 'ProvisionalExecution',
        key: instanceKey,
        classification: instanceClassification,
        regions: controlsOf(
          instance.function.statements,
          instanceKey,
          instanceClassification,
          context,
        ),
      }),
    )
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (expression._tag !== 'EffectBlock') continue
      const identity = Instances.effectIdentity(instance.key, expression.site)
      const key: ExecutionKey = Object.freeze({
        _tag: 'EffectRunnerExecution',
        owner: instance.key,
        site: expression.site,
        identity,
        runner: Hir.effectRunnerId(instance.key.declaration, expression.site),
      })
      const runnerClassification = classificationOfEffect(discovery, identity)
      const runnerContext: BuildContext = {
        ...context,
        bindings: new Map([...context.bindings, ...bindingsOfStatements(expression.statements)]),
      }
      const regions = controlsOf(expression.statements, key, runnerClassification, runnerContext)
      const specializedClassification = regions.reduce<Classification>((classification, region) => {
        if (region.outcome._tag !== 'RunSuspendableEffect') return classification
        if (region.outcome.runner.classification === 'Suspendable') return 'Suspendable'
        return classification === 'Synchronous' ? 'Unknown' : classification
      }, runnerClassification)
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
  return Object.freeze({
    _tag: 'ProvisionalMirModule',
    module: discovery.rootModule,
    executions: Object.freeze(executions),
  })
}

/** Returns the exact provisional execution classification for one lowered MIR function. */
export const classificationOfExecution = (
  self: Module,
  instance: Instances.InstanceKey,
): Classification =>
  providedClassification(self, instance) ??
  executionForInstance(self, instance)?.classification ??
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
  const selected =
    control?.outcome._tag === 'RunSuspendableEffect'
      ? control.outcome.runner.classification
      : execution === undefined
        ? 'Unknown'
        : execution.classification === 'Unknown'
          ? 'Unknown'
          : execution.classification === 'Synchronous' &&
              providedClassification(self, instance) === 'Suspendable'
            ? 'Suspendable'
            : 'Synchronous'
  return selected
}

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
  runner: DeclarationIndex.CanonicalId,
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
