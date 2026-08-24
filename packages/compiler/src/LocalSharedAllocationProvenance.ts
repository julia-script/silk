import * as ConformanceProof from './ConformanceProof.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

type SharedFromAllocation = Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' }>
type ExecutionFromAllocation = Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' }>

interface ConcreteOrigin {
  readonly _tag: 'ConcreteOrigin'
  readonly element: Type.Type
  readonly span: SourceSpan.SourceSpan
}

interface ExecutionOrigin {
  readonly _tag: 'ExecutionOrigin'
  readonly arguments: ReadonlyArray<Type.GenericArgument>
  readonly span: SourceSpan.SourceSpan
}

interface ParameterOrigin {
  readonly _tag: 'ParameterOrigin'
  readonly ordinal: number
}

interface InvalidOrigin {
  readonly _tag: 'InvalidOrigin'
  readonly description: string
  readonly span: SourceSpan.SourceSpan
}

interface ConflictOrigin {
  readonly _tag: 'ConflictOrigin'
  readonly span: SourceSpan.SourceSpan
}

interface UnreachedOrigin {
  readonly _tag: 'UnreachedOrigin'
}

interface ServiceOrigin {
  readonly _tag: 'ServiceOrigin'
  readonly owner: Instances.Instance
  readonly service: Type.Nominal
  readonly operation: string
  readonly role: string
  readonly layout: Origin
  readonly span: SourceSpan.SourceSpan
}

interface ProviderBoundOrigin {
  readonly _tag: 'ProviderBoundOrigin'
  readonly protected: Origin
  readonly owner: Instances.Instance
  readonly provider: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>['provider']
  readonly span: SourceSpan.SourceSpan
}

type Origin =
  | ConcreteOrigin
  | ExecutionOrigin
  | ParameterOrigin
  | InvalidOrigin
  | ConflictOrigin
  | UnreachedOrigin
  | ServiceOrigin
  | ProviderBoundOrigin

/** One exact HIR initializer whose allocation originated at `sharedLayout<T>`. */
export interface Fact {
  readonly _tag: 'LocalSharedAllocationProvenanceFact'
  readonly owner: string
  readonly expression: SharedFromAllocation
  readonly element: Type.Type
  readonly span: SourceSpan.SourceSpan
}

/** One exact HIR initializer whose allocation originated at the same execution layout. */
export interface ExecutionFact {
  readonly _tag: 'ExecutionAllocationProvenanceFact'
  readonly owner: string
  readonly expression: ExecutionFromAllocation
  readonly arguments: ReadonlyArray<Type.GenericArgument>
  readonly span: SourceSpan.SourceSpan
}

/** Canonical interprocedural provenance facts retained by target layout planning and MIR lowering. */
export interface Plan {
  readonly _tag: 'LocalSharedAllocationProvenancePlan'
  readonly facts: ReadonlyArray<Fact>
  readonly executionFacts: ReadonlyArray<ExecutionFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const unreached: UnreachedOrigin = Object.freeze({ _tag: 'UnreachedOrigin' })

const sameSpan = (left: SourceSpan.SourceSpan, right: SourceSpan.SourceSpan): boolean =>
  left.sourceId === right.sourceId && left.start === right.start && left.end === right.end

const sameOrigin = (left: Origin, right: Origin): boolean => {
  if (left._tag !== right._tag) return false
  switch (left._tag) {
    case 'ConcreteOrigin':
      return right._tag === 'ConcreteOrigin' && Type.equals(left.element, right.element)
    case 'ExecutionOrigin':
      return (
        right._tag === 'ExecutionOrigin' &&
        left.arguments.length === right.arguments.length &&
        left.arguments.every((argument, ordinal) => {
          const other = right.arguments.at(ordinal)
          return (
            other !== undefined &&
            Type.genericArgumentKey(argument) === Type.genericArgumentKey(other)
          )
        })
      )
    case 'ParameterOrigin':
      return right._tag === 'ParameterOrigin' && left.ordinal === right.ordinal
    case 'InvalidOrigin':
      return right._tag === 'InvalidOrigin' && left.description === right.description
    case 'ConflictOrigin':
    case 'UnreachedOrigin':
      return true
    case 'ServiceOrigin':
      return (
        right._tag === 'ServiceOrigin' &&
        ownerKey(left.owner) === ownerKey(right.owner) &&
        Type.equals(left.service, right.service) &&
        left.operation === right.operation &&
        left.role === right.role &&
        sameOrigin(left.layout, right.layout)
      )
    case 'ProviderBoundOrigin':
      return (
        right._tag === 'ProviderBoundOrigin' &&
        ownerKey(left.owner) === ownerKey(right.owner) &&
        sameSpan(left.provider.span, right.provider.span) &&
        sameOrigin(left.protected, right.protected)
      )
  }
}

const originSpan = (origin: Origin): SourceSpan.SourceSpan | undefined =>
  origin._tag === 'ConcreteOrigin' ||
  origin._tag === 'ExecutionOrigin' ||
  origin._tag === 'InvalidOrigin' ||
  origin._tag === 'ConflictOrigin' ||
  origin._tag === 'ServiceOrigin' ||
  origin._tag === 'ProviderBoundOrigin'
    ? origin.span
    : undefined

const mergeOrigin = (left: Origin, right: Origin): Origin => {
  if (left._tag === 'UnreachedOrigin') return right
  if (right._tag === 'UnreachedOrigin' || sameOrigin(left, right)) return left
  const span = originSpan(left) ?? originSpan(right)
  return span === undefined ? unreached : Object.freeze({ _tag: 'ConflictOrigin', span })
}

const ownerKey = (instance: Instances.Instance): string => Instances.keyText(instance.key)

const nestedStatements = (
  statements: ReadonlyArray<Hir.Statement>,
): ReadonlyArray<Hir.Statement> => {
  const found: Array<Hir.Statement> = []
  const seen = new Set<Hir.Statement>()
  const visit = (items: ReadonlyArray<Hir.Statement>): void => {
    for (const statement of items) {
      if (seen.has(statement)) continue
      seen.add(statement)
      found.push(statement)
      if (statement._tag === 'Unsafe') visit(statement.statements)
      if (statement._tag === 'If' || statement._tag === 'IfLet') {
        visit(statement.taken)
        visit(statement.otherwise)
      }
      if (statement._tag === 'While') visit(statement.body)
      for (const expression of Hir.statementExpressions(statement).flatMap(Hir.expressionTree)) {
        if (expression._tag === 'EffectBlock') visit(expression.statements)
      }
    }
  }
  visit(statements)
  return Object.freeze(found)
}

interface FunctionContext {
  readonly instance: Instances.Instance
  readonly bindings: ReadonlyMap<number, Hir.Expression>
  readonly patternBindings: ReadonlyMap<string, Hir.Expression>
  readonly writtenBindings: ReadonlySet<number>
}

const patternBindingKey = (
  binding: Extract<Hir.Expression, { readonly _tag: 'PatternBindingReference' }>['binding'],
): string =>
  `${binding.arm.match.span.sourceId}:${binding.arm.match.span.start}:${binding.arm.match.span.end}:${binding.arm.ordinal}:${binding.ordinal}`

const returnedExpressions = (
  statements: ReadonlyArray<Hir.Statement>,
): ReadonlyArray<Hir.Expression> =>
  statements.flatMap((statement): ReadonlyArray<Hir.Expression> => {
    switch (statement._tag) {
      case 'Return':
        return [statement.expression]
      case 'Unsafe':
        return returnedExpressions(statement.statements)
      case 'If':
      case 'IfLet':
        return [
          ...returnedExpressions(statement.taken),
          ...returnedExpressions(statement.otherwise),
        ]
      case 'While':
        return returnedExpressions(statement.body)
      default:
        return []
    }
  })

/** Plans exact source allocation provenance over specialized HIR, including ordinary calls. */
export const plan = (discovery: Instances.Discovery, index: DeclarationIndex.Index): Plan => {
  const instances = new Map(discovery.instances.map((instance) => [ownerKey(instance), instance]))
  const contexts = new Map<string, FunctionContext>()
  for (const instance of discovery.instances) {
    const statements = nestedStatements(instance.function.statements)
    contexts.set(
      ownerKey(instance),
      Object.freeze({
        instance,
        bindings: new Map(
          statements.flatMap((statement) =>
            statement._tag === 'Bind'
              ? [[statement.binding.ordinal, statement.initializer] as const]
              : [],
          ),
        ),
        patternBindings: new Map(
          statements
            .flatMap(Hir.statementExpressions)
            .flatMap(Hir.expressionTree)
            .flatMap((expression) =>
              expression._tag === 'Match'
                ? expression.arms.flatMap((arm) =>
                    arm.bindings.map(
                      (binding) =>
                        [patternBindingKey(binding.id), expression.scrutinee] as const,
                    ),
                  )
                : [],
            ),
        ),
        writtenBindings: new Set(
          statements.flatMap((statement) =>
            statement._tag === 'Write' && statement.place._tag === 'WritePlace'
              ? [statement.place.root.ordinal]
              : [],
          ),
        ),
      }),
    )
  }

  const callAt = (
    instance: Instances.Instance,
    expression: { readonly span: SourceSpan.SourceSpan },
  ): Instances.CallInstance | undefined =>
    discovery.calls.find(
      (call) =>
        Instances.keyText(call.owner) === ownerKey(instance) &&
        sameSpan(call.span, expression.span),
    )

  const targetAt = (
    instance: Instances.Instance,
    expression: Extract<
      Hir.Expression,
      { readonly _tag: 'Call' | 'EffectConstruct' | 'CallableApply' }
    >,
  ): Instances.Instance | undefined => {
    const recorded = callAt(instance, expression)
    if (recorded !== undefined) return instances.get(Instances.keyText(recorded.target))
    if (expression._tag === 'CallableApply') return undefined
    const typeArguments = expression.typeArguments.map((argument) =>
      Type.substituteGenericArgument(argument, instance.substitution),
    )
    const matches = Instances.matchingSpecialization(discovery, {
      declaration: expression.target,
      typeArguments,
    })
    return matches.length === 1 ? matches.at(0) : undefined
  }

  const summaries = new Map<string, Origin>()
  const summarize = (instance: Instances.Instance, resolving: ReadonlySet<string>): Origin => {
    const identity = ownerKey(instance)
    const cached = summaries.get(identity)
    if (cached !== undefined) return cached
    const returns = returnedExpressions(instance.function.statements)
    const firstReturn = returns.at(0)
    if (firstReturn === undefined) return unreached
    if (resolving.has(identity))
      return Object.freeze({
        _tag: 'InvalidOrigin',
        description: 'recursive or unresolved allocation provenance',
        span: firstReturn.span,
      })
    const parameters = Object.freeze(
      instance.function.declaration.parameters.map(
        (_, ordinal): Origin => Object.freeze({ _tag: 'ParameterOrigin', ordinal }),
      ),
    )
    const result = returns
      .map((expression) =>
        originOf(expression, instance, parameters, new Set([...resolving, identity])),
      )
      .reduce(mergeOrigin, unreached)
    summaries.set(identity, result)
    return result
  }

  const substitute = (origin: Origin, arguments_: ReadonlyArray<Origin>): Origin => {
    if (origin._tag === 'ParameterOrigin') return arguments_.at(origin.ordinal) ?? unreached
    if (origin._tag === 'ServiceOrigin')
      return Object.freeze({ ...origin, layout: substitute(origin.layout, arguments_) })
    if (origin._tag === 'ProviderBoundOrigin')
      return Object.freeze({ ...origin, protected: substitute(origin.protected, arguments_) })
    return origin
  }

  function originOf(
    expression: Hir.Expression,
    instance: Instances.Instance,
    parameterOrigins: ReadonlyArray<Origin>,
    resolving: ReadonlySet<string>,
    activeBindings = new Set<number>(),
  ): Origin {
    const context = contexts.get(ownerKey(instance))
    if (expression._tag === 'ParameterReference')
      return parameterOrigins.at(expression.parameter.ordinal) ?? unreached
    if (expression._tag === 'BindingReference') {
      if (context?.writtenBindings.has(expression.binding.ordinal))
        return Object.freeze({
          _tag: 'InvalidOrigin',
          description: 'mutable allocation provenance',
          span: expression.span,
        })
      if (activeBindings.has(expression.binding.ordinal))
        return Object.freeze({
          _tag: 'InvalidOrigin',
          description: 'recursive allocation provenance',
          span: expression.span,
        })
      const initializer = context?.bindings.get(expression.binding.ordinal)
      return initializer === undefined
        ? Object.freeze({
            _tag: 'InvalidOrigin',
            description: 'unknown allocation provenance',
            span: expression.span,
          })
        : originOf(
            initializer,
            instance,
            parameterOrigins,
            resolving,
            new Set(activeBindings).add(expression.binding.ordinal),
          )
    }
    if (expression._tag === 'PatternBindingReference') {
      const scrutinee = context?.patternBindings.get(patternBindingKey(expression.binding))
      return scrutinee === undefined
        ? Object.freeze({
            _tag: 'InvalidOrigin',
            description: 'unknown pattern allocation provenance',
            span: expression.span,
          })
        : originOf(scrutinee, instance, parameterOrigins, resolving, activeBindings)
    }
    if (expression._tag === 'Move' || expression._tag === 'Run')
      return originOf(expression.subject, instance, parameterOrigins, resolving, activeBindings)
    if (expression._tag === 'UnionConvert')
      return originOf(expression.source, instance, parameterOrigins, resolving, activeBindings)
    if (expression._tag === 'EffectBindRequirement')
      return Object.freeze({
        _tag: 'ProviderBoundOrigin',
        protected: originOf(
          expression.protected,
          instance,
          parameterOrigins,
          resolving,
          activeBindings,
        ),
        owner: instance,
        provider: expression.provider,
        span: expression.span,
      })
    if (expression._tag === 'EffectResult')
      return originOf(expression.protected, instance, parameterOrigins, resolving, activeBindings)
    if (expression._tag === 'EffectCatch')
      return originOf(expression.protected, instance, parameterOrigins, resolving, activeBindings)
    if (expression._tag === 'EffectBlock') {
      const returns = returnedExpressions(expression.statements)
      return returns.length === 0
        ? unreached
        : returns
            .map((returned) =>
              originOf(returned, instance, parameterOrigins, resolving, activeBindings),
            )
            .reduce(mergeOrigin, unreached)
    }
    if (expression._tag === 'Match')
      return expression.arms
        .filter((arm) => arm.reachable)
        .map((arm) => originOf(arm.result, instance, parameterOrigins, resolving, activeBindings))
        .reduce(mergeOrigin, unreached)
    if (expression._tag === 'BuiltinCall' && expression.operation === 'SharedLayout') {
      const raw = expression.typeArguments.at(0)
      return raw !== undefined && Type.isTypeArgument(raw)
        ? Object.freeze({
            _tag: 'ConcreteOrigin',
            element: Type.substitute(raw, instance.substitution),
            span: expression.span,
          })
        : Object.freeze({
            _tag: 'InvalidOrigin',
            description: 'unknown layout provenance',
            span: expression.span,
          })
    }
    if (expression._tag === 'BuiltinCall' && expression.operation === 'ExecutionLayout')
      return Object.freeze({
        _tag: 'ExecutionOrigin',
        arguments: Object.freeze(
          expression.typeArguments.map((argument) =>
            Type.substituteGenericArgument(argument, instance.substitution),
          ),
        ),
        span: expression.span,
      })
    if (expression._tag === 'BuiltinCall' && expression.operation === 'StorageAcquire') {
      const argument = expression.arguments.at(0)
      return argument === undefined
        ? Object.freeze({
            _tag: 'InvalidOrigin',
            description: 'unknown layout provenance',
            span: expression.span,
          })
        : originOf(argument, instance, parameterOrigins, resolving, activeBindings)
    }
    if (expression._tag === 'ServiceEffectConstruct') {
      if (!Type.equals(expression.type.success, Type.allocation))
        return Object.freeze({
          _tag: 'InvalidOrigin',
          description: 'non-allocation effect provenance',
          span: expression.span,
        })
      const layouts = expression.arguments.filter(
        (argument) =>
          'type' in argument &&
          Type.equals(Type.substitute(argument.type, instance.substitution), Type.layout),
      )
      const layout = layouts.length === 1 ? layouts.at(0) : undefined
      const service = Type.substitute(expression.service, instance.substitution)
      return layout === undefined || !Type.isNominal(service)
        ? Object.freeze({
            _tag: 'InvalidOrigin',
            description: 'unknown allocator layout provenance',
            span: expression.span,
          })
        : Object.freeze({
            _tag: 'ServiceOrigin',
            owner: instance,
            service,
            operation: expression.operation,
            role: expression.role,
            layout: originOf(layout, instance, parameterOrigins, resolving, activeBindings),
            span: expression.span,
          })
    }
    if (expression._tag === 'Call' || expression._tag === 'EffectConstruct') {
      const target = targetAt(instance, expression)
      if (target === undefined)
        return Object.freeze({
          _tag: 'InvalidOrigin',
          description: 'unknown helper allocation provenance',
          span: expression.span,
        })
      const arguments_ = expression.arguments.map((argument) =>
        originOf(argument, instance, parameterOrigins, resolving, activeBindings),
      )
      return substitute(summarize(target, resolving), arguments_)
    }
    if (expression._tag === 'CallableApply') {
      const target = targetAt(instance, expression)
      if (target === undefined)
        return Object.freeze({
          _tag: 'InvalidOrigin',
          description: 'unknown callable allocation provenance',
          span: expression.span,
        })
      const arguments_: Array<Origin> = Array.from(
        { length: target.function.declaration.parameterCount },
        () => unreached,
      )
      if (
        expression.realization === 'DirectErasedSection' &&
        expression.callee._tag === 'CallableSection'
      ) {
        expression.callee.remainingParameters.forEach((parameterOrdinal, ordinal) => {
          const argument = expression.arguments.at(ordinal)
          if (argument !== undefined)
            arguments_[parameterOrdinal] = originOf(
              argument,
              instance,
              parameterOrigins,
              resolving,
              activeBindings,
            )
        })
        for (const capture of expression.callee.captures)
          arguments_[capture.parameterOrdinal] = originOf(
            capture.value,
            instance,
            parameterOrigins,
            resolving,
            activeBindings,
          )
      } else {
        expression.arguments.forEach((argument, ordinal) => {
          arguments_[ordinal] = originOf(
            argument,
            instance,
            parameterOrigins,
            resolving,
            activeBindings,
          )
        })
      }
      return substitute(summarize(target, resolving), arguments_)
    }
    return Object.freeze({
      _tag: 'InvalidOrigin',
      description: 'non-shared layout provenance',
      span: expression.span,
    })
  }

  interface Provider {
    readonly capability: Type.Nominal
    readonly providerType: Type.Nominal
    readonly role: string
    readonly witness: NonNullable<ReturnType<typeof ConformanceProof.witness>>
  }

  const selectedProvider = (
    owner: Instances.Instance,
    provider: ProviderBoundOrigin['provider'],
  ): Provider | undefined => {
    const proof = Instances.requirementSelection(owner, provider)
    if (proof === undefined) return undefined
    const capability = proof.selected.capability
    const providerType = proof.provider
    if (capability === undefined || !Type.isNominal(capability) || !Type.isNominal(providerType))
      return undefined
    const witness = provider.witness ?? ConformanceProof.witness(index, providerType, capability)
    return witness === undefined
      ? undefined
      : Object.freeze({
          capability,
          providerType,
          role: proof.selected.role,
          witness,
        })
  }

  const resolve = (origin: Origin, providers: ReadonlyArray<Provider> = []): Origin => {
    if (origin._tag === 'ProviderBoundOrigin') {
      const selected = selectedProvider(origin.owner, origin.provider)
      return resolve(
        origin.protected,
        selected === undefined ? providers : Object.freeze([...providers, selected]),
      )
    }
    if (origin._tag !== 'ServiceOrigin') return origin
    const explicitlyBound = [...providers]
      .reverse()
      .find(
        (candidate) =>
          candidate.role === origin.role && Type.equals(candidate.capability, origin.service),
      )
    const sameProvidedOwner = (
      candidate: Instances.InstanceKey['declaration'],
      expected: Instances.InstanceKey['declaration'],
    ): boolean =>
      candidate.module === expected.module &&
      (candidate.name === expected.name || candidate.name.startsWith(`${expected.name}$provided$`))
    const reachesExecutionOwner = (
      candidate: Instances.Instance,
      expected: Instances.Instance,
      seen = new Set<string>(),
    ): boolean => {
      const identity = ownerKey(candidate)
      if (seen.has(identity)) return false
      if (
        identity === ownerKey(expected) ||
        sameProvidedOwner(candidate.key.declaration, expected.key.declaration)
      )
        return true
      const nextSeen = new Set(seen).add(identity)
      return discovery.calls
        .filter((call) => Instances.keyText(call.owner) === identity)
        .some((call) => {
          const target = instances.get(Instances.keyText(call.target))
          return target !== undefined && reachesExecutionOwner(target, expected, nextSeen)
        })
    }
    // An ordinary effect helper executes with the provider bound around the helper construction at
    // its caller. The provider node therefore lives in the caller HIR, while the service operation
    // whose allocation provenance we must prove lives in the callee HIR. Follow that structural
    // call edge instead of requiring the helper to inline or recognizing it by declaration name.
    const forwarded =
      explicitlyBound === undefined
        ? discovery.instances.flatMap(
            (caller): ReadonlyArray<Provider> =>
              caller.function.statements
                .flatMap(Hir.statementExpressions)
                .flatMap(Hir.expressionTree)
                .flatMap((candidate): ReadonlyArray<Provider> => {
                  if (candidate._tag !== 'EffectBindRequirement') return []
                  const reachesOwner = Hir.expressionTree(candidate.protected).some((nested) => {
                    if (nested._tag === 'ParameterReference') {
                      const identity = Instances.parameterEffectIdentity(
                        caller.function,
                        caller.key,
                        nested.parameter.ordinal,
                      )
                      const effect =
                        identity === undefined
                          ? undefined
                          : discovery.effects.find((item) => item.identity === identity)
                      return (
                        effect !== undefined &&
                        (() => {
                          const effectOwner = instances.get(Instances.keyText(effect.owner))
                          return (
                            effectOwner !== undefined &&
                            reachesExecutionOwner(effectOwner, origin.owner)
                          )
                        })()
                      )
                    }
                    if (
                      nested._tag !== 'Call' &&
                      nested._tag !== 'EffectConstruct' &&
                      nested._tag !== 'CallableApply'
                    )
                      return false
                    const resultEffect = callAt(caller, nested)?.resultEffect
                    const effect =
                      resultEffect === undefined
                        ? undefined
                        : discovery.effects.find((candidate) => candidate.identity === resultEffect)
                    if (
                      effect !== undefined &&
                      sameProvidedOwner(effect.runner, origin.owner.key.declaration)
                    )
                      return true
                    const target = targetAt(caller, nested)
                    return target !== undefined && reachesExecutionOwner(target, origin.owner)
                  })
                  if (!reachesOwner) return []
                  const selected = selectedProvider(caller, candidate.provider)
                  return selected === undefined ? [] : [selected]
                }),
          )
        : []
    const candidates =
      explicitlyBound === undefined
        ? forwarded.filter(
            (candidate) =>
              candidate.role === origin.role && Type.equals(candidate.capability, origin.service),
          )
        : [explicitlyBound]
    if (candidates.length === 0)
      return Object.freeze({
        _tag: 'InvalidOrigin',
        description: 'unproved service allocation provenance',
        span: origin.span,
      })
    // One generic ordinary helper may be reached through several lexical provider bindings. Its
    // initializer is safe only when every reaching implementation preserves the requested layout;
    // selecting the first provider would let a valid caller authorize a forged sibling call.
    return candidates
      .map((provider): Origin => {
        if (provider.witness._tag !== 'SourceConformanceWitness')
          return Object.freeze({
            _tag: 'InvalidOrigin',
            description: 'unproved service allocation provenance',
            span: origin.span,
          })
        const implementation = ConformanceProof.witnessOperation(provider.witness, origin.operation)
        const targets =
          implementation === undefined
            ? []
            : Instances.matchingSpecialization(discovery, {
                declaration: implementation,
                typeArguments: provider.witness.typeArguments,
              })
        const target = targets.length === 1 ? targets.at(0) : undefined
        if (target === undefined)
          return Object.freeze({
            _tag: 'InvalidOrigin',
            description: 'unresolved service implementation provenance',
            span: origin.span,
          })
        const arguments_: Array<Origin> = target.specialization.parameters.map(() =>
          Object.freeze({
            _tag: 'InvalidOrigin' as const,
            description: 'non-layout service parameter provenance',
            span: origin.span,
          }),
        )
        const layoutParameters = target.specialization.parameters.flatMap((parameter, ordinal) =>
          Type.equals(parameter, Type.layout) ? [ordinal] : [],
        )
        const layoutParameter = layoutParameters.length === 1 ? layoutParameters.at(0) : undefined
        if (layoutParameter === undefined)
          return Object.freeze({
            _tag: 'InvalidOrigin',
            description: 'unresolved service layout parameter provenance',
            span: origin.span,
          })
        arguments_[layoutParameter] = origin.layout
        const implementationOrigin = resolve(substitute(summarize(target, new Set()), arguments_))
        return implementationOrigin._tag === 'UnreachedOrigin'
          ? resolve(origin.layout, providers)
          : implementationOrigin
      })
      .reduce(mergeOrigin, unreached)
  }

  const parameterOrigins = new Map<string, Array<Origin>>()
  for (const instance of discovery.instances)
    parameterOrigins.set(
      ownerKey(instance),
      Array.from({ length: instance.function.declaration.parameterCount }, () => unreached),
    )

  let changed = true
  while (changed) {
    changed = false
    for (const caller of discovery.instances) {
      const callerParameters = parameterOrigins.get(ownerKey(caller))
      if (callerParameters === undefined) continue
      const expressions = caller.function.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .filter(
          (
            candidate,
          ): candidate is Extract<
            Hir.Expression,
            { readonly _tag: 'Call' | 'EffectConstruct' | 'CallableApply' }
          > =>
            candidate._tag === 'Call' ||
            candidate._tag === 'EffectConstruct' ||
            candidate._tag === 'CallableApply',
        )
      for (const expression of expressions) {
        const target = targetAt(caller, expression)
        const targetParameters =
          target === undefined ? undefined : parameterOrigins.get(ownerKey(target))
        if (targetParameters === undefined) continue
        const arguments_: Array<readonly [number, Hir.Expression]> = []
        if (
          expression._tag === 'CallableApply' &&
          expression.realization === 'DirectErasedSection' &&
          expression.callee._tag === 'CallableSection'
        ) {
          expression.callee.remainingParameters.forEach((parameterOrdinal, ordinal) => {
            const argument = expression.arguments.at(ordinal)
            if (argument !== undefined) arguments_.push([parameterOrdinal, argument])
          })
          for (const capture of expression.callee.captures)
            arguments_.push([capture.parameterOrdinal, capture.value])
        } else {
          expression.arguments.forEach((argument, ordinal) => {
            arguments_.push([ordinal, argument])
          })
        }
        arguments_.forEach(([ordinal, argument]) => {
          const previous = targetParameters.at(ordinal)
          if (previous === undefined) return
          const next = mergeOrigin(
            previous,
            originOf(argument, caller, callerParameters, new Set([ownerKey(caller)])),
          )
          if (sameOrigin(previous, next)) return
          targetParameters[ordinal] = next
          changed = true
        })
      }
    }
  }

  const facts: Array<Fact> = []
  const executionFacts: Array<ExecutionFact> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  for (const instance of discovery.instances) {
    const parameters = parameterOrigins.get(ownerKey(instance)) ?? []
    const seen = new Set<Hir.Expression>()
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (seen.has(expression)) continue
      seen.add(expression)
      if (expression._tag !== 'BuiltinCall' || expression.operation !== 'SharedFromAllocation')
        continue
      const raw = expression.typeArguments.at(0)
      const allocation = expression.arguments.at(0)
      const expected =
        raw !== undefined && Type.isTypeArgument(raw)
          ? Type.substitute(raw, instance.substitution)
          : undefined
      const unresolved =
        allocation === undefined
          ? Object.freeze({
              _tag: 'InvalidOrigin' as const,
              description: 'missing allocation provenance',
              span: expression.span,
            })
          : originOf(allocation, instance, parameters, new Set([ownerKey(instance)]))
      const actual = resolve(unresolved)
      if (
        expected !== undefined &&
        actual._tag === 'ConcreteOrigin' &&
        Type.equals(expected, actual.element)
      ) {
        facts.push(
          Object.freeze({
            _tag: 'LocalSharedAllocationProvenanceFact',
            owner: ownerKey(instance),
            expression,
            element: actual.element,
            span: actual.span,
          }),
        )
        continue
      }
      if (expected === undefined) continue
      const span =
        actual._tag === 'ConcreteOrigin' ||
        actual._tag === 'InvalidOrigin' ||
        actual._tag === 'ConflictOrigin'
          ? actual.span
          : (allocation?.span ?? expression.span)
      const description =
        actual._tag === 'ConcreteOrigin'
          ? Type.encode(actual.element)
          : actual._tag === 'InvalidOrigin'
            ? actual.description
            : actual._tag === 'ConflictOrigin'
              ? 'conflicting allocation provenance'
              : 'unknown allocation provenance'
      diagnostics.push(
        Diagnostic.localSharedLayoutMismatch(
          Type.encode(expected),
          description,
          span,
          expression.span,
        ),
      )
    }
  }
  for (const instance of discovery.instances) {
    const parameters = parameterOrigins.get(ownerKey(instance)) ?? []
    const seen = new Set<Hir.Expression>()
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (seen.has(expression)) continue
      seen.add(expression)
      if (expression._tag !== 'BuiltinCall' || expression.operation !== 'ExecutionFromAllocation')
        continue
      const expected = Object.freeze(
        expression.typeArguments.map((argument) =>
          Type.substituteGenericArgument(argument, instance.substitution),
        ),
      )
      const allocation = expression.arguments.at(0)
      const unresolved =
        allocation === undefined
          ? Object.freeze({
              _tag: 'InvalidOrigin' as const,
              description: 'missing allocation provenance',
              span: expression.span,
            })
          : originOf(allocation, instance, parameters, new Set([ownerKey(instance)]))
      const actual = resolve(unresolved)
      if (
        actual._tag === 'ExecutionOrigin' &&
        expected.length === actual.arguments.length &&
        expected.every((argument, ordinal) => {
          const other = actual.arguments.at(ordinal)
          return (
            other !== undefined &&
            Type.genericArgumentKey(argument) === Type.genericArgumentKey(other)
          )
        })
      ) {
        executionFacts.push(
          Object.freeze({
            _tag: 'ExecutionAllocationProvenanceFact',
            owner: ownerKey(instance),
            expression,
            arguments: actual.arguments,
            span: actual.span,
          }),
        )
        continue
      }
      const span = originSpan(actual) ?? allocation?.span ?? expression.span
      const description =
        actual._tag === 'ExecutionOrigin'
          ? actual.arguments.map(Type.genericArgumentKey).join(',')
          : actual._tag === 'InvalidOrigin'
            ? actual.description
            : actual._tag === 'ConflictOrigin'
              ? 'conflicting allocation provenance'
              : 'unknown allocation provenance'
      diagnostics.push(
        Diagnostic.executionLayoutMismatch(
          expected.map(Type.genericArgumentKey).join(','),
          description,
          span,
          expression.span,
        ),
      )
    }
  }
  return Object.freeze({
    _tag: 'LocalSharedAllocationProvenancePlan',
    facts: Object.freeze(facts),
    executionFacts: Object.freeze(executionFacts),
    diagnostics: Diagnostic.merge(diagnostics),
  })
}

/** Finds the exact source allocation fact for one specialized HIR initializer. */
export const find = (
  self: Plan,
  owner: Instances.InstanceKey,
  expression: SharedFromAllocation,
): Fact | undefined => {
  const identity = Instances.keyText(owner)
  return self.facts.find((fact) => fact.owner === identity && fact.expression === expression)
}

/** Finds the exact source allocation fact for one specialized execution initializer. */
export const findExecution = (
  self: Plan,
  owner: Instances.InstanceKey,
  expression: ExecutionFromAllocation,
): ExecutionFact | undefined => {
  const identity = Instances.keyText(owner)
  return self.executionFacts.find(
    (fact) => fact.owner === identity && fact.expression === expression,
  )
}

/** Empty provenance surface for hand-built layout plans. */
export const empty = (): Plan =>
  Object.freeze({
    _tag: 'LocalSharedAllocationProvenancePlan',
    facts: Object.freeze([]),
    executionFacts: Object.freeze([]),
    diagnostics: Object.freeze([]),
  })
