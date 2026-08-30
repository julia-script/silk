import * as CleanupPlan from './CleanupPlan.js'
import * as ConformanceProof from './ConformanceProof.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Elaboration from './Elaboration.js'
import * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'
import * as TypeInference from './internal/TypeInference.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Specialization from './Specialization.js'
import * as SuspensionMode from './SuspensionMode.js'
import * as Type from './Type.js'

type Instance = Instances.Instance
type InstanceKey = Instances.InstanceKey
type IntrinsicCall = Instances.IntrinsicCall

export interface SuspensionGraph {
  readonly roots: ReadonlyMap<SuspensionMode.Mode, ReadonlySet<string>>
  readonly dependencies: ReadonlyMap<string, ReadonlySet<string>>
  readonly effectIdentities: ReadonlySet<string>
  readonly permitted: ReadonlyMap<string, ReadonlySet<SuspensionMode.Mode>>
  readonly unavailable: ReadonlySet<string>
  /** Exact provider operation specializations discovered through bound service dispatch. */
  readonly providedTargets: ReadonlyArray<{
    readonly owner: InstanceKey
    readonly target: InstanceKey
    readonly span: Hir.Expression['span']
  }>
}

export interface CallTarget {
  readonly declaration: DeclarationFacts.CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly structuralProvider?: Type.Type
}

export const instanceNode = (key: InstanceKey, encode: (key: InstanceKey) => string): string =>
  `instance\u0000${encode(key)}`

export const effectNode = (identity: string): string => `effect\u0000${identity}`

const compareIntrinsicCalls = (left: IntrinsicCall, right: IntrinsicCall): number =>
  Intrinsic.operationText(left.operation).localeCompare(Intrinsic.operationText(right.operation)) ||
  left.span.sourceId.localeCompare(right.span.sourceId) ||
  left.span.start - right.span.start ||
  left.span.end - right.span.end

/** Collects the canonical intrinsic identities retained by reachable concrete instances. */
export const reachableIntrinsics = (
  instances: ReadonlyArray<Instance>,
  index: DeclarationIndex.Index,
): ReadonlyArray<IntrinsicCall> => {
  const retained = new Map<string, IntrinsicCall>()
  for (const instance of instances) {
    for (const statement of instance.function.statements) {
      for (const root of Hir.statementExpressions(statement)) {
        for (const expression of Hir.expressionTree(root)) {
          const selected =
            expression._tag === 'BoundOperationCall'
              ? (() => {
                  const capability = Type.substitute(expression.capability, instance.substitution)
                  return Type.isNominal(capability)
                    ? ConformanceProof.interfaceOperationIntrinsic(
                        index,
                        Type.substitute(expression.provider, instance.substitution),
                        capability,
                        expression.operation,
                      )?.id
                    : undefined
                })()
              : undefined
          let operation: Intrinsic.OperationId | undefined
          if (expression._tag === 'BuiltinCall') {
            operation = expression.intrinsic
          } else if (expression._tag === 'EffectCatch') {
            operation = expression.intrinsic
          } else if (
            (expression._tag === 'FunctionItem' || expression._tag === 'CallableSection') &&
            expression.target._tag === 'BuiltinCallableTarget'
          ) {
            operation = expression.target.intrinsic
          } else {
            operation = selected
          }
          if (operation === undefined) continue
          const span = expression.span
          const key = `${Intrinsic.operationText(operation)}\u0000${span.sourceId}\u0000${span.start}\u0000${span.end}`
          retained.set(key, Object.freeze({ _tag: 'ReachableIntrinsicCall', operation, span }))
        }
      }
    }
  }
  return Object.freeze([...retained.values()].sort(compareIntrinsicCalls))
}

/** Computes normalized direct/nested/external-park facts for every execution node. */
export const suspensionSummaries = (
  graph: SuspensionGraph,
): ReadonlyMap<string, SuspensionMode.Summary> => SuspensionMode.summarize(graph)

export interface Operations {
  readonly specializeInstanceType: (
    type: Type.Type,
    owner: Instances.InstanceKey,
    substitutions: ReadonlyArray<Type.Substitution>,
  ) => Type.Type
  readonly keyOf: (
    declaration: DeclarationFacts.CanonicalId,
    contract: Hir.ContractFact,
    typeParameters?: ReadonlyArray<Type.Parameter>,
    typeArguments?: ReadonlyArray<Type.GenericArgument>,
  ) => Instances.InstanceKey
  readonly keyText: (key: Instances.InstanceKey) => string
  readonly requirementBindings: (
    fn: Hir.HirFunction,
  ) => ReadonlyArray<Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>>
  readonly selectedRequirement: (
    binding: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>,
    substitution: Type.Substitution,
  ) => Type.Requirement | undefined
  readonly requirementBindingWitness: (
    binding: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>,
    substitution: Type.Substitution,
    index: DeclarationIndex.Index,
  ) => DeclarationFacts.ConformanceWitness | undefined
  readonly forwardedRequirementBinding: (
    fn: Hir.HirFunction,
  ) => Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }> | undefined
  readonly instanceSubstitution: (
    fn: Hir.HirFunction,
    key: Instances.InstanceKey,
  ) => Type.Substitution | undefined
  readonly effectParameterOrdinals: (
    fn: Hir.HirFunction,
    substitution: Type.Substitution,
  ) => ReadonlyArray<number>
  readonly callableParameterOrdinals: (
    fn: Hir.HirFunction,
    substitution: Type.Substitution,
  ) => ReadonlyArray<number>
  readonly parameterEffectIdentity: (
    fn: Hir.HirFunction,
    key: Instances.InstanceKey,
    ordinal: number,
  ) => string | undefined
  readonly parameterEffectRepresentationArgument: (
    fn: Hir.HirFunction,
    key: Instances.InstanceKey,
    ordinal: number,
  ) => Type.EffectIdentityArgument | Type.CompositeEffectRepresentationArgument | undefined
  readonly parameterCallableIdentity: (
    fn: Hir.HirFunction,
    key: Instances.InstanceKey,
    ordinal: number,
  ) => Type.CallableIdentityArgument | undefined
  readonly effectIdentity: (owner: Instances.InstanceKey, site: Hir.EffectSiteId) => string
  readonly callableIdentity: (self: Instances.CallableInstance) => string
  readonly callableEnvironmentIdentity: (
    self: Instances.CallableInstance,
  ) => Type.CallableEnvironmentIdentity
}

export const make = (operations: Operations) => {
  const {
    specializeInstanceType,
    keyOf,
    keyText,
    requirementBindings,
    selectedRequirement,
    requirementBindingWitness,
    forwardedRequirementBinding,
    instanceSubstitution,
    effectParameterOrdinals,
    callableParameterOrdinals,
    parameterEffectIdentity,
    parameterEffectRepresentationArgument,
    parameterCallableIdentity,
    effectIdentity,
  } = operations
  type InstanceKey = Instances.InstanceKey
  type Instance = Instances.Instance
  type CallInstance = Instances.CallInstance
  type CallableInstance = Instances.CallableInstance
  type EffectInstance = Instances.EffectInstance
  const sameVisibleTypeArguments = (
    left: ReadonlyArray<Type.GenericArgument>,
    right: ReadonlyArray<Type.GenericArgument>,
  ): boolean => {
    const leftVisible = left.filter((argument) => !Type.isHiddenExecutableArgument(argument))
    const rightVisible = right.filter((argument) => !Type.isHiddenExecutableArgument(argument))
    return (
      leftVisible.length === rightVisible.length &&
      leftVisible.every((argument, ordinal) => {
        const expected = rightVisible.at(ordinal)
        return expected !== undefined && Type.equalsGenericArgument(argument, expected)
      })
    )
  }
  const sameExactOwner = (left: InstanceKey, right: Type.ExecutableSpecializationOwner): boolean =>
    left.declaration.module === right.declaration.module &&
    left.declaration.name === right.declaration.name &&
    left.typeArguments.length === right.typeArguments.length &&
    left.typeArguments.every((argument, ordinal) => {
      const expected = right.typeArguments.at(ordinal)
      return expected !== undefined && Type.equalsGenericArgument(argument, expected)
    })
  /** Converts proved strict-subterm obligations into the only structurally descending call edges. */
  const witnessDependencyCallTargets = (
    index: DeclarationIndex.Index,
    provider: Type.Type,
    capability: Type.Nominal,
  ): ReadonlyArray<CallTarget> =>
    ConformanceProof.witnessDependencyTargets(index, provider, capability).map((dependency) =>
      Object.freeze({
        declaration: dependency.implementation,
        typeArguments: dependency.typeArguments,
        ...(dependency.structuralProvider === undefined
          ? {}
          : { structuralProvider: dependency.structuralProvider }),
      }),
    )

  /** Collects every Drop hook a cleanup plan will invoke, so cleanup reaches hook instances. */
  const hookCalls = (
    cleanup: CleanupPlan.CleanupPlan,
    index: DeclarationIndex.Index,
  ): ReadonlyArray<CallTarget> => {
    switch (cleanup._tag) {
      case 'HookCleanup':
        return [
          ...witnessDependencyCallTargets(index, cleanup.type, Type.dropCapability),
          Object.freeze({ declaration: cleanup.hook, typeArguments: cleanup.typeArguments }),
          ...hookCalls(cleanup.inner, index),
        ]
      case 'StructCleanup':
        return cleanup.fields.flatMap((field) => hookCalls(field.cleanup, index))
      case 'NominalUnionCleanup':
        return cleanup.variants.flatMap((variant) =>
          variant.fields.flatMap((field) => hookCalls(field.cleanup, index)),
        )
      case 'ArrayCleanup':
        return hookCalls(cleanup.element, index)
      case 'UnionCleanup':
        return cleanup.cases.flatMap((entry) => hookCalls(entry.cleanup, index))
      case 'RawBufferCleanup':
        return hookCalls(cleanup.allocation, index)
      case 'CallableCleanup':
        return cleanup.slots.flatMap((slot) => hookCalls(slot.cleanup, index))
      case 'EffectCleanup':
        return cleanup.slots.flatMap((slot) => hookCalls(slot.cleanup, index))
      default:
        return []
    }
  }

  /**
   * Reports whether a call must carry hidden identity arguments to be lowerable.
   *
   * Effect and callable values are both compiler-private: neither has a target layout, and both are
   * erased by monomorphizing the target on the argument's hidden concrete identity. A call passing
   * either therefore cannot be specialized on its explicit type arguments alone — the target would
   * be instantiated without the identity, and lowering would drop the parameter it cannot type.
   *
   * The judgement runs under the caller instance's substitution: an argument whose declared type is
   * a bare type parameter is still an Effect or callable value when the caller's own instance bound
   * that parameter to one, exactly as if the type had been spelled at the call.
   */
  const carriesHiddenIdentity = (
    expression: Extract<Hir.Expression, { readonly _tag: 'Call' | 'EffectConstruct' }>,
    substitution: Type.Substitution,
  ): boolean => {
    const carrier = (type: Type.Type): boolean => {
      const specialized = Type.substitute(type, substitution)
      return Type.isEffect(specialized) || Type.isCallable(specialized)
    }
    return (
      Type.isEffect(Type.substitute(expression.type, substitution)) ||
      expression.arguments.some(
        (argument) => argument._tag !== 'Unavailable' && carrier(argument.type),
      )
    )
  }

  const callTargets = (
    expression: Hir.Expression,
    index: DeclarationIndex.Index,
    substitution: Type.Substitution,
  ): ReadonlyArray<CallTarget> => {
    if (expression._tag === 'Run') return callTargets(expression.subject, index, substitution)
    if (expression._tag === 'EffectCatch')
      return [
        ...callTargets(expression.protected, index, substitution),
        ...callTargets(expression.handler, index, substitution),
      ]
    if (expression._tag === 'EffectBindRequirement') {
      // A source-declared witness makes provision dispatch to its qualified operation, so the
      // operation is reachable even though no ordinary call names it.
      const witness = expression.provider.witness
      return [
        ...callTargets(expression.protected, index, substitution),
        ...(witness?._tag === 'SourceConformanceWitness'
          ? witnessDependencyCallTargets(index, witness.provider, witness.capability)
          : []),
      ]
    }
    if (expression._tag === 'Move') return callTargets(expression.subject, index, substitution)
    if (expression._tag === 'RuntimeStringView')
      return callTargets(expression.source, index, substitution)
    if (
      expression._tag === 'StringEquality' ||
      expression._tag === 'EnumEquality' ||
      expression._tag === 'ShortCircuit'
    ) {
      return [
        ...callTargets(expression.left, index, substitution),
        ...callTargets(expression.right, index, substitution),
      ]
    }
    if (expression._tag === 'UnionConvert')
      return callTargets(expression.source, index, substitution)
    if (expression._tag === 'Project') return callTargets(expression.subject, index, substitution)
    if (expression._tag === 'IndexPlace') {
      return [
        ...callTargets(expression.subject, index, substitution),
        ...callTargets(expression.index, index, substitution),
      ]
    }
    if (expression._tag === 'SliceLength') return callTargets(expression.slice, index, substitution)
    if (expression._tag === 'SliceIndexPlace') {
      return [
        ...callTargets(expression.slice, index, substitution),
        ...callTargets(expression.index, index, substitution),
      ]
    }
    if (expression._tag === 'Construct' || expression._tag === 'ConstructUnionVariant') {
      return expression.fields.flatMap((field) => callTargets(field.value, index, substitution))
    }
    if (expression._tag === 'ArrayConstruct') {
      return expression.elements.flatMap((element) => callTargets(element, index, substitution))
    }
    if (expression._tag === 'BuiltinCall' || expression._tag === 'BoundOperationCall') {
      return expression.arguments.flatMap((argument) => callTargets(argument, index, substitution))
    }
    if (expression._tag === 'FunctionItem') return []
    if (expression._tag === 'CallableSection') {
      return expression.captures.flatMap((capture) =>
        callTargets(capture.value, index, substitution),
      )
    }
    if (expression._tag === 'CallableApply') {
      return [
        ...callTargets(expression.callee, index, substitution),
        ...expression.arguments.flatMap((argument) => callTargets(argument, index, substitution)),
      ]
    }
    if (expression._tag === 'Match') {
      return [
        ...callTargets(expression.scrutinee, index, substitution),
        ...expression.arms.flatMap((arm) => {
          if (arm.reachable) {
            return [
              ...(arm.guard === undefined ? [] : callTargets(arm.guard, index, substitution)),
              ...callTargets(arm.result, index, substitution),
            ]
          }
          return []
        }),
      ]
    }
    if (expression._tag === 'EffectBlock') {
      return expression.statements.flatMap((statement) =>
        Hir.statementExpressions(statement).flatMap((child) =>
          callTargets(child, index, substitution),
        ),
      )
    }
    if (
      expression._tag !== 'Call' &&
      expression._tag !== 'EffectConstruct' &&
      expression._tag !== 'ServiceEffectConstruct'
    )
      return []
    const nested = expression.arguments.flatMap((argument) =>
      callTargets(argument, index, substitution),
    )
    if (expression._tag === 'ServiceEffectConstruct') return nested
    return carriesHiddenIdentity(expression, substitution)
      ? nested
      : [
          Object.freeze({
            declaration: expression.target,
            typeArguments: expression.typeArguments,
          }),
          ...nested,
        ]
  }

  const bodyCallTargets = (
    fn: Hir.HirFunction,
    index: DeclarationIndex.Index,
    substitution: Type.Substitution,
  ): ReadonlyArray<CallTarget> =>
    fn.statements.flatMap((statement) =>
      Hir.statementExpressions(statement).flatMap((expression) =>
        callTargets(expression, index, substitution),
      ),
    )

  const requirementBindingCallTargets = (
    fn: Hir.HirFunction,
    substitution: Type.Substitution,
    index: DeclarationIndex.Index,
  ): ReadonlyArray<CallTarget> =>
    (forwardedRequirementBinding(fn) === undefined ? requirementBindings(fn) : []).flatMap(
      (binding) => {
        const witness = requirementBindingWitness(binding, substitution, index)
        return witness?._tag === 'SourceConformanceWitness'
          ? witnessDependencyCallTargets(index, witness.provider, witness.capability)
          : []
      },
    )

  const forwardedRequirementCallTargets = (
    calls: ReadonlyArray<CallInstance>,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
  ): ReadonlyArray<CallTarget> =>
    calls.flatMap((call) => {
      const target = functionByKey(results, call.target)
      const binding = target === undefined ? undefined : forwardedRequirementBinding(target)
      if (target === undefined || binding === undefined) return []
      const substitution = instanceSubstitution(target, call.target)
      if (substitution === undefined) return []
      const witness = requirementBindingWitness(binding, substitution, index)
      return witness?._tag === 'SourceConformanceWitness'
        ? witnessDependencyCallTargets(index, witness.provider, witness.capability)
        : []
    })

  const slotDropHookTargets = (
    fn: Hir.HirFunction,
    index: DeclarationIndex.Index,
    substitution: Type.Substitution,
  ): ReadonlyArray<CallTarget> => {
    const walk = (expression: Hir.Expression): ReadonlyArray<CallTarget> => {
      const own =
        expression._tag === 'BuiltinCall' &&
        (expression.operation === 'SlotDrop' || expression.operation === 'ExecutionPark')
          ? (expression.operation === 'ExecutionPark'
              ? expression.typeArguments.slice(0, 1)
              : expression.typeArguments
            ).flatMap((argument) =>
              (() => {
                const specialized = Type.substituteGenericArgument(argument, substitution)
                return Type.isTypeArgument(specialized)
                  ? hookCalls(CleanupPlan.cleanupPlan(index, specialized), index)
                  : []
              })(),
            )
          : []
      if (expression._tag === 'Match') {
        return [
          ...own,
          ...walk(expression.scrutinee),
          ...expression.arms.flatMap((arm) => {
            if (arm.reachable) {
              return [...(arm.guard === undefined ? [] : walk(arm.guard)), ...walk(arm.result)]
            }
            return []
          }),
        ]
      }
      return [...own, ...Hir.expressionChildren(expression).flatMap(walk)]
    }
    return fn.statements.flatMap((statement) => Hir.statementExpressions(statement).flatMap(walk))
  }

  /**
   * Collects the provider functions a specialized body's bound operations dispatch to.
   *
   * A source witness is reachable through the operator that spells its operation, or through the
   * bound's own name when no operator spells it, and through no ordinary call — so discovery has to
   * read the conformance itself. Both spellings walk one conformance, because the witness a
   * specialization selects does not depend on how the body names the operation; a scalar argument
   * maps the same operation to a sealed intrinsic and contributes no target.
   */
  const interfaceWitnessTargets = (
    fn: Hir.HirFunction,
    index: DeclarationIndex.Index,
    substitution: Type.Substitution,
  ): ReadonlyArray<CallTarget> => {
    const walk = (expression: Hir.Expression): ReadonlyArray<CallTarget> => {
      let bound:
        | {
            readonly capability: Type.Nominal
            readonly provider: Type.Type
            readonly operation: string
            readonly contract: DeclarationFacts.InterfaceOperationApplicationFact
          }
        | undefined
      if (expression._tag === 'BuiltinCall') {
        bound = expression.interfaceOperation
      } else if (expression._tag === 'BoundOperationCall') {
        bound = expression
      } else {
        bound = undefined
      }
      const capability =
        bound === undefined ? undefined : Type.substitute(bound.capability, substitution)
      const provider =
        bound === undefined ? undefined : Type.substitute(bound.provider, substitution)
      const target =
        bound === undefined ||
        provider === undefined ||
        capability === undefined ||
        !Type.isNominal(capability)
          ? undefined
          : ConformanceProof.interfaceWitnessTarget(index, provider, capability, bound.operation)
      const dependencies =
        provider === undefined || capability === undefined || !Type.isNominal(capability)
          ? []
          : witnessDependencyCallTargets(index, provider, capability)
      // A conditional witness is generic in its header's binders, so the target carries the arguments
      // this specialization proved rather than reaching code through an unsubstituted declaration.
      let own: CallTarget[]
      if (target === undefined && dependencies.length === 0) {
        own = []
      } else {
        own = [
          ...dependencies,
          ...(target === undefined
            ? []
            : [
                Object.freeze({
                  declaration: target.implementation,
                  typeArguments: target.typeArguments,
                  ...(target.structuralProvider === undefined
                    ? {}
                    : { structuralProvider: target.structuralProvider }),
                }),
              ]),
        ]
      }
      if (expression._tag === 'Match') {
        return [
          ...own,
          ...walk(expression.scrutinee),
          ...expression.arms.flatMap((arm) => {
            if (arm.reachable) {
              return [...(arm.guard === undefined ? [] : walk(arm.guard)), ...walk(arm.result)]
            }
            return []
          }),
        ]
      }
      return [...own, ...Hir.expressionChildren(expression).flatMap(walk)]
    }
    return fn.statements.flatMap((statement) => Hir.statementExpressions(statement).flatMap(walk))
  }

  const callableBindings = (fn: Hir.HirFunction): ReadonlyMap<number, Hir.Expression> => {
    const bindings = new Map<number, Hir.Expression>()
    const expression = (value: Hir.Expression): void => {
      if (value._tag === 'EffectBlock') {
        statements(value.statements)
        return
      }
      for (const child of Hir.expressionChildren(value)) expression(child)
    }
    const statements = (body: ReadonlyArray<Hir.Statement>): void => {
      for (const statement of body) {
        if (statement._tag === 'Bind') {
          bindings.set(statement.binding.ordinal, statement.initializer)
          expression(statement.initializer)
        }
        if (statement._tag === 'Unsafe') statements(statement.statements)
        else if (statement._tag === 'If') {
          expression(statement.condition)
          statements(statement.taken)
          statements(statement.otherwise)
        } else if (statement._tag === 'While') {
          expression(statement.condition)
          statements(statement.body)
        } else if (statement._tag !== 'Bind') {
          for (const root of Hir.statementExpressions(statement)) expression(root)
        }
      }
    }
    statements(fn.statements)
    return bindings
  }

  /**
   * Follows a callable through a closed, source-visible return chain without inventing a runtime
   * dictionary for its quantified contract. Only exact expression identity is forwarded: branches,
   * computed callables, opaque declarations, and recursion stop the proof.
   */
  const forwardedCallableParameter = (
    fn: Hir.HirFunction,
    results: ReadonlyMap<string, Elaboration.Result>,
    resolving: ReadonlySet<string> = new Set(),
  ): number | undefined => {
    const key_ =
      fn.declaration.canonical._tag === 'Canonical'
        ? `${fn.declaration.canonical.id.module}\u0000${fn.declaration.canonical.id.name}`
        : undefined
    if (key_ === undefined || resolving.has(key_) || fn.contract._tag !== 'Contract')
      return undefined
    const leading = fn.statements.slice(0, -1)
    const returned = fn.statements.at(-1)
    if (
      fn.contract.parameters.length !== 1 ||
      returned?._tag !== 'Return' ||
      leading.some((statement) => statement._tag !== 'Bind')
    )
      return undefined
    const forwardedBindings = new Set<number>()
    const next = new Set(resolving).add(key_)
    const expression = (
      current: Hir.Expression,
      bindings: ReadonlySet<number> = new Set(),
    ): number | undefined => {
      if (current._tag === 'Move') return expression(current.subject, bindings)
      if (current._tag === 'ParameterReference') return current.parameter.ordinal
      if (current._tag === 'BindingReference') {
        const ordinal = current.binding.ordinal
        if (bindings.has(ordinal)) return undefined
        const binding = leading.find(
          (statement) => statement._tag === 'Bind' && statement.binding.ordinal === ordinal,
        )
        if (binding?._tag !== 'Bind') return undefined
        forwardedBindings.add(ordinal)
        return expression(binding.initializer, new Set(bindings).add(ordinal))
      }
      if (current._tag !== 'Call') return undefined
      const target = targetFunction(results, current.target)
      const forwarded =
        target === undefined ? undefined : forwardedCallableParameter(target, results, next)
      const argument = forwarded === undefined ? undefined : current.arguments.at(forwarded)
      return argument === undefined ? undefined : expression(argument, bindings)
    }
    const forwarded = expression(returned.expression)
    return forwarded === undefined ||
      leading.some(
        (statement) =>
          statement._tag !== 'Bind' || !forwardedBindings.has(statement.binding.ordinal),
      )
      ? undefined
      : forwarded
  }

  const staticallyForwardedCallable = (
    expression: Hir.Expression,
    fn: Hir.HirFunction,
    results: ReadonlyMap<string, Elaboration.Result>,
    arguments_: ReadonlyArray<Hir.Expression> = Object.freeze([]),
    resolving: ReadonlySet<string> = new Set(),
  ): Hir.Expression | undefined => {
    if (expression._tag === 'Move')
      return staticallyForwardedCallable(expression.subject, fn, results, arguments_, resolving)
    if (expression._tag === 'BindingReference') {
      const initializer = callableBindings(fn).get(expression.binding.ordinal)
      return initializer === undefined
        ? undefined
        : staticallyForwardedCallable(initializer, fn, results, arguments_, resolving)
    }
    if (expression._tag === 'ParameterReference') return arguments_.at(expression.parameter.ordinal)
    if (expression._tag !== 'Call')
      return expression._tag === 'CallableSection' || expression._tag === 'FunctionItem'
        ? expression
        : undefined
    const identity = `${expression.target.module}\u0000${expression.target.name}`
    if (resolving.has(identity)) return undefined
    const target = targetFunction(results, expression.target)
    const forwarded =
      target === undefined ? undefined : forwardedCallableParameter(target, results, resolving)
    const argument = forwarded === undefined ? undefined : expression.arguments.at(forwarded)
    return argument === undefined
      ? undefined
      : staticallyForwardedCallable(
          argument,
          fn,
          results,
          arguments_,
          new Set([...resolving, identity]),
        )
  }

  const callableOriginOf = (
    expression: Hir.Expression,
    context: EffectOriginContext,
  ): Type.CallableIdentityArgument | undefined => {
    if (expression._tag === 'FunctionItem') {
      const target = Hir.callableTargetIdentity(expression.target)
      const typeArguments = expression.typeArguments.map((argument) =>
        Type.substituteGenericArgument(argument, context.substitution),
      )
      const identity =
        target._tag === 'Declaration'
          ? `declaration:${target.module}:${target.name}`
          : `builtin:${target.actor}:${target.operation}`
      return Type.callableIdentityArgument(identity, target, typeArguments)
    }
    if (expression._tag === 'CallableSection') {
      const typeArguments = expression.typeArguments.map((argument) =>
        Type.substituteGenericArgument(argument, context.substitution),
      )
      const environment = Hir.callableEnvironmentIdentity(expression.site, {
        declaration: Object.freeze({
          module: context.owner.declaration.module,
          name: context.owner.declaration.name,
        }),
        typeArguments: context.owner.typeArguments,
      })
      const target = Hir.callableTargetIdentity(expression.target)
      const identity =
        target._tag === 'Declaration'
          ? `declaration:${target.module}:${target.name}`
          : `builtin:${target.actor}:${target.operation}`
      return Type.callableIdentityArgument(identity, target, typeArguments, environment)
    }
    if (expression._tag === 'ParameterReference')
      return parameterCallableIdentity(context.fn, context.owner, expression.parameter.ordinal)
    if (expression._tag === 'BindingReference') {
      const initializer = callableBindings(context.fn).get(expression.binding.ordinal)
      return initializer === undefined ? undefined : callableOriginOf(initializer, context)
    }
    if (expression._tag === 'Move') return callableOriginOf(expression.subject, context)
    if (expression._tag === 'Call') {
      const forwarded = staticallyForwardedCallable(expression, context.fn, context.results)
      if (forwarded !== undefined && forwarded !== expression)
        return callableOriginOf(forwarded, context)
      const targetKey = targetKeyOfCall(expression, context)
      const target =
        targetKey === undefined ? undefined : targetFunction(context.results, expression.target)
      return targetKey === undefined || target === undefined
        ? undefined
        : resultCallableIdentity(
            target,
            targetKey,
            context.results,
            context.index,
            context.resolving,
          )
    }
    if (expression._tag === 'CallableApply') {
      const targetKey = targetKeyOfCallableApply(expression, context)
      if (targetKey === undefined) return undefined
      const target = targetFunction(context.results, targetKey.declaration)
      return target === undefined
        ? undefined
        : resultCallableIdentity(
            target,
            targetKey,
            context.results,
            context.index,
            context.resolving,
          )
    }
    return undefined
  }

  const callableSubstitutionOf = (
    expression: Hir.Expression,
    context: EffectOriginContext,
  ): Type.Substitution => {
    if (expression._tag === 'CallableSection')
      return new Map(
        Array.from(expression.substitution.entries()).map(([key, argument]) => [
          key,
          Type.substituteGenericArgument(argument, context.substitution),
        ]),
      )
    if (expression._tag === 'BindingReference') {
      const initializer = callableBindings(context.fn).get(expression.binding.ordinal)
      return initializer === undefined ? new Map() : callableSubstitutionOf(initializer, context)
    }
    if (expression._tag === 'Move') return callableSubstitutionOf(expression.subject, context)
    if (expression._tag === 'Call') {
      const forwarded = staticallyForwardedCallable(expression, context.fn, context.results)
      return forwarded === undefined || forwarded === expression
        ? new Map()
        : callableSubstitutionOf(forwarded, context)
    }
    return new Map()
  }

  const appliedCallableOriginOf = (
    expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
    context: EffectOriginContext,
  ): Type.CallableIdentityArgument | undefined => {
    const callable = callableOriginOf(expression.callee, context)
    if (callable?.target._tag !== 'Declaration') return callable
    const declaration: DeclarationFacts.CanonicalId = Object.freeze({
      _tag: 'CanonicalDeclarationId',
      module: callable.target.module,
      name: callable.target.name,
    })
    const target = targetFunction(context.results, declaration)
    if (target === undefined) return callable
    const inferredAtSection = callableSubstitutionOf(expression.callee, context)
    const inferred = target.declaration.typeParameters.map((parameter, ordinal) => {
      const argument =
        expression.substitution.get(Type.key(parameter.type)) ??
        inferredAtSection.get(Type.key(parameter.type)) ??
        (callable.typeArguments.length === target.declaration.typeParameters.length
          ? callable.typeArguments.at(ordinal)
          : undefined)
      return argument === undefined
        ? undefined
        : Type.substituteGenericArgument(argument, context.substitution)
    })
    if (inferred.some((argument) => argument === undefined)) return callable
    return Type.callableIdentityArgument(
      callable.identity,
      callable.target,
      inferred.filter((argument): argument is Type.GenericArgument => argument !== undefined),
      callable.environment,
    )
  }

  const callableApplicationArgument = (
    expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
    ordinal: number,
    context: EffectOriginContext,
  ): Hir.Expression | undefined => {
    const callable = callableValue(expression.callee, callableBindings(context.fn))
    const section = callable?._tag === 'CallableSection' ? callable : undefined
    if (section === undefined) return expression.arguments.at(ordinal)
    const captured = section.captures.find((capture) => capture.parameterOrdinal === ordinal)
    if (captured !== undefined) return captured.value
    const argumentOrdinal = section.remainingParameters.indexOf(ordinal)
    return argumentOrdinal < 0 ? undefined : expression.arguments.at(argumentOrdinal)
  }

  const targetKeyOfCallableApply = (
    expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
    context: EffectOriginContext,
  ): InstanceKey | undefined => {
    const callable = appliedCallableOriginOf(expression, context)
    if (callable?.target._tag !== 'Declaration') return undefined
    const declaration: DeclarationFacts.CanonicalId = Object.freeze({
      _tag: 'CanonicalDeclarationId',
      module: callable.target.module,
      name: callable.target.name,
    })
    const target = targetFunction(context.results, declaration)
    if (target === undefined) return undefined
    const parameters = target.declaration.typeParameters.map((parameter) => parameter.type)
    const targetSubstitution = TypeInference.substitution(parameters, callable.typeArguments)
    if (targetSubstitution === undefined) return undefined
    const hiddenArguments: Array<
      | Type.EffectIdentityArgument
      | Type.CallableIdentityArgument
      | Type.CompositeEffectRepresentationArgument
    > = []
    for (const ordinal of effectParameterOrdinals(target, targetSubstitution)) {
      const argument = callableApplicationArgument(expression, ordinal, context)
      const compositeRepresentation =
        argument === undefined ? undefined : compositeEffectRepresentationOf(argument, context)
      if (compositeRepresentation !== undefined) {
        hiddenArguments.push(compositeRepresentation)
        continue
      }
      let identity = argument === undefined ? undefined : effectOriginOf(argument, context)
      if (identity === undefined) {
        const service = argument === undefined ? undefined : serviceEffectRecipe(argument, context)
        const serviceTarget = forwardedServiceTargetOfCallableApply(
          expression,
          target,
          targetSubstitution,
          ordinal,
          context,
          service,
        )
        const serviceFunction =
          serviceTarget === undefined
            ? undefined
            : targetFunction(context.results, serviceTarget.declaration)
        const serviceIdentity =
          serviceTarget === undefined || serviceFunction === undefined
            ? undefined
            : resultEffectIdentity(
                serviceFunction,
                serviceTarget,
                context.results,
                context.index,
                context.resolving,
              )
        if (argument !== undefined && service !== undefined && serviceIdentity !== undefined) {
          const inheritedResolver = context.resolveServiceEffectIdentity
          identity = effectOriginOf(argument, {
            ...context,
            resolveServiceEffectIdentity: (candidate) =>
              candidate === service.expression ? serviceIdentity : inheritedResolver?.(candidate),
          })
        }
      }
      if (identity === undefined) return undefined
      hiddenArguments.push(Type.effectIdentityArgument(identity))
    }
    for (const ordinal of callableParameterOrdinals(target, targetSubstitution)) {
      const argument = callableApplicationArgument(expression, ordinal, context)
      const identity = argument === undefined ? undefined : callableOriginOf(argument, context)
      if (identity === undefined) return undefined
      hiddenArguments.push(identity)
    }
    const key = keyOf(declaration, target.contract, parameters, [
      ...callable.typeArguments,
      ...hiddenArguments,
    ])
    context.recordResolvedCall?.(expression, key)
    return key
  }

  interface EffectOriginContext {
    readonly fn: Hir.HirFunction
    readonly owner: InstanceKey
    readonly substitution: Type.Substitution
    readonly results: ReadonlyMap<string, Elaboration.Result>
    readonly index: DeclarationIndex.Index
    readonly resolving: ReadonlySet<string>
    readonly resolveEffectIdentity?: (identity: Type.EffectIdentityArgument) => string | undefined
    readonly serviceRecipesOfIdentity?: (
      identity: string,
      resolving: ReadonlySet<string>,
    ) => ReadonlyArray<ServiceEffectRecipe>
    readonly resolveServiceEffectIdentity?: (
      expression: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>,
    ) => string | undefined
    readonly recordResolvedCall?: (
      expression: Extract<
        Hir.Expression,
        { readonly _tag: 'Call' | 'EffectConstruct' | 'CallableApply' }
      >,
      target: InstanceKey,
    ) => void
  }

  interface ServiceEffectRecipe {
    readonly expression: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>
    readonly context: EffectOriginContext
  }

  function returnedExpression(fn: Hir.HirFunction): Hir.Expression | undefined {
    const terminal = fn.statements.at(-1)
    return terminal?._tag === 'Return' ? terminal.expression : undefined
  }

  function resultCallableIdentity(
    fn: Hir.HirFunction,
    owner: InstanceKey,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
    resolving: ReadonlySet<string> = new Set(),
  ): Type.CallableIdentityArgument | undefined {
    const substitution = instanceSubstitution(fn, owner)
    const expression = returnedExpression(fn)
    if (substitution === undefined || expression === undefined || fn.contract._tag !== 'Contract')
      return undefined
    const result = Type.substitute(fn.contract.result, substitution)
    const contract = Type.isRepresented(result) ? result.contract : result
    if (!Type.isCallable(contract)) return undefined
    const identity = keyText(owner)
    if (resolving.has(identity)) return undefined
    return callableOriginOf(expression, {
      fn,
      owner,
      substitution,
      results,
      index,
      resolving: new Set(resolving).add(identity),
    })
  }

  function serviceEffectRecipes(
    expression: Hir.Expression,
    context: EffectOriginContext,
    resolving: ReadonlySet<number> = new Set(),
  ): ReadonlyArray<ServiceEffectRecipe> {
    if (expression._tag === 'ServiceEffectConstruct')
      return Object.freeze([Object.freeze({ expression, context })])
    if (expression._tag === 'EffectBlock')
      return Object.freeze(
        expression.statements.flatMap((statement) =>
          Hir.statementExpressions(statement).flatMap((nested) =>
            serviceEffectRecipes(nested, context, resolving),
          ),
        ),
      )
    if (expression._tag === 'Run')
      return serviceEffectRecipes(expression.subject, context, resolving)
    if (expression._tag === 'Move')
      return serviceEffectRecipes(expression.subject, context, resolving)
    if (expression._tag === 'UnionConvert')
      return serviceEffectRecipes(expression.source, context, resolving)
    if (expression._tag === 'Match')
      return Object.freeze(
        expression.arms.flatMap((arm) =>
          arm.reachable ? serviceEffectRecipes(arm.result, context, resolving) : [],
        ),
      )
    if (expression._tag === 'EffectCatch')
      return Object.freeze([
        ...serviceEffectRecipes(expression.protected, context, resolving),
        ...serviceEffectRecipes(expression.handler, context, resolving),
      ])
    if (expression._tag === 'Call' || expression._tag === 'EffectConstruct')
      return Object.freeze(
        expression.arguments.flatMap((argument) =>
          serviceEffectRecipes(argument, context, resolving),
        ),
      )
    if (expression._tag === 'CallableSection')
      return Object.freeze(
        expression.captures.flatMap((capture) =>
          serviceEffectRecipes(capture.value, context, resolving),
        ),
      )
    if (expression._tag === 'CallableApply') {
      const children =
        expression.evaluation === 'LeftThenCallable'
          ? [...expression.arguments, expression.callee]
          : [expression.callee, ...expression.arguments]
      return Object.freeze(
        children.flatMap((child) => serviceEffectRecipes(child, context, resolving)),
      )
    }
    if (expression._tag === 'BuiltinCall' || expression._tag === 'BoundOperationCall')
      return Object.freeze(
        expression.arguments.flatMap((argument) =>
          serviceEffectRecipes(argument, context, resolving),
        ),
      )
    if (expression._tag === 'ParameterReference') {
      const identity = parameterEffectIdentity(
        context.fn,
        context.owner,
        expression.parameter.ordinal,
      )
      return identity === undefined
        ? Object.freeze([])
        : (context.serviceRecipesOfIdentity?.(identity, context.resolving) ?? Object.freeze([]))
    }
    if (expression._tag !== 'BindingReference') return Object.freeze([])
    const ordinal = expression.binding.ordinal
    if (resolving.has(ordinal)) return Object.freeze([])
    const initializer = callableBindings(context.fn).get(ordinal)
    return initializer === undefined
      ? Object.freeze([])
      : serviceEffectRecipes(initializer, context, new Set(resolving).add(ordinal))
  }

  const serviceEffectRecipe = (
    expression: Hir.Expression,
    context: EffectOriginContext,
  ): ServiceEffectRecipe | undefined => {
    const recipes = serviceEffectRecipes(expression, context)
    return recipes.length === 1 ? recipes.at(0) : undefined
  }

  const constrainedServiceTarget = (
    target: Hir.HirFunction,
    targetSubstitution: Type.Substitution,
    effectParameter: number,
    service: ServiceEffectRecipe,
    context: EffectOriginContext,
  ): InstanceKey | undefined => {
    if (target.contract._tag !== 'Contract') return undefined
    const parameter = target.contract.parameters.at(effectParameter)
    const effect =
      parameter === undefined ? undefined : Type.substitute(parameter, targetSubstitution)
    if (effect === undefined || !Type.isEffect(effect)) return undefined
    for (const constraint of target.contract.constraints) {
      if (constraint._tag !== 'ProviderSelectionConstraint') continue
      const provider = Type.substitute(constraint.provider, targetSubstitution)
      const selected = Type.substituteRequirementsRow(constraint.selected, targetSubstitution)
      const source = Type.substituteRequirementsRow(constraint.source, targetSubstitution)
      const concrete = RowAlgebra.concretize(Type.requirementRowPolicy(), selected)
      const requirement = concrete._tag === 'Concrete' ? concrete.row.members.at(0) : undefined
      const hasProviderParameter = target.contract.parameters.some((candidate, ordinal) => {
        if (ordinal === effectParameter) return false
        const specialized = Type.substitute(candidate, targetSubstitution)
        if (constraint.mode === 'Take') return Type.equals(specialized, provider)
        return (
          Type.isReference(specialized) &&
          specialized.access === constraint.mode &&
          Type.equals(specialized.target, provider)
        )
      })
      if (
        !Type.isNominal(provider) ||
        concrete._tag !== 'Concrete' ||
        concrete.row.members.length !== 1 ||
        requirement === undefined ||
        !Type.isNominal(requirement.capability) ||
        !RowAlgebra.equals(Type.requirementRowPolicy(), effect.requirementRow, source) ||
        !hasProviderParameter ||
        !Type.equals(
          Type.substitute(service.expression.service, service.context.substitution),
          requirement.capability,
        ) ||
        service.expression.role !== requirement.role ||
        (service.expression.access !== 'Shared' &&
          constraint.mode !== 'Exclusive' &&
          constraint.mode !== 'Take')
      )
        continue
      const witness = ConformanceProof.witness(context.index, provider, requirement.capability)
      if (witness?._tag === 'SourceConformanceWitness')
        return targetKeyOfServiceCall(service.expression, witness, service.context)
    }
    return undefined
  }

  function forwardedServiceTargetOfCallableApply(
    expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
    target: Hir.HirFunction,
    targetSubstitution: Type.Substitution,
    effectParameter: number,
    context: EffectOriginContext,
    serviceOverride?: ServiceEffectRecipe,
  ): InstanceKey | undefined {
    const binding = forwardedRequirementBinding(target)
    const argument = callableApplicationArgument(expression, effectParameter, context)
    const service =
      serviceOverride ??
      (argument === undefined ? undefined : serviceEffectRecipe(argument, context))
    if (service === undefined) return undefined
    if (binding === undefined)
      return constrainedServiceTarget(target, targetSubstitution, effectParameter, service, context)
    const selected = selectedRequirement(binding, targetSubstitution)
    const witness = requirementBindingWitness(binding, targetSubstitution, context.index)
    if (
      selected === undefined ||
      !Type.isNominal(selected.capability) ||
      witness?._tag !== 'SourceConformanceWitness' ||
      !Type.equals(
        Type.substitute(service.expression.service, service.context.substitution),
        selected.capability,
      ) ||
      service.expression.role !== selected.role ||
      (service.expression.access !== 'Shared' &&
        binding.provider.selectionAccess !== 'Exclusive' &&
        binding.provider.selectionAccess !== 'Take')
    )
      return undefined
    return targetKeyOfServiceCall(service.expression, witness, service.context)
  }

  const forwardedServiceTargetOfCall = (
    expression: Extract<Hir.Expression, { readonly _tag: 'Call' | 'EffectConstruct' }>,
    target: Hir.HirFunction,
    targetSubstitution: Type.Substitution,
    effectParameter: number,
    context: EffectOriginContext,
    serviceOverride?: ServiceEffectRecipe,
  ): InstanceKey | undefined => {
    const binding = forwardedRequirementBinding(target)
    const argument = expression.arguments.at(effectParameter)
    const service =
      serviceOverride ??
      (argument === undefined ? undefined : serviceEffectRecipe(argument, context))
    if (service === undefined) return undefined
    if (binding === undefined)
      return constrainedServiceTarget(target, targetSubstitution, effectParameter, service, context)
    const selected = selectedRequirement(binding, targetSubstitution)
    const witness = requirementBindingWitness(binding, targetSubstitution, context.index)
    if (
      selected === undefined ||
      !Type.isNominal(selected.capability) ||
      witness?._tag !== 'SourceConformanceWitness' ||
      !Type.equals(
        Type.substitute(service.expression.service, service.context.substitution),
        selected.capability,
      ) ||
      service.expression.role !== selected.role ||
      (service.expression.access !== 'Shared' &&
        binding.provider.selectionAccess !== 'Exclusive' &&
        binding.provider.selectionAccess !== 'Take')
    )
      return undefined
    return targetKeyOfServiceCall(service.expression, witness, service.context)
  }

  const compositeEffectRepresentationOf = (
    expression: Hir.Expression,
    context: EffectOriginContext,
  ): Type.CompositeEffectRepresentationArgument | undefined => {
    if (expression._tag === 'Unavailable') return undefined
    const specialized = Specialization.specializeType(context.owner, expression.type, [
      context.substitution,
    ])
    if (
      Type.isRepresented(specialized) &&
      Type.isEffect(specialized.contract) &&
      Type.isCompositeEffectRepresentationArgument(specialized.representation.argument)
    )
      return specialized.representation.argument
    if (expression._tag === 'BindingReference') {
      const initializer = callableBindings(context.fn).get(expression.binding.ordinal)
      return initializer === undefined
        ? undefined
        : compositeEffectRepresentationOf(initializer, context)
    }
    if (expression._tag === 'ParameterReference') {
      const representation = parameterEffectRepresentationArgument(
        context.fn,
        context.owner,
        expression.parameter.ordinal,
      )
      return representation !== undefined &&
        Type.isCompositeEffectRepresentationArgument(representation)
        ? representation
        : undefined
    }
    if (expression._tag === 'Move')
      return compositeEffectRepresentationOf(expression.subject, context)
    if (expression._tag === 'UnionConvert')
      return compositeEffectRepresentationOf(expression.source, context)
    if (expression._tag === 'Call' || expression._tag === 'EffectConstruct') {
      const targetKey = targetKeyOfCall(expression, context)
      const target =
        targetKey === undefined ? undefined : targetFunction(context.results, targetKey.declaration)
      const substitution =
        targetKey === undefined || target === undefined
          ? undefined
          : instanceSubstitution(target, targetKey)
      const returned = target === undefined ? undefined : returnedExpression(target)
      if (
        substitution === undefined ||
        returned === undefined ||
        targetKey === undefined ||
        target === undefined
      )
        return undefined
      return compositeEffectRepresentationOf(returned, {
        ...context,
        fn: target,
        owner: targetKey,
        substitution,
      })
    }
    if (expression._tag === 'CallableApply') {
      const targetKey = targetKeyOfCallableApply(expression, context)
      const target =
        targetKey === undefined ? undefined : targetFunction(context.results, targetKey.declaration)
      const substitution =
        targetKey === undefined || target === undefined
          ? undefined
          : instanceSubstitution(target, targetKey)
      const returned = target === undefined ? undefined : returnedExpression(target)
      if (
        substitution === undefined ||
        returned === undefined ||
        targetKey === undefined ||
        target === undefined
      )
        return undefined
      return compositeEffectRepresentationOf(returned, {
        ...context,
        fn: target,
        owner: targetKey,
        substitution,
      })
    }
    return undefined
  }

  const targetKeyOfCall = (
    expression: Extract<Hir.Expression, { readonly _tag: 'Call' | 'EffectConstruct' }>,
    context: EffectOriginContext,
  ): InstanceKey | undefined => {
    const target = targetFunction(context.results, expression.target)
    if (target === undefined) return undefined
    const typeArguments = expression.typeArguments.map((argument) =>
      Type.substituteGenericArgument(argument, context.substitution),
    )
    const targetSubstitution = TypeInference.substitution(
      target.declaration.typeParameters.map((parameter) => parameter.type),
      typeArguments,
    )
    if (targetSubstitution === undefined) return undefined
    const hiddenArguments: Array<
      | Type.EffectIdentityArgument
      | Type.CallableIdentityArgument
      | Type.CompositeEffectRepresentationArgument
    > = []
    for (const ordinal of effectParameterOrdinals(target, targetSubstitution)) {
      const argument = expression.arguments.at(ordinal)
      const compositeRepresentation =
        argument === undefined ? undefined : compositeEffectRepresentationOf(argument, context)
      if (compositeRepresentation !== undefined) {
        hiddenArguments.push(compositeRepresentation)
        continue
      }
      let identity = argument === undefined ? undefined : effectOriginOf(argument, context)
      if (identity === undefined) {
        const service = argument === undefined ? undefined : serviceEffectRecipe(argument, context)
        const serviceTarget = forwardedServiceTargetOfCall(
          expression,
          target,
          targetSubstitution,
          ordinal,
          context,
          service,
        )
        const serviceFunction =
          serviceTarget === undefined
            ? undefined
            : targetFunction(context.results, serviceTarget.declaration)
        const serviceIdentity =
          serviceTarget === undefined || serviceFunction === undefined
            ? undefined
            : resultEffectIdentity(
                serviceFunction,
                serviceTarget,
                context.results,
                context.index,
                context.resolving,
              )
        if (argument !== undefined && service !== undefined && serviceIdentity !== undefined) {
          const inheritedResolver = context.resolveServiceEffectIdentity
          identity = effectOriginOf(argument, {
            ...context,
            resolveServiceEffectIdentity: (candidate) =>
              candidate === service.expression ? serviceIdentity : inheritedResolver?.(candidate),
          })
        }
      }
      if (identity === undefined) return undefined
      hiddenArguments.push(Type.effectIdentityArgument(identity))
    }
    for (const ordinal of callableParameterOrdinals(target, targetSubstitution)) {
      const argument = expression.arguments.at(ordinal)
      const identity = argument === undefined ? undefined : callableOriginOf(argument, context)
      if (identity === undefined) return undefined
      hiddenArguments.push(identity)
    }
    const key = keyOf(
      expression.target,
      target.contract,
      target.declaration.typeParameters.map((parameter) => parameter.type),
      [...typeArguments, ...hiddenArguments],
    )
    context.recordResolvedCall?.(expression, key)
    return key
  }

  function targetKeyOfServiceCall(
    expression: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>,
    witness: Extract<
      DeclarationFacts.ConformanceWitness,
      { readonly _tag: 'SourceConformanceWitness' }
    >,
    context: EffectOriginContext,
  ): InstanceKey | undefined {
    const operation = ConformanceProof.witnessOperation(witness, expression.operation)
    const target = operation === undefined ? undefined : targetFunction(context.results, operation)
    if (operation === undefined || target === undefined) return undefined
    const service = Type.substitute(expression.service, context.substitution)
    const serviceArgumentCount = Type.isNominal(service) ? service.arguments.length : 0
    const typeArguments = [
      ...witness.typeArguments,
      ...expression.typeArguments
        .slice(serviceArgumentCount)
        .map((argument) => Type.substituteGenericArgument(argument, context.substitution)),
    ]
    const targetSubstitution = TypeInference.substitution(
      target.declaration.typeParameters.map((parameter) => parameter.type),
      typeArguments,
    )
    if (targetSubstitution === undefined) return undefined
    const hiddenArguments: Array<
      | Type.EffectIdentityArgument
      | Type.CallableIdentityArgument
      | Type.CompositeEffectRepresentationArgument
    > = []
    for (const ordinal of effectParameterOrdinals(target, targetSubstitution)) {
      const argument = ordinal === 0 ? undefined : expression.arguments.at(ordinal - 1)
      const compositeRepresentation =
        argument === undefined ? undefined : compositeEffectRepresentationOf(argument, context)
      if (compositeRepresentation !== undefined) {
        hiddenArguments.push(compositeRepresentation)
        continue
      }
      const identity = argument === undefined ? undefined : effectOriginOf(argument, context)
      if (identity === undefined) return undefined
      hiddenArguments.push(Type.effectIdentityArgument(identity))
    }
    for (const ordinal of callableParameterOrdinals(target, targetSubstitution)) {
      const argument = ordinal === 0 ? undefined : expression.arguments.at(ordinal - 1)
      const identity = argument === undefined ? undefined : callableOriginOf(argument, context)
      if (identity === undefined) return undefined
      hiddenArguments.push(identity)
    }
    return keyOf(
      operation,
      target.contract,
      target.declaration.typeParameters.map((parameter) => parameter.type),
      [...typeArguments, ...hiddenArguments],
    )
  }

  const resultEffectIdentity = (
    fn: Hir.HirFunction,
    owner: InstanceKey,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
    resolving: ReadonlySet<string> = new Set(),
  ): string | undefined => {
    const substitution = instanceSubstitution(fn, owner)
    const expression = returnedExpression(fn)
    if (substitution === undefined || expression === undefined) return undefined
    if (
      fn.contract._tag !== 'Contract' ||
      !Type.isEffect(Type.substitute(fn.contract.result, substitution))
    )
      return undefined
    const identity = keyText(owner)
    if (resolving.has(identity)) return undefined
    return effectOriginOf(expression, {
      fn,
      owner,
      substitution,
      results,
      index,
      resolving: new Set(resolving).add(identity),
    })
  }

  const effectOriginOf = (
    expression: Hir.Expression,
    context: EffectOriginContext,
  ): string | undefined => {
    const exactIdentity = (type: Type.Type): string | undefined => {
      const specialized = Specialization.specializeType(context.owner, type, [context.substitution])
      return Type.isRepresented(specialized) &&
        Type.isEffect(specialized.contract) &&
        Type.isExactRepresentationArgument(specialized.representation.argument) &&
        Type.isEffectIdentityArgument(specialized.representation.argument.identity)
        ? (context.resolveEffectIdentity?.(specialized.representation.argument.identity) ??
            specialized.representation.argument.identity.identity)
        : undefined
    }
    if (
      expression._tag !== 'Unavailable' &&
      expression._tag !== 'Call' &&
      expression._tag !== 'EffectConstruct' &&
      expression._tag !== 'ServiceEffectConstruct'
    ) {
      const identity = exactIdentity(expression.type)
      if (identity !== undefined) return identity
    }
    if (expression._tag === 'Call' || expression._tag === 'EffectConstruct') {
      const target = targetFunction(context.results, expression.target)
      if (target !== undefined && forwardedRequirementBinding(target) !== undefined) {
        const identity = exactIdentity(expression.type)
        if (identity !== undefined) return identity
      }
    }
    if (expression._tag === 'Project') {
      const declaration = DeclarationFacts.byCanonical(context.index, {
        _tag: 'CanonicalDeclarationId',
        module: expression.nominal.module,
        name: expression.nominal.name,
      })
      const field =
        declaration?._tag === 'StructDeclaration'
          ? declaration.fields.find(
              (candidate) => candidate.id.ordinal === expression.field.ordinal,
            )
          : undefined
      const substitution =
        declaration?._tag === 'StructDeclaration'
          ? TypeInference.substitution(
              declaration.typeParameters.map((parameter) => parameter.type),
              expression.nominal.arguments,
            )
          : undefined
      if (field?.declaredType._tag === 'RepresentationParameter' && substitution !== undefined) {
        const argument = substitution.get(Type.key(field.declaredType.parameter))
        if (
          argument !== undefined &&
          Type.isExactRepresentationArgument(argument) &&
          Type.isEffectIdentityArgument(argument.identity)
        )
          return context.resolveEffectIdentity?.(argument.identity) ?? argument.identity.identity
      }
      if (field?.declaredType._tag === 'Resolved' && substitution !== undefined) {
        const projected = Type.substitute(field.declaredType.type, substitution)
        const identity = exactIdentity(projected)
        if (identity !== undefined) return identity
      }
    }
    if (expression._tag === 'EffectBlock') return effectIdentity(context.owner, expression.site)
    if (expression._tag === 'EffectCatch')
      return effectIdentity(
        context.owner,
        Hir.effectCatchSite(context.fn.declaration.id, context.owner.declaration, expression.span),
      )
    if (
      (expression._tag === 'BoundOperationCall' || expression._tag === 'BuiltinCall') &&
      expression.witnessEffectSite !== undefined
    )
      return effectIdentity(context.owner, expression.witnessEffectSite)
    if (expression._tag === 'ParameterReference')
      return parameterEffectIdentity(context.fn, context.owner, expression.parameter.ordinal)
    if (expression._tag === 'BindingReference') {
      const initializer = callableBindings(context.fn).get(expression.binding.ordinal)
      return initializer === undefined ? undefined : effectOriginOf(initializer, context)
    }
    if (expression._tag === 'Move') return effectOriginOf(expression.subject, context)
    if (expression._tag === 'UnionConvert') return effectOriginOf(expression.source, context)
    if (expression._tag === 'Match') {
      const identities = expression.arms.flatMap((arm) => {
        if (!arm.reachable) return []
        const identity = effectOriginOf(arm.result, context)
        return identity === undefined ? [] : [identity]
      })
      return identities.length !== 0 && new Set(identities).size === 1
        ? identities.at(0)
        : undefined
    }
    if (expression._tag === 'ServiceEffectConstruct')
      return context.resolveServiceEffectIdentity?.(expression)
    if (expression._tag === 'CallableApply') {
      const targetKey = targetKeyOfCallableApply(expression, context)
      if (targetKey === undefined) return undefined
      const target = targetFunction(context.results, targetKey.declaration)
      if (target === undefined) return undefined
      return resultEffectIdentity(
        target,
        targetKey,
        context.results,
        context.index,
        context.resolving,
      )
    }
    if (expression._tag !== 'Call' && expression._tag !== 'EffectConstruct') return undefined
    const targetKey = targetKeyOfCall(expression, context)
    const target =
      targetKey === undefined ? undefined : targetFunction(context.results, expression.target)
    if (targetKey === undefined || target === undefined) return undefined
    return resultEffectIdentity(
      target,
      targetKey,
      context.results,
      context.index,
      context.resolving,
    )
  }

  const effectSuccesses = (
    fn: Hir.HirFunction,
    owner: InstanceKey,
    substitution: Type.Substitution,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
  ): NonNullable<Instance['effectSuccesses']> => {
    const context: EffectOriginContext = {
      fn,
      owner,
      substitution,
      results,
      index,
      resolving: new Set<string>(),
    }
    return Object.freeze(
      fn.statements
        .flatMap(Hir.statementExpressions)
        .flatMap(Hir.expressionTree)
        .flatMap((expression) => {
          if (expression._tag !== 'EffectBlock') return []
          const success = Type.substitute(expression.type.success, substitution)
          if (!Type.isEffect(success)) return []
          const terminal = expression.statements.at(-1)
          const identity =
            terminal?._tag === 'Return' ? effectOriginOf(terminal.expression, context) : undefined
          return identity === undefined ? [] : [Object.freeze({ site: expression.site, identity })]
        }),
    )
  }

  const directCallInstances = (
    fn: Hir.HirFunction,
    owner: InstanceKey,
    substitution: Type.Substitution,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
  ): ReadonlyArray<CallInstance> => {
    const expressions = fn.statements.flatMap(Hir.statementExpressions).flatMap(Hir.expressionTree)
    const expressionOrder = new Map(expressions.map((expression, ordinal) => [expression, ordinal]))
    const calls = new Map<string, { readonly call: CallInstance; readonly ordinal: number }>()
    const record = (
      expression: Extract<
        Hir.Expression,
        { readonly _tag: 'Call' | 'EffectConstruct' | 'CallableApply' }
      >,
      target: InstanceKey,
    ): void => {
      const ordinal = expressionOrder.get(expression)
      // Origin inference may inspect a callee's returned expression. Those calls belong to that
      // callee's discovery pass, not to the owner whose argument identity is currently inferred.
      if (ordinal === undefined) return
      const targetFn = targetFunction(results, target.declaration)
      if (targetFn === undefined) return
      const resultEffect = resultEffectIdentity(targetFn, target, results, index)
      const span = expression.span
      calls.set(
        `${span.sourceId}:${span.start}:${span.end}\u0000${keyText(target)}`,
        Object.freeze({
          call: Object.freeze({
            _tag: 'CallInstance',
            owner,
            span,
            target,
            ...(resultEffect === undefined ? {} : { resultEffect }),
          }),
          ordinal,
        }),
      )
    }
    const context: EffectOriginContext = {
      fn,
      owner,
      substitution,
      results,
      index,
      resolving: new Set<string>(),
      recordResolvedCall: record,
    }
    expressions.forEach((expression) => {
      if (expression._tag === 'CallableApply' && Type.isEffect(expression.type)) {
        const target = targetKeyOfCallableApply(expression, context)
        if (target === undefined) return
        record(expression, target)
        return
      }
      if (expression._tag !== 'Call' && expression._tag !== 'EffectConstruct') return
      // Calls carrying neither an Effect nor a callable value remain on the original
      // finite-specialization path. Resolving them here as well would bypass its
      // polymorphic-recursion guard.
      if (!carriesHiddenIdentity(expression, substitution)) return
      const target = targetKeyOfCall(expression, context)
      if (target !== undefined) record(expression, target)
    })
    return Object.freeze(
      [...calls.values()]
        .sort((left, right) => left.ordinal - right.ordinal)
        .map(({ call }) => call),
    )
  }

  const callableValue = (
    expression: Hir.Expression,
    bindings: ReadonlyMap<number, Hir.Expression>,
  ): Extract<Hir.Expression, { readonly _tag: 'FunctionItem' | 'CallableSection' }> | undefined => {
    if (expression._tag === 'FunctionItem' || expression._tag === 'CallableSection')
      return expression
    if (expression._tag === 'BindingReference') {
      const initializer = bindings.get(expression.binding.ordinal)
      return initializer === undefined ? undefined : callableValue(initializer, bindings)
    }
    if (expression._tag === 'Move') return callableValue(expression.subject, bindings)
    return undefined
  }

  const mergeSubstitution = (
    first: Type.Substitution,
    second: Type.Substitution,
  ): Type.Substitution => new Map([...first, ...second])

  const callableExpressions = (fn: Hir.HirFunction): ReadonlyArray<Hir.Expression> =>
    fn.statements.flatMap((statement) =>
      Hir.statementExpressions(statement).flatMap(Hir.expressionTree),
    )

  const declarationTarget = (
    target: Hir.CallableTarget,
  ): DeclarationFacts.CanonicalId | undefined =>
    target._tag === 'DeclarationCallableTarget' ? target.declaration : undefined

  function targetFunction(
    results: ReadonlyMap<string, Elaboration.Result>,
    target: DeclarationFacts.CanonicalId,
  ): Hir.HirFunction | undefined {
    return results
      .get(target.module)
      ?.hir.functions.find(
        (candidate) =>
          candidate.declaration.canonical._tag === 'Canonical' &&
          candidate.declaration.canonical.id.name === target.name,
      )
  }

  const targetArguments = (
    target: Hir.CallableTarget,
    substitution: Type.Substitution,
    results: ReadonlyMap<string, Elaboration.Result>,
  ): ReadonlyArray<Type.GenericArgument> | undefined => {
    const declaration = declarationTarget(target)
    if (declaration === undefined) return Object.freeze([])
    const fn = targetFunction(results, declaration)
    if (fn === undefined) return undefined
    const arguments_ = fn.declaration.typeParameters.flatMap((parameter) => {
      const type = substitution.get(Type.key(parameter.type))
      return type === undefined ? [] : [type]
    })
    return arguments_.length === fn.declaration.typeParameters.length
      ? Object.freeze(arguments_)
      : undefined
  }

  const callableCallTargets = (
    fn: Hir.HirFunction,
    owner: InstanceKey,
    ownerSubstitution: Type.Substitution,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
  ): ReadonlyArray<CallTarget> => {
    const bindings = callableBindings(fn)
    const targets: Array<CallTarget> = []
    const context: EffectOriginContext = Object.freeze({
      fn,
      owner,
      substitution: ownerSubstitution,
      results,
      index,
      resolving: new Set<string>(),
    })
    for (const expression of callableExpressions(fn)) {
      if (expression._tag === 'CallableApply') {
        const key = targetKeyOfCallableApply(expression, context)
        if (key !== undefined) {
          targets.push(
            Object.freeze({ declaration: key.declaration, typeArguments: key.typeArguments }),
          )
          continue
        }
        // A finite Effect composite deliberately has no single hidden effect identity. Preserve
        // the ordinary specialization edge for such applications so lowering can recover the
        // result representation from the specialized declaration, while exact callable/effect
        // identities above continue to select their fully specialized target.
        const fallback =
          callableValue(expression.callee, bindings) ??
          staticallyForwardedCallable(expression.callee, fn, results)
        if (fallback?._tag === 'FunctionItem' || fallback?._tag === 'CallableSection') {
          const declaration = declarationTarget(fallback.target)
          const substitution =
            fallback._tag === 'CallableSection'
              ? mergeSubstitution(fallback.substitution, expression.substitution)
              : expression.substitution
          const arguments_ = targetArguments(fallback.target, substitution, results)
          if (declaration !== undefined && arguments_ !== undefined)
            targets.push(Object.freeze({ declaration, typeArguments: arguments_ }))
        }
        continue
      }
      const value =
        expression._tag === 'FunctionItem' || expression._tag === 'CallableSection'
          ? expression
          : undefined
      if (
        value === undefined ||
        (value._tag !== 'FunctionItem' && value._tag !== 'CallableSection')
      )
        continue
      const declaration = declarationTarget(value.target)
      if (declaration === undefined) continue
      const target = targetFunction(results, declaration)
      if (target === undefined) continue
      const substitution = value._tag === 'CallableSection' ? value.substitution : new Map()
      let arguments_: ReadonlyArray<Type.GenericArgument> | undefined
      if (value._tag === 'FunctionItem') {
        if (value.typeArguments.length === target.declaration.typeParameters.length)
          arguments_ = Object.freeze(
            value.typeArguments.map((argument) =>
              Type.substituteGenericArgument(argument, ownerSubstitution),
            ),
          )
      } else {
        arguments_ = targetArguments(value.target, substitution, results)
      }
      if (arguments_ === undefined) continue
      const targetSubstitution = TypeInference.substitution(
        target.declaration.typeParameters.map((parameter) => parameter.type),
        arguments_,
      )
      if (targetSubstitution === undefined) continue
      const hidden: Array<Type.EffectIdentityArgument | Type.CallableIdentityArgument> = []
      let complete = true
      for (const ordinal of effectParameterOrdinals(target, targetSubstitution)) {
        const capture =
          value._tag === 'CallableSection'
            ? value.captures.find((candidate) => candidate.parameterOrdinal === ordinal)
            : undefined
        const identity = capture === undefined ? undefined : effectOriginOf(capture.value, context)
        if (identity === undefined) {
          complete = false
          break
        }
        hidden.push(Type.effectIdentityArgument(identity))
      }
      if (!complete) continue
      for (const ordinal of callableParameterOrdinals(target, targetSubstitution)) {
        const capture =
          value._tag === 'CallableSection'
            ? value.captures.find((candidate) => candidate.parameterOrdinal === ordinal)
            : undefined
        const identity =
          capture === undefined ? undefined : callableOriginOf(capture.value, context)
        if (identity === undefined) {
          complete = false
          break
        }
        hidden.push(identity)
      }
      if (complete)
        targets.push(
          Object.freeze({ declaration, typeArguments: Object.freeze([...arguments_, ...hidden]) }),
        )
    }
    return Object.freeze(targets)
  }

  const forwardedRequirementTargets = (
    targets: ReadonlyArray<CallTarget>,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
  ): ReadonlyArray<CallTarget> =>
    targets.flatMap((target) => {
      const fn = targetFunction(results, target.declaration)
      const binding = fn === undefined ? undefined : forwardedRequirementBinding(fn)
      if (fn === undefined || binding === undefined) return []
      const substitution = TypeInference.substitution(
        fn.declaration.typeParameters.map((parameter) => parameter.type),
        target.typeArguments.filter((argument) => !Type.isHiddenExecutableArgument(argument)),
      )
      if (substitution === undefined) return []
      const witness = requirementBindingWitness(binding, substitution, index)
      return witness?._tag === 'SourceConformanceWitness'
        ? witnessDependencyCallTargets(index, witness.provider, witness.capability)
        : []
    })

  const concreteCallables = (
    fn: Hir.HirFunction,
    owner: InstanceKey,
    ownerSubstitution: Type.Substitution,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
  ): ReadonlyArray<CallableInstance> => {
    const expressions = callableExpressions(fn)
    const bindings = callableBindings(fn)
    const sections = expressions.flatMap((expression) =>
      expression._tag === 'CallableSection' ? [expression] : [],
    )
    const seen = new Set<string>()
    const instances: Array<CallableInstance> = []
    for (const section of sections) {
      const site = Hir.executableSiteKey(section.site)
      if (seen.has(site)) continue
      seen.add(site)
      const applications = expressions.flatMap((expression) =>
        expression._tag === 'CallableApply' &&
        (callableValue(expression.callee, bindings) ??
          staticallyForwardedCallable(expression.callee, fn, results)) === section
          ? [expression]
          : [],
      )
      const candidates: ReadonlyArray<Type.Substitution> =
        applications.length === 0
          ? [new Map()]
          : applications.map((application) => application.substitution)
      for (const applicationSubstitution of candidates) {
        const raw = mergeSubstitution(section.substitution, applicationSubstitution)
        const substitution = new Map(
          [...raw].map(([parameter, argument]) => [
            parameter,
            Type.substituteGenericArgument(argument, ownerSubstitution),
          ]),
        )
        const type = specializeInstanceType(section.type, owner, [ownerSubstitution, substitution])
        const arguments_ = targetArguments(section.target, substitution, results)
        const captureTypes = section.captures.flatMap((capture) =>
          capture.value._tag === 'Unavailable'
            ? []
            : [
                specializeInstanceType(capture.value.type, owner, [
                  ownerSubstitution,
                  substitution,
                ]),
              ],
        )
        if (
          !Type.isCallable(type) ||
          !Type.isRuntimeConcrete(type) ||
          arguments_ === undefined ||
          arguments_.some((argument) => !Type.isRuntimeConcreteGenericArgument(argument)) ||
          captureTypes.length !== section.captures.length ||
          captureTypes.some((capture) => !Type.isRuntimeConcrete(capture))
        ) {
          continue
        }
        instances.push(
          Object.freeze({
            _tag: 'CallableInstance',
            owner,
            site: section.site,
            target: section.target,
            typeArguments: arguments_,
            substitution,
            captureTypes: Object.freeze(captureTypes),
            captures: Object.freeze(
              section.captures.flatMap((capture, ordinal) => {
                const type_ = captureTypes.at(ordinal)
                const callableIdentity =
                  type_ !== undefined && Type.isCallable(type_)
                    ? callableOriginOf(capture.value, {
                        fn,
                        owner,
                        substitution: ownerSubstitution,
                        results,
                        index,
                        resolving: new Set<string>(),
                      })
                    : undefined
                if (type_ === undefined) {
                  return []
                }
                return [
                  Object.freeze({
                    ordinal: capture.ordinal,
                    parameterOrdinal: capture.parameterOrdinal,
                    access: capture.access,
                    type: type_,
                    ...(callableIdentity === undefined ? {} : { callableIdentity }),
                  }),
                ]
              }),
            ),
            type,
            mode: section.mode,
          }),
        )
      }
    }
    return Object.freeze(instances)
  }

  /**
   * Collects specialized semantic facts for every source Effect construction. Represented fields
   * consume these published facts instead of recovering a runner from construction syntax.
   */
  const concreteEffects = (
    instances: ReadonlyArray<Instance>,
    suspension: ReadonlyMap<string, SuspensionMode.Summary>,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
    callables: ReadonlyArray<CallableInstance>,
  ): ReadonlyArray<EffectInstance> => {
    const effects = new Map<string, EffectInstance>()
    for (const instance of instances) {
      const bindings = callableBindings(instance.function)
      const context: EffectOriginContext = Object.freeze({
        fn: instance.function,
        owner: instance.key,
        substitution: instance.substitution,
        results,
        index,
        resolving: new Set<string>(),
      })
      const blocks = callableExpressions(instance.function).flatMap((expression) =>
        expression._tag === 'EffectBlock' ? [expression] : [],
      )
      for (const block of blocks) {
        const specializedType = specializeInstanceType(block.type, instance.key, [
          instance.substitution,
        ])
        if (!Type.isEffect(specializedType) || !Type.isRuntimeConcrete(specializedType)) continue
        const providedRequirements = block.statements
          .flatMap(Hir.statementExpressions)
          .flatMap(Hir.expressionTree)
          .flatMap((expression) => {
            if (expression._tag !== 'EffectBindRequirement') return []
            const selected = selectedRequirement(expression, instance.substitution)
            return selected !== undefined && Type.isNominal(selected.capability)
              ? [
                  Object.freeze({
                    parameter: expression.provider.parameter?.ordinal,
                    capability: selected.capability,
                    role: selected.role,
                    requirementAccess: selected.access,
                    providerAccess: expression.provider.selectionAccess,
                  }),
                ]
              : []
          })
          .sort((left, right) => {
            const leftKey = `${left.parameter ?? -1}\0${Type.key(left.capability)}\0${left.role}`
            const rightKey = `${right.parameter ?? -1}\0${Type.key(right.capability)}\0${right.role}`
            if (leftKey < rightKey) {
              return -1
            }
            if (leftKey > rightKey) {
              return 1
            }
            return 0
          })
        const captures = block.captures.flatMap((capture, ordinal) => {
          const source = capture.binding === undefined ? 'Parameter' : 'Binding'
          const sourceOrdinal = capture.binding?.ordinal ?? capture.parameter?.ordinal
          const initializer =
            sourceOrdinal === undefined || source === 'Parameter'
              ? undefined
              : bindings.get(sourceOrdinal)
          let sourceType: Type.Type | undefined
          if (sourceOrdinal === undefined) {
            sourceType = undefined
          } else if (source === 'Parameter') {
            if (instance.function.contract._tag === 'Contract') {
              sourceType = instance.function.contract.parameters.at(sourceOrdinal)
            } else {
              sourceType = undefined
            }
          } else if (initializer === undefined || initializer._tag === 'Unavailable') {
            sourceType = undefined
          } else {
            sourceType = initializer.type
          }
          if (sourceOrdinal === undefined || sourceType === undefined) return []
          const specialized = specializeInstanceType(sourceType, instance.key, [
            instance.substitution,
          ])
          if (!Type.isRuntimeConcrete(specialized)) return []
          let capturedEffectIdentity: string | undefined
          const representedEffect =
            Type.isRepresented(specialized) &&
            Type.isEffect(specialized.contract) &&
            Type.isExactRepresentationArgument(specialized.representation.argument) &&
            Type.isEffectIdentityArgument(specialized.representation.argument.identity)
          if (Type.isEffect(specialized) || representedEffect) {
            if (source === 'Parameter') {
              capturedEffectIdentity = parameterEffectIdentity(
                instance.function,
                instance.key,
                sourceOrdinal,
              )
            } else if (initializer === undefined) {
              capturedEffectIdentity = undefined
            } else {
              capturedEffectIdentity = effectOriginOf(initializer, context)
            }
          } else {
            capturedEffectIdentity = undefined
          }
          let capturedCallableIdentity: Type.CallableIdentityArgument | undefined
          if (Type.isCallable(specialized)) {
            if (source === 'Parameter') {
              capturedCallableIdentity = parameterCallableIdentity(
                instance.function,
                instance.key,
                sourceOrdinal,
              )
            } else if (initializer === undefined) {
              capturedCallableIdentity = undefined
            } else {
              capturedCallableIdentity = callableOriginOf(initializer, context)
            }
          } else {
            capturedCallableIdentity = undefined
          }
          let providedRequirement:
            | Readonly<{
                parameter: number | undefined
                capability: Type.Nominal
                role: string
                requirementAccess: Type.Requirement['access']
                providerAccess: 'Take' | 'Exclusive' | 'Shared'
              }>
            | undefined
          if (source === 'Parameter') {
            providedRequirement =
              providedRequirements.find((requirement) => requirement.parameter === sourceOrdinal) ??
              instance.specialization.evidence
                .flatMap((evidence) => {
                  if (
                    evidence._tag !== 'RequirementSelection' ||
                    !Type.isNominal(evidence.selected.capability)
                  )
                    return []
                  const providerMatches = Type.isReference(specialized)
                    ? Type.equals(specialized.target, evidence.provider) &&
                      specialized.access === evidence.providerMode
                    : evidence.providerMode === 'Take' &&
                      Type.equals(specialized, evidence.provider)
                  return providerMatches
                    ? [
                        Object.freeze({
                          parameter: sourceOrdinal,
                          capability: evidence.selected.capability,
                          role: evidence.selected.role,
                          requirementAccess: evidence.selected.access,
                          providerAccess: evidence.providerMode,
                        }),
                      ]
                    : []
                })
                .at(0)
          } else {
            providedRequirement = undefined
          }
          return [
            Object.freeze({
              ordinal,
              source,
              sourceOrdinal,
              access: capture.access,
              type: specialized,
              ...(capturedEffectIdentity === undefined
                ? {}
                : { effectIdentity: capturedEffectIdentity }),
              ...(capturedCallableIdentity === undefined
                ? {}
                : { callableIdentity: capturedCallableIdentity }),
              ...(providedRequirement === undefined
                ? {}
                : {
                    providedRequirement: Object.freeze({
                      capability: providedRequirement.capability,
                      role: providedRequirement.role,
                      requirementAccess: providedRequirement.requirementAccess,
                      providerAccess: providedRequirement.providerAccess,
                    }),
                  }),
            }),
          ]
        })
        if (captures.length !== block.captures.length) continue
        const identity = effectIdentity(instance.key, block.site)
        effects.set(
          identity,
          Object.freeze({
            _tag: 'EffectInstance',
            representationIdentity: Hir.effectRepresentationIdentity(block.site),
            identity,
            owner: instance.key,
            site: block.site,
            runner: Hir.effectRunnerId(instance.key.declaration, block.site),
            typeArguments: Object.freeze([...instance.key.typeArguments]),
            captures: Object.freeze(captures),
            type: specializedType,
            suspension: suspension.get(effectNode(identity)) ?? SuspensionMode.direct,
          }),
        )
      }
      const catches = callableExpressions(instance.function).flatMap((expression) =>
        expression._tag === 'EffectCatch' ? [expression] : [],
      )
      for (const catch_ of catches) {
        if (catch_.protected._tag === 'Unavailable' || catch_.handler._tag === 'Unavailable')
          continue
        const type = specializeInstanceType(catch_.type, instance.key, [instance.substitution])
        const protectedType = specializeInstanceType(catch_.protected.type, instance.key, [
          instance.substitution,
        ])
        const handlerType = specializeInstanceType(catch_.handler.type, instance.key, [
          instance.substitution,
        ])
        const protectedIdentity = effectOriginOf(catch_.protected, context)
        const handlerIdentity = callableOriginOf(catch_.handler, context)
        if (
          !Type.isEffect(type) ||
          !Type.isRuntimeConcrete(type) ||
          !Type.isEffect(protectedType) ||
          !Type.isCallable(handlerType) ||
          protectedIdentity === undefined ||
          handlerIdentity === undefined
        )
          continue
        const site = Hir.effectCatchSite(
          instance.function.declaration.id,
          instance.key.declaration,
          catch_.span,
        )
        const identity = effectIdentity(instance.key, site)
        effects.set(
          identity,
          Object.freeze({
            _tag: 'EffectInstance',
            representationIdentity: Hir.effectRepresentationIdentity(site),
            identity,
            owner: instance.key,
            site,
            runner: Hir.effectRunnerId(instance.key.declaration, site),
            typeArguments: Object.freeze([...instance.key.typeArguments]),
            captures: Object.freeze([
              Object.freeze({
                ordinal: 0,
                source: 'Binding' as const,
                sourceOrdinal: 0,
                access: 'Take' as const,
                type: protectedType,
                effectIdentity: protectedIdentity,
              }),
              Object.freeze({
                ordinal: 1,
                source: 'Binding' as const,
                sourceOrdinal: 1,
                access: 'Take' as const,
                type: handlerType,
                callableIdentity: handlerIdentity,
              }),
            ]),
            type,
            suspension: suspension.get(effectNode(identity)) ?? SuspensionMode.direct,
          }),
        )
      }
    }
    const callableFor = (identity: Type.CallableIdentityArgument): CallableInstance | undefined =>
      callables.find(
        (candidate) =>
          identity.environment !== undefined &&
          Type.equalsCallableEnvironmentIdentity(
            identity.environment,
            Hir.callableEnvironmentIdentity(candidate.site, candidate.owner),
          ) &&
          Hir.matchesCallableTargetIdentity(candidate.target, identity.target) &&
          identity.typeArguments.length === candidate.typeArguments.length &&
          identity.typeArguments.every((argument, ordinal) => {
            const expected = candidate.typeArguments.at(ordinal)
            return expected !== undefined && Type.equalsGenericArgument(argument, expected)
          }),
      )
    let refined = new Map(effects)
    for (let pass = 0; pass <= effects.size; pass += 1) {
      let changed = false
      const next = new Map(refined)
      for (const [identity, effect] of refined) {
        if (effect.site.ordinal !== -1) continue
        const captures = effect.captures.map((capture) => {
          if (capture.source !== 'Parameter' || capture.access !== 'Take') return capture
          const capturedEffect =
            capture.effectIdentity === undefined ? undefined : refined.get(capture.effectIdentity)
          const capturedCallable =
            capture.callableIdentity === undefined
              ? undefined
              : callableFor(capture.callableIdentity)
          const copy =
            capturedEffect?.type.access === 'Shared' ||
            capturedCallable?.mode === 'Shared' ||
            (capture.effectIdentity === undefined &&
              capture.callableIdentity === undefined &&
              ConformanceProof.copyType(index, capture.type))
          return copy ? Object.freeze({ ...capture, access: 'Copy' as const }) : capture
        })
        let access: 'Take' | 'Exclusive' | 'Shared'
        if (captures.some((capture) => capture.access === 'Take')) {
          access = 'Take'
        } else if (captures.some((capture) => capture.access === 'Exclusive')) {
          access = 'Exclusive'
        } else {
          access = 'Shared'
        }
        if (
          access === effect.type.access &&
          captures.every((capture, ordinal) => capture === effect.captures.at(ordinal))
        )
          continue
        changed = true
        next.set(
          identity,
          Object.freeze({
            ...effect,
            captures: Object.freeze(captures),
            type: Type.effectWithRows(
              effect.type.success,
              effect.type.failureRow,
              access,
              effect.type.requirementRow,
            ),
          }),
        )
      }
      refined = next
      if (!changed) break
    }
    return Object.freeze(
      [...refined.values()].sort((left, right) => left.identity.localeCompare(right.identity)),
    )
  }

  const functionByKey = (
    results: ReadonlyMap<string, Elaboration.Result>,
    key: InstanceKey,
  ): Hir.HirFunction | undefined =>
    results
      .get(key.declaration.module)
      ?.hir.functions.find(
        (fn) =>
          fn.declaration.canonical._tag === 'Canonical' &&
          fn.declaration.canonical.id.name === key.declaration.name,
      )

  const instanceNode = (key: InstanceKey): string => `instance\u0000${keyText(key)}`

  const suspensionGraph = (
    instances: ReadonlyArray<Instance>,
    results: ReadonlyMap<string, Elaboration.Result>,
    index: DeclarationIndex.Index,
  ): SuspensionGraph => {
    const nestedRoots = new Set<string>()
    const externalRoots = new Set<string>()
    const dependencies = new Map<string, Set<string>>()
    const effectIdentities = new Set<string>()
    const permitted = new Map<string, Set<SuspensionMode.Mode>>()
    const unavailable = new Set<string>()
    const providedTargets = new Map<
      string,
      {
        readonly owner: InstanceKey
        readonly target: InstanceKey
        readonly span: Hir.Expression['span']
      }
    >()
    const serviceCalls = new Map<
      string,
      {
        readonly service: Type.Nominal
        readonly role: string
        readonly access: 'Shared' | 'Exclusive'
        readonly operation: string
        readonly expression: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>
        readonly context: EffectOriginContext
      }
    >()
    interface ProviderBinding {
      readonly node: string
      readonly execution: string
      readonly protectedTargets: ReadonlyArray<string>
      readonly selected: Type.Requirement
      readonly witness: DeclarationFacts.ConformanceWitness
      readonly providerAccess: 'Shared' | 'Exclusive' | 'Take'
    }
    const providerBindings = new Map<string, ProviderBinding>()
    const resolveEffectIdentity = (identity: Type.EffectIdentityArgument): string | undefined => {
      const candidates = instances.flatMap((instance) =>
        callableExpressions(instance.function).flatMap((expression) =>
          expression._tag === 'EffectBlock' &&
          Hir.effectRepresentationIdentity(expression.site) === identity.identity
            ? [
                Object.freeze({
                  owner: instance.key,
                  identity: effectIdentity(instance.key, expression.site),
                }),
              ]
            : [],
        ),
      )
      const owner = identity.owner
      if (owner === undefined)
        return candidates.length === 1 ? candidates.at(0)?.identity : undefined
      const exact = candidates.filter((candidate) => sameExactOwner(candidate.owner, owner))
      if (exact.length === 1) return exact.at(0)?.identity
      const visible = candidates.filter(
        (candidate) =>
          candidate.owner.declaration.module === owner.declaration.module &&
          candidate.owner.declaration.name === owner.declaration.name &&
          sameVisibleTypeArguments(candidate.owner.typeArguments, owner.typeArguments),
      )
      return visible.length === 1 ? visible.at(0)?.identity : undefined
    }
    const serviceRecipesOfIdentity = (
      identity: string,
      resolving: ReadonlySet<string>,
    ): ReadonlyArray<ServiceEffectRecipe> => {
      if (resolving.has(identity)) return Object.freeze([])
      const candidates = instances.filter((candidate) => candidate.resultEffect === identity)
      const candidate = candidates.length === 1 ? candidates.at(0) : undefined
      const expression =
        candidate === undefined ? undefined : returnedExpression(candidate.function)
      if (candidate === undefined || expression === undefined) return Object.freeze([])
      const recipeContext: EffectOriginContext = {
        fn: candidate.function,
        owner: candidate.key,
        substitution: candidate.substitution,
        results,
        index,
        resolving: new Set(resolving).add(identity),
        resolveEffectIdentity,
        serviceRecipesOfIdentity,
      }
      return serviceEffectRecipes(expression, recipeContext)
    }
    const addDependency = (owner: string, target: string): void => {
      const targets = dependencies.get(owner) ?? new Set<string>()
      targets.add(target)
      dependencies.set(owner, targets)
    }
    const executionNodeForKey = (key: InstanceKey): string => {
      const result = instances.find(
        (candidate) => keyText(candidate.key) === keyText(key),
      )?.resultEffect
      return result === undefined ? instanceNode(key) : effectNode(result)
    }
    const serviceCallNode = (
      owner: InstanceKey,
      expression: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>,
    ): string =>
      `service\0${keyText(owner)}\0${expression.span.sourceId}:${expression.span.start}:${expression.span.end}`
    const providerBindingNode = (
      owner: InstanceKey,
      expression: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>,
    ): string =>
      `provider\0${keyText(owner)}\0${expression.span.sourceId}:${expression.span.start}:${expression.span.end}`
    for (const instance of instances) {
      const context: EffectOriginContext = {
        fn: instance.function,
        owner: instance.key,
        substitution: instance.substitution,
        results,
        index,
        resolving: new Set<string>(),
        resolveEffectIdentity,
        serviceRecipesOfIdentity,
      }
      const bindings = callableBindings(instance.function)

      const effectOrigins = (expression: Hir.Expression): ReadonlyArray<string> => {
        if (expression._tag === 'EffectBindRequirement') return effectOrigins(expression.protected)
        if (expression._tag === 'BindingReference') {
          const initializer = bindings.get(expression.binding.ordinal)
          return initializer === undefined ? [] : effectOrigins(initializer)
        }
        if (expression._tag === 'Move') return effectOrigins(expression.subject)
        if (expression._tag === 'UnionConvert') return effectOrigins(expression.source)
        if (expression._tag === 'Match') {
          return Object.freeze([
            ...new Set(
              expression.arms.flatMap((arm) => (arm.reachable ? effectOrigins(arm.result) : [])),
            ),
          ])
        }
        const composite = compositeEffectRepresentationOf(expression, context)
        if (composite !== undefined)
          return Object.freeze(
            composite.alternatives.flatMap((alternative) => {
              if (!Type.isEffectIdentityArgument(alternative.identity)) return []
              const identity = resolveEffectIdentity(alternative.identity)
              return identity === undefined ? [] : [identity]
            }),
          )
        const identity = effectOriginOf(expression, context)
        return identity === undefined ? [] : Object.freeze([identity])
      }

      const selectedInterfaceEffectTarget = (
        expression: Extract<
          Hir.Expression,
          { readonly _tag: 'BoundOperationCall' | 'BuiltinCall' }
        >,
      ): InstanceKey | undefined => {
        const bound =
          expression._tag === 'BoundOperationCall' ? expression : expression.interfaceOperation
        if (bound === undefined || expression.witnessEffectSite === undefined) return undefined
        const capability = Type.substitute(bound.capability, instance.substitution)
        const provider = Type.substitute(bound.provider, instance.substitution)
        if (!Type.isNominal(capability)) return undefined
        const selected = ConformanceProof.interfaceWitnessTarget(
          index,
          provider,
          capability,
          bound.operation,
        )
        const target =
          selected === undefined ? undefined : targetFunction(results, selected.implementation)
        return selected === undefined || target === undefined
          ? undefined
          : keyOf(
              selected.implementation,
              target.contract,
              target.declaration.typeParameters.map((parameter) => parameter.type),
              selected.typeArguments,
            )
      }

      const executionTargets = (expression: Hir.Expression): ReadonlyArray<string> => {
        if (expression._tag === 'EffectBindRequirement')
          return Object.freeze([providerBindingNode(instance.key, expression)])
        if (expression._tag === 'BindingReference') {
          const initializer = bindings.get(expression.binding.ordinal)
          return initializer === undefined ? [] : executionTargets(initializer)
        }
        if (expression._tag === 'Move') return executionTargets(expression.subject)
        if (expression._tag === 'UnionConvert') return executionTargets(expression.source)
        if (expression._tag === 'Match') {
          return Object.freeze([
            ...new Set(
              expression.arms.flatMap((arm) => (arm.reachable ? executionTargets(arm.result) : [])),
            ),
          ])
        }
        if (
          (expression._tag === 'BoundOperationCall' || expression._tag === 'BuiltinCall') &&
          expression.witnessEffectSite !== undefined
        ) {
          const identity = effectIdentity(instance.key, expression.witnessEffectSite)
          const execution = effectNode(identity)
          effectIdentities.add(identity)
          const targetKey = selectedInterfaceEffectTarget(expression)
          if (targetKey !== undefined) addDependency(execution, executionNodeForKey(targetKey))
          return Object.freeze([execution])
        }
        if (expression._tag === 'EffectConstruct') {
          const target = targetKeyOfCall(expression, context)
          const targetFn =
            target === undefined ? undefined : targetFunction(results, target.declaration)
          const identity =
            target === undefined || targetFn === undefined
              ? undefined
              : resultEffectIdentity(targetFn, target, results, index)
          if (identity !== undefined) {
            return Object.freeze([effectNode(identity)])
          }
          if (target === undefined) {
            return []
          }
          return Object.freeze([instanceNode(target)])
        }
        if (expression._tag === 'ServiceEffectConstruct') {
          const service = Type.substitute(expression.service, instance.substitution)
          if (!Type.isNominal(service)) return []
          const node = serviceCallNode(instance.key, expression)
          serviceCalls.set(
            node,
            Object.freeze({
              service,
              role: expression.role,
              access: expression.access,
              operation: expression.operation,
              expression,
              context,
            }),
          )
          return Object.freeze([node])
        }
        return Object.freeze(effectOrigins(expression).map(effectNode))
      }

      const isSuspensionSubject = (expression: Hir.Expression): boolean => {
        if (expression._tag === 'BuiltinCall') return expression.operation === 'EffectSuspend'
        if (expression._tag === 'BindingReference') {
          const initializer = bindings.get(expression.binding.ordinal)
          return initializer !== undefined && isSuspensionSubject(initializer)
        }
        if (expression._tag === 'Move') return isSuspensionSubject(expression.subject)
        if (expression._tag === 'UnionConvert') return isSuspensionSubject(expression.source)
        if (expression._tag === 'EffectBindRequirement')
          return isSuspensionSubject(expression.protected)
        return false
      }

      const isExternalParkSubject = (expression: Hir.Expression): boolean => {
        if (expression._tag === 'BuiltinCall' && expression.operation === 'ExecutionPark')
          return true
        if (expression._tag === 'BindingReference') {
          const initializer = bindings.get(expression.binding.ordinal)
          return initializer !== undefined && isExternalParkSubject(initializer)
        }
        if (expression._tag === 'Move') return isExternalParkSubject(expression.subject)
        if (expression._tag === 'UnionConvert') return isExternalParkSubject(expression.source)
        return false
      }

      const recordForwardedServiceTargets = (
        expression: Extract<Hir.Expression, { readonly _tag: 'Call' | 'EffectConstruct' }>,
        execution: string,
      ): void => {
        const wrapperKey = targetKeyOfCall(expression, context)
        const wrapper =
          wrapperKey === undefined ? undefined : targetFunction(results, wrapperKey.declaration)
        const wrapperSubstitution =
          wrapper === undefined || wrapperKey === undefined
            ? undefined
            : instanceSubstitution(wrapper, wrapperKey)
        if (wrapper === undefined || wrapperSubstitution === undefined) return
        for (const ordinal of effectParameterOrdinals(wrapper, wrapperSubstitution)) {
          const argument = expression.arguments.at(ordinal)
          for (const service of argument === undefined
            ? []
            : serviceEffectRecipes(argument, context)) {
            const target = forwardedServiceTargetOfCall(
              expression,
              wrapper,
              wrapperSubstitution,
              ordinal,
              context,
              service,
            )
            if (target === undefined) continue
            const span = service.expression.span
            providedTargets.set(
              `${keyText(instance.key)}\0${keyText(target)}\0${span.sourceId}:${span.start}:${span.end}`,
              Object.freeze({ owner: instance.key, target, span }),
            )
            addDependency(execution, executionNodeForKey(target))
          }
        }
      }

      const recordForwardedCallableServiceTargets = (
        expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
        execution: string,
      ): void => {
        const wrapperKey = targetKeyOfCallableApply(expression, context)
        const wrapper =
          wrapperKey === undefined ? undefined : targetFunction(results, wrapperKey.declaration)
        const wrapperSubstitution =
          wrapper === undefined || wrapperKey === undefined
            ? undefined
            : instanceSubstitution(wrapper, wrapperKey)
        if (wrapper === undefined || wrapperSubstitution === undefined) return
        for (const ordinal of effectParameterOrdinals(wrapper, wrapperSubstitution)) {
          const argument = callableApplicationArgument(expression, ordinal, context)
          for (const service of argument === undefined
            ? []
            : serviceEffectRecipes(argument, context)) {
            const target = forwardedServiceTargetOfCallableApply(
              expression,
              wrapper,
              wrapperSubstitution,
              ordinal,
              context,
              service,
            )
            if (target === undefined) continue
            const span = service.expression.span
            providedTargets.set(
              `${keyText(instance.key)}\0${keyText(target)}\0${span.sourceId}:${span.start}:${span.end}`,
              Object.freeze({ owner: instance.key, target, span }),
            )
            addDependency(execution, executionNodeForKey(target))
          }
        }
      }

      const scanExpression = (expression: Hir.Expression, execution: string): void => {
        if (expression._tag === 'EffectBlock') {
          const identity = effectIdentity(instance.key, expression.site)
          effectIdentities.add(identity)
          scanStatements(expression.statements, effectNode(identity))
          return
        }
        if (expression._tag === 'EffectCatch') {
          const site = Hir.effectCatchSite(
            instance.function.declaration.id,
            instance.key.declaration,
            expression.span,
          )
          const identity = effectIdentity(instance.key, site)
          const catchExecution = effectNode(identity)
          effectIdentities.add(identity)
          for (const target of executionTargets(expression.protected))
            addDependency(catchExecution, target)
          const handler = callableOriginOf(expression.handler, context)
          if (handler?.target._tag === 'Declaration') {
            const declaration: DeclarationFacts.CanonicalId = Object.freeze({
              _tag: 'CanonicalDeclarationId',
              module: handler.target.module,
              name: handler.target.name,
            })
            const target = targetFunction(results, declaration)
            const targetKey =
              target === undefined
                ? undefined
                : keyOf(
                    declaration,
                    target.contract,
                    target.declaration.typeParameters.map((parameter) => parameter.type),
                    handler.typeArguments,
                  )
            const handlerEffect =
              target === undefined || targetKey === undefined
                ? undefined
                : resultEffectIdentity(target, targetKey, results, index)
            if (handlerEffect !== undefined)
              addDependency(catchExecution, effectNode(handlerEffect))
            else if (targetKey !== undefined) addDependency(catchExecution, instanceNode(targetKey))
          }
          scanExpression(expression.protected, catchExecution)
          scanExpression(expression.handler, catchExecution)
          return
        }
        if (expression._tag === 'EffectBindRequirement') {
          const bindingExecution =
            instance.resultEffect === undefined ? execution : effectNode(instance.resultEffect)
          const protectedTargets = executionTargets(expression.protected)
          const node = providerBindingNode(instance.key, expression)
          addDependency(bindingExecution, node)
          for (const target of protectedTargets) addDependency(node, target)
          const selected = selectedRequirement(expression, instance.substitution)
          const witness =
            requirementBindingWitness(expression, instance.substitution, index) ??
            expression.provider.witness
          if (
            selected !== undefined &&
            Type.isNominal(selected.capability) &&
            witness !== undefined
          )
            providerBindings.set(
              node,
              Object.freeze({
                node,
                execution: bindingExecution,
                protectedTargets,
                selected,
                witness,
                providerAccess: expression.provider.selectionAccess,
              }),
            )
        } else if (expression._tag === 'Call') {
          const target = targetKeyOfCall(expression, context)
          if (target !== undefined) addDependency(execution, instanceNode(target))
          recordForwardedServiceTargets(expression, execution)
        } else if (expression._tag === 'EffectConstruct') {
          const target = targetKeyOfCall(expression, context)
          if (target !== undefined) addDependency(execution, instanceNode(target))
          recordForwardedServiceTargets(expression, execution)
        } else if (
          (expression._tag === 'BoundOperationCall' || expression._tag === 'BuiltinCall') &&
          expression.witnessEffectSite !== undefined
        ) {
          for (const target of executionTargets(expression)) addDependency(execution, target)
        } else if (expression._tag === 'ServiceEffectConstruct') {
          for (const target of executionTargets(expression)) addDependency(execution, target)
        } else if (expression._tag === 'CallableApply') {
          const target = targetKeyOfCallableApply(expression, context)
          if (target !== undefined) addDependency(execution, instanceNode(target))
          recordForwardedCallableServiceTargets(expression, execution)
        } else if (expression._tag === 'Run') {
          if (isSuspensionSubject(expression.subject)) {
            nestedRoots.add(execution)
          } else if (isExternalParkSubject(expression.subject)) {
            externalRoots.add(execution)
          } else {
            for (const target of executionTargets(expression.subject))
              addDependency(execution, target)
          }
        }
        for (const child of Hir.expressionChildren(expression)) scanExpression(child, execution)
      }
      const scanStatements = (
        statements: ReadonlyArray<Hir.Statement>,
        execution: string,
      ): void => {
        for (const statement of statements) {
          for (const expression of Hir.statementExpressions(statement))
            scanExpression(expression, execution)
        }
      }
      scanStatements(instance.function.statements, instanceNode(instance.key))
    }

    interface ProviderTraversal {
      readonly node: string
      readonly environment: ReadonlyArray<ProviderBinding>
    }
    const pendingProviders: Array<ProviderTraversal> = [...providerBindings.values()].map(
      (binding) => Object.freeze({ node: binding.node, environment: Object.freeze([]) }),
    )
    const visitedProviders = new Set<string>()
    const selectedEdges: Array<readonly [string, string]> = []
    while (pendingProviders.length > 0) {
      const current = pendingProviders.shift()
      if (current === undefined) continue
      const traversalKey = `${current.node}\0${current.environment
        .map((binding) => binding.node)
        .join('\x01')}`
      if (visitedProviders.has(traversalKey)) continue
      visitedProviders.add(traversalKey)
      const entered = providerBindings.get(current.node)
      const environment =
        entered === undefined ||
        current.environment.some((binding) => binding.node === entered.node)
          ? current.environment
          : Object.freeze([...current.environment, entered])
      const serviceCall = serviceCalls.get(current.node)
      if (serviceCall !== undefined) {
        const binding = environment.findLast(
          (candidate) =>
            Type.equals(serviceCall.service, candidate.selected.capability) &&
            serviceCall.role === candidate.selected.role &&
            (serviceCall.access === 'Shared' ||
              candidate.providerAccess === 'Exclusive' ||
              candidate.providerAccess === 'Take'),
        )
        if (binding?.witness._tag === 'SourceConformanceWitness') {
          const target = targetKeyOfServiceCall(
            serviceCall.expression,
            binding.witness,
            serviceCall.context,
          )
          if (target !== undefined) {
            providedTargets.set(
              `${keyText(serviceCall.context.owner)}\0${keyText(target)}\0${serviceCall.expression.span.sourceId}:${serviceCall.expression.span.start}:${serviceCall.expression.span.end}`,
              Object.freeze({
                owner: serviceCall.context.owner,
                target,
                span: serviceCall.expression.span,
              }),
            )
            const targetNode = executionNodeForKey(target)
            selectedEdges.push(Object.freeze([binding.execution, targetNode]))
            pendingProviders.push(Object.freeze({ node: targetNode, environment }))
          }
        }
      }
      for (const target of dependencies.get(current.node) ?? [])
        pendingProviders.push(Object.freeze({ node: target, environment }))
    }
    for (const [owner, target] of selectedEdges) addDependency(owner, target)

    return Object.freeze({
      roots: new Map<SuspensionMode.Mode, ReadonlySet<string>>([
        ['NestedTransfer', nestedRoots],
        ['ExternalPark', externalRoots],
      ]),
      dependencies,
      effectIdentities,
      permitted,
      unavailable,
      providedTargets: Object.freeze([...providedTargets.values()]),
    })
  }

  return Object.freeze({
    functionByKey,
    instanceNode,
    effectNode,
    hookCalls,
    bodyCallTargets,
    interfaceWitnessTargets,
    requirementBindingCallTargets,
    forwardedRequirementCallTargets,
    slotDropHookTargets,
    directCallInstances,
    callableCallTargets,
    forwardedRequirementTargets,
    resultCallableIdentity,
    resultEffectIdentity,
    effectSuccesses,
    concreteCallables,
    concreteEffects,
    suspensionGraph,
  })
}
