import * as DeclarationIndex from './DeclarationIndex.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as Ownership from './Ownership.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Specialization from './Specialization.js'
import * as TargetConstant from './TargetConstant.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/**
 * Lowering preserves source control as canonical acyclic regions. Repetition is represented by a
 * loop region plus lexical repeat/exit outcomes; backend-private CFGs are derived later.
 */

const i32: Extract<Mir.Type, { readonly _tag: 'i32' }> = Object.freeze({ _tag: 'i32' })
const usize: Extract<Mir.Type, { readonly _tag: 'usize' }> = Object.freeze({ _tag: 'usize' })
const bool: Extract<Mir.Type, { readonly _tag: 'bool' }> = Object.freeze({ _tag: 'bool' })
const character: Extract<Mir.Type, { readonly _tag: 'char' }> = Object.freeze({ _tag: 'char' })

const isOsOperation = (
  operation: Hir.BuiltinOperation,
): operation is Extract<Hir.BuiltinOperation, `Os${string}`> => operation.startsWith('Os')

const mirType = (
  type: Type.Type,
  substitution: Type.Substitution = new Map(),
): Mir.Type | undefined => {
  const specialized = Type.substitute(type, substitution)
  if (!Type.isConcrete(specialized)) return undefined
  return typeof specialized === 'string'
    ? Type.isBuiltin(specialized)
      ? Object.freeze({ _tag: specialized })
      : Type.isString(specialized)
        ? Object.freeze({ _tag: 'String', type: specialized })
        : Type.isNever(specialized)
          ? Object.freeze({ _tag: 'Bottom', type: specialized })
          : undefined
    : Type.isNominal(specialized)
      ? Object.freeze({ _tag: 'Nominal', type: specialized })
      : Type.isFixedArray(specialized)
        ? Object.freeze({ _tag: 'FixedArray', type: specialized })
        : Type.isSlice(specialized)
          ? Object.freeze({ _tag: 'Slice', type: specialized })
          : Type.isReference(specialized)
            ? Object.freeze({ _tag: 'Reference', type: specialized })
            : Type.isUnion(specialized)
              ? Object.freeze({ _tag: 'Union', type: specialized })
              : Type.isEffect(specialized)
                ? Object.freeze({ _tag: 'EffectOutcome', type: specialized })
                : undefined
}

const local = (ordinal: number): Mir.LocalId => Object.freeze({ _tag: 'Local', ordinal })

const spanKey = (span: SourceSpan.SourceSpan): string => `${span.start}:${span.end}`
const patternKey = (binding: Match.BindingId): string =>
  `${spanKey(binding.arm.match.span)}:${binding.arm.ordinal}:${binding.ordinal}`
const borrowKey = (borrow: Hir.BorrowId): string =>
  `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`

interface ProvidedRequirement {
  readonly capability: Type.Nominal
  readonly providerType: Type.Nominal
  readonly witness: DeclarationIndex.ConformanceWitness
  readonly role: string
  readonly access: 'Shared' | 'Exclusive' | 'Take'
  readonly local?: Mir.LocalId
}

const specializeProvider = (
  fn: FunctionLowering,
  provider: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>['provider'],
): ProvidedRequirement | undefined => {
  const capability = fn.semantic(provider.capability)
  const providerType = fn.semantic(provider.providerType)
  if (!Type.isNominal(capability) || !Type.isNominal(providerType)) return undefined
  const witness = provider.witness ?? DeclarationIndex.witness(fn.index, providerType, capability)
  if (witness === undefined) return undefined
  return Object.freeze({
    capability,
    providerType,
    witness,
    role: provider.role,
    access: provider.access,
  })
}

class FunctionLowering {
  readonly regions: Array<Mir.Region | undefined> = []
  readonly localTypes: Array<Mir.Type> = []
  readonly bindingLocals = new Map<number, Mir.LocalId>()
  readonly parameterLocals = new Map<number, Mir.LocalId>()
  readonly effectRecipes = new Map<number, Hir.Expression>()
  readonly effectLoanEnds = new Map<number, ReadonlyArray<Hir.BorrowId>>()
  readonly patternLocals = new Map<string, Mir.LocalId>()
  readonly loanLocals = new Map<string, Mir.LocalId>()
  readonly slotLoans = new Map<number, ReadonlyArray<Hir.BorrowId>>()
  readonly callableDefinitions = new Map<
    number,
    Extract<Mir.Operation, { readonly _tag: 'MakeCallable' }>
  >()
  private operations: Array<Mir.Operation> = []

  constructor(
    readonly layout: Layout.Plan,
    readonly index: DeclarationIndex.Index,
    parameterTypes: ReadonlyArray<Mir.Type>,
    readonly ownership: Ownership.FunctionOwnership | undefined,
    readonly substitution: Type.Substitution,
    readonly effectOutcome: Type.Effect | undefined,
    readonly owner: Instances.Instance,
    readonly instances: ReadonlyArray<Instances.Instance>,
    readonly calls: ReadonlyArray<Instances.CallInstance>,
    readonly effectResults: ReadonlyMap<
      string,
      Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
    >,
    readonly generatedRunners: Array<GeneratedEffectRunner>,
    readonly providedRequirements: ReadonlyArray<ProvidedRequirement> = Object.freeze([]),
  ) {
    this.localTypes.push(...parameterTypes)
    parameterTypes.forEach((_, ordinal) => {
      this.parameterLocals.set(ordinal, local(ordinal))
    })
  }

  reserve(): Mir.RegionId {
    const id = Object.freeze({ _tag: 'Region' as const, ordinal: this.regions.length })
    this.regions.push(undefined)
    return id
  }

  publish(region: Mir.Region): void {
    this.regions[region.id.ordinal] = region
  }

  capture<A>(body: () => A): readonly [A, ReadonlyArray<Mir.Operation>] {
    const previous = this.operations
    this.operations = []
    const result = body()
    const operations = Object.freeze([...this.operations])
    this.operations = previous
    return [result, operations]
  }

  alloc(type: Mir.Type): Mir.LocalId {
    const id = local(this.localTypes.length)
    this.localTypes.push(type)
    return id
  }

  emit(operation: Mir.Operation): void {
    this.operations.push(operation)
    if (operation._tag === 'MakeCallable')
      this.callableDefinitions.set(operation.destination.ordinal, operation)
    if (operation._tag === 'Move') {
      const definition = this.callableDefinitions.get(operation.source.ordinal)
      if (definition !== undefined)
        this.callableDefinitions.set(operation.destination.ordinal, definition)
    }
  }

  type(type: Type.Type): Mir.Type | undefined {
    return mirType(type, this.substitution)
  }

  semantic(type: Type.Type): Type.Type {
    return Type.substitute(type, this.substitution)
  }

  semanticArgument(argument: Type.GenericArgument): Type.GenericArgument {
    return Type.substituteGenericArgument(argument, this.substitution)
  }

  call(span: SourceSpan.SourceSpan): Instances.CallInstance | undefined {
    const exact = this.calls.find(
      (call) =>
        Instances.keyText(call.owner) === Instances.keyText(this.owner.key) &&
        call.span.sourceId === span.sourceId &&
        call.span.start === span.start &&
        call.span.end === span.end,
    )
    if (exact !== undefined || this.providedRequirements.length === 0) return exact
    // A provided generated runner reuses the base Effect body's HIR and call sites but has a
    // private synthesized InstanceKey that discovery never owns. Resolve its calls through the
    // source owner whose body is being specialized; provider dispatch is represented separately.
    const sameSite = this.calls.filter(
      (call) =>
        call.span.sourceId === span.sourceId &&
        call.span.start === span.start &&
        call.span.end === span.end,
    )
    return sameSite.length === 1 ? sameSite.at(0) : undefined
  }
}

const forwardedRequirementBinding = (
  instance: Instances.Instance,
): Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }> | undefined => {
  const returned = instance.function.statements.at(-1)
  if (instance.function.statements.length !== 1 || returned?._tag !== 'Return') return undefined
  const block = returned.expression
  if (block._tag !== 'EffectBlock' || block.statements.length !== 2) return undefined
  const binding = block.statements.at(0)
  const completed = block.statements.at(1)
  if (
    binding?._tag !== 'Bind' ||
    binding.initializer._tag !== 'EffectBindRequirement' ||
    binding.initializer.provider.access !== 'Exclusive' ||
    binding.initializer.protected._tag !== 'Move' ||
    binding.initializer.protected.subject._tag !== 'ParameterReference' ||
    binding.initializer.protected.subject.parameter.ordinal !== 0 ||
    binding.initializer.provider.parameter?.ordinal !== 1 ||
    completed?._tag !== 'Return' ||
    completed.expression._tag !== 'Run' ||
    completed.expression.subject._tag !== 'BindingReference' ||
    completed.expression.subject.binding.ordinal !== binding.binding.ordinal
  )
    return undefined
  return binding.initializer
}

const callableApplicationArgument = (
  expression: Extract<Hir.Expression, { readonly _tag: 'CallableApply' }>,
  ordinal: number,
): Hir.Expression | undefined => {
  const section = expression.callee._tag === 'CallableSection' ? expression.callee : undefined
  if (section === undefined) return expression.arguments.at(ordinal)
  const captured = section.captures.find((capture) => capture.parameterOrdinal === ordinal)
  if (captured !== undefined) return captured.value
  return ordinal === section.omittedParameter ? expression.arguments.at(0) : undefined
}

const inlineForwardedRequirement = (
  fn: FunctionLowering,
  expression: Hir.Expression,
):
  | {
      readonly binding: Extract<Hir.Expression, { readonly _tag: 'EffectBindRequirement' }>
      readonly provider: Extract<Hir.Expression, { readonly _tag: 'ValueBorrow' }>
    }
  | undefined => {
  if (expression._tag !== 'CallableApply' && expression._tag !== 'EffectConstruct') return undefined
  const call = fn.call(expression.span)
  const declaration =
    expression._tag === 'EffectConstruct'
      ? expression.target
      : expression.callee._tag === 'CallableSection' &&
          expression.callee.target._tag === 'DeclarationCallableTarget'
        ? expression.callee.target.declaration
        : undefined
  const candidates =
    declaration === undefined
      ? []
      : fn.instances.filter(
          (instance) =>
            instance.key.declaration.module === declaration.module &&
            instance.key.declaration.name === declaration.name,
        )
  const inferredArguments = (() => {
    if (expression._tag === 'EffectConstruct')
      return expression.typeArguments.map((argument) => fn.semanticArgument(argument))
    if (expression.callee._tag !== 'CallableSection') return undefined
    const candidate = candidates.at(0)
    if (candidate === undefined) return undefined
    const inferred = new Map([...expression.callee.substitution, ...expression.substitution])
    const arguments_ = candidate.function.declaration.typeParameters.flatMap((parameter) => {
      const argument = inferred.get(Type.key(parameter.type))
      return argument === undefined ? [] : [fn.semanticArgument(argument)]
    })
    return arguments_.length === candidate.function.declaration.typeParameters.length
      ? arguments_
      : undefined
  })()
  const target =
    call !== undefined
      ? fn.instances.find(
          (instance) => Instances.keyText(instance.key) === Instances.keyText(call.target),
        )
      : inferredArguments === undefined
        ? undefined
        : (candidates.find((candidate) => {
            const explicit = candidate.key.typeArguments.filter(
              (argument) => !Type.isHiddenIdentityArgument(argument),
            )
            return (
              explicit.length === inferredArguments.length &&
              explicit.every(
                (argument, ordinal) =>
                  Type.genericArgumentKey(argument) ===
                  Type.genericArgumentKey(inferredArguments.at(ordinal) ?? argument),
              )
            )
          }) ?? (candidates.length === 1 ? candidates.at(0) : undefined))
  const forwarded = target === undefined ? undefined : forwardedRequirementBinding(target)
  if (target === undefined || forwarded === undefined) return undefined
  const protected_ =
    expression._tag === 'CallableApply'
      ? callableApplicationArgument(expression, 0)
      : expression.arguments.at(0)
  const provider =
    expression._tag === 'CallableApply'
      ? callableApplicationArgument(expression, 1)
      : expression.arguments.at(1)
  if (protected_ === undefined || provider?._tag !== 'ValueBorrow') return undefined
  if (provider.root._tag === 'PatternSliceRoot') return undefined
  const capability = Type.substitute(forwarded.provider.capability, target.substitution)
  const providerType = Type.substitute(forwarded.provider.providerType, target.substitution)
  const type = fn.semantic(expression.type)
  if (!Type.isNominal(capability) || !Type.isNominal(providerType) || !Type.isEffect(type))
    return undefined
  const witness = DeclarationIndex.witness(fn.index, providerType, capability)
  if (witness === undefined) return undefined
  return Object.freeze({
    binding: Object.freeze({
      _tag: 'EffectBindRequirement',
      protected: protected_,
      provider: Object.freeze({
        ...(provider.root._tag === 'BindingSliceRoot'
          ? { binding: provider.root.binding }
          : { parameter: provider.root.parameter }),
        capability,
        providerType,
        witness,
        role: forwarded.provider.role,
        access: provider.access,
        span: provider.span,
      }),
      type,
      span: expression.span,
    }),
    provider,
  })
}

interface GeneratedEffectRunner {
  readonly id: DeclarationIndex.CanonicalId
  readonly owner: Instances.Instance
  readonly block: Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }>
  readonly type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly specializationKey: string
  readonly providedRequirements: ReadonlyArray<Omit<ProvidedRequirement, 'local'>>
}

const instanceText = (
  declaration: { readonly module: string; readonly name: string },
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): string => Specialization.key({ declaration, typeArguments })

const effectEntryAdapterId = (module: string): DeclarationIndex.CanonicalId =>
  Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module,
    name: '$effect-entry',
  })

const baseRunnerKey = (owner: Instances.InstanceKey, site: Hir.EffectSiteId): string =>
  `${instanceText(owner.declaration, owner.typeArguments)}\u0000${Hir.executableSiteKey(site)}`

const witnessKey = (witness: DeclarationIndex.ConformanceWitness): string =>
  witness._tag === 'SourceConformanceWitness'
    ? `${witness._tag}:${witness.operations
        .map(
          (operation) =>
            `${operation.name}=${operation.implementation.module}.${operation.implementation.name}`,
        )
        .join(',')}`
    : `${witness._tag}:${Type.key(witness.provider)}`

const providedRunnerKey = (
  type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  requirements: ReadonlyArray<ProvidedRequirement>,
): string =>
  `${baseRunnerKey(type.environment.instance, type.site)}\u0000${requirements
    .map(
      (requirement) =>
        `${Type.key(requirement.capability)}@${requirement.role}:${requirement.access}:${Type.key(requirement.providerType)}:${witnessKey(requirement.witness)}`,
    )
    .join('\u0000')}`

const effectValueType = (
  layout: Layout.Plan,
  instance: Instances.InstanceKey,
  block: Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }>,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  const environment = layout.effectEnvironments.find(
    (candidate) =>
      candidate._tag === 'EffectEnvironment' &&
      instanceText(candidate.instance.declaration, candidate.instance.typeArguments) ===
        instanceText(instance.declaration, instance.typeArguments) &&
      Hir.sameExecutableSite(candidate.site, block.site),
  )
  if (environment?._tag !== 'EffectEnvironment') return undefined
  return Object.freeze({
    _tag: 'EffectValue',
    type: environment.effect,
    site: block.site,
    environment,
  })
}

const effectValueByIdentity = (
  layout: Layout.Plan,
  identity: string,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  const environment = layout.effectEnvironments.find(
    (
      candidate,
    ): candidate is Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
      candidate._tag === 'EffectEnvironment' &&
      Instances.effectIdentity(candidate.instance, candidate.site) === identity,
  )
  return environment === undefined
    ? undefined
    : Object.freeze({
        _tag: 'EffectValue',
        type: environment.effect,
        site: environment.site,
        environment,
      })
}

const callableValueByIdentity = (
  layout: Layout.Plan,
  identity: Type.CallableIdentityArgument,
  type: Type.Callable,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const target: Hir.CallableTarget =
    identity.target._tag === 'Declaration'
      ? Object.freeze({
          _tag: 'DeclarationCallableTarget',
          declaration: Object.freeze({
            _tag: 'CanonicalDeclarationId',
            module: identity.target.module,
            name: identity.target.name,
          }),
        })
      : Object.freeze({
          _tag: 'BuiltinCallableTarget',
          actor: identity.target.actor,
          operation: identity.target.operation as Hir.BuiltinOperation,
          intrinsic: Object.freeze({
            _tag: 'IntrinsicOperationId',
            actor: identity.target.intrinsic.actor,
            name: identity.target.intrinsic.name,
          }),
        })
  const environment =
    identity.environment === undefined
      ? undefined
      : layout.callableEnvironments.find(
          (
            candidate,
          ): candidate is Extract<
            Layout.CallableEnvironment,
            { readonly _tag: 'CallableEnvironment' }
          > =>
            candidate._tag === 'CallableEnvironment' &&
            Instances.callableIdentity(candidate.callable) === identity.environment,
        )
  if (identity.environment !== undefined && environment === undefined) return undefined
  const specializedType =
    environment === undefined
      ? Object.freeze({ ...type, mode: 'Shared' as const })
      : Object.freeze({ ...environment.callable.type, mode: environment.callable.mode })
  return Object.freeze({
    _tag: 'CallableValue',
    type: specializedType,
    target,
    ...(environment === undefined ? {} : { site: environment.callable.site, environment }),
  })
}

const requirementsFor = (
  available: ReadonlyArray<ProvidedRequirement>,
  effect: Type.Effect,
): ReadonlyArray<ProvidedRequirement> | undefined => {
  const selected = effect.requirements.map((requirement) =>
    available.find(
      (candidate) =>
        candidate.role === requirement.role &&
        Type.equals(candidate.capability, requirement.capability) &&
        (requirement.access === 'Shared' || candidate.access === 'Exclusive'),
    ),
  )
  return selected.every((candidate) => candidate !== undefined)
    ? Object.freeze(selected.flatMap((candidate) => (candidate === undefined ? [] : [candidate])))
    : undefined
}

const ensureProvidedRunner = (
  fn: FunctionLowering,
  type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  requirements: ReadonlyArray<ProvidedRequirement>,
): DeclarationIndex.CanonicalId | undefined => {
  const key = providedRunnerKey(type, requirements)
  const existing = fn.generatedRunners.find((candidate) => candidate.specializationKey === key)
  if (existing !== undefined) return existing.id
  const baseKey = baseRunnerKey(type.environment.instance, type.site)
  const base = fn.generatedRunners.find((candidate) => candidate.specializationKey === baseKey)
  if (base === undefined) return undefined
  const id: DeclarationIndex.CanonicalId = Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: base.id.module,
    name: `${base.id.name}$provided$${fn.generatedRunners.length}`,
  })
  fn.generatedRunners.push(
    Object.freeze({
      id,
      owner: base.owner,
      block: base.block,
      type: base.type,
      specializationKey: key,
      providedRequirements: Object.freeze(
        requirements.map(({ local: _local, ...requirement }) => Object.freeze(requirement)),
      ),
    }),
  )
  return id
}

const runtimeRequirementArguments = (
  requirements: ReadonlyArray<ProvidedRequirement> | undefined,
): ReadonlyArray<Mir.LocalId> =>
  Object.freeze(
    requirements?.flatMap((requirement) =>
      requirement.witness._tag !== 'SourceConformanceWitness' || requirement.local === undefined
        ? []
        : [requirement.local],
    ) ?? [],
  )

const sameSite = (left: Hir.CallableSiteId, right: Hir.CallableSiteId): boolean =>
  Hir.sameExecutableSite(left, right)

const callableValueType = (
  fn: FunctionLowering,
  section: Extract<Hir.Expression, { readonly _tag: 'CallableSection' }>,
  applicationSubstitution: Type.Substitution = new Map(),
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const expected = Type.substitute(
    Type.substitute(section.type, fn.substitution),
    new Map([...section.substitution, ...applicationSubstitution]),
  )
  const candidates = fn.layout.callableEnvironments.filter(
    (
      candidate,
    ): candidate is Extract<Layout.CallableEnvironment, { readonly _tag: 'CallableEnvironment' }> =>
      candidate._tag === 'CallableEnvironment' &&
      instanceText(candidate.callable.owner.declaration, candidate.callable.owner.typeArguments) ===
        instanceText(fn.owner.key.declaration, fn.owner.key.typeArguments) &&
      sameSite(candidate.callable.site, section.site) &&
      (!Type.isConcrete(expected) || Type.equals(candidate.callable.type, expected)),
  )
  const environment = candidates.length === 1 ? candidates.at(0) : undefined
  if (environment === undefined) return undefined
  return Object.freeze({
    _tag: 'CallableValue',
    type: environment.callable.type,
    target: environment.callable.target,
    site: section.site,
    environment,
  })
}

const directCallableSectionValueType = (
  fn: FunctionLowering,
  section: Extract<Hir.Expression, { readonly _tag: 'CallableSection' }>,
  applicationSubstitution: Type.Substitution,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const type = Type.substitute(
    Type.substitute(section.type, fn.substitution),
    new Map([...section.substitution, ...applicationSubstitution]),
  )
  return Type.isCallable(type) && Type.isConcrete(type)
    ? Object.freeze({ _tag: 'CallableValue', type, target: section.target })
    : undefined
}

const functionItemValueType = (
  fn: FunctionLowering,
  item: Extract<Hir.Expression, { readonly _tag: 'FunctionItem' }>,
  applicationSubstitution: Type.Substitution = new Map(),
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const type = Type.substitute(Type.substitute(item.type, fn.substitution), applicationSubstitution)
  return Type.isCallable(type) && Type.isConcrete(type)
    ? Object.freeze({ _tag: 'CallableValue', type, target: item.target })
    : undefined
}

interface LoweredExpression {
  readonly result: Mir.LocalId
}

interface LoweredPlace {
  readonly root: Mir.LocalId
  readonly selectors: ReadonlyArray<Mir.PlaceSelector>
}

const lowerRunEffectValue = (
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
    effectType.type.failures.length === 0 || fn.effectOutcome === undefined
      ? undefined
      : fn.type(fn.effectOutcome)
  if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome') return undefined
  const tagMappings = effectType.type.failures.flatMap((failure, sourceOrdinal) => {
    const target = propagationType?.type.failures.findIndex((candidate) =>
      Type.equals(candidate, failure),
    )
    return target === undefined || target < 0
      ? []
      : [Object.freeze({ source: sourceOrdinal + 1, target: target + 1 })]
  })
  if (tagMappings.length !== effectType.type.failures.length) return undefined
  const propagationShape =
    propagationType === undefined ? undefined : Layout.callingShape(fn.layout, propagationType.type)
  const provided = requirementsFor(availableRequirements, effectType.type)
  const providedRunner =
    provided === undefined || provided.length === 0
      ? undefined
      : ensureProvidedRunner(fn, effectType, provided)
  if (provided !== undefined && provided.length > 0 && providedRunner === undefined)
    return undefined
  fn.emit(
    Object.freeze({
      _tag: 'RunEffectValue',
      destination,
      outcome,
      effect,
      runner:
        providedRunner ??
        Hir.effectRunnerId(effectType.environment.instance.declaration, effectType.site),
      runnerTypeArguments: effectType.environment.instance.typeArguments,
      arguments: runtimeRequirementArguments(provided),
      outcomeType,
      ...(propagationType === undefined ? {} : { propagationType }),
      tagMappings: Object.freeze(tagMappings),
      propagationLaneCount: propagationShape?.laneCount ?? 0,
      ...(propagationType === undefined || propagationReleases(fn, span).length === 0
        ? {}
        : { releases: propagationReleases(fn, span) }),
      type: successType,
      provenance: authored(span),
    }),
  )
  return Object.freeze({ result: destination })
}

const lowerPlacePath = (
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

const lowerPlace = (
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
      provenance: Object.freeze({ span: expression.span, generated: false }),
    }),
  )
  return Object.freeze({ result: destination })
}

const endLoans = (
  fn: FunctionLowering,
  loans: ReadonlyArray<Hir.BorrowId>,
  span: SourceSpan.SourceSpan,
): void => {
  for (const borrow of loans) {
    const held = fn.loanLocals.get(borrowKey(borrow))
    if (held === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'EndLoan',
        borrow,
        slice: held,
        provenance: generated(span),
      }),
    )
    fn.loanLocals.delete(borrowKey(borrow))
  }
}

const endRunLoans = (fn: FunctionLowering, span: SourceSpan.SourceSpan): void => {
  // A constructed effect holds its argument borrows until the run that consumes it: ownership
  // records that end site, so every lowering path for run must release them here.
  for (const loan of fn.ownership?.loans ?? []) {
    if (loan.origin !== 'EffectCapture' && loan.origin !== 'ValueBorrow') continue
    if (loan.endSpan.sourceId !== span.sourceId || loan.endSpan.end > span.end) {
      continue
    }
    endLoans(fn, [loan.id], span)
  }
}

const endReturnedViewLoans = (fn: FunctionLowering, span: SourceSpan.SourceSpan): void => {
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

const retainedEffectLoans = (
  fn: FunctionLowering,
  expression: Hir.Expression,
): ReadonlyArray<Hir.BorrowId> => {
  const retained = new Map<string, Hir.BorrowId>()
  for (const child of Hir.expressionTree(expression)) {
    if (child._tag === 'BindingReference') {
      for (const borrow of fn.effectLoanEnds.get(child.binding.ordinal) ?? [])
        retained.set(borrowKey(borrow), borrow)
    }
    if (child._tag !== 'CallableSection') continue
    for (const capture of child.captures) {
      if (capture.value._tag !== 'SliceBorrow' && capture.value._tag !== 'ValueBorrow') continue
      retained.set(borrowKey(capture.value.borrow), capture.value.borrow)
    }
  }
  return Object.freeze([...retained.values()])
}

const borrowedWriteRoot = (
  fn: FunctionLowering,
  root: Hir.BorrowedWritePlace['root'],
): Mir.LocalId | undefined =>
  root._tag === 'ParameterSliceRoot'
    ? fn.parameterLocals.get(root.parameter.ordinal)
    : fn.bindingLocals.get(root.binding.ordinal)

const lowerServiceEffectValue = (
  fn: FunctionLowering,
  subject: Extract<Hir.Expression, { readonly _tag: 'ServiceEffectConstruct' }>,
  availableRequirements: ReadonlyArray<ProvidedRequirement>,
): LoweredExpression | undefined => {
  const provided = availableRequirements.find(
    (requirement) =>
      requirement.role === subject.role &&
      Type.equals(requirement.capability, subject.service) &&
      (subject.access === 'Shared' || requirement.access === 'Exclusive'),
  )
  if (provided?.witness._tag !== 'SourceConformanceWitness' || provided.local === undefined)
    return undefined
  const target = DeclarationIndex.witnessOperation(provided.witness, subject.operation)
  if (target === undefined) return undefined
  const loweredArguments = subject.arguments.map((argument) => lowerExpression(fn, argument))
  if (loweredArguments.some((argument) => argument === undefined)) return undefined
  const typeArguments = Object.freeze<ReadonlyArray<Type.GenericArgument>>([])
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

const lowerEffectExecution = (
  fn: FunctionLowering,
  subject: Hir.Expression,
  success: Type.Type,
  span: SourceSpan.SourceSpan,
  availableRequirements: ReadonlyArray<ProvidedRequirement> = fn.providedRequirements,
): LoweredExpression | undefined => {
  if (subject._tag === 'BindingReference') {
    const storedRecipe = fn.effectRecipes.get(subject.binding.ordinal)
    if (storedRecipe !== undefined)
      return lowerEffectExecution(fn, storedRecipe, success, span, availableRequirements)
  }

  const forwarded = inlineForwardedRequirement(fn, subject)
  if (forwarded !== undefined) {
    const selectedProvider = specializeProvider(fn, forwarded.binding.provider)
    const provider = lowerExpression(fn, forwarded.provider)
    if (selectedProvider === undefined || provider === undefined) return undefined
    const result = lowerEffectExecution(
      fn,
      forwarded.binding.protected,
      success,
      span,
      Object.freeze([
        ...availableRequirements,
        Object.freeze({ ...selectedProvider, local: provider.result }),
      ]),
    )
    if (result === undefined) return undefined
    endRunLoans(fn, span)
    endLoans(fn, [forwarded.provider.borrow], span)
    if (
      forwarded.binding.protected._tag === 'EffectConstruct' ||
      forwarded.binding.protected._tag === 'ServiceEffectConstruct'
    )
      endLoans(fn, forwarded.binding.protected.loanEnds, span)
    return result
  }

  if (subject._tag === 'EffectBindRequirement') {
    if (subject.provider.access === 'Take') return undefined
    const selectedProvider = specializeProvider(fn, subject.provider)
    if (selectedProvider === undefined) return undefined
    let providerLoan: { readonly borrow: Hir.BorrowId; readonly slice: Mir.LocalId } | undefined
    let provided: ProvidedRequirement = selectedProvider
    if (selectedProvider.witness._tag === 'SourceConformanceWitness') {
      const provider =
        subject.provider.binding !== undefined
          ? fn.bindingLocals.get(subject.provider.binding.ordinal)
          : subject.provider.parameter !== undefined
            ? fn.parameterLocals.get(subject.provider.parameter.ordinal)
            : undefined
      const forwardedReference =
        subject.provider.parameter === undefined || provider === undefined
          ? undefined
          : fn.localTypes.at(provider.ordinal)
      const providerType = fn.type(selectedProvider.providerType)
      const referenceType = fn.type(
        Object.freeze({
          _tag: 'ReferenceType' as const,
          access: subject.provider.access,
          target: selectedProvider.providerType,
        }),
      )
      const loan = fn.ownership?.loans.find(
        (candidate) =>
          (candidate.origin === 'EffectCapture' ||
            candidate.origin === 'CallableCapture' ||
            candidate.origin === 'ValueBorrow') &&
          candidate.access === subject.provider.access &&
          candidate.startSpan.start === subject.provider.span.start &&
          candidate.startSpan.end === subject.provider.span.end,
      )
      if (
        provider !== undefined &&
        forwardedReference?._tag === 'Reference' &&
        forwardedReference.type.access === subject.provider.access &&
        Type.equals(forwardedReference.type.target, selectedProvider.providerType)
      ) {
        provided = Object.freeze({ ...selectedProvider, local: provider })
      } else {
        if (
          provider === undefined ||
          providerType?._tag !== 'Nominal' ||
          referenceType?._tag !== 'Reference' ||
          loan === undefined
        )
          return undefined
        const reference = fn.alloc(referenceType)
        fn.emit(
          Object.freeze({
            _tag: 'BeginLoan',
            borrow: loan.id,
            destination: reference,
            root: provider,
            selectors: Object.freeze([]),
            sourceType: providerType,
            type: referenceType,
            access: subject.provider.access,
            reborrow: false,
            suspendsParent: false,
            provenance: authored(subject.provider.span),
          }),
        )
        provided = Object.freeze({ ...selectedProvider, local: reference })
        providerLoan = Object.freeze({ borrow: loan.id, slice: reference })
      }
    }
    const result = lowerEffectExecution(
      fn,
      subject.protected,
      success,
      span,
      Object.freeze([...availableRequirements, provided]),
    )
    if (result === undefined) return undefined
    endRunLoans(fn, span)
    if (
      subject.protected._tag === 'EffectConstruct' ||
      subject.protected._tag === 'ServiceEffectConstruct'
    )
      endLoans(fn, subject.protected.loanEnds, span)
    if (providerLoan !== undefined) {
      fn.emit(
        Object.freeze({
          _tag: 'EndLoan',
          borrow: providerLoan.borrow,
          slice: providerLoan.slice,
          provenance: generated(subject.provider.span),
        }),
      )
    }
    return result
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
  if (lowered === undefined || loweredType?._tag !== 'EffectValue') return undefined
  return lowerRunEffectValue(fn, lowered.result, loweredType, success, span, availableRequirements)
}

/**
 * Redirects one bound operator to the provider's own function when specialization lands on a type
 * whose conformance maps the operation to source rather than to a sealed intrinsic.
 *
 * An interface operation never consumes its operands — a builtin operator does not, and a generic
 * body is checked with the element type non-`Copy` — so the witness observes each operand through a
 * shared borrow of the local the operand already lowered to. The borrow lives only across the call,
 * and its identity cannot collide with an argument borrow at the same span because a bound operand
 * has the bound's own type and is therefore never itself a borrow expression.
 */
const emitWitnessDispatch = (
  fn: FunctionLowering,
  target: DeclarationIndex.InterfaceWitnessTarget,
  argumentLocals: ReadonlyArray<Mir.LocalId>,
  operandTypes: ReadonlyArray<{ readonly source: Mir.Type; readonly reference: Mir.Type }>,
  resultType: Mir.Type,
  span: SourceSpan.SourceSpan,
): Mir.LocalId | undefined => {
  const borrows: Array<{ readonly borrow: Hir.BorrowId; readonly local: Mir.LocalId }> = []
  for (const [ordinal, argument] of argumentLocals.entries()) {
    const operand = operandTypes.at(ordinal)
    if (operand === undefined || operand.reference._tag !== 'Reference') return undefined
    const borrow = Object.freeze({
      _tag: 'BorrowId' as const,
      function: fn.owner.function.declaration.id,
      callSpan: span,
      ordinal,
    })
    const destination = fn.alloc(operand.reference)
    fn.emit(
      Object.freeze({
        _tag: 'BeginLoan',
        borrow,
        destination,
        root: argument,
        selectors: Object.freeze([]),
        sourceType: operand.source,
        type: operand.reference,
        access: 'Shared',
        reborrow: false,
        suspendsParent: false,
        provenance: generated(span),
      }),
    )
    borrows.push(Object.freeze({ borrow, local: destination }))
  }
  const destination = fn.alloc(resultType)
  fn.emit(
    Object.freeze({
      _tag: 'Call',
      destination,
      target: target.implementation,
      // A conditional witness is one generic function per header, so the direct target carries the
      // arguments this specialization proved. Nothing else travels: a requirement's own witness is
      // reached through its own instance, never through a value handed to this call.
      typeArguments: target.typeArguments,
      arguments: Object.freeze(borrows.map((entry) => entry.local)),
      type: resultType,
      provenance: generated(span),
    }),
  )
  for (const entry of borrows)
    fn.emit(
      Object.freeze({
        _tag: 'EndLoan',
        borrow: entry.borrow,
        slice: entry.local,
        provenance: generated(span),
      }),
    )
  return destination
}

const lowerInterfaceWitnessCall = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'BuiltinCall' }>,
  argumentLocals: ReadonlyArray<Mir.LocalId>,
): Mir.LocalId | undefined => {
  const bound = expression.interfaceOperation
  if (bound === undefined) return undefined
  const provider = fn.semantic(bound.provider)
  const capability = fn.semantic(bound.capability)
  if (!Type.isNominal(capability)) return undefined
  const target = DeclarationIndex.interfaceWitnessTarget(
    fn.index,
    provider,
    capability,
    bound.operation,
  )
  const providerType = fn.type(provider)
  const resultType = fn.type(expression.type)
  const referenceType = fn.type(Type.reference('Shared', provider))
  if (
    target === undefined ||
    providerType === undefined ||
    resultType === undefined ||
    referenceType?._tag !== 'Reference'
  )
    return undefined
  return emitWitnessDispatch(
    fn,
    target,
    argumentLocals,
    argumentLocals.map(() => Object.freeze({ source: providerType, reference: referenceType })),
    resultType,
    expression.span,
  )
}

/**
 * Redirects one bound operation call to the provider's own function, the fallback the operator path
 * reaches through `lowerInterfaceWitnessCall`.
 *
 * An operation no operator spells is reached only by naming the bound, and the witness it selects is
 * a source function exactly as often as an operator's is. Its operands are not all the bound's own
 * type — a contract may declare an operand of a fixed type the interface never parameterizes — so
 * each operand is borrowed at the type it lowered to rather than at the provider's.
 */
const lowerBoundWitnessCall = (
  fn: FunctionLowering,
  expression: Extract<Hir.Expression, { readonly _tag: 'BoundOperationCall' }>,
  provider: Type.Type,
  capability: Type.Nominal,
  argumentLocals: ReadonlyArray<Mir.LocalId>,
): Mir.LocalId | undefined => {
  const target = DeclarationIndex.interfaceWitnessTarget(
    fn.index,
    provider,
    capability,
    expression.operation,
  )
  const resultType = fn.type(expression.type)
  if (target === undefined || resultType === undefined) return undefined
  const operandTypes = argumentLocals.flatMap((argument) => {
    const source = fn.localTypes.at(argument.ordinal)
    const reference =
      source === undefined ? undefined : fn.type(Type.reference('Shared', Mir.semanticType(source)))
    return source === undefined || reference === undefined
      ? []
      : [Object.freeze({ source, reference })]
  })
  if (operandTypes.length !== argumentLocals.length) return undefined
  return emitWitnessDispatch(fn, target, argumentLocals, operandTypes, resultType, expression.span)
}

function lowerExpression(
  fn: FunctionLowering,
  expression: Hir.Expression,
): LoweredExpression | undefined {
  const lowered = lowerExpressionInner(fn, expression)
  endReturnedViewLoans(fn, expression.span)
  return lowered
}

function lowerExpressionInner(
  fn: FunctionLowering,
  expression: Hir.Expression,
): LoweredExpression | undefined {
  switch (expression._tag) {
    case 'IntegerLiteral': {
      const type = fn.type(expression.type)
      if (type === undefined || !Type.isBuiltin(Mir.semanticType(type))) return undefined
      const destination = fn.alloc(type)
      // Lowering is the first phase that holds the selected target, and every engine reads the MIR
      // it produces, so this is where a pointer-width fact becomes one exact number.
      const value =
        expression.targetConstant === undefined
          ? expression.value
          : TargetConstant.value(
              expression.targetConstant,
              TargetConstant.pointerBits(fn.layout.target),
            )
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type,
          value,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'FloatingLiteral': {
      const type = fn.type(expression.type)
      if (type?._tag !== expression.type) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type,
          value: expression.bits,
          provenance: authored(expression.span),
        }),
      )
      return { result: destination }
    }
    case 'StaticStringLiteral': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'String') return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'StaticString',
          destination,
          data: expression.data.id,
          byteLength: expression.data.bytes.length,
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'RuntimeStringView': {
      const source = lowerExpression(fn, expression.source)
      const sourceType = source === undefined ? undefined : fn.localTypes.at(source.result.ordinal)
      const type = fn.type(expression.type)
      if (
        source === undefined ||
        sourceType?._tag !== 'Slice' ||
        !Type.equals(sourceType.type, Type.slice('Shared', 'u8')) ||
        type?._tag !== 'String'
      )
        return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'StringFromUtf8Unchecked',
          destination,
          bytes: source.result,
          heldLoans: expression.heldLoans,
          authorization: 'Unsafe',
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'StringEquality': {
      const left = lowerExpression(fn, expression.left)
      const right = lowerExpression(fn, expression.right)
      const leftType = left === undefined ? undefined : fn.localTypes.at(left.result.ordinal)
      const rightType = right === undefined ? undefined : fn.localTypes.at(right.result.ordinal)
      if (
        left === undefined ||
        right === undefined ||
        leftType?._tag !== 'String' ||
        rightType?._tag !== 'String'
      )
        return undefined
      const destination = fn.alloc(bool)
      fn.emit(
        Object.freeze({
          _tag: 'StringEqualsExact',
          destination,
          left: left.result,
          right: right.result,
          negated: expression.negated,
          type: bool,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'StaticByteViewLiteral': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'Slice') return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'StaticView',
          destination,
          data: expression.data.id,
          length: expression.data.bytes.length,
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'UnitLiteral': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'Nominal' || !Type.equals(type.type, Type.unit)) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Construct',
          destination,
          type,
          fields: Object.freeze([]),
          provenance: authored(expression.span),
        }),
      )
      return { result: destination }
    }
    case 'BooleanLiteral': {
      const destination = fn.alloc(bool)
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type: bool,
          value: expression.value ? 1 : 0,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'CharacterLiteral': {
      const destination = fn.alloc(character)
      fn.emit(
        Object.freeze({
          _tag: 'Literal',
          destination,
          type: character,
          value: expression.value,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'ParameterReference': {
      const parameter = fn.parameterLocals.get(expression.parameter.ordinal)
      return parameter === undefined ? undefined : { result: parameter }
    }
    case 'BindingReference': {
      const bound = fn.bindingLocals.get(expression.binding.ordinal)
      if (bound === undefined) return undefined
      return { result: bound }
    }
    case 'PatternBindingReference': {
      const bound = fn.patternLocals.get(patternKey(expression.binding))
      if (bound === undefined) return undefined
      return { result: bound }
    }
    case 'Move':
      return lowerExpression(fn, expression.subject)
    case 'Replace': {
      // Swap one writable place: the old value reads out before the replacement commits, and
      // both halves ride the existing checked place operations.
      const place = expression.place
      const root =
        place._tag === 'BorrowedWritePlace'
          ? borrowedWriteRoot(fn, place.root)
          : fn.bindingLocals.get(place.root.ordinal)
      const rootType = root === undefined ? undefined : fn.localTypes.at(root.ordinal)
      const type = fn.type(place.type)
      if (root === undefined || rootType === undefined || type === undefined) return undefined
      const selectors =
        place._tag === 'BorrowedWritePlace'
          ? lowerBorrowedWriteSelectors(fn, place.selectors)
          : lowerWriteSelectors(fn, place.selectors)
      if (selectors === undefined) return undefined
      fn.emit(
        Object.freeze({
          _tag: 'CheckPlace',
          root,
          selectors,
          type,
          provenance: authored(place.span),
        }),
      )
      const value = lowerExpression(fn, expression.value)
      if (value === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'ReadPlace',
          destination,
          root,
          selectors,
          type,
          consume: true,
          provenance: authored(expression.span),
        }),
      )
      fn.emit(
        Object.freeze({
          _tag: 'WritePlace',
          root,
          selectors,
          source: value.result,
          rootType,
          type,
          mutable: true,
          replacement: 'Copy',
          commit: 'AfterCleanup',
          provenance: authored(expression.span),
        }),
      )
      return { result: destination }
    }
    case 'FunctionItem': {
      const type = functionItemValueType(fn, expression)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'MakeCallable',
          destination,
          target: expression.target,
          typeArguments: Object.freeze([]),
          captures: Object.freeze([]),
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'CallableSection': {
      const type = callableValueType(fn, expression)
      if (type === undefined || type.environment === undefined) return undefined
      const captures: Array<{
        readonly ordinal: number
        readonly parameterOrdinal: number
        readonly source: Mir.LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }> = []
      for (const capture of expression.captures) {
        const lowered = lowerExpression(fn, capture.value)
        if (lowered === undefined) return undefined
        captures.push(
          Object.freeze({
            ordinal: capture.ordinal,
            parameterOrdinal: capture.parameterOrdinal,
            source: lowered.result,
            access: capture.access,
          }),
        )
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'MakeCallable',
          destination,
          target: expression.target,
          typeArguments: type.environment.callable.typeArguments,
          captures: Object.freeze(captures),
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'CallableApply': {
      const directSection =
        expression.realization === 'DirectErasedSection' &&
        expression.callee._tag === 'CallableSection'
          ? expression.callee
          : undefined
      const directItem = expression.callee._tag === 'FunctionItem' ? expression.callee : undefined
      const call = fn.call(expression.span)
      const directType =
        directSection !== undefined
          ? directCallableSectionValueType(fn, directSection, expression.substitution)
          : directItem !== undefined
            ? functionItemValueType(fn, directItem, expression.substitution)
            : undefined
      const arguments_: Array<Mir.LocalId> = []
      const captures: Array<{
        readonly ordinal: number
        readonly parameterOrdinal: number
        readonly source: Mir.LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }> = []
      let callable: Mir.LocalId | undefined
      let callableType: Type.Callable | undefined
      let target: Hir.CallableTarget | undefined
      let typeArguments: ReadonlyArray<Type.GenericArgument> = Object.freeze([])
      const lowerArguments = (): boolean => {
        for (const argument of expression.arguments) {
          const lowered = lowerExpression(fn, argument)
          if (lowered === undefined) return false
          arguments_.push(lowered.result)
        }
        return true
      }
      const lowerCallee = (): boolean => {
        if (directSection !== undefined || directItem !== undefined) {
          if (directType === undefined) return false
          callableType = directType.type
          target = directType.target
          typeArguments =
            call?.target.typeArguments ??
            directType.environment?.callable.typeArguments ??
            Object.freeze(
              [...expression.substitution.values()].map((argument) =>
                fn.semanticArgument(argument),
              ),
            )
          if (directSection !== undefined) {
            for (const capture of directSection.captures) {
              const lowered = lowerExpression(fn, capture.value)
              if (lowered === undefined) return false
              captures.push(
                Object.freeze({
                  ordinal: capture.ordinal,
                  parameterOrdinal: capture.parameterOrdinal,
                  source: lowered.result,
                  access: capture.access,
                }),
              )
            }
          }
          return true
        }
        const lowered = lowerExpression(fn, expression.callee)
        const loweredType =
          lowered === undefined ? undefined : fn.localTypes.at(lowered.result.ordinal)
        if (lowered === undefined || loweredType?._tag !== 'CallableValue') return false
        callable = lowered.result
        callableType = loweredType.type
        typeArguments = loweredType.environment?.callable.typeArguments ?? Object.freeze([])
        return true
      }
      const lowered =
        expression.evaluation === 'LeftThenCallable'
          ? lowerArguments() && lowerCallee()
          : lowerCallee() && lowerArguments()
      const type =
        call?.resultEffect === undefined
          ? fn.type(expression.type)
          : effectValueByIdentity(fn.layout, call.resultEffect)
      if (!lowered || type === undefined || callableType === undefined) return undefined
      const definition =
        callable === undefined ? undefined : fn.callableDefinitions.get(callable.ordinal)
      const realizedTarget = target ?? definition?.target
      if (
        realizedTarget?._tag === 'BuiltinCallableTarget' &&
        Scalar.isCheckedOperation(realizedTarget.operation) &&
        type._tag === 'Union'
      ) {
        const sourceScalar = Scalar.find(realizedTarget.actor)
        const valueScalar = Scalar.conversionTarget(realizedTarget.operation) ?? sourceScalar
        const scalarOperation = sourceScalar?.operations.find(
          (operation) => operation.code === realizedTarget.operation,
        )
        const realizedCaptures = definition?.captures ?? captures
        const ordered: Array<Mir.LocalId | undefined> = Array.from({
          length: scalarOperation?.arity ?? 0,
        })
        for (const capture of realizedCaptures) ordered[capture.parameterOrdinal] = capture.source
        for (const argument of arguments_) {
          const empty = ordered.indexOf(undefined)
          if (empty >= 0) ordered[empty] = argument
        }
        const operands = ordered.filter((operand): operand is Mir.LocalId => operand !== undefined)
        const first = operands.at(0)
        const sourceType = first === undefined ? undefined : fn.localTypes.at(first.ordinal)
        if (
          sourceScalar?.category !== 'Integer' ||
          valueScalar?.category !== 'Integer' ||
          scalarOperation === undefined ||
          operands.length !== scalarOperation.arity ||
          sourceType?._tag !== sourceScalar.spelling ||
          operands.some((operand) => fn.localTypes.at(operand.ordinal)?._tag !== sourceType._tag)
        )
          return undefined
        const success = Type.some(valueScalar.spelling)
        const failure = Type.none
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'CheckedInteger' as const,
            operation: scalarOperation.code,
            destination,
            operands: Object.freeze(operands),
            sourceType,
            valueType: Object.freeze({ _tag: valueScalar.spelling }),
            type,
            success,
            failure,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'ApplyCallable',
          destination,
          ...(callable === undefined ? {} : { callable }),
          ...(target === undefined ? {} : { target }),
          typeArguments,
          captures: Object.freeze(captures),
          arguments: Object.freeze(arguments_),
          callableType,
          access: callableType.mode,
          evaluation: expression.evaluation,
          realization: callable === undefined ? 'DirectErasedSection' : expression.realization,
          type,
          provenance: authored(expression.span),
        }),
      )
      for (const capture of directSection?.captures ?? []) {
        if (capture.value._tag !== 'SliceBorrow' && capture.value._tag !== 'ValueBorrow') continue
        const held = fn.loanLocals.get(borrowKey(capture.value.borrow))
        if (held === undefined) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'EndLoan',
            borrow: capture.value.borrow,
            slice: held,
            provenance: generated(expression.span),
          }),
        )
        fn.loanLocals.delete(borrowKey(capture.value.borrow))
      }
      return Object.freeze({ result: destination })
    }
    case 'EffectConstruct': {
      const call = fn.call(expression.span)
      const typeArguments =
        call?.target.typeArguments ??
        expression.typeArguments.map((argument) => fn.semanticArgument(argument))
      const resultType =
        (call?.resultEffect === undefined
          ? undefined
          : effectValueByIdentity(fn.layout, call.resultEffect)) ??
        fn.effectResults.get(instanceText(expression.target, typeArguments))
      if (resultType === undefined) return undefined
      const arguments_: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        arguments_.push(lowered.result)
      }
      const destination = fn.alloc(resultType)
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          typeArguments: Object.freeze(typeArguments),
          arguments: Object.freeze(arguments_),
          type: resultType,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'EffectBlock': {
      const type = effectValueType(fn.layout, fn.owner.key, expression)
      if (type === undefined) return undefined
      const captures: Array<{
        readonly source: Mir.LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }> = []
      for (const [ordinal, capture] of expression.captures.entries()) {
        const source =
          capture.binding === undefined
            ? capture.parameter === undefined
              ? undefined
              : fn.parameterLocals.get(capture.parameter.ordinal)
            : fn.bindingLocals.get(capture.binding.ordinal)
        if (source === undefined) return undefined
        const access = type.environment.fields.at(ordinal)?.access
        if (access === undefined) return undefined
        captures.push(Object.freeze({ source, access }))
      }
      const destination = fn.alloc(type)
      const runner = Hir.effectRunnerId(fn.owner.key.declaration, expression.site)
      fn.emit(
        Object.freeze({
          _tag: 'MakeEffect',
          destination,
          runner,
          runnerTypeArguments: fn.owner.key.typeArguments,
          captures: Object.freeze(captures),
          type,
          provenance: authored(expression.span),
        }),
      )
      if (
        !fn.generatedRunners.some(
          (candidate) =>
            candidate.specializationKey === baseRunnerKey(fn.owner.key, expression.site),
        )
      ) {
        fn.generatedRunners.push(
          Object.freeze({
            id: runner,
            owner: fn.owner,
            block: expression,
            type,
            specializationKey: baseRunnerKey(fn.owner.key, expression.site),
            providedRequirements: Object.freeze([]),
          }),
        )
      }
      return Object.freeze({ result: destination })
    }
    case 'EffectResult':
      return undefined
    case 'Run': {
      const resultRecipe =
        expression.subject._tag === 'BindingReference'
          ? fn.effectRecipes.get(expression.subject.binding.ordinal)
          : expression.subject
      if (resultRecipe?._tag === 'EffectResult') {
        const loweredProtected =
          resultRecipe.protected._tag === 'ServiceEffectConstruct'
            ? lowerServiceEffectValue(fn, resultRecipe.protected, fn.providedRequirements)
            : lowerExpression(fn, resultRecipe.protected)
        const protectedType =
          loweredProtected === undefined
            ? undefined
            : fn.localTypes.at(loweredProtected.result.ordinal)
        if (loweredProtected === undefined || protectedType?._tag !== 'EffectValue')
          return undefined
        const provided = requirementsFor(fn.providedRequirements, protectedType.type)
        if (provided === undefined) return undefined
        const runner =
          provided.length === 0
            ? Hir.effectRunnerId(protectedType.environment.instance.declaration, protectedType.site)
            : ensureProvidedRunner(fn, protectedType, provided)
        if (runner === undefined) return undefined
        const arguments_ = runtimeRequirementArguments(provided)
        const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
          _tag: 'EffectOutcome',
          type: protectedType.type,
        })
        const resultType = fn.type(expression.type)
        const failureValueType = Type.failureValue(protectedType.type.failures)
        const successType = Type.resultSuccess(protectedType.type.success)
        const failureType = Type.resultFailure(failureValueType)
        const resultUnionNormalization = Type.union([successType, failureType])
        const resultUnion =
          resultUnionNormalization._tag === 'Normalized' &&
          Type.isUnion(resultUnionNormalization.type)
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
          resultType?._tag === 'Nominal'
            ? Layout.callingShape(fn.layout, resultType.type)
            : undefined
        const outcomeShape = Layout.callingShape(fn.layout, protectedType.type)
        const failureValueShape = Layout.callingShape(fn.layout, failureValueType)
        const successTag = resultUnion?.members.findIndex((member) =>
          Type.equals(member, successType),
        )
        const failureTag = resultUnion?.members.findIndex((member) =>
          Type.equals(member, failureType),
        )
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
            effect: loweredProtected.result,
            runner,
            runnerTypeArguments: protectedType.environment.instance.typeArguments,
            arguments: arguments_,
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
            provenance: authored(expression.span),
          }),
        )
        endRunLoans(fn, expression.span)
        if (
          resultRecipe.protected._tag === 'EffectConstruct' ||
          resultRecipe.protected._tag === 'ServiceEffectConstruct'
        )
          endLoans(fn, resultRecipe.protected.loanEnds, expression.span)
        return Object.freeze({ result: destination })
      }
      if (resultRecipe !== undefined && inlineForwardedRequirement(fn, resultRecipe) !== undefined)
        return lowerEffectExecution(fn, resultRecipe, expression.type, expression.span)
      if (resultRecipe?._tag === 'ServiceEffectConstruct')
        return lowerEffectExecution(fn, resultRecipe, expression.type, expression.span)
      const recipe =
        expression.subject._tag === 'BindingReference'
          ? fn.effectRecipes.get(expression.subject.binding.ordinal)
          : expression.subject
      // Compiler-backed effects lower directly from their recipe. Lowering the effect expression
      // first would form every borrowed argument twice before the dedicated operation is emitted.
      const loweredSubject =
        recipe?._tag === 'BuiltinCall' ? undefined : lowerExpression(fn, expression.subject)
      const effectValueType =
        loweredSubject === undefined ? undefined : fn.localTypes.at(loweredSubject.result.ordinal)
      if (loweredSubject !== undefined && effectValueType?._tag === 'EffectValue') {
        const outcomeType: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
          _tag: 'EffectOutcome',
          type: effectValueType.type,
        })
        const structuralSuccess = fn.semantic(expression.type)
        const successType = Type.isEffect(structuralSuccess)
          ? effectValueByIdentity(
              fn.layout,
              effectValueType.environment.successEffectIdentity ?? '',
            )
          : fn.type(expression.type)
        if (successType === undefined || successType._tag === 'EffectOutcome') return undefined
        const outcome = fn.alloc(outcomeType)
        const destination = fn.alloc(successType)
        const propagationType =
          effectValueType.type.failures.length === 0 || fn.effectOutcome === undefined
            ? undefined
            : fn.type(fn.effectOutcome)
        if (propagationType !== undefined && propagationType._tag !== 'EffectOutcome')
          return undefined
        const tagMappings = effectValueType.type.failures.flatMap((failure, source) => {
          const target = propagationType?.type.failures.findIndex((candidate) =>
            Type.equals(candidate, failure),
          )
          return target === undefined || target < 0
            ? []
            : [Object.freeze({ source: source + 1, target: target + 1 })]
        })
        if (tagMappings.length !== effectValueType.type.failures.length) return undefined
        const propagationShape =
          propagationType === undefined
            ? undefined
            : Layout.callingShape(fn.layout, propagationType.type)
        const releases = propagationReleases(fn, expression.span)
        const provided = requirementsFor(fn.providedRequirements, effectValueType.type)
        const providedRunner =
          provided === undefined || provided.length === 0
            ? undefined
            : ensureProvidedRunner(fn, effectValueType, provided)
        if (provided !== undefined && provided.length > 0 && providedRunner === undefined)
          return undefined
        fn.emit(
          Object.freeze({
            _tag: 'RunEffectValue',
            destination,
            outcome,
            effect: loweredSubject.result,
            runner:
              providedRunner ??
              Hir.effectRunnerId(
                effectValueType.environment.instance.declaration,
                effectValueType.site,
              ),
            runnerTypeArguments: effectValueType.environment.instance.typeArguments,
            arguments: runtimeRequirementArguments(provided),
            outcomeType,
            ...(propagationType === undefined ? {} : { propagationType }),
            tagMappings: Object.freeze(tagMappings),
            propagationLaneCount: propagationShape?.laneCount ?? 0,
            ...(propagationType === undefined || releases.length === 0 ? {} : { releases }),
            type: successType,
            provenance: authored(expression.span),
          }),
        )
        endRunLoans(fn, expression.span)
        if (expression.subject._tag === 'EffectConstruct')
          endLoans(fn, expression.subject.loanEnds, expression.span)
        return Object.freeze({ result: destination })
      }
      if (recipe?._tag === 'EffectBindRequirement' && recipe.protected._tag !== 'BuiltinCall')
        return lowerEffectExecution(fn, recipe, expression.type, expression.span)
      if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'EffectSuspend') {
        const deferred = recipe.arguments.at(0)
        return deferred === undefined
          ? undefined
          : lowerEffectExecution(fn, deferred, expression.type, expression.span)
      }
      if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'StorageAcquire') {
        const [layoutExpression] = recipe.arguments
        if (layoutExpression === undefined || fn.effectOutcome === undefined) return undefined
        const loweredLayout = lowerExpression(fn, layoutExpression)
        const type = fn.type(expression.type)
        const propagationType = fn.type(fn.effectOutcome)
        const failureTag = fn.effectOutcome.failures.findIndex((failure) =>
          Type.equals(failure, Type.outOfMemory),
        )
        if (
          loweredLayout === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.allocation) ||
          propagationType?._tag !== 'EffectOutcome' ||
          failureTag < 0
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'Allocate' as const,
            destination,
            layout: loweredLayout.result,
            type,
            failure: Type.outOfMemory,
            propagationType,
            failureTag: failureTag + 1,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
      if (recipe?._tag === 'BuiltinCall' && recipe.operation === 'HostWrite') {
        const [streamExpression, bytesExpression] = recipe.arguments
        if (
          streamExpression === undefined ||
          bytesExpression === undefined ||
          fn.effectOutcome === undefined
        )
          return undefined
        const stream = lowerExpression(fn, streamExpression)
        const bytes = lowerExpression(fn, bytesExpression)
        const type = fn.type(expression.type)
        const propagationType = fn.type(fn.effectOutcome)
        const failureTag = fn.effectOutcome.failures.findIndex((failure) =>
          Type.equals(failure, Type.streamWriteFailure),
        )
        if (
          stream === undefined ||
          bytes === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit) ||
          propagationType?._tag !== 'EffectOutcome' ||
          failureTag < 0
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'HostWrite' as const,
            destination,
            stream: stream.result,
            bytes: bytes.result,
            type,
            failure: Type.streamWriteFailure,
            propagationType,
            failureTag: failureTag + 1,
            provenance: authored(expression.span),
          }),
        )
        return Object.freeze({ result: destination })
      }
      if (recipe?._tag === 'BuiltinCall' && isOsOperation(recipe.operation)) {
        const arguments_: Array<Mir.LocalId> = []
        for (const argument of recipe.arguments) {
          const lowered = lowerExpression(fn, argument)
          if (lowered === undefined) return undefined
          arguments_.push(lowered.result)
        }
        const type = fn.type(expression.type)
        if (type === undefined) return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'OsCall' as const,
            operation: recipe.intrinsic,
            destination,
            arguments: Object.freeze(arguments_),
            type,
            provenance: authored(expression.span),
          }),
        )
        for (const borrow of recipe.loanEnds) {
          const held = fn.loanLocals.get(borrowKey(borrow))
          if (held === undefined) continue
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan' as const,
              borrow,
              slice: held,
              provenance: generated(expression.span),
            }),
          )
          fn.loanLocals.delete(borrowKey(borrow))
        }
        return Object.freeze({ result: destination })
      }
      if (recipe?._tag === 'EffectBindRequirement') {
        const provider =
          recipe.provider.binding !== undefined
            ? fn.bindingLocals.get(recipe.provider.binding.ordinal)
            : recipe.provider.parameter !== undefined
              ? fn.parameterLocals.get(recipe.provider.parameter.ordinal)
              : undefined
        if (provider === undefined) return undefined
        const selectedProvider = specializeProvider(fn, recipe.provider)
        if (selectedProvider === undefined) return undefined
        if (recipe.provider.access === 'Take') return undefined
        const loweredProtected = lowerExpression(fn, recipe.protected)
        const protectedType =
          loweredProtected === undefined
            ? undefined
            : fn.localTypes.at(loweredProtected.result.ordinal)
        if (loweredProtected === undefined || protectedType?._tag !== 'EffectValue')
          return undefined
        if (selectedProvider.witness._tag !== 'SourceConformanceWitness') {
          const result = lowerRunEffectValue(
            fn,
            loweredProtected.result,
            protectedType,
            expression.type,
            expression.span,
            Object.freeze([...fn.providedRequirements, selectedProvider]),
          )
          if (result !== undefined) {
            endRunLoans(fn, expression.span)
            if (recipe.protected._tag === 'EffectConstruct')
              endLoans(fn, recipe.protected.loanEnds, expression.span)
          }
          return result
        }
        const providerType = fn.type(selectedProvider.providerType)
        const referenceType = fn.type(
          Object.freeze({
            _tag: 'ReferenceType' as const,
            access: recipe.provider.access,
            target: selectedProvider.providerType,
          }),
        )
        const loan = fn.ownership?.loans.find(
          (candidate) =>
            candidate.origin === 'EffectCapture' &&
            candidate.access === recipe.provider.access &&
            candidate.startSpan.start === recipe.provider.span.start &&
            candidate.startSpan.end === recipe.provider.span.end,
        )
        if (
          providerType?._tag !== 'Nominal' ||
          referenceType?._tag !== 'Reference' ||
          loan === undefined
        )
          return undefined
        const reference = fn.alloc(referenceType)
        fn.emit(
          Object.freeze({
            _tag: 'BeginLoan',
            borrow: loan.id,
            destination: reference,
            root: provider,
            selectors: Object.freeze([]),
            sourceType: providerType,
            type: referenceType,
            access: recipe.provider.access,
            reborrow: false,
            suspendsParent: false,
            provenance: authored(recipe.provider.span),
          }),
        )
        const provided: ProvidedRequirement = Object.freeze({
          ...selectedProvider,
          local: reference,
        })
        const result = lowerRunEffectValue(
          fn,
          loweredProtected.result,
          protectedType,
          expression.type,
          expression.span,
          Object.freeze([...fn.providedRequirements, provided]),
        )
        if (result === undefined) return undefined
        endRunLoans(fn, expression.span)
        if (recipe.protected._tag === 'EffectConstruct')
          endLoans(fn, recipe.protected.loanEnds, expression.span)
        fn.emit(
          Object.freeze({
            _tag: 'EndLoan',
            borrow: loan.id,
            slice: reference,
            provenance: generated(recipe.provider.span),
          }),
        )
        return result
      }
      if (recipe?._tag !== 'EffectConstruct') return undefined
      const arguments_: Array<Mir.LocalId> = []
      for (const argument of recipe.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        arguments_.push(lowered.result)
      }
      const outcomeType = fn.type(recipe.type)
      const successType = fn.type(expression.type)
      if (
        outcomeType?._tag !== 'EffectOutcome' ||
        successType === undefined ||
        successType._tag === 'EffectOutcome'
      )
        return undefined
      const outcome = fn.alloc(outcomeType)
      const destination = fn.alloc(successType)
      if (recipe.type.failures.length > 0) {
        const propagationType =
          fn.effectOutcome === undefined ? undefined : fn.type(fn.effectOutcome)
        const propagationShape =
          fn.effectOutcome === undefined
            ? undefined
            : Layout.callingShape(fn.layout, fn.effectOutcome)
        if (propagationType?._tag !== 'EffectOutcome' || propagationShape === undefined)
          return undefined
        const tagMappings = recipe.type.failures.flatMap((failure, source) => {
          const target = propagationType.type.failures.findIndex((candidate) =>
            Type.equals(candidate, failure),
          )
          return target < 0 ? [] : [Object.freeze({ source: source + 1, target: target + 1 })]
        })
        if (tagMappings.length !== recipe.type.failures.length) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'RunEffect',
            destination,
            outcome,
            target: recipe.target,
            typeArguments: Object.freeze(
              recipe.typeArguments.map((argument) => fn.semanticArgument(argument)),
            ),
            arguments: Object.freeze(arguments_),
            outcomeType,
            propagationType,
            ...(propagationReleases(fn, expression.span).length === 0
              ? {}
              : { releases: propagationReleases(fn, expression.span) }),
            tagMappings: Object.freeze(tagMappings),
            propagationLaneCount: propagationShape.laneCount,
            type: successType,
            provenance: authored(expression.span),
          }),
        )
        // The effect held its argument borrows for exactly this run; end them here.
        for (const borrow of recipe.loanEnds) {
          const held = fn.loanLocals.get(borrowKey(borrow))
          if (held === undefined) continue
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan',
              borrow,
              slice: held,
              provenance: generated(expression.span),
            }),
          )
          fn.loanLocals.delete(borrowKey(borrow))
        }
        return Object.freeze({ result: destination })
      }
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination: outcome,
          target: recipe.target,
          typeArguments: Object.freeze(
            recipe.typeArguments.map((argument) => fn.semanticArgument(argument)),
          ),
          arguments: Object.freeze(arguments_),
          type: outcomeType,
          provenance: authored(expression.span),
        }),
      )
      for (const borrow of recipe.loanEnds) {
        const held = fn.loanLocals.get(borrowKey(borrow))
        if (held === undefined) continue
        fn.emit(
          Object.freeze({
            _tag: 'EndLoan',
            borrow,
            slice: held,
            provenance: generated(expression.span),
          }),
        )
        fn.loanLocals.delete(borrowKey(borrow))
      }
      fn.emit(
        Object.freeze({
          _tag: 'UnpackEffectSuccess',
          destination,
          source: outcome,
          type: successType,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'UnionConvert': {
      const source = lowerExpression(fn, expression.source)
      // Effect access is a semantic ownership coercion. Hidden construction identity has already
      // selected one concrete EffectValue layout, so the runtime representation is unchanged.
      if (expression.conversion === 'EffectAccess') return source
      const sourceType = fn.type(expression.sourceType)
      const targetType = fn.type(expression.target)
      const sourceShape = Layout.callingShape(fn.layout, fn.semantic(expression.sourceType))
      const targetShape = Layout.callingShape(fn.layout, fn.semantic(expression.target))
      if (
        source === undefined ||
        sourceShape === undefined ||
        targetShape === undefined ||
        (sourceType?._tag !== 'Nominal' && sourceType?._tag !== 'Union') ||
        targetType?._tag !== 'Union'
      ) {
        return undefined
      }
      const destination = fn.alloc(targetType)
      // Canonical union member order can change under substitution (parameter keys sort
      // differently from concrete keys), so the mapping recomputes at the instantiation.
      const substituted = TypeCompatibility.check(
        fn.semantic(expression.sourceType),
        fn.semantic(expression.target),
      )
      const mappings =
        substituted._tag === 'Inject' || substituted._tag === 'Widen'
          ? substituted.mappings
          : expression.mappings
      fn.emit(
        Object.freeze({
          _tag: 'ConvertUnion',
          destination,
          source: source.result,
          sourceType,
          targetType,
          conversion: expression.conversion,
          mappings,
          sourceShape,
          targetShape,
          access: expression.access,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'ShortCircuit': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'bool') return undefined
      const left = lowerExpression(fn, expression.left)
      if (left === undefined) return undefined
      // The right operand's operations stay nested so that the engines can emit them under the
      // branch instead of before it. It is pure by elaboration, so nothing there needs releasing
      // on the path that skips it.
      const [right, rightOperations] = fn.capture(() => lowerExpression(fn, expression.right))
      if (right === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'ShortCircuit',
          operator: expression.operator,
          destination,
          left: left.result,
          right: Object.freeze({ operations: rightOperations, result: right.result }),
          type,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'Match': {
      if (expression.scrutinee._tag === 'Unavailable') return undefined
      const scrutinee = lowerExpression(fn, expression.scrutinee)
      const scrutineeType = fn.type(expression.scrutinee.type)
      const resultType = fn.type(expression.type)
      const scrutineeShape = Layout.callingShape(fn.layout, fn.semantic(expression.scrutinee.type))
      const resultShape = Layout.callingShape(fn.layout, fn.semantic(expression.type))
      if (
        scrutinee === undefined ||
        (scrutineeType?._tag !== 'Nominal' && scrutineeType?._tag !== 'Union') ||
        resultType === undefined ||
        scrutineeShape === undefined ||
        resultShape === undefined
      ) {
        return undefined
      }
      const ownership = fn.ownership?.matches.find(
        (candidate) =>
          candidate.id.span.start === expression.id.span.start &&
          candidate.id.span.end === expression.id.span.end,
      )
      const specializeMember = (member: Type.Nominal): Type.Nominal | undefined => {
        const specialized = fn.semantic(member)
        return Type.isNominal(specialized) ? specialized : undefined
      }
      const members = expression.members.flatMap((member) => {
        const specialized = specializeMember(member)
        return specialized === undefined ? [] : [specialized]
      })
      if (members.length !== expression.members.length) return undefined
      const arms: Array<Mir.MatchArm> = []
      for (const arm of expression.arms) {
        if (!arm.reachable) continue
        const member = arm.member === undefined ? undefined : specializeMember(arm.member)
        if (arm.member !== undefined && member === undefined) return undefined
        const before = arm.before.flatMap((candidate) => {
          const specialized = specializeMember(candidate)
          return specialized === undefined ? [] : [specialized]
        })
        const after = arm.after.flatMap((candidate) => {
          const specialized = specializeMember(candidate)
          return specialized === undefined ? [] : [specialized]
        })
        if (before.length !== arm.before.length || after.length !== arm.after.length)
          return undefined
        const bindings: Array<Mir.MatchBinding> = []
        for (const binding of arm.bindings) {
          const type = fn.type(binding.type)
          if (type === undefined) return undefined
          const destination = fn.alloc(type)
          fn.patternLocals.set(patternKey(binding.id), destination)
          bindings.push(
            Object.freeze({
              id: binding.id,
              destination,
              path: binding.path,
              type,
              access: binding.access,
              provenance: authored(binding.span),
            }),
          )
        }
        const guardExpression = arm.guard
        const guard =
          guardExpression === undefined
            ? undefined
            : (() => {
                const [lowered, operations] = fn.capture(() => lowerExpression(fn, guardExpression))
                return lowered === undefined
                  ? undefined
                  : Object.freeze({ operations, result: lowered.result })
              })()
        if (guardExpression !== undefined && guard === undefined) return undefined
        const [selectedResult, selectedOperations] = fn.capture(() =>
          lowerExpression(fn, arm.result),
        )
        if (selectedResult === undefined) return undefined
        const ownedArm = ownership?.arms.find(
          (candidate) => candidate.id.ordinal === arm.id.ordinal,
        )
        arms.push(
          Object.freeze({
            id: arm.id,
            ...(member === undefined ? {} : { member }),
            universal: arm.universal,
            before: Object.freeze(before),
            after: Object.freeze(after),
            bindings: Object.freeze(bindings),
            ...(guard === undefined ? {} : { guard }),
            selected: Object.freeze({
              access: expression.access,
              operations: selectedOperations,
              result: selectedResult.result,
              cleanup: Object.freeze(
                (ownedArm?.cleanup ?? []).map((release) =>
                  Object.freeze({
                    path: release.path,
                    cleanup: specializedCleanup(fn, release.cleanup),
                  }),
                ),
              ),
              endBorrow: expression.access === 'Shared' || expression.access === 'Exclusive',
            }),
            provenance: authored(arm.span),
          }),
        )
        for (const binding of arm.bindings) fn.patternLocals.delete(patternKey(binding.id))
      }
      const destination = fn.alloc(resultType)
      const decisions = members.map((member) =>
        Object.freeze({
          member,
          candidates: Object.freeze(
            arms
              .filter(
                (arm) =>
                  arm.universal || (arm.member !== undefined && Type.equals(arm.member, member)),
              )
              .map((arm) => arm.id),
          ),
        }),
      )
      fn.emit(
        Object.freeze({
          _tag: 'Match',
          id: expression.id,
          destination,
          scrutinee: scrutinee.result,
          scrutineeType,
          scrutineeShape,
          access: expression.access,
          members: Object.freeze(members),
          decisions: Object.freeze(decisions),
          arms: Object.freeze(arms),
          type: resultType,
          resultShape,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'Construct': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'Nominal') return undefined
      const canonicalFields = new Map(
        expression.fields.map((field) => [field.field.ordinal, field] as const),
      )
      const loweredFields = new Map<number, Mir.LocalId>()
      for (const fieldId of expression.evaluationOrder) {
        const field = canonicalFields.get(fieldId.ordinal)
        if (field === undefined) return undefined
        const lowered = lowerExpression(fn, field.value)
        if (lowered === undefined) return undefined
        loweredFields.set(field.field.ordinal, lowered.result)
      }
      const fields = expression.fields.flatMap((field) => {
        const value = loweredFields.get(field.field.ordinal)
        return value === undefined ? [] : [Object.freeze({ field: field.field, value })]
      })
      if (fields.length !== expression.fields.length) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Construct',
          destination,
          type,
          fields: Object.freeze(fields),
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'ArrayConstruct': {
      const type = fn.type(expression.type)
      if (type?._tag !== 'FixedArray') return undefined
      const elements: Array<Mir.LocalId> = []
      for (const element of expression.elements) {
        const lowered = lowerExpression(fn, element)
        if (lowered === undefined) return undefined
        elements.push(lowered.result)
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'ConstructArray',
          destination,
          type,
          elements: Object.freeze(elements),
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'Project': {
      return lowerPlace(fn, expression)
    }
    case 'IndexPlace': {
      return lowerPlace(fn, expression)
    }
    case 'SliceBorrow': {
      const root =
        expression.root._tag === 'BindingSliceRoot'
          ? fn.bindingLocals.get(expression.root.binding.ordinal)
          : expression.root._tag === 'ParameterSliceRoot'
            ? local(expression.root.parameter.ordinal)
            : fn.patternLocals.get(patternKey(expression.root.binding))
      const sourceType = fn.type(expression.source)
      const type = fn.type(expression.type)
      if (
        root === undefined ||
        (sourceType?._tag !== 'FixedArray' && sourceType?._tag !== 'Slice') ||
        type?._tag !== 'Slice'
      ) {
        return undefined
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow: expression.borrow,
          destination,
          root,
          selectors: Object.freeze([]),
          sourceType,
          type,
          access: expression.access,
          reborrow: expression.reborrow,
          suspendsParent: expression.suspendsParent,
          provenance: authored(expression.span),
        }),
      )
      fn.loanLocals.set(borrowKey(expression.borrow), destination)
      return Object.freeze({ result: destination })
    }
    case 'ValueBorrow': {
      const root =
        expression.root._tag === 'BindingSliceRoot'
          ? fn.bindingLocals.get(expression.root.binding.ordinal)
          : expression.root._tag === 'ParameterSliceRoot'
            ? local(expression.root.parameter.ordinal)
            : fn.patternLocals.get(patternKey(expression.root.binding))
      const sourceType = fn.type(expression.source)
      const type = fn.type(expression.type)
      if (root === undefined || sourceType === undefined || type?._tag !== 'Reference') {
        return undefined
      }
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'BeginLoan',
          borrow: expression.borrow,
          destination,
          root,
          selectors: Object.freeze(
            expression.path.map((field) =>
              Object.freeze({
                _tag: 'FieldSelector' as const,
                field,
                provenance: authored(expression.span),
              }),
            ),
          ),
          sourceType,
          type,
          access: expression.access,
          reborrow: false,
          suspendsParent: false,
          provenance: authored(expression.span),
        }),
      )
      fn.loanLocals.set(borrowKey(expression.borrow), destination)
      return Object.freeze({ result: destination })
    }
    case 'SliceLength': {
      const slice = lowerExpression(fn, expression.slice)
      const sliceType = slice === undefined ? undefined : fn.localTypes.at(slice.result.ordinal)
      if (
        slice === undefined ||
        sliceType === undefined ||
        !Type.isSlice(Mir.semanticType(sliceType))
      ) {
        return undefined
      }
      const destination = fn.alloc(usize)
      fn.emit(
        Object.freeze({
          _tag: 'SliceLength',
          destination,
          slice: slice.result,
          type: usize,
          provenance: authored(expression.span),
        }),
      )
      return Object.freeze({ result: destination })
    }
    case 'SliceIndexPlace': {
      return lowerPlace(fn, expression)
    }
    case 'Call': {
      const argumentLocals: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        argumentLocals.push(lowered.result)
      }
      const call = fn.call(expression.span)
      const type =
        (call?.resultEffect === undefined
          ? undefined
          : effectValueByIdentity(fn.layout, call.resultEffect)) ?? fn.type(expression.type)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Call',
          destination,
          target: expression.target,
          typeArguments: Object.freeze(
            call?.target.typeArguments ??
              expression.typeArguments.map((argument) => fn.semanticArgument(argument)),
          ),
          arguments: Object.freeze(argumentLocals),
          type,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      for (const borrow of expression.loanEnds) {
        const slice = fn.loanLocals.get(borrowKey(borrow))
        if (slice === undefined) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'EndLoan',
            borrow,
            slice,
            provenance: generated(expression.span),
          }),
        )
        fn.loanLocals.delete(borrowKey(borrow))
      }
      return { result: destination }
    }
    case 'BoundOperationCall': {
      // The bound named the operation; the specialization names the witness. Only here is the type
      // argument known, so only here can the conformance say which compiler-known operation the
      // call runs — two providers of one interface may answer one operation with two unrelated
      // instructions, and an operator's width-neutral lowering cannot stand in for that.
      const capability = fn.semantic(expression.capability)
      const provider = fn.semantic(expression.provider)
      if (!Type.isNominal(capability)) return undefined
      const selected = DeclarationIndex.interfaceOperationIntrinsic(
        fn.index,
        provider,
        capability,
        expression.operation,
      )
      // A witness names either a sealed intrinsic or a function of the provider's own actor, and
      // which one it names is the conformance's business rather than the call's. The operator path
      // has always read both; this call reads the second one here.
      if (selected?.rule._tag !== 'BuiltinRule') {
        const argumentLocals: Array<Mir.LocalId> = []
        for (const argument of expression.arguments) {
          const lowered = lowerExpression(fn, argument)
          if (lowered === undefined) return undefined
          argumentLocals.push(lowered.result)
        }
        const result = lowerBoundWitnessCall(fn, expression, provider, capability, argumentLocals)
        if (result === undefined) return undefined
        for (const borrow of expression.loanEnds) {
          const loan = fn.loanLocals.get(borrowKey(borrow))
          if (loan === undefined) continue
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan' as const,
              borrow,
              slice: loan,
              provenance: generated(expression.span),
            }),
          )
          fn.loanLocals.delete(borrowKey(borrow))
        }
        return Object.freeze({ result })
      }
      // The call the witness names, at this call's own span: the enclosing `lowerExpression` ends
      // this span's returned-view loans once, after the operation it selected has been lowered.
      return lowerExpressionInner(
        fn,
        Object.freeze({
          _tag: 'BuiltinCall',
          operation: selected.rule.operation,
          intrinsic: selected.id,
          typeArguments: Object.freeze([]),
          arguments: expression.arguments,
          loanEnds: expression.loanEnds,
          heldLoans: Object.freeze([]),
          type: expression.type,
          span: expression.span,
        }),
      )
    }
    case 'BuiltinCall': {
      const argumentLocals: Array<Mir.LocalId> = []
      for (const argument of expression.arguments) {
        const lowered = lowerExpression(fn, argument)
        if (lowered === undefined) return undefined
        argumentLocals.push(lowered.result)
      }
      const finishBuiltin = (result: Mir.LocalId): { readonly result: Mir.LocalId } => {
        const slot = argumentLocals.at(0)
        const inherited =
          expression.operation === 'SlotWrite' ||
          expression.operation === 'SlotTake' ||
          expression.operation === 'SlotCopy' ||
          expression.operation === 'SlotDrop'
            ? slot === undefined
              ? []
              : (fn.slotLoans.get(slot.ordinal) ?? [])
            : []
        const endings = new Map(
          [...expression.loanEnds, ...inherited].map((borrow) => [borrowKey(borrow), borrow]),
        )
        for (const borrow of endings.values()) {
          const loan = fn.loanLocals.get(borrowKey(borrow))
          if (loan === undefined) continue
          fn.emit(
            Object.freeze({
              _tag: 'EndLoan' as const,
              borrow,
              slice: loan,
              provenance: generated(expression.span),
            }),
          )
          fn.loanLocals.delete(borrowKey(borrow))
        }
        if (slot !== undefined && inherited.length > 0) fn.slotLoans.delete(slot.ordinal)
        return Object.freeze({ result })
      }
      const witnessCall = lowerInterfaceWitnessCall(fn, expression, argumentLocals)
      if (witnessCall !== undefined) return finishBuiltin(witnessCall)
      if (expression.operation === 'LayoutOf') {
        const raw = expression.typeArguments.at(0)
        const element = raw === undefined ? undefined : fn.semantic(raw)
        const elementLayout = element === undefined ? undefined : Layout.entry(fn.layout, element)
        const layoutEntry = Layout.entry(fn.layout, Type.layout)
        const type = fn.type(Type.layout)
        if (
          elementLayout === undefined ||
          layoutEntry?.representation._tag !== 'Aggregate' ||
          type?._tag !== 'Nominal'
        )
          return undefined
        const fields: Array<{
          readonly field: DeclarationIndex.FieldId
          readonly value: Mir.LocalId
        }> = []
        for (const field of layoutEntry.representation.fields) {
          const value = fn.alloc(usize)
          fn.emit(
            Object.freeze({
              _tag: 'Literal' as const,
              destination: value,
              type: usize,
              value: BigInt(field.name === 'bytes' ? elementLayout.size : elementLayout.alignment),
              provenance: generated(expression.span),
            }),
          )
          fields.push(Object.freeze({ field: field.id, value }))
        }
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'Construct' as const,
            destination,
            type,
            fields: Object.freeze(fields),
            provenance: generated(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (
        expression.operation === 'EffectSuspend' ||
        expression.operation === 'StorageAcquire' ||
        expression.operation === 'HostWrite'
      )
        return undefined
      if (expression.operation === 'RawBufferFrom') {
        const [allocation, count] = argumentLocals
        const type = fn.type(expression.type)
        const raw = Type.isRawBuffer(expression.type)
          ? Type.typeArgumentAt(expression.type, 0)
          : undefined
        const element = raw === undefined ? undefined : fn.semantic(raw)
        const elementLayout = element === undefined ? undefined : Layout.entry(fn.layout, element)
        if (
          allocation === undefined ||
          count === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.isRawBuffer(type.type) ||
          element === undefined ||
          elementLayout === undefined
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferFrom' as const,
            destination,
            allocation,
            count,
            element,
            stride:
              Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
            elementAlignment: elementLayout.alignment,
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferCount') {
        const [buffer] = argumentLocals
        if (buffer === undefined) return undefined
        const destination = fn.alloc(usize)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferCount' as const,
            destination,
            buffer,
            type: usize,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferRead') {
        const [buffer, index] = argumentLocals
        const type = fn.type(expression.type)
        if (buffer === undefined || index === undefined || type === undefined) return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferRead' as const,
            destination,
            buffer,
            index,
            element: fn.semantic(expression.type),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferView' || expression.operation === 'RawBufferViewMut') {
        const [buffer, offset, length] = argumentLocals
        const type = fn.type(expression.type)
        const element = Type.isSlice(expression.type) ? expression.type.element : undefined
        const semanticElement = element === undefined ? undefined : fn.semantic(element)
        const elementLayout =
          semanticElement === undefined ? undefined : Layout.entry(fn.layout, semanticElement)
        if (
          buffer === undefined ||
          offset === undefined ||
          length === undefined ||
          type?._tag !== 'Slice' ||
          semanticElement === undefined ||
          elementLayout === undefined
        ) {
          return undefined
        }
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferView' as const,
            destination,
            buffer,
            offset,
            length,
            element: semanticElement,
            stride:
              Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
            access: expression.operation === 'RawBufferView' ? 'Shared' : 'Exclusive',
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferSlot') {
        const [buffer, index] = argumentLocals
        const type = fn.type(expression.type)
        const element = Type.isSlot(expression.type)
          ? Type.typeArgumentAt(expression.type, 0)
          : undefined
        if (
          buffer === undefined ||
          index === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.isSlot(type.type) ||
          element === undefined
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferSlot' as const,
            destination,
            buffer,
            index,
            element: fn.semantic(element),
            type,
            provenance: authored(expression.span),
          }),
        )
        fn.slotLoans.set(destination.ordinal, expression.heldLoans)
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferCopy') {
        const [buffer, offset, source, length] = argumentLocals
        const sourceArgument = expression.arguments.at(2)
        const sourceType = sourceArgument?._tag === 'Unavailable' ? undefined : sourceArgument?.type
        const element =
          sourceType !== undefined && Type.isSlice(sourceType) ? sourceType.element : undefined
        const semanticElement = element === undefined ? undefined : fn.semantic(element)
        const elementLayout =
          semanticElement === undefined ? undefined : Layout.entry(fn.layout, semanticElement)
        const type = fn.type(expression.type)
        if (
          buffer === undefined ||
          offset === undefined ||
          source === undefined ||
          length === undefined ||
          semanticElement === undefined ||
          elementLayout === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit)
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferCopy' as const,
            destination,
            buffer,
            offset,
            source,
            length,
            element: semanticElement,
            stride:
              Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment,
            retainsSource: Mir.isStructurallyCopy(fn.layout, semanticElement),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'RawBufferFill') {
        const [buffer, offset, length, value] = argumentLocals
        const type = fn.type(expression.type)
        if (
          buffer === undefined ||
          offset === undefined ||
          length === undefined ||
          value === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit)
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'RawBufferFill' as const,
            destination,
            buffer,
            offset,
            length,
            value,
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotWrite') {
        const [slot, value] = argumentLocals
        const slotArgument = expression.arguments.at(0)
        const slotType = slotArgument?._tag === 'Unavailable' ? undefined : slotArgument?.type
        const slotElement =
          slotType !== undefined && Type.isSlot(slotType)
            ? Type.typeArgumentAt(slotType, 0)
            : undefined
        const type = fn.type(expression.type)
        if (
          slot === undefined ||
          value === undefined ||
          slotElement === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit)
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'SlotWrite' as const,
            destination,
            slot,
            value,
            element: fn.semantic(slotElement),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotTake') {
        const [slot] = argumentLocals
        const type = fn.type(expression.type)
        if (slot === undefined || type === undefined) return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'SlotTake' as const,
            destination,
            slot,
            element: fn.semantic(expression.type),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotCopy') {
        const [slot] = argumentLocals
        const type = fn.type(expression.type)
        if (slot === undefined || type === undefined) return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'SlotCopy' as const,
            destination,
            slot,
            element: fn.semantic(expression.type),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'SlotDrop') {
        const [slot] = argumentLocals
        const slotArgument = expression.arguments.at(0)
        const slotType = slotArgument?._tag === 'Unavailable' ? undefined : slotArgument?.type
        const element =
          slotType !== undefined && Type.isSlot(slotType)
            ? Type.typeArgumentAt(slotType, 0)
            : undefined
        const type = fn.type(expression.type)
        if (
          slot === undefined ||
          element === undefined ||
          type?._tag !== 'Nominal' ||
          !Type.equals(type.type, Type.unit)
        )
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'SlotDrop' as const,
            destination,
            slot,
            element: fn.semantic(element),
            cleanup: concreteCleanup(fn, fn.semantic(element)),
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (isOsOperation(expression.operation)) return undefined
      if (expression.operation === 'StringFromUtf8Unchecked') return undefined
      if (expression.operation === 'StringUtf8Bytes') {
        const [string] = argumentLocals
        const stringType = string === undefined ? undefined : fn.localTypes.at(string.ordinal)
        const type = fn.type(expression.type)
        if (string === undefined || stringType?._tag !== 'String' || type?._tag !== 'Slice')
          return undefined
        const destination = fn.alloc(type)
        fn.emit(
          Object.freeze({
            _tag: 'StringUtf8Bytes',
            destination,
            string,
            heldLoans: expression.heldLoans,
            type,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'StringByteLength') {
        const [string] = argumentLocals
        const stringType = string === undefined ? undefined : fn.localTypes.at(string.ordinal)
        if (string === undefined || stringType?._tag !== 'String') return undefined
        const destination = fn.alloc(usize)
        fn.emit(
          Object.freeze({
            _tag: 'StringByteLength',
            destination,
            string,
            type: usize,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'StringEqualsExact') return undefined
      const conversionTarget = Scalar.conversionTarget(expression.operation)
      if (Scalar.isCheckedOperation(expression.operation)) {
        const [first] = argumentLocals
        const sourceType = first === undefined ? undefined : fn.localTypes.at(first.ordinal)
        const semanticSource = sourceType === undefined ? undefined : Mir.semanticType(sourceType)
        const sourceScalar =
          typeof semanticSource === 'string' ? Scalar.find(semanticSource) : undefined
        const valueScalar = conversionTarget ?? sourceScalar
        const targetType = fn.type(expression.type)
        if (
          first === undefined ||
          sourceScalar?.category !== 'Integer' ||
          valueScalar?.category !== 'Integer' ||
          sourceType?._tag !== sourceScalar.spelling ||
          targetType?._tag !== 'Union' ||
          argumentLocals.some((local) => fn.localTypes.at(local.ordinal)?._tag !== sourceType._tag)
        )
          return undefined
        const success = Type.some(valueScalar.spelling)
        const failure = Type.none
        if (
          !targetType.type.members.some((member) => Type.equals(member, success)) ||
          !targetType.type.members.some((member) => Type.equals(member, failure))
        )
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'CheckedInteger' as const,
            operation: expression.operation,
            destination,
            operands: Object.freeze(argumentLocals),
            sourceType,
            valueType: Object.freeze({ _tag: valueScalar.spelling }),
            type: targetType,
            success,
            failure,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (conversionTarget !== undefined) {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const semanticSource = sourceType === undefined ? undefined : Mir.semanticType(sourceType)
        const sourceScalar =
          typeof semanticSource === 'string' ? Scalar.find(semanticSource) : undefined
        const targetType = fn.type(expression.type)
        if (
          source === undefined ||
          (sourceScalar?.category !== 'Integer' && sourceScalar?.category !== 'Floating') ||
          sourceType?._tag !== sourceScalar.spelling ||
          targetType?._tag !== conversionTarget.spelling
        )
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag:
              sourceScalar.category === 'Integer'
                ? ('ConvertInteger' as const)
                : ('ConvertScalar' as const),
            destination,
            source,
            sourceType,
            type: targetType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      const floatConversionTarget = Scalar.floatConversionTarget(expression.operation)
      if (floatConversionTarget !== undefined) {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const semanticSource = sourceType === undefined ? undefined : Mir.semanticType(sourceType)
        const sourceScalar =
          typeof semanticSource === 'string' ? Scalar.find(semanticSource) : undefined
        const targetType = fn.type(expression.type)
        if (
          source === undefined ||
          sourceScalar === undefined ||
          sourceScalar.category === 'Boolean' ||
          sourceType?._tag !== sourceScalar.spelling ||
          targetType?._tag !== floatConversionTarget.spelling
        )
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'ConvertScalar' as const,
            destination,
            source,
            sourceType,
            type: targetType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'ToBits' || expression.operation === 'FromBits') {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const targetType = fn.type(expression.type)
        if (source === undefined || sourceType === undefined || targetType === undefined)
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'ReinterpretScalar' as const,
            destination,
            source,
            sourceType: sourceType as Mir.ScalarType,
            type: targetType as Mir.ScalarType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (expression.operation === 'Sin' || expression.operation === 'Cos') {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const targetType = fn.type(expression.type)
        if (source === undefined || sourceType === undefined || targetType === undefined)
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'FloatTranscendental' as const,
            operation: expression.operation,
            destination,
            source,
            sourceType: sourceType as Mir.ScalarType,
            type: targetType as Mir.ScalarType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (
        expression.operation === 'IsNaN' ||
        expression.operation === 'IsInfinite' ||
        expression.operation === 'IsFinite' ||
        expression.operation === 'IsNormal' ||
        expression.operation === 'IsSubnormal' ||
        expression.operation === 'IsSignNegative' ||
        expression.operation === 'Sqrt' ||
        (expression.operation === 'Negate' &&
          argumentLocals.some((local) => {
            const type = fn.localTypes.at(local.ordinal)
            const semantic = type === undefined ? undefined : Mir.semanticType(type)
            return typeof semantic === 'string' && Scalar.find(semantic)?.category === 'Floating'
          }))
      ) {
        const [source] = argumentLocals
        const sourceType = source === undefined ? undefined : fn.localTypes.at(source.ordinal)
        const targetType = fn.type(expression.type)
        if (source === undefined || sourceType === undefined || targetType === undefined)
          return undefined
        const destination = fn.alloc(targetType)
        fn.emit(
          Object.freeze({
            _tag: 'FloatUnary' as const,
            operation: expression.operation,
            destination,
            source,
            sourceType: sourceType as Mir.ScalarType,
            type: targetType as Mir.ScalarType,
            provenance: authored(expression.span),
          }),
        )
        return finishBuiltin(destination)
      }
      if (
        expression.operation === 'Not' ||
        expression.operation === 'Negate' ||
        expression.operation === 'BitNot' ||
        expression.operation === 'WrappingNegate' ||
        expression.operation === 'SaturatingNegate'
      ) {
        const [subject] = argumentLocals
        if (subject === undefined) return undefined
        const operandType = fn.localTypes.at(subject.ordinal)
        if (operandType === undefined) return undefined
        const semanticOperand = Mir.semanticType(operandType)
        const scalar =
          typeof semanticOperand === 'string' ? Scalar.find(semanticOperand) : undefined
        if (expression.operation !== 'Not' && scalar?.category !== 'Integer') return undefined
        const pointerBits = fn.layout.target.pointerSize === 4 ? 32 : 64
        const constant =
          expression.operation === 'BitNot' && scalar?.category === 'Integer'
            ? scalar.signedness === 'Signed'
              ? -1n
              : Scalar.range(scalar, pointerBits).maximum
            : 0n
        const zero = fn.alloc(operandType)
        fn.emit(
          Object.freeze({
            _tag: 'Literal',
            destination: zero,
            type: operandType,
            value: constant,
            provenance: Object.freeze({ span: expression.span, generated: true }),
          }),
        )
        const destination = fn.alloc(operandType)
        fn.emit(
          Object.freeze({
            _tag: 'Binary',
            operator:
              expression.operation === 'Not'
                ? 'Equals'
                : expression.operation === 'BitNot'
                  ? 'BitXor'
                  : expression.operation === 'WrappingNegate'
                    ? 'WrappingSubtract'
                    : expression.operation === 'SaturatingNegate'
                      ? 'SaturatingSubtract'
                      : 'Subtract',
            destination,
            left: expression.operation === 'Not' ? subject : zero,
            right: expression.operation === 'Not' ? zero : subject,
            type: operandType,
            provenance: Object.freeze({ span: expression.span, generated: false }),
          }),
        )
        return { result: destination }
      }
      if (!Mir.isBinaryOperator(expression.operation)) return undefined
      const [left, right] = argumentLocals
      if (left === undefined || right === undefined) return undefined
      const type = fn.type(expression.type)
      if (type === undefined) return undefined
      const destination = fn.alloc(type)
      fn.emit(
        Object.freeze({
          _tag: 'Binary',
          operator: expression.operation,
          destination,
          left,
          right,
          type,
          provenance: Object.freeze({ span: expression.span, generated: false }),
        }),
      )
      return { result: destination }
    }
    case 'Unavailable':
      return undefined
  }
}

interface ExitIndex {
  readonly returns: ReadonlyMap<string, Ownership.ExitPlan>
  readonly scopeEnds: ReadonlyMap<string, Ownership.ExitPlan>
  readonly armEnds: ReadonlyMap<string, Ownership.ExitPlan>
  readonly loopFallthroughs: ReadonlyMap<number, Ownership.ExitPlan>
  readonly transfers: ReadonlyMap<string, Ownership.ExitPlan>
}

const indexExits = (plan: Ownership.FunctionOwnership | undefined): ExitIndex => {
  const returns = new Map<string, Ownership.ExitPlan>()
  const scopeEnds = new Map<string, Ownership.ExitPlan>()
  const armEnds = new Map<string, Ownership.ExitPlan>()
  const loopFallthroughs = new Map<number, Ownership.ExitPlan>()
  const transfers = new Map<string, Ownership.ExitPlan>()
  for (const exit of plan?.exits ?? []) {
    switch (exit.kind) {
      case 'Return':
        returns.set(spanKey(exit.span), exit)
        break
      case 'ScopeEnd':
        scopeEnds.set(spanKey(exit.span), exit)
        break
      case 'ArmEnd':
        armEnds.set(`${spanKey(exit.span)}:${exit.arm ?? 'Taken'}`, exit)
        break
      case 'LoopFallthrough':
        if (exit.target !== undefined) loopFallthroughs.set(exit.target.ordinal, exit)
        break
      case 'Break':
      case 'Continue':
        transfers.set(spanKey(exit.span), exit)
        break
    }
  }
  return { returns, scopeEnds, armEnds, loopFallthroughs, transfers }
}

const concreteCleanup = (
  fn: FunctionLowering,
  type: Type.Type,
  seen = new Set<string>(),
): Ownership.CleanupPlan => Ownership.cleanupPlan(fn.index, type, seen)

const specializedCleanup = (
  fn: FunctionLowering,
  cleanup: Ownership.CleanupPlan,
): Ownership.CleanupPlan =>
  Ownership.specializeCleanup(cleanup, fn.substitution, (type) => concreteCleanup(fn, type))

const cleanupForLocal = (
  fn: FunctionLowering,
  cleanup: Ownership.CleanupPlan,
  localType: Mir.Type,
): Ownership.CleanupPlan => {
  const specialized = specializedCleanup(fn, cleanup)
  if (localType._tag === 'EffectValue') {
    const cleanupForEnvironment = (
      effectValue: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
      seen: ReadonlySet<string>,
    ): Ownership.CleanupPlan => {
      const identity = Instances.effectIdentity(effectValue.environment.instance, effectValue.site)
      if (seen.has(identity)) return Object.freeze({ _tag: 'NoCleanup', type: effectValue.type })
      const next = new Set(seen).add(identity)
      let laneOffset = 0
      const slots = effectValue.environment.fields
        .flatMap((field, ordinal) => {
          const nested =
            field.effectIdentity === undefined
              ? undefined
              : effectValueByIdentity(fn.layout, field.effectIdentity)
          const callable =
            field.callableIdentity === undefined || !Type.isCallable(field.type)
              ? undefined
              : callableValueByIdentity(fn.layout, field.callableIdentity, field.type)
          const laneCount =
            field.representation === 'Borrow'
              ? 1
              : callable === undefined
                ? nested === undefined
                  ? (Layout.callingShape(fn.layout, field.type)?.laneCount ?? 0)
                  : Layout.effectEnvironmentLanes(fn.layout, nested.environment).length
                : callable.environment === undefined
                  ? 0
                  : Layout.callableEnvironmentLanes(fn.layout, callable.environment).length
          const currentOffset = laneOffset
          laneOffset += laneCount
          if (field.representation === 'Borrow') return []
          const fieldCleanup =
            callable === undefined
              ? nested === undefined
                ? concreteCleanup(fn, field.type)
                : cleanupForEnvironment(nested, next)
              : callableLocalCleanup(fn, callable)
          return fieldCleanup._tag === 'NoCleanup'
            ? []
            : [
                Object.freeze({
                  ordinal,
                  laneOffset: currentOffset,
                  laneCount,
                  cleanup: fieldCleanup,
                }),
              ]
        })
        .reverse()
      return slots.length === 0
        ? Object.freeze({ _tag: 'NoCleanup', type: effectValue.type })
        : Object.freeze({
            _tag: 'EffectCleanup',
            type: effectValue.type,
            site: effectValue.site,
            slots: Object.freeze(slots),
          })
    }
    return cleanupForEnvironment(localType, new Set())
  }
  if (specialized._tag !== 'CallableCleanup' || localType._tag !== 'CallableValue') {
    return specialized
  }
  const fields = localType.environment?.fields ?? []
  return Object.freeze({
    _tag: 'CallableCleanup',
    type: localType.type,
    site: specialized.site,
    slots: Object.freeze(
      specialized.slots.flatMap((slot) => {
        const field = fields.find((candidate) => candidate.ordinal === slot.ordinal)
        return field === undefined
          ? []
          : [Object.freeze({ ordinal: slot.ordinal, cleanup: concreteCleanup(fn, field.type) })]
      }),
    ),
  })
}

/**
 * The Drop operations a propagating failure must execute before it leaves this function:
 * every owner the ownership phase saw live at the run site, resolved to this function's
 * locals. Sites without a local here belong to a different compiled body and are skipped.
 */
const propagationReleases = (
  fn: FunctionLowering,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Mir.DropOperation> => {
  const exit = fn.ownership?.exits.find(
    (candidate) =>
      candidate.kind === 'Propagation' &&
      candidate.span.start === span.start &&
      candidate.span.end === span.end,
  )
  if (exit === undefined) return Object.freeze([])
  return Object.freeze(
    exit.releases.flatMap((release): ReadonlyArray<Mir.DropOperation> => {
      if (release.cleanup._tag === 'NoCleanup') return []
      const site = release.binding.site
      const local =
        site._tag === 'Let'
          ? fn.bindingLocals.get(site.binding.ordinal)
          : site._tag === 'Parameter'
            ? fn.parameterLocals.get(site.parameter.ordinal)
            : undefined
      const localType = local === undefined ? undefined : fn.localTypes.at(local.ordinal)
      if (local === undefined || localType === undefined) return []
      return [
        Object.freeze({
          _tag: 'Drop' as const,
          local,
          cleanup: cleanupForLocal(fn, release.binding.cleanup, localType),
          provenance: generated(span),
        }),
      ]
    }),
  )
}

const callableLocalCleanup = (
  fn: FunctionLowering,
  localType: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>,
): Ownership.CleanupPlan => {
  const environment = localType.environment
  if (environment === undefined || localType.site === undefined)
    return Object.freeze({ _tag: 'NoCleanup', type: localType.type })
  return Object.freeze({
    _tag: 'CallableCleanup',
    type: localType.type,
    site: localType.site,
    slots: Object.freeze(
      [...environment.fields]
        .reverse()
        .flatMap((field) =>
          field.access === 'Take' && !copyType(field.type)
            ? [Object.freeze({ ordinal: field.ordinal, cleanup: concreteCleanup(fn, field.type) })]
            : [],
        ),
    ),
  })
}

const emitReleases = (fn: FunctionLowering, exit: Ownership.ExitPlan | undefined): void => {
  for (const borrow of exit?.loanEnds ?? []) {
    const slice = fn.loanLocals.get(borrowKey(borrow))
    if (slice === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'EndLoan',
        borrow,
        slice,
        provenance: generated(exit?.span ?? borrow.callSpan),
      }),
    )
    fn.loanLocals.delete(borrowKey(borrow))
  }
  for (const release of exit?.releases ?? []) {
    if (release.binding.site._tag !== 'Let') continue
    const ordinal = release.binding.site.binding.ordinal
    endLoans(fn, fn.effectLoanEnds.get(ordinal) ?? [], exit?.span ?? release.binding.liveTo)
    fn.effectLoanEnds.delete(ordinal)
  }
  for (const release of exit?.releases ?? []) {
    const site = release.binding.site
    const dropped =
      site._tag === 'Parameter'
        ? fn.parameterLocals.get(site.parameter.ordinal)
        : fn.bindingLocals.get(site.binding.ordinal)
    if (dropped === undefined) continue
    const localType = fn.localTypes.at(dropped.ordinal)
    if (localType === undefined) continue
    fn.emit(
      Object.freeze({
        _tag: 'Drop',
        local: dropped,
        cleanup: cleanupForLocal(fn, release.cleanup, localType),
        provenance: Object.freeze({ span: release.binding.liveFrom, generated: true }),
      }),
    )
  }
}

const copyType = (type: Type.Type): boolean =>
  Type.isBuiltin(type) || (Type.isFixedArray(type) && copyType(type.element))

const ownerFields = (ownerLoop: Mir.LoopId | undefined): { readonly ownerLoop?: Mir.LoopId } =>
  ownerLoop === undefined ? {} : { ownerLoop }

const generated = (span: SourceSpan.SourceSpan): Mir.Provenance =>
  Object.freeze({ span, generated: true })

const authored = (span: SourceSpan.SourceSpan): Mir.Provenance =>
  Object.freeze({ span, generated: false })

const lowerWriteSelectors = (
  fn: FunctionLowering,
  selectors: ReadonlyArray<Hir.WriteSelector>,
): ReadonlyArray<Mir.PlaceSelector> | undefined => {
  const lowered: Array<Mir.PlaceSelector> = []
  for (const selector of selectors) {
    if (selector._tag === 'Field') {
      lowered.push(
        Object.freeze({
          _tag: 'FieldSelector',
          field: selector.field,
          provenance: authored(selector.span),
        }),
      )
      continue
    }
    const index =
      selector.bounds._tag === 'Proven'
        ? Object.freeze({ _tag: 'Proven' as const, value: selector.bounds.index })
        : (() => {
            const expression = lowerExpression(fn, selector.index)
            return expression === undefined
              ? undefined
              : Object.freeze({ _tag: 'Runtime' as const, local: expression.result })
          })()
    if (index === undefined) return undefined
    lowered.push(
      Object.freeze({
        _tag: 'ElementSelector',
        length: selector.array.length,
        index,
        provenance: authored(selector.span),
      }),
    )
  }
  return Object.freeze(lowered)
}

const lowerBorrowedWriteSelectors = (
  fn: FunctionLowering,
  selectors: ReadonlyArray<Hir.BorrowedWriteSelector>,
): ReadonlyArray<Mir.PlaceSelector> | undefined => {
  const lowered: Array<Mir.PlaceSelector> = []
  for (const selector of selectors) {
    if (selector._tag === 'Field') {
      lowered.push(
        Object.freeze({
          _tag: 'FieldSelector',
          field: selector.field,
          provenance: authored(selector.span),
        }),
      )
      continue
    }
    const index = lowerExpression(fn, selector.index)
    if (index === undefined) return undefined
    lowered.push(
      Object.freeze({
        _tag: 'SliceElementSelector',
        index: index.result,
        access: selector.slice.access,
        provenance: authored(selector.span),
      }),
    )
  }
  return Object.freeze(lowered)
}

const lowerSequence = (
  fn: FunctionLowering,
  statements: ReadonlyArray<Hir.Statement>,
  exits: ExitIndex,
  ownerLoop: Mir.LoopId | undefined,
  terminal: Mir.Outcome,
  reserved?: Mir.RegionId,
  armExit?: Ownership.ExitPlan,
): Mir.RegionId | undefined => {
  const id = reserved ?? fn.reserve()
  const [statement, ...rest] = statements
  if (statement === undefined) {
    const [, releases] = fn.capture(() => emitReleases(fn, armExit))
    if (releases.length > 0) {
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome: terminal,
        }),
      )
    } else {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: terminal,
        }),
      )
    }
    return id
  }

  if (statement._tag === 'UnavailableStatement') {
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations: Object.freeze([]),
        outcome: Object.freeze({
          _tag: 'Trap',
          reason: 'unavailable statement',
          provenance: generated(statement.span),
        }),
      }),
    )
    return id
  }

  if (statement._tag === 'Bind') {
    if (
      inlineForwardedRequirement(fn, statement.initializer) !== undefined ||
      statement.initializer._tag === 'ServiceEffectConstruct' ||
      (statement.initializer._tag === 'EffectConstruct' &&
        fn.call(statement.initializer.span)?.resultEffect === undefined &&
        fn.effectResults.get(
          instanceText(
            statement.initializer.target,
            statement.initializer.typeArguments.map((argument) => fn.semanticArgument(argument)),
          ),
        ) === undefined) ||
      statement.initializer._tag === 'EffectResult' ||
      statement.initializer._tag === 'EffectBindRequirement' ||
      (statement.initializer._tag === 'BuiltinCall' && Type.isEffect(statement.initializer.type))
    ) {
      fn.effectRecipes.set(statement.binding.ordinal, statement.initializer)
      const following = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: following,
            provenance: generated(statement.span),
          }),
        }),
      )
      return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
        ? undefined
        : id
    }
    const [initializer, operations] = fn.capture(() => {
      const lowered = lowerExpression(fn, statement.initializer)
      if (lowered === undefined) return undefined
      const destination = fn.alloc(fn.localTypes.at(lowered.result.ordinal) ?? i32)
      fn.emit(
        Object.freeze({
          _tag: 'Move',
          destination,
          source: lowered.result,
          provenance: authored(statement.span),
        }),
      )
      const heldLoans = fn.slotLoans.get(lowered.result.ordinal)
      if (heldLoans !== undefined) {
        fn.slotLoans.delete(lowered.result.ordinal)
        fn.slotLoans.set(destination.ordinal, heldLoans)
      }
      fn.bindingLocals.set(statement.binding.ordinal, destination)
      const destinationType = fn.localTypes.at(destination.ordinal)
      if (destinationType?._tag === 'EffectValue') {
        const retained = retainedEffectLoans(fn, statement.initializer)
        if (retained.length > 0) fn.effectLoanEnds.set(statement.binding.ordinal, retained)
      }
      return destination
    })
    if (initializer === undefined) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Evaluate') {
    const [evaluated, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (evaluated === undefined) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Write') {
    const place = statement.place
    const root =
      place._tag === 'BorrowedWritePlace'
        ? borrowedWriteRoot(fn, place.root)
        : fn.bindingLocals.get(place.root.ordinal)
    const rootType = root === undefined ? undefined : fn.localTypes.at(root.ordinal)
    const type = fn.type(place.type)
    const [written, operations] = fn.capture(() => {
      if (root === undefined || rootType === undefined || type === undefined) return false
      const selectors =
        place._tag === 'BorrowedWritePlace'
          ? lowerBorrowedWriteSelectors(fn, place.selectors)
          : lowerWriteSelectors(fn, place.selectors)
      if (selectors === undefined) return false
      fn.emit(
        Object.freeze({
          _tag: 'CheckPlace',
          root,
          selectors,
          type,
          provenance: authored(place.span),
        }),
      )
      const value = lowerExpression(fn, statement.value)
      if (value === undefined) return false
      fn.emit(
        Object.freeze({
          _tag: 'WritePlace',
          root,
          selectors,
          source: value.result,
          rootType,
          type,
          mutable: true,
          replacement: copyType(fn.semantic(statement.place.type)) ? 'Copy' : 'Owned',
          commit: 'AfterCleanup',
          provenance: authored(statement.span),
        }),
      )
      endReturnedViewLoans(fn, statement.span)
      return true
    })
    if (!written) return undefined
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Drop') {
    const droppedExpression =
      statement.expression._tag === 'Move' ? statement.expression.subject : statement.expression
    const [lowered, operations] = fn.capture(() => lowerExpression(fn, statement.expression))
    if (lowered === undefined) return undefined
    const localType = fn.localTypes.at(lowered.result.ordinal)
    if (localType === undefined) return undefined
    const droppedBinding =
      droppedExpression._tag === 'BindingReference' ? droppedExpression.binding.ordinal : undefined
    const bindingFact =
      droppedBinding !== undefined
        ? Ownership.allBindings(fn.ownership).find(
            (binding) =>
              binding.site._tag === 'Let' && binding.site.binding.ordinal === droppedBinding,
          )
        : undefined
    const ownershipLoanReleases = (fn.ownership?.loans ?? []).flatMap((loan) => {
      if (loan.endSpan.start !== statement.span.start || loan.endSpan.end !== statement.span.end) {
        return []
      }
      const slice = fn.loanLocals.get(borrowKey(loan.id))
      if (slice === undefined) return []
      fn.loanLocals.delete(borrowKey(loan.id))
      return [
        Object.freeze({
          _tag: 'EndLoan' as const,
          borrow: loan.id,
          slice,
          provenance: generated(statement.span),
        }),
      ]
    })
    const retainedLoanReleases = (
      droppedBinding === undefined ? [] : (fn.effectLoanEnds.get(droppedBinding) ?? [])
    ).flatMap((borrow) => {
      const slice = fn.loanLocals.get(borrowKey(borrow))
      if (slice === undefined) return []
      fn.loanLocals.delete(borrowKey(borrow))
      return [
        Object.freeze({
          _tag: 'EndLoan' as const,
          borrow,
          slice,
          provenance: generated(statement.span),
        }),
      ]
    })
    if (droppedBinding !== undefined) fn.effectLoanEnds.delete(droppedBinding)
    const loanReleases = [...ownershipLoanReleases, ...retainedLoanReleases].filter(
      (release, ordinal, releases) =>
        releases.findIndex(
          (candidate) => borrowKey(candidate.borrow) === borrowKey(release.borrow),
        ) === ordinal,
    )
    const cleanup = fn.reserve()
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: cleanup,
          provenance: generated(statement.span),
        }),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'CleanupRegion',
        id: cleanup,
        ...ownerFields(ownerLoop),
        releases: Object.freeze([
          ...loanReleases,
          Object.freeze({
            _tag: 'Drop',
            local: lowered.result,
            cleanup: cleanupForLocal(
              fn,
              bindingFact === undefined
                ? concreteCleanup(fn, Mir.semanticType(localType))
                : bindingFact.cleanup,
              localType,
            ),
            provenance: authored(statement.span),
          }),
        ]),
        outcome: Object.freeze({
          _tag: 'Forward',
          target: following,
          provenance: generated(statement.span),
        }),
      }),
    )
    return lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
      ? undefined
      : id
  }

  if (statement._tag === 'Unsafe') {
    const body = fn.reserve()
    const following = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations: Object.freeze([]),
        outcome: Object.freeze({
          _tag: 'Forward',
          target: body,
          provenance: authored(statement.span),
        }),
      }),
    )
    const forward = Object.freeze({
      _tag: 'Forward' as const,
      target: following,
      provenance: generated(statement.span),
    })
    if (
      lowerSequence(
        fn,
        statement.statements,
        exits,
        ownerLoop,
        forward,
        body,
        exits.scopeEnds.get(spanKey(statement.span)),
      ) === undefined ||
      lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
    )
      return undefined
    return id
  }

  if (statement._tag === 'If') {
    const conditional = fn.reserve()
    const taken = fn.reserve()
    const otherwise = fn.reserve()
    const following = fn.reserve()
    const [condition, operations] = fn.capture(() => lowerExpression(fn, statement.condition))
    if (condition === undefined) return undefined
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: conditional,
          provenance: generated(statement.span),
        }),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'ConditionalRegion',
        id: conditional,
        ...ownerFields(ownerLoop),
        condition: condition.result,
        taken,
        otherwise,
        following,
        provenance: authored(statement.span),
      }),
    )
    const forward = Object.freeze({
      _tag: 'Forward' as const,
      target: following,
      provenance: generated(statement.span),
    })
    if (
      lowerSequence(
        fn,
        statement.taken,
        exits,
        ownerLoop,
        forward,
        taken,
        exits.armEnds.get(`${spanKey(statement.span)}:Taken`),
      ) === undefined ||
      lowerSequence(
        fn,
        statement.otherwise,
        exits,
        ownerLoop,
        forward,
        otherwise,
        exits.armEnds.get(`${spanKey(statement.span)}:Otherwise`),
      ) === undefined ||
      lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
    ) {
      return undefined
    }
    return id
  }

  if (statement._tag === 'While') {
    const loop: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: statement.loop.ordinal })
    const conditionId = fn.reserve()
    const bodyId = fn.reserve()
    const following = fn.reserve()
    const [condition, conditionOperations] = fn.capture(() =>
      lowerExpression(fn, statement.condition),
    )
    if (condition === undefined) return undefined
    fn.publish(
      Object.freeze({
        _tag: 'LoopRegion',
        id,
        ...ownerFields(ownerLoop),
        loop,
        ...(ownerLoop === undefined ? {} : { parent: ownerLoop }),
        condition: conditionId,
        conditionValue: condition.result,
        body: bodyId,
        following,
        provenance: authored(statement.span),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id: conditionId,
        ownerLoop: loop,
        operations: conditionOperations,
        outcome: Object.freeze({ _tag: 'Yield', provenance: generated(statement.span) }),
      }),
    )
    const repeat = Object.freeze({
      _tag: 'Repeat' as const,
      loop,
      provenance: generated(statement.span),
    })
    if (
      lowerSequence(
        fn,
        statement.body,
        exits,
        loop,
        repeat,
        bodyId,
        exits.loopFallthroughs.get(statement.loop.ordinal),
      ) === undefined ||
      lowerSequence(fn, rest, exits, ownerLoop, terminal, following, armExit) === undefined
    ) {
      return undefined
    }
    return id
  }

  if (statement._tag === 'Break' || statement._tag === 'Continue') {
    const target: Mir.LoopId = Object.freeze({ _tag: 'Loop', ordinal: statement.target.ordinal })
    const outcome: Mir.Outcome = Object.freeze({
      _tag: statement._tag === 'Break' ? ('Exit' as const) : ('Repeat' as const),
      loop: target,
      provenance: authored(statement.span),
    })
    const [, releases] = fn.capture(() =>
      emitReleases(fn, exits.transfers.get(spanKey(statement.span))),
    )
    if (releases.length === 0) {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome,
        }),
      )
    } else {
      const cleanup = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Forward',
            target: cleanup,
            provenance: generated(statement.span),
          }),
        }),
      )
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id: cleanup,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome,
        }),
      )
    }
    return id
  }

  if (statement._tag === 'Fail') {
    const specializedFailure = fn.semantic(statement.failure)
    if (Type.isNever(specializedFailure)) {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations: Object.freeze([]),
          outcome: Object.freeze({
            _tag: 'Trap',
            reason: 'unreachable failure of never',
            provenance: generated(statement.span),
          }),
        }),
      )
      return id
    }
    const [failedValue, operations] = fn.capture(() => {
      const failed = lowerExpression(fn, statement.expression)
      const outcomeType = fn.effectOutcome === undefined ? undefined : fn.type(fn.effectOutcome)
      if (
        failed === undefined ||
        outcomeType?._tag !== 'EffectOutcome' ||
        (!Type.isNominal(specializedFailure) && !Type.isUnion(specializedFailure))
      )
        return undefined
      const destination = fn.alloc(outcomeType)
      if (Type.isNominal(specializedFailure)) {
        const tag = outcomeType.type.failures.findIndex((failure) =>
          Type.equals(failure, specializedFailure),
        )
        if (tag < 0) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'PackEffectOutcome' as const,
            destination,
            source: failed.result,
            tag: tag + 1,
            type: outcomeType,
            provenance: authored(statement.span),
          }),
        )
      } else {
        const sourceType = fn.type(specializedFailure)
        if (sourceType?._tag !== 'Union') return undefined
        const mappings = specializedFailure.members.flatMap((member, source) => {
          const target = outcomeType.type.failures.findIndex((failure) =>
            Type.equals(failure, member),
          )
          return target < 0 ? [] : [Object.freeze({ source, target: target + 1 })]
        })
        if (mappings.length !== specializedFailure.members.length) return undefined
        fn.emit(
          Object.freeze({
            _tag: 'PackEffectFailureUnion' as const,
            destination,
            source: failed.result,
            sourceType,
            mappings: Object.freeze(mappings),
            type: outcomeType,
            provenance: authored(statement.span),
          }),
        )
      }
      return destination
    })
    if (failedValue === undefined) return undefined
    const failureOutcome: Mir.Outcome = Object.freeze({
      _tag: 'Return',
      value: failedValue,
      provenance: authored(statement.span),
    })
    const [, releases] = fn.capture(() =>
      emitReleases(fn, exits.returns.get(spanKey(statement.span))),
    )
    if (releases.length === 0) {
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations,
          outcome: failureOutcome,
        }),
      )
    } else {
      const cleanup = fn.reserve()
      fn.publish(
        Object.freeze({
          _tag: 'OperationRegion',
          id,
          ...ownerFields(ownerLoop),
          operations,
          outcome: Object.freeze({
            _tag: 'Forward',
            target: cleanup,
            provenance: generated(statement.span),
          }),
        }),
      )
      fn.publish(
        Object.freeze({
          _tag: 'CleanupRegion',
          id: cleanup,
          ...ownerFields(ownerLoop),
          releases: Object.freeze(
            releases.flatMap((operation) =>
              operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
            ),
          ),
          outcome: failureOutcome,
        }),
      )
    }
    return id
  }

  const [returnedValue, operations] = fn.capture(() => {
    const returned = lowerExpression(fn, statement.expression)
    if (returned === undefined) return undefined
    if (fn.effectOutcome === undefined) return returned.result
    const outcomeType = fn.type(fn.effectOutcome)
    if (outcomeType?._tag !== 'EffectOutcome') return undefined
    const destination = fn.alloc(outcomeType)
    fn.emit(
      Object.freeze({
        _tag: 'PackEffectOutcome',
        destination,
        source: returned.result,
        tag: 0,
        type: outcomeType,
        provenance: authored(statement.span),
      }),
    )
    return destination
  })
  if (returnedValue === undefined) return undefined
  const returnOutcome: Mir.Outcome = Object.freeze({
    _tag: 'Return',
    value: returnedValue,
    provenance: authored(statement.span),
  })
  const [, releases] = fn.capture(() =>
    emitReleases(fn, exits.returns.get(spanKey(statement.span))),
  )
  if (releases.length === 0) {
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: returnOutcome,
      }),
    )
  } else {
    const cleanup = fn.reserve()
    fn.publish(
      Object.freeze({
        _tag: 'OperationRegion',
        id,
        ...ownerFields(ownerLoop),
        operations,
        outcome: Object.freeze({
          _tag: 'Forward',
          target: cleanup,
          provenance: generated(statement.span),
        }),
      }),
    )
    fn.publish(
      Object.freeze({
        _tag: 'CleanupRegion',
        id: cleanup,
        ...ownerFields(ownerLoop),
        releases: Object.freeze(
          releases.flatMap((operation) =>
            operation._tag === 'Drop' || operation._tag === 'EndLoan' ? [operation] : [],
          ),
        ),
        outcome: returnOutcome,
      }),
    )
  }
  return id
}

const trapFunction = (
  instance: Instances.Instance,
  reason: string,
  span: SourceSpan.SourceSpan,
): Mir.MirFunction => {
  const parameterCount = instance.function.declaration.parameterCount
  return Object.freeze({
    _tag: 'MirFunction',
    id: instance.key.declaration,
    instance: instance.key,
    parameterCount,
    localTypes: Object.freeze(Array.from({ length: parameterCount }, () => i32)),
    result: i32,
    entry: Object.freeze({ _tag: 'Region', ordinal: 0 }),
    regions: Object.freeze([
      Object.freeze({
        _tag: 'OperationRegion' as const,
        id: Object.freeze({ _tag: 'Region' as const, ordinal: 0 }),
        operations: Object.freeze([]),
        outcome: Object.freeze({
          _tag: 'Trap' as const,
          reason,
          provenance: Object.freeze({ span, generated: true }),
        }),
      }),
    ]),
  })
}

const planFor = (
  ownership: Ownership.ModuleOwnership | undefined,
  fn: Hir.HirFunction,
): Ownership.FunctionOwnership | undefined =>
  ownership?.functions.find(
    (candidate) => candidate.declaration.id.ordinal === fn.declaration.id.ordinal,
  )

const bodySpan = (fn: Hir.HirFunction): SourceSpan.SourceSpan =>
  fn.statements.at(-1)?.span ?? fn.declaration.syntax.span

const returnedEffectBlock = (
  fn: Hir.HirFunction,
): Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }> | undefined => {
  const terminal = fn.statements.at(-1)
  if (terminal?._tag !== 'Return') return undefined
  const returned = terminal.expression
  if (returned._tag === 'EffectBlock') return returned
  if (returned._tag !== 'BindingReference') return undefined
  const binding = fn.statements.find(
    (statement): statement is Extract<Hir.Statement, { readonly _tag: 'Bind' }> =>
      statement._tag === 'Bind' && statement.binding.ordinal === returned.binding.ordinal,
  )
  return binding?.initializer._tag === 'EffectBlock' ? binding.initializer : undefined
}

const lowerInstance = (
  instance: Instances.Instance,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>,
  generatedRunners: Array<GeneratedEffectRunner>,
): Mir.MirFunction => {
  const fn = instance.function
  const plan = planFor(ownership, fn)

  if (plan !== undefined && plan.verdict._tag === 'Violation') {
    return trapFunction(instance, 'ownership violation', plan.verdict.cause.span)
  }

  const contract = fn.contract
  const parameterTypes =
    contract._tag === 'Contract'
      ? contract.parameters.flatMap((type, ordinal) => {
          if (Type.isEffect(Type.substitute(type, instance.substitution))) {
            const identity = Instances.parameterEffectIdentity(fn, instance.key, ordinal)
            const effectValue =
              identity === undefined ? undefined : effectValueByIdentity(layout, identity)
            return effectValue === undefined ? [] : [effectValue]
          }
          const specialized = Type.substitute(type, instance.substitution)
          if (Type.isCallable(specialized)) {
            const identity = Instances.parameterCallableIdentity(fn, instance.key, ordinal)
            const callable =
              identity === undefined
                ? undefined
                : callableValueByIdentity(layout, identity, specialized)
            return callable === undefined ? [] : [callable]
          }
          const lowered = mirType(type, instance.substitution)
          return lowered === undefined ? [] : [lowered]
        })
      : Array.from({ length: fn.declaration.parameterCount }, () => i32)
  const effectOutcome =
    contract._tag === 'Contract' && contract.functionKind === 'Effect'
      ? Type.effect(
          Type.substitute(contract.result, instance.substitution),
          (contract.failures ?? []).flatMap((failure) => {
            const specialized = Type.substitute(failure, instance.substitution)
            return Type.isNominal(specialized) ? [specialized] : []
          }),
          'Shared',
          (contract.requirements ?? []).flatMap((requirement) => {
            const capability = Type.substitute(requirement.capability, instance.substitution)
            return Type.isNominal(capability) ? [Object.freeze({ ...requirement, capability })] : []
          }),
        )
      : undefined
  const returnedBlock = contract._tag === 'Contract' ? returnedEffectBlock(fn) : undefined
  const hiddenEffectResult =
    returnedBlock === undefined ? undefined : effectValueType(layout, instance.key, returnedBlock)
  const specializedEffectResult =
    instance.resultEffect === undefined
      ? undefined
      : effectValueByIdentity(layout, instance.resultEffect)
  const resultType =
    specializedEffectResult ??
    hiddenEffectResult ??
    (contract._tag === 'Contract'
      ? mirType(effectOutcome ?? contract.result, instance.substitution)
      : i32)
  if (resultType === undefined) {
    return trapFunction(instance, 'unavailable contract type', bodySpan(fn))
  }

  const lowering = new FunctionLowering(
    layout,
    index,
    parameterTypes,
    plan,
    instance.substitution,
    effectOutcome,
    instance,
    instances,
    calls,
    effectResults,
    generatedRunners,
  )
  const terminal: Mir.Outcome = Object.freeze({
    _tag: 'Trap',
    reason: 'body fell through without return',
    provenance: generated(bodySpan(fn)),
  })
  const entry = lowerSequence(lowering, fn.statements, indexExits(plan), undefined, terminal)

  if (entry === undefined || lowering.regions.some((region) => region === undefined)) {
    const unavailable = Hir.firstUnavailable(fn)
    return trapFunction(instance, 'unavailable body', unavailable?.span ?? bodySpan(fn))
  }

  return Object.freeze({
    _tag: 'MirFunction',
    id: instance.key.declaration,
    instance: instance.key,
    parameterCount: fn.declaration.parameterCount,
    localTypes: Object.freeze([...lowering.localTypes]),
    result: resultType,
    entry,
    regions: Object.freeze(
      lowering.regions.flatMap((region) => (region === undefined ? [] : [region])),
    ),
  })
}

const lowerEffectRunner = (
  spec: GeneratedEffectRunner,
  ownership: Ownership.ModuleOwnership | undefined,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
  instances: ReadonlyArray<Instances.Instance>,
  calls: ReadonlyArray<Instances.CallInstance>,
  effectResults: ReadonlyMap<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>,
  generatedRunners: Array<GeneratedEffectRunner>,
): Mir.MirFunction | undefined => {
  const { owner, block, type } = spec
  const id = spec.id
  const instance: Instances.InstanceKey = Object.freeze({
    _tag: 'InstanceKey',
    declaration: id,
    typeArguments: owner.key.typeArguments,
    contractRow: Object.freeze([
      ...owner.key.contractRow,
      `effect-site:${Hir.executableSiteKey(block.site)}`,
    ]),
  })
  const captureParameterTypes = type.environment.fields.flatMap((field) => {
    if (field.effectIdentity !== undefined) {
      const effectValue = effectValueByIdentity(layout, field.effectIdentity)
      return effectValue === undefined ? [] : [effectValue]
    }
    if (field.callableIdentity !== undefined && Type.isCallable(field.type)) {
      const callable = callableValueByIdentity(layout, field.callableIdentity, field.type)
      return callable === undefined ? [] : [callable]
    }
    const lowered = mirType(field.type)
    if (lowered === undefined) return []
    if (field.representation === 'Value') return [lowered]
    if (field.access !== 'Shared' && field.access !== 'Exclusive') return []
    return [
      Object.freeze({ _tag: 'EffectBorrow' as const, type: field.type, access: field.access }),
    ]
  })
  if (captureParameterTypes.length !== block.captures.length) return undefined
  const parameterizedRequirements = spec.providedRequirements.filter(
    (requirement) => requirement.witness._tag === 'SourceConformanceWitness',
  )
  const requirementParameterTypes = parameterizedRequirements.flatMap((requirement) => {
    if (requirement.access === 'Take') return []
    const type = mirType(
      Object.freeze({
        _tag: 'ReferenceType' as const,
        access: requirement.access,
        target: requirement.providerType,
      }),
    )
    return type === undefined ? [] : [type]
  })
  if (requirementParameterTypes.length !== parameterizedRequirements.length) return undefined
  const parameterTypes = Object.freeze([...captureParameterTypes, ...requirementParameterTypes])
  const plan = planFor(ownership, owner.function)
  const lowering = new FunctionLowering(
    layout,
    index,
    parameterTypes,
    plan,
    owner.substitution,
    type.type,
    owner,
    instances,
    calls,
    effectResults,
    generatedRunners,
    Object.freeze(
      spec.providedRequirements.map((requirement) => {
        const ordinal = parameterizedRequirements.indexOf(requirement)
        return Object.freeze({
          ...requirement,
          ...(ordinal < 0 ? {} : { local: local(captureParameterTypes.length + ordinal) }),
        })
      }),
    ),
  )
  lowering.parameterLocals.clear()
  block.captures.forEach((capture, ordinal) => {
    const captureLocal = local(ordinal)
    if (capture.binding !== undefined)
      lowering.bindingLocals.set(capture.binding.ordinal, captureLocal)
    if (capture.parameter !== undefined)
      lowering.parameterLocals.set(capture.parameter.ordinal, captureLocal)
  })
  const terminal: Mir.Outcome = Object.freeze({
    _tag: 'Trap',
    reason: 'effect body fell through without return',
    provenance: generated(block.span),
  })
  const entry = lowerSequence(lowering, block.statements, indexExits(plan), undefined, terminal)
  if (entry === undefined || lowering.regions.some((region) => region === undefined)) {
    return undefined
  }
  const result: Extract<Mir.Type, { readonly _tag: 'EffectOutcome' }> = Object.freeze({
    _tag: 'EffectOutcome',
    type: type.type,
  })
  return Object.freeze({
    _tag: 'MirFunction',
    id,
    instance,
    parameterCount: parameterTypes.length,
    localTypes: Object.freeze([...lowering.localTypes]),
    result,
    entry,
    regions: Object.freeze(
      lowering.regions.flatMap((region) => (region === undefined ? [] : [region])),
    ),
  })
}

/** Lowers the discovered instances into one MIR program module in discovery order. */
export const lowerProgram = (
  discovery: Instances.Discovery,
  ownership: ReadonlyMap<string, Ownership.ModuleOwnership>,
  layout: Layout.Plan,
  index: DeclarationIndex.Index,
): Mir.Module => {
  const staticDataById = new Map<
    string,
    Extract<
      Hir.Expression,
      { readonly _tag: 'StaticStringLiteral' | 'StaticByteViewLiteral' }
    >['data']
  >()
  for (const instance of discovery.instances) {
    for (const expression of instance.function.statements
      .flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)) {
      if (expression._tag === 'StaticStringLiteral' || expression._tag === 'StaticByteViewLiteral')
        staticDataById.set(expression.data.id, expression.data)
    }
  }
  const staticData = Object.freeze(
    [...staticDataById.values()].sort((left, right) => left.id.localeCompare(right.id)),
  )
  const effectResults = new Map<string, Extract<Mir.Type, { readonly _tag: 'EffectValue' }>>()
  const generatedRunners: Array<GeneratedEffectRunner> = []
  for (const instance of discovery.instances) {
    const block = returnedEffectBlock(instance.function)
    if (block === undefined) continue
    const type = effectValueType(layout, instance.key, block)
    if (type !== undefined) {
      effectResults.set(instanceText(instance.key.declaration, instance.key.typeArguments), type)
      generatedRunners.push(
        Object.freeze({
          id: Hir.effectRunnerId(instance.key.declaration, block.site),
          owner: instance,
          block,
          type,
          specializationKey: baseRunnerKey(instance.key, block.site),
          providedRequirements: Object.freeze([]),
        }),
      )
    }
  }
  const functions = discovery.instances.map((instance) =>
    lowerInstance(
      instance,
      ownership.get(instance.key.declaration.module),
      layout,
      index,
      discovery.instances,
      discovery.calls,
      effectResults,
      generatedRunners,
    ),
  )
  const loweredRunners: Array<{
    readonly spec: GeneratedEffectRunner
    readonly runner: Mir.MirFunction
  }> = []
  for (let ordinal = 0; ordinal < generatedRunners.length; ordinal += 1) {
    const generated = generatedRunners.at(ordinal)
    if (generated === undefined) continue
    const runner = lowerEffectRunner(
      generated,
      ownership.get(generated.owner.key.declaration.module),
      layout,
      index,
      discovery.instances,
      discovery.calls,
      effectResults,
      generatedRunners,
    )
    if (runner !== undefined) loweredRunners.push(Object.freeze({ spec: generated, runner }))
  }
  // Lowering a provided parent can discover provided children after their open bases were already
  // visited. Filter only after the worklist reaches its fixed point so backends never compile an
  // unreachable open runner that still calls another open runner without provider arguments.
  const unresolvedOpenBase = (spec: GeneratedEffectRunner): boolean => {
    const entryOwnsRunner =
      discovery.entry._tag === 'Resolved' &&
      discovery.entry.kind === 'Effect' &&
      instanceText(spec.owner.key.declaration, spec.owner.key.typeArguments) ===
        instanceText(discovery.entry.key.declaration, discovery.entry.key.typeArguments)
    return (
      !entryOwnsRunner &&
      spec.providedRequirements.length === 0 &&
      spec.type.type.requirements.length > 0
    )
  }
  const runnerKey = (
    declaration: DeclarationIndex.CanonicalId,
    typeArguments: ReadonlyArray<Type.GenericArgument>,
  ): string => instanceText(declaration, typeArguments)
  const retainedRunners = new Set(
    loweredRunners
      .filter(({ spec }) => !unresolvedOpenBase(spec))
      .map(({ spec }) => runnerKey(spec.id, spec.owner.key.typeArguments)),
  )
  const retainReferencedRunners = (fn: Mir.MirFunction): boolean => {
    let changed = false
    for (const operation of Mir.operations(fn)) {
      if (
        operation._tag !== 'RunEffectValue' &&
        operation._tag !== 'RunStaticEffect' &&
        operation._tag !== 'ReifyEffect'
      )
        continue
      const key = runnerKey(operation.runner, operation.runnerTypeArguments)
      if (!retainedRunners.has(key)) {
        retainedRunners.add(key)
        changed = true
      }
    }
    return changed
  }
  for (const fn of functions) retainReferencedRunners(fn)
  let retainedChanged = true
  while (retainedChanged) {
    retainedChanged = false
    for (const { spec, runner } of loweredRunners) {
      if (!retainedRunners.has(runnerKey(spec.id, spec.owner.key.typeArguments))) continue
      if (retainReferencedRunners(runner)) retainedChanged = true
    }
  }
  functions.push(
    ...loweredRunners.flatMap(({ spec, runner }) => {
      return retainedRunners.has(runnerKey(spec.id, spec.owner.key.typeArguments)) ? [runner] : []
    }),
  )
  if (discovery.entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'MirModule',
      module: discovery.rootModule,
      intrinsics: discovery.intrinsics,
      entry: Object.freeze({ _tag: 'UnavailableEntry', reason: discovery.entry.reason }),
      layout,
      staticData,
      functions: Object.freeze(functions),
    })
  }
  const resolvedEntry = discovery.entry
  let entry: Mir.Entry
  if (resolvedEntry.kind === 'Ordinary') {
    entry = Object.freeze({
      _tag: 'OrdinaryEntry',
      target: resolvedEntry.key,
      machine: resolvedEntry.key,
    })
  } else {
    const target = functions.find(
      (fn) =>
        instanceText(fn.instance.declaration, fn.instance.typeArguments) ===
        instanceText(resolvedEntry.key.declaration, resolvedEntry.key.typeArguments),
    )
    const runnerSpec = generatedRunners.find(
      (candidate) =>
        instanceText(candidate.owner.key.declaration, candidate.owner.key.typeArguments) ===
        instanceText(resolvedEntry.key.declaration, resolvedEntry.key.typeArguments),
    )
    const runner =
      runnerSpec === undefined
        ? undefined
        : functions.find((fn) =>
            Mir.matchesInstance(fn, runnerSpec.id, resolvedEntry.key.typeArguments),
          )
    if (target?.result._tag !== 'EffectValue' || runner?.result._tag !== 'EffectOutcome') {
      throw new RangeError('Effect entry lowering lost its constructor or runner')
    }
    const adapterId = effectEntryAdapterId(discovery.rootModule)
    const adapterKey: Instances.InstanceKey = Object.freeze({
      _tag: 'InstanceKey',
      declaration: adapterId,
      typeArguments: Object.freeze([]),
      contractRow: Object.freeze(['generated:effect-entry']),
    })
    const span = target.regions
      .flatMap((region) =>
        region._tag === 'OperationRegion'
          ? region.operations.map((operation) => operation.provenance.span)
          : region._tag === 'CleanupRegion'
            ? region.releases.map((operation) => operation.provenance.span)
            : [region.provenance.span],
      )
      .at(0)
    if (span === undefined) throw new RangeError('Effect entry lowering lost source provenance')
    const failures = resolvedEntry.failures.map((failure, ordinal) =>
      Object.freeze({
        tag: ordinal + 1,
        type: failure.type,
        report: failure.report,
        payload: local(ordinal + 3),
        cleanup: Ownership.cleanupPlan(index, failure.type),
      }),
    )
    const effect = local(1)
    const outcome = local(2)
    const status = local(0)
    functions.push(
      Object.freeze({
        _tag: 'MirFunction',
        id: adapterId,
        instance: adapterKey,
        parameterCount: 0,
        localTypes: Object.freeze([
          i32,
          target.result,
          runner.result,
          ...failures.map((failure) =>
            Object.freeze({ _tag: 'Nominal' as const, type: failure.type }),
          ),
        ]),
        result: i32,
        entry: Object.freeze({ _tag: 'Region', ordinal: 0 }),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: Object.freeze({ _tag: 'Region' as const, ordinal: 0 }),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'CloseEffectEntry' as const,
                destination: status,
                effect,
                outcome,
                target: resolvedEntry.key.declaration,
                runner: runner.id,
                typeArguments: resolvedEntry.key.typeArguments,
                effectType: target.result,
                outcomeType: runner.result,
                failures: Object.freeze(failures),
                type: i32,
                provenance: generated(span),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: status,
              provenance: generated(span),
            }),
          }),
        ]),
      }),
    )
    entry = Object.freeze({
      _tag: 'EffectEntry',
      target: resolvedEntry.key,
      machine: adapterKey,
      requirements: resolvedEntry.requirements,
      failures: Object.freeze(
        resolvedEntry.failures.map((failure, ordinal) =>
          Object.freeze({ tag: ordinal + 1, type: failure.type, report: failure.report }),
        ),
      ),
    })
  }
  return Object.freeze({
    _tag: 'MirModule',
    module: discovery.rootModule,
    intrinsics: discovery.intrinsics,
    entry,
    layout,
    staticData,
    functions: Object.freeze(functions),
  })
}
