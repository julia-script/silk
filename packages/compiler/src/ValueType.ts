import type * as DeclarationIndex from './DeclarationIndex.js'
import type {} from './EntryAssembly.js'
import * as FieldRealization from './FieldRealization.js'
import type {} from './Forwarding.js'
import type { FunctionLowering } from './FunctionLowering.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import type * as Intrinsic from './Intrinsic.js'
import * as Layout from './Layout.js'
import type { ProvidedRequirement } from './Lower.js'
import type {} from './LowerExpression.js'
import type * as Mir from './Mir.js'
import * as OpaqueRealization from './OpaqueRealization.js'
import * as Specialization from './Specialization.js'
import * as Type from './Type.js'

export interface GeneratedBlockEffectRunner {
  readonly _tag: 'BlockEffectRunner'
  readonly id: DeclarationIndex.CanonicalId
  readonly owner: Instances.Instance
  readonly block: Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }>
  readonly type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly specializationKey: string
  readonly providedRequirements: ReadonlyArray<Omit<ProvidedRequirement, 'local'>>
}

export interface GeneratedWitnessEffectRunner {
  readonly _tag: 'WitnessEffectRunner'
  readonly id: DeclarationIndex.CanonicalId
  readonly owner: Instances.Instance
  readonly expression: Extract<
    Hir.Expression,
    { readonly _tag: 'BuiltinCall' | 'BoundOperationCall' }
  >
  readonly target?: DeclarationIndex.InterfaceWitnessTarget
  readonly intrinsic?: Intrinsic.Operation
  readonly type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly specializationKey: string
  readonly providedRequirements: ReadonlyArray<Omit<ProvidedRequirement, 'local'>>
}

export interface GeneratedCatchEffectRunner {
  readonly _tag: 'CatchEffectRunner'
  readonly id: DeclarationIndex.CanonicalId
  readonly owner: Instances.Instance
  readonly expression: Extract<Hir.Expression, { readonly _tag: 'EffectCatch' }>
  readonly type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly protectedType: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>
  readonly handlerType: Extract<Mir.Type, { readonly _tag: 'CallableValue' }>
  readonly specializationKey: string
  readonly providedRequirements: ReadonlyArray<Omit<ProvidedRequirement, 'local'>>
}

export type GeneratedEffectRunner =
  | GeneratedBlockEffectRunner
  | GeneratedWitnessEffectRunner
  | GeneratedCatchEffectRunner

export const instanceText = (
  declaration: { readonly module: string; readonly name: string },
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): string => Specialization.key({ declaration, typeArguments })

export const effectEntryAdapterId = (module: string): DeclarationIndex.CanonicalId =>
  Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module,
    name: '$effect-entry',
  })

export const unitEntryAdapterId = (module: string): DeclarationIndex.CanonicalId =>
  Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module,
    name: '$unit-entry',
  })

export const baseRunnerKey = (owner: Instances.InstanceKey, site: Hir.EffectSiteId): string =>
  `${instanceText(owner.declaration, owner.typeArguments)}\u0000${Hir.executableSiteKey(site)}`

export const witnessKey = (witness: DeclarationIndex.ConformanceWitness): string =>
  witness._tag === 'SourceConformanceWitness'
    ? `${witness._tag}:${witness.operations
        .map(
          (operation) =>
            `${operation.name}=${instanceText(operation.implementation, witness.typeArguments)}`,
        )
        .join(',')}`
    : `${witness._tag}:${Type.key(witness.provider)}`

export const providedContractEntry = (requirement: Omit<ProvidedRequirement, 'local'>): string =>
  `provided:${Type.key(requirement.capability)}@${requirement.role}:${requirement.requirementAccess}:${requirement.access}:${Type.key(requirement.providerType)}:${requirement.witness._tag}`

export const providedRunnerKey = (
  type: Extract<Mir.Type, { readonly _tag: 'EffectValue' }>,
  requirements: ReadonlyArray<ProvidedRequirement>,
): string =>
  `${baseRunnerKey(type.environment.instance, type.site)}\u0000${requirements
    .map(
      (requirement) =>
        `${Type.key(requirement.capability)}@${requirement.role}:${requirement.access}:${Type.key(requirement.providerType)}:${witnessKey(requirement.witness)}`,
    )
    .join('\u0000')}`

export const effectValueType = (
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

export const witnessEffectValueType = (
  layout: Layout.Plan,
  instance: Instances.InstanceKey,
  site: Hir.EffectSiteId,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  const environment = layout.effectEnvironments.find(
    (candidate) =>
      candidate._tag === 'EffectEnvironment' &&
      instanceText(candidate.instance.declaration, candidate.instance.typeArguments) ===
        instanceText(instance.declaration, instance.typeArguments) &&
      Hir.sameExecutableSite(candidate.site, site),
  )
  return environment?._tag !== 'EffectEnvironment'
    ? undefined
    : Object.freeze({ _tag: 'EffectValue', type: environment.effect, site, environment })
}

export const effectValueByIdentity = (
  layout: Layout.Plan,
  identity: string,
  owner?: Type.ExecutableSpecializationOwner,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  const available = layout.effectEnvironments.filter(
    (
      candidate,
    ): candidate is Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
      candidate._tag === 'EffectEnvironment',
  )
  const environment =
    available.find(
      (candidate) => Instances.effectIdentity(candidate.instance, candidate.site) === identity,
    ) ??
    available.find((candidate) => candidate.successEffectIdentity === identity) ??
    available.find(
      (candidate) =>
        Hir.effectRepresentationIdentity(candidate.site) === identity &&
        owner !== undefined &&
        candidate.instance.declaration.module === owner.declaration.module &&
        candidate.instance.declaration.name === owner.declaration.name &&
        sameArguments(candidate.instance.typeArguments, owner.typeArguments),
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

export const effectCompositeShape = (
  layout: Layout.Plan,
  type: Extract<Mir.Type, { readonly _tag: 'EffectComposite' }>,
): Layout.CallingShape => {
  const shape = Layout.callingShape(layout, type.type)
  if (shape?.tree._tag !== 'EffectCompositeShape')
    throw new RangeError('Effect composite has no canonical calling shape')
  return shape
}

export const callableValueByIdentity = (
  layout: Layout.Plan,
  identity: Type.CallableIdentityArgument,
  type: Type.Callable,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const target = Hir.callableTargetFromIdentity(identity.target)
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
            FieldRealization.matchesIdentity(identity, candidate.callable),
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

export const sameArguments = (
  left: ReadonlyArray<Type.GenericArgument>,
  right: ReadonlyArray<Type.GenericArgument>,
): boolean =>
  left.length === right.length &&
  left.every((argument, ordinal) => {
    const candidate = right.at(ordinal)
    return candidate !== undefined && Type.equalsGenericArgument(argument, candidate)
  })

export const representedValueType = (
  layout: Layout.Plan,
  catalog: OpaqueRealization.Catalog,
  type: Type.Type,
  substitution: Type.Substitution,
):
  | Extract<Mir.Type, { readonly _tag: 'CallableValue' | 'EffectValue' | 'EffectComposite' }>
  | undefined => {
  const specialized = Type.substitute(type, substitution)
  if (!Type.isRepresented(specialized)) return undefined
  const representation = specialized.representation.argument
  if (
    Type.isCompositeEffectRepresentationArgument(representation) &&
    Type.isEffect(specialized.contract)
  ) {
    const alternatives = representation.alternatives.flatMap((alternative) =>
      Type.isEffectIdentityArgument(alternative.identity)
        ? (effectValueByIdentity(
            layout,
            alternative.identity.identity,
            alternative.identity.owner,
          ) ?? [])
        : [],
    )
    return alternatives.length !== representation.alternatives.length
      ? undefined
      : Object.freeze({
          _tag: 'EffectComposite',
          type: specialized,
          contract: specialized.contract,
          alternatives: Object.freeze(alternatives),
        })
  }
  if (Type.isExactRepresentationArgument(representation)) {
    if (
      Type.isCallable(specialized.contract) &&
      Type.isCallableIdentityArgument(representation.identity)
    )
      return callableValueByIdentity(layout, representation.identity, specialized.contract)
    if (
      Type.isEffect(specialized.contract) &&
      Type.isEffectIdentityArgument(representation.identity)
    )
      return effectValueByIdentity(
        layout,
        representation.identity.identity,
        representation.identity.owner,
      )
    return undefined
  }
  const opaque = representation
  if (!Type.isOpaqueRepresentationArgument(opaque)) return undefined
  const definition = OpaqueRealization.definitionOf(catalog, opaque)
  if (definition === undefined) return undefined
  const realization = definition.realization
  if (realization?._tag !== 'ExactRepresentationArgument') return undefined
  if (
    Type.isCallable(specialized.contract) &&
    Type.isCallableIdentityArgument(realization.identity)
  ) {
    const identity = realization.identity
    if (identity.environment === undefined)
      return callableValueByIdentity(layout, identity, specialized.contract)
    const environment = layout.callableEnvironments.find(
      (
        candidate,
      ): candidate is Extract<
        Layout.CallableEnvironment,
        { readonly _tag: 'CallableEnvironment' }
      > =>
        candidate._tag === 'CallableEnvironment' &&
        candidate.callable.owner.declaration.module === definition.construction.producer.module &&
        candidate.callable.owner.declaration.name === definition.construction.producer.name &&
        sameArguments(
          candidate.callable.owner.typeArguments.filter(
            (argument) => !Type.isHiddenIdentityArgument(argument),
          ),
          definition.construction.arguments,
        ) &&
        definition.construction.site ===
          Type.callableEnvironmentKey(Instances.callableEnvironmentIdentity(candidate.callable)),
    )
    return environment === undefined
      ? undefined
      : callableValueByIdentity(
          layout,
          Type.callableIdentityArgument(
            identity.identity,
            identity.target,
            identity.typeArguments,
            Instances.callableEnvironmentIdentity(environment.callable),
          ),
          specialized.contract,
        )
  }
  if (Type.isEffect(specialized.contract) && Type.isEffectIdentityArgument(realization.identity)) {
    const environment = layout.effectEnvironments.find(
      (
        candidate,
      ): candidate is Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =>
        candidate._tag === 'EffectEnvironment' &&
        candidate.instance.declaration.module === definition.construction.producer.module &&
        candidate.instance.declaration.name === definition.construction.producer.name &&
        sameArguments(
          candidate.instance.typeArguments.filter(
            (argument) => !Type.isHiddenIdentityArgument(argument),
          ),
          definition.construction.arguments,
        ) &&
        definition.construction.site === Hir.effectRepresentationIdentity(candidate.site),
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
  return undefined
}

export const storedCallableValueType = (
  layout: Layout.Plan,
  type: Type.Type,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  if (!Type.isRepresented(type) || !Type.isCallable(type.contract)) return undefined
  const representation = Layout.entry(layout, type)?.representation
  if (representation?._tag !== 'CallableEnvironment') return undefined
  const realization = representation.realization
  const environment =
    realization.site === undefined
      ? undefined
      : layout.callableEnvironments.find(
          (
            candidate,
          ): candidate is Extract<
            Layout.CallableEnvironment,
            { readonly _tag: 'CallableEnvironment' }
          > =>
            candidate._tag === 'CallableEnvironment' &&
            FieldRealization.matchesCallable(realization, candidate.callable),
        )
  if (realization.site !== undefined && environment === undefined) return undefined
  return Object.freeze({
    _tag: 'CallableValue',
    type: realization.contract,
    target: Hir.callableTargetFromIdentity(realization.target),
    ...(realization.site === undefined ? {} : { site: realization.site }),
    ...(environment === undefined ? {} : { environment }),
    storage: Object.freeze({
      _tag: 'StoredCallableField',
      type,
      realization,
    }),
  })
}

export const storedEffectValueType = (
  layout: Layout.Plan,
  type: Type.Type,
): Extract<Mir.Type, { readonly _tag: 'EffectValue' }> | undefined => {
  if (!Type.isRepresented(type) || !Type.isEffect(type.contract)) return undefined
  if (Type.isOpaqueRepresentationArgument(type.representation.argument)) return undefined
  const entry = Layout.entry(layout, type)
  const representation = entry?.representation
  if (entry === undefined || representation?._tag !== 'StoredEffectEnvironment') return undefined
  const realization = representation.realization
  const environment: Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> =
    Object.freeze({
      _tag: 'EffectEnvironment',
      instance: realization.runnerInstance,
      site: realization.site,
      effect: realization.contract,
      fields: representation.fields,
      size: entry.size,
      alignment: entry.alignment,
      tailPadding: representation.tailPadding,
    })
  return Object.freeze({
    _tag: 'EffectValue',
    type: realization.contract,
    site: realization.site,
    environment,
    storage: Object.freeze({
      _tag: 'StoredEffectField',
      type,
      realization,
    }),
  })
}

export const requirementsFor = (
  available: ReadonlyArray<ProvidedRequirement>,
  effect: Type.Effect,
): ReadonlyArray<ProvidedRequirement> | undefined => {
  const selected = Type.requirementMembers(effect).map((requirement) =>
    available.find(
      (candidate) =>
        candidate.role === requirement.role &&
        Type.equals(candidate.capability, requirement.capability) &&
        (requirement.access === 'Shared' ||
          candidate.access === 'Exclusive' ||
          candidate.access === 'Take'),
    ),
  )
  return selected.every((candidate) => candidate !== undefined)
    ? Object.freeze(
        selected.flatMap((candidate, ordinal) => {
          const requirement = Type.requirementMembers(effect).at(ordinal)
          return candidate === undefined || requirement === undefined
            ? []
            : [Object.freeze({ ...candidate, requirementAccess: requirement.access })]
        }),
      )
    : undefined
}

export const ensureProvidedRunner = (
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
      ...base,
      id,
      specializationKey: key,
      providedRequirements: Object.freeze(
        requirements.map(({ local: _local, ...requirement }) => Object.freeze(requirement)),
      ),
    }),
  )
  return id
}

export const runtimeRequirementArguments = (
  requirements: ReadonlyArray<ProvidedRequirement> | undefined,
): ReadonlyArray<Mir.LocalId> =>
  Object.freeze(
    requirements?.flatMap((requirement) =>
      requirement.witness._tag !== 'SourceConformanceWitness' || requirement.local === undefined
        ? []
        : [requirement.local],
    ) ?? [],
  )

export const providerBindings = (
  requirements: ReadonlyArray<ProvidedRequirement> | undefined,
): Extract<Mir.Operation, { readonly _tag: 'RunEffectValue' }>['providers'] =>
  Object.freeze(
    requirements?.map((requirement) =>
      Object.freeze({
        capability: requirement.capability,
        providerType: requirement.providerType,
        witness: requirement.witness,
        role: requirement.role,
        requirementAccess: requirement.requirementAccess,
        access: requirement.access,
        ...(requirement.witness._tag === 'SourceConformanceWitness' &&
        requirement.local !== undefined
          ? { argument: requirement.local }
          : {}),
      }),
    ) ?? [],
  )

export const sameSite = (left: Hir.CallableSiteId, right: Hir.CallableSiteId): boolean =>
  Hir.sameExecutableSite(left, right)

export const callableValueType = (
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
      (!Type.isRuntimeConcrete(expected) || Type.equals(candidate.callable.type, expected)),
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

export const directCallableSectionValueType = (
  fn: FunctionLowering,
  section: Extract<Hir.Expression, { readonly _tag: 'CallableSection' }>,
  applicationSubstitution: Type.Substitution,
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const type = Type.substitute(
    Type.substitute(section.type, fn.substitution),
    new Map([...section.substitution, ...applicationSubstitution]),
  )
  return Type.isCallable(type) && Type.isRuntimeConcrete(type)
    ? Object.freeze({ _tag: 'CallableValue', type, target: section.target })
    : undefined
}

export const functionItemValueType = (
  fn: FunctionLowering,
  item: Extract<Hir.Expression, { readonly _tag: 'FunctionItem' }>,
  applicationSubstitution: Type.Substitution = new Map(),
): Extract<Mir.Type, { readonly _tag: 'CallableValue' }> | undefined => {
  const type = Type.substitute(Type.substitute(item.type, fn.substitution), applicationSubstitution)
  return Type.isCallable(type) && Type.isRuntimeConcrete(type)
    ? Object.freeze({ _tag: 'CallableValue', type, target: item.target })
    : undefined
}
