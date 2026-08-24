import * as ConformanceProof from './ConformanceProof.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as FieldRealization from './FieldRealization.js'
import type * as Hir from './Hir.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Type from './Type.js'

export type CallableEnvironmentLocator =
  | { readonly _tag: 'CallableEnvironmentSite'; readonly site: Hir.CallableSiteId }
  | {
      readonly _tag: 'CallableEnvironmentIdentity'
      readonly identity: Type.CallableEnvironmentIdentity
    }

/** The symbolic recursive cleanup of one complete logical owner. */
export type CleanupPlan =
  | { readonly _tag: 'NoCleanup'; readonly type: DeclarationFacts.SemanticType }
  | { readonly _tag: 'ParameterCleanup'; readonly type: Type.Parameter }
  | {
      readonly _tag: 'AllocationCleanup'
      readonly type: Type.Nominal
      readonly ticket: 'ActiveReclaimTicket'
    }
  | {
      readonly _tag: 'RawBufferCleanup'
      readonly type: Type.Nominal
      readonly allocation: Extract<CleanupPlan, { readonly _tag: 'AllocationCleanup' }>
    }
  | {
      /** One opaque dynamic decrement-or-last-cleanup action for a complete strong handle. */
      readonly _tag: 'LocalSharedCoreCleanup'
      readonly type: Type.Nominal
      readonly element: Type.Type
      readonly allocation: Extract<CleanupPlan, { readonly _tag: 'AllocationCleanup' }>
    }
  | {
      /** Dynamic cleanup through the exact metadata retained by one opaque Execution package. */
      readonly _tag: 'ExecutionCleanup'
      readonly type: Type.Nominal
      readonly allocation: Extract<CleanupPlan, { readonly _tag: 'AllocationCleanup' }>
    }
  | {
      /** Discharges one affine generation authority retained by an opaque Wake. */
      readonly _tag: 'WakeCleanup'
      readonly type: Type.Nominal
      readonly allocation: Extract<CleanupPlan, { readonly _tag: 'AllocationCleanup' }>
    }
  | {
      readonly _tag: 'HookCleanup'
      readonly type: Type.Nominal
      readonly hook: DeclarationFacts.CanonicalId
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly inner: CleanupPlan
    }
  | {
      readonly _tag: 'StructCleanup'
      readonly type: Type.Nominal
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationFacts.FieldId
        readonly cleanup: CleanupPlan
      }>
    }
  | {
      readonly _tag: 'ArrayCleanup'
      readonly type: Type.FixedArray
      readonly length: number
      readonly element: CleanupPlan
    }
  | {
      readonly _tag: 'UnionCleanup'
      readonly type: Type.StructuralUnion
      readonly cases: ReadonlyArray<{
        readonly member: Type.Type
        readonly ordinal: number
        readonly cleanup: CleanupPlan
      }>
    }
  | {
      readonly _tag: 'CallableCleanup'
      readonly type: Type.Callable
      readonly environment: CallableEnvironmentLocator
      readonly slots: ReadonlyArray<{ readonly ordinal: number; readonly cleanup: CleanupPlan }>
    }
  | {
      readonly _tag: 'RepresentedCallableCleanup'
      readonly type: Type.Represented
      readonly contract: Type.Callable
    }
  | {
      readonly _tag: 'RepresentedEffectCleanup'
      readonly type: Type.Represented
      readonly contract: Type.Effect
    }
  | {
      readonly _tag: 'EffectCompositeCleanup'
      readonly type: Type.Represented
      readonly alternatives: ReadonlyArray<CleanupPlan>
    }
  | {
      readonly _tag: 'EffectCleanup'
      readonly type: Type.Effect
      readonly site: Hir.EffectSiteId
      readonly slots: ReadonlyArray<{
        readonly ordinal: number
        readonly laneOffset: number
        readonly laneCount: number
        readonly cleanup: CleanupPlan
      }>
    }

export const hasHook = (self: CleanupPlan): boolean =>
  self._tag === 'HookCleanup' ||
  (self._tag === 'StructCleanup' && self.fields.some((field) => hasHook(field.cleanup))) ||
  (self._tag === 'ArrayCleanup' && hasHook(self.element)) ||
  (self._tag === 'UnionCleanup' && self.cases.some((entry) => hasHook(entry.cleanup))) ||
  ((self._tag === 'CallableCleanup' || self._tag === 'EffectCleanup') &&
    self.slots.some((slot) => hasHook(slot.cleanup))) ||
  (self._tag === 'EffectCompositeCleanup' &&
    self.alternatives.some((alternative) => hasHook(alternative))) ||
  (self._tag === 'RawBufferCleanup' && hasHook(self.allocation))

export const reclaims = (self: CleanupPlan): boolean =>
  self._tag === 'AllocationCleanup' ||
  self._tag === 'RawBufferCleanup' ||
  self._tag === 'LocalSharedCoreCleanup' ||
  self._tag === 'ExecutionCleanup' ||
  self._tag === 'WakeCleanup' ||
  (self._tag === 'HookCleanup' && reclaims(self.inner)) ||
  (self._tag === 'StructCleanup' && self.fields.some((field) => reclaims(field.cleanup))) ||
  (self._tag === 'ArrayCleanup' && reclaims(self.element)) ||
  (self._tag === 'UnionCleanup' && self.cases.some((entry) => reclaims(entry.cleanup))) ||
  ((self._tag === 'CallableCleanup' || self._tag === 'EffectCleanup') &&
    self.slots.some((slot) => reclaims(slot.cleanup))) ||
  (self._tag === 'EffectCompositeCleanup' &&
    self.alternatives.some((alternative) => reclaims(alternative)))

export const hasEffect = (self: CleanupPlan): boolean => hasHook(self) || reclaims(self)
export const cleanupFields = (
  index: DeclarationIndex.Index,
  type: Type.Nominal,
  seen = new Set<string>(),
): ReadonlyArray<DeclarationFacts.FieldId> => {
  const key = Type.key(type)
  if (seen.has(key)) return Object.freeze([])
  const nextSeen = new Set(seen).add(key)
  const declaration = DeclarationFacts.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') return Object.freeze([])
  return Object.freeze(
    declaration.fields.flatMap((field) => {
      if (field.declaredType._tag !== 'Resolved') return [field.id]
      const nested = field.declaredType.type
      return Type.isNominal(nested)
        ? [field.id, ...cleanupFields(index, nested, nextSeen)]
        : [field.id]
    }),
  )
}

export const cleanupPlan = (
  index: DeclarationIndex.Index,
  type: DeclarationFacts.SemanticType,
  seen = new Set<string>(),
): CleanupPlan => {
  if (Type.isBuiltin(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isString(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isNever(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (ConformanceProof.copyType(index, type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isParameter(type)) return Object.freeze({ _tag: 'ParameterCleanup', type })
  if (Type.isSlice(type) || Type.isReference(type))
    return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.isEffect(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  if (Type.equals(type, Type.allocation))
    return Object.freeze({
      _tag: 'AllocationCleanup',
      type: Type.allocation,
      ticket: 'ActiveReclaimTicket',
    })
  if (Type.isRawBuffer(type))
    return Object.freeze({
      _tag: 'RawBufferCleanup',
      type,
      allocation: Object.freeze({
        _tag: 'AllocationCleanup',
        type: Type.allocation,
        ticket: 'ActiveReclaimTicket',
      }),
    })
  if (Type.isSharedCore(type)) {
    const element = Type.typeArgumentAt(type, 0)
    return element === undefined
      ? Object.freeze({ _tag: 'NoCleanup', type })
      : Object.freeze({
          _tag: 'LocalSharedCoreCleanup',
          type,
          element,
          allocation: Object.freeze({
            _tag: 'AllocationCleanup',
            type: Type.allocation,
            ticket: 'ActiveReclaimTicket',
          }),
        })
  }
  if (Type.isExecution(type))
    return Object.freeze({
      _tag: 'ExecutionCleanup',
      type,
      allocation: Object.freeze({
        _tag: 'AllocationCleanup',
        type: Type.allocation,
        ticket: 'ActiveReclaimTicket',
      }),
    })
  if (Type.isWake(type))
    return Object.freeze({
      _tag: 'WakeCleanup',
      type,
      allocation: Object.freeze({
        _tag: 'AllocationCleanup',
        type: Type.allocation,
        ticket: 'ActiveReclaimTicket',
      }),
    })
  if (Type.isFixedArray(type)) {
    if (type.length === 0) return Object.freeze({ _tag: 'NoCleanup', type })
    return Object.freeze({
      _tag: 'ArrayCleanup',
      type,
      length: type.length,
      element: cleanupPlan(index, type.element, seen),
    })
  }
  if (Type.isUnion(type)) {
    return Object.freeze({
      _tag: 'UnionCleanup',
      type,
      cases: Object.freeze(
        type.members.map((member, ordinal) =>
          Object.freeze({
            member,
            ordinal,
            cleanup: cleanupPlan(index, member, seen),
          }),
        ),
      ),
    })
  }
  if (Type.isCallable(type)) return Object.freeze({ _tag: 'NoCleanup', type })
  // A stored executable representation owes its enclosing aggregate an exactly-once release of
  // every owned environment lane. Concrete specialization resolves the shared realization.
  if (Type.isRepresented(type))
    return Type.isCallable(type.contract)
      ? Object.freeze({ _tag: 'RepresentedCallableCleanup', type, contract: type.contract })
      : Type.isEffect(type.contract)
        ? Object.freeze({ _tag: 'RepresentedEffectCleanup', type, contract: type.contract })
        : Object.freeze({ _tag: 'NoCleanup', type })
  const key = Type.key(type)
  if (seen.has(key)) return Object.freeze({ _tag: 'NoCleanup', type })
  const declaration = DeclarationFacts.byCanonical(index, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') {
    return Object.freeze({ _tag: 'NoCleanup', type })
  }
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const nextSeen = new Set(seen).add(key)
  const structPlan: CleanupPlan = Object.freeze({
    _tag: 'StructCleanup',
    type,
    fields: Object.freeze(
      declaration.fields.map((field) =>
        Object.freeze({
          field: field.id,
          cleanup:
            field.declaredType._tag === 'Resolved'
              ? cleanupPlan(index, Type.substitute(field.declaredType.type, substitution), nextSeen)
              : Object.freeze({ _tag: 'NoCleanup' as const, type: 'i32' as const }),
        }),
      ),
    ),
  })
  // A source Drop conformance runs its hook before automatic field cleanup.
  const witness = ConformanceProof.witness(index, type, Type.dropCapability)
  if (witness?._tag !== 'SourceConformanceWitness') return structPlan
  const conformance = index.modules
    .find((module) => module.module === witness.module)
    ?.conformances.find((candidate) => candidate.ordinal === witness.ordinal)
  if (conformance?.provider._tag !== 'Resolved') return structPlan
  const inferred = new Map<string, Type.GenericArgument>()
  if (!TypeInference.infer(conformance.provider.type, type, inferred)) return structPlan
  return Object.freeze({
    _tag: 'HookCleanup',
    type,
    hook: Object.freeze({
      _tag: 'CanonicalDeclarationId' as const,
      module: witness.module,
      name: `drop@impl#${witness.ordinal}`,
    }),
    typeArguments: Object.freeze(
      conformance.typeParameters.map(
        (parameter) => inferred.get(Type.key(parameter.type)) ?? parameter.type,
      ),
    ),
    inner: structPlan,
  })
}

export const cleanupTypeAtPath = (
  index: DeclarationIndex.Index,
  root: DeclarationFacts.SemanticType | undefined,
  path: ReadonlyArray<DeclarationFacts.FieldId>,
): DeclarationFacts.SemanticType | undefined => {
  let current = root
  for (const fieldId of path) {
    if (current === undefined || !Type.isNominal(current)) return undefined
    const declaration = DeclarationFacts.byCanonical(index, {
      _tag: 'CanonicalDeclarationId',
      module: current.module,
      name: current.name,
    })
    if (declaration?._tag !== 'StructDeclaration') return undefined
    const substitution = TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      current.arguments,
    )
    if (substitution === undefined) return undefined
    const field = declaration.fields.find(
      (candidate) =>
        candidate.id.struct.ordinal === fieldId.struct.ordinal &&
        candidate.id.ordinal === fieldId.ordinal,
    )
    current =
      field?.declaredType._tag === 'Resolved'
        ? Type.substitute(field.declaredType.type, substitution)
        : undefined
  }
  return current
}

/** Substitutes one checked symbolic cleanup proof into a concrete instance. */
export const specializeCleanup = (
  cleanup: CleanupPlan,
  substitution: Type.Substitution,
  resolveConcrete?: (type: Type.Type) => CleanupPlan,
): CleanupPlan => {
  const type = Type.substitute(cleanup.type, substitution)
  switch (cleanup._tag) {
    case 'NoCleanup':
      return Object.freeze({ _tag: 'NoCleanup', type })
    case 'ParameterCleanup':
      return Type.isParameter(type)
        ? Object.freeze({ _tag: 'ParameterCleanup', type })
        : (resolveConcrete?.(type) ?? Object.freeze({ _tag: 'NoCleanup', type }))
    case 'AllocationCleanup':
      return Type.equals(type, Type.allocation)
        ? Object.freeze({
            _tag: 'AllocationCleanup',
            type: Type.allocation,
            ticket: 'ActiveReclaimTicket',
          })
        : Object.freeze({ _tag: 'NoCleanup', type })
    case 'RawBufferCleanup':
      return Type.isRawBuffer(type)
        ? Object.freeze({
            _tag: 'RawBufferCleanup',
            type,
            allocation: cleanup.allocation,
          })
        : Object.freeze({ _tag: 'NoCleanup', type })
    case 'LocalSharedCoreCleanup': {
      if (!Type.isSharedCore(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      const element = Type.typeArgumentAt(type, 0)
      if (element === undefined) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'LocalSharedCoreCleanup',
        type,
        element,
        allocation: cleanup.allocation,
      })
    }
    case 'ExecutionCleanup':
      return Type.isExecution(type)
        ? Object.freeze({ _tag: 'ExecutionCleanup', type, allocation: cleanup.allocation })
        : Object.freeze({ _tag: 'NoCleanup', type })
    case 'WakeCleanup':
      return Type.isWake(type)
        ? Object.freeze({ _tag: 'WakeCleanup', type, allocation: cleanup.allocation })
        : Object.freeze({ _tag: 'NoCleanup', type })
    case 'HookCleanup':
      if (!Type.isNominal(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'HookCleanup',
        type,
        hook: cleanup.hook,
        typeArguments: Object.freeze(
          cleanup.typeArguments.map((argument) =>
            Type.substituteGenericArgument(argument, substitution),
          ),
        ),
        inner: specializeCleanup(cleanup.inner, substitution, resolveConcrete),
      })
    case 'StructCleanup':
      if (!Type.isNominal(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'StructCleanup',
        type,
        fields: Object.freeze(
          cleanup.fields.map((field) =>
            Object.freeze({
              field: field.field,
              cleanup: specializeCleanup(field.cleanup, substitution, resolveConcrete),
            }),
          ),
        ),
      })
    case 'ArrayCleanup':
      if (!Type.isFixedArray(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'ArrayCleanup',
        type,
        length: type.length,
        element: specializeCleanup(cleanup.element, substitution, resolveConcrete),
      })
    case 'UnionCleanup':
      if (!Type.isUnion(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'UnionCleanup',
        type,
        cases: Object.freeze(
          cleanup.cases.map((entry, ordinal) => {
            const member = type.members.at(ordinal)
            return Object.freeze({
              member: member ?? entry.member,
              ordinal: entry.ordinal,
              cleanup: specializeCleanup(entry.cleanup, substitution, resolveConcrete),
            })
          }),
        ),
      })
    case 'CallableCleanup':
      if (!Type.isCallable(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'CallableCleanup',
        type,
        environment: cleanup.environment,
        slots: Object.freeze(
          cleanup.slots.map((slot) =>
            Object.freeze({
              ordinal: slot.ordinal,
              cleanup: specializeCleanup(slot.cleanup, substitution, resolveConcrete),
            }),
          ),
        ),
      })
    case 'EffectCleanup':
      if (!Type.isEffect(type)) return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'EffectCleanup',
        type,
        site: cleanup.site,
        slots: Object.freeze(
          cleanup.slots.map((slot) =>
            Object.freeze({
              ordinal: slot.ordinal,
              laneOffset: slot.laneOffset,
              laneCount: slot.laneCount,
              cleanup: specializeCleanup(slot.cleanup, substitution, resolveConcrete),
            }),
          ),
        ),
      })
    case 'EffectCompositeCleanup':
      if (!Type.isRepresented(type) || !Type.isEffect(type.contract))
        return Object.freeze({ _tag: 'NoCleanup', type })
      return Object.freeze({
        _tag: 'EffectCompositeCleanup',
        type,
        alternatives: Object.freeze(
          cleanup.alternatives.map((alternative) =>
            specializeCleanup(alternative, substitution, resolveConcrete),
          ),
        ),
      })
    case 'RepresentedCallableCleanup': {
      if (!Type.isRepresented(type) || !Type.isCallable(type.contract))
        return Object.freeze({ _tag: 'NoCleanup', type })
      // The caller resolves the complete instance's realization; without one the obligation stays
      // symbolic rather than collapsing to "nothing to clean".
      const resolved = resolveConcrete?.(type)
      return resolved === undefined
        ? Object.freeze({
            _tag: 'RepresentedCallableCleanup',
            type,
            contract: type.contract,
          })
        : resolved
    }
    case 'RepresentedEffectCleanup': {
      if (!Type.isRepresented(type) || !Type.isEffect(type.contract))
        return Object.freeze({ _tag: 'NoCleanup', type })
      const resolved = resolveConcrete?.(type)
      return resolved === undefined
        ? Object.freeze({ _tag: 'RepresentedEffectCleanup', type, contract: type.contract })
        : resolved
    }
  }
}

/**
 * The concrete cleanup one aggregate owes for the callable it stores in a field.
 *
 * Every fact comes from the shared runtime realization: the specialized environment's site, its
 * ordered capture lanes, and which of those lanes the aggregate owns. Lanes are released
 * last-captured first, exactly as a callable binding's own environment is, so a stored callable and
 * a direct one clean in the same order.
 */
export const realizedCallableCleanup = (
  index: DeclarationIndex.Index,
  realization: FieldRealization.CallableRealization,
): CleanupPlan => {
  const type = realization.contract
  const site = realization.site
  const environment = realization.environment
  const owned = realization.captures.filter((capture) => capture.owned)
  if (site === undefined || environment === undefined || owned.length === 0)
    return Object.freeze({ _tag: 'NoCleanup', type })
  return Object.freeze({
    _tag: 'CallableCleanup',
    type,
    environment: Object.freeze({
      _tag: 'CallableEnvironmentIdentity',
      identity: environment,
    }),
    slots: Object.freeze(
      [...owned].reverse().map((capture) =>
        Object.freeze({
          ordinal: capture.ordinal,
          cleanup: cleanupPlan(index, capture.type),
        }),
      ),
    ),
  })
}
