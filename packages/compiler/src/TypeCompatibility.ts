import * as Lifetime from './Lifetime.js'
import * as Canonical from './internal/Canonical.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Type from './Type.js'

/** One canonical source-member to target-member relationship for an implicit union conversion. */
export interface MemberMapping {
  readonly _tag: 'UnionMemberMapping'
  readonly source: Type.Type
  readonly sourceOrdinal: number
  readonly target: Type.Type
  readonly targetOrdinal: number
}

/** The one closed compatibility relation used at immediate expected-type boundaries. */
export type Compatibility =
  | { readonly _tag: 'Lifetime'; readonly source: Type.Type; readonly target: Type.Type }
  | {
      readonly _tag: 'Exact'
      readonly source: Type.Type
      readonly target: Type.Type
    }
  | {
      readonly _tag: 'Inject'
      readonly source: Type.Type
      readonly target: Type.StructuralUnion
      readonly mappings: ReadonlyArray<MemberMapping>
    }
  | {
      readonly _tag: 'Widen'
      readonly source: Type.StructuralUnion | Type.Bottom
      readonly target: Type.StructuralUnion
      readonly mappings: ReadonlyArray<MemberMapping>
    }
  | {
      readonly _tag: 'CallableMode'
      readonly source: Type.Callable
      readonly target: Type.Callable
    }
  | {
      readonly _tag: 'EffectAccess'
      readonly source: Type.Effect
      readonly target: Type.Effect
    }
  | {
      readonly _tag: 'ReferenceAccess'
      readonly source: Type.Reference
      readonly target: Type.Reference
    }
  | {
      readonly _tag: 'PointerWeakening'
      readonly source: Type.Pointer
      readonly target: Type.Pointer
    }
  | {
      readonly _tag: 'Bottom'
      readonly source: Type.Bottom
      readonly target: Type.Type
    }
  | {
      readonly _tag: 'Incompatible'
      readonly source: Type.Type
      readonly target: Type.Type
      readonly missing: ReadonlyArray<Type.Type>
    }

const sourceMembers = (source: Type.Type): ReadonlyArray<Type.Type> | undefined => {
  if (Type.isNever(source)) return Object.freeze([])
  if (Type.isUnion(source)) return source.members
  return Object.freeze([source])
}

/** Variance proven by declared storage; unknown/opaque parameter positions stay invariant. */
export type Variance = 'Covariant' | 'Contravariant' | 'Invariant' | 'Bivariant'

export interface Work {
  comparisons: number
  cacheHits: number
  outlivesObligations: number
  rigidBinders: number
}

export interface Context {
  readonly assumptions: Lifetime.Assumptions
  readonly nominalVariance: ReadonlyMap<string, ReadonlyArray<Variance>>
  readonly outlives?: (longer: Lifetime.Lifetime, shorter: Lifetime.Lifetime) => boolean
  readonly commitOutlives?: (longer: Lifetime.Lifetime, shorter: Lifetime.Lifetime) => void
  readonly typeBounds: ReadonlyArray<Type.TypeOutlives>
  readonly typeOutlives?: (type: Type.Type, lifetime: Lifetime.Lifetime) => boolean
  readonly commitTypeOutlives?: (type: Type.Type, lifetime: Lifetime.Lifetime) => void
  readonly work: Work
}

interface Proof {
  readonly result: Compatibility
  readonly obligations: ReadonlyArray<() => void>
}
const comparisons = new WeakMap<Context, Map<string, Proof>>()
const proofFrames = new WeakMap<Work, Array<Array<() => void>>>()

const retain = (self: Context, obligations: ReadonlyArray<() => void>): void => {
  const current = proofFrames.get(self.work)?.at(-1)
  if (current === undefined) for (const obligation of obligations) obligation()
  else if (obligations.length > 0)
    current.push(() => {
      for (const obligation of obligations) obligation()
    })
}

/** Keeps obligations from successful inference alternatives and discards rejected trials. */
export const commitWhen = <A>(
  self: Context,
  evaluate: () => A,
  accepted: (result: A) => boolean,
): A => {
  const frames = proofFrames.get(self.work) ?? []
  proofFrames.set(self.work, frames)
  const obligations: Array<() => void> = []
  frames.push(obligations)
  let result: A
  try {
    result = evaluate()
  } finally {
    frames.pop()
  }
  if (accepted(result)) retain(self, obligations)
  return result
}

/** Builds one comparison universe over already resolved summaries and lifetime assumptions. */
export const context = (
  options: {
    readonly assumptions?: Lifetime.Assumptions
    readonly nominalVariance?: ReadonlyMap<string, ReadonlyArray<Variance>>
    readonly outlives?: Context['outlives']
    readonly commitOutlives?: Context['commitOutlives']
    readonly typeBounds?: ReadonlyArray<Type.TypeOutlives>
    readonly typeOutlives?: Context['typeOutlives']
    readonly commitTypeOutlives?: Context['commitTypeOutlives']
  } = {},
): Context =>
  Object.freeze({
    assumptions: options.assumptions ?? Lifetime.assumptions([]),
    nominalVariance: new Map(options.nominalVariance ?? []),
    typeBounds: Type.normalizeTypeOutlives(options.typeBounds ?? []),
    ...(options.typeOutlives === undefined ? {} : { typeOutlives: options.typeOutlives }),
    ...(options.outlives === undefined ? {} : { outlives: options.outlives }),
    ...(options.commitOutlives === undefined ? {} : { commitOutlives: options.commitOutlives }),
    ...(options.commitTypeOutlives === undefined
      ? {}
      : { commitTypeOutlives: options.commitTypeOutlives }),
    work: { comparisons: 0, cacheHits: 0, outlivesObligations: 0, rigidBinders: 0 },
  })

/** Canonical declaration key used by the already-derived nominal variance catalog. */
export const nominalVarianceKey = (self: Type.Nominal): string =>
  Type.key(Type.specializeNominal(self, []))

const outlives = (
  self: Context,
  longer: Lifetime.Lifetime,
  shorter: Lifetime.Lifetime,
): boolean => {
  self.work.outlivesObligations += 1
  if (Lifetime.outlives(self.assumptions, longer, shorter)) return true
  if (longer._tag === 'PlaceholderLifetime' || shorter._tag === 'PlaceholderLifetime') return false
  const proven = self.outlives?.(longer, shorter) ?? false
  if (proven && self.commitOutlives !== undefined)
    retain(self, [() => self.commitOutlives?.(longer, shorter)])
  return proven
}

/** Proves one selected structural data-validity obligation in the current comparison scope. */
export const typeOutlives = (self: Context, bound: Type.TypeOutlives): boolean => {
  if (
    Type.satisfiesOutlives(bound.type, bound.lifetime, self.typeBounds, (longer, shorter) =>
      Lifetime.outlives(self.assumptions, longer, shorter),
    )
  )
    return true
  const proven = self.typeOutlives?.(bound.type, bound.lifetime) ?? false
  if (proven && self.commitTypeOutlives !== undefined)
    retain(self, [() => self.commitTypeOutlives?.(bound.type, bound.lifetime)])
  return proven
}

const nestedQuantifier = (self: Type.Type): boolean =>
  Type.someSubterm(
    self,
    (type) => (Type.isCallable(type) || Type.isEffect(type)) && type.lifetimeBinders.length > 0,
  )

const equivalent = (source: Type.Type, target: Type.Type, self: Context): boolean =>
  isCompatible(check(source, target, self)) && isCompatible(check(target, source, self))

const genericCompatible = (
  source: Type.GenericArgument,
  target: Type.GenericArgument,
  variance: Variance,
  self: Context,
): boolean => {
  if (variance === 'Bivariant') return true
  if (variance === 'Invariant') {
    if (Lifetime.isLifetime(source) || Lifetime.isLifetime(target))
      return (
        Lifetime.isLifetime(source) &&
        Lifetime.isLifetime(target) &&
        outlives(self, source, target) &&
        outlives(self, target, source)
      )
    return Type.isTypeArgument(source) && Type.isTypeArgument(target)
      ? equivalent(source, target, self)
      : Type.equalsGenericArgument(source, target)
  }
  if (variance === 'Contravariant') return genericCompatible(target, source, 'Covariant', self)
  if (Lifetime.isLifetime(source) || Lifetime.isLifetime(target))
    return (
      Lifetime.isLifetime(source) && Lifetime.isLifetime(target) && outlives(self, source, target)
    )
  return Type.isTypeArgument(source) && Type.isTypeArgument(target)
    ? isCompatible(check(source, target, self))
    : Type.equalsGenericArgument(source, target)
}

/** Compares one explicitly expected finite quantified callable without inference or resolution. */
const callableCompatible = (
  source: Type.Callable,
  target: Type.Callable,
  self: Context,
): boolean => {
  if (source.lifetimeBinders.length > 0 && target.lifetimeBinders.length === 0) {
    const instantiated = TypeInference.instantiateOfferedCallable(source, target, self)
    return instantiated !== undefined && callableCompatible(instantiated, target, self)
  }
  if (
    source.parameters.length !== target.parameters.length ||
    (source.unsafe && !target.unsafe) ||
    !Type.compareAccess(target.mode, source.mode)
  )
    return false
  if (!outlives(self, source.environment, target.environment)) return false
  if (
    (source.lifetimeBinders.length !== 0 || target.lifetimeBinders.length !== 0) &&
    [...source.parameters, source.result, ...target.parameters, target.result].some(
      nestedQuantifier,
    )
  )
    return false
  if (
    source.lifetimeBinders.length !== target.lifetimeBinders.length &&
    source.lifetimeBinders.length !== 0
  )
    return false
  const sourceSubstitution = new Map<string, Lifetime.Lifetime>()
  const targetSubstitution = new Map<string, Lifetime.Lifetime>()
  const universe = Canonical.record('CallableComparison', [
    Type.key(source),
    Type.key(target),
    self.assumptions.key,
  ])
  for (const [ordinal, binder] of target.lifetimeBinders.entries()) {
    const rigid = Lifetime.placeholder(binder, universe)
    self.work.rigidBinders += 1
    targetSubstitution.set(Lifetime.key(binder), rigid)
    const offered = source.lifetimeBinders.at(ordinal)
    if (offered !== undefined) sourceSubstitution.set(Lifetime.key(offered), rigid)
  }
  const substitutedBounds = (
    bounds: ReadonlyArray<Lifetime.Outlives>,
    substitution: ReadonlyMap<string, Lifetime.Lifetime>,
  ): ReadonlyArray<Lifetime.Outlives> =>
    bounds.map((bound) => ({
      longer: Lifetime.substitute(bound.longer, substitution),
      shorter: Lifetime.substitute(bound.shorter, substitution),
    }))
  const substituteTypeBounds = (
    bounds: ReadonlyArray<Type.TypeOutlives>,
    substitution: ReadonlyMap<string, Lifetime.Lifetime>,
  ): ReadonlyArray<Type.TypeOutlives> =>
    bounds.map((bound) => ({
      type: Type.substituteLifetimes(bound.type, substitution),
      lifetime: Lifetime.substitute(bound.lifetime, substitution),
    }))
  const formation = Type.executableFormationRequirements(source)
  const scoped = Object.freeze({
    ...self,
    typeBounds: [
      ...self.typeBounds,
      ...substituteTypeBounds(target.typeOutlives, targetSubstitution),
      ...formation.typeOutlives,
    ],
    assumptions: Lifetime.assumptions([
      ...self.assumptions.bounds,
      ...substitutedBounds(target.lifetimeBounds, targetSubstitution),
      ...formation.lifetimeBounds,
    ]),
  })
  if (
    !substitutedBounds(source.lifetimeBounds, sourceSubstitution).every((bound) =>
      outlives(scoped, bound.longer, bound.shorter),
    ) ||
    !substituteTypeBounds(source.typeOutlives, sourceSubstitution).every((bound) =>
      typeOutlives(scoped, bound),
    )
  )
    return false
  // Placeholders are introduced only into compared signature positions, never into the caller's
  // substitution or published types. Free surrounding lifetimes therefore cannot absorb them.
  return (
    source.parameters.every((parameter, ordinal) => {
      const expected = target.parameters.at(ordinal)
      return (
        expected !== undefined &&
        isCompatible(
          check(
            Type.substituteLifetimes(expected, targetSubstitution),
            Type.substituteLifetimes(parameter, sourceSubstitution),
            scoped,
          ),
        )
      )
    }) &&
    isCompatible(
      check(
        Type.substituteLifetimes(source.result, sourceSubstitution),
        Type.substituteLifetimes(target.result, targetSubstitution),
        scoped,
      ),
    )
  )
}

/** Checks a selected type relationship against finite, memoized semantic obligations. */
export const check = (
  source: Type.Type,
  target: Type.Type,
  self: Context = context(),
): Compatibility => {
  const identity = Canonical.record('Comparison', [
    Type.key(source),
    Type.key(target),
    self.assumptions.key,
  ])
  const cache = comparisons.get(self) ?? new Map<string, Proof>()
  comparisons.set(self, cache)
  const previous = cache.get(identity)
  if (previous !== undefined) {
    self.work.cacheHits += 1
    retain(self, previous.obligations)
    return previous.result
  }
  self.work.comparisons += 1
  return commitWhen(
    self,
    () => {
      const result = compareSelected(source, target, self)
      const obligations = isCompatible(result)
        ? [...(proofFrames.get(self.work)?.at(-1) ?? [])]
        : []
      cache.set(identity, { result, obligations })
      return result
    },
    isCompatible,
  )
}

const compareSelected = (source: Type.Type, target: Type.Type, self: Context): Compatibility => {
  if (
    Type.isRepresented(source) &&
    Type.isRepresented(target) &&
    Type.equalsGenericArgument(source.representation.argument, target.representation.argument) &&
    isCompatible(check(source.contract, target.contract, self)) &&
    Type.representationAdmissibility(source.contract, target.representation.requiredBound, self)
      ._tag === 'Admitted'
  )
    return Object.freeze({ _tag: 'Lifetime', source, target })
  if (Type.isRepresented(source) && !Type.isRepresented(target) && !Type.isUnion(target))
    return check(source.contract, target, self)
  const unsupported = (type: Type.Type): boolean =>
    Type.someSubterm(
      type,
      (part) =>
        Type.isCallable(part) &&
        part.lifetimeBinders.length > 0 &&
        [...part.parameters, part.result].some(nestedQuantifier),
    )
  if (unsupported(source) || unsupported(target))
    return Object.freeze({ _tag: 'Incompatible', source, target, missing: Object.freeze([source]) })
  if (Type.equals(source, target)) return Object.freeze({ _tag: 'Exact', source, target })
  if (Type.isNever(source)) return Object.freeze({ _tag: 'Bottom', source, target })
  if (
    Type.isString(source) &&
    Type.isString(target) &&
    outlives(self, source.lifetime, target.lifetime)
  )
    return Object.freeze({ _tag: 'Lifetime', source, target })
  if (
    Type.isReference(source) &&
    Type.isReference(target) &&
    Type.compareAccess(source.access, target.access) &&
    outlives(self, source.lifetime, target.lifetime) &&
    (target.access === 'Exclusive'
      ? equivalent(source.target, target.target, self)
      : isCompatible(check(source.target, target.target, self)))
  )
    return Object.freeze({ _tag: 'ReferenceAccess', source, target })
  if (
    Type.isSlice(source) &&
    Type.isSlice(target) &&
    Type.compareAccess(source.access, target.access) &&
    outlives(self, source.lifetime, target.lifetime) &&
    (target.access === 'Exclusive'
      ? equivalent(source.element, target.element, self)
      : isCompatible(check(source.element, target.element, self)))
  )
    return Object.freeze({ _tag: 'Lifetime', source, target })
  if (
    Type.isFixedArray(source) &&
    Type.isFixedArray(target) &&
    source.length === target.length &&
    isCompatible(check(source.element, target.element, self))
  )
    return Object.freeze({ _tag: 'Lifetime', source, target })
  if (
    Type.isNominal(source) &&
    Type.isNominal(target) &&
    nominalVarianceKey(source) === nominalVarianceKey(target) &&
    source.arguments.length === target.arguments.length
  ) {
    const variance =
      self.nominalVariance.get(nominalVarianceKey(source)) ??
      (Type.isSlot(source) ? (['Covariant', 'Invariant'] as const) : undefined)
    if (
      source.arguments.every((argument, ordinal) => {
        const expected = target.arguments.at(ordinal)
        return (
          expected !== undefined &&
          genericCompatible(argument, expected, variance?.at(ordinal) ?? 'Invariant', self)
        )
      })
    )
      return Object.freeze({ _tag: 'Lifetime', source, target })
  }
  // Raw pointer pointees remain invariant at the immediate mutability-widening boundary.
  if (
    Type.isPointer(source) &&
    Type.isPointer(target) &&
    Type.pointerQualifiersWeaken(source, target) &&
    equivalent(source.pointee, target.pointee, self)
  )
    return Object.freeze({ _tag: 'PointerWeakening', source, target })
  if (
    Type.isCallable(source) &&
    Type.isCallable(target) &&
    callableCompatible(source, target, self)
  )
    return Object.freeze({ _tag: 'CallableMode', source, target })
  if (Type.isEffect(source) && Type.isEffect(target)) {
    const sameOutputs =
      isCompatible(check(source.success, target.success, self)) &&
      isCompatible(check(Type.failureType(source), Type.failureType(target), self))
    const compatibleRequirements =
      Type.requirementMembers(source).every((requirement) =>
        Type.requirementMembers(target).some(
          (expected) =>
            Type.equals(requirement.capability, expected.capability) &&
            requirement.role === expected.role &&
            Type.requirementSatisfies(expected, requirement),
        ),
      ) &&
      Type.requirementRowParameters(source).every((parameter) =>
        Type.requirementRowParameters(target).some((expected) => Type.equals(parameter, expected)),
      )
    const formation = Type.executableFormationRequirements(source)
    const boundsContext = Object.freeze({
      ...self,
      typeBounds: [...self.typeBounds, ...target.typeOutlives, ...formation.typeOutlives],
      assumptions: Lifetime.assumptions([
        ...self.assumptions.bounds,
        ...target.lifetimeBounds,
        ...formation.lifetimeBounds,
      ]),
    })
    if (
      source.lifetimeBinders.length === 0 &&
      target.lifetimeBinders.length === 0 &&
      source.lifetimeBounds.every((bound) =>
        outlives(boundsContext, bound.longer, bound.shorter),
      ) &&
      source.typeOutlives.every((bound) => typeOutlives(boundsContext, bound)) &&
      outlives(self, source.environment, target.environment) &&
      Type.compareAccess(target.access, source.access) &&
      sameOutputs &&
      compatibleRequirements
    )
      return Object.freeze({ _tag: 'EffectAccess', source, target })
  }
  const members = sourceMembers(source)
  if (members === undefined || !Type.isUnion(target)) {
    return Object.freeze({
      _tag: 'Incompatible',
      source,
      target,
      missing: Object.freeze([source]),
    })
  }
  const targetOrdinalOf = (member: Type.Type): number | undefined => {
    const exact = target.members.findIndex((candidate) => Type.equals(candidate, member))
    if (exact >= 0) return exact
    const related = target.members.findIndex((candidate) =>
      isCompatible(check(member, candidate, self)),
    )
    if (related >= 0) return related
    if (!Type.isRepresented(member)) return undefined
    const opaque = target.members.findIndex(
      (candidate) =>
        Type.isRepresented(candidate) &&
        Type.isOpaqueRepresentationArgument(candidate.representation.argument) &&
        Type.equals(candidate.contract, member.contract),
    )
    if (opaque >= 0) return opaque
    const contract = target.members.findIndex(
      (candidate) => !Type.isRepresented(candidate) && Type.equals(candidate, member.contract),
    )
    return contract < 0 ? undefined : contract
  }
  const missing = members.filter((member) => targetOrdinalOf(member) === undefined)
  if (missing.length > 0) {
    return Object.freeze({
      _tag: 'Incompatible',
      source,
      target,
      missing: Object.freeze(missing),
    })
  }
  const mappings = Object.freeze(
    members.flatMap((member, sourceOrdinal): ReadonlyArray<MemberMapping> => {
      const targetOrdinal = targetOrdinalOf(member)
      return targetOrdinal === undefined
        ? []
        : [
            Object.freeze({
              _tag: 'UnionMemberMapping',
              source: member,
              sourceOrdinal,
              target: target.members[targetOrdinal] ?? member,
              targetOrdinal,
            }),
          ]
    }),
  )
  if (!Type.isUnion(source) && !Type.isNever(source)) {
    return Object.freeze({ _tag: 'Inject', source, target, mappings })
  }
  if (Type.isUnion(source) || Type.isNever(source)) {
    return Object.freeze({ _tag: 'Widen', source, target, mappings })
  }
  return Object.freeze({
    _tag: 'Incompatible',
    source,
    target,
    missing: Object.freeze([source]),
  })
}

/** Whether a compatibility result permits the expected-context use. */
export const isCompatible = (self: Compatibility): boolean => self._tag !== 'Incompatible'
