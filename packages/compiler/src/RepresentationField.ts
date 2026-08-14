import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Type from './Type.js'

/** Stable identity of one directly represented nominal field. */
export interface Id {
  readonly _tag: 'RepresentedFieldId'
  readonly nominal: DeclarationIndex.CanonicalId
  readonly ordinal: number
}

/** Declaration-owned symbolic representation requirement for one field. */
export interface Plan {
  readonly _tag: 'RepresentationFieldPlan'
  readonly id: Id
  readonly parameter: Type.Parameter
  readonly requiredBound: Type.RepresentationBound
}

/** Source-only context for an unavailable resolution; never part of identity or lookup keys. */
export interface Provenance {
  readonly field: SourceSpan.SourceSpan
  readonly parameter: SourceSpan.SourceSpan
}

/** Why one symbolic represented field did not become a concrete specialization fact. */
export type UnavailableReason =
  | {
      readonly _tag: 'MissingRepresentationArgument'
      readonly requiredBound: Type.RepresentationBound
    }
  | {
      readonly _tag: 'OpenRepresentationArgument'
      readonly requiredBound: Type.RepresentationBound
    }
  | {
      readonly _tag: 'WrongRepresentationArgumentKind'
      readonly requiredBound: Type.RepresentationBound
    }
  | {
      readonly _tag: 'IncompatibleRepresentationArgument'
      readonly requiredBound: Type.RepresentationBound
      readonly detail: string
    }

/** One field representation resolved for a complete nominal instance. */
export type Resolution =
  | {
      readonly _tag: 'ResolvedRepresentationField'
      readonly id: Id
      readonly instance: Type.Nominal
      readonly argument: Type.ExactRepresentationArgument
      readonly requiredBound: Type.RepresentationBound
      readonly admissibility: Extract<
        Type.RepresentationAdmissibility,
        { readonly _tag: 'Admitted' }
      >
    }
  | {
      readonly _tag: 'UnavailableRepresentationField'
      readonly id: Id
      readonly instance: Type.Nominal
      readonly reason: UnavailableReason
      readonly provenance: Provenance
    }

/** Complete-instance lookup table for resolved and explicitly unavailable field representations. */
export interface Index {
  readonly _tag: 'RepresentationFieldIndex'
  readonly resolutions: ReadonlyArray<Resolution>
}

/** Constructs a stable field identity without using field spelling or source position. */
export const makeId = (nominal: DeclarationIndex.CanonicalId, ordinal: number): Id =>
  Object.freeze({ _tag: 'RepresentedFieldId', nominal: Object.freeze({ ...nominal }), ordinal })

/** Canonical identity key; source provenance is intentionally absent. */
export const idKey = (self: Id): string =>
  `${self.nominal.module}.${self.nominal.name}:field:${self.ordinal}`

/** Complete specialization key for one represented field. */
export const key = (instance: Type.Nominal, id: Id): string => `${Type.key(instance)}:${idKey(id)}`

const canonicalOf = (
  nominal: DeclarationIndex.CanonicalId | Type.Nominal,
): DeclarationIndex.CanonicalId =>
  nominal._tag === 'CanonicalDeclarationId'
    ? nominal
    : Object.freeze({
        _tag: 'CanonicalDeclarationId',
        module: nominal.module,
        name: nominal.name,
      })

interface SymbolicUse {
  readonly parameter: Type.Parameter
  readonly requiredBound: Type.RepresentationBound
}

const plansOfInternal = (
  declarations: DeclarationIndex.Index,
  nominal: DeclarationIndex.CanonicalId | Type.Nominal,
  seen: ReadonlySet<string>,
): ReadonlyArray<Plan> => {
  const canonical = canonicalOf(nominal)
  const canonicalKey = `${canonical.module}.${canonical.name}`
  if (seen.has(canonicalKey)) return Object.freeze([])
  const declaration = declarations.modules
    .flatMap((module) => module.structs)
    .find(
      (candidate) =>
        candidate.canonical._tag === 'Canonical' &&
        candidate.canonical.id.module === canonical.module &&
        candidate.canonical.id.name === canonical.name,
    )
  if (declaration === undefined) return Object.freeze([])
  const next = new Set(seen).add(canonicalKey)
  const symbolicUse = (type: Type.Type): SymbolicUse | undefined => {
    if (Type.isRepresented(type)) {
      const argument = type.representation.argument
      return Type.isRepresentationParameterArgument(argument)
        ? Object.freeze({
            parameter: argument.parameter,
            requiredBound: type.representation.requiredBound,
          })
        : undefined
    }
    if (Type.isFixedArray(type) || Type.isSlice(type)) return symbolicUse(type.element)
    if (Type.isUnion(type)) {
      for (const member of type.members) {
        const use = symbolicUse(member)
        if (use !== undefined) return use
      }
      return undefined
    }
    if (!Type.isNominal(type) || Type.isIntrinsicNominal(type)) return undefined
    const nested = declarations.modules
      .flatMap((module) => module.structs)
      .find(
        (candidate) =>
          candidate.canonical._tag === 'Canonical' &&
          candidate.canonical.id.module === type.module &&
          candidate.canonical.id.name === type.name,
      )
    if (nested === undefined) return undefined
    const substitution = Type.substitution(
      nested.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    )
    if (substitution === undefined) return undefined
    for (const plan of plansOfInternal(declarations, type, next)) {
      const argument = substitution.get(Type.key(plan.parameter))
      const bound = Type.substitute(plan.requiredBound, substitution)
      if (
        argument !== undefined &&
        Type.isRepresentationParameterArgument(argument) &&
        (Type.isCallable(bound) || Type.isEffect(bound))
      )
        return Object.freeze({ parameter: argument.parameter, requiredBound: bound })
    }
    return undefined
  }
  return Object.freeze(
    declaration.fields.flatMap((field, ordinal): ReadonlyArray<Plan> => {
      if (field.declaredType._tag !== 'Resolved') return []
      const use = symbolicUse(field.declaredType.type)
      if (use === undefined) return []
      return [
        Object.freeze({
          _tag: 'RepresentationFieldPlan' as const,
          id: makeId(canonical, ordinal),
          parameter: use.parameter,
          requiredBound: use.requiredBound,
        }),
      ]
    }),
  )
}

/** Collects source-ordered represented-field plans, including forwarded nested representations. */
export const plansOf = (
  declarations: DeclarationIndex.Index,
  nominal: DeclarationIndex.CanonicalId | Type.Nominal,
): ReadonlyArray<Plan> => plansOfInternal(declarations, nominal, new Set())

const provenanceOf = (declarations: DeclarationIndex.Index, plan: Plan): Provenance | undefined => {
  const declaration = declarations.modules
    .flatMap((module) => module.structs)
    .find(
      (candidate) =>
        candidate.canonical._tag === 'Canonical' &&
        candidate.canonical.id.module === plan.id.nominal.module &&
        candidate.canonical.id.name === plan.id.nominal.name,
    )
  const field = declaration?.fields.at(plan.id.ordinal)
  const parameter = declaration?.typeParameters.find(
    (candidate) => Type.key(candidate.type) === Type.key(plan.parameter),
  )
  return field === undefined || parameter === undefined
    ? undefined
    : Object.freeze({ field: field.syntax.span, parameter: parameter.syntax.span })
}

const resolvePlan = (
  declaration: DeclarationIndex.StructFact,
  instance: Type.Nominal,
  plan: Plan,
  provenance: Provenance,
): Resolution => {
  const substitution = new Map<string, Type.GenericArgument>(
    declaration.typeParameters.flatMap((parameter, ordinal) => {
      const argument = instance.arguments.at(ordinal)
      return argument === undefined ? [] : [[Type.key(parameter.type), argument]]
    }),
  )
  const substitutedBound = Type.substitute(plan.requiredBound, substitution)
  const requiredBound =
    Type.isCallable(substitutedBound) || Type.isEffect(substitutedBound)
      ? substitutedBound
      : plan.requiredBound
  const argument = substitution.get(Type.key(plan.parameter))
  const unavailable = (reason: UnavailableReason): Resolution =>
    Object.freeze({
      _tag: 'UnavailableRepresentationField',
      id: plan.id,
      instance,
      reason,
      provenance,
    })
  if (argument === undefined)
    return unavailable(Object.freeze({ _tag: 'MissingRepresentationArgument', requiredBound }))
  if (Type.isRepresentationParameterArgument(argument))
    return unavailable(Object.freeze({ _tag: 'OpenRepresentationArgument', requiredBound }))
  if (!Type.isExactRepresentationArgument(argument))
    return unavailable(Object.freeze({ _tag: 'WrongRepresentationArgumentKind', requiredBound }))
  const admissibility = Type.representationAdmissibility(argument.contract, requiredBound)
  if (admissibility._tag !== 'Admitted')
    return unavailable(
      Object.freeze({
        _tag: 'IncompatibleRepresentationArgument',
        requiredBound,
        detail:
          admissibility._tag === 'Unavailable'
            ? admissibility.reason
            : 'representation admissibility remained open',
      }),
    )
  return Object.freeze({
    _tag: 'ResolvedRepresentationField',
    id: plan.id,
    instance,
    argument,
    requiredBound,
    admissibility,
  })
}

/** Resolves represented fields for complete nominal instances, deduplicated by structural key. */
export const resolveFields = (
  declarations: DeclarationIndex.Index,
  instances: ReadonlyArray<Type.Nominal>,
): Index => {
  const resolutions = new Map<string, Resolution>()
  for (const instance of instances) {
    const declaration = declarations.modules
      .flatMap((module) => module.structs)
      .find(
        (candidate) =>
          candidate.canonical._tag === 'Canonical' &&
          candidate.canonical.id.module === instance.module &&
          candidate.canonical.id.name === instance.name,
      )
    if (declaration === undefined) continue
    for (const plan of plansOf(declarations, instance)) {
      const provenance = provenanceOf(declarations, plan)
      if (provenance === undefined) continue
      const resolved = resolvePlan(declaration, instance, plan, provenance)
      const resolutionKey = key(instance, plan.id)
      if (!resolutions.has(resolutionKey)) resolutions.set(resolutionKey, resolved)
    }
  }
  return Object.freeze({
    _tag: 'RepresentationFieldIndex',
    resolutions: Object.freeze([...resolutions.values()]),
  })
}

/** Looks one field resolution up by its complete nominal instance and stable field identity. */
export const lookup = (self: Index, instance: Type.Nominal, id: Id): Resolution | undefined => {
  const expected = key(instance, id)
  return self.resolutions.find((resolution) => key(resolution.instance, resolution.id) === expected)
}
