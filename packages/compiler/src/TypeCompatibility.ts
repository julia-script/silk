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

/** Checks exact identity, nominal injection, or monotonic union widening without inference. */
export const check = (source: Type.Type, target: Type.Type): Compatibility => {
  if (Type.isRepresented(source) && !Type.isRepresented(target) && !Type.isUnion(target))
    return check(source.contract, target)
  if (Type.equals(source, target)) return Object.freeze({ _tag: 'Exact', source, target })
  if (Type.isNever(source)) return Object.freeze({ _tag: 'Bottom', source, target })
  if (
    Type.isReference(source) &&
    Type.isReference(target) &&
    Type.compareAccess(source.access, target.access) &&
    Type.equals(source.target, target.target)
  )
    return Object.freeze({ _tag: 'ReferenceAccess', source, target })
  if (Type.isCallable(source) && Type.isCallable(target)) {
    if (
      (!source.unsafe || target.unsafe) &&
      Type.compareAccess(target.mode, source.mode) &&
      source.parameters.length === target.parameters.length &&
      source.parameters.every((parameter, index) =>
        Type.equals(parameter, target.parameters.at(index) ?? 'never'),
      ) &&
      isCompatible(check(source.result, target.result))
    ) {
      return Object.freeze({ _tag: 'CallableMode', source, target })
    }
  }
  if (Type.isEffect(source) && Type.isEffect(target)) {
    const sameOutputs =
      Type.equals(source.success, target.success) &&
      Type.equals(Type.failureType(source), Type.failureType(target))
    const compatibleRequirements =
      Type.requirementMembers(source).length === Type.requirementMembers(target).length &&
      Type.requirementMembers(source).every((requirement, index) => {
        const expected = Type.requirementMembers(target).at(index)
        return (
          expected !== undefined &&
          Type.equals(requirement.capability, expected.capability) &&
          requirement.role === expected.role &&
          Type.requirementSatisfies(expected, requirement)
        )
      }) &&
      Type.requirementRowParameters(source).length ===
        Type.requirementRowParameters(target).length &&
      Type.requirementRowParameters(source).every((parameter, index) =>
        Type.equals(parameter, Type.requirementRowParameters(target).at(index) ?? 'never'),
      )
    if (Type.compareAccess(target.access, source.access) && sameOutputs && compatibleRequirements)
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
