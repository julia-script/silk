import * as Type from './Type.js'

/** One canonical source-member to target-member relationship for an implicit union conversion. */
export interface MemberMapping {
  readonly _tag: 'UnionMemberMapping'
  readonly source: Type.Nominal
  readonly sourceOrdinal: number
  readonly target: Type.Nominal
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
      readonly source: Type.Nominal
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

const sourceMembers = (source: Type.Type): ReadonlyArray<Type.Nominal> | undefined => {
  if (Type.isNever(source)) return Object.freeze([])
  if (Type.isNominal(source)) return Object.freeze([source])
  if (Type.isUnion(source)) return source.members
  return undefined
}

/** Checks exact identity, nominal injection, or monotonic union widening without inference. */
export const check = (source: Type.Type, target: Type.Type): Compatibility => {
  if (Type.equals(source, target)) return Object.freeze({ _tag: 'Exact', source, target })
  if (Type.isNever(source)) return Object.freeze({ _tag: 'Bottom', source, target })
  if (Type.isCallable(source) && Type.isCallable(target)) {
    const sourceRank = source.mode === 'Shared' ? 0 : source.mode === 'Exclusive' ? 1 : 2
    const targetRank = target.mode === 'Shared' ? 0 : target.mode === 'Exclusive' ? 1 : 2
    if (
      sourceRank <= targetRank &&
      source.parameters.length === target.parameters.length &&
      source.parameters.every((parameter, index) =>
        Type.equals(parameter, target.parameters.at(index) ?? 'never'),
      ) &&
      Type.equals(source.result, target.result)
    ) {
      return Object.freeze({ _tag: 'CallableMode', source, target })
    }
  }
  if (Type.isEffect(source) && Type.isEffect(target)) {
    const sourceRank = source.access === 'Shared' ? 0 : source.access === 'Exclusive' ? 1 : 2
    const targetRank = target.access === 'Shared' ? 0 : target.access === 'Exclusive' ? 1 : 2
    const sameOutputs =
      Type.equals(source.success, target.success) &&
      Type.failureMembers(source).length === Type.failureMembers(target).length &&
      Type.failureMembers(source).every((failure, index) =>
        Type.equals(failure, Type.failureMembers(target).at(index) ?? 'never'),
      ) &&
      Type.failureRowParameters(source).length === Type.failureRowParameters(target).length &&
      Type.failureRowParameters(source).every((parameter, index) =>
        Type.equals(parameter, Type.failureRowParameters(target).at(index) ?? 'never'),
      )
    const compatibleRequirements =
      Type.requirementMembers(source).length === Type.requirementMembers(target).length &&
      Type.requirementMembers(source).every((requirement, index) => {
        const expected = Type.requirementMembers(target).at(index)
        return (
          expected !== undefined &&
          Type.equals(requirement.capability, expected.capability) &&
          requirement.role === expected.role &&
          (requirement.access === expected.access || expected.access === 'Exclusive')
        )
      }) &&
      Type.requirementRowParameters(source).length ===
        Type.requirementRowParameters(target).length &&
      Type.requirementRowParameters(source).every((parameter, index) =>
        Type.equals(parameter, Type.requirementRowParameters(target).at(index) ?? 'never'),
      )
    if (sourceRank <= targetRank && sameOutputs && compatibleRequirements)
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
  const targetOrdinals = new Map(
    target.members.map((member, ordinal) => [Type.key(member), ordinal] as const),
  )
  const missing = members.filter((member) => !targetOrdinals.has(Type.key(member)))
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
      const targetOrdinal = targetOrdinals.get(Type.key(member))
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
  if (Type.isNominal(source)) {
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
