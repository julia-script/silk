import * as RowAlgebra from '../RowAlgebra.js'
import * as Type from '../Type.js'

/** Reads the semantic Effect contract of a direct or represented executable value. */
export const fromType = (type: Type.Type): Type.Effect | undefined => {
  const contract = Type.isRepresented(type) ? type.contract : type
  return Type.isEffect(contract) ? contract : undefined
}

/** Exact requirement evidence allowed to remove one row member from an executable Effect. */
export interface RequirementAuthorization {
  readonly capability: Type.Type
  readonly role: string
  readonly requirementAccess: Type.Requirement['access']
}

const sameRequirement = (left: Type.Requirement, right: Type.Requirement): boolean =>
  left.role === right.role &&
  left.access === right.access &&
  Type.equals(left.capability, right.capability)

/** Semantic machine identity after erasing only outer access and executable lifetime proofs. */
export const key = (self: Type.Effect): string =>
  JSON.stringify([
    Type.key(self.success),
    RowAlgebra.key(Type.failureRowPolicy(), self.failureRow),
    RowAlgebra.key(Type.requirementRowPolicy(), self.requirementRow),
  ])

/** Exact executable channels; outer access and executable lifetime proofs are non-runtime facts. */
export const equals = (left: Type.Effect, right: Type.Effect): boolean =>
  Type.equals(left.success, right.success) &&
  RowAlgebra.equals(Type.failureRowPolicy(), left.failureRow, right.failureRow) &&
  RowAlgebra.equals(Type.requirementRowPolicy(), left.requirementRow, right.requirementRow)

/** Proves that exact selected requirements account for the complete candidate/requested row delta. */
export const providerSubtractionMatches = (
  candidate: Type.Effect,
  requested: Type.Effect,
  authorizations: ReadonlyArray<RequirementAuthorization>,
): boolean => {
  if (
    !Type.equals(candidate.success, requested.success) ||
    !RowAlgebra.equals(Type.failureRowPolicy(), candidate.failureRow, requested.failureRow)
  )
    return false
  const candidateRequirements = Type.requirementMembers(candidate)
  const requestedRequirements = Type.requirementMembers(requested)
  if (
    !requestedRequirements.every((requirement) =>
      candidateRequirements.some((member) => sameRequirement(member, requirement)),
    )
  )
    return false
  const removed = candidateRequirements.filter(
    (requirement) => !requestedRequirements.some((member) => sameRequirement(member, requirement)),
  )
  return (
    removed.length > 0 &&
    removed.every((requirement) =>
      authorizations.some(
        (authorization) =>
          authorization.role === requirement.role &&
          authorization.requirementAccess === requirement.access &&
          Type.equals(authorization.capability, requirement.capability),
      ),
    ) &&
    RowAlgebra.equals(
      Type.requirementRowPolicy(),
      RowAlgebra.without(
        Type.requirementRowPolicy(),
        candidate.requirementRow,
        RowAlgebra.concrete(Type.requirementRowPolicy(), removed),
      ),
      requested.requirementRow,
    )
  )
}

/** Exact executable identity, optionally after a proven provider-row subtraction. */
export const matches = (
  candidate: Type.Effect,
  requested: Type.Effect,
  authorizations: ReadonlyArray<RequirementAuthorization> = [],
): boolean =>
  equals(candidate, requested) || providerSubtractionMatches(candidate, requested, authorizations)
