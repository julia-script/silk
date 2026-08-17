import type * as FiniteRow from './FiniteRow.js'

/** Access stored by a service requirement row. */
export type Access = 'Shared' | 'Exclusive'

/** Access used by a provider-selection constraint. */
export type ProviderAccess = Access | 'Take'

/** One exact capability-role-access requirement member. */
export interface Member<Capability> {
  readonly capability: Capability
  readonly role: string
  readonly access: Access
}

/** Constructs the finite-row policy for a capability identity domain. */
export const policy = <Capability>(
  capabilityKey: (capability: Capability) => string,
): FiniteRow.Policy<Member<Capability>> =>
  Object.freeze({
    collisionKey: (member: Member<Capability>) =>
      `${capabilityKey(member.capability)}@${member.role}`,
    memberKey: (member: Member<Capability>) =>
      `${capabilityKey(member.capability)}@${member.role}:${member.access}`,
    merge: (left: Member<Capability>, right: Member<Capability>): Member<Capability> =>
      Object.freeze({
        capability: left.capability,
        role: left.role,
        access:
          left.access === 'Exclusive' || right.access === 'Exclusive' ? 'Exclusive' : 'Shared',
      }),
  })

/** Whether a fixed provider mode may satisfy a stored requirement access. */
export const providerCanSelect = (providerAccess: ProviderAccess, storedAccess: Access): boolean =>
  providerAccess !== 'Shared' || storedAccess === 'Shared'
