import * as Canonical from './internal/Canonical.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Type from './Type.js'

export type ProviderMode = 'Shared' | 'Exclusive' | 'Take'

export interface NominalMember {
  readonly _tag: 'NominalMemberConstraint'
  readonly selected: Type.Type
  readonly source: Type.FailureRow
}

export interface FailureSubset {
  readonly _tag: 'FailureSubsetConstraint'
  readonly selected: Type.FailureRow
  readonly source: Type.FailureRow
}

export interface RequirementSubset {
  readonly _tag: 'RequirementSubsetConstraint'
  readonly selected: Type.RequirementsRow
  readonly source: Type.RequirementsRow
}

export interface ProviderSelection {
  readonly _tag: 'ProviderSelectionConstraint'
  readonly mode: ProviderMode
  readonly provider: Type.Type
  readonly selected: Type.RequirementsRow
  readonly source: Type.RequirementsRow
}

export type Constraint = NominalMember | FailureSubset | RequirementSubset | ProviderSelection

export interface Occurrence {
  readonly constraint: Constraint
  readonly origin: RowAlgebra.SourceOrigin
}

export type WitnessOrigin =
  | {
      readonly _tag: 'SourceWitness'
      readonly declaration: { readonly module: string; readonly name: string }
    }
  | { readonly _tag: 'IntrinsicWitness'; readonly operation: string }

/** Stable witness identity with every specialized generic argument needed for reachability. */
export interface WitnessIdentity {
  readonly origin: WitnessOrigin
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
}

export type ProviderMatch =
  | { readonly _tag: 'Identity' }
  | { readonly _tag: 'Conformance'; readonly witness: WitnessIdentity }

export type ConformanceOutcome =
  | { readonly _tag: 'NoMatch' }
  | { readonly _tag: 'Unique'; readonly match: ProviderMatch }
  | { readonly _tag: 'Ambiguous'; readonly witnesses: ReadonlyArray<WitnessIdentity> }
  | { readonly _tag: 'Invalid'; readonly reason: string }

export type ConstraintEvidence =
  | {
      readonly _tag: 'Assumed'
      readonly wantedKey: string
      readonly wanted: Constraint
      readonly substitution: Type.Substitution
    }
  | {
      readonly _tag: 'Member'
      readonly selected: Type.Nominal
      readonly source: Type.FailureRow
    }
  | {
      readonly _tag: 'FailureSubset'
      readonly selected: Type.FailureRow
      readonly source: Type.FailureRow
    }
  | {
      readonly _tag: 'RequirementSubset'
      readonly selected: Type.RequirementsRow
      readonly source: Type.RequirementsRow
    }
  | {
      readonly _tag: 'RequirementSelection'
      readonly wantedKey: string
      readonly wanted: ProviderSelection
      readonly selected: Type.Requirement
      readonly provider: Type.Type
      readonly providerMatch: ProviderMatch
      readonly providerMode: ProviderMode
    }

export const nominalMember = (selected: Type.Type, source: Type.FailureRow): NominalMember =>
  Object.freeze({ _tag: 'NominalMemberConstraint', selected, source })

export const failureSubset = (selected: Type.FailureRow, source: Type.FailureRow): FailureSubset =>
  Object.freeze({ _tag: 'FailureSubsetConstraint', selected, source })

export const requirementSubset = (
  selected: Type.RequirementsRow,
  source: Type.RequirementsRow,
): RequirementSubset => Object.freeze({ _tag: 'RequirementSubsetConstraint', selected, source })

export const providerSelection = (
  mode: ProviderMode,
  provider: Type.Type,
  selected: Type.RequirementsRow,
  source: Type.RequirementsRow,
): ProviderSelection =>
  Object.freeze({ _tag: 'ProviderSelectionConstraint', mode, provider, selected, source })

export const witnessKey = (self: WitnessIdentity): string =>
  Canonical.record('WitnessIdentity', [
    self.origin._tag === 'SourceWitness'
      ? Canonical.record('Source', [self.origin.declaration.module, self.origin.declaration.name])
      : Canonical.record('Intrinsic', [self.origin.operation]),
    Canonical.array(self.typeArguments.map(Type.genericArgumentKey)),
  ])

export const providerMatchKey = (self: ProviderMatch): string =>
  self._tag === 'Identity'
    ? Canonical.record('ProviderIdentity')
    : Canonical.record('ProviderConformance', [witnessKey(self.witness)])

const substitutionKey = (self: Type.Substitution): string =>
  Canonical.array(
    [...self.entries()]
      .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
      .map(([parameter, argument]) =>
        Canonical.record('SubstitutionEntry', [parameter, Type.genericArgumentKey(argument)]),
      ),
  )

/** Canonical serialization identity for symbolic and concrete proof data. */
export const evidenceKey = (self: ConstraintEvidence): string => {
  switch (self._tag) {
    case 'Assumed':
      return Canonical.record('AssumedEvidence', [
        self.wantedKey,
        key(self.wanted),
        substitutionKey(self.substitution),
      ])
    case 'Member':
      return Canonical.record('MemberEvidence', [
        Type.key(self.selected),
        RowAlgebra.key(Type.failureRowPolicy(), self.source),
      ])
    case 'FailureSubset':
      return Canonical.record('FailureSubsetEvidence', [
        RowAlgebra.key(Type.failureRowPolicy(), self.selected),
        RowAlgebra.key(Type.failureRowPolicy(), self.source),
      ])
    case 'RequirementSubset':
      return Canonical.record('RequirementSubsetEvidence', [
        RowAlgebra.key(Type.requirementRowPolicy(), self.selected),
        RowAlgebra.key(Type.requirementRowPolicy(), self.source),
      ])
    case 'RequirementSelection':
      return Canonical.record('RequirementSelectionEvidence', [
        self.wantedKey,
        key(self.wanted),
        Type.requirementRowPolicy().finite.memberKey(self.selected),
        Type.key(self.provider),
        providerMatchKey(self.providerMatch),
        self.providerMode,
      ])
  }
}

/** Span-independent semantic constraint identity. */
export const key = (self: Constraint): string => {
  switch (self._tag) {
    case 'NominalMemberConstraint':
      return Canonical.record('NominalMemberConstraint', [
        Type.key(self.selected),
        RowAlgebra.key(Type.failureRowPolicy(), self.source),
      ])
    case 'FailureSubsetConstraint':
      return Canonical.record('FailureSubsetConstraint', [
        RowAlgebra.key(Type.failureRowPolicy(), self.selected),
        RowAlgebra.key(Type.failureRowPolicy(), self.source),
      ])
    case 'RequirementSubsetConstraint':
      return Canonical.record('RequirementSubsetConstraint', [
        RowAlgebra.key(Type.requirementRowPolicy(), self.selected),
        RowAlgebra.key(Type.requirementRowPolicy(), self.source),
      ])
    case 'ProviderSelectionConstraint':
      return Canonical.record('ProviderSelectionConstraint', [
        self.mode,
        Type.key(self.provider),
        RowAlgebra.key(Type.requirementRowPolicy(), self.selected),
        RowAlgebra.key(Type.requirementRowPolicy(), self.source),
      ])
  }
}

export const assumed = (wanted: Constraint, substitution: Type.Substitution): ConstraintEvidence =>
  Object.freeze({ _tag: 'Assumed', wantedKey: key(wanted), wanted, substitution })

export const requirementSelectionEvidence = (
  wanted: ProviderSelection,
  selected: Type.Requirement,
  providerMatch: ProviderMatch,
): Extract<ConstraintEvidence, { readonly _tag: 'RequirementSelection' }> =>
  Object.freeze({
    _tag: 'RequirementSelection',
    wantedKey: key(wanted),
    wanted,
    selected,
    provider: wanted.provider,
    providerMatch,
    providerMode: wanted.mode,
  })

/** Applies only independently established substitutions; constraint solving never inverts rows. */
export const substitute = (self: Constraint, substitution: Type.Substitution): Constraint => {
  switch (self._tag) {
    case 'NominalMemberConstraint': {
      const selected = Type.substitute(self.selected, substitution)
      return nominalMember(selected, Type.substituteFailureRow(self.source, substitution))
    }
    case 'FailureSubsetConstraint':
      return failureSubset(
        Type.substituteFailureRow(self.selected, substitution),
        Type.substituteFailureRow(self.source, substitution),
      )
    case 'RequirementSubsetConstraint':
      return requirementSubset(
        Type.substituteRequirementsRow(self.selected, substitution),
        Type.substituteRequirementsRow(self.source, substitution),
      )
    case 'ProviderSelectionConstraint':
      return providerSelection(
        self.mode,
        Type.substitute(self.provider, substitution),
        Type.substituteRequirementsRow(self.selected, substitution),
        Type.substituteRequirementsRow(self.source, substitution),
      )
  }
}
