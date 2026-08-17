import * as Constraint from './Constraint.js'
import type * as FiniteRow from './FiniteRow.js'
import * as Canonical from './internal/Canonical.js'
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Type from './Type.js'

export type CandidateStatus =
  | { readonly _tag: 'Unique'; readonly match: Constraint.ProviderMatch }
  | { readonly _tag: 'Ambiguous'; readonly witnesses: ReadonlyArray<Constraint.WitnessIdentity> }
  | { readonly _tag: 'Invalid'; readonly reason: string }

export interface Relation {
  readonly wanted: Constraint.ProviderSelection
  readonly origins: ReadonlyArray<RowAlgebra.SourceOrigin>
}

export interface CandidateRecord {
  readonly member: Type.Requirement
  readonly status: CandidateStatus
}

export interface RelationCandidates {
  readonly constraintKey: string
  readonly wanted: Constraint.ProviderSelection
  readonly origins: ReadonlyArray<RowAlgebra.SourceOrigin>
  readonly candidates: ReadonlyMap<string, CandidateRecord>
}

export interface RelationPayload {
  readonly constraintKey: string
  readonly fullCandidateKeySet: ReadonlyArray<string>
}

export interface DiagnosticLocations {
  readonly primary: RowAlgebra.SourceOrigin
  readonly relations: ReadonlyArray<{
    readonly constraintKey: string
    readonly origins: ReadonlyArray<RowAlgebra.SourceOrigin>
  }>
}

export type SelectionDiagnostic =
  | {
      readonly _tag: 'SelectedRowCardinality'
      readonly count: number
      readonly locations: DiagnosticLocations
    }
  | {
      readonly _tag: 'ProviderNoMatch'
      readonly constraintKey: string
      readonly locations: DiagnosticLocations
    }
  | {
      readonly _tag: 'JointSelectionConflict'
      readonly payload: { readonly relations: ReadonlyArray<RelationPayload> }
      readonly locations: DiagnosticLocations
    }
  | {
      readonly _tag: 'ConformanceAmbiguity'
      readonly memberKey: string
      readonly constraintKey: string
      readonly witnesses: ReadonlyArray<Constraint.WitnessIdentity>
      readonly locations: DiagnosticLocations
    }
  | {
      readonly _tag: 'InvalidConformance'
      readonly memberKey: string
      readonly constraintKey: string
      readonly reason: string
      readonly locations: DiagnosticLocations
    }
  | {
      readonly _tag: 'ProviderAmbiguity'
      readonly payload: {
        readonly survivingCandidates: ReadonlyArray<string>
        readonly relations: ReadonlyArray<RelationPayload>
      }
      readonly locations: DiagnosticLocations
    }

export type Result =
  | {
      readonly _tag: 'Selected'
      readonly member: Type.Requirement
      readonly evidence: ReadonlyArray<
        Extract<Constraint.ConstraintEvidence, { readonly _tag: 'RequirementSelection' }>
      >
    }
  | { readonly _tag: 'Rejected'; readonly diagnostics: ReadonlyArray<SelectionDiagnostic> }

export interface ConformanceOracle {
  readonly match: (provider: Type.Type, capability: Type.Nominal) => Constraint.ConformanceOutcome
}

const compareText = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0

const originKey = (origin: RowAlgebra.SourceOrigin): string =>
  Canonical.record('SourceOrigin', [origin.sourceId, `${origin.start}`, `${origin.end}`])

const canonicalOrigins = (
  origins: Iterable<RowAlgebra.SourceOrigin>,
): ReadonlyArray<RowAlgebra.SourceOrigin> =>
  Object.freeze(
    [...new Map([...origins].map((origin) => [originKey(origin), origin])).values()].sort(
      (left, right) => compareText(originKey(left), originKey(right)),
    ),
  )

const policy = RequirementRow.policy<NonNullable<Type.Requirement['capability']>>(Type.key)
const memberKey = policy.memberKey

const providerAccess = (mode: Constraint.ProviderMode): RequirementRow.ProviderAccess =>
  mode === 'Take' ? 'Take' : mode

const candidateStatus = (
  wanted: Constraint.ProviderSelection,
  member: Type.Requirement,
  oracle: ConformanceOracle,
): CandidateStatus | undefined => {
  if (!RequirementRow.providerCanSelect(providerAccess(wanted.mode), member.access))
    return undefined
  if (Type.equals(wanted.provider, member.capability))
    return Object.freeze({ _tag: 'Unique', match: Object.freeze({ _tag: 'Identity' }) })
  if (!Type.isNominal(member.capability)) return undefined
  const outcome = oracle.match(wanted.provider, member.capability)
  switch (outcome._tag) {
    case 'NoMatch':
      return undefined
    case 'Unique':
      return Object.freeze({ _tag: 'Unique', match: outcome.match })
    case 'Ambiguous':
      return Object.freeze({
        _tag: 'Ambiguous',
        witnesses: Object.freeze(
          [...outcome.witnesses].sort((left, right) =>
            compareText(Constraint.witnessKey(left), Constraint.witnessKey(right)),
          ),
        ),
      })
    case 'Invalid':
      return Object.freeze({ _tag: 'Invalid', reason: outcome.reason })
  }
}

const concreteSource = (
  wanted: Constraint.ProviderSelection,
): FiniteRow.FiniteRow<Type.Requirement> | undefined => {
  const concrete = RowAlgebra.concretize(Type.requirementRowPolicy(), wanted.source)
  return concrete._tag === 'Concrete' ? concrete.row : undefined
}

const relationCandidates = (relation: Relation, oracle: ConformanceOracle): RelationCandidates => {
  const candidates = new Map<string, CandidateRecord>()
  for (const member of concreteSource(relation.wanted)?.members ?? []) {
    const status = candidateStatus(relation.wanted, member, oracle)
    if (status !== undefined) candidates.set(memberKey(member), Object.freeze({ member, status }))
  }
  return Object.freeze({
    constraintKey: Constraint.key(relation.wanted),
    wanted: relation.wanted,
    origins: canonicalOrigins(relation.origins),
    candidates: new Map(
      [...candidates.entries()].sort(([left], [right]) => compareText(left, right)),
    ),
  })
}

/** Groups both textual duplicates and post-substitution semantic-key collisions. */
export const groupRelations = (relations: ReadonlyArray<Relation>): ReadonlyArray<Relation> => {
  const grouped = new Map<string, Relation>()
  for (const relation of relations) {
    const key = Constraint.key(relation.wanted)
    const existing = grouped.get(key)
    grouped.set(
      key,
      Object.freeze({
        wanted: existing?.wanted ?? relation.wanted,
        origins: canonicalOrigins([...(existing?.origins ?? []), ...relation.origins]),
      }),
    )
  }
  return Object.freeze(
    [...grouped.entries()]
      .sort(([left], [right]) => compareText(left, right))
      .map(([, relation]) => relation),
  )
}

/** Builds every relation's complete candidate map without emitting diagnostics. */
export const candidates = (
  relations: ReadonlyArray<Relation>,
  oracle: ConformanceOracle,
): ReadonlyArray<RelationCandidates> =>
  Object.freeze(groupRelations(relations).map((relation) => relationCandidates(relation, oracle)))

const payload = (relations: ReadonlyArray<RelationCandidates>): ReadonlyArray<RelationPayload> =>
  Object.freeze(
    relations.map((relation) =>
      Object.freeze({
        constraintKey: relation.constraintKey,
        fullCandidateKeySet: Object.freeze([...relation.candidates.keys()]),
      }),
    ),
  )

const locations = (
  relations: ReadonlyArray<RelationCandidates>,
  responsible?: RowAlgebra.SourceOrigin,
): DiagnosticLocations => {
  const first = relations.flatMap((relation) => relation.origins).at(0)
  const primary = responsible ?? first ?? Object.freeze({ sourceId: '', start: 0, end: 0 })
  return Object.freeze({
    primary,
    relations: Object.freeze(
      relations.map((relation) =>
        Object.freeze({
          constraintKey: relation.constraintKey,
          origins: relation.origins,
        }),
      ),
    ),
  })
}

const selectedFinite = (
  selected: Type.RequirementsRow | undefined,
): FiniteRow.FiniteRow<Type.Requirement> | undefined => {
  if (selected === undefined) return undefined
  const concrete = RowAlgebra.concretize(Type.requirementRowPolicy(), selected)
  return concrete._tag === 'Concrete' ? concrete.row : undefined
}

/** Solves one conjunctive selected-row variable after all ordinary substitutions are known. */
export const solve = (options: {
  readonly relations: ReadonlyArray<Relation>
  readonly selected?: Type.RequirementsRow
  readonly responsible?: RowAlgebra.SourceOrigin
  readonly oracle: ConformanceOracle
}): Result => {
  const maps = candidates(options.relations, options.oracle)
  const selected = selectedFinite(options.selected)
  if (options.selected !== undefined && (selected === undefined || selected.members.length !== 1))
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: Object.freeze([
        Object.freeze({
          _tag: 'SelectedRowCardinality',
          count: selected?.members.length ?? 0,
          locations: locations(maps, options.responsible),
        }),
      ]),
    })

  const selectedMember = selected?.members.at(0)
  const considered =
    selectedMember === undefined
      ? maps
      : maps.map((relation) => {
          const key = memberKey(selectedMember)
          const candidate = relation.candidates.get(key)
          return Object.freeze({
            ...relation,
            candidates: new Map(candidate === undefined ? [] : [[key, candidate]]),
          })
        })
  const empty = considered.filter((relation) => relation.candidates.size === 0)
  if (empty.length > 0)
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: Object.freeze(
        empty.map((relation) =>
          Object.freeze({
            _tag: 'ProviderNoMatch',
            constraintKey: relation.constraintKey,
            locations: locations([relation]),
          }),
        ),
      ),
    })

  const firstKeys = new Set(considered.at(0)?.candidates.keys() ?? [])
  const surviving = [...firstKeys]
    .filter((key) => considered.every((relation) => relation.candidates.has(key)))
    .sort(compareText)
  if (surviving.length === 0)
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: Object.freeze([
        Object.freeze({
          _tag: 'JointSelectionConflict',
          payload: Object.freeze({ relations: payload(considered) }),
          locations: locations(considered, options.responsible),
        }),
      ]),
    })

  const statusDiagnostics: Array<SelectionDiagnostic> = []
  for (const key of surviving)
    for (const relation of considered) {
      const status = relation.candidates.get(key)?.status
      if (status?._tag === 'Ambiguous')
        statusDiagnostics.push(
          Object.freeze({
            _tag: 'ConformanceAmbiguity',
            memberKey: key,
            constraintKey: relation.constraintKey,
            witnesses: status.witnesses,
            locations: locations([relation]),
          }),
        )
      if (status?._tag === 'Invalid')
        statusDiagnostics.push(
          Object.freeze({
            _tag: 'InvalidConformance',
            memberKey: key,
            constraintKey: relation.constraintKey,
            reason: status.reason,
            locations: locations([relation]),
          }),
        )
    }
  if (statusDiagnostics.length > 0)
    return Object.freeze({ _tag: 'Rejected', diagnostics: Object.freeze(statusDiagnostics) })
  if (surviving.length > 1)
    return Object.freeze({
      _tag: 'Rejected',
      diagnostics: Object.freeze([
        Object.freeze({
          _tag: 'ProviderAmbiguity',
          payload: Object.freeze({
            survivingCandidates: Object.freeze(surviving),
            relations: payload(considered),
          }),
          locations: locations(considered, options.responsible),
        }),
      ]),
    })

  const survivingKey = surviving.at(0)
  if (survivingKey === undefined)
    return Object.freeze({ _tag: 'Rejected', diagnostics: Object.freeze([]) })
  const first = considered.at(0)?.candidates.get(survivingKey)
  if (first === undefined)
    return Object.freeze({ _tag: 'Rejected', diagnostics: Object.freeze([]) })
  const evidence = considered.flatMap((relation) => {
    const candidate = relation.candidates.get(survivingKey)
    return candidate?.status._tag === 'Unique'
      ? [
          Constraint.requirementSelectionEvidence(
            relation.wanted,
            candidate.member,
            candidate.status.match,
          ),
        ]
      : []
  })
  return Object.freeze({
    _tag: 'Selected',
    member: first.member,
    evidence: Object.freeze(evidence),
  })
}
