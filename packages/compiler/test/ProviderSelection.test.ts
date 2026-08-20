import { assert, it } from '@effect/vitest'
import * as Constraint from '../src/Constraint.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as ProviderSelection from '../src/ProviderSelection.js'
import * as RequirementRow from '../src/RequirementRow.js'
import * as RowAlgebra from '../src/RowAlgebra.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as Type from '../src/Type.js'
import { unreachable } from './support/raise.js'

const provider = Type.nominal('work', 'Provider')
const capability = (name: string): Type.Nominal => Type.nominal('work', name)
const requirement = (
  name: string,
  access: Type.Requirement['access'] = 'Exclusive',
  role = 'Default',
): Type.Requirement => ({ capability: capability(name), access, role })
const row = (members: ReadonlyArray<Type.Requirement>): Type.RequirementsRow =>
  RowAlgebra.concrete(Type.requirementRowPolicy(), members)
const selectedParameter = Type.parameter(
  { module: 'work', name: 'select' },
  0,
  'S',
  'RequirementRow',
)
const selected = RowAlgebra.parameter<
  Type.Requirement,
  Type.Parameter,
  Type.RequirementMemberShape
>(selectedParameter)
const origin = (start: number): SourceSpan.SourceSpan =>
  SourceSpan.fromOffsets('main', start, start + 1) ?? unreachable('expected a valid source span')
const applicationOrigin = origin(0)
const relation = (
  names: ReadonlyArray<string>,
  start: number,
  mode: Constraint.ProviderMode = 'Exclusive',
): ProviderSelection.Relation => ({
  wanted: Constraint.providerSelection(
    mode,
    provider,
    selected,
    row(names.map((name) => requirement(name))),
  ),
  origins: [origin(start)],
})

const witness = (name: string): Constraint.WitnessIdentity => ({
  origin: {
    _tag: 'SourceWitness',
    declaration: { module: 'work', name: `${name}Witness` },
  },
  typeArguments: [],
})

const oracle = (
  outcomes: Readonly<Record<string, Constraint.ConformanceOutcome>> = {},
): ProviderSelection.ConformanceOracle => ({
  match: (_provider, required) =>
    outcomes[required.name] ?? {
      _tag: 'Unique',
      match: { _tag: 'Conformance', witness: witness(required.name) },
    },
})

const memberKey = RequirementRow.policy<Type.Nominal | Type.Parameter>(Type.key).collisionKey

it('rejects an empty relation set as an internal invariant violation', () => {
  assert.throws(
    () =>
      ProviderSelection.solve({
        relations: [],
        responsible: applicationOrigin,
        oracle: oracle(),
      }),
    RangeError,
  )
})

it('intersects complete candidate maps before selecting a late common member', () => {
  const first = relation(['A', 'B', 'C'], 20)
  const second = relation(['C'], 10)
  const solved = ProviderSelection.solve({
    relations: [first, second],
    responsible: applicationOrigin,
    oracle: oracle(),
  })
  const permuted = ProviderSelection.solve({
    relations: [second, first],
    responsible: applicationOrigin,
    oracle: oracle(),
  })

  assert.strictEqual(solved._tag, 'Selected')
  assert.strictEqual(permuted._tag, 'Selected')
  if (solved._tag === 'Selected' && permuted._tag === 'Selected') {
    assert.strictEqual(Type.encode(solved.member.capability), 'work.C')
    assert.strictEqual(Type.encode(permuted.member.capability), 'work.C')
    assert.strictEqual(solved.evidence.length, 2)
    assert.deepEqual(
      solved.evidence.map((evidence) => evidence.wantedKey).sort(),
      permuted.evidence.map((evidence) => evidence.wantedKey).sort(),
    )
  }
})

it('reports every empty relation before irrelevant surviving statuses', () => {
  const noMatch = oracle({
    A: { _tag: 'NoMatch' },
    B: { _tag: 'NoMatch' },
    C: { _tag: 'Ambiguous', witnesses: [witness('First'), witness('Second')] },
  })
  const solved = ProviderSelection.solve({
    relations: [relation(['A'], 30), relation(['B'], 10), relation(['C'], 20)],
    responsible: applicationOrigin,
    oracle: noMatch,
  })

  assert.strictEqual(solved._tag, 'Rejected')
  if (solved._tag === 'Rejected')
    assert.deepEqual(
      solved.diagnostics.map((diagnostic) => diagnostic.problem._tag),
      ['ProviderNoMatch', 'ProviderNoMatch'],
    )
  if (solved._tag === 'Rejected')
    assert.deepEqual(
      solved.diagnostics.map(Diagnostic.providerSelection).map((item) => item.code),
      ['SEM0123', 'SEM0123'],
    )
})

it('separates span-free conflict payloads from local ordered locations', () => {
  const responsible = origin(5)
  const solved = ProviderSelection.solve({
    relations: [relation(['A'], 30), relation(['B'], 10)],
    responsible,
    oracle: oracle(),
  })

  assert.strictEqual(solved._tag, 'Rejected')
  if (solved._tag === 'Rejected') {
    const diagnostic = solved.diagnostics.at(0)
    assert.strictEqual(diagnostic?.problem._tag, 'JointSelectionConflict')
    if (diagnostic?.problem._tag === 'JointSelectionConflict') {
      assert.strictEqual(Diagnostic.providerSelection(diagnostic).code, 'SEM0124')
      assert.strictEqual(diagnostic.locations.primary, responsible)
      assert.deepEqual(
        diagnostic.problem.payload.relations.map((entry) => entry.fullCandidateKeySet),
        [[memberKey(requirement('A'))], [memberKey(requirement('B'))]],
      )
      assert.deepEqual(
        diagnostic.locations.relations.flatMap((entry) => entry.origins),
        [origin(30), origin(10)],
      )
    }
  }
})

it('reports common ambiguity plus each unequal full relation candidate set', () => {
  const solved = ProviderSelection.solve({
    relations: [relation(['A', 'B', 'C'], 10), relation(['A', 'B', 'D'], 20)],
    responsible: origin(5),
    oracle: oracle(),
  })

  assert.strictEqual(solved._tag, 'Rejected')
  if (solved._tag === 'Rejected') {
    const diagnostic = solved.diagnostics.at(0)
    assert.strictEqual(diagnostic?.problem._tag, 'ProviderAmbiguity')
    if (diagnostic?.problem._tag === 'ProviderAmbiguity') {
      assert.strictEqual(Diagnostic.providerSelection(diagnostic).code, 'SEM0125')
      assert.deepEqual(diagnostic.problem.payload.survivingCandidates, [
        memberKey(requirement('A')),
        memberKey(requirement('B')),
      ])
      assert.deepEqual(
        diagnostic.problem.payload.relations.map((entry) => entry.fullCandidateKeySet),
        [
          [memberKey(requirement('A')), memberKey(requirement('B')), memberKey(requirement('C'))],
          [memberKey(requirement('A')), memberKey(requirement('B')), memberKey(requirement('D'))],
        ],
      )
    }
  }
})

it('discards eliminated bad statuses but diagnoses surviving ambiguous and invalid matches', () => {
  const eliminated = ProviderSelection.solve({
    relations: [relation(['A', 'C'], 10), relation(['C'], 20)],
    responsible: applicationOrigin,
    oracle: oracle({
      A: { _tag: 'Ambiguous', witnesses: [witness('First'), witness('Second')] },
    }),
  })
  assert.strictEqual(eliminated._tag, 'Selected')

  for (const outcome of [
    { _tag: 'Ambiguous', witnesses: [witness('First'), witness('Second')] },
    { _tag: 'Invalid', reason: 'conditional witness failed' },
  ] satisfies ReadonlyArray<Constraint.ConformanceOutcome>) {
    const surviving = ProviderSelection.solve({
      relations: [relation(['C', 'D'], 10), relation(['C'], 20)],
      responsible: applicationOrigin,
      oracle: oracle({ C: outcome }),
    })
    assert.strictEqual(surviving._tag, 'Rejected')
    if (surviving._tag === 'Rejected')
      assert.deepEqual(
        surviving.diagnostics.map((diagnostic) => diagnostic.problem._tag),
        outcome._tag === 'Ambiguous'
          ? ['ConformanceAmbiguity', 'ConformanceAmbiguity']
          : ['InvalidConformance', 'InvalidConformance'],
      )
    if (surviving._tag === 'Rejected')
      assert.deepEqual(
        surviving.diagnostics.map(Diagnostic.providerSelection).map((item) => item.code),
        outcome._tag === 'Ambiguous' ? ['SEM0127', 'SEM0127'] : ['SEM0128', 'SEM0128'],
      )
  }
})

it('coalesces duplicate wanted keys and canonicalizes occurrence origins', () => {
  const duplicate = relation(['A'], 30)
  const grouped = ProviderSelection.groupRelations([
    duplicate,
    { wanted: duplicate.wanted, origins: [origin(20), origin(10)] },
  ])
  assert.strictEqual(grouped.length, 1)
  assert.deepEqual(grouped.at(0)?.origins, [origin(10), origin(20), origin(30)])

  const solved = ProviderSelection.solve({
    relations: grouped,
    responsible: applicationOrigin,
    oracle: oracle(),
  })
  assert.strictEqual(solved._tag, 'Selected')
  if (solved._tag === 'Selected') assert.strictEqual(solved.evidence.length, 1)
})

it('checks explicit selector cardinality and reports access after matching the key', () => {
  const base = relation(['A'], 10, 'Shared')
  const empty = ProviderSelection.solve({
    relations: [base],
    selected: row([]),
    responsible: applicationOrigin,
    oracle: oracle(),
  })
  const multiple = ProviderSelection.solve({
    relations: [base],
    selected: row([requirement('A'), requirement('B')]),
    responsible: applicationOrigin,
    oracle: oracle(),
  })
  const accessMismatch = ProviderSelection.solve({
    relations: [base],
    selected: row([requirement('A', 'Shared')]),
    responsible: applicationOrigin,
    oracle: oracle(),
  })

  for (const result of [empty, multiple]) {
    assert.strictEqual(result._tag, 'Rejected')
    if (result._tag !== 'Rejected') continue
    const diagnostic = result.diagnostics.at(0)
    assert.strictEqual(diagnostic?.problem._tag, 'SelectedRowCardinality')
    assert.strictEqual(
      diagnostic === undefined ? undefined : Diagnostic.providerSelection(diagnostic).code,
      'SEM0126',
    )
  }
  assert.strictEqual(accessMismatch._tag, 'Rejected')
  if (accessMismatch._tag === 'Rejected')
    assert.strictEqual(accessMismatch.diagnostics.at(0)?.problem._tag, 'ProviderAccessMismatch')
})

it('distinguishes exact provider identity from unique conformance evidence', () => {
  const exactMember = requirement('Provider', 'Shared')
  const exactWanted = Constraint.providerSelection('Shared', provider, selected, row([exactMember]))
  const exact = ProviderSelection.solve({
    relations: [{ wanted: exactWanted, origins: [origin(10)] }],
    responsible: applicationOrigin,
    oracle: oracle(),
  })
  const conformance = ProviderSelection.solve({
    relations: [relation(['A'], 10)],
    responsible: applicationOrigin,
    oracle: oracle(),
  })

  assert.strictEqual(exact._tag, 'Selected')
  assert.strictEqual(conformance._tag, 'Selected')
  if (exact._tag === 'Selected' && conformance._tag === 'Selected') {
    assert.strictEqual(exact.evidence.at(0)?.providerMatch._tag, 'Identity')
    assert.strictEqual(conformance.evidence.at(0)?.providerMatch._tag, 'Conformance')
  }
})
