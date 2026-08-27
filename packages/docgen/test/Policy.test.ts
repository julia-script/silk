import { assert, it } from '@effect/vitest'
import * as Analysis from '@silklang/compiler/Analysis'
import * as Effect from 'effect/Effect'
import * as Policy from '../src/Policy.js'
import * as Project from '../src/Project.js'
import * as Fixture from './fixtures/PolicyFixture.js'

const encoder = new TextEncoder()

const checked = Effect.fnUntraced(function* (name: string, source: string) {
  const snapshot = yield* Analysis.ofSource(`policy/${name}`, encoder.encode(source))
  const project = Project.make(snapshot)
  const module = project.modules.find((candidate) => candidate.name === `policy/${name}`)
  assert.isDefined(module)
  return { snapshot, violations: Policy.check(module, snapshot, project) }
})

it.effect('accepts complete teaching structure and resolvable semantic links', () =>
  Effect.gen(function* () {
    const { violations } = yield* checked('complete', Fixture.complete)
    assert.deepStrictEqual(violations, [])
  }),
)

it.effect('allows omitted optional sections and obvious undocumented operands', () =>
  Effect.gen(function* () {
    const { violations } = yield* checked('omitted', Fixture.omittedSections)
    assert.deepStrictEqual(violations, [])
  }),
)

it.effect(
  'requires modules, public roots, public fields, and service operations but not helpers',
  () =>
    Effect.gen(function* () {
      const { violations } = yield* checked('missing', Fixture.missingCoverage)
      const identities = violations
        .filter((entry) => entry.code === 'MissingDocumentation')
        .map((entry) => entry.identity)
      assert.deepStrictEqual(identities, [
        'policy/missing',
        'policy/missing::Problem',
        'policy/missing::Problem::field:0',
        'policy/missing::Recovery',
        'policy/missing::Recovery::operation:recover',
      ])
      assert.isFalse(violations.some((entry) => entry.identity.includes('helper')))
      assert.isTrue(
        violations.every(
          (entry) => entry.source.sourceId === 'policy/missing' && entry.source.start >= 0,
        ),
      )
    }),
)

it.effect('reports summary, section, ownership, title, and fence-shape violations', () =>
  Effect.gen(function* () {
    const { violations } = yield* checked('shape', Fixture.malformedShape)
    const codes = new Set(violations.map((entry) => entry.code))
    for (const code of [
      'SummaryShape',
      'UnknownSection',
      'SectionOrder',
      'ParameterPlacement',
      'ExampleTitle',
      'FenceAttributes',
    ] as const)
      assert.isTrue(codes.has(code), `expected ${code}; saw ${[...codes].join(', ')}`)
  }),
)

it.effect('reports unresolved stdlib-style semantic links', () =>
  Effect.gen(function* () {
    const { violations } = yield* checked('links', Fixture.unresolvedLink)
    assert.deepStrictEqual(
      violations.filter((entry) => entry.code === 'UnresolvedLink').map((entry) => entry.identity),
      ['policy/links', 'policy/links::value'],
    )
  }),
)

it.effect('keeps ordinary compilation independent of maintained documentation policy', () =>
  Effect.gen(function* () {
    const { snapshot, violations } = yield* checked('independent', Fixture.malformedShape)
    assert.isAbove(violations.length, 0)
    assert.deepStrictEqual(Analysis.diagnostics(snapshot), [])
  }),
)
