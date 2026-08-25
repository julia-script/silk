import { assert, it } from '@effect/vitest'
import * as Match from '../src/Match.js'
import * as Type from '../src/Type.js'

const token = Type.nominal('main', 'Token')
const end = Type.nominal('main', 'End')

it('subtracts only reachable unguarded member decisions', () => {
  const coverage = Match.cover(
    [Match.structuralMember(token), Match.structuralMember(end)],
    [
      { member: Match.structuralMember(token), universal: false, guarded: true },
      { member: Match.structuralMember(token), universal: false, guarded: false },
      { member: Match.structuralMember(token), universal: false, guarded: true },
      { member: Match.structuralMember(end), universal: false, guarded: false },
    ],
  )

  assert.deepEqual(
    coverage.transitions.map((transition) => ({
      before: transition.before.map(Match.encodeIdentity),
      after: transition.after.map(Match.encodeIdentity),
      reachable: transition.reachable,
    })),
    [
      { before: ['main.Token', 'main.End'], after: ['main.Token', 'main.End'], reachable: true },
      { before: ['main.Token', 'main.End'], after: ['main.End'], reachable: true },
      { before: ['main.End'], after: ['main.End'], reachable: false },
      { before: ['main.End'], after: [], reachable: true },
    ],
  )
  assert.strictEqual(coverage.exhaustive, true)
})

it('treats an unguarded universal decision as terminal coverage', () => {
  const coverage = Match.cover(
    [Match.structuralMember(token), Match.structuralMember(end)],
    [
      { universal: true, guarded: false },
      { member: Match.structuralMember(token), universal: false, guarded: false },
    ],
  )

  assert.deepEqual(coverage.missing, [])
  assert.deepEqual(
    coverage.transitions.map((transition) => transition.reachable),
    [true, false],
  )
})

it('joins equal types precisely and nominal results canonically', () => {
  assert.deepEqual(Match.join(['i32', 'i32']), { _tag: 'Joined', type: 'i32' })
  const joined = Match.join([token, 'never', end])
  assert.strictEqual(joined._tag, 'Joined')
  if (joined._tag === 'Joined')
    assert.strictEqual(Type.encode(joined.type), 'main.End | main.Token')
  const ordinary = Match.join(['i32', 'bool'])
  assert.strictEqual(ordinary._tag, 'Joined')
  if (ordinary._tag === 'Joined') assert.strictEqual(Type.encode(ordinary.type), 'bool | i32')
})
