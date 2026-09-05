import { assert, it } from '@effect/vitest'
import * as Lifetime from '../src/Lifetime.js'
import * as Match from '../src/Match.js'
import * as Type from '../src/Type.js'

const token = Type.nominal('main', 'Token')
const end = Type.nominal('main', 'End')

it('erases only lifetime identity when matching checked runtime variants', () => {
  const leaf = (lifetime: Lifetime.Lifetime, element: Type.Type, name = 'Value') => {
    const type = Type.nominal('main', 'Choice', [Type.reference('Shared', element, lifetime)])
    return Match.nominalUnionVariant(
      type,
      type,
      {
        _tag: 'CanonicalUnionVariantId',
        union: { _tag: 'CanonicalDeclarationId', module: 'main', name: 'Choice' },
        name,
      },
      0,
    )
  }
  const authored = leaf(Lifetime.bound({ module: 'main', name: 'inspect' }, 0, 'a'), 'i32')
  const physical = leaf(Lifetime.staticLifetime, 'i32')
  const decision = { member: authored, universal: false, guarded: false }
  assert.strictEqual(Match.identityEquals(authored, physical), false)
  assert.strictEqual(Match.cover([physical], [decision]).exhaustive, false)
  assert.strictEqual(Match.cover([physical], [decision], 'Runtime').exhaustive, true)
  assert.strictEqual(
    Match.selects(authored, leaf(Lifetime.staticLifetime, 'bool'), 'Runtime'),
    false,
  )
  assert.strictEqual(
    Match.selects(authored, leaf(Lifetime.staticLifetime, 'i32', 'Empty'), 'Runtime'),
    false,
  )
})

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
  const local = Lifetime.local({ module: 'main', name: 'join' }, 'body', 0)
  for (const view of [
    Type.string,
    (region: Lifetime.Lifetime) => Type.reference('Shared', 'i32', region),
    (region: Lifetime.Lifetime) => Type.slice('Shared', 'u8', region),
  ]) {
    assert.deepEqual(Match.join([view(Lifetime.staticLifetime), view(local)]), {
      _tag: 'Joined',
      type: view(local),
    })
    assert.deepEqual(Match.join([view(local), view(Lifetime.staticLifetime)]), {
      _tag: 'Joined',
      type: view(local),
    })
  }
  for (const unrelated of [
    Lifetime.local({ module: 'main', name: 'join' }, 'body', 1),
    Lifetime.bound({ module: 'main', name: 'join' }, 0, 'a'),
  ]) {
    const separate = Match.join([Type.string(local), Type.string(unrelated)])
    assert.strictEqual(separate._tag, 'Joined')
    if (separate._tag === 'Joined') assert.isTrue(Type.isUnion(separate.type))
  }
  const ordinary = Match.join(['i32', 'bool'])
  assert.strictEqual(ordinary._tag, 'Joined')
  if (ordinary._tag === 'Joined') assert.strictEqual(Type.encode(ordinary.type), 'bool | i32')
})
