import { assert, it } from '@effect/vitest'
import * as Type from '../src/Type.js'

const owner = Object.freeze({ module: 'representation/type', name: 'Pair' })
const value = Type.parameter(owner, 0, 'A')
const callableBound = Type.callable([value], value)
const representation = Type.parameter(owner, 1, 'F', 'CallableRepresentation', callableBound)
const open = Type.representationParameterArgument(representation)

const callableIdentity = (name: string) =>
  Type.callableIdentityArgument(`representation/type.${name}`, {
    _tag: 'Declaration',
    module: 'representation/type',
    name,
  })

it('separates exact identity, required bounds, and per-use admissibility', () => {
  const shared = Type.callable(['i32'], 'i32')
  const once = Type.callable(['i32'], 'i32', 'Take')
  const exact = Type.exactRepresentationArgument(callableIdentity('decode'), shared)
  const sharedUse = Type.represented(shared, shared, exact)
  const onceUse = Type.represented(shared, once, exact)

  assert.strictEqual(sharedUse.representation.admissibility._tag, 'Admitted')
  assert.strictEqual(onceUse.representation.admissibility._tag, 'Admitted')
  assert.strictEqual(Type.equals(sharedUse, onceUse), true)
  assert.strictEqual(
    Type.equalsGenericArgument(sharedUse.representation.argument, onceUse.representation.argument),
    true,
  )
  assert.notDeepEqual(sharedUse.representation.requiredBound, onceUse.representation.requiredBound)
})

it('intersects compatible public bounds at their most restrictive access', () => {
  const shared = Type.callable(['i32'], 'i32')
  const exclusive = Type.callable(['i32'], 'i32', 'Exclusive')
  const take = Type.callable(['i32'], 'i32', 'Take')
  const incompatible = Type.callable(['i32'], 'bool')
  const sharedEffect = Type.effect('i32', [])
  const takeEffect = Type.effect('i32', [], 'Take')

  assert.deepEqual(Type.intersectRepresentationBounds(take, exclusive), exclusive)
  assert.deepEqual(Type.intersectRepresentationBounds(take, shared), shared)
  assert.strictEqual(Type.intersectRepresentationBounds(shared, incompatible), undefined)
  assert.deepEqual(Type.intersectRepresentationBounds(takeEffect, sharedEffect), sharedEffect)
  assert.strictEqual(Type.intersectRepresentationBounds(shared, sharedEffect), undefined)
})

it('keeps one ordered kinded argument vector on nominal applications', () => {
  const failure = Type.parameter(owner, 2, 'E', 'FailureRow')
  const requirements = Type.parameter(owner, 3, 'R', 'RequirementRow')
  const exact = Type.exactRepresentationArgument(
    callableIdentity('decode'),
    Type.callable(['i32'], 'i32'),
  )
  const arguments_: ReadonlyArray<Type.GenericArgument> = [
    'i32',
    Type.failureRowArgument([Type.nominal('representation/type', 'Problem')]),
    Type.requirementRowArgument([
      {
        capability: Type.nominal('representation/type', 'Console'),
        role: 'DefaultRole',
        access: 'Shared',
      },
    ]),
    exact,
  ]
  const parameters = [value, failure, requirements, representation]
  const applied = Type.nominal('representation/type', 'Container', arguments_)

  assert.notStrictEqual(Type.substitution(parameters, arguments_), undefined)
  assert.strictEqual(applied.arguments.length, 4)
  assert.strictEqual(Type.isTypeArgument(applied.arguments.at(0) ?? exact), true)
  assert.strictEqual(Type.isFailureRowArgument(applied.arguments.at(1) ?? exact), true)
  assert.strictEqual(Type.isRequirementRowArgument(applied.arguments.at(2) ?? exact), true)
  assert.strictEqual(Type.isExactRepresentationArgument(applied.arguments.at(3) ?? 'i32'), true)
  assert.include(Type.key(applied), 'exact-representation:callable-identity:')
})

it('implements deterministic equality, ordering, hashing, encoding, and unavailable recovery', () => {
  const first = Type.exactRepresentationArgument(
    callableIdentity('first'),
    Type.callable(['i32'], 'i32'),
  )
  const repeated = Type.exactRepresentationArgument(
    callableIdentity('first'),
    Type.callable(['i32'], 'i32'),
  )
  const second = Type.exactRepresentationArgument(
    callableIdentity('second'),
    Type.callable(['i32'], 'i32'),
  )
  const differentContract = Type.exactRepresentationArgument(
    callableIdentity('first'),
    Type.callable(['i32'], 'bool'),
  )
  const unavailable = Type.unavailableGenericArgument('CallableRepresentation', 'damaged bound')

  assert.strictEqual(Type.equalsGenericArgument(first, repeated), true)
  assert.strictEqual(Type.equalsGenericArgument(first, differentContract), false)
  assert.strictEqual(Type.hashGenericArgument(first), Type.hashGenericArgument(repeated))
  assert.notStrictEqual(Type.hashGenericArgument(first), Type.hashGenericArgument(second))
  assert.isBelow(Type.compareGenericArgument(first, second), 0)
  assert.strictEqual(Type.encodeGenericArgument(first), 'typeof(representation/type.first)')
  assert.strictEqual(
    Type.encodeGenericArgument(unavailable),
    '<unavailable CallableRepresentation: damaged bound>',
  )
  assert.strictEqual(Type.isConcreteGenericArgument(unavailable), false)
  assert.strictEqual(Type.prefixSubstitution([representation], [unavailable]), undefined)
})

it('keys exact generic callable specializations by their complete normalized instance', () => {
  const genericIdentity = Type.callableIdentityArgument(
    'representation/type.identity',
    { _tag: 'Declaration', module: 'representation/type', name: 'identity' },
    [value],
  )
  const generic = Type.exactRepresentationArgument(genericIdentity, Type.callable([value], value))
  const concrete = Type.substituteGenericArgument(generic, new Map([[Type.key(value), 'i32']]))

  assert.strictEqual(Type.isExactRepresentationArgument(concrete), true)
  assert.notStrictEqual(Type.genericArgumentKey(generic), Type.genericArgumentKey(concrete))
  assert.notStrictEqual(
    Type.key(Type.nominal('representation/type', 'Parser', [generic])),
    Type.key(Type.nominal('representation/type', 'Parser', [concrete])),
  )
  assert.include(Type.genericArgumentKey(concrete), 'arguments=<builtin:i32>')
  assert.include(Type.genericArgumentKey(concrete), 'callable:Shared<(builtin:i32)->builtin:i32>')
})

it('preserves an exact intrinsic contract when substituting a narrower contextual use bound', () => {
  const takeBound = Type.callable([value], value, 'Take')
  const sharedContract = Type.callable(['i32'], 'i32')
  const exact = Type.exactRepresentationArgument(callableIdentity('shared'), sharedContract)
  const represented = Type.represented(callableBound, takeBound, open)
  const substituted = Type.substitute(
    represented,
    new Map<string, Type.GenericArgument>([
      [Type.key(value), 'i32'],
      [Type.key(representation), exact],
    ]),
  )

  assert.strictEqual(Type.isRepresented(substituted), true)
  if (!Type.isRepresented(substituted)) return
  assert.deepEqual(substituted.contract, sharedContract)
  assert.strictEqual(substituted.representation.admissibility._tag, 'Admitted')
})

it('finds representation divergence through Effect rows and structural unions', () => {
  const first = Type.exactRepresentationArgument(
    callableIdentity('first'),
    Type.callable(['i32'], 'i32'),
  )
  const second = Type.exactRepresentationArgument(
    callableIdentity('second'),
    Type.callable(['i32'], 'i32'),
  )
  const failure = (argument: Type.RepresentationArgument) =>
    Type.nominal('representation/type', 'Failure', [argument])
  const capability = (argument: Type.RepresentationArgument) =>
    Type.nominal('representation/type', 'Capability', [argument])
  const effect = (argument: Type.RepresentationArgument) =>
    Type.effect('i32', [failure(argument)], 'Shared', [
      { capability: capability(argument), role: 'DefaultRole', access: 'Shared' },
    ])
  const union = (argument: Type.RepresentationArgument) =>
    Type.union([
      Type.nominal('representation/type', 'First', [argument]),
      Type.nominal('representation/type', 'Second'),
    ])

  const firstUnion = union(first)
  const secondUnion = union(second)

  assert.deepEqual(Type.firstRepresentationDivergence(effect(first), effect(second)), {
    left: first,
    right: second,
  })
  assert.strictEqual(firstUnion._tag, 'Normalized')
  assert.strictEqual(secondUnion._tag, 'Normalized')
  if (firstUnion._tag === 'Normalized' && secondUnion._tag === 'Normalized')
    assert.deepEqual(Type.firstRepresentationDivergence(firstUnion.type, secondUnion.type), {
      left: first,
      right: second,
    })
})

it('keeps open parameters inside every generic argument non-concrete', () => {
  const failure = Type.parameter(owner, 2, 'E', 'FailureRow')
  const openIdentity = Type.callableIdentityArgument(
    'representation/type.open',
    { _tag: 'Declaration', module: 'representation/type', name: 'open' },
    [value],
  )
  const exact = Type.exactRepresentationArgument(openIdentity, Type.callable(['i32'], 'i32'))
  const nominal = Type.nominal('representation/type', 'OpenRows', [
    Type.failureRowArgument([], [failure]),
    exact,
  ])

  assert.strictEqual(Type.isConcreteGenericArgument(exact), false)
  assert.strictEqual(
    Type.isConcreteGenericArgument(
      Type.substituteGenericArgument(exact, new Map([[Type.key(value), 'i32']])),
    ),
    true,
  )
  assert.strictEqual(Type.isConcrete(nominal), false)
  assert.deepEqual(
    Type.parameters(nominal).map((parameter) => parameter.name),
    ['E', 'A'],
  )
})

it('unifies repeated open representation references and rejects the first mismatch', () => {
  const first = Type.exactRepresentationArgument(
    callableIdentity('first'),
    Type.callable(['i32'], 'i32'),
  )
  const second = Type.exactRepresentationArgument(
    callableIdentity('second'),
    Type.callable(['i32'], 'i32'),
  )
  const pattern = Type.nominal('representation/type', 'RepeatedUse', [open, open])
  const same = Type.nominal('representation/type', 'RepeatedUse', [first, first])
  const conflict = Type.nominal('representation/type', 'RepeatedUse', [first, second])
  const accepted = new Map<string, Type.GenericArgument>()
  const rejected = new Map<string, Type.GenericArgument>()

  assert.strictEqual(Type.infer(pattern, same, accepted), true)
  assert.strictEqual(
    Type.equalsGenericArgument(accepted.get(Type.key(representation)) ?? open, first),
    true,
  )
  assert.strictEqual(Type.infer(pattern, conflict, rejected), false)
  assert.strictEqual(
    Type.equalsGenericArgument(rejected.get(Type.key(representation)) ?? open, first),
    true,
  )
})
