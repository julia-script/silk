import { assert, it } from '@effect/vitest'
import * as InterfaceWitnessInference from '../src/InterfaceWitnessInference.js'
import * as Lifetime from '../src/Lifetime.js'
import * as Type from '../src/Type.js'

const owner = Object.freeze({ module: 'witness-inference', name: 'target' })

const binder = (
  ordinal: number,
  name: string,
  kind: Type.ParameterKind = 'Value',
  representationBound?: Type.RepresentationBound,
): Type.Parameter => Type.parameter(owner, ordinal, name, kind, representationBound)

it('infers type, row, callable, and Effect representation binders in declaration order', () => {
  const value = binder(0, 'T')
  const failures = binder(1, 'E')
  const requirements = binder(2, 'R', 'RequirementRow')
  const callableBound = Type.callable(['i32'], 'bool', {
    environment: Lifetime.staticLifetime,
    lifetimeBinders: [],
  })
  const representation = binder(3, 'F', 'CallableRepresentation', callableBound)
  const effectBound = Type.effect('bool', [], {
    environment: Lifetime.staticLifetime,
    lifetimeBinders: [],
  })
  const effectRepresentation = binder(4, 'G', 'EffectRepresentation', effectBound)
  const decodeError = Type.nominal('witness-inference', 'DecodeError')
  const clock = Type.nominal('witness-inference', 'Clock')
  const callable = Type.exactRepresentationArgument(
    Type.callableIdentityArgument(
      'declaration:witness-inference:predicate',
      Object.freeze({
        _tag: 'Declaration',
        module: 'witness-inference',
        name: 'predicate',
      }),
    ),
    callableBound,
  )
  const effect = Type.exactRepresentationArgument(
    Type.effectIdentityArgument('effect:witness-inference:predicate'),
    effectBound,
  )
  const inference = InterfaceWitnessInference.infer(
    [value, failures, requirements, representation, effectRepresentation],
    [
      Object.freeze({
        label: 'provider',
        pattern: Type.nominal('witness-inference', 'Schema', [
          value,
          Type.representationParameterArgument(representation),
          Type.representationParameterArgument(effectRepresentation),
        ]),
        actual: Type.nominal('witness-inference', 'Schema', ['i32', callable, effect]),
      }),
      Object.freeze({
        label: 'rows',
        pattern: Type.effect(
          'bool',
          [failures],
          { environment: Lifetime.staticLifetime, lifetimeBinders: [] },
          'Shared',
          [],
          [requirements],
        ),
        actual: Type.effect(
          'bool',
          [decodeError],
          { environment: Lifetime.staticLifetime, lifetimeBinders: [] },
          'Shared',
          [Object.freeze({ capability: clock, role: 'DefaultRole', access: 'Shared' })],
        ),
      }),
    ],
  )

  assert.strictEqual(inference._tag, 'Inferred')
  if (inference._tag !== 'Inferred') return
  assert.deepEqual(inference.arguments.map(Type.genericArgumentKey), [
    Type.key('i32'),
    Type.genericArgumentKey(Type.failureValue([decodeError])),
    Type.genericArgumentKey(
      Type.requirementRowArgument([
        Object.freeze({ capability: clock, role: 'DefaultRole', access: 'Shared' }),
      ]),
    ),
    Type.genericArgumentKey(callable),
    Type.genericArgumentKey(effect),
  ])
})

it('reports the first declaration-ordered unresolved binder', () => {
  const inferred = binder(0, 'T')
  const unresolved = binder(1, 'U')
  const inference = InterfaceWitnessInference.infer(
    [inferred, unresolved],
    [Object.freeze({ label: 'receiver', pattern: inferred, actual: 'i32' })],
  )

  assert.deepEqual(inference, {
    _tag: 'Failed',
    problem: { _tag: 'UnresolvedBinder', binder: unresolved },
  })
})

it('reports deterministic conflicting evidence with both contract positions', () => {
  const value = binder(0, 'T')
  const inference = InterfaceWitnessInference.infer(
    [value],
    [
      Object.freeze({ label: 'receiver', pattern: value, actual: 'i32' }),
      Object.freeze({ label: 'success', pattern: value, actual: 'bool' }),
    ],
  )

  assert.deepEqual(inference, {
    _tag: 'Failed',
    problem: {
      _tag: 'ConflictingBinder',
      binder: value,
      previous: 'i32',
      conflicting: 'bool',
      previousConstraint: 'receiver',
      conflictingConstraint: 'success',
    },
  })
})

it('reports repeated conflicting evidence within one structural contract position', () => {
  const value = binder(0, 'T')
  const inference = InterfaceWitnessInference.infer(
    [value],
    [
      Object.freeze({
        label: 'receiver value',
        pattern: Type.nominal('witness-inference', 'Pair', [value, value]),
        actual: Type.nominal('witness-inference', 'Pair', ['i32', 'bool']),
      }),
    ],
  )

  assert.deepEqual(inference, {
    _tag: 'Failed',
    problem: {
      _tag: 'ConflictingBinder',
      binder: value,
      previous: 'i32',
      conflicting: 'bool',
      previousConstraint: 'receiver value (earlier occurrence)',
      conflictingConstraint: 'receiver value (later occurrence)',
    },
  })
})
