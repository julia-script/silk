import { assert, it } from '@effect/vitest'
import * as CallableContract from '../src/CallableContract.js'
import * as Constraint from '../src/Constraint.js'
import * as FiniteRow from '../src/FiniteRow.js'
import * as TypeInference from '../src/internal/TypeInference.js'
import * as RequirementRow from '../src/RequirementRow.js'
import * as RowAlgebra from '../src/RowAlgebra.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as Type from '../src/Type.js'
import * as TypeCompatibility from '../src/TypeCompatibility.js'
import { unreachable } from './support/raise.js'

const span = (sourceId: string, start: number, end: number): SourceSpan.SourceSpan =>
  SourceSpan.fromOffsets(sourceId, start, end) ?? unreachable('expected a valid source span')

it('keeps nominal identity independent of field shape and import spelling', () => {
  const first = Type.nominal('syntax/Tree', 'Node')
  const repeated = Type.nominal('syntax/Tree', 'Node')
  const otherModule = Type.nominal('hir/Tree', 'Node')

  assert.strictEqual(Type.equals(first, repeated), true)
  assert.strictEqual(Type.equals(first, otherModule), false)
  assert.strictEqual(Type.encode(first), 'syntax/Tree.Node')
  assert.strictEqual(Object.isFrozen(first), true)
})

it('selects failure-carrier members only under their explicit tag convention', () => {
  const first = Type.nominal('types/failure-carrier', 'First')
  const second = Type.nominal('types/failure-carrier', 'Second')
  const normalized = Type.union([first, second])
  const union =
    normalized._tag === 'Normalized' && Type.isUnion(normalized.type)
      ? normalized.type
      : unreachable('expected a structural union')
  const effect = Type.effect('i32', [first, second])

  assert.strictEqual(Type.failureCarrierMember(first, 0, 'ZeroBased'), first)
  assert.strictEqual(Type.failureCarrierMember('i32', 0, 'ZeroBased'), 'i32')
  assert.strictEqual(Type.failureCarrierMember(union, 0, 'ZeroBased'), first)
  assert.strictEqual(Type.failureCarrierMember(union, 1, 'ZeroBased'), second)
  assert.strictEqual(Type.failureCarrierMember(effect, 1, 'OneBased'), first)
  assert.strictEqual(Type.failureCarrierMember(effect, 2, 'OneBased'), second)
  assert.isUndefined(Type.failureCarrierMember(first, 1, 'ZeroBased'))
  assert.isUndefined(Type.failureCarrierMember(union, 1, 'OneBased'))
  assert.isUndefined(Type.failureCarrierMember(effect, 0, 'OneBased'))
  assert.isUndefined(Type.failureCarrierMember(effect, 1, 'ZeroBased'))
  for (const invalid of [-1, 0.5, Number.MAX_SAFE_INTEGER + 1, Number.NaN]) {
    assert.isUndefined(Type.failureCarrierMember(union, invalid, 'ZeroBased'))
    assert.isUndefined(Type.failureCarrierMember(effect, invalid, 'OneBased'))
  }
})

it('refuses carrier tags whose member order can change after specialization', () => {
  const owner = Object.freeze({ module: 'types/failure-carrier', name: 'specialize' })
  const failures = Type.parameter(owner, 0, 'E')
  const member = Type.parameter(owner, 1, 'T')
  const alpha = Type.nominal('types/failure-carrier', 'Alpha')
  const zed = Type.nominal('types/failure-carrier', 'Zed')
  const openEffect = Type.effect('i32', [zed, failures])

  assert.isUndefined(Type.failureCarrierMember(openEffect, 1, 'OneBased'))
  const specializedEffect = Type.substitute(
    openEffect,
    new Map([[Type.key(failures), Type.failureValue([alpha])]]),
  )
  const concreteEffect = Type.isEffect(specializedEffect)
    ? specializedEffect
    : unreachable('expected a specialized Effect carrier')
  assert.deepEqual(Type.failureMembers(concreteEffect), [alpha, zed])
  assert.isTrue(
    Type.equals(
      Type.failureCarrierMember(concreteEffect, 1, 'OneBased') ??
        unreachable('expected first specialized failure'),
      alpha,
    ),
  )
  assert.isTrue(
    Type.equals(
      Type.failureCarrierMember(concreteEffect, 2, 'OneBased') ??
        unreachable('expected second specialized failure'),
      zed,
    ),
  )

  const genericBox = Type.nominal('types/failure-carrier', 'Box', [member])
  const concreteBox = Type.nominal('types/failure-carrier', 'Box', ['i32'])
  const unavailableBox = Type.nominal('types/failure-carrier', 'Box', [
    Type.unavailableGenericArgument('Value', 'unresolved carrier argument'),
  ])
  const unavailableInner = Type.nominal('types/failure-carrier', 'Inner', [
    Type.unavailableGenericArgument('Value', 'unresolved nested carrier argument'),
  ])
  const unavailableOuter = Type.nominal('types/failure-carrier', 'Outer', [unavailableInner])
  const concreteOuter = Type.nominal('types/failure-carrier', 'Outer', [
    Type.nominal('types/failure-carrier', 'Inner', ['i32']),
  ])
  const normalized = Type.union([genericBox, concreteBox, zed])
  const openUnion =
    normalized._tag === 'Normalized' && Type.isUnion(normalized.type)
      ? normalized.type
      : unreachable('expected an open structural union')
  assert.isUndefined(Type.failureCarrierMember(genericBox, 0, 'ZeroBased'))
  assert.isUndefined(Type.failureCarrierMember(unavailableBox, 0, 'ZeroBased'))
  assert.isFalse(Type.isRuntimeConcrete(unavailableOuter))
  assert.isFalse(Type.isRuntimeConcreteGenericArgument(unavailableOuter))
  assert.isUndefined(Type.failureCarrierMember(unavailableOuter, 0, 'ZeroBased'))
  assert.isTrue(Type.isRuntimeConcrete(concreteOuter))
  assert.strictEqual(Type.failureCarrierMember(concreteOuter, 0, 'ZeroBased'), concreteOuter)
  assert.isUndefined(Type.failureCarrierMember(openUnion, 0, 'ZeroBased'))

  const unavailableEffect = Type.effect(unavailableOuter, [unavailableOuter], 'Shared', [
    { capability: unavailableOuter, role: 'DefaultRole', access: 'Shared' },
  ])
  const unavailableUnionResult = Type.union([unavailableOuter, zed])
  const unavailableUnion =
    unavailableUnionResult._tag === 'Normalized' && Type.isUnion(unavailableUnionResult.type)
      ? unavailableUnionResult.type
      : unreachable('expected a nested-unavailable structural union')
  const unavailableCallable = Type.callable([unavailableOuter], unavailableOuter)
  const unavailableIdentity = Type.effectIdentityArgument('types/failure-carrier.effect', {
    declaration: owner,
    typeArguments: [unavailableOuter],
  })
  const concreteContract = Type.effect('i32', [])
  const unavailableRepresentation = Type.represented(
    concreteContract,
    concreteContract,
    Type.exactRepresentationArgument(unavailableIdentity, concreteContract),
  )
  assert.isFalse(Type.isRuntimeConcrete(unavailableEffect))
  assert.isUndefined(Type.failureCarrierMember(unavailableEffect, 1, 'OneBased'))
  assert.isFalse(Type.isRuntimeConcrete(unavailableUnion))
  assert.isUndefined(Type.failureCarrierMember(unavailableUnion, 0, 'ZeroBased'))
  assert.isFalse(Type.isRuntimeConcrete(unavailableCallable))
  assert.isFalse(Type.isRuntimeConcrete(unavailableRepresentation))
  assert.isFalse(Type.isRuntimeConcreteGenericArgument(unavailableIdentity))
  assert.isFalse(Type.isRuntimeConcreteGenericArgument(Type.failureValue([unavailableOuter])))
  assert.isFalse(
    Type.isRuntimeConcreteGenericArgument(
      Type.requirementRowArgument([
        { capability: unavailableOuter, role: 'DefaultRole', access: 'Shared' },
      ]),
    ),
  )
  assert.isTrue(
    Type.isRuntimeConcrete(
      Type.effect(concreteOuter, [zed], 'Shared', [
        { capability: concreteOuter, role: 'DefaultRole', access: 'Shared' },
      ]),
    ),
  )

  const specializedUnion = Type.substitute(openUnion, new Map([[Type.key(member), 'i32']]))
  const concreteUnion = Type.isUnion(specializedUnion)
    ? specializedUnion
    : unreachable('expected a specialized structural union carrier')
  assert.strictEqual(concreteUnion.members.length, 2)
  assert.isTrue(
    Type.equals(
      Type.failureCarrierMember(concreteUnion, 0, 'ZeroBased') ??
        unreachable('expected first specialized union member'),
      concreteUnion.members.at(0) ?? unreachable('expected first canonical union member'),
    ),
  )
  assert.isTrue(
    Type.equals(
      Type.failureCarrierMember(concreteUnion, 1, 'ZeroBased') ??
        unreachable('expected second specialized union member'),
      concreteUnion.members.at(1) ?? unreachable('expected second canonical union member'),
    ),
  )
})

it('specializes executable owners throughout nested callable schemas without capturing binders', () => {
  const declaration = Object.freeze({ module: 'types/schema-owner', name: 'outer' })
  const openOwner = Type.parameter(declaration, 0, 'T')
  const nestedOwner = Object.freeze({ module: 'types/schema-owner', name: 'section' })
  const nested = Type.parameter(nestedOwner, 0, 'A')
  const marker = Type.nominal('types/schema-owner', 'Marker')
  const effect = Type.effect('i32', [])
  const ownedIdentity = Type.exactRepresentationArgument(
    Type.effectIdentityArgument('types/schema-owner.effect', {
      declaration,
      typeArguments: [openOwner],
    }),
    effect,
  )
  const provider = Type.nominal('types/schema-owner', 'Provider', [ownedIdentity])
  const capability = Type.nominal('types/schema-owner', 'Service')
  const requirements = Type.requirementRowArgument([
    Object.freeze({ capability, role: 'DefaultRole', access: 'Shared' }),
  ]).row
  const constraint = Constraint.providerSelection('Shared', provider, requirements, requirements)
  const evidence = Constraint.assumed(constraint, new Map([[Type.key(nested), ownedIdentity]]))
  const contract = CallableContract.make({
    functionKind: 'Function',
    binders: [nested],
    result: 'i32',
    constraints: [constraint],
  })
  const callable = Type.callable([], 'i32', 'Shared', {
    contract,
    binders: [nested],
    constraints: [constraint],
    evidence: [evidence],
    substitution: new Map([[Type.key(nested), ownedIdentity]]),
    contractKey: CallableContract.key(contract),
    constraintKeys: [Constraint.key(constraint)],
    evidenceKeys: [Constraint.evidenceKey(evidence)],
    origins: [span('types/schema-owner', 0, 1)],
  })

  const specialized = Type.specializeExecutableOwner(
    callable,
    Object.freeze({ declaration, typeArguments: [marker] }),
    Constraint.specializeCallableSchemaExecutableOwner,
  )
  assert.isTrue(Type.isCallable(specialized))
  const specializedCallable = Type.isCallable(specialized)
    ? specialized
    : unreachable('expected specialized callable type')
  const schema = specializedCallable.schema ?? unreachable('expected specialized callable schema')
  assert.deepEqual(schema.binders, [nested])
  assert.notStrictEqual(schema.contractKey, callable.schema?.contractKey)
  assert.deepEqual(schema.constraintKeys, schema.constraints.map(Constraint.key))
  assert.deepEqual(schema.evidenceKeys, schema.evidence.map(Constraint.evidenceKey))
  const argument = schema.substitution.get(Type.key(nested))
  assert.isTrue(argument !== undefined && Type.isExactRepresentationArgument(argument))
  const exactArgument =
    argument !== undefined && Type.isExactRepresentationArgument(argument)
      ? argument
      : unreachable('expected exact specialized schema substitution')
  assert.isTrue(Type.isEffectIdentityArgument(exactArgument.identity))
  const effectIdentity = Type.isEffectIdentityArgument(exactArgument.identity)
    ? exactArgument.identity
    : unreachable('expected specialized Effect identity')
  assert.deepEqual(effectIdentity.owner?.typeArguments, [marker])
})

it('orders built-in and nominal types by canonical keys', () => {
  const values: ReadonlyArray<Type.Type> = [
    Type.nominal('syntax/Tree', 'Node'),
    'i32',
    Type.nominal('ast/Tree', 'Node'),
    'bool',
  ]

  assert.deepEqual([...values].sort(Type.compare).map(Type.encode), [
    'bool',
    'i32',
    'ast/Tree.Node',
    'syntax/Tree.Node',
  ])
  assert.strictEqual(Type.isBuiltin('i32'), true)
  assert.strictEqual(Type.isNominal(values[0] ?? 'i32'), true)
})

it('keeps string canonical, non-scalar, borrowed, and structurally atomic', () => {
  const owner = { module: 'work', name: 'identity' }
  const parameter = Type.parameter(owner, 0, 'T')
  const substitution = new Map([[Type.key(parameter), 'u8' as const]])
  const values: ReadonlyArray<Type.Type> = [Type.string, 'i32', Type.nominal('text', 'Owner')]

  assert.strictEqual(Type.isString(Type.string), true)
  assert.strictEqual(Type.isBuiltin(Type.string), false)
  assert.strictEqual(Type.key(Type.string), 'string')
  assert.strictEqual(Type.encode(Type.string), 'string')
  assert.deepEqual([...values].sort(Type.compare).map(Type.encode), ['i32', 'text.Owner', 'string'])
  assert.strictEqual(Type.substitute(Type.string, substitution), Type.string)
  assert.strictEqual(Type.isConcrete(Type.string), true)
  assert.deepEqual(Type.parameters(Type.string), [])
  assert.deepEqual(Type.nominals(Type.string), [])
  assert.strictEqual(Type.containsBorrow(Type.string), true)
  assert.strictEqual(
    TypeCompatibility.isCompatible(TypeCompatibility.check(Type.string, Type.string)),
    true,
  )
  assert.strictEqual(
    TypeCompatibility.isCompatible(
      TypeCompatibility.check(Type.string, Type.slice('Shared', 'u8')),
    ),
    false,
  )
})

it('keeps fixed-array element type and length in recursive structural identity', () => {
  const three = Type.fixedArray('i32', 3)
  const repeated = Type.fixedArray('i32', 3)
  const four = Type.fixedArray('i32', 4)
  const nested = Type.fixedArray(Type.fixedArray(Type.nominal('model/Token', 'Token'), 0), 2)

  assert.strictEqual(Type.equals(three, repeated), true)
  assert.strictEqual(Type.equals(three, four), false)
  assert.strictEqual(Type.isFixedArray(three), true)
  assert.strictEqual(Type.encode(nested), 'Array<Array<model/Token.Token, 0>, 2>')
  assert.deepEqual(Type.nominals(nested).map(Type.encode), ['model/Token.Token'])
  assert.strictEqual(Object.isFrozen(three), true)
})

it('normalizes structural unions as canonical ordinary sets', () => {
  const token = Type.nominal('model/Token', 'Token')
  const end = Type.nominal('model/End', 'End')
  const first = Type.union([token, end, token])
  const permuted = Type.union([end, token])
  assert.strictEqual(first._tag, 'Normalized')
  assert.strictEqual(permuted._tag, 'Normalized')
  if (first._tag !== 'Normalized' || permuted._tag !== 'Normalized') return
  assert.strictEqual(Type.isUnion(first.type), true)
  assert.strictEqual(Type.equals(first.type, permuted.type), true)
  assert.strictEqual(Type.encode(first.type), 'model/End.End | model/Token.Token')
  assert.deepEqual(Type.nominals(first.type).map(Type.encode), [
    'model/End.End',
    'model/Token.Token',
  ])
  assert.strictEqual(Object.isFrozen(first.type), true)
  assert.strictEqual(Type.isUnion(first.type) ? Object.isFrozen(first.type.members) : false, true)
})

it('finds generic nominal dependencies nested inside union members', () => {
  const hidden = Type.nominal('model/Private', 'Hidden')
  const box = Type.nominal('model/Box', 'Box', [hidden])
  const other = Type.nominal('model/Other', 'Other')
  const union = Type.union([box, other])
  assert.strictEqual(union._tag, 'Normalized')
  if (union._tag !== 'Normalized') return
  assert.deepEqual(Type.nominals(union.type).map(Type.encode), [
    'model/Box.Box<model/Private.Hidden>',
    'model/Private.Hidden',
    'model/Other.Other',
  ])
})

it('normalizes empty, singleton, scalar, and aggregate union members', () => {
  const token = Type.nominal('model/Token', 'Token')
  const empty = Type.union(['never'])
  const singleton = Type.union(['never', token, token])
  const aggregate = Type.union([token, 'i32', Type.fixedArray(token, 2)])
  assert.deepEqual(empty, { _tag: 'Normalized', type: 'never' })
  assert.strictEqual(singleton._tag, 'Normalized')
  if (singleton._tag === 'Normalized') assert.strictEqual(Type.equals(singleton.type, token), true)
  assert.strictEqual(aggregate._tag, 'Normalized')
  if (aggregate._tag === 'Normalized')
    assert.strictEqual(
      Type.encode(aggregate.type),
      'Array<model/Token.Token, 2> | i32 | model/Token.Token',
    )
})

it('admits only finite detached union storage and renormalizes generic members', () => {
  const owner = { module: 'model/GenericUnion', name: 'choose' }
  const left = Type.parameter(owner, 0, 'L')
  const right = Type.parameter(owner, 1, 'R')
  const borrowed = Type.slice('Shared', 'i32')
  assert.deepEqual(Type.union(['i32', borrowed]), {
    _tag: 'InvalidMembers',
    members: [borrowed],
  })
  const callable = Type.callable(['i32'], 'i32')
  const effect = Type.effect('i32', [])
  assert.deepEqual(Type.union(['i32', callable]), {
    _tag: 'InvalidMembers',
    members: [callable],
  })
  assert.deepEqual(Type.union(['i32', effect]), {
    _tag: 'InvalidMembers',
    members: [effect],
  })

  const open = Type.union([left, right])
  assert.strictEqual(open._tag, 'Normalized')
  if (open._tag !== 'Normalized') return
  const specialized = Type.substitute(
    open.type,
    new Map([
      [Type.key(left), 'i32'],
      [Type.key(right), 'i32'],
    ]),
  )
  assert.strictEqual(specialized, 'i32')
})

it('normalizes compiler-private effect contract identity and traverses substitutions', () => {
  const owner = { module: 'work', name: 'load' }
  const parameter = Type.parameter(owner, 0, 'T')
  const first = Type.nominal('errors', 'First')
  const second = Type.nominal('errors', 'Second')
  const contract = Type.effect(parameter, [second, first, second], 'Take')
  const permuted = Type.effect(parameter, [first, second], 'Take')

  assert.strictEqual(Type.isEffect(contract), true)
  assert.strictEqual(Type.equals(contract, permuted), true)
  assert.strictEqual(Type.encode(contract), 'once Effect<T ! errors.First | errors.Second>')
  assert.deepEqual(Type.parameters(contract), [parameter])
  assert.deepEqual(Type.nominals(contract).map(Type.encode), ['errors.First', 'errors.Second'])

  const substituted = Type.substitute(contract, new Map([[Type.key(parameter), 'usize']]))
  assert.strictEqual(Type.encode(substituted), 'once Effect<usize ! errors.First | errors.Second>')
  assert.strictEqual(Type.isConcrete(substituted), true)
})

it('canonicalizes callable contracts and orders invocation guarantees', () => {
  const owner = { module: 'work', name: 'apply' }
  const parameter = Type.parameter(owner, 0, 'T')
  const shared = Type.callable([parameter, 'bool'], parameter)
  const unsafe = Type.callable([parameter, 'bool'], parameter, 'Shared', undefined, true)
  const exclusive = Type.callable([parameter, 'bool'], parameter, 'Exclusive')
  const once = Type.callable([parameter, 'bool'], parameter, 'Take')
  const substitution = new Map([[Type.key(parameter), 'i32' as const]])

  assert.strictEqual(Type.encode(shared), 'fn(T, bool) -> T')
  assert.strictEqual(Type.encode(unsafe), 'unsafe fn(T, bool) -> T')
  assert.strictEqual(Type.encode(exclusive), 'mut fn(T, bool) -> T')
  assert.strictEqual(Type.encode(once), 'once fn(T, bool) -> T')
  assert.strictEqual(Type.encode(Type.substitute(shared, substitution)), 'fn(i32, bool) -> i32')
  assert.strictEqual(
    Type.encode(Type.substitute(unsafe, substitution)),
    'unsafe fn(i32, bool) -> i32',
  )
  assert.deepEqual(Type.parameters(shared), [parameter])
  assert.strictEqual(
    TypeCompatibility.isCompatible(TypeCompatibility.check(shared, exclusive)),
    true,
  )
  assert.strictEqual(TypeCompatibility.isCompatible(TypeCompatibility.check(shared, once)), true)
  assert.strictEqual(TypeCompatibility.isCompatible(TypeCompatibility.check(exclusive, once)), true)
  assert.strictEqual(
    TypeCompatibility.isCompatible(TypeCompatibility.check(exclusive, shared)),
    false,
  )
  assert.strictEqual(
    TypeCompatibility.isCompatible(TypeCompatibility.check(once, exclusive)),
    false,
  )
  assert.strictEqual(TypeCompatibility.isCompatible(TypeCompatibility.check(shared, unsafe)), true)
  assert.strictEqual(TypeCompatibility.isCompatible(TypeCompatibility.check(unsafe, shared)), false)
})

it('applies the Shared < Exclusive < Take order across compatibility and inference', () => {
  const accesses = ['Shared', 'Exclusive', 'Take'] as const
  const expected = [
    [true, true, true],
    [false, true, true],
    [false, false, true],
  ] as const

  for (const [requiredOrdinal, required] of accesses.entries()) {
    for (const [suppliedOrdinal, supplied] of accesses.entries()) {
      const accepted = expected.at(requiredOrdinal)?.at(suppliedOrdinal) ?? false
      const requiredCallable = Type.callable(['i32'], 'i32', required)
      const suppliedCallable = Type.callable(['i32'], 'i32', supplied)
      const requiredEffect = Type.effect('i32', [], required)
      const suppliedEffect = Type.effect('i32', [], supplied)

      assert.strictEqual(Type.compareAccess(supplied, required), accepted)
      assert.strictEqual(
        TypeCompatibility.isCompatible(TypeCompatibility.check(requiredCallable, suppliedCallable)),
        accepted,
      )
      assert.strictEqual(
        TypeCompatibility.isCompatible(TypeCompatibility.check(requiredEffect, suppliedEffect)),
        accepted,
      )
      assert.strictEqual(
        TypeInference.infer(suppliedCallable, requiredCallable, new Map()),
        accepted,
      )
      assert.strictEqual(TypeInference.infer(suppliedEffect, requiredEffect, new Map()), accepted)
    }
  }
})

it('searches every nested type position including requirement capabilities', () => {
  const borrowed = Type.slice('Shared', 'u8')
  const capability = Type.nominal('test', 'Capability', [borrowed])
  const effect = Type.effect('i32', [], 'Shared', [
    { capability, role: 'DefaultRole', access: 'Shared' },
  ])

  assert.strictEqual(Type.someSubterm(effect, Type.isSlice), true)
  assert.strictEqual(Type.containsBorrow(effect), true)
  assert.strictEqual(Type.containsViewBorrow(effect), true)
  assert.strictEqual(Type.containsPositionRestrictedBorrow(effect), true)
})

it('normalizes finite rows and applies total exact set operations deterministically', () => {
  const policy: FiniteRow.Policy<string> = {
    collisionKey: (member) => member,
    memberKey: (member) => member,
    merge: (left) => left,
  }
  const first = FiniteRow.make(policy, ['C', 'A', 'B', 'A'])
  const permuted = FiniteRow.make(policy, ['B', 'C', 'A'])
  const selected = FiniteRow.make(policy, ['C', 'missing', 'A'])

  assert.deepEqual(first.members, ['A', 'B', 'C'])
  assert.strictEqual(FiniteRow.equals(policy, first, permuted), true)
  assert.strictEqual(FiniteRow.key(policy, first), FiniteRow.key(policy, permuted))
  assert.deepEqual(FiniteRow.intersection(policy, first, selected).members, ['A', 'C'])
  assert.deepEqual(FiniteRow.difference(policy, first, selected).members, ['B'])
  assert.deepEqual(
    FiniteRow.difference(policy, first, FiniteRow.make(policy, ['missing'])).members,
    ['A', 'B', 'C'],
  )
  assert.strictEqual(FiniteRow.isSubset(policy, FiniteRow.make(policy, ['A', 'C']), first), true)
  assert.strictEqual(
    FiniteRow.encode(policy, first, (member) => member),
    'A | B | C',
  )
})

it('keeps requirement union joins and exact membership separate from key difference', () => {
  type Capability = 'Clock' | 'Logger'
  type Member = RequirementRow.Member<Capability>
  const policy = RequirementRow.policy<Capability>((capability) => capability)
  const member = (
    capability: Capability,
    access: RequirementRow.Access,
    role = 'Default',
  ): Member => ({ capability, access, role })

  for (const [left, right, expected] of [
    ['Shared', 'Shared', 'Shared'],
    ['Shared', 'Exclusive', 'Exclusive'],
    ['Exclusive', 'Shared', 'Exclusive'],
    ['Exclusive', 'Exclusive', 'Exclusive'],
  ] as const) {
    const united = FiniteRow.union(
      policy,
      FiniteRow.make(policy, [member('Logger', left)]),
      FiniteRow.make(policy, [member('Logger', right)]),
    )
    assert.strictEqual(united.members.at(0)?.access, expected)
  }

  const shared = FiniteRow.make(policy, [member('Logger', 'Shared')])
  const exclusive = FiniteRow.make(policy, [member('Logger', 'Exclusive')])
  assert.strictEqual(FiniteRow.has(policy, shared, member('Logger', 'Shared')), true)
  assert.strictEqual(FiniteRow.has(policy, shared, member('Logger', 'Exclusive')), false)
  assert.strictEqual(FiniteRow.has(policy, exclusive, member('Logger', 'Shared')), false)
  assert.strictEqual(FiniteRow.has(policy, exclusive, member('Logger', 'Exclusive')), true)
  assert.deepEqual(FiniteRow.intersection(policy, shared, exclusive).members, [])
  assert.deepEqual(FiniteRow.intersection(policy, exclusive, shared).members, [])
  assert.deepEqual(FiniteRow.intersection(policy, shared, shared).members, shared.members)
  assert.deepEqual(FiniteRow.intersection(policy, exclusive, exclusive).members, exclusive.members)
  assert.strictEqual(FiniteRow.isSubset(policy, shared, exclusive), false)
  assert.strictEqual(FiniteRow.isSubset(policy, exclusive, shared), false)
  assert.strictEqual(FiniteRow.isSubset(policy, shared, shared), true)
  assert.strictEqual(FiniteRow.isSubset(policy, exclusive, exclusive), true)
  assert.deepEqual(FiniteRow.difference(policy, shared, exclusive).members, [])
  assert.deepEqual(FiniteRow.difference(policy, exclusive, shared).members, [])
  assert.deepEqual(FiniteRow.difference(policy, shared, shared).members, [])
  assert.deepEqual(FiniteRow.difference(policy, exclusive, exclusive).members, [])

  const collisions = FiniteRow.make(policy, [
    member('Logger', 'Shared'),
    member('Logger', 'Exclusive'),
    member('Clock', 'Shared'),
    member('Logger', 'Shared', 'Audit'),
  ])
  assert.deepEqual(collisions.members, [
    member('Clock', 'Shared'),
    member('Logger', 'Shared', 'Audit'),
    member('Logger', 'Exclusive'),
  ])
})

it('renormalizes requirement collisions introduced by substitution', () => {
  const owner = { module: 'work', name: 'provide' }
  const capability = Type.parameter(owner, 0, 'P')
  const logger = Type.nominal('silk/logging', 'Logger')
  const contract = Type.effect('never', [], 'Shared', [
    { capability, role: 'Default', access: 'Shared' },
    { capability: logger, role: 'Default', access: 'Exclusive' },
  ])
  const substituted = Type.substitute(contract, new Map([[Type.key(capability), logger]]))

  assert.strictEqual(Type.isEffect(substituted), true)
  if (Type.isEffect(substituted))
    assert.deepEqual(Type.requirementMembers(substituted), [
      { capability: logger, role: 'Default', access: 'Exclusive' },
    ])
})

it('checks provider compatibility independently from exact stored access', () => {
  assert.strictEqual(RequirementRow.providerCanSelect('Shared', 'Shared'), true)
  assert.strictEqual(RequirementRow.providerCanSelect('Shared', 'Exclusive'), false)
  assert.strictEqual(RequirementRow.providerCanSelect('Exclusive', 'Shared'), true)
  assert.strictEqual(RequirementRow.providerCanSelect('Exclusive', 'Exclusive'), true)
  assert.strictEqual(RequirementRow.providerCanSelect('Take', 'Shared'), true)
  assert.strictEqual(RequirementRow.providerCanSelect('Take', 'Exclusive'), true)
})

it('defers concrete difference until generic member keys finish specializing', () => {
  const owner = { module: 'work', name: 'difference' }
  const left = Type.parameter(owner, 0, 'A')
  const right = Type.parameter(owner, 1, 'B')
  const problem = (argument: Type.Type): Type.Nominal => Type.nominal('work', 'Problem', [argument])
  const failurePolicy = Type.failureRowPolicy()
  const openFailures = RowAlgebra.without(
    failurePolicy,
    RowAlgebra.concrete(failurePolicy, [problem(left)]),
    RowAlgebra.concrete(failurePolicy, [problem(right)]),
  )
  assert.strictEqual(openFailures.expression._tag, 'Without')

  const concrete = Type.nominal('work', 'Token')
  const substitution = new Map([
    [Type.key(left), concrete],
    [Type.key(right), concrete],
  ])
  const failures = Type.substituteFailureRow(openFailures, substitution)
  assert.deepEqual(RowAlgebra.concretize(failurePolicy, failures), {
    _tag: 'Concrete',
    row: { members: [] },
  })

  const capability = (argument: Type.Type): Type.Nominal =>
    Type.nominal('work', 'Capability', [argument])
  const requirementPolicy = Type.requirementRowPolicy()
  const requirement = (
    argument: Type.Type,
    access: Type.Requirement['access'],
    role: string,
  ): Type.Requirement => ({ capability: capability(argument), access, role })
  const specialize = (
    source: Type.Requirement,
    selected: Type.Requirement,
  ): ReadonlyArray<Type.Requirement> => {
    const open = RowAlgebra.without(
      requirementPolicy,
      RowAlgebra.concrete(requirementPolicy, [source]),
      RowAlgebra.concrete(requirementPolicy, [selected]),
    )
    assert.strictEqual(open.expression._tag, 'Without')
    const specialized = Type.substituteRequirementsRow(open, substitution)
    const result = RowAlgebra.concretize(requirementPolicy, specialized)
    assert.strictEqual(result._tag, 'Concrete')
    return result._tag === 'Concrete' ? result.row.members : []
  }

  assert.deepEqual(
    specialize(requirement(left, 'Exclusive', 'Audit'), requirement(right, 'Exclusive', 'Audit')),
    [],
  )
  assert.deepEqual(
    specialize(requirement(left, 'Exclusive', 'Audit'), requirement(right, 'Shared', 'Audit')),
    [],
  )
  assert.deepEqual(
    specialize(
      requirement(left, 'Exclusive', 'Audit'),
      requirement(right, 'Exclusive', 'DefaultRole'),
    ),
    [requirement(concrete, 'Exclusive', 'Audit')],
  )
})

it('normalizes open row union ACI and retains erased member obligations', () => {
  interface SymbolicMember {
    readonly parameter: string
  }
  const finite: FiniteRow.Policy<string> = {
    collisionKey: (member) => member,
    memberKey: (member) => member,
    merge: (left) => left,
  }
  const policy: RowAlgebra.Policy<string, string, SymbolicMember, string> = {
    finite,
    concreteMemberMaySpecialize: () => false,
    rowParameterKey: (parameter) => parameter,
    symbolicMemberKey: (member) => member.parameter,
    symbolicMemberParameters: (member) => [member.parameter],
    memberParameterKey: (parameter) => parameter,
    memberWellFormedKey: (member) => `FailureMember:${member.parameter}`,
    allowsSetCancellation: true,
  }
  const firstOrigin = span('main', 20, 21)
  const earlierOrigin = span('main', 10, 11)
  const a = RowAlgebra.parameter<string, string, SymbolicMember>('A')
  const b = RowAlgebra.parameter<string, string, SymbolicMember>('B')
  const s1 = RowAlgebra.singleton(policy, { parameter: 'S' }, firstOrigin)
  const s2 = RowAlgebra.singleton(policy, { parameter: 'S' }, earlierOrigin)
  const left = RowAlgebra.union(policy, RowAlgebra.union(policy, a, b), s1)
  const right = RowAlgebra.union(policy, s2, RowAlgebra.union(policy, b, a))

  assert.strictEqual(RowAlgebra.equals(policy, left, right), true)
  assert.strictEqual(RowAlgebra.key(policy, left), RowAlgebra.key(policy, right))
  assert.deepEqual(right.memberWellFormed.at(0)?.origins, [earlierOrigin])

  const repeated = RowAlgebra.union(policy, left, s2)
  assert.deepEqual(repeated.memberWellFormed.at(0)?.origins, [earlierOrigin, firstOrigin])
  assert.strictEqual(RowAlgebra.key(policy, repeated), RowAlgebra.key(policy, left))
  const obligation = repeated.memberWellFormed.at(0)
  assert.isDefined(obligation)
  if (obligation !== undefined) {
    assert.deepEqual(RowAlgebra.diagnosticLocations(obligation), {
      primary: earlierOrigin,
      secondary: [firstOrigin],
    })
    const responsible = span('call', 30, 31)
    assert.deepEqual(RowAlgebra.diagnosticLocations(obligation, responsible), {
      primary: responsible,
      secondary: [earlierOrigin, firstOrigin],
    })
  }

  const erased = RowAlgebra.without(policy, s1, s2)
  assert.strictEqual(
    RowAlgebra.encode(policy, erased, String, String, (member) => member.parameter),
    '',
  )
  assert.deepEqual(RowAlgebra.parameters(policy, erased), { rows: [], members: ['S'] })
  assert.strictEqual(RowAlgebra.concretize(policy, erased)._tag, 'Residual')
  const emptyLeft = RowAlgebra.without(policy, RowAlgebra.concrete(policy, []), s1)
  assert.deepEqual(RowAlgebra.parameters(policy, emptyLeft), { rows: [], members: ['S'] })
})

it('substitutes symbolic members residually or concretely and reports invalid singleton domains', () => {
  interface SymbolicMember {
    readonly parameter: string
  }
  const policy: RowAlgebra.Policy<string, string, SymbolicMember, string> = {
    finite: {
      collisionKey: (member) => member,
      memberKey: (member) => member,
      merge: (left) => left,
    },
    concreteMemberMaySpecialize: () => false,
    rowParameterKey: (parameter) => parameter,
    symbolicMemberKey: (member) => member.parameter,
    symbolicMemberParameters: (member) => [member.parameter],
    memberParameterKey: (parameter) => parameter,
    memberWellFormedKey: (member) => `FailureMember:${member.parameter}`,
    allowsSetCancellation: true,
  }
  const origin = span('main', 4, 5)
  const symbolic = RowAlgebra.singleton(policy, { parameter: 'S' }, origin)
  const source = RowAlgebra.union(
    policy,
    RowAlgebra.parameter<string, string, SymbolicMember>('E'),
    symbolic,
  )
  const expression = RowAlgebra.without(policy, source, symbolic)
  const noRows = () => undefined

  const residual = RowAlgebra.substitute(policy, expression, {
    row: noRows,
    member: (member) => ({
      _tag: 'Residual',
      member: { parameter: `${member.parameter}2` },
    }),
  })
  assert.strictEqual(residual._tag, 'Substituted')
  if (residual._tag === 'Substituted')
    assert.deepEqual(RowAlgebra.parameters(policy, residual.row), {
      rows: ['E'],
      members: ['S2'],
    })

  const concrete = RowAlgebra.substitute(policy, expression, {
    row: (parameter) =>
      parameter === 'E' ? RowAlgebra.concrete(policy, ['Problem', 'Other']) : undefined,
    member: () => ({ _tag: 'Concrete', member: 'Problem' }),
  })
  assert.strictEqual(concrete._tag, 'Substituted')
  if (concrete._tag === 'Substituted') {
    const finite = RowAlgebra.concretize(policy, concrete.row)
    assert.strictEqual(finite._tag, 'Concrete')
    if (finite._tag === 'Concrete') assert.deepEqual(finite.row.members, ['Other'])

    const distinctOpen = RowAlgebra.parameter<string, string, SymbolicMember>('OnlyOther')
    const sameConcrete = RowAlgebra.substitute(policy, distinctOpen, {
      row: () => RowAlgebra.concrete(policy, ['Other']),
      member: (member) => ({ _tag: 'Residual', member }),
    })
    assert.strictEqual(sameConcrete._tag, 'Substituted')
    if (sameConcrete._tag === 'Substituted')
      assert.strictEqual(RowAlgebra.equals(policy, concrete.row, sameConcrete.row), true)
  }

  const invalid = RowAlgebra.substitute(policy, expression, {
    row: noRows,
    member: () => ({ _tag: 'InvalidSingleton', reason: 'expected one nominal failure' }),
  })
  assert.deepEqual(invalid, {
    _tag: 'InvalidMembers',
    invalid: [
      {
        key: 'FailureMember:S',
        reason: 'expected one nominal failure',
        origins: [origin],
      },
    ],
  })
})

it('applies safe cancellation only in domains that opt into set laws', () => {
  const finite: FiniteRow.Policy<string> = {
    collisionKey: (member) => member,
    memberKey: (member) => member,
    merge: (left) => left,
  }
  const makePolicy = (
    allowsSetCancellation: boolean,
  ): RowAlgebra.Policy<string, string, string, string> => ({
    finite,
    concreteMemberMaySpecialize: () => false,
    rowParameterKey: (parameter) => parameter,
    symbolicMemberKey: (member) => member,
    symbolicMemberParameters: (member) => [member],
    memberParameterKey: (parameter) => parameter,
    memberWellFormedKey: (member) => member,
    allowsSetCancellation,
  })
  const setPolicy = makePolicy(true)
  const accessPolicy = makePolicy(false)
  const a = RowAlgebra.parameter<string, string, string>('A')
  const b = RowAlgebra.parameter<string, string, string>('B')
  const source = RowAlgebra.union(setPolicy, a, b)
  const cancelled = RowAlgebra.without(setPolicy, source, a)
  const retained = RowAlgebra.without(accessPolicy, source, a)

  assert.strictEqual(
    RowAlgebra.encode(setPolicy, cancelled, String, String, String),
    'Without<B, A>',
  )
  assert.strictEqual(
    RowAlgebra.encode(accessPolicy, retained, String, String, String),
    'Without<A | B, A>',
  )
})

it('keeps failure and fixed-role requirement member parameters domain-specific', () => {
  const owner = { module: 'work', name: 'generic' }
  const s = Type.parameter(owner, 0, 'S')
  const p = Type.parameter(owner, 1, 'P')
  const q = Type.parameter(owner, 2, 'Q')
  const origin = span('main', 8, 9)
  const failurePolicy = Type.failureRowPolicy()
  const requirementPolicy = Type.requirementRowPolicy()
  const failure = RowAlgebra.singleton(failurePolicy, Type.failureMemberShape(s), origin)
  const requirement = RowAlgebra.singleton(
    requirementPolicy,
    Type.requirementMemberShape(p, 'Exclusive', 'Audit'),
    origin,
  )

  assert.deepEqual(RowAlgebra.parameters(failurePolicy, failure), {
    rows: [],
    members: [s],
  })
  assert.deepEqual(RowAlgebra.parameters(requirementPolicy, requirement), {
    rows: [],
    members: [p],
  })
  assert.strictEqual(
    RowAlgebra.encode(
      requirementPolicy,
      requirement,
      (member) => `${member.access}:${Type.encode(member.capability)}@${member.role}`,
      Type.encode,
      (member) => `${member.access}:${member.capability.name}@${member.role}`,
    ),
    'Exclusive:P@Audit',
  )

  const residual = RowAlgebra.substitute(requirementPolicy, requirement, {
    row: () => undefined,
    member: (member) => ({
      _tag: 'Residual',
      member: Type.requirementMemberShape(q, member.access, member.role),
    }),
  })
  assert.strictEqual(residual._tag, 'Substituted')
  if (residual._tag === 'Substituted')
    assert.deepEqual(RowAlgebra.parameters(requirementPolicy, residual.row), {
      rows: [],
      members: [q],
    })

  const logger = Type.nominal('silk/logging', 'Logger')
  const concreteRequirement = RowAlgebra.substitute(requirementPolicy, requirement, {
    row: () => undefined,
    member: (member) => ({
      _tag: 'Concrete',
      member: { capability: logger, access: member.access, role: member.role },
    }),
  })
  assert.strictEqual(concreteRequirement._tag, 'Substituted')
  if (concreteRequirement._tag === 'Substituted') {
    const finite = RowAlgebra.concretize(requirementPolicy, concreteRequirement.row)
    assert.strictEqual(finite._tag, 'Concrete')
    if (finite._tag === 'Concrete')
      assert.deepEqual(finite.row.members, [
        { capability: logger, access: 'Exclusive', role: 'Audit' },
      ])
  }

  const invalid = RowAlgebra.substitute(requirementPolicy, requirement, {
    row: () => undefined,
    member: () => ({
      _tag: 'InvalidSingleton',
      reason: 'requirement capability must specialize to a service nominal',
    }),
  })
  assert.strictEqual(invalid._tag, 'InvalidMembers')

  const invalidFailure = RowAlgebra.substitute(failurePolicy, failure, {
    row: () => undefined,
    member: () => ({
      _tag: 'InvalidSingleton',
      reason: 'failure member specialized to a union',
    }),
  })
  assert.strictEqual(invalidFailure._tag, 'InvalidMembers')
})

it('keys callable contracts and branded constraint evidence without source locations', () => {
  const owner = { module: 'work', name: 'provide' }
  const selectedParameter = Type.parameter(owner, 0, 'S', 'RequirementRow')
  const providerParameter = Type.parameter(owner, 1, 'P')
  const logger = Type.nominal('silk/logging', 'Logger')
  const clock = Type.nominal('work', 'Clock')
  const loggerRequirement: Type.Requirement = {
    capability: logger,
    role: 'Default',
    access: 'Exclusive',
  }
  const selected = RowAlgebra.parameter<
    Type.Requirement,
    Type.Parameter,
    Type.RequirementMemberShape
  >(selectedParameter)
  const loggerSource = RowAlgebra.concrete(Type.requirementRowPolicy(), [loggerRequirement])
  const clockSource = RowAlgebra.concrete(Type.requirementRowPolicy(), [
    { capability: clock, role: 'Default', access: 'Exclusive' },
  ])
  const loggerWanted = Constraint.providerSelection(
    'Exclusive',
    providerParameter,
    selected,
    loggerSource,
  )
  const clockWanted = Constraint.providerSelection(
    'Exclusive',
    providerParameter,
    selected,
    clockSource,
  )
  const loggerEvidence = Constraint.requirementSelectionEvidence(loggerWanted, loggerRequirement, {
    _tag: 'Identity',
  })
  const clockEvidence = Constraint.requirementSelectionEvidence(
    clockWanted,
    { capability: clock, role: 'Default', access: 'Exclusive' },
    { _tag: 'Identity' },
  )

  assert.notStrictEqual(Constraint.key(loggerWanted), Constraint.key(clockWanted))
  assert.notStrictEqual(loggerEvidence.wantedKey, clockEvidence.wantedKey)
  assert.notStrictEqual(
    Constraint.evidenceKey(loggerEvidence),
    Constraint.evidenceKey(clockEvidence),
  )
  assert.strictEqual(
    Constraint.evidenceKey(loggerEvidence),
    Constraint.evidenceKey(
      Constraint.requirementSelectionEvidence(loggerWanted, loggerRequirement, {
        _tag: 'Identity',
      }),
    ),
  )

  const contract = CallableContract.make({
    functionKind: 'Effect',
    binders: [selectedParameter, providerParameter],
    parameters: [{ type: Type.reference('Exclusive', providerParameter), mode: 'Exclusive' }],
    result: Type.effect('never', []),
    constraints: [loggerWanted],
    captures: [{ parameter: 0, capture: 0 }],
  })
  assert.strictEqual(CallableContract.key(contract), CallableContract.key(contract))
  assert.strictEqual(Object.isFrozen(contract), true)

  const quantified = Type.callable(
    [Type.reference('Exclusive', providerParameter)],
    Type.effect('never', []),
    'Exclusive',
    {
      contract,
      binders: contract.binders,
      constraints: contract.constraints,
      evidence: [Constraint.assumed(loggerWanted, new Map())],
      substitution: new Map(),
      contractKey: CallableContract.key(contract),
      constraintKeys: contract.constraints.map(Constraint.key),
      evidenceKeys: [Constraint.evidenceKey(Constraint.assumed(loggerWanted, new Map()))],
      origins: [span('work', 10, 20)],
    },
  )
  const movedOrigin = Type.callable(
    quantified.parameters,
    quantified.result,
    quantified.mode,
    quantified.schema === undefined
      ? undefined
      : { ...quantified.schema, origins: [span('work', 30, 40)] },
  )
  assert.deepEqual(Type.parameters(quantified), [])
  assert.strictEqual(Type.isConcrete(quantified), true)
  assert.strictEqual(Type.key(quantified), Type.key(movedOrigin))
  assert.notStrictEqual(
    Type.key(quantified),
    Type.key(Type.callable(quantified.parameters, quantified.result, quantified.mode)),
  )
})

it('keeps neutral witness identity specialization-complete and origin-distinct', () => {
  const declaration = { module: 'work', name: 'loggerWitness' }
  const sourceI32: Constraint.WitnessIdentity = {
    origin: { _tag: 'SourceWitness', declaration },
    typeArguments: ['i32'],
  }
  const sourceBool: Constraint.WitnessIdentity = {
    origin: { _tag: 'SourceWitness', declaration },
    typeArguments: ['bool'],
  }
  const intrinsic: Constraint.WitnessIdentity = {
    origin: { _tag: 'IntrinsicWitness', operation: 'work.loggerWitness' },
    typeArguments: ['i32'],
  }

  assert.notStrictEqual(Constraint.witnessKey(sourceI32), Constraint.witnessKey(sourceBool))
  assert.notStrictEqual(Constraint.witnessKey(sourceI32), Constraint.witnessKey(intrinsic))
  assert.notStrictEqual(
    Constraint.providerMatchKey({ _tag: 'Identity' }),
    Constraint.providerMatchKey({ _tag: 'Conformance', witness: sourceI32 }),
  )
})
