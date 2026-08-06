import { assert, it } from '@effect/vitest'
import * as Type from '../src/Type.js'

it('keeps nominal identity independent of field shape and import spelling', () => {
  const first = Type.nominal('syntax/Tree', 'Node')
  const repeated = Type.nominal('syntax/Tree', 'Node')
  const otherModule = Type.nominal('hir/Tree', 'Node')

  assert.strictEqual(Type.equals(first, repeated), true)
  assert.strictEqual(Type.equals(first, otherModule), false)
  assert.strictEqual(Type.encode(first), 'syntax/Tree.Node')
  assert.strictEqual(Object.isFrozen(first), true)
})

it('orders built-in and nominal types by canonical keys', () => {
  const values: ReadonlyArray<Type.Type> = [
    Type.nominal('syntax/Tree', 'Node'),
    'I32',
    Type.nominal('ast/Tree', 'Node'),
    'Bool',
  ]

  assert.deepEqual([...values].sort(Type.compare).map(Type.encode), [
    'Bool',
    'I32',
    'ast/Tree.Node',
    'syntax/Tree.Node',
  ])
  assert.strictEqual(Type.isBuiltin('I32'), true)
  assert.strictEqual(Type.isNominal(values[0] ?? 'I32'), true)
})

it('keeps fixed-array element type and length in recursive structural identity', () => {
  const three = Type.fixedArray('I32', 3)
  const repeated = Type.fixedArray('I32', 3)
  const four = Type.fixedArray('I32', 4)
  const nested = Type.fixedArray(Type.fixedArray(Type.nominal('model/Token', 'Token'), 0), 2)

  assert.strictEqual(Type.equals(three, repeated), true)
  assert.strictEqual(Type.equals(three, four), false)
  assert.strictEqual(Type.isFixedArray(three), true)
  assert.strictEqual(Type.encode(nested), 'Array<Array<model/Token.Token, 0>, 2>')
  assert.deepEqual(Type.nominals(nested).map(Type.encode), ['model/Token.Token'])
  assert.strictEqual(Object.isFrozen(three), true)
})

it('normalizes structural unions as canonical nominal sets', () => {
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

it('collapses empty and singleton unions and rejects non-nominal leaves', () => {
  const token = Type.nominal('model/Token', 'Token')
  const empty = Type.union(['Never'])
  const singleton = Type.union(['Never', token, token])
  const invalid = Type.union([token, 'I32', Type.fixedArray(token, 2)])
  assert.deepEqual(empty, { _tag: 'Normalized', type: 'Never' })
  assert.strictEqual(singleton._tag, 'Normalized')
  if (singleton._tag === 'Normalized') assert.strictEqual(Type.equals(singleton.type, token), true)
  assert.strictEqual(invalid._tag, 'InvalidMembers')
  if (invalid._tag === 'InvalidMembers')
    assert.deepEqual(invalid.members.map(Type.encode), ['I32', 'Array<model/Token.Token, 2>'])
})
