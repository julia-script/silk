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
