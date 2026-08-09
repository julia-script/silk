import { assert, it } from '@effect/vitest'
import * as Operator from '../src/Operator.js'

it('publishes the closed prefix vocabulary', () => {
  assert.strictEqual(Operator.prefix('Minus'), 'Negate')
  assert.strictEqual(Operator.prefix('Bang'), 'Not')
  assert.strictEqual(Operator.prefix('Plus'), undefined)
  assert.strictEqual(Operator.prefixSpelling('Negate'), '-')
  assert.strictEqual(Operator.prefixSpelling('Not'), '!')
})

it('orders infix operators by immutable precedence metadata', () => {
  const multiply = Operator.infix('Star')
  const add = Operator.infix('Plus')
  const relational = Operator.infix('Less')
  const equality = Operator.infix('EqualEqual')

  assert.notStrictEqual(multiply, undefined)
  assert.notStrictEqual(add, undefined)
  assert.notStrictEqual(relational, undefined)
  assert.notStrictEqual(equality, undefined)
  if (
    multiply === undefined ||
    add === undefined ||
    relational === undefined ||
    equality === undefined
  )
    return

  assert.strictEqual(multiply.precedence > add.precedence, true)
  assert.strictEqual(add.precedence > relational.precedence, true)
  assert.strictEqual(relational.precedence > equality.precedence, true)
  assert.strictEqual(multiply.associativity, 'Left')
  assert.strictEqual(relational.associativity, 'None')
  assert.strictEqual(Object.isFrozen(multiply), true)
})

it('publishes canonical builtin targets including type-selected equality', () => {
  assert.deepEqual(Operator.target('Add'), { actor: 'i32', operation: 'add' })
  assert.deepEqual(Operator.target('Not'), { actor: 'bool', operation: 'not' })
  assert.deepEqual(Operator.target('Equals'), { actor: 'i32', operation: 'equals' })
  assert.deepEqual(Operator.target('Equals', 'bool'), { actor: 'bool', operation: 'equals' })
  assert.deepEqual(Operator.target('NotEquals', 'bool'), {
    actor: 'bool',
    operation: 'notEquals',
  })
})
