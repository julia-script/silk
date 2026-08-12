import { assert, it } from '@effect/vitest'
import * as Operator from '../src/Operator.js'

it('publishes the closed prefix vocabulary', () => {
  assert.strictEqual(Operator.prefix('Minus'), 'Negate')
  assert.strictEqual(Operator.prefix('Bang'), 'Not')
  assert.strictEqual(Operator.prefix('Plus'), undefined)
  assert.strictEqual(Operator.prefixSpelling('Negate'), '-')
  assert.strictEqual(Operator.prefixSpelling('Not'), '!')
  assert.strictEqual(Operator.prefix('Tilde'), 'BitNot')
  assert.strictEqual(Operator.prefixSpelling('BitNot'), '~')
})

it('orders the bitwise level below equality and above the pipeline', () => {
  const equality = Operator.infix('EqualEqual')
  const and = Operator.infix('Ampersand')
  const or = Operator.infix('Pipe')
  const xor = Operator.infix('Caret')

  assert.notStrictEqual(and, undefined)
  assert.notStrictEqual(or, undefined)
  assert.notStrictEqual(xor, undefined)
  if (equality === undefined || and === undefined || or === undefined || xor === undefined) return

  assert.deepEqual(
    [and.operator, or.operator, xor.operator],
    ['BitAnd', 'BitOr', 'BitXor'],
    'the three spellings map to the three named bitwise operations',
  )
  assert.deepEqual([and.spelling, or.spelling, xor.spelling], ['&', '|', '^'])
  for (const info of [and, or, xor]) {
    assert.strictEqual(info.precedence < equality.precedence, true)
    assert.strictEqual(info.precedence > Operator.pipelinePrecedence, true)
    assert.strictEqual(info.associativity, 'Left')
    assert.strictEqual(Object.isFrozen(info), true)
  }
  assert.strictEqual(
    and.precedence === or.precedence && or.precedence === xor.precedence,
    true,
    'the three bitwise spellings share one precedence level',
  )
  assert.strictEqual(and.precedence < Operator.prefixPrecedence, true)
})

it('targets the named bitwise operation of the selected integer actor', () => {
  assert.deepEqual(Operator.target('BitAnd', 'u32'), { actor: 'u32', operation: 'bitAnd' })
  assert.deepEqual(Operator.target('BitOr', 'u32'), { actor: 'u32', operation: 'bitOr' })
  assert.deepEqual(Operator.target('BitXor', 'u64'), { actor: 'u64', operation: 'bitXor' })
  assert.deepEqual(Operator.target('BitNot', 'i64'), { actor: 'i64', operation: 'bitNot' })
  assert.deepEqual(Operator.target('BitAnd'), { actor: 'i32', operation: 'bitAnd' })
  assert.deepEqual(
    Operator.target('BitAnd', 'f64'),
    { actor: 'i32', operation: 'bitAnd' },
    'a non-integer actor falls back to the default integer so analysis reports a type diagnostic',
  )
  assert.deepEqual(Operator.target('BitNot', 'bool'), { actor: 'i32', operation: 'bitNot' })
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
