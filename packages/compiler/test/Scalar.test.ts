import { assert, it } from '@effect/vitest'
import * as Intrinsic from '../src/Intrinsic.js'
import * as Scalar from '../src/Scalar.js'
import * as Type from '../src/Type.js'

it('owns the current scalar vocabulary in stable presentation order', () => {
  assert.deepEqual(
    Scalar.all().map((scalar) => scalar.spelling),
    ['I32', 'Usize', 'Bool'],
  )
  assert.strictEqual(Scalar.isSpelling('I32'), true)
  assert.strictEqual(Scalar.isSpelling('String'), false)
  assert.strictEqual(Type.isBuiltin('Usize'), true)
  assert.strictEqual(Object.isFrozen(Scalar.all()), true)
  assert.strictEqual(Scalar.all().every(Object.isFrozen), true)
  assert.strictEqual(
    Scalar.all().every((scalar) => Object.isFrozen(scalar.operations)),
    true,
  )
})

it('resolves fixed and target-width scalar facts without phase-specific cases', () => {
  assert.strictEqual(Scalar.bits(Scalar.defaultInteger, 64), 32)
  assert.strictEqual(Scalar.bits(Scalar.pointerInteger, 64), 64)
  assert.deepEqual(Scalar.resolveLayout(Scalar.pointerInteger, 8, 8), {
    size: 8,
    alignment: 8,
  })
  assert.deepEqual(
    Scalar.defaultInteger.operations.map((candidate) => candidate.spelling),
    [
      'negate',
      'add',
      'subtract',
      'multiply',
      'divide',
      'remainder',
      'equals',
      'notEquals',
      'lessThan',
      'lessOrEqual',
      'greaterThan',
      'greaterOrEqual',
    ],
  )
  for (const scalar of Scalar.all()) {
    const intrinsic = Intrinsic.findActor(scalar.spelling)
    assert.deepEqual(
      intrinsic?.operations.map((candidate) => candidate.spelling),
      scalar.operations.map((candidate) => candidate.spelling),
    )
  }
})
