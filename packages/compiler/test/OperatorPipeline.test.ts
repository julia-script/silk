import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'
const encoder = new TextEncoder()

it.effect('lowers negation to generated zero plus source-authored trapping subtraction', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'golden/negation',
      encoder.encode('pub fn main() -> i32 { let value = 42 return -value }'),
      'aarch64-apple-darwin',
    )
    const fn = Analysis.loweredMir(snapshot).functions.at(0)
    const operations = fn === undefined ? [] : MirVerification.operations(fn)
    const zero = operations.find(
      (operation) => operation._tag === 'Literal' && operation.value === 0n,
    )
    const subtraction = operations.find(
      (operation) => operation._tag === 'Binary' && operation.operator === 'Subtract',
    )

    assert.strictEqual(zero?._tag, 'Literal')
    assert.strictEqual(zero?.provenance.generated, true)
    assert.strictEqual(subtraction?._tag, 'Binary')
    assert.strictEqual(subtraction?.provenance.generated, false)
  }),
)
