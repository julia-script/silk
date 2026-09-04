import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('order-interface/main', encoder.encode(source))

it.effect('rejects a type with no Order witness', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.order { Order }
pub fn main() -> bool {
  return Order.less(true, false)
}`)
    assert.isTrue(
      Analysis.diagnostics(self).some((diagnostic) =>
        diagnostic.message.includes('does not implement Order'),
      ),
      Json.stringify(Analysis.diagnostics(self).map((diagnostic) => diagnostic.message)),
    )
  }),
)

it.effect('selects one concrete comparison with no runtime dispatch', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.order { Order }
pub fn main() -> i32 {
  let ordered = Order.less<i32>(1, 2)
  if ordered { return 42 }
  return 0
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const operations = Analysis.loweredMir(self).functions.flatMap(MirVerification.operations)
    const comparisons = operations.filter(
      (operation) => operation._tag === 'Binary' && operation.operator === 'LessThan',
    )
    // One comparison, and it results in bool rather than in the bound's type parameter.
    assert.strictEqual(comparisons.length, 1)
    if (comparisons[0]?._tag === 'Binary') assert.deepEqual(comparisons[0].type, { _tag: 'bool' })
    // Interface selection is static: no provider slot and no dispatch reaches the lowered program.
    assert.isFalse(
      operations.some((operation) =>
        ['Switch', 'Provide', 'ServiceCall', 'ServiceEffectConstruct'].includes(operation._tag),
      ),
    )
  }),
)

it.effect('specializes one bound per conforming integer width', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.order { Order }
pub fn main() -> i32 {
  let narrow = Order.less<u8>(1, 2)
  let wide = Order.less<i64>(1, 2)
  if narrow { if wide { return 42 } }
  return 0
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const operations = Analysis.loweredMir(self).functions.flatMap(MirVerification.operations)
    const comparisons = operations.filter(
      (operation) => operation._tag === 'Binary' && operation.operator === 'LessThan',
    )
    // One comparison per width: the two instantiations lower to two separate primitives rather
    // than sharing one dispatch through a provider.
    assert.strictEqual(comparisons.length, 2)
    assert.isFalse(
      operations.some((operation) =>
        ['Switch', 'Provide', 'ServiceCall', 'ServiceEffectConstruct'].includes(operation._tag),
      ),
    )
  }),
)
