import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('order-interface/main', encoder.encode(source))

const evaluated = (source: string) =>
  Effect.gen(function* () {
    const self = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      Json.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
    )
    return outcome._tag === 'Completed' ? Number(outcome.result.value) : undefined
  })

it.effect('narrows a three-way comparison to each of the three results', () =>
  Effect.gen(function* () {
    const value = yield* evaluated(`import silk.order { Order }
pub fn main() -> i32 {
  let low = Order.compare<i32>(1, 2)
  let same = Order.compare<i32>(7, 7)
  let high = Order.compare<i32>(9, 3)
  let mut score = 0
  if Order.isLess(&low) { score = score + 1 }
  if Order.isEqual(&same) { score = score + 10 }
  if Order.isGreater(&high) { score = score + 100 }
  return score
}`)
    assert.strictEqual(value, 111)
  }),
)

it.effect('reports exactly one of the three results for one comparison', () =>
  Effect.gen(function* () {
    // A witness cannot disagree with itself: equality is derived from the same strict `lessThan`
    // the ordering uses, so no comparison can be both less and equal.
    const value = yield* evaluated(`import silk.order { Order }
pub fn main() -> i32 {
  let outcome = Order.compare<i32>(4, 4)
  let mut hits = 0
  if Order.isLess(&outcome) { hits = hits + 1 }
  if Order.isEqual(&outcome) { hits = hits + 1 }
  if Order.isGreater(&outcome) { hits = hits + 1 }
  if hits == 1 { return 42 }
  return hits
}`)
    assert.strictEqual(value, 42)
  }),
)

it.effect('ships an Order witness for every integer type', () =>
  Effect.gen(function* () {
    const value = yield* evaluated(`import silk.order { Order }
pub fn main() -> i32 {
  let mut score = 0
  if Order.less<u8>(1, 2) { score = score + 1 }
  if Order.less<u16>(1, 2) { score = score + 1 }
  if Order.less<u32>(1, 2) { score = score + 1 }
  if Order.less<u64>(1, 2) { score = score + 1 }
  if Order.less<usize>(1, 2) { score = score + 1 }
  if Order.less<i8>(0 - 2, 1) { score = score + 1 }
  if Order.less<i16>(0 - 2, 1) { score = score + 1 }
  if Order.less<i32>(0 - 2, 1) { score = score + 1 }
  if Order.less<i64>(0 - 2, 1) { score = score + 1 }
  if Order.less<isize>(0 - 2, 1) { score = score + 1 }
  return score
}`)
    assert.strictEqual(value, 10)
  }),
)

it.effect('derives equality from the same strict comparison', () =>
  Effect.gen(function* () {
    const value = yield* evaluated(`import silk.order { Order }
pub fn main() -> i32 {
  let mut score = 0
  if Order.equal<i32>(5, 5) { score = score + 1 }
  if Order.equal<i32>(5, 6) { score = score + 10 }
  if Order.equal<i32>(6, 5) { score = score + 10 }
  return score
}`)
    assert.strictEqual(value, 1)
  }),
)

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
