import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Builder from '../src/Builder.js'
import * as ByteString from '../src/ByteString.js'
import * as Constant from '../src/Constant.js'
import { LlvmError } from '../src/LlvmError.js'
import * as Type from '../src/Type.js'

it.effect('normalizes arbitrary-width signed and unsigned integers exactly', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i129 = yield* Type.integer(builder, 129)
    const minusOne = yield* Constant.integerSigned(builder, i129, -1n)
    const maximum = (1n << 129n) - 1n
    const unsignedMaximum = yield* Constant.integerUnsigned(builder, i129, maximum)

    assert.strictEqual(yield* Constant.integerBitPattern(builder, minusOne), maximum)
    assert.strictEqual(yield* Constant.integerBitPattern(builder, unsignedMaximum), maximum)
    assert.notStrictEqual(minusOne, unsignedMaximum)
    assert.include(
      (yield* Effect.flip(Constant.integerUnsigned(builder, i129, maximum + 1n))).message,
      'does not fit',
    )
  }),
)

it.effect('preserves raw floating NaN payloads and canonicalizes equal records', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const double = yield* Type.double(builder)
    const payload = Uint8Array.of(1, 0, 0, 0, 0, 0, 0xf8, 0x7f)
    const first = yield* Constant.floatingRaw(builder, double, 'double', payload)
    const second = yield* Constant.floatingRaw(builder, double, 'double', payload)

    assert.strictEqual(first, second)
    assert.deepEqual(ByteString.toUint8Array(yield* Constant.floatingBits(builder, first)), payload)
  }),
)

it.effect('validates aggregate shapes before interning', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i8 = yield* Type.integer(builder, 8)
    const i32 = yield* Type.integer(builder, 32)
    const array = yield* Type.array(builder, i32, 2)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const byte = yield* Constant.integerUnsigned(builder, i8, 2)
    const mismatch = yield* Effect.flip(Constant.aggregate(builder, array, [one, byte]))
    const valid = yield* Constant.aggregate(builder, array, [one, one])

    assert.include(mismatch.message, 'element type')
    assert.strictEqual(yield* Constant.tag(builder, valid), 'Aggregate')
  }),
)

it.effect('represents special, string, splat, cast, binary, and assembly constants', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i32 = yield* Type.integer(builder, 32)
    const vector = yield* Type.vector(builder, i32, 4)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const two = yield* Constant.integerUnsigned(builder, i32, 2)
    const splat = yield* Constant.splat(builder, vector, one)
    const sum = yield* Constant.binary(builder, 'add nuw', one, two)
    const cast = yield* Constant.cast(builder, 'bitcast', sum, i32)
    const text = yield* Constant.string(builder, 'silk', { nullTerminated: true })
    const zero = yield* Constant.zero(builder, vector)
    const functionType = yield* Type.functionType(builder, i32, [])
    const assembly = yield* Constant.assembly(builder, functionType, 'nop', '', {
      sideEffect: true,
    })

    assert.strictEqual(yield* Constant.tag(builder, splat), 'Splat')
    assert.strictEqual(yield* Constant.tag(builder, cast), 'Cast')
    assert.strictEqual(yield* Constant.tag(builder, text), 'String')
    assert.strictEqual(yield* Constant.tag(builder, zero), 'Special')
    assert.strictEqual(yield* Constant.tag(builder, assembly), 'Assembly')
  }),
)

it.effect('rejects constants across builder owners', () =>
  Effect.gen(function* () {
    const first = yield* Builder.make()
    const second = yield* Builder.make()
    const i1 = yield* Type.integer(first, 1)
    const value = yield* Constant.integerUnsigned(first, i1, 1)
    const error = yield* Effect.flip(Constant.typeOf(second, value))
    assert.include(error.message, 'different LLVM builder')
  }),
)

it.effect('rejects invalid number inputs as typed integer failures rather than defects', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i64 = yield* Type.integer(builder, 64)
    for (const value of [1.5, Number.NaN, Number.POSITIVE_INFINITY, Number.MAX_SAFE_INTEGER + 1]) {
      const unsigned = yield* Effect.flip(Constant.integerUnsigned(builder, i64, value))
      const signed = yield* Effect.flip(Constant.integerSigned(builder, i64, value))
      assert.instanceOf(unsigned, LlvmError)
      assert.instanceOf(signed, LlvmError)
      assert.strictEqual(unsigned.reason._tag, 'InvalidInput')
    }
  }),
)
