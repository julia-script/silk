import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Builder from '../src/Builder.js'
import * as ByteString from '../src/ByteString.js'
import * as Constant from '../src/Constant.js'
import * as Type from '../src/Type.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'normalizes arbitrary-width signed and unsigned integers exactly',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i129 = yield* Type.integer(builder, 129)
    const minusOne = yield* Constant.integerSigned(builder, i129, -1n)
    const maximum = (1n << 129n) - 1n
    const unsignedMaximum = yield* Constant.integerUnsigned(builder, i129, maximum)

    expect(yield* Constant.integerBitPattern(builder, minusOne)).toBe(maximum)
    expect(yield* Constant.integerBitPattern(builder, unsignedMaximum)).toBe(maximum)
    expect(minusOne).not.toBe(unsignedMaximum)
    expect(
      (yield* Effect.flip(Constant.integerUnsigned(builder, i129, maximum + 1n))).message,
    ).toContain('does not fit')
  }, TestRuntime.runPromise),
)

test(
  'preserves raw floating NaN payloads and canonicalizes equal records',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const double = yield* Type.double(builder)
    const payload = Uint8Array.of(1, 0, 0, 0, 0, 0, 0xf8, 0x7f)
    const first = yield* Constant.floatingRaw(builder, double, 'double', payload)
    const second = yield* Constant.floatingRaw(builder, double, 'double', payload)

    expect(first).toBe(second)
    expect(ByteString.toUint8Array(yield* Constant.floatingBits(builder, first))).toEqual(payload)
  }, TestRuntime.runPromise),
)

test(
  'validates aggregate shapes before interning',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const i8 = yield* Type.integer(builder, 8)
    const i32 = yield* Type.integer(builder, 32)
    const array = yield* Type.array(builder, i32, 2)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const byte = yield* Constant.integerUnsigned(builder, i8, 2)
    const mismatch = yield* Effect.flip(Constant.aggregate(builder, array, [one, byte]))
    const valid = yield* Constant.aggregate(builder, array, [one, one])

    expect(mismatch.message).toContain('element type')
    expect(yield* Constant.tag(builder, valid)).toBe('Aggregate')
  }, TestRuntime.runPromise),
)

test(
  'represents special, string, splat, cast, binary, and assembly constants',
  Effect.fnUntraced(function* () {
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

    expect(yield* Constant.tag(builder, splat)).toBe('Splat')
    expect(yield* Constant.tag(builder, cast)).toBe('Cast')
    expect(yield* Constant.tag(builder, text)).toBe('String')
    expect(yield* Constant.tag(builder, zero)).toBe('Special')
    expect(yield* Constant.tag(builder, assembly)).toBe('Assembly')
  }, TestRuntime.runPromise),
)

test(
  'rejects constants across builder owners',
  Effect.fnUntraced(function* () {
    const first = yield* Builder.make()
    const second = yield* Builder.make()
    const i1 = yield* Type.integer(first, 1)
    const value = yield* Constant.integerUnsigned(first, i1, 1)
    const error = yield* Effect.flip(Constant.typeOf(second, value))
    expect(error.message).toContain('different LLVM builder')
  }, TestRuntime.runPromise),
)
