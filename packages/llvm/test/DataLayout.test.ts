import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as AddrSpace from '../src/AddrSpace.js'
import * as Alignment from '../src/Alignment.js'
import * as Builder from '../src/Builder.js'
import * as DataLayout from '../src/DataLayout.js'
import { LlvmError } from '../src/LlvmError.js'

it.effect('parses explicit primitive, pointer, native-width, stack, and non-integral fields', () =>
  Effect.gen(function* () {
    const layout = yield* DataLayout.parse(
      'e-m:e-p:64:64:128:32-p1:32:32-i32:32:64-f64:64-v128:128-n8:16:32:64-S128-ni:1:3',
    )
    const addressOne = yield* AddrSpace.make(1)

    assert.strictEqual(layout.endian, 'little')
    assert.strictEqual(DataLayout.integerSpec(layout, 32)?.preferredAlignment.byteUnits, 8n)
    assert.strictEqual(DataLayout.floatSpec(layout, 64)?.abiAlignment.byteUnits, 8n)
    assert.strictEqual(DataLayout.vectorSpec(layout, 128)?.abiAlignment.byteUnits, 16n)
    assert.strictEqual(DataLayout.pointerSpec(layout, addressOne)?.indexBitWidth, 32)
    assert.deepEqual(layout.nativeIntegerWidths, [8, 16, 32, 64])
    assert.strictEqual(layout.stackAlignment.byteUnits, 16n)
    assert.deepEqual(
      layout.nonIntegralAddressSpaces.map((address) => address.value),
      [1, 3],
    )
  }),
)

it.effect('rejects malformed layouts through LlvmError, including Builder.make', () =>
  Effect.gen(function* () {
    const parseError = yield* Effect.flip(DataLayout.parse('e-i32:24'))
    const builderError = yield* Effect.flip(Builder.make({ dataLayout: 'e-unknown' }))

    assert.strictEqual(parseError._tag, 'LlvmError')
    assert.strictEqual(parseError.operation, 'DataLayout.parse')
    assert.strictEqual(builderError.operation, 'DataLayout.parse')
  }),
)

it.effect('models alignments and address spaces without lossy numbers', () =>
  Effect.gen(function* () {
    const alignment = yield* Alignment.fromByteUnits(16n)
    const address = yield* AddrSpace.make(42)
    assert.strictEqual(Alignment.toByteUnits(alignment), 16n)
    assert.strictEqual(Alignment.compare(Alignment.defaultAlignment, alignment), -1)
    assert.strictEqual(AddrSpace.render(address), 'addrspace(42)')
  }),
)

it.effect('rejects invalid numeric alignment inputs through LlvmError', () =>
  Effect.gen(function* () {
    for (const input of [1.5, Number.NaN, Number.POSITIVE_INFINITY, Number.NEGATIVE_INFINITY]) {
      const error = yield* Effect.flip(Alignment.fromByteUnits(input))
      assert.instanceOf(error, LlvmError)
      assert.strictEqual(error.reason._tag, 'InvalidInput')
    }
    for (const input of [Number.MAX_SAFE_INTEGER + 1, -1, 0, 3]) {
      assert.instanceOf(yield* Effect.flip(Alignment.fromByteUnits(input)), LlvmError)
    }
  }),
)
