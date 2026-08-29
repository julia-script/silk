import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as AddrSpace from '../src/AddrSpace.js'
import * as Alignment from '../src/Alignment.js'
import * as Builder from '../src/Builder.js'
import * as ByteString from '../src/ByteString.js'
import * as DataLayout from '../src/DataLayout.js'
import { LlvmError } from '../src/LlvmError.js'

const primitivePayload = (spec: DataLayout.PrimitiveSpec | undefined) =>
  spec === undefined
    ? undefined
    : {
        bitWidth: spec.bitWidth,
        abiAlignment: spec.abiAlignment.byteUnits,
        preferredAlignment: spec.preferredAlignment.byteUnits,
      }

const pointerPayload = (spec: DataLayout.PointerSpec | undefined) =>
  spec === undefined
    ? undefined
    : {
        addressSpace: spec.addressSpace.value,
        bitWidth: spec.bitWidth,
        abiAlignment: spec.abiAlignment.byteUnits,
        preferredAlignment: spec.preferredAlignment.byteUnits,
        indexBitWidth: spec.indexBitWidth,
      }

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

it.effect('matches LLVM 22.1.8 last-wins precedence for repeated keyed specifications', () =>
  Effect.gen(function* () {
    const addressOne = yield* AddrSpace.make(1)
    const cases = [
      {
        input:
          'i32:32:64-i32:64:128-f64:64:128-f64:32:64-v128:128:256-v128:64:128-p:64:64:128:32-p0:32:32:64:16-p1:64:64:128:32-p1:32:32:64:16',
        primitive: {
          integer: { bitWidth: 32, abiAlignment: 8n, preferredAlignment: 16n },
          float: { bitWidth: 64, abiAlignment: 4n, preferredAlignment: 8n },
          vector: { bitWidth: 128, abiAlignment: 8n, preferredAlignment: 16n },
        },
        pointerZero: {
          addressSpace: 0,
          bitWidth: 32,
          abiAlignment: 4n,
          preferredAlignment: 8n,
          indexBitWidth: 16,
        },
        pointerOne: {
          addressSpace: 1,
          bitWidth: 32,
          abiAlignment: 4n,
          preferredAlignment: 8n,
          indexBitWidth: 16,
        },
      },
      {
        input:
          'i32:64:128-i32:32:64-f64:32:64-f64:64:128-v128:64:128-v128:128:256-p0:32:32:64:16-p:64:64:128:32-p1:32:32:64:16-p1:64:64:128:32',
        primitive: {
          integer: { bitWidth: 32, abiAlignment: 4n, preferredAlignment: 8n },
          float: { bitWidth: 64, abiAlignment: 8n, preferredAlignment: 16n },
          vector: { bitWidth: 128, abiAlignment: 16n, preferredAlignment: 32n },
        },
        pointerZero: {
          addressSpace: 0,
          bitWidth: 64,
          abiAlignment: 8n,
          preferredAlignment: 16n,
          indexBitWidth: 32,
        },
        pointerOne: {
          addressSpace: 1,
          bitWidth: 64,
          abiAlignment: 8n,
          preferredAlignment: 16n,
          indexBitWidth: 32,
        },
      },
    ] as const

    for (const testCase of cases) {
      const layout = yield* DataLayout.parse(testCase.input)

      assert.deepEqual(
        primitivePayload(DataLayout.integerSpec(layout, 32)),
        testCase.primitive.integer,
      )
      assert.deepEqual(primitivePayload(DataLayout.floatSpec(layout, 64)), testCase.primitive.float)
      assert.deepEqual(
        primitivePayload(DataLayout.vectorSpec(layout, 128)),
        testCase.primitive.vector,
      )
      assert.deepEqual(pointerPayload(DataLayout.pointerSpec(layout)), testCase.pointerZero)
      assert.deepEqual(
        pointerPayload(DataLayout.pointerSpec(layout, addressOne)),
        testCase.pointerOne,
      )
    }
  }),
)

it.effect('matches pinned LLVM 22.1.8 effective integer specifications', () =>
  Effect.gen(function* () {
    const completeEmpty = DataLayout.empty
    const endianOnly = yield* DataLayout.parse('e')
    const sparseSource = 'e-i16:16:16-i16:32:32-i32:64:64'
    const sparseOverride = yield* DataLayout.parse(sparseSource)

    assert.deepEqual(completeEmpty.integers, [])
    assert.deepEqual(endianOnly.integers, [])
    assert.deepEqual(sparseOverride.integers.map(primitivePayload), [
      { bitWidth: 16, abiAlignment: 4n, preferredAlignment: 4n },
      { bitWidth: 32, abiAlignment: 8n, preferredAlignment: 8n },
    ])
    assert.isTrue(ByteString.equals(DataLayout.render(completeEmpty), ByteString.empty))
    assert.isTrue(ByteString.equals(DataLayout.render(endianOnly), ByteString.fromString('e')))
    assert.isTrue(
      ByteString.equals(DataLayout.render(sparseOverride), ByteString.fromString(sparseSource)),
    )

    for (const layout of [completeEmpty, endianOnly]) {
      assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(layout, 8)), {
        bitWidth: 8,
        abiAlignment: 1n,
        preferredAlignment: 1n,
      })
      assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(layout, 9)), {
        bitWidth: 16,
        abiAlignment: 2n,
        preferredAlignment: 2n,
      })
      assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(layout, 65)), {
        bitWidth: 64,
        abiAlignment: 4n,
        preferredAlignment: 8n,
      })
    }

    assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(sparseOverride, 8)), {
      bitWidth: 8,
      abiAlignment: 1n,
      preferredAlignment: 1n,
    })
    assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(sparseOverride, 16)), {
      bitWidth: 16,
      abiAlignment: 4n,
      preferredAlignment: 4n,
    })
    assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(sparseOverride, 9)), {
      bitWidth: 16,
      abiAlignment: 4n,
      preferredAlignment: 4n,
    })
    assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(sparseOverride, 24)), {
      bitWidth: 32,
      abiAlignment: 8n,
      preferredAlignment: 8n,
    })
    assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(sparseOverride, 40)), {
      bitWidth: 64,
      abiAlignment: 4n,
      preferredAlignment: 8n,
    })
    assert.deepEqual(primitivePayload(DataLayout.effectiveIntegerSpec(sparseOverride, 128)), {
      bitWidth: 64,
      abiAlignment: 4n,
      preferredAlignment: 8n,
    })
  }),
)

it.effect('keeps semantic collections canonical without rewriting the source layout', () =>
  Effect.gen(function* () {
    const input =
      'E-e-m:e-m:o-S64-S128-P1-P2-G3-G4-A5-A6-Fi32-Fn64-n32:64-n8:16-ni:3-ni:1-i64:64-i8:8-i64:128-f64:64-f32:32-f64:128-v128:128-v64:64-v128:256-p2:32:32-p:64:64-p2:64:64'
    const layout = yield* DataLayout.parse(input)

    assert.strictEqual(layout.endian, 'little')
    assert.strictEqual(layout.mangling, 'o')
    assert.strictEqual(layout.stackAlignment.byteUnits, 16n)
    assert.strictEqual(layout.programAddressSpace.value, 2)
    assert.strictEqual(layout.globalAddressSpace.value, 4)
    assert.strictEqual(layout.allocaAddressSpace.value, 6)
    assert.strictEqual(layout.functionPointerAlignment, 'Fn64')
    assert.deepEqual(layout.nativeIntegerWidths, [8, 16, 32, 64])
    assert.deepEqual(
      layout.nonIntegralAddressSpaces.map((address) => address.value),
      [1, 3],
    )
    assert.deepEqual(
      layout.integers.map((spec) => spec.bitWidth),
      [8, 64],
    )
    assert.deepEqual(
      layout.floats.map((spec) => spec.bitWidth),
      [32, 64],
    )
    assert.deepEqual(
      layout.vectors.map((spec) => spec.bitWidth),
      [64, 128],
    )
    assert.deepEqual(
      layout.pointers.map((spec) => spec.addressSpace.value),
      [0, 2],
    )
    assert.strictEqual(DataLayout.integerSpec(layout, 64)?.abiAlignment.byteUnits, 16n)
    assert.strictEqual(DataLayout.floatSpec(layout, 64)?.abiAlignment.byteUnits, 16n)
    assert.strictEqual(DataLayout.vectorSpec(layout, 128)?.abiAlignment.byteUnits, 32n)
    assert.strictEqual(DataLayout.pointerSpec(layout, yield* AddrSpace.make(2))?.bitWidth, 64)
    assert.isTrue(ByteString.equals(DataLayout.render(layout), ByteString.fromString(input)))
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

it.effect('matches pinned LLVM aggregate alignment parsing and validation', () =>
  Effect.gen(function* () {
    const absent = yield* DataLayout.parse('e')
    const zero = yield* DataLayout.parse('e-a:0')
    const zeroWithPreferred = yield* DataLayout.parse('e-a:0:64')
    const nonzero = yield* DataLayout.parse('e-a:64:128')
    const maximum = yield* DataLayout.parse('e-a:32768:32768')
    const repeatedSource = 'e-a:64:128-a:0:64'
    const repeated = yield* DataLayout.parse(repeatedSource)

    assert.strictEqual(absent.aggregate.abiAlignment.byteUnits, 1n)
    assert.strictEqual(absent.aggregate.preferredAlignment.byteUnits, 8n)
    assert.strictEqual(zero.aggregate.abiAlignment.byteUnits, 1n)
    assert.strictEqual(zero.aggregate.preferredAlignment.byteUnits, 1n)
    assert.strictEqual(zeroWithPreferred.aggregate.abiAlignment.byteUnits, 1n)
    assert.strictEqual(zeroWithPreferred.aggregate.preferredAlignment.byteUnits, 8n)
    assert.strictEqual(nonzero.aggregate.abiAlignment.byteUnits, 8n)
    assert.strictEqual(nonzero.aggregate.preferredAlignment.byteUnits, 16n)
    assert.strictEqual(maximum.aggregate.abiAlignment.byteUnits, 4096n)
    assert.strictEqual(maximum.aggregate.preferredAlignment.byteUnits, 4096n)
    assert.strictEqual(repeated.aggregate.abiAlignment.byteUnits, 1n)
    assert.strictEqual(repeated.aggregate.preferredAlignment.byteUnits, 8n)
    assert.strictEqual(
      ByteString.equals(DataLayout.render(repeated), ByteString.fromString(repeatedSource)),
      true,
    )

    for (const malformed of ['e-a:0:0', 'e-a:24', 'e-a:64:32', 'e-a:65536', 'e-a:0:65536']) {
      const error = yield* Effect.flip(DataLayout.parse(malformed))
      assert.instanceOf(error, LlvmError)
      assert.strictEqual(error.operation, 'DataLayout.parse')
      assert.deepEqual(error.reason, { _tag: 'InvalidInput', input: malformed.slice(2) })
      assert.include(error.message, malformed.slice(2))
    }
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
