import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Builder from '../src/Builder.js'
import { LlvmError } from '../src/LlvmError.js'
import * as Type from '../src/Type.js'

it.effect('structurally interns primitive, compound, and target-extension types', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const firstI32 = yield* Type.integer(builder, 32)
    const secondI32 = yield* Type.integer(builder, 32)
    const firstArray = yield* Type.array(builder, firstI32, 4n)
    const secondArray = yield* Type.array(builder, secondI32, 4)
    const firstFunction = yield* Type.functionType(builder, firstI32, [firstArray])
    const secondFunction = yield* Type.functionType(builder, secondI32, [secondArray])
    const firstExtension = yield* Type.targetExtension(builder, 'spirv.Image', [firstI32], [1n])
    const secondExtension = yield* Type.targetExtension(builder, 'spirv.Image', [secondI32], [1])

    assert.strictEqual(firstI32, secondI32)
    assert.strictEqual(firstArray, secondArray)
    assert.strictEqual(firstFunction, secondFunction)
    assert.strictEqual(firstExtension, secondExtension)
    assert.strictEqual(yield* Type.integerBitWidth(builder, firstI32), 32)
  }),
)

it.effect('completes one recursive named structure body under a stable identity', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const node = yield* Type.namedStructure(builder, 'Node')
    const sameNode = yield* Type.namedStructure(builder, 'Node')
    yield* Type.setNamedBody(builder, node, [node])
    const secondBodyError = yield* Effect.flip(Type.setNamedBody(builder, node, []))
    const recursiveSizeError = yield* Effect.flip(Type.sizeOf(builder, node))

    assert.strictEqual(node, sameNode)
    assert.deepEqual((yield* Type.aggregateShape(builder, node)).fields, [node])
    assert.include(secondBodyError.message, 'only be assigned once')
    assert.strictEqual(recursiveSizeError.message, 'Type has no fixed size')
  }),
)

it.effect('computes fixed aggregate layout and rejects cross-owner handles', () =>
  Effect.gen(function* () {
    const first = yield* Builder.make({ dataLayout: 'e-p:64:64-i8:8-i32:32' })
    const second = yield* Builder.make({ dataLayout: 'e-p:64:64-i8:8-i32:32' })
    const i8 = yield* Type.integer(first, 8)
    const i32 = yield* Type.integer(first, 32)
    const structure = yield* Type.structure(first, [i8, i32])
    const ownerError = yield* Effect.flip(Type.tag(second, structure))

    assert.strictEqual(yield* Type.sizeOf(first, structure), 8n)
    assert.strictEqual((yield* Type.alignmentOf(first, structure)).byteUnits, 4n)
    assert.include(ownerError.message, 'different LLVM builder')
  }),
)

it.effect('matches pinned LLVM aggregate alignment for structures and arrays', () =>
  Effect.gen(function* () {
    const absentBuilder = yield* Builder.make({ dataLayout: 'e-p:64:64-i8:8' })
    const absentI8 = yield* Type.integer(absentBuilder, 8)
    const absent = yield* Type.structure(absentBuilder, [absentI8])

    const zeroBuilder = yield* Builder.make({ dataLayout: 'e-a:0:64-p:64:64-i8:8' })
    const zeroI8 = yield* Type.integer(zeroBuilder, 8)
    const zero = yield* Type.structure(zeroBuilder, [zeroI8])

    const builder = yield* Builder.make({
      dataLayout: 'e-a:64:64-p:64:64-i8:8-i128:128',
    })
    const i8 = yield* Type.integer(builder, 8)
    const i128 = yield* Type.integer(builder, 128)
    const anonymous = yield* Type.structure(builder, [i8])
    const empty = yield* Type.structure(builder, [])
    const packed = yield* Type.structure(builder, [i8], { packed: true })
    const named = yield* Type.namedStructure(builder, 'AggregateAligned')
    yield* Type.setNamedBody(builder, named, [i8])
    const strongerField = yield* Type.structure(builder, [i128])
    const array = yield* Type.array(builder, i8, 3)

    assert.strictEqual(yield* Type.sizeOf(absentBuilder, absent), 1n)
    assert.strictEqual((yield* Type.alignmentOf(absentBuilder, absent)).byteUnits, 1n)
    assert.strictEqual(yield* Type.sizeOf(zeroBuilder, zero), 1n)
    assert.strictEqual((yield* Type.alignmentOf(zeroBuilder, zero)).byteUnits, 1n)
    assert.strictEqual(yield* Type.sizeOf(builder, anonymous), 8n)
    assert.strictEqual((yield* Type.alignmentOf(builder, anonymous)).byteUnits, 8n)
    assert.strictEqual(yield* Type.sizeOf(builder, empty), 0n)
    assert.strictEqual((yield* Type.alignmentOf(builder, empty)).byteUnits, 8n)
    assert.strictEqual(yield* Type.sizeOf(builder, packed), 1n)
    assert.strictEqual((yield* Type.alignmentOf(builder, packed)).byteUnits, 1n)
    assert.strictEqual(yield* Type.sizeOf(builder, named), 8n)
    assert.strictEqual((yield* Type.alignmentOf(builder, named)).byteUnits, 8n)
    assert.strictEqual(yield* Type.sizeOf(builder, strongerField), 16n)
    assert.strictEqual((yield* Type.alignmentOf(builder, strongerField)).byteUnits, 16n)
    assert.strictEqual(yield* Type.sizeOf(builder, array), 3n)
    assert.strictEqual((yield* Type.alignmentOf(builder, array)).byteUnits, 1n)
  }),
)

it.effect('matches pinned LLVM 22.1.8 integer allocation size and aggregate propagation', () =>
  Effect.gen(function* () {
    const cases = [
      { name: 'complete empty', dataLayout: '' },
      { name: 'endian only', dataLayout: 'e' },
      { name: 'sparse override', dataLayout: 'e-i16:32-i32:64' },
    ] as const

    for (const testCase of cases) {
      const builder = yield* Builder.make({ dataLayout: testCase.dataLayout })
      const i9 = yield* Type.integer(builder, 9)
      const i24 = yield* Type.integer(builder, 24)
      const i40 = yield* Type.integer(builder, 40)
      const i65 = yield* Type.integer(builder, 65)

      const expected =
        testCase.name === 'sparse override'
          ? [
              [4n, 4n],
              [8n, 8n],
              [8n, 4n],
              [12n, 4n],
            ]
          : [
              [2n, 2n],
              [4n, 4n],
              [8n, 4n],
              [12n, 4n],
            ]

      const types = [i9, i24, i40, i65]
      for (const [index, type] of types.entries()) {
        const result = expected[index]
        assert.isDefined(result, `${testCase.name}: missing expected layout for type ${index}`)
        assert.strictEqual(yield* Type.sizeOf(builder, type), result[0], testCase.name)
        assert.strictEqual(
          (yield* Type.alignmentOf(builder, type)).byteUnits,
          result[1],
          testCase.name,
        )
      }

      if (testCase.name === 'sparse override') {
        const array = yield* Type.array(builder, i9, 3)
        const vector = yield* Type.vector(builder, i9, 3)
        const naturallyAlignedVector = yield* Type.vector(builder, i9, 8)
        const explicitVectorBuilder = yield* Builder.make({
          dataLayout: 'e-i16:32-i32:64-v27:64',
        })
        const explicitVectorElement = yield* Type.integer(explicitVectorBuilder, 9)
        const explicitlyAlignedVector = yield* Type.vector(
          explicitVectorBuilder,
          explicitVectorElement,
          3,
        )
        const structure = yield* Type.structure(builder, [i9, i40])
        assert.strictEqual(yield* Type.sizeOf(builder, array), 12n)
        assert.strictEqual((yield* Type.alignmentOf(builder, array)).byteUnits, 4n)
        assert.strictEqual(yield* Type.sizeOf(builder, vector), 4n)
        assert.strictEqual((yield* Type.alignmentOf(builder, vector)).byteUnits, 4n)
        assert.strictEqual(yield* Type.sizeOf(builder, naturallyAlignedVector), 16n)
        assert.strictEqual(
          (yield* Type.alignmentOf(builder, naturallyAlignedVector)).byteUnits,
          16n,
        )
        assert.strictEqual(yield* Type.sizeOf(explicitVectorBuilder, explicitlyAlignedVector), 8n)
        assert.strictEqual(
          (yield* Type.alignmentOf(explicitVectorBuilder, explicitlyAlignedVector)).byteUnits,
          8n,
        )
        assert.strictEqual(yield* Type.sizeOf(builder, structure), 12n)
        assert.strictEqual((yield* Type.alignmentOf(builder, structure)).byteUnits, 4n)
      }
    }
  }),
)

it.effect('accepts boundary widths and rejects invalid widths before mutation', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    assert.strictEqual(yield* Type.integerBitWidth(builder, yield* Type.integer(builder, 1)), 1)
    assert.strictEqual(
      yield* Type.integerBitWidth(builder, yield* Type.integer(builder, 0xff_ffff)),
      0xff_ffff,
    )
    assert.strictEqual((yield* Effect.flip(Type.integer(builder, 0))).operation, 'Type.integer')
  }),
)

it.effect('validates array lengths and target-extension integer parameters without defects', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const i8 = yield* Type.integer(builder, 8)
    for (const length of [1.5, Number.NaN, Number.POSITIVE_INFINITY, Number.MAX_SAFE_INTEGER + 1]) {
      assert.instanceOf(yield* Effect.flip(Type.array(builder, i8, length)), LlvmError)
    }
    assert.instanceOf(yield* Effect.flip(Type.array(builder, i8, -1)), LlvmError)
    assert.instanceOf(
      yield* Effect.flip(Type.array(builder, i8, 0x1_0000_0000_0000_0000n)),
      LlvmError,
    )
    assert.instanceOf(
      yield* Effect.flip(Type.targetExtension(builder, 'test', [], [Number.NaN])),
      LlvmError,
    )
  }),
)
