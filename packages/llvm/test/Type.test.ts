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
