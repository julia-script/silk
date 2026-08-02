import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
import * as Builder from '../src/Builder.js'
import * as Type from '../src/Type.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'structurally interns primitive, compound, and target-extension types',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const firstI32 = yield* Type.integer(builder, 32)
    const secondI32 = yield* Type.integer(builder, 32)
    const firstArray = yield* Type.array(builder, firstI32, 4n)
    const secondArray = yield* Type.array(builder, secondI32, 4)
    const firstFunction = yield* Type.functionType(builder, firstI32, [firstArray])
    const secondFunction = yield* Type.functionType(builder, secondI32, [secondArray])
    const firstExtension = yield* Type.targetExtension(builder, 'spirv.Image', [firstI32], [1n])
    const secondExtension = yield* Type.targetExtension(builder, 'spirv.Image', [secondI32], [1])

    expect(firstI32).toBe(secondI32)
    expect(firstArray).toBe(secondArray)
    expect(firstFunction).toBe(secondFunction)
    expect(firstExtension).toBe(secondExtension)
    expect(yield* Type.integerBitWidth(builder, firstI32)).toBe(32)
  }, TestRuntime.runPromise),
)

test(
  'completes one recursive named structure body under a stable identity',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const node = yield* Type.namedStructure(builder, 'Node')
    const sameNode = yield* Type.namedStructure(builder, 'Node')
    yield* Type.setNamedBody(builder, node, [node])
    const secondBodyError = yield* Effect.flip(Type.setNamedBody(builder, node, []))
    const recursiveSizeError = yield* Effect.flip(Type.sizeOf(builder, node))

    expect(node).toBe(sameNode)
    expect((yield* Type.aggregateShape(builder, node)).fields).toEqual([node])
    expect(secondBodyError.message).toContain('only be assigned once')
    expect(recursiveSizeError.message).toBe('Type has no fixed size')
  }, TestRuntime.runPromise),
)

test(
  'computes fixed aggregate layout and rejects cross-owner handles',
  Effect.fnUntraced(function* () {
    const first = yield* Builder.make({ dataLayout: 'e-p:64:64-i8:8-i32:32' })
    const second = yield* Builder.make({ dataLayout: 'e-p:64:64-i8:8-i32:32' })
    const i8 = yield* Type.integer(first, 8)
    const i32 = yield* Type.integer(first, 32)
    const structure = yield* Type.structure(first, [i8, i32])
    const ownerError = yield* Effect.flip(Type.tag(second, structure))

    expect(yield* Type.sizeOf(first, structure)).toBe(8n)
    expect((yield* Type.alignmentOf(first, structure)).byteUnits).toBe(4n)
    expect(ownerError.message).toContain('different LLVM builder')
  }, TestRuntime.runPromise),
)

test(
  'accepts boundary widths and rejects invalid widths before mutation',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    expect(yield* Type.integerBitWidth(builder, yield* Type.integer(builder, 1))).toBe(1)
    expect(yield* Type.integerBitWidth(builder, yield* Type.integer(builder, 0xff_ffff))).toBe(
      0xff_ffff,
    )
    expect((yield* Effect.flip(Type.integer(builder, 0))).operation).toBe('Type.integer')
  }, TestRuntime.runPromise),
)
