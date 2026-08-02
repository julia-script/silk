import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import { pipe } from 'effect/Function'
import * as AddrSpace from '../src/AddrSpace.js'
import * as Alignment from '../src/Alignment.js'
import * as Block from '../src/Block.js'
import * as Builder from '../src/Builder.js'
import * as Constant from '../src/Constant.js'
import * as FastMath from '../src/FastMath.js'
import * as FunctionActor from '../src/Function.js'
import * as FunctionBody from '../src/FunctionBody.js'
import * as IntegerMath from '../src/IntegerMath.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as IrText from '../src/IrText.js'
import { LlvmError } from '../src/LlvmError.js'
import * as MemoryAccess from '../src/MemoryAccess.js'
import * as Type from '../src/Type.js'
import * as Value from '../src/Value.js'

it.effect('models fast-math and memory settings with pinned text and bit encodings', () =>
  Effect.gen(function* () {
    const flags = FastMath.make({ noNaNs: true, allowContract: true })
    assert.deepEqual(FastMath.toText(flags), ['nnan', 'contract'])
    assert.strictEqual(FastMath.toBitcode(flags), (1 << 1) | (1 << 5))
    assert.deepEqual(FastMath.toText(FastMath.fast), ['fast'])
    const integerDataFirst = IntegerMath.withExact(IntegerMath.withNoSignedWrap(IntegerMath.none))
    const integerPipeable = pipe(
      IntegerMath.none,
      IntegerMath.withNoSignedWrap(),
      IntegerMath.withExact(),
    )
    assert.deepEqual(integerPipeable, integerDataFirst)
    assert.deepEqual(IntegerMath.toText(integerPipeable), ['nsw', 'exact'])
    assert.deepEqual(
      pipe(IntegerMath.none, IntegerMath.withNoUnsignedWrap(false)),
      IntegerMath.withNoUnsignedWrap(IntegerMath.none, false),
    )

    const combinedDataFirst = FastMath.combine(FastMath.none, {
      noNaNs: true,
      allowContract: true,
    })
    const combinedPipeable = pipe(
      FastMath.none,
      FastMath.combine({ noNaNs: true, allowContract: true }),
    )
    assert.deepEqual(combinedPipeable, combinedDataFirst)
    assert.notStrictEqual(combinedPipeable, FastMath.none)

    const alignment = yield* Alignment.fromByteUnits(16)
    const access = MemoryAccess.withAtomic(
      MemoryAccess.withVolatile(MemoryAccess.make({ alignment })),
      'acquire',
      'singlethread',
    )
    const pipeableAccess = pipe(
      MemoryAccess.make({ alignment }),
      MemoryAccess.withVolatile(),
      MemoryAccess.withAtomic('acquire', 'singlethread'),
    )
    assert.deepEqual(pipeableAccess, access)
    assert.deepEqual(
      pipe(MemoryAccess.make(), MemoryAccess.withVolatile(false)),
      MemoryAccess.withVolatile(MemoryAccess.make(), false),
    )
    assert.deepEqual(access, {
      kind: 'volatile',
      alignment,
      syncScope: 'singlethread',
      ordering: 'acquire',
    })
    assert.strictEqual(yield* MemoryAccess.alignmentCode(alignment), 5)
    const oversizedAlignment = yield* Alignment.fromByteUnits(1n << 63n)
    const oversizedFailure = yield* Effect.flip(MemoryAccess.alignmentCode(oversizedAlignment))
    assert.instanceOf(oversizedFailure, LlvmError)
    assert.strictEqual(oversizedFailure.operation, 'MemoryAccess.alignmentCode')
    for (const ordering of [
      'none',
      'unordered',
      'monotonic',
      'release',
      'seq_cst',
    ] satisfies ReadonlyArray<MemoryAccess.AtomicOrdering>) {
      yield* MemoryAccess.validateStoreOrdering(ordering)
    }
    for (const ordering of [
      'acquire',
      'acq_rel',
    ] satisfies ReadonlyArray<MemoryAccess.AtomicOrdering>) {
      assert.instanceOf(yield* Effect.flip(MemoryAccess.validateStoreOrdering(ordering)), LlvmError)
    }
    for (const ordering of [
      'none',
      'unordered',
      'monotonic',
      'acquire',
      'seq_cst',
    ] satisfies ReadonlyArray<MemoryAccess.AtomicOrdering>) {
      yield* MemoryAccess.validateLoadOrdering(ordering)
    }
    for (const ordering of [
      'release',
      'acq_rel',
    ] satisfies ReadonlyArray<MemoryAccess.AtomicOrdering>) {
      assert.instanceOf(yield* Effect.flip(MemoryAccess.validateLoadOrdering(ordering)), LlvmError)
    }
    assert.instanceOf(
      yield* Effect.flip(MemoryAccess.validateCompareExchange('monotonic', 'acquire')),
      LlvmError,
    )
  }),
)

it.effect('constructs memory, vector, and atomic instructions with exact types', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const voidType = yield* Type.voidType(builder)
    const i1 = yield* Type.integer(builder, 1)
    const i32 = yield* Type.integer(builder, 32)
    const pointer = yield* Type.pointer(builder)
    const vector = yield* Type.vector(builder, i32, 4)
    const aggregate = yield* Type.structure(builder, [i32, vector])
    const signature = yield* Type.functionType(builder, voidType, [pointer, i32, vector])
    const fn = yield* FunctionActor.declare(builder, 'advanced', signature)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const zero = yield* Constant.integerUnsigned(builder, i32, 0)
    const trueValue = yield* Constant.integerUnsigned(builder, i1, 1)
    const vectorZero = yield* Constant.zero(builder, vector)
    const alignment = yield* Alignment.fromByteUnits(4)

    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const address = yield* Value.argument(body, 0)
        const value = yield* Value.argument(body, 1)
        const vectorValue = yield* Value.argument(body, 2)
        const slot = yield* FunctionBody.alloca(body, aggregate, 'slot', { alignment })
        const field = yield* FunctionBody.structuredGetElementPtr(
          body,
          aggregate,
          slot,
          [0],
          'field',
          { inbounds: true },
        )
        yield* FunctionBody.store(body, value, field, { alignment })
        yield* FunctionBody.load(body, i32, field, 'loaded', { alignment, kind: 'volatile' })
        yield* FunctionBody.extractElement(body, vectorValue, zero, 'element')
        const inserted = yield* FunctionBody.insertElement(body, vectorZero, value, one, 'inserted')
        yield* FunctionBody.shuffleVector(body, inserted, vectorZero, vectorZero, 'shuffled')
        yield* FunctionBody.atomicRmw(body, 'add', address, one, 'old', {
          alignment,
          ordering: 'monotonic',
        })
        yield* FunctionBody.compareExchange(body, address, value, one, 'pair', {
          alignment,
          ordering: 'acq_rel',
          failureOrdering: 'acquire',
          weak: true,
        })
        yield* FunctionBody.fence(body, 'seq_cst')
        yield* FunctionBody.select(body, trueValue, value, one, 'selected')
        yield* FunctionBody.returnVoid(body)
      }),
    )

    const text = yield* IrText.render(builder)
    assert.include(text, 'alloca { i32, <4 x i32> }')
    assert.include(text, 'getelementptr inbounds')
    assert.include(text, 'load volatile i32')
    assert.include(text, 'atomicrmw add')
    assert.include(text, 'cmpxchg weak')
    assert.include(text, 'fence seq_cst')
  }),
)

it.effect(
  'resolves the pinned intrinsic inventory canonically and preserves low-level constants',
  () =>
    Effect.gen(function* () {
      const builder = yield* Builder.make()
      const voidType = yield* Type.voidType(builder)
      const i8 = yield* Type.integer(builder, 8)
      const i64 = yield* Type.integer(builder, 64)
      const pointer = yield* Type.pointer(builder)
      const memory = yield* Intrinsic.resolve(builder, 'memcpy', [pointer, pointer, i64])
      assert.strictEqual(
        yield* Intrinsic.resolve(builder, 'memcpy', [pointer, pointer, i64]),
        memory,
      )
      assert.include(Intrinsic.inventory, 'vector.reduce.fmaximum')
      assert.lengthOf(Intrinsic.inventory, 158)
      assert.lengthOf(Intrinsic.catalog, 158)
      assert.strictEqual(
        Intrinsic.catalog.find((entry) => entry.id === 'memcpy')?.signature,
        'built-in',
      )

      const functionType = yield* Type.functionType(builder, voidType, [])
      const fn = yield* FunctionActor.declare(builder, 'assembly_and_intrinsics', functionType)
      yield* FunctionActor.buildBody(
        builder,
        fn,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'entry')
          const slot = yield* FunctionBody.alloca(body, i8, 'slot')
          const length = yield* Constant.integerUnsigned(builder, i64, 1)
          yield* Intrinsic.memset(
            body,
            slot,
            yield* Constant.integerUnsigned(builder, i8, 0),
            length,
          )
          yield* Intrinsic.assumeCold(body)
          yield* FunctionBody.callAssembly(body, functionType, 'nop', '', [], undefined, {
            sideEffect: true,
          })
          yield* FunctionBody.returnVoid(body)
        }),
      )

      const dso = yield* Constant.dsoLocalEquivalent(builder, fn)
      const noCfi = yield* Constant.noCfi(builder, fn)
      const address = yield* Constant.blockAddress(builder, fn, 0)
      assert.strictEqual(yield* Constant.tag(builder, dso), 'FunctionReference')
      assert.strictEqual(yield* Constant.tag(builder, noCfi), 'FunctionReference')
      assert.strictEqual(yield* Constant.tag(builder, address), 'BlockAddress')

      const text = yield* IrText.render(builder)
      assert.include(text, '@llvm.memset.p0.i64')
      assert.include(text, 'asm sideeffect "nop", ""')
      assert.include(text, '[ "cold"() ]')
    }),
)

it.effect('preserves scalable splats, poison masks, and vector GEP result shapes', () =>
  Effect.gen(function* () {
    const builder = yield* Builder.make()
    const voidType = yield* Type.voidType(builder)
    const i32 = yield* Type.integer(builder, 32)
    const pointer = yield* Type.pointer(builder)
    const fixed = yield* Type.vector(builder, i32, 4)
    const scalable = yield* Type.scalableVector(builder, i32, 4)
    const signature = yield* Type.functionType(builder, voidType, [pointer, i32, scalable])
    const fn = yield* FunctionActor.declare(builder, 'vector_shapes', signature)
    const vectorIndex = yield* Constant.zero(builder, fixed)
    const poisonMask = yield* Constant.poison(builder, scalable)

    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const base = yield* Value.argument(body, 0)
        const scalar = yield* Value.argument(body, 1)
        const input = yield* Value.argument(body, 2)
        const pointers = yield* FunctionBody.getElementPtr(
          body,
          i32,
          base,
          [vectorIndex],
          'pointers',
        )
        assert.strictEqual(yield* Type.tag(builder, yield* Value.typeOf(body, pointers)), 'Vector')
        const splat = yield* FunctionBody.splatVector(body, scalable, scalar, 'splat')
        assert.strictEqual(yield* Type.tag(builder, yield* Value.typeOf(body, splat)), 'Vector')
        yield* FunctionBody.shuffleVector(body, input, input, poisonMask, 'poison_shuffle')
        const badMask = yield* Constant.zero(builder, fixed)
        const mismatch = yield* Effect.flip(
          FunctionBody.shuffleVector(body, input, input, badMask, 'bad'),
        )
        assert.include(mismatch.message, 'compatible vectors')
        yield* FunctionBody.returnVoid(body)
      }),
    )
  }),
)

it.effect(
  'rejects invalid memory paths, alignments, flag families, and atomic relations transactionally',
  () =>
    Effect.gen(function* () {
      assert.include((yield* Effect.flip(Alignment.fromByteUnits(3))).message, 'power of two')
      const builder = yield* Builder.make()
      const voidType = yield* Type.voidType(builder)
      const i32 = yield* Type.integer(builder, 32)
      const pointer = yield* Type.pointer(builder)
      const aggregate = yield* Type.structure(builder, [i32])
      const signature = yield* Type.functionType(builder, voidType, [pointer, i32])
      const fn = yield* FunctionActor.declare(builder, 'invalid_advanced', signature)

      yield* FunctionActor.buildBody(
        builder,
        fn,
        Effect.fnUntraced(function* (body) {
          yield* Block.make(body, 'entry')
          const address = yield* Value.argument(body, 0)
          const value = yield* Value.argument(body, 1)
          const local = yield* FunctionBody.alloca(body, aggregate, 'local', {
            addressSpace: yield* AddrSpace.make(1),
            inAlloca: true,
          })
          assert.include(
            (yield* Effect.flip(FunctionBody.structuredGetElementPtr(body, aggregate, local, [9])))
              .message,
            'outside',
          )
          assert.include(
            (yield* Effect.flip(
              FunctionBody.load(body, i32, address, 'bad_load', { ordering: 'release' }),
            )).message,
            'loads cannot',
          )
          assert.include(
            (yield* Effect.flip(
              FunctionBody.compareExchange(body, address, value, value, 'bad_cmpxchg', {
                ordering: 'monotonic',
                failureOrdering: 'acquire',
              }),
            )).message,
            'stronger',
          )
          assert.include(
            (yield* Effect.flip(
              FunctionBody.binary(body, 'add', value, value, 'bad_fast', { fastMath: true }),
            )).message,
            'Fast-math',
          )
          yield* FunctionBody.returnVoid(body)
        }),
      )

      assert.include(yield* IrText.render(builder), 'alloca inalloca')
    }),
)
