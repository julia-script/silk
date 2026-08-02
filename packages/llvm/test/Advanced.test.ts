import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import { expect, test } from 'vitest'
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
import * as MemoryAccess from '../src/MemoryAccess.js'
import { SilkError } from '../src/SilkError.js'
import * as Type from '../src/Type.js'
import * as Value from '../src/Value.js'

const TestRuntime = ManagedRuntime.make(Layer.empty)

test(
  'models fast-math and memory settings with pinned text and bit encodings',
  Effect.fnUntraced(function* () {
    const flags = FastMath.make({ noNaNs: true, allowContract: true })
    expect(FastMath.toText(flags)).toEqual(['nnan', 'contract'])
    expect(FastMath.toBitcode(flags)).toBe((1 << 1) | (1 << 5))
    expect(FastMath.toText(FastMath.fast)).toEqual(['fast'])
    expect(
      IntegerMath.toText(IntegerMath.withExact(IntegerMath.withNoSignedWrap(IntegerMath.none))),
    ).toEqual(['nsw', 'exact'])

    const alignment = yield* Alignment.fromByteUnits(16)
    const access = MemoryAccess.withAtomic(
      MemoryAccess.withVolatile(MemoryAccess.make({ alignment })),
      'acquire',
      'singlethread',
    )
    expect(access).toEqual({
      kind: 'volatile',
      alignment,
      syncScope: 'singlethread',
      ordering: 'acquire',
    })
    expect(MemoryAccess.alignmentCode(alignment)).toBe(5)
    for (const ordering of ['none', 'unordered', 'monotonic', 'release', 'seq_cst']) {
      yield* MemoryAccess.validateStoreOrdering(ordering)
    }
    for (const ordering of ['acquire', 'acq_rel']) {
      expect(yield* Effect.flip(MemoryAccess.validateStoreOrdering(ordering))).toBeInstanceOf(
        SilkError,
      )
    }
    for (const ordering of ['none', 'unordered', 'monotonic', 'acquire', 'seq_cst']) {
      yield* MemoryAccess.validateLoadOrdering(ordering)
    }
    for (const ordering of ['release', 'acq_rel']) {
      expect(yield* Effect.flip(MemoryAccess.validateLoadOrdering(ordering))).toBeInstanceOf(
        SilkError,
      )
    }
    expect(
      yield* Effect.flip(MemoryAccess.validateCompareExchange('monotonic', 'acquire')),
    ).toBeInstanceOf(SilkError)
  }, TestRuntime.runPromise),
)

test(
  'constructs memory, vector, and atomic instructions with exact types',
  Effect.fnUntraced(function* () {
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
    expect(text).toContain('alloca { i32, <4 x i32> }')
    expect(text).toContain('getelementptr inbounds')
    expect(text).toContain('load volatile i32')
    expect(text).toContain('atomicrmw add')
    expect(text).toContain('cmpxchg weak')
    expect(text).toContain('fence seq_cst')
  }, TestRuntime.runPromise),
)

test(
  'resolves the pinned intrinsic inventory canonically and preserves low-level constants',
  Effect.fnUntraced(function* () {
    const builder = yield* Builder.make()
    const voidType = yield* Type.voidType(builder)
    const i8 = yield* Type.integer(builder, 8)
    const i64 = yield* Type.integer(builder, 64)
    const pointer = yield* Type.pointer(builder)
    const memory = yield* Intrinsic.resolve(builder, 'memcpy', [pointer, pointer, i64])
    expect(yield* Intrinsic.resolve(builder, 'memcpy', [pointer, pointer, i64])).toBe(memory)
    expect(Intrinsic.inventory).toContain('vector.reduce.fmaximum')
    expect(Intrinsic.inventory).toHaveLength(158)
    expect(Intrinsic.catalog).toHaveLength(158)
    expect(Intrinsic.catalog.find((entry) => entry.id === 'memcpy')?.signature).toBe('built-in')

    const functionType = yield* Type.functionType(builder, voidType, [])
    const fn = yield* FunctionActor.declare(builder, 'assembly_and_intrinsics', functionType)
    yield* FunctionActor.buildBody(
      builder,
      fn,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const slot = yield* FunctionBody.alloca(body, i8, 'slot')
        const length = yield* Constant.integerUnsigned(builder, i64, 1)
        yield* Intrinsic.memset(body, slot, yield* Constant.integerUnsigned(builder, i8, 0), length)
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
    expect(yield* Constant.tag(builder, dso)).toBe('FunctionReference')
    expect(yield* Constant.tag(builder, noCfi)).toBe('FunctionReference')
    expect(yield* Constant.tag(builder, address)).toBe('BlockAddress')

    const text = yield* IrText.render(builder)
    expect(text).toContain('@llvm.memset.p0.i64')
    expect(text).toContain('asm sideeffect "nop", ""')
    expect(text).toContain('[ "cold"() ]')
  }, TestRuntime.runPromise),
)

test(
  'preserves scalable splats, poison masks, and vector GEP result shapes',
  Effect.fnUntraced(function* () {
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
        expect(yield* Type.tag(builder, yield* Value.typeOf(body, pointers))).toBe('Vector')
        const splat = yield* FunctionBody.splatVector(body, scalable, scalar, 'splat')
        expect(yield* Type.tag(builder, yield* Value.typeOf(body, splat))).toBe('Vector')
        yield* FunctionBody.shuffleVector(body, input, input, poisonMask, 'poison_shuffle')
        const badMask = yield* Constant.zero(builder, fixed)
        const mismatch = yield* Effect.flip(
          FunctionBody.shuffleVector(body, input, input, badMask, 'bad'),
        )
        expect(mismatch.message).toContain('compatible vectors')
        yield* FunctionBody.returnVoid(body)
      }),
    )
  }, TestRuntime.runPromise),
)

test(
  'rejects invalid memory paths, alignments, flag families, and atomic relations transactionally',
  Effect.fnUntraced(function* () {
    expect((yield* Effect.flip(Alignment.fromByteUnits(3))).message).toContain('power of two')
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
        expect(
          (yield* Effect.flip(FunctionBody.structuredGetElementPtr(body, aggregate, local, [9])))
            .message,
        ).toContain('outside')
        expect(
          (yield* Effect.flip(
            FunctionBody.load(body, i32, address, 'bad_load', { ordering: 'release' }),
          )).message,
        ).toContain('loads cannot')
        expect(
          (yield* Effect.flip(
            FunctionBody.compareExchange(body, address, value, value, 'bad_cmpxchg', {
              ordering: 'monotonic',
              failureOrdering: 'acquire',
            }),
          )).message,
        ).toContain('stronger')
        expect(
          (yield* Effect.flip(
            FunctionBody.binary(body, 'add', value, value, 'bad_fast', { fastMath: true }),
          )).message,
        ).toContain('Fast-math')
        yield* FunctionBody.returnVoid(body)
      }),
    )

    expect(yield* IrText.render(builder)).toContain('alloca inalloca')
  }, TestRuntime.runPromise),
)
