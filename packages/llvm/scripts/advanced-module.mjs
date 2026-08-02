import * as Effect from 'effect/Effect'
import * as AddrSpace from '../dist/AddrSpace.js'
import * as Alignment from '../dist/Alignment.js'
import * as Bitcode from '../dist/Bitcode.js'
import * as Block from '../dist/Block.js'
import * as Builder from '../dist/Builder.js'
import * as Constant from '../dist/Constant.js'
import * as FunctionActor from '../dist/Function.js'
import * as FunctionBody from '../dist/FunctionBody.js'
import * as Intrinsic from '../dist/Intrinsic.js'
import * as IrText from '../dist/IrText.js'
import * as Type from '../dist/Type.js'
import * as Value from '../dist/Value.js'

const output = await Effect.runPromise(
  Effect.gen(function* () {
    const builder = yield* Builder.make({
      sourceFilename: 'advanced.ll',
      dataLayout: 'e-p:64:64-i32:32-i64:64-n32:64',
      targetTriple: 'aarch64-unknown-linux',
    })
    const voidType = yield* Type.voidType(builder)
    const i8 = yield* Type.integer(builder, 8)
    const i32 = yield* Type.integer(builder, 32)
    const i64 = yield* Type.integer(builder, 64)
    const float = yield* Type.float(builder)
    const pointer = yield* Type.pointer(builder)
    const vector = yield* Type.vector(builder, i32, 4)
    const pair = yield* Type.structure(builder, [i32, vector])
    const alignment = yield* Alignment.fromByteUnits(4)
    const addressSpaceOne = yield* AddrSpace.make(1)
    const zero = yield* Constant.integerUnsigned(builder, i32, 0)
    const one = yield* Constant.integerUnsigned(builder, i32, 1)
    const vectorZero = yield* Constant.zero(builder, vector)

    const advancedType = yield* Type.functionType(builder, voidType, [pointer, i32, vector])
    const advanced = yield* FunctionActor.declare(builder, 'advanced', advancedType)
    yield* FunctionActor.buildBody(
      builder,
      advanced,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const address = yield* Value.argument(body, 0)
        const value = yield* Value.argument(body, 1)
        const vectorValue = yield* Value.argument(body, 2)
        const slot = yield* FunctionBody.alloca(body, pair, 'slot', { alignment })
        const field = yield* FunctionBody.structuredGetElementPtr(body, pair, slot, [0], 'field', {
          inbounds: true,
        })
        yield* FunctionBody.store(body, value, field, { alignment, kind: 'volatile' })
        yield* FunctionBody.load(body, i32, field, 'loaded', { alignment })
        yield* FunctionBody.extractElement(body, vectorValue, zero, 'first')
        const inserted = yield* FunctionBody.insertElement(body, vectorZero, value, one, 'inserted')
        yield* FunctionBody.shuffleVector(body, inserted, vectorZero, vectorZero, 'shuffled')
        yield* FunctionBody.atomicRmw(body, 'add', address, one, 'old', {
          alignment,
          ordering: 'monotonic',
        })
        yield* FunctionBody.compareExchange(body, address, value, one, 'pair_result', {
          alignment,
          ordering: 'acq_rel',
          failureOrdering: 'acquire',
          weak: true,
        })
        yield* FunctionBody.load(body, i32, address, 'atomic_loaded', {
          alignment,
          ordering: 'acquire',
          syncScope: 'singlethread',
        })
        yield* FunctionBody.store(body, value, address, {
          alignment,
          ordering: 'release',
        })
        yield* FunctionBody.fence(body, 'seq_cst')
        yield* FunctionBody.returnVoid(body)
      }),
    )

    const scalable = yield* Type.scalableVector(builder, i32, 4)
    const scalableType = yield* Type.functionType(builder, voidType, [i32, scalable])
    const scalableUser = yield* FunctionActor.declare(builder, 'scalable_vectors', scalableType)
    yield* FunctionActor.buildBody(
      builder,
      scalableUser,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const scalar = yield* Value.argument(body, 0)
        const input = yield* Value.argument(body, 1)
        yield* FunctionBody.splatVector(body, scalable, scalar, 'splat')
        yield* FunctionBody.shuffleVector(
          body,
          input,
          input,
          yield* Constant.poison(builder, scalable),
          'poison_shuffle',
        )
        yield* FunctionBody.returnVoid(body)
      }),
    )

    const atomicType = yield* Type.functionType(builder, voidType, [pointer, i32, float])
    const atomicInventory = yield* FunctionActor.declare(builder, 'atomic_inventory', atomicType)
    yield* FunctionActor.buildBody(
      builder,
      atomicInventory,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const address = yield* Value.argument(body, 0)
        const integer = yield* Value.argument(body, 1)
        const floating = yield* Value.argument(body, 2)
        for (const operation of [
          'xchg',
          'add',
          'sub',
          'and',
          'nand',
          'or',
          'xor',
          'max',
          'min',
          'umax',
          'umin',
        ]) {
          yield* FunctionBody.atomicRmw(body, operation, address, integer, operation, {
            ordering: 'monotonic',
          })
        }
        for (const operation of ['fadd', 'fsub', 'fmax', 'fmin']) {
          yield* FunctionBody.atomicRmw(body, operation, address, floating, operation, {
            kind: 'volatile',
            ordering: 'seq_cst',
            syncScope: 'singlethread',
          })
        }
        yield* FunctionBody.returnVoid(body)
      }),
    )

    const intrinsicType = yield* Type.functionType(builder, voidType, [pointer, pointer, i64, i8])
    const intrinsicUser = yield* FunctionActor.declare(builder, 'intrinsic_user', intrinsicType)
    yield* FunctionActor.buildBody(
      builder,
      intrinsicUser,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const destination = yield* Value.argument(body, 0)
        const source = yield* Value.argument(body, 1)
        const length = yield* Value.argument(body, 2)
        const byte = yield* Value.argument(body, 3)
        yield* FunctionBody.alloca(body, i8, 'inalloca_slot', {
          addressSpace: addressSpaceOne,
          alignment,
          inAlloca: true,
        })
        yield* Intrinsic.memcpy(body, destination, source, length, {
          destinationAlignment: alignment,
          sourceAlignment: alignment,
        })
        yield* Intrinsic.memmove(body, destination, source, length, {
          destinationAlignment: alignment,
          sourceAlignment: alignment,
          volatile: true,
        })
        yield* Intrinsic.memset(body, destination, byte, length, {
          destinationAlignment: alignment,
        })
        yield* Intrinsic.assumeCold(body)
        const assemblyType = yield* Type.functionType(builder, voidType, [])
        yield* FunctionBody.callAssembly(body, assemblyType, 'nop', '', [], undefined, {
          sideEffect: true,
          alignStack: true,
        })
        yield* FunctionBody.returnVoid(body)
      }),
    )

    const indirectType = yield* Type.functionType(builder, voidType, [pointer])
    const indirect = yield* FunctionActor.declare(builder, 'indirect', indirectType)
    yield* FunctionActor.buildBody(
      builder,
      indirect,
      Effect.fnUntraced(function* (body) {
        const entry = yield* Block.make(body, 'entry')
        const target = yield* Block.make(body, 'target')
        yield* Block.setInsertionPoint(body, entry)
        yield* FunctionBody.indirectBranch(body, yield* Value.argument(body, 0), [target])
        yield* Block.setInsertionPoint(body, target)
        yield* FunctionBody.returnVoid(body)
      }),
    )
    yield* Constant.blockAddress(builder, indirect, 1)
    yield* Constant.dsoLocalEquivalent(builder, indirect)
    yield* Constant.noCfi(builder, indirect)

    const varargType = yield* Type.functionType(builder, i32, [pointer], { variadic: true })
    const vararg = yield* FunctionActor.declare(builder, 'read_vararg', varargType)
    yield* FunctionActor.buildBody(
      builder,
      vararg,
      Effect.fnUntraced(function* (body) {
        yield* Block.make(body, 'entry')
        const list = yield* Value.argument(body, 0)
        yield* FunctionBody.returnValue(body, yield* FunctionBody.vaArg(body, list, i32, 'next'))
      }),
    )

    return process.argv[2] === 'bitcode'
      ? Buffer.from(yield* Bitcode.encode(builder))
      : Buffer.from(yield* IrText.render(builder))
  }),
)

process.stdout.write(output)
