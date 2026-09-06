import * as Alignment from '@silklang/llvm/Alignment'
import * as Type from './Type.js'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import { alignUp } from './internal/Align.js'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeType from './NativeType.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'PointerNull'
      | 'PointerIsNull'
      | 'PointerAddress'
      | 'PointerRequalify'
      | 'PointerFromStorage'
      | 'PointerAt'
      | 'PointerRead'
      | 'PointerWrite'
  }
>

const accessAlignment = (context: Context, local: Mir.LocalId, offset: number): number => {
  const pointer = context.storage.fn.localTypes.at(local.ordinal)
  if (pointer?._tag !== 'Pointer') throw new RangeError('Pointer access lost its qualified type')
  const layout = Layout.entry(context.program.layout, pointer.type.pointee)
  if (layout === undefined) throw new RangeError('Pointer access lost its pointee layout')
  let alignment = pointer.type.alignment === 'Natural' ? layout.alignment : pointer.type.alignment
  while (offset % alignment !== 0) alignment /= 2
  return alignment
}

/**
 * Lowers raw pointers as one LLVM pointer lane: the lane a borrow already carries, so formation
 * copies it, offset is a byte `getelementptr`, and read/write move the pointee's lanes through it.
 */
export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const { builder, body, program, pointer, i32, lanePointers, types, storage } = context
  const destination = operation.destination.ordinal
  switch (operation._tag) {
    case 'PointerNull': {
      storage.locals.set(destination, Object.freeze([yield* Constant.nullValue(builder, pointer)]))
      return
    }
    case 'PointerAddress': {
      const addressType = yield* LlvmType.integer(builder, program.layout.target.pointerSize * 8)
      const address = yield* FunctionBody.cast(
        body,
        'ptrtoint',
        NativeStorage.readScalar(storage, operation.pointer),
        addressType,
        `ptr_address${destination}`,
      )
      storage.locals.set(destination, Object.freeze([address]))
      return
    }
    case 'PointerIsNull': {
      // icmp takes integers only, so the lane is compared as its pointer-width address.
      const addressType = yield* LlvmType.integer(builder, program.layout.target.pointerSize * 8)
      const flag = yield* FunctionBody.integerCompare(
        body,
        'eq',
        yield* FunctionBody.cast(
          body,
          'ptrtoint',
          NativeStorage.readScalar(storage, operation.pointer),
          addressType,
          `ptr_is_null${destination}_address`,
        ),
        yield* Constant.integerUnsigned(builder, addressType, 0n),
        `ptr_is_null${destination}_flag`,
      )
      storage.locals.set(
        destination,
        Object.freeze([
          yield* FunctionBody.cast(body, 'zext', flag, i32, `ptr_is_null${destination}`),
        ]),
      )
      return
    }
    case 'PointerRequalify':
    case 'PointerFromStorage': {
      // A reference is its address lane; a slice is address then length.
      const address = NativeStorage.readLocal(storage, operation.source).at(0)
      if (address === undefined)
        throw new RangeError('LLVM pointer formation lost its address lane')
      const sourceType = storage.fn.localTypes.at(operation.source.ordinal)
      const pointerAddress =
        sourceType !== undefined && Type.isSlot(Mir.semanticType(sourceType))
          ? yield* FunctionBody.cast(
              body,
              'inttoptr',
              address,
              pointer,
              `slot_address${destination}`,
            )
          : address
      storage.locals.set(destination, Object.freeze([pointerAddress]))
      return
    }
    case 'PointerAt': {
      const pointee = Layout.entry(program.layout, operation.type.type.pointee)
      const countType = storage.fn.localTypes.at(operation.count.ordinal)
      if (pointee === undefined || countType === undefined)
        throw new RangeError('LLVM pointer offset lost its pointee layout')
      const countLane = NativeType.lanesFor(types, countType).at(0)
      if (countLane === undefined) throw new RangeError('LLVM pointer offset lost its count lane')
      const stride = alignUp(pointee.size, pointee.alignment)
      const bytes = yield* FunctionBody.binary(
        body,
        'mul',
        NativeStorage.readScalar(storage, operation.count),
        yield* Constant.integerUnsigned(
          builder,
          NativeType.laneType(types, countLane),
          BigInt(stride),
        ),
        `ptr_offset${destination}_bytes`,
      )
      storage.locals.set(
        destination,
        Object.freeze([
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            NativeStorage.readScalar(storage, operation.pointer),
            bytes,
            `ptr_offset${destination}`,
          ),
        ]),
      )
      return
    }
    case 'PointerRead': {
      const base = NativeStorage.readScalar(storage, operation.pointer)
      const semantic = Mir.semanticType(operation.type)
      const values: Array<Value.Input> = []
      for (const [ordinal, lane] of NativeType.lanesFor(types, operation.type).entries()) {
        const offset = LayoutVerify.laneOffset(program.layout, semantic, lane.path)
        if (offset === undefined) throw new RangeError('LLVM pointer read lost a pointee lane')
        values.push(
          yield* FunctionBody.load(
            body,
            NativeType.laneType(types, lane),
            yield* NativeLanePointer.lanePointer(
              lanePointers,
              body,
              base,
              offset,
              `ptr_read${destination}_${ordinal}_ptr`,
            ),
            `ptr_read${destination}_${ordinal}`,
            {
              alignment: yield* Alignment.fromByteUnits(
                accessAlignment(context, operation.pointer, offset),
              ),
            },
          ),
        )
      }
      storage.locals.set(destination, Object.freeze(values))
      return
    }
    case 'PointerWrite': {
      const base = NativeStorage.readScalar(storage, operation.pointer)
      const valueType = storage.fn.localTypes.at(operation.value.ordinal)
      if (valueType === undefined) throw new RangeError('LLVM pointer write lost its value type')
      const semantic = Mir.semanticType(valueType)
      const values = NativeStorage.readLocal(storage, operation.value)
      for (const [ordinal, lane] of NativeType.lanesFor(types, valueType).entries()) {
        const offset = LayoutVerify.laneOffset(program.layout, semantic, lane.path)
        const value = values.at(ordinal)
        if (offset === undefined || value === undefined)
          throw new RangeError('LLVM pointer write lost a pointee lane')
        yield* FunctionBody.store(
          body,
          value,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            base,
            offset,
            `ptr_write${destination}_${ordinal}_ptr`,
          ),
          {
            alignment: yield* Alignment.fromByteUnits(
              accessAlignment(context, operation.pointer, offset),
            ),
          },
        )
      }
      storage.locals.set(destination, Object.freeze([]))
      yield* NativeStorage.reloadAddressRoots(storage)
      return
    }
  }
})
