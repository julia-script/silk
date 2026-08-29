import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as FloatingPoint from './FloatingPoint.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeArith from './NativeArith.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeType from './NativeType.js'
import * as Scalar from './Scalar.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'BindMatch'
      | 'Literal'
      | 'EnumConstant'
      | 'EnumValue'
      | 'EnumEquality'
      | 'StaticView'
      | 'StaticString'
      | 'StringFromUtf8Unchecked'
      | 'StringUtf8Bytes'
      | 'StringByteLength'
      | 'StringEqualsExact'
  }
>

export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const {
    arith,
    body,
    builder,
    i32,
    memcmp,
    staticPointers,
    storage: nativeStorage,
    types,
    usizeType,
  } = context
  const initialTrapBlock = context.state.trapBlock
  const trapBlock = initialTrapBlock
  const checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'BindMatch': {
      const physical = Layout.coverageFieldSlots(operation.shape, operation.member, operation.path)
      if (physical === undefined) {
        throw new RangeError('LLVM match lost a pattern payload path')
      }
      const source = NativeStorage.readLocal(nativeStorage, operation.scrutinee)
      const sourceLanes = operation.shape.lanes
      const targetLanes = NativeType.lanesFor(types, operation.type)
      const selected: Array<Value.Input> = []
      for (const [targetOrdinal, ordinal] of physical.entries()) {
        const value = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        const targetLane = targetLanes.at(targetOrdinal)
        if (value === undefined || sourceLane === undefined || targetLane === undefined) {
          continue
        }
        selected.push(
          yield* NativeArith.coerceLane(
            arith.lane,
            value,
            sourceLane,
            targetLane,
            `match${operation.destination.ordinal}_${targetOrdinal}_lane`,
          ),
        )
      }
      if (selected.length !== targetLanes.length) {
        throw new RangeError(
          `LLVM match binding %${operation.destination.ordinal} disagrees with its payload lanes (${physical.length} selected, ${targetLanes.length} required)`,
        )
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze(selected))
      break
    }
    case 'EnumConstant': {
      const lane = NativeType.lanesFor(types, operation.type).at(0)
      if (lane === undefined) throw new RangeError('LLVM enum constant lost its lane')
      const physicalType = NativeType.laneType(types, lane)
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([
          operation.representation.signedness === 'Signed'
            ? yield* Constant.integerSigned(builder, physicalType, operation.discriminant)
            : yield* Constant.integerUnsigned(builder, physicalType, operation.discriminant),
        ]),
      )
      break
    }
    case 'EnumValue': {
      nativeStorage.locals.set(
        operation.destination.ordinal,
        NativeStorage.readLocal(nativeStorage, operation.source),
      )
      break
    }
    case 'EnumEquality': {
      const left = NativeStorage.readLocal(nativeStorage, operation.left).at(0)
      const right = NativeStorage.readLocal(nativeStorage, operation.right).at(0)
      if (left === undefined || right === undefined)
        throw new RangeError('LLVM enum equality lost an operand lane')
      const compared = yield* FunctionBody.integerCompare(
        body,
        operation.negated ? 'ne' : 'eq',
        left,
        right,
        `enum${operation.destination.ordinal}_${operation.negated ? 'not_equal' : 'equal'}`,
      )
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([
          yield* FunctionBody.cast(
            body,
            'zext',
            compared,
            i32,
            `enum${operation.destination.ordinal}_result`,
          ),
        ]),
      )
      break
    }
    case 'Literal': {
      const lane = NativeType.lanesFor(types, operation.type).at(0)
      if (lane === undefined) throw new RangeError('LLVM literal lost its lane')
      const physicalType = NativeType.laneType(types, lane)
      const semantic = Mir.semanticType(operation.type)
      const floating = typeof semantic === 'string' ? Scalar.find(semantic) : undefined
      if (floating?.category === 'Floating') {
        nativeStorage.locals.set(
          operation.destination.ordinal,
          Object.freeze([
            yield* Constant.floatingRaw(
              builder,
              physicalType,
              floating.spelling === 'f32' ? 'float' : 'double',
              FloatingPoint.littleEndianBytes({
                width: floating.spelling === 'f32' ? 32 : 64,
                bits: BigInt(operation.value),
              }),
            ),
          ]),
        )
        break
      }
      const unsigned =
        typeof semantic === 'string' && Scalar.find(semantic)?.signedness === 'Unsigned'
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([
          unsigned
            ? yield* Constant.integerUnsigned(builder, physicalType, BigInt(operation.value))
            : yield* Constant.integerSigned(builder, physicalType, BigInt(operation.value)),
        ]),
      )
      break
    }
    case 'StaticView': {
      const address = staticPointers.get(operation.data)
      if (address === undefined || usizeType === undefined) {
        throw new RangeError('LLVM static view lost its data placement or usize type')
      }
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([
          address,
          yield* Constant.integerUnsigned(builder, usizeType, operation.length),
        ]),
      )
      break
    }
    case 'StaticString': {
      const address = staticPointers.get(operation.data)
      if (address === undefined || usizeType === undefined) {
        throw new RangeError('LLVM static string lost its data placement or usize type')
      }
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([
          address,
          yield* Constant.integerUnsigned(builder, usizeType, operation.byteLength),
        ]),
      )
      break
    }
    case 'StringFromUtf8Unchecked': {
      nativeStorage.locals.set(
        operation.destination.ordinal,
        NativeStorage.readLocal(nativeStorage, operation.bytes),
      )
      break
    }
    case 'StringUtf8Bytes': {
      nativeStorage.locals.set(
        operation.destination.ordinal,
        NativeStorage.readLocal(nativeStorage, operation.string),
      )
      break
    }
    case 'StringByteLength': {
      const length = NativeStorage.readLocal(nativeStorage, operation.string).at(1)
      if (length === undefined) {
        throw new RangeError('LLVM string lost its byte-length lane')
      }
      nativeStorage.locals.set(operation.destination.ordinal, Object.freeze([length]))
      break
    }
    case 'StringEqualsExact': {
      const [leftAddress, leftLength] = NativeStorage.readLocal(nativeStorage, operation.left)
      const [rightAddress, rightLength] = NativeStorage.readLocal(nativeStorage, operation.right)
      if (
        leftAddress === undefined ||
        leftLength === undefined ||
        rightAddress === undefined ||
        rightLength === undefined ||
        memcmp === undefined ||
        usizeType === undefined
      ) {
        throw new RangeError('LLVM string equality lost its lanes or runtime helper')
      }
      const lengthsEqual = yield* FunctionBody.integerCompare(
        body,
        'eq',
        leftLength,
        rightLength,
        `string${operation.destination.ordinal}_lengths_equal`,
      )
      const zeroLength = yield* Constant.integerUnsigned(builder, usizeType, 0n)
      const comparedLength = yield* FunctionBody.select(
        body,
        lengthsEqual,
        leftLength,
        zeroLength,
        `string${operation.destination.ordinal}_compared_length`,
      )
      const compared = yield* FunctionBody.callDirect(
        body,
        memcmp,
        [leftAddress, rightAddress, comparedLength],
        `string${operation.destination.ordinal}_memcmp`,
      )
      if (compared === undefined) {
        throw new RangeError('LLVM string equality produced no comparison result')
      }
      const zero = yield* Constant.integerSigned(builder, i32, 0n)
      const bytesEqual = yield* FunctionBody.integerCompare(
        body,
        'eq',
        compared,
        zero,
        `string${operation.destination.ordinal}_bytes_equal`,
      )
      const exact = yield* FunctionBody.binary(
        body,
        'and',
        lengthsEqual,
        bytesEqual,
        `string${operation.destination.ordinal}_exact`,
      )
      const selected = operation.negated
        ? yield* FunctionBody.integerCompare(
            body,
            'eq',
            exact,
            yield* Constant.integerUnsigned(builder, yield* LlvmType.integer(builder, 1), 0n),
            `string${operation.destination.ordinal}_negated`,
          )
        : exact
      nativeStorage.locals.set(
        operation.destination.ordinal,
        Object.freeze([
          yield* FunctionBody.cast(
            body,
            'zext',
            selected,
            i32,
            `string${operation.destination.ordinal}_result`,
          ),
        ]),
      )
      break
    }
  }
  if (trapBlock !== initialTrapBlock) context.state.trapBlock = trapBlock
  context.state.checkOrdinal = checkOrdinal
})
