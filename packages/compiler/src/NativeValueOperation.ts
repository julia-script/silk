import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as FloatingPoint from './FloatingPoint.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeFunction from './NativeFunction.js'
import type { LoweringContext } from './NativeOperation.js'
import * as Scalar from './Scalar.js'

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'BindMatch'
      | 'Literal'
      | 'StaticView'
      | 'StaticString'
      | 'StringFromUtf8Unchecked'
      | 'StringUtf8Bytes'
      | 'StringByteLength'
      | 'StringEqualsExact'
  }
>

export const emit = Effect.fnUntraced(function* (context: LoweringContext, operation: Operation) {
  const {
    body,
    builder,
    coerceLane,
    i32,
    laneType,
    lanesFor,
    locals,
    memcmp,
    staticPointers,
    usizeType,
  } = context
  const initialTrapBlock = context.state.trapBlock
  const trapBlock = initialTrapBlock
  const checkOrdinal = context.state.checkOrdinal
  switch (operation._tag) {
    case 'BindMatch': {
      const physical = Layout.memberFieldSlots(
        operation.shape,
        operation.member,
        operation.binding.path,
      )
      if (physical === undefined) {
        throw new RangeError('LLVM match lost a pattern payload path')
      }
      const source = NativeFunction.readLocal(locals, operation.scrutinee)
      const sourceLanes = operation.shape.lanes
      const targetLanes = lanesFor(operation.binding.type)
      const selected: Array<Value.Input> = []
      for (const [targetOrdinal, ordinal] of physical.entries()) {
        const value = source.at(ordinal)
        const sourceLane = sourceLanes.at(ordinal)
        const targetLane = targetLanes.at(targetOrdinal)
        if (value === undefined || sourceLane === undefined || targetLane === undefined) {
          continue
        }
        selected.push(
          yield* coerceLane(
            value,
            sourceLane,
            targetLane,
            `match${operation.binding.destination.ordinal}_${targetOrdinal}_lane`,
          ),
        )
      }
      if (selected.length !== targetLanes.length) {
        throw new RangeError('LLVM match binding disagrees with its payload lanes')
      }
      locals.set(operation.binding.destination.ordinal, Object.freeze(selected))
      break
    }
    case 'Literal': {
      const lane = lanesFor(operation.type).at(0)
      if (lane === undefined) throw new RangeError('LLVM literal lost its lane')
      const physicalType = laneType(lane)
      const semantic = Mir.semanticType(operation.type)
      const floating = typeof semantic === 'string' ? Scalar.find(semantic) : undefined
      if (floating?.category === 'Floating') {
        locals.set(
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
      locals.set(
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
      locals.set(
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
      locals.set(
        operation.destination.ordinal,
        Object.freeze([
          address,
          yield* Constant.integerUnsigned(builder, usizeType, operation.byteLength),
        ]),
      )
      break
    }
    case 'StringFromUtf8Unchecked': {
      locals.set(operation.destination.ordinal, NativeFunction.readLocal(locals, operation.bytes))
      break
    }
    case 'StringUtf8Bytes': {
      locals.set(operation.destination.ordinal, NativeFunction.readLocal(locals, operation.string))
      break
    }
    case 'StringByteLength': {
      const length = NativeFunction.readLocal(locals, operation.string).at(1)
      if (length === undefined) {
        throw new RangeError('LLVM string lost its byte-length lane')
      }
      locals.set(operation.destination.ordinal, Object.freeze([length]))
      break
    }
    case 'StringEqualsExact': {
      const [leftAddress, leftLength] = NativeFunction.readLocal(locals, operation.left)
      const [rightAddress, rightLength] = NativeFunction.readLocal(locals, operation.right)
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
      locals.set(
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
