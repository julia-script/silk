import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import type * as Layout from './Layout.js'
import type * as Mir from './Mir.js'
import * as Scalar from './Scalar.js'

export interface LaneContext {
  readonly body: FunctionBody.FunctionBody
  readonly pointerBits: 32 | 64
  readonly i32: LlvmType.Type
  readonly integerTypes: ReadonlyMap<number, LlvmType.Type>
  readonly laneType: (lane: Layout.CallingLane) => LlvmType.Type
}

/** Reinterprets and resizes one physical ABI lane without changing its semantic bits. */
export const coerceLane = Effect.fnUntraced(function* (
  context: LaneContext,
  input: Value.Input,
  source: Layout.CallingLane,
  target: Layout.CallingLane,
  name: string,
): Effect.fn.Return<Value.Input, LlvmError.LlvmError> {
  const sourceIsAddress = typeof source.type !== 'string'
  const targetIsAddress = typeof target.type !== 'string'
  if (sourceIsAddress && targetIsAddress) return input
  const scalarBits = (lane: Layout.CallingLane): number =>
    typeof lane.type !== 'string'
      ? context.pointerBits
      : (() => {
          const scalar = Scalar.find(lane.type) ?? Scalar.defaultInteger
          return scalar.category === 'Boolean' ? 32 : Scalar.bits(scalar, context.pointerBits)
        })()
  const sourceBits = scalarBits(source)
  const targetBits = scalarBits(target)
  const sourceScalar = typeof source.type === 'string' ? Scalar.find(source.type) : undefined
  const targetScalar = typeof target.type === 'string' ? Scalar.find(target.type) : undefined
  const sourceFloating = sourceScalar?.category === 'Floating'
  const targetFloating = targetScalar?.category === 'Floating'
  const sourceIntegerType = context.integerTypes.get(sourceBits) ?? context.i32
  const targetIntegerType = context.integerTypes.get(targetBits) ?? context.i32
  if (
    !sourceIsAddress &&
    !targetIsAddress &&
    sourceBits === targetBits &&
    sourceFloating === targetFloating
  )
    return input
  let bits = sourceIsAddress
    ? yield* FunctionBody.cast(context.body, 'ptrtoint', input, sourceIntegerType, `${name}_bits`)
    : sourceFloating
      ? yield* FunctionBody.cast(context.body, 'bitcast', input, sourceIntegerType, `${name}_bits`)
      : input
  if (sourceBits !== targetBits)
    bits = yield* FunctionBody.cast(
      context.body,
      targetBits > sourceBits ? 'zext' : 'trunc',
      bits,
      targetIntegerType,
      `${name}_width`,
    )
  if (targetIsAddress)
    return yield* FunctionBody.cast(context.body, 'inttoptr', bits, context.laneType(target), name)
  return targetFloating
    ? yield* FunctionBody.cast(context.body, 'bitcast', bits, context.laneType(target), name)
    : bits
})

export type IntegerPredicate =
  | 'eq'
  | 'ne'
  | 'slt'
  | 'sle'
  | 'sgt'
  | 'sge'
  | 'ult'
  | 'ule'
  | 'ugt'
  | 'uge'

/** Selects the one LLVM integer predicate for a Silk comparison. */
export const comparisonPredicate = (
  operation: Mir.BinaryOperator,
  unsigned: boolean,
): IntegerPredicate | undefined => {
  switch (operation) {
    case 'Equals':
      return 'eq'
    case 'NotEquals':
      return 'ne'
    case 'LessThan':
      return unsigned ? 'ult' : 'slt'
    case 'LessOrEqual':
      return unsigned ? 'ule' : 'sle'
    case 'GreaterThan':
      return unsigned ? 'ugt' : 'sgt'
    case 'GreaterOrEqual':
      return unsigned ? 'uge' : 'sge'
    default:
      return undefined
  }
}
