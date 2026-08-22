import type * as LlvmType from '@silk-effect/llvm/Type'
import { alignUp } from './internal/Align.js'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as Scalar from './Scalar.js'
import type * as Target from './Target.js'
import * as SilkType from './Type.js'

/** LLVM types and target plan needed to lower one MIR type without closure capture. */
export interface LoweringContext {
  readonly program: Mir.Module
  readonly i32: LlvmType.Type
  readonly f32: LlvmType.Type
  readonly f64: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly integerTypes: ReadonlyMap<number, LlvmType.Type>
}

/** Resolves the physical ABI lanes of one MIR value. */
export const lanesFor = (
  context: LoweringContext,
  type: Mir.Type,
): ReadonlyArray<Layout.CallingLane> => {
  if (type._tag === 'EffectComposite') {
    const payloadTypes = type.alternatives.flatMap((alternative) =>
      lanesFor(context, alternative).map((lane) => lane.type),
    )
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([Object.freeze({ _tag: 'UnionTagSelector' as const })]),
        type: 'i32' as const,
      }),
      ...payloadTypes.map((laneType, slot) =>
        Object.freeze({
          _tag: 'CallingLane' as const,
          path: Object.freeze([Object.freeze({ _tag: 'UnionPayloadSelector' as const, slot })]),
          type: laneType,
        }),
      ),
    ])
  }
  if (type._tag === 'EffectBorrow')
    return Object.freeze([
      Object.freeze({
        _tag: 'CallingLane' as const,
        path: Object.freeze([]),
        type: Object.freeze({
          _tag: 'Address' as const,
          element: type.type,
          bits: context.program.layout.target.pointerSize === 4 ? 32 : 64,
        }),
      }),
    ])
  if (type._tag === 'EffectValue' && type.storage !== undefined) {
    const shape = Layout.callingShape(context.program.layout, type.storage.type)
    if (shape === undefined) throw new RangeError('LLVM backend lost a stored Effect calling shape')
    return shape.lanes
  }
  if (type._tag === 'EffectValue')
    return Layout.effectEnvironmentLanes(context.program.layout, type.environment)
  if (type._tag === 'CallableValue' && type.storage !== undefined) {
    const shape = Layout.callingShape(context.program.layout, type.storage.type)
    if (shape === undefined)
      throw new RangeError('LLVM backend lost a stored callable calling shape')
    return shape.lanes
  }
  if (type._tag === 'CallableValue')
    return type.environment === undefined
      ? Object.freeze([])
      : Layout.callableEnvironmentLanes(context.program.layout, type.environment)
  const shape = Layout.callingShape(context.program.layout, Mir.semanticType(type))
  if (shape === undefined)
    throw new RangeError(`LLVM backend lost calling shape for ${Mir.semanticType(type)}`)
  return shape.lanes
}

/** Resolves value lanes when an EffectBorrow is loaded rather than passed by address. */
export const valueLanesFor = (
  context: LoweringContext,
  type: Mir.Type,
): ReadonlyArray<Layout.CallingLane> => {
  if (type._tag !== 'EffectBorrow') return lanesFor(context, type)
  const shape = Layout.callingShape(context.program.layout, type.type)
  if (shape === undefined)
    throw new RangeError(
      `LLVM backend lost borrowed calling shape for ${SilkType.encode(type.type)}`,
    )
  return shape.lanes
}

/** Resolves the LLVM storage type of one planned calling lane. */
export const laneType = (context: LoweringContext, lane: Layout.CallingLane): LlvmType.Type => {
  if (typeof lane.type !== 'string') return context.pointer
  const scalar = Scalar.find(lane.type)
  if (scalar === undefined) return context.i32
  if (scalar.category === 'Floating') return scalar.spelling === 'f32' ? context.f32 : context.f64
  const bits = Scalar.bits(scalar, context.program.layout.target.pointerSize === 4 ? 32 : 64)
  return context.integerTypes.get(bits) ?? context.i32
}

export const laneStorage = (
  target: Target.Target,
  lane: Layout.CallingLane,
): { readonly size: number; readonly alignment: number } => {
  if (typeof lane.type !== 'string')
    return Object.freeze({ size: target.pointerSize, alignment: target.pointerAlignment })
  const scalar = Scalar.find(lane.type)
  const bits = Scalar.bits(scalar ?? Scalar.defaultInteger, target.pointerSize === 4 ? 32 : 64)
  const size = bits / 8
  return Object.freeze({ size, alignment: Math.min(size, 8) })
}

/** Packs native calling lanes using their target ABI storage. */
export const packLanes = (
  target: Target.Target,
  lanes: ReadonlyArray<Layout.CallingLane>,
  start = 0,
): {
  readonly entries: ReadonlyArray<{ readonly lane: Layout.CallingLane; readonly offset: number }>
  readonly end: number
  readonly alignment: number
} => {
  let cursor = start
  let alignment = 1
  const entries = lanes.map((lane) => {
    const storage = laneStorage(target, lane)
    cursor = alignUp(cursor, storage.alignment)
    const entry = Object.freeze({ lane, offset: cursor })
    cursor += storage.size
    alignment = Math.max(alignment, storage.alignment)
    return entry
  })
  return Object.freeze({ entries: Object.freeze(entries), end: cursor, alignment })
}
