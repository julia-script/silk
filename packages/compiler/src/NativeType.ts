import type * as LlvmType from '@silklang/llvm/Type'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as Mir from './Mir.js'
import * as Scalar from './Scalar.js'
import * as SilkType from './Type.js'
import * as ValueStorage from './ValueStorage.js'

/** LLVM types and target plan needed to lower one MIR type without closure capture. */
export interface LoweringContext {
  readonly program: Mir.Module
  readonly i32: LlvmType.Type
  readonly f32: LlvmType.Type
  readonly f64: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly integerTypes: ReadonlyMap<number, LlvmType.Type>
}

/** Addressable storage follows concrete executable environments, not erased public types. */
export const addressLayout = (
  layout: Layout.Plan,
  type: Mir.Type,
): { readonly size: number; readonly alignment: number } | undefined => {
  if (type._tag === 'EffectOutcome') return ValueStorage.find(layout, 'Outcome', type.type)
  if (type._tag === 'EffectComposite')
    return ValueStorage.find(layout, 'CompositeCarrier', type.type)
  if (type._tag === 'EffectValue')
    return type.storage === undefined ? type.environment : Layout.entry(layout, type.storage.type)
  if (type._tag === 'CallableValue')
    return type.storage === undefined
      ? (type.environment ?? { size: 0, alignment: 1 })
      : Layout.entry(layout, type.storage.type)
  return Layout.entry(layout, Mir.semanticType(type))
}

/** Resolves an addressable lane through nested concrete captures and ordinary field paths. */
export const addressLaneOffset = (
  layout: Layout.Plan,
  type: Mir.Type,
  lane: Layout.CallingLane,
  ordinal: number,
): number | undefined => {
  if (type._tag === 'EffectOutcome')
    return ValueStorage.find(layout, 'Outcome', type.type)?.slots.at(ordinal)?.offset
  if (type._tag === 'EffectComposite')
    return ValueStorage.find(layout, 'CompositeCarrier', type.type)?.slots.at(ordinal)?.offset
  let placements: ReadonlyArray<Layout.EnvironmentLanePlacement> | undefined
  if (type._tag === 'EffectValue' && type.storage === undefined)
    placements = Layout.effectEnvironmentLanePlacements(layout, type.environment)
  else if (
    type._tag === 'CallableValue' &&
    type.storage === undefined &&
    type.environment !== undefined
  )
    placements = Layout.callableEnvironmentLanePlacements(layout, type.environment)
  if (placements !== undefined) {
    const placement = placements.at(ordinal)
    if (placement === undefined) return undefined
    const offset =
      placement.root === undefined
        ? 0
        : LayoutVerify.laneOffset(layout, placement.root, placement.lane.path)
    return offset === undefined ? undefined : placement.byteOffset + offset
  }
  return LayoutVerify.laneOffset(
    layout,
    (type._tag === 'EffectValue' || type._tag === 'CallableValue') && type.storage !== undefined
      ? type.storage.type
      : Mir.semanticType(type),
    lane.path,
  )
}

/** Selects a stored slot or a planner-owned active-alternative conversion. */
export const addressLocation = (
  layout: Layout.Plan,
  type: Mir.Type,
  lane: Layout.CallingLane,
  ordinal: number,
): ValueStorage.Location | undefined => {
  if (type._tag === 'EffectOutcome' || type._tag === 'EffectComposite') {
    const offset = addressLaneOffset(layout, type, lane, ordinal)
    return offset === undefined ? undefined : { _tag: 'Slot', lane, offset }
  }
  let placements: ReadonlyArray<Layout.EnvironmentLanePlacement> | undefined
  if (type._tag === 'EffectValue' && type.storage === undefined)
    placements = Layout.effectEnvironmentLanePlacements(layout, type.environment)
  else if (
    type._tag === 'CallableValue' &&
    type.storage === undefined &&
    type.environment !== undefined
  )
    placements = Layout.callableEnvironmentLanePlacements(layout, type.environment)
  if (placements !== undefined) {
    const placement = placements.at(ordinal)
    if (placement === undefined) return undefined
    return placement.root === undefined
      ? { _tag: 'Slot', lane: placement.lane, offset: placement.byteOffset }
      : ValueStorage.location(layout, placement.root, placement.lane, placement.byteOffset)
  }
  return ValueStorage.location(
    layout,
    (type._tag === 'EffectValue' || type._tag === 'CallableValue') && type.storage !== undefined
      ? type.storage.type
      : Mir.semanticType(type),
    lane,
  )
}

/** Resolves the physical ABI lanes of one MIR value. */
export const lanesFor = (
  context: LoweringContext,
  type: Mir.Type,
): ReadonlyArray<Layout.CallingLane> => {
  if (type._tag === 'EffectComposite') {
    const registered = Layout.callingShape(context.program.layout, type.type)
    if (registered?.tree._tag !== 'EffectCompositeShape')
      throw new RangeError('LLVM backend lost a planned composite calling shape')
    return registered.lanes
  }
  if (type._tag === 'EnvironmentBorrow')
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
    throw new RangeError(`LLVM backend lost calling shape for ${Mir.typeText(type)}`)
  return shape.lanes
}

/** Resolves value lanes when an EnvironmentBorrow is loaded rather than passed by address. */
export const valueLanesFor = (
  context: LoweringContext,
  type: Mir.Type,
): ReadonlyArray<Layout.CallingLane> => {
  if (type._tag !== 'EnvironmentBorrow') return lanesFor(context, type)
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
  if (scalar === undefined) throw new RangeError('LLVM calling lane has no semantic scalar')
  const physical = Scalar.resolveLayout(scalar, context.program.layout.target)
  if (scalar.category === 'Floating') return physical.size === 4 ? context.f32 : context.f64
  const type = context.integerTypes.get(physical.size * 8)
  if (type === undefined) throw new RangeError('LLVM calling lane lost its planned integer type')
  return type
}
