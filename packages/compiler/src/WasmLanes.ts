import * as Instr from '@silk-effect/wasm/Instr'
import * as ValType from '@silk-effect/wasm/ValType'
import { alignUp } from './internal/Align.js'
import * as LayoutPlan from './Layout.js'
import * as Mir from './Mir.js'
import * as Scalar from './Scalar.js'
export const i32 = ValType.i32
export const i64 = ValType.i64
export const f32 = ValType.f32
export const f64 = ValType.f64

/** The zero instruction for one physical WebAssembly lane. */
export const zeroConst = (type: ValType.ValType): Instr.Instr =>
  type === i64
    ? Instr.i64Const(0n)
    : type === f32
      ? Instr.f32Const(0)
      : type === f64
        ? Instr.f64Const(0)
        : Instr.i32Const(0)

/**
 * The lanes one MIR type occupies, for locals, signatures, and suspension transfer slots alike.
 *
 * A stored executable must resolve through its storage type's calling shape: that is the only
 * producer that prefixes each lane path with the owning `EffectCaptureSelector` or
 * `CallableCaptureSelector`. `effectEnvironmentLanes`/`callableEnvironmentLanes` emit the same
 * lane count and value types but prefix-less paths, so a caller that reads `lane.path` — such as
 * place-read realization — cannot match them against a destination lane. Every lane computation
 * shares this function so the two can never disagree.
 */
export const laneKindsOf = (
  plan: LayoutPlan.Plan,
  type: Mir.Type,
): ReadonlyArray<LayoutPlan.CallingLane> => {
  if (type._tag === 'EffectComposite') {
    const shape = LayoutPlan.callingShape(plan, type.type)
    if (shape !== undefined) return shape.lanes
    const payloadTypes = type.alternatives.flatMap((alternative) =>
      laneKindsOf(plan, alternative).map((lane) => lane.type),
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
  if (type._tag === 'EffectValue' && type.storage !== undefined) {
    const shape = LayoutPlan.callingShape(plan, type.storage.type)
    if (shape === undefined) throw new RangeError('Wasm backend lost a stored Effect calling shape')
    return shape.lanes
  }
  if (type._tag === 'EffectValue') return LayoutPlan.effectEnvironmentLanes(plan, type.environment)
  if (type._tag === 'CallableValue' && type.storage !== undefined) {
    const shape = LayoutPlan.callingShape(plan, type.storage.type)
    if (shape === undefined)
      throw new RangeError('Wasm backend lost a stored callable calling shape')
    return shape.lanes
  }
  if (type._tag === 'CallableValue')
    return type.environment === undefined
      ? Object.freeze([])
      : LayoutPlan.callableEnvironmentLanes(plan, type.environment)
  const shape = LayoutPlan.callingShape(plan, Mir.semanticType(type))
  if (shape === undefined) throw new RangeError('Wasm backend lost a calling shape')
  return shape.lanes
}

/** Whether a MIR value carries an address into an address-taken caller frame root. */
export const carriesBorrowAddress = (plan: LayoutPlan.Plan, type: Mir.Type): boolean => {
  if (type._tag === 'EffectBorrow') return true
  const lanes = laneKindsOf(plan, type)
  if (type._tag === 'EffectValue' || type._tag === 'CallableValue') {
    return lanes.some((lane) => typeof lane.type !== 'string')
  }
  return lanes.some(
    (lane) =>
      typeof lane.type !== 'string' &&
      lane.path.some(
        (selector) =>
          selector._tag === 'ReferenceAddressSelector' || selector._tag === 'SliceAddressSelector',
      ),
  )
}

export const laneValueType = (
  plan: LayoutPlan.Plan,
  lane: LayoutPlan.CallingLane,
): ValType.ValType => {
  if (typeof lane.type !== 'string') return i32
  const scalar = Scalar.find(lane.type)
  if (scalar === undefined) return i32
  if (scalar.category === 'Floating') return scalar.spelling === 'f32' ? f32 : f64
  return Scalar.bits(scalar, plan.target.pointerSize === 4 ? 32 : 64) === 64 ? i64 : i32
}

export const laneLoadMnemonic = (
  plan: LayoutPlan.Plan,
  lane: LayoutPlan.CallingLane,
): Instr.MemoryAccessMnemonic => {
  if (typeof lane.type !== 'string') return 'i32.load'
  const scalar = Scalar.find(lane.type)
  if (scalar?.category === 'Floating') return scalar.spelling === 'f32' ? 'f32.load' : 'f64.load'
  if (scalar?.category !== 'Integer') return 'i32.load'
  const bits = Scalar.bits(scalar, plan.target.pointerSize === 4 ? 32 : 64)
  if (bits === 64) return 'i64.load'
  if (bits === 16) return scalar.signedness === 'Signed' ? 'i32.load16_s' : 'i32.load16_u'
  if (bits === 8) return scalar.signedness === 'Signed' ? 'i32.load8_s' : 'i32.load8_u'
  return 'i32.load'
}

export const laneStoreMnemonic = (
  plan: LayoutPlan.Plan,
  lane: LayoutPlan.CallingLane,
): Instr.MemoryAccessMnemonic => {
  if (typeof lane.type !== 'string') return 'i32.store'
  const scalar = Scalar.find(lane.type)
  if (scalar?.category === 'Floating') return scalar.spelling === 'f32' ? 'f32.store' : 'f64.store'
  if (scalar?.category !== 'Integer') return 'i32.store'
  const bits = Scalar.bits(scalar, plan.target.pointerSize === 4 ? 32 : 64)
  if (bits === 64) return 'i64.store'
  if (bits === 16) return 'i32.store16'
  if (bits === 8) return 'i32.store8'
  return 'i32.store'
}

export interface PackedWasmLane {
  readonly offset: number
  readonly type: ValType.ValType
}

export const packWasmLanes = (
  lanes: ReadonlyArray<LayoutPlan.CallingLane>,
  plan: LayoutPlan.Plan,
  start = 0,
): { readonly lanes: ReadonlyArray<PackedWasmLane>; readonly end: number } => {
  let cursor = start
  const packed = lanes.map((lane) => {
    const type = laneValueType(plan, lane)
    const width = type === i64 || type === f64 ? 8 : 4
    cursor = alignUp(cursor, width)
    const entry = Object.freeze({ offset: cursor, type })
    cursor += width
    return entry
  })
  return Object.freeze({ lanes: Object.freeze(packed), end: cursor })
}
