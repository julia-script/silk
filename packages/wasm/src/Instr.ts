/**
 * WebAssembly instructions as plain immutable data.
 *
 * Instructions are context-free values constructed without a builder or Effect: a function body
 * is an ordinary `ReadonlyArray<Instr>` that composes with array logic. Entity references inside
 * instructions are opaque handles; ownership and typing are validated when the body is committed
 * with `Func.define`, and handles resolve to numeric indices only at emission.
 *
 * @since 0.0.0
 */
import type * as Data from './Data.js'
import type * as Elem from './Elem.js'
import type * as Func from './Func.js'
import type * as Global from './Global.js'
import * as InstructionTable from './internal/InstructionTable.js'
import type * as Memory from './Memory.js'
import type * as Table from './Table.js'
import type * as Tag from './Tag.js'
import type * as Type from './Type.js'
import * as ValType from './ValType.js'

/**
 * Mnemonic of an instruction that carries no immediate operand.
 *
 * @category instructions
 * @since 0.0.0
 */
export type PlainMnemonic =
  | 'unreachable'
  | 'nop'
  | 'return'
  | 'drop'
  | 'select'
  | 'ref.is_null'
  | 'throw_ref'
  | 'ref.eq'
  | 'ref.as_non_null'
  | 'any.convert_extern'
  | 'extern.convert_any'
  | 'ref.i31'
  | 'i31.get_s'
  | 'i31.get_u'
  | 'array.len'
  | 'i32.eqz'
  | 'i32.eq'
  | 'i32.ne'
  | 'i32.lt_s'
  | 'i32.lt_u'
  | 'i32.gt_s'
  | 'i32.gt_u'
  | 'i32.le_s'
  | 'i32.le_u'
  | 'i32.ge_s'
  | 'i32.ge_u'
  | 'i64.eqz'
  | 'i64.eq'
  | 'i64.ne'
  | 'i64.lt_s'
  | 'i64.lt_u'
  | 'i64.gt_s'
  | 'i64.gt_u'
  | 'i64.le_s'
  | 'i64.le_u'
  | 'i64.ge_s'
  | 'i64.ge_u'
  | 'f32.eq'
  | 'f32.ne'
  | 'f32.lt'
  | 'f32.gt'
  | 'f32.le'
  | 'f32.ge'
  | 'f64.eq'
  | 'f64.ne'
  | 'f64.lt'
  | 'f64.gt'
  | 'f64.le'
  | 'f64.ge'
  | 'i32.clz'
  | 'i32.ctz'
  | 'i32.popcnt'
  | 'i32.add'
  | 'i32.sub'
  | 'i32.mul'
  | 'i32.div_s'
  | 'i32.div_u'
  | 'i32.rem_s'
  | 'i32.rem_u'
  | 'i32.and'
  | 'i32.or'
  | 'i32.xor'
  | 'i32.shl'
  | 'i32.shr_s'
  | 'i32.shr_u'
  | 'i32.rotl'
  | 'i32.rotr'
  | 'i64.clz'
  | 'i64.ctz'
  | 'i64.popcnt'
  | 'i64.add'
  | 'i64.sub'
  | 'i64.mul'
  | 'i64.div_s'
  | 'i64.div_u'
  | 'i64.rem_s'
  | 'i64.rem_u'
  | 'i64.and'
  | 'i64.or'
  | 'i64.xor'
  | 'i64.shl'
  | 'i64.shr_s'
  | 'i64.shr_u'
  | 'i64.rotl'
  | 'i64.rotr'
  | 'f32.abs'
  | 'f32.neg'
  | 'f32.ceil'
  | 'f32.floor'
  | 'f32.trunc'
  | 'f32.nearest'
  | 'f32.sqrt'
  | 'f32.add'
  | 'f32.sub'
  | 'f32.mul'
  | 'f32.div'
  | 'f32.min'
  | 'f32.max'
  | 'f32.copysign'
  | 'f64.abs'
  | 'f64.neg'
  | 'f64.ceil'
  | 'f64.floor'
  | 'f64.trunc'
  | 'f64.nearest'
  | 'f64.sqrt'
  | 'f64.add'
  | 'f64.sub'
  | 'f64.mul'
  | 'f64.div'
  | 'f64.min'
  | 'f64.max'
  | 'f64.copysign'
  | 'i32.wrap_i64'
  | 'i32.trunc_f32_s'
  | 'i32.trunc_f32_u'
  | 'i32.trunc_f64_s'
  | 'i32.trunc_f64_u'
  | 'i64.extend_i32_s'
  | 'i64.extend_i32_u'
  | 'i64.trunc_f32_s'
  | 'i64.trunc_f32_u'
  | 'i64.trunc_f64_s'
  | 'i64.trunc_f64_u'
  | 'f32.convert_i32_s'
  | 'f32.convert_i32_u'
  | 'f32.convert_i64_s'
  | 'f32.convert_i64_u'
  | 'f32.demote_f64'
  | 'f64.convert_i32_s'
  | 'f64.convert_i32_u'
  | 'f64.convert_i64_s'
  | 'f64.convert_i64_u'
  | 'f64.promote_f32'
  | 'i32.reinterpret_f32'
  | 'i64.reinterpret_f64'
  | 'f32.reinterpret_i32'
  | 'f64.reinterpret_i64'
  | 'i32.extend8_s'
  | 'i32.extend16_s'
  | 'i64.extend8_s'
  | 'i64.extend16_s'
  | 'i64.extend32_s'
  | 'i32.trunc_sat_f32_s'
  | 'i32.trunc_sat_f32_u'
  | 'i32.trunc_sat_f64_s'
  | 'i32.trunc_sat_f64_u'
  | 'i64.trunc_sat_f32_s'
  | 'i64.trunc_sat_f32_u'
  | 'i64.trunc_sat_f64_s'
  | 'i64.trunc_sat_f64_u'
  | 'atomic.fence'
  | 'i8x16.swizzle'
  | 'i8x16.splat'
  | 'i16x8.splat'
  | 'i32x4.splat'
  | 'i64x2.splat'
  | 'f32x4.splat'
  | 'f64x2.splat'
  | 'i8x16.eq'
  | 'i8x16.ne'
  | 'i8x16.lt_s'
  | 'i8x16.lt_u'
  | 'i8x16.gt_s'
  | 'i8x16.gt_u'
  | 'i8x16.le_s'
  | 'i8x16.le_u'
  | 'i8x16.ge_s'
  | 'i8x16.ge_u'
  | 'i16x8.eq'
  | 'i16x8.ne'
  | 'i16x8.lt_s'
  | 'i16x8.lt_u'
  | 'i16x8.gt_s'
  | 'i16x8.gt_u'
  | 'i16x8.le_s'
  | 'i16x8.le_u'
  | 'i16x8.ge_s'
  | 'i16x8.ge_u'
  | 'i32x4.eq'
  | 'i32x4.ne'
  | 'i32x4.lt_s'
  | 'i32x4.lt_u'
  | 'i32x4.gt_s'
  | 'i32x4.gt_u'
  | 'i32x4.le_s'
  | 'i32x4.le_u'
  | 'i32x4.ge_s'
  | 'i32x4.ge_u'
  | 'f32x4.eq'
  | 'f32x4.ne'
  | 'f32x4.lt'
  | 'f32x4.gt'
  | 'f32x4.le'
  | 'f32x4.ge'
  | 'f64x2.eq'
  | 'f64x2.ne'
  | 'f64x2.lt'
  | 'f64x2.gt'
  | 'f64x2.le'
  | 'f64x2.ge'
  | 'v128.not'
  | 'v128.and'
  | 'v128.andnot'
  | 'v128.or'
  | 'v128.xor'
  | 'v128.bitselect'
  | 'v128.any_true'
  | 'f32x4.demote_f64x2_zero'
  | 'f64x2.promote_low_f32x4'
  | 'i8x16.abs'
  | 'i8x16.neg'
  | 'i8x16.popcnt'
  | 'i8x16.all_true'
  | 'i8x16.bitmask'
  | 'i8x16.narrow_i16x8_s'
  | 'i8x16.narrow_i16x8_u'
  | 'f32x4.ceil'
  | 'f32x4.floor'
  | 'f32x4.trunc'
  | 'f32x4.nearest'
  | 'i8x16.shl'
  | 'i8x16.shr_s'
  | 'i8x16.shr_u'
  | 'i8x16.add'
  | 'i8x16.add_sat_s'
  | 'i8x16.add_sat_u'
  | 'i8x16.sub'
  | 'i8x16.sub_sat_s'
  | 'i8x16.sub_sat_u'
  | 'f64x2.ceil'
  | 'f64x2.floor'
  | 'i8x16.min_s'
  | 'i8x16.min_u'
  | 'i8x16.max_s'
  | 'i8x16.max_u'
  | 'f64x2.trunc'
  | 'i8x16.avgr_u'
  | 'i16x8.extadd_pairwise_i8x16_s'
  | 'i16x8.extadd_pairwise_i8x16_u'
  | 'i32x4.extadd_pairwise_i16x8_s'
  | 'i32x4.extadd_pairwise_i16x8_u'
  | 'i16x8.abs'
  | 'i16x8.neg'
  | 'i16x8.q15mulr_sat_s'
  | 'i16x8.all_true'
  | 'i16x8.bitmask'
  | 'i16x8.narrow_i32x4_s'
  | 'i16x8.narrow_i32x4_u'
  | 'i16x8.extend_low_i8x16_s'
  | 'i16x8.extend_high_i8x16_s'
  | 'i16x8.extend_low_i8x16_u'
  | 'i16x8.extend_high_i8x16_u'
  | 'i16x8.shl'
  | 'i16x8.shr_s'
  | 'i16x8.shr_u'
  | 'i16x8.add'
  | 'i16x8.add_sat_s'
  | 'i16x8.add_sat_u'
  | 'i16x8.sub'
  | 'i16x8.sub_sat_s'
  | 'i16x8.sub_sat_u'
  | 'f64x2.nearest'
  | 'i16x8.mul'
  | 'i16x8.min_s'
  | 'i16x8.min_u'
  | 'i16x8.max_s'
  | 'i16x8.max_u'
  | 'i16x8.avgr_u'
  | 'i16x8.extmul_low_i8x16_s'
  | 'i16x8.extmul_high_i8x16_s'
  | 'i16x8.extmul_low_i8x16_u'
  | 'i16x8.extmul_high_i8x16_u'
  | 'i32x4.abs'
  | 'i32x4.neg'
  | 'i32x4.all_true'
  | 'i32x4.bitmask'
  | 'i32x4.extend_low_i16x8_s'
  | 'i32x4.extend_high_i16x8_s'
  | 'i32x4.extend_low_i16x8_u'
  | 'i32x4.extend_high_i16x8_u'
  | 'i32x4.shl'
  | 'i32x4.shr_s'
  | 'i32x4.shr_u'
  | 'i32x4.add'
  | 'i32x4.sub'
  | 'i32x4.mul'
  | 'i32x4.min_s'
  | 'i32x4.min_u'
  | 'i32x4.max_s'
  | 'i32x4.max_u'
  | 'i32x4.dot_i16x8_s'
  | 'i32x4.extmul_low_i16x8_s'
  | 'i32x4.extmul_high_i16x8_s'
  | 'i32x4.extmul_low_i16x8_u'
  | 'i32x4.extmul_high_i16x8_u'
  | 'i64x2.abs'
  | 'i64x2.neg'
  | 'i64x2.all_true'
  | 'i64x2.bitmask'
  | 'i64x2.extend_low_i32x4_s'
  | 'i64x2.extend_high_i32x4_s'
  | 'i64x2.extend_low_i32x4_u'
  | 'i64x2.extend_high_i32x4_u'
  | 'i64x2.shl'
  | 'i64x2.shr_s'
  | 'i64x2.shr_u'
  | 'i64x2.add'
  | 'i64x2.sub'
  | 'i64x2.mul'
  | 'i64x2.eq'
  | 'i64x2.ne'
  | 'i64x2.lt_s'
  | 'i64x2.gt_s'
  | 'i64x2.le_s'
  | 'i64x2.ge_s'
  | 'i64x2.extmul_low_i32x4_s'
  | 'i64x2.extmul_high_i32x4_s'
  | 'i64x2.extmul_low_i32x4_u'
  | 'i64x2.extmul_high_i32x4_u'
  | 'f32x4.abs'
  | 'f32x4.neg'
  | 'f32x4.sqrt'
  | 'f32x4.add'
  | 'f32x4.sub'
  | 'f32x4.mul'
  | 'f32x4.div'
  | 'f32x4.min'
  | 'f32x4.max'
  | 'f32x4.pmin'
  | 'f32x4.pmax'
  | 'f64x2.abs'
  | 'f64x2.neg'
  | 'f64x2.sqrt'
  | 'f64x2.add'
  | 'f64x2.sub'
  | 'f64x2.mul'
  | 'f64x2.div'
  | 'f64x2.min'
  | 'f64x2.max'
  | 'f64x2.pmin'
  | 'f64x2.pmax'
  | 'i32x4.trunc_sat_f32x4_s'
  | 'i32x4.trunc_sat_f32x4_u'
  | 'f32x4.convert_i32x4_s'
  | 'f32x4.convert_i32x4_u'
  | 'i32x4.trunc_sat_f64x2_s_zero'
  | 'i32x4.trunc_sat_f64x2_u_zero'
  | 'f64x2.convert_low_i32x4_s'
  | 'f64x2.convert_low_i32x4_u'
  | 'i8x16.relaxed_swizzle'
  | 'i32x4.relaxed_trunc_f32x4_s'
  | 'i32x4.relaxed_trunc_f32x4_u'
  | 'i32x4.relaxed_trunc_f64x2_s_zero'
  | 'i32x4.relaxed_trunc_f64x2_u_zero'
  | 'f32x4.relaxed_madd'
  | 'f32x4.relaxed_nmadd'
  | 'f64x2.relaxed_madd'
  | 'f64x2.relaxed_nmadd'
  | 'i8x16.relaxed_laneselect'
  | 'i16x8.relaxed_laneselect'
  | 'i32x4.relaxed_laneselect'
  | 'i64x2.relaxed_laneselect'
  | 'f32x4.relaxed_min'
  | 'f32x4.relaxed_max'
  | 'f64x2.relaxed_min'
  | 'f64x2.relaxed_max'
  | 'i16x8.relaxed_q15mulr_s'
  | 'i16x8.relaxed_dot_i8x16_i7x16_s'
  | 'i32x4.relaxed_dot_i8x16_i7x16_add_s'

/**
 * Mnemonic of a memory load or store instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export type MemoryAccessMnemonic =
  | 'i32.load'
  | 'i64.load'
  | 'f32.load'
  | 'f64.load'
  | 'i32.load8_s'
  | 'i32.load8_u'
  | 'i32.load16_s'
  | 'i32.load16_u'
  | 'i64.load8_s'
  | 'i64.load8_u'
  | 'i64.load16_s'
  | 'i64.load16_u'
  | 'i64.load32_s'
  | 'i64.load32_u'
  | 'i32.store'
  | 'i64.store'
  | 'f32.store'
  | 'f64.store'
  | 'i32.store8'
  | 'i32.store16'
  | 'i64.store8'
  | 'i64.store16'
  | 'i64.store32'

/**
 * Mnemonic of a SIMD memory load or store instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export type SimdMemoryAccessMnemonic =
  | 'v128.load'
  | 'v128.load8x8_s'
  | 'v128.load8x8_u'
  | 'v128.load16x4_s'
  | 'v128.load16x4_u'
  | 'v128.load32x2_s'
  | 'v128.load32x2_u'
  | 'v128.load8_splat'
  | 'v128.load16_splat'
  | 'v128.load32_splat'
  | 'v128.load64_splat'
  | 'v128.load32_zero'
  | 'v128.load64_zero'
  | 'v128.store'

/**
 * Mnemonic of a SIMD lane-addressed memory load or store instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export type SimdLaneMemoryMnemonic =
  | 'v128.load8_lane'
  | 'v128.load16_lane'
  | 'v128.load32_lane'
  | 'v128.load64_lane'
  | 'v128.store8_lane'
  | 'v128.store16_lane'
  | 'v128.store32_lane'
  | 'v128.store64_lane'

/**
 * Mnemonic of a SIMD lane extract or replace instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export type SimdLaneMnemonic =
  | 'i8x16.extract_lane_s'
  | 'i8x16.extract_lane_u'
  | 'i8x16.replace_lane'
  | 'i16x8.extract_lane_s'
  | 'i16x8.extract_lane_u'
  | 'i16x8.replace_lane'
  | 'i32x4.extract_lane'
  | 'i32x4.replace_lane'
  | 'i64x2.extract_lane'
  | 'i64x2.replace_lane'
  | 'f32x4.extract_lane'
  | 'f32x4.replace_lane'
  | 'f64x2.extract_lane'
  | 'f64x2.replace_lane'

/**
 * Mnemonic of an atomic memory instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export type AtomicAccessMnemonic =
  | 'memory.atomic.notify'
  | 'memory.atomic.wait32'
  | 'memory.atomic.wait64'
  | 'i32.atomic.load'
  | 'i64.atomic.load'
  | 'i32.atomic.load8_u'
  | 'i32.atomic.load16_u'
  | 'i64.atomic.load8_u'
  | 'i64.atomic.load16_u'
  | 'i64.atomic.load32_u'
  | 'i32.atomic.store'
  | 'i64.atomic.store'
  | 'i32.atomic.store8'
  | 'i32.atomic.store16'
  | 'i64.atomic.store8'
  | 'i64.atomic.store16'
  | 'i64.atomic.store32'
  | 'i32.atomic.rmw.add'
  | 'i64.atomic.rmw.add'
  | 'i32.atomic.rmw8.add_u'
  | 'i32.atomic.rmw16.add_u'
  | 'i64.atomic.rmw8.add_u'
  | 'i64.atomic.rmw16.add_u'
  | 'i64.atomic.rmw32.add_u'
  | 'i32.atomic.rmw.sub'
  | 'i64.atomic.rmw.sub'
  | 'i32.atomic.rmw8.sub_u'
  | 'i32.atomic.rmw16.sub_u'
  | 'i64.atomic.rmw8.sub_u'
  | 'i64.atomic.rmw16.sub_u'
  | 'i64.atomic.rmw32.sub_u'
  | 'i32.atomic.rmw.and'
  | 'i64.atomic.rmw.and'
  | 'i32.atomic.rmw8.and_u'
  | 'i32.atomic.rmw16.and_u'
  | 'i64.atomic.rmw8.and_u'
  | 'i64.atomic.rmw16.and_u'
  | 'i64.atomic.rmw32.and_u'
  | 'i32.atomic.rmw.or'
  | 'i64.atomic.rmw.or'
  | 'i32.atomic.rmw8.or_u'
  | 'i32.atomic.rmw16.or_u'
  | 'i64.atomic.rmw8.or_u'
  | 'i64.atomic.rmw16.or_u'
  | 'i64.atomic.rmw32.or_u'
  | 'i32.atomic.rmw.xor'
  | 'i64.atomic.rmw.xor'
  | 'i32.atomic.rmw8.xor_u'
  | 'i32.atomic.rmw16.xor_u'
  | 'i64.atomic.rmw8.xor_u'
  | 'i64.atomic.rmw16.xor_u'
  | 'i64.atomic.rmw32.xor_u'
  | 'i32.atomic.rmw.xchg'
  | 'i64.atomic.rmw.xchg'
  | 'i32.atomic.rmw8.xchg_u'
  | 'i32.atomic.rmw16.xchg_u'
  | 'i64.atomic.rmw8.xchg_u'
  | 'i64.atomic.rmw16.xchg_u'
  | 'i64.atomic.rmw32.xchg_u'
  | 'i32.atomic.rmw.cmpxchg'
  | 'i64.atomic.rmw.cmpxchg'
  | 'i32.atomic.rmw8.cmpxchg_u'
  | 'i32.atomic.rmw16.cmpxchg_u'
  | 'i64.atomic.rmw8.cmpxchg_u'
  | 'i64.atomic.rmw16.cmpxchg_u'
  | 'i64.atomic.rmw32.cmpxchg_u'

/**
 * A branch-hint value for `br_if` and `if` instructions.
 *
 * @category instructions
 * @since 0.0.0
 */
export type BranchHint = 'likely' | 'unlikely'

/**
 * One catch clause of a `try_table`, targeting an enclosing label by relative depth as seen
 * from the `try_table` instruction itself.
 *
 * @category instructions
 * @since 0.0.0
 */
export type Catch =
  | { readonly _tag: 'Catch'; readonly tag: Tag.Tag; readonly depth: number }
  | { readonly _tag: 'CatchRef'; readonly tag: Tag.Tag; readonly depth: number }
  | { readonly _tag: 'CatchAll'; readonly depth: number }
  | { readonly _tag: 'CatchAllRef'; readonly depth: number }

/**
 * The type of a structured control-flow block.
 *
 * @category instructions
 * @since 0.0.0
 */
export type BlockType =
  | { readonly _tag: 'Empty' }
  | { readonly _tag: 'Value'; readonly type: ValType.ValType }
  | { readonly _tag: 'Func'; readonly type: Type.Type }

/**
 * A WebAssembly instruction as an immutable value.
 *
 * @category instructions
 * @since 0.0.0
 */
export type Instr =
  | { readonly _tag: 'Op'; readonly mnemonic: PlainMnemonic }
  | { readonly _tag: 'I32Const'; readonly value: number }
  | { readonly _tag: 'I64Const'; readonly value: bigint }
  | { readonly _tag: 'F32Const'; readonly value: number }
  | { readonly _tag: 'F64Const'; readonly value: number }
  | { readonly _tag: 'LocalGet'; readonly local: number }
  | { readonly _tag: 'LocalSet'; readonly local: number }
  | { readonly _tag: 'LocalTee'; readonly local: number }
  | { readonly _tag: 'GlobalGet'; readonly global: Global.Global }
  | { readonly _tag: 'GlobalSet'; readonly global: Global.Global }
  | { readonly _tag: 'Call'; readonly func: Func.Func }
  | {
      readonly _tag: 'CallIndirect'
      readonly table: Table.Table
      readonly type: Type.Type
    }
  | { readonly _tag: 'ReturnCall'; readonly func: Func.Func }
  | {
      readonly _tag: 'ReturnCallIndirect'
      readonly table: Table.Table
      readonly type: Type.Type
    }
  | {
      readonly _tag: 'Block'
      readonly blockType: BlockType
      readonly body: ReadonlyArray<Instr>
    }
  | {
      readonly _tag: 'Loop'
      readonly blockType: BlockType
      readonly body: ReadonlyArray<Instr>
    }
  | {
      readonly _tag: 'If'
      readonly blockType: BlockType
      readonly thenBody: ReadonlyArray<Instr>
      readonly elseBody: ReadonlyArray<Instr>
      readonly hint: BranchHint | undefined
    }
  | { readonly _tag: 'Br'; readonly depth: number }
  | { readonly _tag: 'BrIf'; readonly depth: number; readonly hint: BranchHint | undefined }
  | {
      readonly _tag: 'BrTable'
      readonly depths: ReadonlyArray<number>
      readonly defaultDepth: number
    }
  | { readonly _tag: 'SelectTyped'; readonly types: ReadonlyArray<ValType.ValType> }
  | { readonly _tag: 'RefNull'; readonly heapType: ValType.HeapType }
  | { readonly _tag: 'RefFunc'; readonly func: Func.Func }
  | { readonly _tag: 'V128Const'; readonly bytes: ReadonlyArray<number> }
  | { readonly _tag: 'Shuffle'; readonly lanes: ReadonlyArray<number> }
  | {
      readonly _tag: 'SimdLane'
      readonly mnemonic: SimdLaneMnemonic
      readonly lane: number
    }
  | {
      readonly _tag: 'SimdMemoryAccess'
      readonly mnemonic: SimdMemoryAccessMnemonic
      readonly memory: Memory.Memory
      readonly align: number
      readonly offset: number | bigint
    }
  | {
      readonly _tag: 'SimdMemoryLane'
      readonly mnemonic: SimdLaneMemoryMnemonic
      readonly memory: Memory.Memory
      readonly align: number
      readonly offset: number | bigint
      readonly lane: number
    }
  | {
      readonly _tag: 'AtomicAccess'
      readonly mnemonic: AtomicAccessMnemonic
      readonly memory: Memory.Memory
      readonly align: number
      readonly offset: number | bigint
    }
  | {
      readonly _tag: 'MemoryAccess'
      readonly mnemonic: MemoryAccessMnemonic
      readonly memory: Memory.Memory
      readonly align: number
      readonly offset: number | bigint
    }
  | { readonly _tag: 'MemorySize'; readonly memory: Memory.Memory }
  | { readonly _tag: 'MemoryGrow'; readonly memory: Memory.Memory }
  | {
      readonly _tag: 'MemoryInit'
      readonly data: Data.Data
      readonly memory: Memory.Memory
    }
  | { readonly _tag: 'DataDrop'; readonly data: Data.Data }
  | {
      readonly _tag: 'MemoryCopy'
      readonly destination: Memory.Memory
      readonly source: Memory.Memory
    }
  | { readonly _tag: 'MemoryFill'; readonly memory: Memory.Memory }
  | { readonly _tag: 'Throw'; readonly tag: Tag.Tag }
  | {
      readonly _tag: 'TryTable'
      readonly blockType: BlockType
      readonly catches: ReadonlyArray<Catch>
      readonly body: ReadonlyArray<Instr>
    }
  | { readonly _tag: 'StructNew'; readonly type: Type.Type; readonly defaults: boolean }
  | {
      readonly _tag: 'StructGet'
      readonly type: Type.Type
      readonly field: number
      readonly sign: 's' | 'u' | undefined
    }
  | { readonly _tag: 'StructSet'; readonly type: Type.Type; readonly field: number }
  | { readonly _tag: 'ArrayNew'; readonly type: Type.Type; readonly defaults: boolean }
  | { readonly _tag: 'ArrayNewFixed'; readonly type: Type.Type; readonly length: number }
  | { readonly _tag: 'ArrayNewData'; readonly type: Type.Type; readonly data: Data.Data }
  | { readonly _tag: 'ArrayNewElem'; readonly type: Type.Type; readonly elem: Elem.Elem }
  | {
      readonly _tag: 'ArrayGet'
      readonly type: Type.Type
      readonly sign: 's' | 'u' | undefined
    }
  | { readonly _tag: 'ArraySet'; readonly type: Type.Type }
  | { readonly _tag: 'ArrayFill'; readonly type: Type.Type }
  | {
      readonly _tag: 'ArrayCopy'
      readonly destination: Type.Type
      readonly source: Type.Type
    }
  | { readonly _tag: 'ArrayInitData'; readonly type: Type.Type; readonly data: Data.Data }
  | { readonly _tag: 'ArrayInitElem'; readonly type: Type.Type; readonly elem: Elem.Elem }
  | { readonly _tag: 'RefTest'; readonly refType: ValType.RefType }
  | { readonly _tag: 'RefCast'; readonly refType: ValType.RefType }
  | {
      readonly _tag: 'BrOnCast'
      readonly depth: number
      readonly from: ValType.RefType
      readonly to: ValType.RefType
      readonly fail: boolean
    }
  | { readonly _tag: 'BrOnNull'; readonly depth: number }
  | { readonly _tag: 'BrOnNonNull'; readonly depth: number }
  | { readonly _tag: 'CallRef'; readonly type: Type.Type }
  | { readonly _tag: 'ReturnCallRef'; readonly type: Type.Type }
  | { readonly _tag: 'TableGet'; readonly table: Table.Table }
  | { readonly _tag: 'TableSet'; readonly table: Table.Table }
  | { readonly _tag: 'TableSize'; readonly table: Table.Table }
  | { readonly _tag: 'TableGrow'; readonly table: Table.Table }
  | { readonly _tag: 'TableFill'; readonly table: Table.Table }
  | {
      readonly _tag: 'TableCopy'
      readonly destination: Table.Table
      readonly source: Table.Table
    }
  | { readonly _tag: 'TableInit'; readonly elem: Elem.Elem; readonly table: Table.Table }
  | { readonly _tag: 'ElemDrop'; readonly elem: Elem.Elem }

const freeze = <A extends object>(value: A): A => Object.freeze(value)

/**
 * Constructs an instruction that carries no immediate operand.
 *
 * **Example** (Adding two stack values)
 *
 * ```ts
 * import * as Instr from '@silklang/wasm/Instr'
 *
 * const body = [Instr.localGet(0), Instr.localGet(1), Instr.op('i32.add')]
 * ```
 *
 * @category constructors
 * @since 0.0.0
 */
export const op = (mnemonic: PlainMnemonic): Instr => freeze({ _tag: 'Op', mnemonic })

/**
 * Constructs `i32.const` with a 32-bit integer immediate.
 *
 * @category constructors
 * @since 0.0.0
 */
export const i32Const = (value: number): Instr => freeze({ _tag: 'I32Const', value })

/**
 * Constructs `i64.const` with a 64-bit integer immediate.
 *
 * @category constructors
 * @since 0.0.0
 */
export const i64Const = (value: bigint): Instr => freeze({ _tag: 'I64Const', value })

/**
 * Constructs `f32.const` with a 32-bit float immediate.
 *
 * @category constructors
 * @since 0.0.0
 */
export const f32Const = (value: number): Instr => freeze({ _tag: 'F32Const', value })

/**
 * Constructs `f64.const` with a 64-bit float immediate.
 *
 * @category constructors
 * @since 0.0.0
 */
export const f64Const = (value: number): Instr => freeze({ _tag: 'F64Const', value })

/**
 * Constructs `local.get` reading a parameter or local by index.
 *
 * @category constructors
 * @since 0.0.0
 */
export const localGet = (local: number): Instr => freeze({ _tag: 'LocalGet', local })

/**
 * Constructs `local.set` writing a parameter or local by index.
 *
 * @category constructors
 * @since 0.0.0
 */
export const localSet = (local: number): Instr => freeze({ _tag: 'LocalSet', local })

/**
 * Constructs `local.tee` writing a local while keeping the value on the stack.
 *
 * @category constructors
 * @since 0.0.0
 */
export const localTee = (local: number): Instr => freeze({ _tag: 'LocalTee', local })

/**
 * Constructs `global.get` reading a global.
 *
 * @category constructors
 * @since 0.0.0
 */
export const globalGet = (global: Global.Global): Instr => freeze({ _tag: 'GlobalGet', global })

/**
 * Constructs `global.set` writing a mutable global.
 *
 * @category constructors
 * @since 0.0.0
 */
export const globalSet = (global: Global.Global): Instr => freeze({ _tag: 'GlobalSet', global })

/**
 * Constructs `call` targeting an imported or defined function.
 *
 * @category constructors
 * @since 0.0.0
 */
export const call = (func: Func.Func): Instr => freeze({ _tag: 'Call', func })

/**
 * Constructs `call_indirect` through a table with an expected function type.
 *
 * @category constructors
 * @since 0.0.0
 */
export const callIndirect = (table: Table.Table, type: Type.Type): Instr =>
  freeze({ _tag: 'CallIndirect', table, type })

/**
 * Constructs `return_call`, the tail-calling form of `call`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const returnCall = (func: Func.Func): Instr => freeze({ _tag: 'ReturnCall', func })

/**
 * Constructs `return_call_indirect`, the tail-calling form of `call_indirect`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const returnCallIndirect = (table: Table.Table, type: Type.Type): Instr =>
  freeze({ _tag: 'ReturnCallIndirect', table, type })

/**
 * The empty block type `[] -> []`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const emptyBlockType: BlockType = Object.freeze({ _tag: 'Empty' })

/**
 * A block type producing one value.
 *
 * @category constructors
 * @since 0.0.0
 */
export const valueBlockType = (type: ValType.ValType): BlockType => freeze({ _tag: 'Value', type })

/**
 * A block type referencing an interned function type for multi-value blocks.
 *
 * @category constructors
 * @since 0.0.0
 */
export const funcBlockType = (type: Type.Type): BlockType => freeze({ _tag: 'Func', type })

/**
 * Constructs a `block` containing nested instructions.
 *
 * @category constructors
 * @since 0.0.0
 */
export const block = (blockType: BlockType, body: ReadonlyArray<Instr>): Instr =>
  freeze({ _tag: 'Block', blockType, body: Object.freeze([...body]) })

/**
 * Constructs a `loop` containing nested instructions.
 *
 * @category constructors
 * @since 0.0.0
 */
export const loop = (blockType: BlockType, body: ReadonlyArray<Instr>): Instr =>
  freeze({ _tag: 'Loop', blockType, body: Object.freeze([...body]) })

/**
 * Constructs an `if`/`else` with nested then and else instruction sequences.
 *
 * @category constructors
 * @since 0.0.0
 */
export const ifElse = (
  blockType: BlockType,
  thenBody: ReadonlyArray<Instr>,
  elseBody: ReadonlyArray<Instr> = [],
  options: BranchOptions = {},
): Instr =>
  freeze({
    _tag: 'If',
    blockType,
    thenBody: Object.freeze([...thenBody]),
    elseBody: Object.freeze([...elseBody]),
    hint: options.hint,
  })

/**
 * Options accepted by hintable branch constructors.
 *
 * @category constructors
 * @since 0.0.0
 */
export interface BranchOptions {
  /** Marks the branch likely or unlikely for the `metadata.code.branch_hint` section. */
  readonly hint?: BranchHint
}

/**
 * Constructs `br` branching to an enclosing block by relative depth.
 *
 * @category constructors
 * @since 0.0.0
 */
export const br = (depth: number): Instr => freeze({ _tag: 'Br', depth })

/**
 * Constructs `br_if` conditionally branching by relative depth.
 *
 * @category constructors
 * @since 0.0.0
 */
export const brIf = (depth: number, options: BranchOptions = {}): Instr =>
  freeze({ _tag: 'BrIf', depth, hint: options.hint })

/**
 * Constructs `br_table` selecting among branch depths by index.
 *
 * @category constructors
 * @since 0.0.0
 */
export const brTable = (depths: ReadonlyArray<number>, defaultDepth: number): Instr =>
  freeze({ _tag: 'BrTable', depths: Object.freeze([...depths]), defaultDepth })

/**
 * Constructs `select` with an explicit result-type annotation, required for reference types.
 *
 * @category constructors
 * @since 0.0.0
 */
export const selectTyped = (types: ReadonlyArray<ValType.ValType>): Instr =>
  freeze({ _tag: 'SelectTyped', types: Object.freeze([...types]) })

/**
 * Constructs `ref.null` producing a null reference of the given type.
 *
 * @category constructors
 * @since 0.0.0
 */
export const refNull = (input: ValType.HeapTypeInput | ValType.RefType): Instr =>
  freeze({
    _tag: 'RefNull',
    heapType:
      typeof input !== 'string' && '_tag' in input && input._tag === 'Ref'
        ? input.heapType
        : ValType.heapType(input as ValType.HeapTypeInput),
  })

/**
 * Constructs `ref.func` producing a reference to a function.
 *
 * @category constructors
 * @since 0.0.0
 */
export const refFunc = (func: Func.Func): Instr => freeze({ _tag: 'RefFunc', func })

/**
 * Options accepted by {@link memoryAccess}.
 *
 * @category constructors
 * @since 0.0.0
 */
export interface MemoryAccessOptions {
  /** Static address offset in bytes. Defaults to zero. */
  readonly offset?: number | bigint
  /** Base-two logarithm of the alignment. Defaults to the access's natural alignment. */
  readonly align?: number
}

/**
 * Constructs a memory load or store against one memory with optional offset and alignment.
 *
 * **Example** (Loading from the second memory)
 *
 * ```ts
 * import * as Instr from '@silklang/wasm/Instr'
 * import type * as Memory from '@silklang/wasm/Memory'
 *
 * declare const scratch: Memory.Memory
 *
 * const load = Instr.memoryAccess('i32.load', scratch, { offset: 16 })
 * ```
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryAccess = (
  mnemonic: MemoryAccessMnemonic,
  memory: Memory.Memory,
  options: MemoryAccessOptions = {},
): Instr =>
  freeze({
    _tag: 'MemoryAccess',
    mnemonic,
    memory,
    align: options.align ?? InstructionTable.memoryAccessOps[mnemonic].widthLog2,
    offset: options.offset ?? 0,
  })

/**
 * Constructs `memory.size` for one memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memorySize = (memory: Memory.Memory): Instr => freeze({ _tag: 'MemorySize', memory })

/**
 * Constructs `memory.grow` for one memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryGrow = (memory: Memory.Memory): Instr => freeze({ _tag: 'MemoryGrow', memory })

/**
 * Constructs `memory.init` copying a passive data segment into a memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryInit = (data: Data.Data, memory: Memory.Memory): Instr =>
  freeze({ _tag: 'MemoryInit', data, memory })

/**
 * Constructs `data.drop` releasing a passive data segment.
 *
 * @category constructors
 * @since 0.0.0
 */
export const dataDrop = (data: Data.Data): Instr => freeze({ _tag: 'DataDrop', data })

/**
 * Constructs `memory.copy` between two memories, which may be the same memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryCopy = (destination: Memory.Memory, source: Memory.Memory): Instr =>
  freeze({ _tag: 'MemoryCopy', destination, source })

/**
 * Constructs `memory.fill` for one memory.
 *
 * @category constructors
 * @since 0.0.0
 */
export const memoryFill = (memory: Memory.Memory): Instr => freeze({ _tag: 'MemoryFill', memory })

/**
 * Constructs `table.get`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableGet = (table: Table.Table): Instr => freeze({ _tag: 'TableGet', table })

/**
 * Constructs `table.set`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableSet = (table: Table.Table): Instr => freeze({ _tag: 'TableSet', table })

/**
 * Constructs `table.size`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableSize = (table: Table.Table): Instr => freeze({ _tag: 'TableSize', table })

/**
 * Constructs `table.grow`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableGrow = (table: Table.Table): Instr => freeze({ _tag: 'TableGrow', table })

/**
 * Constructs `table.fill`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableFill = (table: Table.Table): Instr => freeze({ _tag: 'TableFill', table })

/**
 * Constructs `table.copy` between two tables, which may be the same table.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableCopy = (destination: Table.Table, source: Table.Table): Instr =>
  freeze({ _tag: 'TableCopy', destination, source })

/**
 * Constructs `table.init` copying a passive element segment into a table.
 *
 * @category constructors
 * @since 0.0.0
 */
export const tableInit = (elem: Elem.Elem, table: Table.Table): Instr =>
  freeze({ _tag: 'TableInit', elem, table })

/**
 * Constructs `elem.drop` releasing a passive element segment.
 *
 * @category constructors
 * @since 0.0.0
 */
export const elemDrop = (elem: Elem.Elem): Instr => freeze({ _tag: 'ElemDrop', elem })

/**
 * Constructs `v128.const` from exactly 16 bytes.
 *
 * @category constructors
 * @since 0.0.0
 */
export const v128Const = (bytes: Uint8Array | ReadonlyArray<number>): Instr =>
  freeze({ _tag: 'V128Const', bytes: Object.freeze([...bytes]) })

/**
 * Constructs `i8x16.shuffle` from exactly 16 lane selectors in `0..31`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const i8x16Shuffle = (lanes: ReadonlyArray<number>): Instr =>
  freeze({ _tag: 'Shuffle', lanes: Object.freeze([...lanes]) })

/**
 * Constructs a SIMD lane extract or replace with an immediate lane index.
 *
 * @category constructors
 * @since 0.0.0
 */
export const simdLane = (mnemonic: SimdLaneMnemonic, lane: number): Instr =>
  freeze({ _tag: 'SimdLane', mnemonic, lane })

/**
 * Constructs a SIMD memory load or store with optional offset and alignment.
 *
 * @category constructors
 * @since 0.0.0
 */
export const simdMemoryAccess = (
  mnemonic: SimdMemoryAccessMnemonic,
  memory: Memory.Memory,
  options: MemoryAccessOptions = {},
): Instr =>
  freeze({
    _tag: 'SimdMemoryAccess',
    mnemonic,
    memory,
    align: options.align ?? InstructionTable.simdMemoryAccessOps[mnemonic].widthLog2,
    offset: options.offset ?? 0,
  })

/**
 * Constructs a SIMD lane-addressed memory load or store.
 *
 * @category constructors
 * @since 0.0.0
 */
export const simdMemoryLane = (
  mnemonic: SimdLaneMemoryMnemonic,
  memory: Memory.Memory,
  lane: number,
  options: MemoryAccessOptions = {},
): Instr =>
  freeze({
    _tag: 'SimdMemoryLane',
    mnemonic,
    memory,
    align: options.align ?? InstructionTable.simdLaneMemoryOps[mnemonic].widthLog2,
    offset: options.offset ?? 0,
    lane,
  })

/**
 * Constructs an atomic memory instruction. Atomic accesses require their exact natural
 * alignment; the default is always valid.
 *
 * @category constructors
 * @since 0.0.0
 */
export const atomicAccess = (
  mnemonic: AtomicAccessMnemonic,
  memory: Memory.Memory,
  options: MemoryAccessOptions = {},
): Instr =>
  freeze({
    _tag: 'AtomicAccess',
    mnemonic,
    memory,
    align: options.align ?? InstructionTable.atomicAccessOps[mnemonic].widthLog2,
    offset: options.offset ?? 0,
  })

/**
 * Constructs `throw`, raising an exception with the tag's payload popped from the stack.
 *
 * @category constructors
 * @since 0.0.0
 */
export const throwTag = (tag: Tag.Tag): Instr => freeze({ _tag: 'Throw', tag })

/**
 * Constructs a `catch` clause delivering the tag's parameters to a label.
 *
 * @category constructors
 * @since 0.0.0
 */
export const catchTag = (tag: Tag.Tag, depth: number): Catch =>
  freeze({ _tag: 'Catch', tag, depth })

/**
 * Constructs a `catch_ref` clause delivering the tag's parameters plus the `exnref`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const catchTagRef = (tag: Tag.Tag, depth: number): Catch =>
  freeze({ _tag: 'CatchRef', tag, depth })

/**
 * Constructs a `catch_all` clause delivering nothing.
 *
 * @category constructors
 * @since 0.0.0
 */
export const catchAll = (depth: number): Catch => freeze({ _tag: 'CatchAll', depth })

/**
 * Constructs a `catch_all_ref` clause delivering only the `exnref`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const catchAllRef = (depth: number): Catch => freeze({ _tag: 'CatchAllRef', depth })

/**
 * Constructs `try_table`: a structured block whose catch clauses route exceptions to enclosing
 * labels.
 *
 * **Example** (Catching an error tag into an enclosing block)
 *
 * ```ts
 * import * as Instr from '@silklang/wasm/Instr'
 * import type * as Tag from '@silklang/wasm/Tag'
 *
 * declare const error: Tag.Tag
 *
 * const body = [
 *   Instr.block(Instr.emptyBlockType, [
 *     Instr.tryTable(Instr.emptyBlockType, [Instr.catchTag(error, 0)], [
 *       Instr.i32Const(1),
 *       Instr.throwTag(error),
 *     ]),
 *   ]),
 * ]
 * ```
 *
 * @category constructors
 * @since 0.0.0
 */
export const tryTable = (
  blockType: BlockType,
  catches: ReadonlyArray<Catch>,
  body: ReadonlyArray<Instr>,
): Instr =>
  freeze({
    _tag: 'TryTable',
    blockType,
    catches: Object.freeze([...catches]),
    body: Object.freeze([...body]),
  })

/**
 * Constructs `struct.new`, popping one value per field, or `struct.new_default` with
 * `defaults: true`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const structNew = (type: Type.Type, options: { readonly defaults?: boolean } = {}): Instr =>
  freeze({ _tag: 'StructNew', type, defaults: options.defaults ?? false })

/**
 * Constructs `struct.get` (or `struct.get_s`/`struct.get_u` for packed fields).
 *
 * @category constructors
 * @since 0.0.0
 */
export const structGet = (type: Type.Type, field: number, sign?: 's' | 'u'): Instr =>
  freeze({ _tag: 'StructGet', type, field, sign })

/**
 * Constructs `struct.set` writing a mutable field.
 *
 * @category constructors
 * @since 0.0.0
 */
export const structSet = (type: Type.Type, field: number): Instr =>
  freeze({ _tag: 'StructSet', type, field })

/**
 * Constructs `array.new`, or `array.new_default` with `defaults: true`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayNew = (type: Type.Type, options: { readonly defaults?: boolean } = {}): Instr =>
  freeze({ _tag: 'ArrayNew', type, defaults: options.defaults ?? false })

/**
 * Constructs `array.new_fixed`, popping `length` elements.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayNewFixed = (type: Type.Type, length: number): Instr =>
  freeze({ _tag: 'ArrayNewFixed', type, length })

/**
 * Constructs `array.new_data`, filling a numeric array from a data segment.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayNewData = (type: Type.Type, data: Data.Data): Instr =>
  freeze({ _tag: 'ArrayNewData', type, data })

/**
 * Constructs `array.new_elem`, filling a reference array from an element segment.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayNewElem = (type: Type.Type, elem: Elem.Elem): Instr =>
  freeze({ _tag: 'ArrayNewElem', type, elem })

/**
 * Constructs `array.get` (or the packed `_s`/`_u` forms).
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayGet = (type: Type.Type, sign?: 's' | 'u'): Instr =>
  freeze({ _tag: 'ArrayGet', type, sign })

/**
 * Constructs `array.set` writing a mutable element.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arraySet = (type: Type.Type): Instr => freeze({ _tag: 'ArraySet', type })

/**
 * Constructs `array.fill`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayFill = (type: Type.Type): Instr => freeze({ _tag: 'ArrayFill', type })

/**
 * Constructs `array.copy` between two array types.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayCopy = (destination: Type.Type, source: Type.Type): Instr =>
  freeze({ _tag: 'ArrayCopy', destination, source })

/**
 * Constructs `array.init_data`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayInitData = (type: Type.Type, data: Data.Data): Instr =>
  freeze({ _tag: 'ArrayInitData', type, data })

/**
 * Constructs `array.init_elem`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const arrayInitElem = (type: Type.Type, elem: Elem.Elem): Instr =>
  freeze({ _tag: 'ArrayInitElem', type, elem })

/**
 * Constructs `ref.test` against a reference type in the operand's hierarchy.
 *
 * @category constructors
 * @since 0.0.0
 */
export const refTest = (refType: ValType.RefType): Instr => freeze({ _tag: 'RefTest', refType })

/**
 * Constructs `ref.cast` to a reference type in the operand's hierarchy.
 *
 * @category constructors
 * @since 0.0.0
 */
export const refCast = (refType: ValType.RefType): Instr => freeze({ _tag: 'RefCast', refType })

/**
 * Constructs `br_on_cast`, branching when the operand casts from `from` to `to`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const brOnCast = (depth: number, from: ValType.RefType, to: ValType.RefType): Instr =>
  freeze({ _tag: 'BrOnCast', depth, from, to, fail: false })

/**
 * Constructs `br_on_cast_fail`, branching when the cast fails.
 *
 * @category constructors
 * @since 0.0.0
 */
export const brOnCastFail = (depth: number, from: ValType.RefType, to: ValType.RefType): Instr =>
  freeze({ _tag: 'BrOnCast', depth, from, to, fail: true })

/**
 * Constructs `br_on_null`, branching when the reference is null and leaving it non-null
 * otherwise.
 *
 * @category constructors
 * @since 0.0.0
 */
export const brOnNull = (depth: number): Instr => freeze({ _tag: 'BrOnNull', depth })

/**
 * Constructs `br_on_non_null`, branching with the non-null reference.
 *
 * @category constructors
 * @since 0.0.0
 */
export const brOnNonNull = (depth: number): Instr => freeze({ _tag: 'BrOnNonNull', depth })

/**
 * Constructs `call_ref`, calling through a typed function reference.
 *
 * @category constructors
 * @since 0.0.0
 */
export const callRef = (type: Type.Type): Instr => freeze({ _tag: 'CallRef', type })

/**
 * Constructs `return_call_ref`, the tail-calling form of `call_ref`.
 *
 * @category constructors
 * @since 0.0.0
 */
export const returnCallRef = (type: Type.Type): Instr => freeze({ _tag: 'ReturnCallRef', type })
