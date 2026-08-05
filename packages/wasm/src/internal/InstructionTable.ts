/**
 * The single source of truth for the baseline instruction set.
 *
 * Every plain (no-immediate) instruction and every memory access instruction has exactly one row
 * here carrying its binary opcode and typing information. The binary encoder, text printer, and
 * the uniform part of the validator all derive from these tables; instructions with
 * context-dependent typing (calls, branches, parametric operations) are handled by named
 * procedures in the validator and only their opcodes appear here.
 *
 * @internal
 */
import type * as Instr from '../Instr.js'
import * as ValType from '../ValType.js'

export type PlainTyping =
  | {
      readonly _tag: 'Uniform'
      readonly params: ReadonlyArray<ValType.ValType>
      readonly results: ReadonlyArray<ValType.ValType>
    }
  /** Typed by a named validator procedure: `unreachable`, `return`, `drop`, `select`, `ref.is_null`. */
  | { readonly _tag: 'Special' }

export interface PlainRow {
  readonly opcode: ReadonlyArray<number>
  readonly typing: PlainTyping
}

const { i32, i64, f32, f64 } = ValType

const uniform = (
  opcode: ReadonlyArray<number>,
  params: ReadonlyArray<ValType.ValType>,
  results: ReadonlyArray<ValType.ValType>,
): PlainRow => ({ opcode, typing: { _tag: 'Uniform', params, results } })

const special = (opcode: ReadonlyArray<number>): PlainRow => ({
  opcode,
  typing: { _tag: 'Special' },
})

const compare = (opcode: number, operand: ValType.ValType): PlainRow =>
  uniform([opcode], [operand, operand], [i32])

const test = (opcode: number, operand: ValType.ValType): PlainRow =>
  uniform([opcode], [operand], [i32])

const binary = (opcode: number, operand: ValType.ValType): PlainRow =>
  uniform([opcode], [operand, operand], [operand])

const unary = (opcode: number, operand: ValType.ValType): PlainRow =>
  uniform([opcode], [operand], [operand])

const convert = (
  opcode: ReadonlyArray<number>,
  from: ValType.ValType,
  to: ValType.ValType,
): PlainRow => uniform(opcode, [from], [to])

export const plainOps: Record<Instr.PlainMnemonic, PlainRow> = {
  unreachable: special([0x00]),
  nop: uniform([0x01], [], []),
  return: special([0x0f]),
  drop: special([0x1a]),
  select: special([0x1b]),
  'ref.is_null': special([0xd1]),
  'i32.eqz': test(0x45, i32),
  'i32.eq': compare(0x46, i32),
  'i32.ne': compare(0x47, i32),
  'i32.lt_s': compare(0x48, i32),
  'i32.lt_u': compare(0x49, i32),
  'i32.gt_s': compare(0x4a, i32),
  'i32.gt_u': compare(0x4b, i32),
  'i32.le_s': compare(0x4c, i32),
  'i32.le_u': compare(0x4d, i32),
  'i32.ge_s': compare(0x4e, i32),
  'i32.ge_u': compare(0x4f, i32),
  'i64.eqz': test(0x50, i64),
  'i64.eq': compare(0x51, i64),
  'i64.ne': compare(0x52, i64),
  'i64.lt_s': compare(0x53, i64),
  'i64.lt_u': compare(0x54, i64),
  'i64.gt_s': compare(0x55, i64),
  'i64.gt_u': compare(0x56, i64),
  'i64.le_s': compare(0x57, i64),
  'i64.le_u': compare(0x58, i64),
  'i64.ge_s': compare(0x59, i64),
  'i64.ge_u': compare(0x5a, i64),
  'f32.eq': compare(0x5b, f32),
  'f32.ne': compare(0x5c, f32),
  'f32.lt': compare(0x5d, f32),
  'f32.gt': compare(0x5e, f32),
  'f32.le': compare(0x5f, f32),
  'f32.ge': compare(0x60, f32),
  'f64.eq': compare(0x61, f64),
  'f64.ne': compare(0x62, f64),
  'f64.lt': compare(0x63, f64),
  'f64.gt': compare(0x64, f64),
  'f64.le': compare(0x65, f64),
  'f64.ge': compare(0x66, f64),
  'i32.clz': unary(0x67, i32),
  'i32.ctz': unary(0x68, i32),
  'i32.popcnt': unary(0x69, i32),
  'i32.add': binary(0x6a, i32),
  'i32.sub': binary(0x6b, i32),
  'i32.mul': binary(0x6c, i32),
  'i32.div_s': binary(0x6d, i32),
  'i32.div_u': binary(0x6e, i32),
  'i32.rem_s': binary(0x6f, i32),
  'i32.rem_u': binary(0x70, i32),
  'i32.and': binary(0x71, i32),
  'i32.or': binary(0x72, i32),
  'i32.xor': binary(0x73, i32),
  'i32.shl': binary(0x74, i32),
  'i32.shr_s': binary(0x75, i32),
  'i32.shr_u': binary(0x76, i32),
  'i32.rotl': binary(0x77, i32),
  'i32.rotr': binary(0x78, i32),
  'i64.clz': unary(0x79, i64),
  'i64.ctz': unary(0x7a, i64),
  'i64.popcnt': unary(0x7b, i64),
  'i64.add': binary(0x7c, i64),
  'i64.sub': binary(0x7d, i64),
  'i64.mul': binary(0x7e, i64),
  'i64.div_s': binary(0x7f, i64),
  'i64.div_u': binary(0x80, i64),
  'i64.rem_s': binary(0x81, i64),
  'i64.rem_u': binary(0x82, i64),
  'i64.and': binary(0x83, i64),
  'i64.or': binary(0x84, i64),
  'i64.xor': binary(0x85, i64),
  'i64.shl': binary(0x86, i64),
  'i64.shr_s': binary(0x87, i64),
  'i64.shr_u': binary(0x88, i64),
  'i64.rotl': binary(0x89, i64),
  'i64.rotr': binary(0x8a, i64),
  'f32.abs': unary(0x8b, f32),
  'f32.neg': unary(0x8c, f32),
  'f32.ceil': unary(0x8d, f32),
  'f32.floor': unary(0x8e, f32),
  'f32.trunc': unary(0x8f, f32),
  'f32.nearest': unary(0x90, f32),
  'f32.sqrt': unary(0x91, f32),
  'f32.add': binary(0x92, f32),
  'f32.sub': binary(0x93, f32),
  'f32.mul': binary(0x94, f32),
  'f32.div': binary(0x95, f32),
  'f32.min': binary(0x96, f32),
  'f32.max': binary(0x97, f32),
  'f32.copysign': binary(0x98, f32),
  'f64.abs': unary(0x99, f64),
  'f64.neg': unary(0x9a, f64),
  'f64.ceil': unary(0x9b, f64),
  'f64.floor': unary(0x9c, f64),
  'f64.trunc': unary(0x9d, f64),
  'f64.nearest': unary(0x9e, f64),
  'f64.sqrt': unary(0x9f, f64),
  'f64.add': binary(0xa0, f64),
  'f64.sub': binary(0xa1, f64),
  'f64.mul': binary(0xa2, f64),
  'f64.div': binary(0xa3, f64),
  'f64.min': binary(0xa4, f64),
  'f64.max': binary(0xa5, f64),
  'f64.copysign': binary(0xa6, f64),
  'i32.wrap_i64': convert([0xa7], i64, i32),
  'i32.trunc_f32_s': convert([0xa8], f32, i32),
  'i32.trunc_f32_u': convert([0xa9], f32, i32),
  'i32.trunc_f64_s': convert([0xaa], f64, i32),
  'i32.trunc_f64_u': convert([0xab], f64, i32),
  'i64.extend_i32_s': convert([0xac], i32, i64),
  'i64.extend_i32_u': convert([0xad], i32, i64),
  'i64.trunc_f32_s': convert([0xae], f32, i64),
  'i64.trunc_f32_u': convert([0xaf], f32, i64),
  'i64.trunc_f64_s': convert([0xb0], f64, i64),
  'i64.trunc_f64_u': convert([0xb1], f64, i64),
  'f32.convert_i32_s': convert([0xb2], i32, f32),
  'f32.convert_i32_u': convert([0xb3], i32, f32),
  'f32.convert_i64_s': convert([0xb4], i64, f32),
  'f32.convert_i64_u': convert([0xb5], i64, f32),
  'f32.demote_f64': convert([0xb6], f64, f32),
  'f64.convert_i32_s': convert([0xb7], i32, f64),
  'f64.convert_i32_u': convert([0xb8], i32, f64),
  'f64.convert_i64_s': convert([0xb9], i64, f64),
  'f64.convert_i64_u': convert([0xba], i64, f64),
  'f64.promote_f32': convert([0xbb], f32, f64),
  'i32.reinterpret_f32': convert([0xbc], f32, i32),
  'i64.reinterpret_f64': convert([0xbd], f64, i64),
  'f32.reinterpret_i32': convert([0xbe], i32, f32),
  'f64.reinterpret_i64': convert([0xbf], i64, f64),
  'i32.extend8_s': unary(0xc0, i32),
  'i32.extend16_s': unary(0xc1, i32),
  'i64.extend8_s': unary(0xc2, i64),
  'i64.extend16_s': unary(0xc3, i64),
  'i64.extend32_s': unary(0xc4, i64),
  'i32.trunc_sat_f32_s': convert([0xfc, 0], f32, i32),
  'i32.trunc_sat_f32_u': convert([0xfc, 1], f32, i32),
  'i32.trunc_sat_f64_s': convert([0xfc, 2], f64, i32),
  'i32.trunc_sat_f64_u': convert([0xfc, 3], f64, i32),
  'i64.trunc_sat_f32_s': convert([0xfc, 4], f32, i64),
  'i64.trunc_sat_f32_u': convert([0xfc, 5], f32, i64),
  'i64.trunc_sat_f64_s': convert([0xfc, 6], f64, i64),
  'i64.trunc_sat_f64_u': convert([0xfc, 7], f64, i64),
}

export interface MemoryAccessRow {
  readonly opcode: number
  readonly valType: ValType.ValType
  /** Base-two logarithm of the access width; the natural and maximum allowed alignment. */
  readonly widthLog2: number
  readonly kind: 'load' | 'store'
}

const load = (opcode: number, valType: ValType.ValType, widthLog2: number): MemoryAccessRow => ({
  opcode,
  valType,
  widthLog2,
  kind: 'load',
})

const store = (opcode: number, valType: ValType.ValType, widthLog2: number): MemoryAccessRow => ({
  opcode,
  valType,
  widthLog2,
  kind: 'store',
})

export const memoryAccessOps: Record<Instr.MemoryAccessMnemonic, MemoryAccessRow> = {
  'i32.load': load(0x28, i32, 2),
  'i64.load': load(0x29, i64, 3),
  'f32.load': load(0x2a, f32, 2),
  'f64.load': load(0x2b, f64, 3),
  'i32.load8_s': load(0x2c, i32, 0),
  'i32.load8_u': load(0x2d, i32, 0),
  'i32.load16_s': load(0x2e, i32, 1),
  'i32.load16_u': load(0x2f, i32, 1),
  'i64.load8_s': load(0x30, i64, 0),
  'i64.load8_u': load(0x31, i64, 0),
  'i64.load16_s': load(0x32, i64, 1),
  'i64.load16_u': load(0x33, i64, 1),
  'i64.load32_s': load(0x34, i64, 2),
  'i64.load32_u': load(0x35, i64, 2),
  'i32.store': store(0x36, i32, 2),
  'i64.store': store(0x37, i64, 3),
  'f32.store': store(0x38, f32, 2),
  'f64.store': store(0x39, f64, 3),
  'i32.store8': store(0x3a, i32, 0),
  'i32.store16': store(0x3b, i32, 1),
  'i64.store8': store(0x3c, i64, 0),
  'i64.store16': store(0x3d, i64, 1),
  'i64.store32': store(0x3e, i64, 2),
}

/** Opcodes for instructions whose typing is handled by named validator procedures. */
export const opcodes = {
  block: 0x02,
  loop: 0x03,
  if: 0x04,
  else: 0x05,
  end: 0x0b,
  br: 0x0c,
  brIf: 0x0d,
  brTable: 0x0e,
  call: 0x10,
  callIndirect: 0x11,
  returnCall: 0x12,
  returnCallIndirect: 0x13,
  selectTyped: 0x1c,
  localGet: 0x20,
  localSet: 0x21,
  localTee: 0x22,
  globalGet: 0x23,
  globalSet: 0x24,
  tableGet: 0x25,
  tableSet: 0x26,
  memorySize: 0x3f,
  memoryGrow: 0x40,
  i32Const: 0x41,
  i64Const: 0x42,
  f32Const: 0x43,
  f64Const: 0x44,
  refNull: 0xd0,
  refFunc: 0xd2,
  prefixFc: 0xfc,
} as const

/** Second bytes of `0xFC`-prefixed instructions that carry immediates. */
export const fcOpcodes = {
  memoryInit: 8,
  dataDrop: 9,
  memoryCopy: 10,
  memoryFill: 11,
  tableInit: 12,
  elemDrop: 13,
  tableCopy: 14,
  tableGrow: 15,
  tableSize: 16,
  tableFill: 17,
} as const
