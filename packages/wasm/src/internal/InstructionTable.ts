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

const { v128 } = ValType

/** `0xFD`-prefixed SIMD opcode with its LEB128-encoded subopcode. */
const simd = (subopcode: number): ReadonlyArray<number> =>
  subopcode < 0x80 ? [0xfd, subopcode] : [0xfd, (subopcode & 0x7f) | 0x80, subopcode >> 7]

const vBinary = (subopcode: number): PlainRow => uniform(simd(subopcode), [v128, v128], [v128])

const vUnary = (subopcode: number): PlainRow => uniform(simd(subopcode), [v128], [v128])

const vTest = (subopcode: number): PlainRow => uniform(simd(subopcode), [v128], [i32])

const vShift = (subopcode: number): PlainRow => uniform(simd(subopcode), [v128, i32], [v128])

const vSplat = (subopcode: number, scalar: ValType.ValType): PlainRow =>
  uniform(simd(subopcode), [scalar], [v128])

const vTernary = (subopcode: number): PlainRow =>
  uniform(simd(subopcode), [v128, v128, v128], [v128])

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
  'atomic.fence': uniform([0xfe, 0x03, 0x00], [], []),
  'i8x16.swizzle': vBinary(14),
  'i8x16.splat': vSplat(15, i32),
  'i16x8.splat': vSplat(16, i32),
  'i32x4.splat': vSplat(17, i32),
  'i64x2.splat': vSplat(18, i64),
  'f32x4.splat': vSplat(19, f32),
  'f64x2.splat': vSplat(20, f64),
  'i8x16.eq': vBinary(35),
  'i8x16.ne': vBinary(36),
  'i8x16.lt_s': vBinary(37),
  'i8x16.lt_u': vBinary(38),
  'i8x16.gt_s': vBinary(39),
  'i8x16.gt_u': vBinary(40),
  'i8x16.le_s': vBinary(41),
  'i8x16.le_u': vBinary(42),
  'i8x16.ge_s': vBinary(43),
  'i8x16.ge_u': vBinary(44),
  'i16x8.eq': vBinary(45),
  'i16x8.ne': vBinary(46),
  'i16x8.lt_s': vBinary(47),
  'i16x8.lt_u': vBinary(48),
  'i16x8.gt_s': vBinary(49),
  'i16x8.gt_u': vBinary(50),
  'i16x8.le_s': vBinary(51),
  'i16x8.le_u': vBinary(52),
  'i16x8.ge_s': vBinary(53),
  'i16x8.ge_u': vBinary(54),
  'i32x4.eq': vBinary(55),
  'i32x4.ne': vBinary(56),
  'i32x4.lt_s': vBinary(57),
  'i32x4.lt_u': vBinary(58),
  'i32x4.gt_s': vBinary(59),
  'i32x4.gt_u': vBinary(60),
  'i32x4.le_s': vBinary(61),
  'i32x4.le_u': vBinary(62),
  'i32x4.ge_s': vBinary(63),
  'i32x4.ge_u': vBinary(64),
  'f32x4.eq': vBinary(65),
  'f32x4.ne': vBinary(66),
  'f32x4.lt': vBinary(67),
  'f32x4.gt': vBinary(68),
  'f32x4.le': vBinary(69),
  'f32x4.ge': vBinary(70),
  'f64x2.eq': vBinary(71),
  'f64x2.ne': vBinary(72),
  'f64x2.lt': vBinary(73),
  'f64x2.gt': vBinary(74),
  'f64x2.le': vBinary(75),
  'f64x2.ge': vBinary(76),
  'v128.not': vUnary(77),
  'v128.and': vBinary(78),
  'v128.andnot': vBinary(79),
  'v128.or': vBinary(80),
  'v128.xor': vBinary(81),
  'v128.bitselect': vTernary(82),
  'v128.any_true': vTest(83),
  'f32x4.demote_f64x2_zero': vUnary(94),
  'f64x2.promote_low_f32x4': vUnary(95),
  'i8x16.abs': vUnary(96),
  'i8x16.neg': vUnary(97),
  'i8x16.popcnt': vUnary(98),
  'i8x16.all_true': vTest(99),
  'i8x16.bitmask': vTest(100),
  'i8x16.narrow_i16x8_s': vBinary(101),
  'i8x16.narrow_i16x8_u': vBinary(102),
  'f32x4.ceil': vUnary(103),
  'f32x4.floor': vUnary(104),
  'f32x4.trunc': vUnary(105),
  'f32x4.nearest': vUnary(106),
  'i8x16.shl': vShift(107),
  'i8x16.shr_s': vShift(108),
  'i8x16.shr_u': vShift(109),
  'i8x16.add': vBinary(110),
  'i8x16.add_sat_s': vBinary(111),
  'i8x16.add_sat_u': vBinary(112),
  'i8x16.sub': vBinary(113),
  'i8x16.sub_sat_s': vBinary(114),
  'i8x16.sub_sat_u': vBinary(115),
  'f64x2.ceil': vUnary(116),
  'f64x2.floor': vUnary(117),
  'i8x16.min_s': vBinary(118),
  'i8x16.min_u': vBinary(119),
  'i8x16.max_s': vBinary(120),
  'i8x16.max_u': vBinary(121),
  'f64x2.trunc': vUnary(122),
  'i8x16.avgr_u': vBinary(123),
  'i16x8.extadd_pairwise_i8x16_s': vUnary(124),
  'i16x8.extadd_pairwise_i8x16_u': vUnary(125),
  'i32x4.extadd_pairwise_i16x8_s': vUnary(126),
  'i32x4.extadd_pairwise_i16x8_u': vUnary(127),
  'i16x8.abs': vUnary(128),
  'i16x8.neg': vUnary(129),
  'i16x8.q15mulr_sat_s': vBinary(130),
  'i16x8.all_true': vTest(131),
  'i16x8.bitmask': vTest(132),
  'i16x8.narrow_i32x4_s': vBinary(133),
  'i16x8.narrow_i32x4_u': vBinary(134),
  'i16x8.extend_low_i8x16_s': vUnary(135),
  'i16x8.extend_high_i8x16_s': vUnary(136),
  'i16x8.extend_low_i8x16_u': vUnary(137),
  'i16x8.extend_high_i8x16_u': vUnary(138),
  'i16x8.shl': vShift(139),
  'i16x8.shr_s': vShift(140),
  'i16x8.shr_u': vShift(141),
  'i16x8.add': vBinary(142),
  'i16x8.add_sat_s': vBinary(143),
  'i16x8.add_sat_u': vBinary(144),
  'i16x8.sub': vBinary(145),
  'i16x8.sub_sat_s': vBinary(146),
  'i16x8.sub_sat_u': vBinary(147),
  'f64x2.nearest': vUnary(148),
  'i16x8.mul': vBinary(149),
  'i16x8.min_s': vBinary(150),
  'i16x8.min_u': vBinary(151),
  'i16x8.max_s': vBinary(152),
  'i16x8.max_u': vBinary(153),
  'i16x8.avgr_u': vBinary(155),
  'i16x8.extmul_low_i8x16_s': vBinary(156),
  'i16x8.extmul_high_i8x16_s': vBinary(157),
  'i16x8.extmul_low_i8x16_u': vBinary(158),
  'i16x8.extmul_high_i8x16_u': vBinary(159),
  'i32x4.abs': vUnary(160),
  'i32x4.neg': vUnary(161),
  'i32x4.all_true': vTest(163),
  'i32x4.bitmask': vTest(164),
  'i32x4.extend_low_i16x8_s': vUnary(167),
  'i32x4.extend_high_i16x8_s': vUnary(168),
  'i32x4.extend_low_i16x8_u': vUnary(169),
  'i32x4.extend_high_i16x8_u': vUnary(170),
  'i32x4.shl': vShift(171),
  'i32x4.shr_s': vShift(172),
  'i32x4.shr_u': vShift(173),
  'i32x4.add': vBinary(174),
  'i32x4.sub': vBinary(177),
  'i32x4.mul': vBinary(181),
  'i32x4.min_s': vBinary(182),
  'i32x4.min_u': vBinary(183),
  'i32x4.max_s': vBinary(184),
  'i32x4.max_u': vBinary(185),
  'i32x4.dot_i16x8_s': vBinary(186),
  'i32x4.extmul_low_i16x8_s': vBinary(188),
  'i32x4.extmul_high_i16x8_s': vBinary(189),
  'i32x4.extmul_low_i16x8_u': vBinary(190),
  'i32x4.extmul_high_i16x8_u': vBinary(191),
  'i64x2.abs': vUnary(192),
  'i64x2.neg': vUnary(193),
  'i64x2.all_true': vTest(195),
  'i64x2.bitmask': vTest(196),
  'i64x2.extend_low_i32x4_s': vUnary(199),
  'i64x2.extend_high_i32x4_s': vUnary(200),
  'i64x2.extend_low_i32x4_u': vUnary(201),
  'i64x2.extend_high_i32x4_u': vUnary(202),
  'i64x2.shl': vShift(203),
  'i64x2.shr_s': vShift(204),
  'i64x2.shr_u': vShift(205),
  'i64x2.add': vBinary(206),
  'i64x2.sub': vBinary(209),
  'i64x2.mul': vBinary(213),
  'i64x2.eq': vBinary(214),
  'i64x2.ne': vBinary(215),
  'i64x2.lt_s': vBinary(216),
  'i64x2.gt_s': vBinary(217),
  'i64x2.le_s': vBinary(218),
  'i64x2.ge_s': vBinary(219),
  'i64x2.extmul_low_i32x4_s': vBinary(220),
  'i64x2.extmul_high_i32x4_s': vBinary(221),
  'i64x2.extmul_low_i32x4_u': vBinary(222),
  'i64x2.extmul_high_i32x4_u': vBinary(223),
  'f32x4.abs': vUnary(224),
  'f32x4.neg': vUnary(225),
  'f32x4.sqrt': vUnary(227),
  'f32x4.add': vBinary(228),
  'f32x4.sub': vBinary(229),
  'f32x4.mul': vBinary(230),
  'f32x4.div': vBinary(231),
  'f32x4.min': vBinary(232),
  'f32x4.max': vBinary(233),
  'f32x4.pmin': vBinary(234),
  'f32x4.pmax': vBinary(235),
  'f64x2.abs': vUnary(236),
  'f64x2.neg': vUnary(237),
  'f64x2.sqrt': vUnary(239),
  'f64x2.add': vBinary(240),
  'f64x2.sub': vBinary(241),
  'f64x2.mul': vBinary(242),
  'f64x2.div': vBinary(243),
  'f64x2.min': vBinary(244),
  'f64x2.max': vBinary(245),
  'f64x2.pmin': vBinary(246),
  'f64x2.pmax': vBinary(247),
  'i32x4.trunc_sat_f32x4_s': vUnary(248),
  'i32x4.trunc_sat_f32x4_u': vUnary(249),
  'f32x4.convert_i32x4_s': vUnary(250),
  'f32x4.convert_i32x4_u': vUnary(251),
  'i32x4.trunc_sat_f64x2_s_zero': vUnary(252),
  'i32x4.trunc_sat_f64x2_u_zero': vUnary(253),
  'f64x2.convert_low_i32x4_s': vUnary(254),
  'f64x2.convert_low_i32x4_u': vUnary(255),
  'i8x16.relaxed_swizzle': vBinary(256),
  'i32x4.relaxed_trunc_f32x4_s': vUnary(257),
  'i32x4.relaxed_trunc_f32x4_u': vUnary(258),
  'i32x4.relaxed_trunc_f64x2_s_zero': vUnary(259),
  'i32x4.relaxed_trunc_f64x2_u_zero': vUnary(260),
  'f32x4.relaxed_madd': vTernary(261),
  'f32x4.relaxed_nmadd': vTernary(262),
  'f64x2.relaxed_madd': vTernary(263),
  'f64x2.relaxed_nmadd': vTernary(264),
  'i8x16.relaxed_laneselect': vTernary(265),
  'i16x8.relaxed_laneselect': vTernary(266),
  'i32x4.relaxed_laneselect': vTernary(267),
  'i64x2.relaxed_laneselect': vTernary(268),
  'f32x4.relaxed_min': vBinary(269),
  'f32x4.relaxed_max': vBinary(270),
  'f64x2.relaxed_min': vBinary(271),
  'f64x2.relaxed_max': vBinary(272),
  'i16x8.relaxed_q15mulr_s': vBinary(273),
  'i16x8.relaxed_dot_i8x16_i7x16_s': vBinary(274),
  'i32x4.relaxed_dot_i8x16_i7x16_add_s': vTernary(275),
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

export interface SimdMemoryAccessRow {
  readonly subopcode: number
  /** Base-two logarithm of the access width; the natural and maximum allowed alignment. */
  readonly widthLog2: number
  readonly kind: 'load' | 'store'
}

const simdLoad = (subopcode: number, widthLog2: number): SimdMemoryAccessRow => ({
  subopcode,
  widthLog2,
  kind: 'load',
})

export const simdMemoryAccessOps: Record<Instr.SimdMemoryAccessMnemonic, SimdMemoryAccessRow> = {
  'v128.load': simdLoad(0, 4),
  'v128.load8x8_s': simdLoad(1, 3),
  'v128.load8x8_u': simdLoad(2, 3),
  'v128.load16x4_s': simdLoad(3, 3),
  'v128.load16x4_u': simdLoad(4, 3),
  'v128.load32x2_s': simdLoad(5, 3),
  'v128.load32x2_u': simdLoad(6, 3),
  'v128.load8_splat': simdLoad(7, 0),
  'v128.load16_splat': simdLoad(8, 1),
  'v128.load32_splat': simdLoad(9, 2),
  'v128.load64_splat': simdLoad(10, 3),
  'v128.load32_zero': simdLoad(92, 2),
  'v128.load64_zero': simdLoad(93, 3),
  'v128.store': { subopcode: 11, widthLog2: 4, kind: 'store' },
}

export interface SimdLaneMemoryRow {
  readonly subopcode: number
  readonly widthLog2: number
  readonly laneCount: number
  readonly kind: 'load' | 'store'
}

export const simdLaneMemoryOps: Record<Instr.SimdLaneMemoryMnemonic, SimdLaneMemoryRow> = {
  'v128.load8_lane': { subopcode: 84, widthLog2: 0, laneCount: 16, kind: 'load' },
  'v128.load16_lane': { subopcode: 85, widthLog2: 1, laneCount: 8, kind: 'load' },
  'v128.load32_lane': { subopcode: 86, widthLog2: 2, laneCount: 4, kind: 'load' },
  'v128.load64_lane': { subopcode: 87, widthLog2: 3, laneCount: 2, kind: 'load' },
  'v128.store8_lane': { subopcode: 88, widthLog2: 0, laneCount: 16, kind: 'store' },
  'v128.store16_lane': { subopcode: 89, widthLog2: 1, laneCount: 8, kind: 'store' },
  'v128.store32_lane': { subopcode: 90, widthLog2: 2, laneCount: 4, kind: 'store' },
  'v128.store64_lane': { subopcode: 91, widthLog2: 3, laneCount: 2, kind: 'store' },
}

export interface SimdLaneRow {
  readonly subopcode: number
  readonly laneCount: number
  readonly scalar: ValType.ValType
  readonly kind: 'extract' | 'replace'
}

const lane = (
  subopcode: number,
  laneCount: number,
  scalar: ValType.ValType,
  kind: 'extract' | 'replace',
): SimdLaneRow => ({ subopcode, laneCount, scalar, kind })

export const simdLaneOps: Record<Instr.SimdLaneMnemonic, SimdLaneRow> = {
  'i8x16.extract_lane_s': lane(21, 16, i32, 'extract'),
  'i8x16.extract_lane_u': lane(22, 16, i32, 'extract'),
  'i8x16.replace_lane': lane(23, 16, i32, 'replace'),
  'i16x8.extract_lane_s': lane(24, 8, i32, 'extract'),
  'i16x8.extract_lane_u': lane(25, 8, i32, 'extract'),
  'i16x8.replace_lane': lane(26, 8, i32, 'replace'),
  'i32x4.extract_lane': lane(27, 4, i32, 'extract'),
  'i32x4.replace_lane': lane(28, 4, i32, 'replace'),
  'i64x2.extract_lane': lane(29, 2, i64, 'extract'),
  'i64x2.replace_lane': lane(30, 2, i64, 'replace'),
  'f32x4.extract_lane': lane(31, 4, f32, 'extract'),
  'f32x4.replace_lane': lane(32, 4, f32, 'replace'),
  'f64x2.extract_lane': lane(33, 2, f64, 'extract'),
  'f64x2.replace_lane': lane(34, 2, f64, 'replace'),
}

export interface AtomicAccessRow {
  readonly subopcode: number
  readonly valType: ValType.ValType
  /** Atomic accesses require exactly this alignment. */
  readonly widthLog2: number
  readonly kind: 'load' | 'store' | 'rmw' | 'cmpxchg' | 'wait' | 'notify'
}

const atomicRow = (
  subopcode: number,
  valType: ValType.ValType,
  widthLog2: number,
  kind: AtomicAccessRow['kind'],
): AtomicAccessRow => ({ subopcode, valType, widthLog2, kind })

export const atomicAccessOps: Record<Instr.AtomicAccessMnemonic, AtomicAccessRow> = {
  'memory.atomic.notify': atomicRow(0, i32, 2, 'notify'),
  'memory.atomic.wait32': atomicRow(1, i32, 2, 'wait'),
  'memory.atomic.wait64': atomicRow(2, i64, 3, 'wait'),
  'i32.atomic.load': atomicRow(16, i32, 2, 'load'),
  'i64.atomic.load': atomicRow(17, i64, 3, 'load'),
  'i32.atomic.load8_u': atomicRow(18, i32, 0, 'load'),
  'i32.atomic.load16_u': atomicRow(19, i32, 1, 'load'),
  'i64.atomic.load8_u': atomicRow(20, i64, 0, 'load'),
  'i64.atomic.load16_u': atomicRow(21, i64, 1, 'load'),
  'i64.atomic.load32_u': atomicRow(22, i64, 2, 'load'),
  'i32.atomic.store': atomicRow(23, i32, 2, 'store'),
  'i64.atomic.store': atomicRow(24, i64, 3, 'store'),
  'i32.atomic.store8': atomicRow(25, i32, 0, 'store'),
  'i32.atomic.store16': atomicRow(26, i32, 1, 'store'),
  'i64.atomic.store8': atomicRow(27, i64, 0, 'store'),
  'i64.atomic.store16': atomicRow(28, i64, 1, 'store'),
  'i64.atomic.store32': atomicRow(29, i64, 2, 'store'),
  'i32.atomic.rmw.add': atomicRow(30, i32, 2, 'rmw'),
  'i64.atomic.rmw.add': atomicRow(31, i64, 3, 'rmw'),
  'i32.atomic.rmw8.add_u': atomicRow(32, i32, 0, 'rmw'),
  'i32.atomic.rmw16.add_u': atomicRow(33, i32, 1, 'rmw'),
  'i64.atomic.rmw8.add_u': atomicRow(34, i64, 0, 'rmw'),
  'i64.atomic.rmw16.add_u': atomicRow(35, i64, 1, 'rmw'),
  'i64.atomic.rmw32.add_u': atomicRow(36, i64, 2, 'rmw'),
  'i32.atomic.rmw.sub': atomicRow(37, i32, 2, 'rmw'),
  'i64.atomic.rmw.sub': atomicRow(38, i64, 3, 'rmw'),
  'i32.atomic.rmw8.sub_u': atomicRow(39, i32, 0, 'rmw'),
  'i32.atomic.rmw16.sub_u': atomicRow(40, i32, 1, 'rmw'),
  'i64.atomic.rmw8.sub_u': atomicRow(41, i64, 0, 'rmw'),
  'i64.atomic.rmw16.sub_u': atomicRow(42, i64, 1, 'rmw'),
  'i64.atomic.rmw32.sub_u': atomicRow(43, i64, 2, 'rmw'),
  'i32.atomic.rmw.and': atomicRow(44, i32, 2, 'rmw'),
  'i64.atomic.rmw.and': atomicRow(45, i64, 3, 'rmw'),
  'i32.atomic.rmw8.and_u': atomicRow(46, i32, 0, 'rmw'),
  'i32.atomic.rmw16.and_u': atomicRow(47, i32, 1, 'rmw'),
  'i64.atomic.rmw8.and_u': atomicRow(48, i64, 0, 'rmw'),
  'i64.atomic.rmw16.and_u': atomicRow(49, i64, 1, 'rmw'),
  'i64.atomic.rmw32.and_u': atomicRow(50, i64, 2, 'rmw'),
  'i32.atomic.rmw.or': atomicRow(51, i32, 2, 'rmw'),
  'i64.atomic.rmw.or': atomicRow(52, i64, 3, 'rmw'),
  'i32.atomic.rmw8.or_u': atomicRow(53, i32, 0, 'rmw'),
  'i32.atomic.rmw16.or_u': atomicRow(54, i32, 1, 'rmw'),
  'i64.atomic.rmw8.or_u': atomicRow(55, i64, 0, 'rmw'),
  'i64.atomic.rmw16.or_u': atomicRow(56, i64, 1, 'rmw'),
  'i64.atomic.rmw32.or_u': atomicRow(57, i64, 2, 'rmw'),
  'i32.atomic.rmw.xor': atomicRow(58, i32, 2, 'rmw'),
  'i64.atomic.rmw.xor': atomicRow(59, i64, 3, 'rmw'),
  'i32.atomic.rmw8.xor_u': atomicRow(60, i32, 0, 'rmw'),
  'i32.atomic.rmw16.xor_u': atomicRow(61, i32, 1, 'rmw'),
  'i64.atomic.rmw8.xor_u': atomicRow(62, i64, 0, 'rmw'),
  'i64.atomic.rmw16.xor_u': atomicRow(63, i64, 1, 'rmw'),
  'i64.atomic.rmw32.xor_u': atomicRow(64, i64, 2, 'rmw'),
  'i32.atomic.rmw.xchg': atomicRow(65, i32, 2, 'rmw'),
  'i64.atomic.rmw.xchg': atomicRow(66, i64, 3, 'rmw'),
  'i32.atomic.rmw8.xchg_u': atomicRow(67, i32, 0, 'rmw'),
  'i32.atomic.rmw16.xchg_u': atomicRow(68, i32, 1, 'rmw'),
  'i64.atomic.rmw8.xchg_u': atomicRow(69, i64, 0, 'rmw'),
  'i64.atomic.rmw16.xchg_u': atomicRow(70, i64, 1, 'rmw'),
  'i64.atomic.rmw32.xchg_u': atomicRow(71, i64, 2, 'rmw'),
  'i32.atomic.rmw.cmpxchg': atomicRow(72, i32, 2, 'cmpxchg'),
  'i64.atomic.rmw.cmpxchg': atomicRow(73, i64, 3, 'cmpxchg'),
  'i32.atomic.rmw8.cmpxchg_u': atomicRow(74, i32, 0, 'cmpxchg'),
  'i32.atomic.rmw16.cmpxchg_u': atomicRow(75, i32, 1, 'cmpxchg'),
  'i64.atomic.rmw8.cmpxchg_u': atomicRow(76, i64, 0, 'cmpxchg'),
  'i64.atomic.rmw16.cmpxchg_u': atomicRow(77, i64, 1, 'cmpxchg'),
  'i64.atomic.rmw32.cmpxchg_u': atomicRow(78, i64, 2, 'cmpxchg'),
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
  v128Const: 12,
  i8x16Shuffle: 13,
  refFunc: 0xd2,
  prefixFc: 0xfc,
  prefixSimd: 0xfd,
  prefixAtomic: 0xfe,
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
