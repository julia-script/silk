import * as Binary from '@silk-effect/wasm/Binary'
import * as Builder from '@silk-effect/wasm/Builder'
import * as Data from '@silk-effect/wasm/Data'
import * as ExportActor from '@silk-effect/wasm/Export'
import * as FuncActor from '@silk-effect/wasm/Func'
import * as Global from '@silk-effect/wasm/Global'
import * as Import from '@silk-effect/wasm/Import'
import * as Instr from '@silk-effect/wasm/Instr'
import * as Memory from '@silk-effect/wasm/Memory'
import * as WasmType from '@silk-effect/wasm/Type'
import * as Validate from '@silk-effect/wasm/Validate'
import * as ValType from '@silk-effect/wasm/ValType'
import * as WatText from '@silk-effect/wasm/WatText'
import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import { symbolFor } from './Backend.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as LayoutPlan from './Layout.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as Ownership from './Ownership.js'
import * as Scalar from './Scalar.js'
import * as StandardStreams from './StandardStreams.js'
import * as Target from './Target.js'
import * as Transcendental from './Transcendental.js'
import * as SilkType from './Type.js'

/**
 * The Wasm backend's heap is a size-class free list over one bump region, so a released block
 * returns to the list its size class owns and the next request of that class reuses it. Repeated
 * acquire/release cycles therefore keep a bounded heap for arbitrary interleaved patterns, not
 * only for the nested ones a LIFO unwind would cover. Nothing here schedules, collects, moves, or
 * compacts: release is an O(1) push driven entirely by the owner that consumed the ticket.
 *
 * Memory above the static data and the shadow stack is laid out as
 *
 * ```text
 * [ 65536 .. heapBase )   one i32 free-list head per size class, plus the irregular list
 * [ heapBase ..       )   blocks, each a 16-byte header followed by its payload
 * ```
 *
 * and one block is
 *
 * ```text
 * header+0   payload capacity in bytes
 * header+4   size-class index, or -1 for an irregular block
 * header+8   next free block while the block sits on a free list
 * header+12  reserved, keeping the payload 16-byte aligned
 * ```
 *
 * A block's payload therefore always begins 16 bytes past its header, which is what lets the
 * compiler-private `$context` reclaim-authority lane carry the header address: release needs no
 * lookup structure to find the block a base pointer belongs to.
 *
 * `heapHeaderBytes` is both the header's size and the alignment every block start keeps.
 */
const heapHeaderBytes = 16
/** Size classes hold payload capacities `1 << (4 + index)`, so class 0 is 16 bytes. */
const heapClassShift = 4
const heapClassCount = 27
/** Blocks too large or too over-aligned for a class share one irregular list. */
const heapIrregularClass = heapClassCount
const heapLargestClassBytes = 1 << (heapClassShift + heapClassCount - 1)
/** The free-list head table sits at the first heap page; wasm memory starts it zeroed. */
const heapTableBase = 65536
const heapBase =
  heapTableBase + Math.ceil(((heapIrregularClass + 1) * 4) / heapHeaderBytes) * heapHeaderBytes

/**
 * The shadow stack may not reach the allocator's region.
 *
 * The two bump regions share one linear memory and point at each other: the shadow stack starts at
 * the end of static data and grows up, while the heap starts at the fixed `heapTableBase` and grows
 * up from there. Without a bound between them a deep enough chain of frames walks over the
 * allocator's free-list table and then over live blocks, and nothing traps at the overwrite — the
 * corrupted bytes are read back later, by whichever guard happens to see them first (#134).
 *
 * `memory.grow` cannot stand in for this bound. Growth appends pages above everything, so the stack
 * reaches the heap long before a reservation runs past the memory that is mapped.
 */
const stackLimit = heapTableBase

/**
 * Linear memory opens with a 16-byte hole so that address 0 is never a live object. The backend
 * spends its first word on a status word: a deliberate trap writes the reason there before
 * `unreachable`, so a host that catches the trap can name what happened instead of inferring it
 * from a byte offset. Memory is exported already, and it survives the trap.
 */
export const statusAddress = 0
/** A frame reservation would have crossed out of the shadow stack's region. */
export const statusStackOverflow = 1

/**
 * A second `Backend` implementation emitting WebAssembly through the Silk wasm builder, for the
 * same MIR subset the bootstrap LLVM backend covers: logical scalar/aggregate locals, trapping
 * arithmetic, direct calls, checked replacement, and canonical structured control regions.
 *
 * The backend returns a final-module artifact: `bytes` carries the validated wasm binary and `wat`
 * carries deterministic inspection text. It never masquerades as LLVM bitcode.
 */

/**
 * MIR already publishes a backend-neutral structured DAG. This backend consumes its conditional
 * and loop regions directly: a loop becomes an exit `block` containing a repeat `loop`, and
 * lexical `Repeat`/`Exit` outcomes become exact `br` depths through the active label stack. It
 * never reconstructs source structure from a CFG and never introduces a dispatch loop.
 */

/** The wasm value type every MIR `i32` and `bool` local lowers to. */
const i32 = ValType.i32
const i64 = ValType.i64
const f32 = ValType.f32
const f64 = ValType.f64

const laneValueType = (plan: LayoutPlan.Plan, lane: LayoutPlan.CallingLane): ValType.ValType => {
  if (typeof lane.type !== 'string') return i32
  const scalar = Scalar.find(lane.type)
  if (scalar === undefined) return i32
  if (scalar.category === 'Floating') return scalar.spelling === 'f32' ? f32 : f64
  return Scalar.bits(scalar, plan.target.pointerSize === 4 ? 32 : 64) === 64 ? i64 : i32
}

const laneLoadMnemonic = (
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

const laneStoreMnemonic = (
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

const alignUp = (value: number, alignment: number): number =>
  Math.ceil(value / alignment) * alignment

interface FrameRoot {
  readonly local: number
  readonly offset: number
  readonly type: Exclude<Mir.Type, { readonly _tag: 'EffectBorrow' | 'EffectOutcome' }>
}

interface FramePlan {
  readonly roots: ReadonlyMap<number, FrameRoot>
  /**
   * The roots whose address leaves the frame — borrow formations and borrow-shaped captures.
   * A callee reaches exactly these through the pointer it was handed, so exactly these have to
   * reload after a call. The remaining roots are addressed only by the cleanup sequence that
   * materializes and reloads them itself, and reloading one after a call would overwrite a live
   * value with frame bytes nothing ever wrote.
   */
  readonly escaping: ReadonlySet<number>
  readonly sliceRoots: ReadonlyMap<number, number>
  readonly size: number
  readonly alignment: number
}

const framePlan = (fn: Mir.MirFunction, plan: LayoutPlan.Plan): FramePlan => {
  const formations = Mir.operations(fn).filter(
    (operation): operation is Extract<Mir.Operation, { readonly _tag: 'BeginLoan' }> =>
      operation._tag === 'BeginLoan',
  )
  const escaping = new Set([
    ...formations.flatMap((operation) =>
      operation.sourceType._tag === 'Slice' ||
      fn.localTypes.at(operation.root.ordinal)?._tag === 'Reference'
        ? []
        : [operation.root.ordinal],
    ),
    ...Mir.operations(fn).flatMap((operation) =>
      operation._tag === 'MakeEffect' || operation._tag === 'MakeCallable'
        ? operation.captures.flatMap((capture, ordinal) =>
            (operation._tag === 'MakeEffect'
              ? operation.type.environment.fields.at(ordinal)?.representation === 'Borrow'
              : capture.access === 'Shared' || capture.access === 'Exclusive') &&
            fn.localTypes.at(capture.source.ordinal)?._tag !== 'EffectBorrow'
              ? [capture.source.ordinal]
              : [],
          )
        : [],
    ),
  ])
  const rootOrdinals = new Set([
    ...escaping,
    // A hook-bearing drop passes `&mut self` into its hook, so the owner needs frame storage.
    ...Mir.operations(fn).flatMap((operation) => {
      const dropped =
        operation._tag === 'Drop'
          ? [operation]
          : operation._tag === 'RunEffect' ||
              operation._tag === 'RunEffectValue' ||
              operation._tag === 'RunStaticEffect'
            ? (operation.releases ?? [])
            : []
      return [
        ...dropped.flatMap((release) =>
          Ownership.cleanupHasHook(release.cleanup) ? [release.local.ordinal] : [],
        ),
        ...(operation._tag === 'CloseEffectEntry'
          ? operation.failures.flatMap((failure) =>
              Ownership.cleanupHasHook(failure.cleanup) ? [failure.payload.ordinal] : [],
            )
          : []),
      ]
    }),
  ])
  const roots = new Map<number, FrameRoot>()
  let cursor = 0
  let alignment = 1
  for (const local of [...rootOrdinals].sort((left, right) => left - right)) {
    const type = fn.localTypes.at(local)
    const storage =
      type?._tag === 'EffectValue'
        ? Object.freeze({ size: type.environment.size, alignment: type.environment.alignment })
        : type === undefined
          ? undefined
          : LayoutPlan.entry(plan, Mir.semanticType(type))
    if (
      type === undefined ||
      type._tag === 'EffectBorrow' ||
      type._tag === 'EffectOutcome' ||
      storage === undefined
    ) {
      throw new RangeError(`Wasm frame lost address-taken root %${local}`)
    }
    cursor = alignUp(cursor, storage.alignment)
    roots.set(local, Object.freeze({ local, offset: cursor, type }))
    cursor += storage.size
    alignment = Math.max(alignment, storage.alignment)
  }
  return Object.freeze({
    roots,
    escaping,
    sliceRoots: new Map(
      formations.map((operation) => [operation.destination.ordinal, operation.root.ordinal]),
    ),
    size: alignUp(cursor, alignment),
    alignment,
  })
}

/** Non-trapping comparisons map straight onto wasm's `i32` relational operators. */
const comparisons: Readonly<Partial<Record<Mir.BinaryOperator, Instr.PlainMnemonic>>> =
  Object.freeze({
    Equals: 'i32.eq',
    NotEquals: 'i32.ne',
    LessThan: 'i32.lt_s',
    LessOrEqual: 'i32.le_s',
    GreaterThan: 'i32.gt_s',
    GreaterOrEqual: 'i32.ge_s',
  })

const unsignedComparisons: Readonly<Partial<Record<Mir.BinaryOperator, Instr.PlainMnemonic>>> =
  Object.freeze({
    Equals: 'i32.eq',
    NotEquals: 'i32.ne',
    LessThan: 'i32.lt_u',
    LessOrEqual: 'i32.le_u',
    GreaterThan: 'i32.gt_u',
    GreaterOrEqual: 'i32.ge_u',
  })

/**
 * Wasm's `i32.div_s` and `i32.rem_s` already trap on a zero divisor and on `MIN / -1`, matching
 * MIR's trapping division exactly — no guard expansion is needed, unlike the LLVM backend's
 * explicit compare-and-branch sequence.
 */
const divisions: Readonly<Partial<Record<Mir.BinaryOperator, Instr.PlainMnemonic>>> = Object.freeze(
  {
    Divide: 'i32.div_s',
    Remainder: 'i32.rem_s',
  },
)

const unsignedDivisions: Readonly<Partial<Record<Mir.BinaryOperator, Instr.PlainMnemonic>>> =
  Object.freeze({ Divide: 'i32.div_u', Remainder: 'i32.rem_u' })

const i64Comparisons: Readonly<Partial<Record<Mir.BinaryOperator, Instr.PlainMnemonic>>> =
  Object.freeze({
    Equals: 'i64.eq',
    NotEquals: 'i64.ne',
    LessThan: 'i64.lt_s',
    LessOrEqual: 'i64.le_s',
    GreaterThan: 'i64.gt_s',
    GreaterOrEqual: 'i64.ge_s',
  })

const unsignedI64Comparisons: Readonly<Partial<Record<Mir.BinaryOperator, Instr.PlainMnemonic>>> =
  Object.freeze({
    Equals: 'i64.eq',
    NotEquals: 'i64.ne',
    LessThan: 'i64.lt_u',
    LessOrEqual: 'i64.le_u',
    GreaterThan: 'i64.gt_u',
    GreaterOrEqual: 'i64.ge_u',
  })

const i64Divisions: Readonly<Partial<Record<Mir.BinaryOperator, Instr.PlainMnemonic>>> =
  Object.freeze({ Divide: 'i64.div_s', Remainder: 'i64.rem_s' })

const unsignedI64Divisions: Readonly<Partial<Record<Mir.BinaryOperator, Instr.PlainMnemonic>>> =
  Object.freeze({ Divide: 'i64.div_u', Remainder: 'i64.rem_u' })

/**
 * Wasm's `i32.add`, `i32.sub`, and `i32.mul` wrap on overflow, but MIR specifies that signed
 * overflow traps. Each is emitted as the wrapping operation followed by an inline overflow check
 * over the operands and the wrapped result, trapping through `unreachable` when it fires.
 *
 * The checks use only the wrapped result and the operands, so no 64-bit widening is needed:
 *
 * - `add`: overflow iff the operands share a sign that the result does not — `(l ^ r) >= 0 &&
 *   (l ^ result) < 0`.
 * - `sub`: overflow iff the operands differ in sign and the result's sign differs from the
 *   left — `(l ^ r) < 0 && (l ^ result) < 0`.
 * - `mul`: verified by dividing back — `result / r == l`, guarded on `r == 0` (never an
 *   overflow) and on the `l == MIN && r == -1` case that would trap the check's own division.
 */
type OverflowShape = 'Add' | 'Subtract' | 'Multiply'

/**
 * Emits `l op r` with its overflow check, leaving the checked result on the stack. Operands are
 * read from `left`/`right` locals rather than the stack so they can be re-read by the check.
 */
const checkedArithmetic = (
  shape: OverflowShape,
  left: number,
  right: number,
  scratch: number,
  bits: 32 | 64 = 32,
): ReadonlyArray<Instr.Instr> => {
  const prefix = bits === 64 ? 'i64' : 'i32'
  const constant = (input: bigint): Instr.Instr =>
    bits === 64 ? Instr.i64Const(input) : Instr.i32Const(Number(input))
  const wrapped: Instr.PlainMnemonic =
    shape === 'Add' ? `${prefix}.add` : shape === 'Subtract' ? `${prefix}.sub` : `${prefix}.mul`
  const compute = [
    Instr.localGet(left),
    Instr.localGet(right),
    Instr.op(wrapped),
    Instr.localSet(scratch),
  ]

  // `overflowed` leaves one i32 boolean on the stack: 1 when the operation overflowed.
  const overflowed: ReadonlyArray<Instr.Instr> =
    shape === 'Multiply'
      ? [
          // r == 0 can never overflow; otherwise l == MIN && r == -1 overflows, and every other
          // case overflows exactly when dividing the result back does not recover `l`.
          Instr.localGet(right),
          Instr.op(`${prefix}.eqz`),
          Instr.ifElse(
            Instr.valueBlockType(i32),
            [Instr.i32Const(0)],
            [
              Instr.localGet(left),
              constant(bits === 64 ? -9223372036854775808n : -2147483648n),
              Instr.op(`${prefix}.eq`),
              Instr.localGet(right),
              constant(-1n),
              Instr.op(`${prefix}.eq`),
              Instr.op('i32.and'),
              Instr.ifElse(
                Instr.valueBlockType(i32),
                [Instr.i32Const(1)],
                [
                  Instr.localGet(scratch),
                  Instr.localGet(right),
                  Instr.op(`${prefix}.div_s`),
                  Instr.localGet(left),
                  Instr.op(`${prefix}.ne`),
                ],
              ),
            ],
          ),
        ]
      : [
          // Sign-based check: the operands' relationship to each other, and the result's sign
          // relative to the left operand.
          Instr.localGet(left),
          Instr.localGet(right),
          Instr.op(`${prefix}.xor`),
          constant(0n),
          ...(shape === 'Add' ? [Instr.op(`${prefix}.ge_s`)] : [Instr.op(`${prefix}.lt_s`)]),
          Instr.localGet(left),
          Instr.localGet(scratch),
          Instr.op(`${prefix}.xor`),
          constant(0n),
          Instr.op(`${prefix}.lt_s`),
          Instr.op('i32.and'),
        ]

  return [
    ...compute,
    ...overflowed,
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    Instr.localGet(scratch),
  ]
}

/** Emits checked unsigned i32 arithmetic for the target-word `usize` lane. */
const checkedUnsignedArithmetic = (
  shape: OverflowShape,
  left: number,
  right: number,
  scratch: number,
  bits: 32 | 64 = 32,
): ReadonlyArray<Instr.Instr> => {
  const prefix = bits === 64 ? 'i64' : 'i32'
  const wrapped: Instr.PlainMnemonic =
    shape === 'Add' ? `${prefix}.add` : shape === 'Subtract' ? `${prefix}.sub` : `${prefix}.mul`
  const compute = [
    Instr.localGet(left),
    Instr.localGet(right),
    Instr.op(wrapped),
    Instr.localSet(scratch),
  ]
  const overflowed: ReadonlyArray<Instr.Instr> =
    shape === 'Add'
      ? [Instr.localGet(scratch), Instr.localGet(left), Instr.op(`${prefix}.lt_u`)]
      : shape === 'Subtract'
        ? [Instr.localGet(left), Instr.localGet(right), Instr.op(`${prefix}.lt_u`)]
        : [
            Instr.localGet(right),
            Instr.op(`${prefix}.eqz`),
            Instr.ifElse(
              Instr.valueBlockType(i32),
              [Instr.i32Const(0)],
              [
                Instr.localGet(scratch),
                Instr.localGet(right),
                Instr.op(`${prefix}.div_u`),
                Instr.localGet(left),
                Instr.op(`${prefix}.ne`),
              ],
            ),
          ]
  return [
    ...compute,
    ...overflowed,
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    Instr.localGet(scratch),
  ]
}

/** Checks an i32-lane result against a narrower logical integer range. */
const checkedSubwordArithmetic = (
  shape: OverflowShape,
  left: number,
  right: number,
  scratch: number,
  minimum: bigint,
  maximum: bigint,
  unsigned: boolean,
): ReadonlyArray<Instr.Instr> => {
  const wrapped: Instr.PlainMnemonic =
    shape === 'Add' ? 'i32.add' : shape === 'Subtract' ? 'i32.sub' : 'i32.mul'
  const below = unsigned
    ? [Instr.localGet(scratch), Instr.i32Const(Number(minimum)), Instr.op('i32.lt_u')]
    : [Instr.localGet(scratch), Instr.i32Const(Number(minimum)), Instr.op('i32.lt_s')]
  const above = unsigned
    ? [Instr.localGet(scratch), Instr.i32Const(Number(maximum)), Instr.op('i32.gt_u')]
    : [Instr.localGet(scratch), Instr.i32Const(Number(maximum)), Instr.op('i32.gt_s')]
  return [
    Instr.localGet(left),
    Instr.localGet(right),
    Instr.op(wrapped),
    Instr.localSet(scratch),
    ...below,
    ...above,
    Instr.op('i32.or'),
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    Instr.localGet(scratch),
  ]
}

/** Computes a wrapping arithmetic result into scratch and leaves an i32 overflow flag. */
const checkedArithmeticOutcome = (
  shape: OverflowShape,
  integer: Scalar.IntegerScalar,
  left: number,
  right: number,
  scratch: number,
  pointerBits: 32 | 64,
): ReadonlyArray<Instr.Instr> => {
  const bits = Scalar.bits(integer, pointerBits)
  const laneBits = bits === 64 ? 64 : 32
  const prefix = laneBits === 64 ? 'i64' : 'i32'
  const unsigned = integer.signedness === 'Unsigned'
  const constant = (value: bigint): Instr.Instr =>
    laneBits === 64 ? Instr.i64Const(value) : Instr.i32Const(Number(value))
  const operation: Instr.PlainMnemonic =
    shape === 'Add' ? `${prefix}.add` : shape === 'Subtract' ? `${prefix}.sub` : `${prefix}.mul`
  const compute = [
    Instr.localGet(left),
    Instr.localGet(right),
    Instr.op(operation),
    Instr.localSet(scratch),
  ]
  if (bits < 32) {
    const range = Scalar.range(integer, pointerBits)
    return [
      ...compute,
      Instr.localGet(scratch),
      Instr.i32Const(Number(range.minimum)),
      Instr.op(unsigned ? 'i32.lt_u' : 'i32.lt_s'),
      Instr.localGet(scratch),
      Instr.i32Const(Number(range.maximum)),
      Instr.op(unsigned ? 'i32.gt_u' : 'i32.gt_s'),
      Instr.op('i32.or'),
    ]
  }
  if (unsigned) {
    return [
      ...compute,
      ...(shape === 'Add'
        ? [Instr.localGet(scratch), Instr.localGet(left), Instr.op(`${prefix}.lt_u`)]
        : shape === 'Subtract'
          ? [Instr.localGet(left), Instr.localGet(right), Instr.op(`${prefix}.lt_u`)]
          : [
              Instr.localGet(right),
              Instr.op(`${prefix}.eqz`),
              Instr.ifElse(
                Instr.valueBlockType(i32),
                [Instr.i32Const(0)],
                [
                  Instr.localGet(scratch),
                  Instr.localGet(right),
                  Instr.op(`${prefix}.div_u`),
                  Instr.localGet(left),
                  Instr.op(`${prefix}.ne`),
                ],
              ),
            ]),
    ]
  }
  const minimum = -(1n << BigInt(laneBits - 1))
  return [
    ...compute,
    ...(shape === 'Multiply'
      ? [
          Instr.localGet(right),
          Instr.op(`${prefix}.eqz`),
          Instr.ifElse(
            Instr.valueBlockType(i32),
            [Instr.i32Const(0)],
            [
              Instr.localGet(left),
              constant(minimum),
              Instr.op(`${prefix}.eq`),
              Instr.localGet(right),
              constant(-1n),
              Instr.op(`${prefix}.eq`),
              Instr.op('i32.and'),
              Instr.ifElse(
                Instr.valueBlockType(i32),
                [Instr.i32Const(1)],
                [
                  Instr.localGet(scratch),
                  Instr.localGet(right),
                  Instr.op(`${prefix}.div_s`),
                  Instr.localGet(left),
                  Instr.op(`${prefix}.ne`),
                ],
              ),
            ],
          ),
        ]
      : [
          Instr.localGet(left),
          Instr.localGet(right),
          Instr.op(`${prefix}.xor`),
          constant(0n),
          Instr.op(shape === 'Add' ? `${prefix}.ge_s` : `${prefix}.lt_s`),
          Instr.localGet(left),
          Instr.localGet(scratch),
          Instr.op(`${prefix}.xor`),
          constant(0n),
          Instr.op(`${prefix}.lt_s`),
          Instr.op('i32.and'),
        ]),
  ]
}

const normalizeSubword = (bits: number, signed: boolean): ReadonlyArray<Instr.Instr> => {
  if (bits >= 32) return []
  if (!signed) return [Instr.i32Const(2 ** bits - 1), Instr.op('i32.and')]
  const shift = 32 - bits
  return [Instr.i32Const(shift), Instr.op('i32.shl'), Instr.i32Const(shift), Instr.op('i32.shr_s')]
}

const saturatingSubwordArithmetic = (
  shape: OverflowShape,
  left: number,
  right: number,
  scratch: number,
  minimum: bigint,
  maximum: bigint,
  unsigned: boolean,
): ReadonlyArray<Instr.Instr> => {
  const operation: Instr.PlainMnemonic =
    shape === 'Add' ? 'i32.add' : shape === 'Subtract' ? 'i32.sub' : 'i32.mul'
  const lessThan: Instr.PlainMnemonic = unsigned ? 'i32.lt_u' : 'i32.lt_s'
  const greaterThan: Instr.PlainMnemonic = unsigned ? 'i32.gt_u' : 'i32.gt_s'
  return [
    Instr.localGet(left),
    Instr.localGet(right),
    Instr.op(operation),
    Instr.localSet(scratch),
    Instr.i32Const(Number(minimum)),
    Instr.localGet(scratch),
    Instr.localGet(scratch),
    Instr.i32Const(Number(minimum)),
    Instr.op(lessThan),
    Instr.op('select'),
    Instr.localSet(scratch),
    Instr.i32Const(Number(maximum)),
    Instr.localGet(scratch),
    Instr.localGet(scratch),
    Instr.i32Const(Number(maximum)),
    Instr.op(greaterThan),
    Instr.op('select'),
  ]
}

const wrappingOrBitwise = (
  operator: Mir.BinaryOperator,
  left: number,
  right: number,
  bits: number,
  signed: boolean,
): ReadonlyArray<Instr.Instr> | undefined => {
  const prefix = bits === 64 ? 'i64' : 'i32'
  const operation: Instr.PlainMnemonic | undefined =
    operator === 'BitAnd'
      ? `${prefix}.and`
      : operator === 'BitOr'
        ? `${prefix}.or`
        : operator === 'BitXor'
          ? `${prefix}.xor`
          : operator === 'WrappingAdd'
            ? `${prefix}.add`
            : operator === 'WrappingSubtract'
              ? `${prefix}.sub`
              : operator === 'WrappingMultiply'
                ? `${prefix}.mul`
                : undefined
  if (operation === undefined) return undefined
  return [
    Instr.localGet(left),
    Instr.localGet(right),
    Instr.op(operation),
    ...normalizeSubword(bits, signed),
  ]
}

const shiftOrRotate = (
  operator: Mir.BinaryOperator,
  left: number,
  right: number,
  bits: number,
  signed: boolean,
): ReadonlyArray<Instr.Instr> | undefined => {
  if (
    operator !== 'ShiftLeft' &&
    operator !== 'ShiftRight' &&
    operator !== 'RotateLeft' &&
    operator !== 'RotateRight'
  )
    return undefined
  const prefix = bits === 64 ? 'i64' : 'i32'
  const constant = (value: number): Instr.Instr =>
    bits === 64 ? Instr.i64Const(BigInt(value)) : Instr.i32Const(value)
  const validate = [
    Instr.localGet(right),
    constant(bits),
    Instr.op(`${prefix}.ge_u`),
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
  ]
  if (operator === 'ShiftLeft' || operator === 'ShiftRight') {
    return [
      ...validate,
      Instr.localGet(left),
      Instr.localGet(right),
      Instr.op(operator === 'ShiftLeft' ? `${prefix}.shl` : `${prefix}.shr_${signed ? 's' : 'u'}`),
      ...(operator === 'ShiftLeft' ? normalizeSubword(bits, signed) : []),
    ]
  }
  if (bits >= 32) {
    return [
      ...validate,
      Instr.localGet(left),
      Instr.localGet(right),
      Instr.op(`${prefix}.${operator === 'RotateLeft' ? 'rotl' : 'rotr'}`),
    ]
  }
  const mask = 2 ** bits - 1
  const leftShift = operator === 'RotateLeft' ? 'i32.shl' : 'i32.shr_u'
  const rightShift = operator === 'RotateLeft' ? 'i32.shr_u' : 'i32.shl'
  return [
    ...validate,
    Instr.localGet(left),
    Instr.i32Const(mask),
    Instr.op('i32.and'),
    Instr.localGet(right),
    Instr.op(leftShift),
    Instr.localGet(left),
    Instr.i32Const(mask),
    Instr.op('i32.and'),
    Instr.i32Const(bits),
    Instr.localGet(right),
    Instr.op('i32.sub'),
    Instr.op(rightShift),
    Instr.op('i32.or'),
    ...normalizeSubword(bits, signed),
  ]
}

/**
 * The wasm local layout of one lowered function. MIR locals occupy the first slots — parameters
 * bind to the leading ones, exactly as in MIR — followed by the emission's own scratch slot.
 */
interface Layout {
  /** Holds a checked arithmetic operation's wrapped result while it is being verified. */
  readonly scratch: number
  /** The corresponding scratch lane for 64-bit integer operations. */
  readonly scratch64: number
  readonly scratchF32?: readonly [number, number]
  readonly scratchF64?: readonly [number, number]
  readonly frameBase?: number
  readonly frameEnd?: number
  readonly framePages?: number
  /** Private i32 temporaries used only by suspension-aware definitions. */
  readonly suspensionScratch?: {
    readonly frame: number
    readonly append: number
    readonly next: number
    readonly allocation: readonly [number, number, number, number, number, number, number]
  }
  /** Every local the definition must declare beyond the function's parameters. */
  readonly declared: ReadonlyArray<FuncActor.Local>
  /** Physical wasm locals realizing each logical MIR local's compiler-selected lanes. */
  readonly slots: ReadonlyArray<ReadonlyArray<number>>
  readonly lanes: ReadonlyArray<ReadonlyArray<LayoutPlan.CallingLane>>
  /** The declared value type of every physical local, parameters first, indexed by local index. */
  readonly physicalTypes: ReadonlyArray<ValType.ValType>
  /** Incoming address parameter for each capture represented internally by its loaded value lanes. */
  readonly borrowPointers: ReadonlyMap<number, number>
  readonly types: ReadonlyArray<Mir.Type>
}

const saturatingWideArithmetic = (
  shape: OverflowShape,
  left: number,
  right: number,
  scratch: number,
  bits: 32 | 64,
  unsigned: boolean,
): ReadonlyArray<Instr.Instr> => {
  const prefix = bits === 64 ? 'i64' : 'i32'
  const constant = (value: bigint): Instr.Instr =>
    bits === 64 ? Instr.i64Const(value) : Instr.i32Const(Number(value))
  const minimum = unsigned ? 0n : -(1n << BigInt(bits - 1))
  const maximum = unsigned ? (1n << BigInt(bits)) - 1n : (1n << BigInt(bits - 1)) - 1n
  const operation: Instr.PlainMnemonic =
    shape === 'Add' ? `${prefix}.add` : shape === 'Subtract' ? `${prefix}.sub` : `${prefix}.mul`
  const compute = [
    Instr.localGet(left),
    Instr.localGet(right),
    Instr.op(operation),
    Instr.localSet(scratch),
  ]
  const overflowed: ReadonlyArray<Instr.Instr> = unsigned
    ? shape === 'Add'
      ? [Instr.localGet(scratch), Instr.localGet(left), Instr.op(`${prefix}.lt_u`)]
      : shape === 'Subtract'
        ? [Instr.localGet(left), Instr.localGet(right), Instr.op(`${prefix}.lt_u`)]
        : [
            Instr.localGet(right),
            Instr.op(`${prefix}.eqz`),
            Instr.ifElse(
              Instr.valueBlockType(i32),
              [Instr.i32Const(0)],
              [
                Instr.localGet(scratch),
                Instr.localGet(right),
                Instr.op(`${prefix}.div_u`),
                Instr.localGet(left),
                Instr.op(`${prefix}.ne`),
              ],
            ),
          ]
    : shape === 'Multiply'
      ? [
          Instr.localGet(right),
          Instr.op(`${prefix}.eqz`),
          Instr.ifElse(
            Instr.valueBlockType(i32),
            [Instr.i32Const(0)],
            [
              Instr.localGet(left),
              constant(minimum),
              Instr.op(`${prefix}.eq`),
              Instr.localGet(right),
              constant(-1n),
              Instr.op(`${prefix}.eq`),
              Instr.op('i32.and'),
              Instr.ifElse(
                Instr.valueBlockType(i32),
                [Instr.i32Const(1)],
                [
                  Instr.localGet(scratch),
                  Instr.localGet(right),
                  Instr.op(`${prefix}.div_s`),
                  Instr.localGet(left),
                  Instr.op(`${prefix}.ne`),
                ],
              ),
            ],
          ),
        ]
      : [
          Instr.localGet(left),
          Instr.localGet(right),
          Instr.op(`${prefix}.xor`),
          constant(0n),
          Instr.op(shape === 'Add' ? `${prefix}.ge_s` : `${prefix}.lt_s`),
          Instr.localGet(left),
          Instr.localGet(scratch),
          Instr.op(`${prefix}.xor`),
          constant(0n),
          Instr.op(`${prefix}.lt_s`),
          Instr.op('i32.and'),
        ]
  const saturation = unsigned
    ? [constant(shape === 'Subtract' ? minimum : maximum)]
    : [
        constant(minimum),
        constant(maximum),
        Instr.localGet(left),
        ...(shape === 'Multiply' ? [Instr.localGet(right), Instr.op(`${prefix}.xor`)] : []),
        constant(0n),
        Instr.op(`${prefix}.lt_s`),
        Instr.op('select'),
      ]
  return [...compute, ...saturation, Instr.localGet(scratch), ...overflowed, Instr.op('select')]
}

const emitIntegerBinaryValue = (
  operator: Mir.BinaryOperator,
  integer: Scalar.IntegerScalar,
  left: number,
  right: number,
  layout: Layout,
  pointerBits: 32 | 64,
): ReadonlyArray<Instr.Instr> => {
  const bits = Scalar.bits(integer, pointerBits)
  const unsigned = integer.signedness === 'Unsigned'
  const comparison =
    bits === 64
      ? (unsigned ? unsignedI64Comparisons : i64Comparisons)[operator]
      : (unsigned ? unsignedComparisons : comparisons)[operator]
  if (comparison !== undefined)
    return [Instr.localGet(left), Instr.localGet(right), Instr.op(comparison)]

  const division =
    bits === 64
      ? (unsigned ? unsignedI64Divisions : i64Divisions)[operator]
      : (unsigned ? unsignedDivisions : divisions)[operator]
  if (division !== undefined) {
    const result = [Instr.localGet(left), Instr.localGet(right), Instr.op(division)]
    if (bits >= 32 || operator !== 'Divide') return result
    const range = Scalar.range(integer, pointerBits)
    return [
      ...result,
      Instr.localSet(layout.scratch),
      Instr.localGet(layout.scratch),
      Instr.i32Const(Number(range.minimum)),
      Instr.op(unsigned ? 'i32.lt_u' : 'i32.lt_s'),
      Instr.localGet(layout.scratch),
      Instr.i32Const(Number(range.maximum)),
      Instr.op(unsigned ? 'i32.gt_u' : 'i32.gt_s'),
      Instr.op('i32.or'),
      Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
      Instr.localGet(layout.scratch),
    ]
  }

  const simple = wrappingOrBitwise(operator, left, right, bits, !unsigned)
  if (simple !== undefined) return simple
  const shifted = shiftOrRotate(operator, left, right, bits, !unsigned)
  if (shifted !== undefined) return shifted

  const shape: OverflowShape | undefined =
    operator === 'Add' || operator === 'SaturatingAdd'
      ? 'Add'
      : operator === 'Subtract' || operator === 'SaturatingSubtract'
        ? 'Subtract'
        : operator === 'Multiply' || operator === 'SaturatingMultiply'
          ? 'Multiply'
          : undefined
  if (shape === undefined) throw new RangeError(`Wasm integer operation ${operator} is unavailable`)
  const saturating = operator.startsWith('Saturating')
  if (bits < 32) {
    const range = Scalar.range(integer, pointerBits)
    return saturating
      ? saturatingSubwordArithmetic(
          shape,
          left,
          right,
          layout.scratch,
          range.minimum,
          range.maximum,
          unsigned,
        )
      : checkedSubwordArithmetic(
          shape,
          left,
          right,
          layout.scratch,
          range.minimum,
          range.maximum,
          unsigned,
        )
  }
  const laneBits = bits === 64 ? 64 : 32
  const scratch = laneBits === 64 ? layout.scratch64 : layout.scratch
  return saturating
    ? saturatingWideArithmetic(shape, left, right, scratch, laneBits, unsigned)
    : (unsigned ? checkedUnsignedArithmetic : checkedArithmetic)(
        shape,
        left,
        right,
        scratch,
        laneBits,
      )
}

const emitIntegerConversionValue = (
  source: Scalar.IntegerScalar,
  target: Scalar.IntegerScalar,
  input: number,
  pointerBits: 32 | 64,
): ReadonlyArray<Instr.Instr> => {
  const sourceBits = Scalar.bits(source, pointerBits)
  const targetBits = Scalar.bits(target, pointerBits)
  const sourceRange = Scalar.range(source, pointerBits)
  const targetRange = Scalar.range(target, pointerBits)
  const sourcePrefix = sourceBits === 64 ? 'i64' : 'i32'
  const sourceConstant = (value: bigint): Instr.Instr =>
    sourceBits === 64 ? Instr.i64Const(value) : Instr.i32Const(Number(value))
  const checks: Array<Instr.Instr> = []
  if (targetRange.minimum > sourceRange.minimum) {
    checks.push(
      Instr.localGet(input),
      sourceConstant(targetRange.minimum),
      Instr.op(`${sourcePrefix}.lt_${source.signedness === 'Signed' ? 's' : 'u'}`),
      Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    )
  }
  if (targetRange.maximum < sourceRange.maximum) {
    checks.push(
      Instr.localGet(input),
      sourceConstant(targetRange.maximum),
      Instr.op(`${sourcePrefix}.gt_${source.signedness === 'Signed' ? 's' : 'u'}`),
      Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    )
  }
  const conversion: ReadonlyArray<Instr.Instr> =
    sourceBits < 64 && targetBits === 64
      ? [Instr.op(`i64.extend_i32_${source.signedness === 'Signed' ? 's' : 'u'}`)]
      : sourceBits === 64 && targetBits < 64
        ? [Instr.op('i32.wrap_i64')]
        : []
  return [
    ...checks,
    Instr.localGet(input),
    ...conversion,
    ...normalizeSubword(targetBits, target.signedness === 'Signed'),
  ]
}

interface MemoryContext {
  readonly memory: Memory.Memory
  readonly stackPointer: Global.Global
  /** Where the shadow stack starts, so a report can rewind the pointer to it. */
  readonly stackBase: number
  /**
   * The first address the shadow stack may not reach, or `undefined` when this module has no heap
   * for it to run into and only the wrap and `memory.grow` guards apply.
   */
  readonly stackLimit: number | undefined
  readonly heapPointer: Global.Global
  readonly frame: FramePlan
  readonly plan: LayoutPlan.Plan
  readonly staticOffsets: ReadonlyMap<string, number>
  readonly standardWrite?: FuncActor.Func
  /** `(bytes, alignment) -> payload address`, answering 0 when the request cannot be served. */
  readonly heapAllocate?: FuncActor.Func
  /** `(header address) -> ()`, returning one block to its size class. Ignores a null header. */
  readonly heapRelease?: FuncActor.Func
}

/**
 * The body of the synthesized allocator. It serves a request from the head of the matching free
 * list when that block fits, and otherwise carves a fresh block off the bump region, growing
 * memory only when the region runs past what is already mapped.
 */
const heapAllocateBody = (
  memory: Memory.Memory,
  heapPointer: Global.Global,
): ReadonlyArray<Instr.Instr> => {
  const [bytes, alignment, align, klass, capacity, listAddress, block, payload, cursor] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8,
  ]
  const load = (address: number, offset = 0): ReadonlyArray<Instr.Instr> => [
    Instr.localGet(address),
    Instr.memoryAccess('i32.load', memory, { offset }),
  ]
  const store = (
    address: number,
    value: ReadonlyArray<Instr.Instr>,
    offset = 0,
  ): ReadonlyArray<Instr.Instr> => [
    Instr.localGet(address),
    ...value,
    Instr.memoryAccess('i32.store', memory, { offset }),
  ]
  /** Pages spanning `[0, cursor)`, as the existing bump growth check computed them. */
  const pagesForCursor: ReadonlyArray<Instr.Instr> = [
    Instr.localGet(cursor),
    Instr.i32Const(1),
    Instr.op('i32.sub'),
    Instr.i32Const(16),
    Instr.op('i32.shr_u'),
    Instr.i32Const(1),
    Instr.op('i32.add'),
  ]
  const refuse: ReadonlyArray<Instr.Instr> = [Instr.i32Const(0), Instr.op('return')]
  return [
    // align = max(alignment, 16). A block start is always 16-aligned, and a zero alignment — which
    // `Layout` construction already rejects — must never widen the mask into every address.
    Instr.i32Const(heapHeaderBytes),
    Instr.localGet(alignment),
    Instr.localGet(alignment),
    Instr.i32Const(heapHeaderBytes),
    Instr.op('i32.lt_u'),
    Instr.op('select'),
    Instr.localSet(align),
    // A request is classified when the block's own 16-byte alignment already satisfies it and its
    // capacity fits the largest class. Everything else shares the irregular list.
    Instr.localGet(align),
    Instr.i32Const(heapHeaderBytes),
    Instr.op('i32.eq'),
    Instr.localGet(bytes),
    Instr.i32Const(heapLargestClassBytes),
    Instr.op('i32.le_u'),
    Instr.op('i32.and'),
    Instr.ifElse(
      Instr.emptyBlockType,
      [
        // klass = bytes <= 16 ? 0 : ceil(log2(bytes)) - 4, capacity = 1 << (klass + 4)
        Instr.i32Const(0),
        Instr.i32Const(32),
        Instr.localGet(bytes),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.op('i32.clz'),
        Instr.op('i32.sub'),
        Instr.i32Const(heapClassShift),
        Instr.op('i32.sub'),
        Instr.localGet(bytes),
        Instr.i32Const(1 << heapClassShift),
        Instr.op('i32.le_u'),
        Instr.op('select'),
        Instr.localSet(klass),
        Instr.i32Const(1),
        Instr.localGet(klass),
        Instr.i32Const(heapClassShift),
        Instr.op('i32.add'),
        Instr.op('i32.shl'),
        Instr.localSet(capacity),
        Instr.localGet(klass),
        Instr.i32Const(2),
        Instr.op('i32.shl'),
        Instr.i32Const(heapTableBase),
        Instr.op('i32.add'),
        Instr.localSet(listAddress),
      ],
      [
        Instr.i32Const(-1),
        Instr.localSet(klass),
        Instr.localGet(bytes),
        Instr.localSet(capacity),
        Instr.i32Const(heapTableBase + heapIrregularClass * 4),
        Instr.localSet(listAddress),
      ],
    ),
    // Reuse the head of the list when it serves the request. Every classified block in a list has
    // the class's capacity and a 16-aligned payload, so only an irregular head needs measuring.
    ...load(listAddress),
    Instr.localTee(block),
    Instr.ifElse(
      Instr.emptyBlockType,
      [
        Instr.localGet(klass),
        Instr.i32Const(0),
        Instr.op('i32.ge_s'),
        ...load(block),
        Instr.localGet(bytes),
        Instr.op('i32.ge_u'),
        Instr.localGet(block),
        Instr.i32Const(heapHeaderBytes),
        Instr.op('i32.add'),
        Instr.localGet(align),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.op('i32.and'),
        Instr.op('i32.eqz'),
        Instr.op('i32.and'),
        Instr.op('i32.or'),
        Instr.ifElse(
          Instr.emptyBlockType,
          [
            ...store(listAddress, load(block, 8)),
            ...store(block, [Instr.i32Const(0)], 8),
            Instr.localGet(block),
            Instr.i32Const(heapHeaderBytes),
            Instr.op('i32.add'),
            Instr.op('return'),
          ],
          [],
        ),
      ],
      [],
    ),
    // Carve a fresh block. The bump cursor stays 16-aligned so every block start is, which is what
    // keeps a classified payload's 16-byte alignment an invariant rather than a coincidence.
    Instr.globalGet(heapPointer),
    Instr.i32Const(heapHeaderBytes - 1),
    Instr.op('i32.add'),
    Instr.i32Const(-heapHeaderBytes),
    Instr.op('i32.and'),
    Instr.localSet(cursor),
    Instr.localGet(cursor),
    Instr.i32Const(heapHeaderBytes),
    Instr.op('i32.add'),
    Instr.localGet(align),
    Instr.i32Const(1),
    Instr.op('i32.sub'),
    Instr.op('i32.add'),
    Instr.i32Const(0),
    Instr.localGet(align),
    Instr.op('i32.sub'),
    Instr.op('i32.and'),
    Instr.localTee(payload),
    Instr.localGet(cursor),
    Instr.op('i32.lt_u'),
    Instr.ifElse(Instr.emptyBlockType, refuse, []),
    Instr.localGet(payload),
    Instr.i32Const(heapHeaderBytes),
    Instr.op('i32.sub'),
    Instr.localSet(block),
    Instr.localGet(payload),
    Instr.localGet(capacity),
    Instr.op('i32.add'),
    Instr.i32Const(heapHeaderBytes - 1),
    Instr.op('i32.add'),
    Instr.localTee(cursor),
    Instr.localGet(payload),
    Instr.op('i32.lt_u'),
    Instr.ifElse(Instr.emptyBlockType, refuse, []),
    Instr.localGet(cursor),
    Instr.i32Const(-heapHeaderBytes),
    Instr.op('i32.and'),
    Instr.localSet(cursor),
    ...pagesForCursor,
    Instr.memorySize(memory),
    Instr.op('i32.gt_u'),
    Instr.ifElse(
      Instr.emptyBlockType,
      [
        ...pagesForCursor,
        Instr.memorySize(memory),
        Instr.op('i32.sub'),
        Instr.memoryGrow(memory),
        Instr.i32Const(-1),
        Instr.op('i32.eq'),
        Instr.ifElse(Instr.emptyBlockType, refuse, []),
      ],
      [],
    ),
    Instr.localGet(cursor),
    Instr.globalSet(heapPointer),
    ...store(block, [Instr.localGet(capacity)]),
    ...store(block, [Instr.localGet(klass)], 4),
    ...store(block, [Instr.i32Const(0)], 8),
    ...store(block, [Instr.i32Const(0)], 12),
    Instr.localGet(payload),
  ]
}

/**
 * The body of the synthesized release. It pushes one block onto the head of its size class's free
 * list and returns. A null header is a no-op, which is what lets a union's conditional cleanup
 * select the inactive case's reclaim context to zero instead of branching around the call — the
 * same shortcut the LLVM backend takes through libc `free`.
 */
const heapReleaseBody = (memory: Memory.Memory): ReadonlyArray<Instr.Instr> => {
  const [header, listAddress] = [0, 1]
  const klass: ReadonlyArray<Instr.Instr> = [
    Instr.localGet(header),
    Instr.memoryAccess('i32.load', memory, { offset: 4 }),
  ]
  return [
    Instr.localGet(header),
    Instr.op('i32.eqz'),
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('return')], []),
    // An unsigned comparison folds the irregular block's -1 class in with any out-of-range index,
    // so a header can never steer the push past the end of the head table.
    ...klass,
    Instr.i32Const(heapIrregularClass),
    ...klass,
    Instr.i32Const(heapClassCount),
    Instr.op('i32.lt_u'),
    Instr.op('select'),
    Instr.i32Const(2),
    Instr.op('i32.shl'),
    Instr.i32Const(heapTableBase),
    Instr.op('i32.add'),
    Instr.localSet(listAddress),
    Instr.localGet(header),
    Instr.localGet(listAddress),
    Instr.memoryAccess('i32.load', memory),
    Instr.memoryAccess('i32.store', memory, { offset: 8 }),
    Instr.localGet(listAddress),
    Instr.localGet(header),
    Instr.memoryAccess('i32.store', memory),
  ]
}

/** Local names reach the `name` custom section, so release builds declare them unnamed. */
const layoutOf = (
  fn: Mir.MirFunction,
  plan: LayoutPlan.Plan,
  frame: FramePlan,
  debug: boolean,
  suspension = false,
): Layout => {
  const named = (type: ValType.ValType, name: string): FuncActor.Local =>
    debug ? { type, name } : { type }
  const localLaneName = (type: Mir.Type, ordinal: number, lane: number): string => {
    const semantic = Mir.semanticType(type)
    if (SilkType.isString(semantic))
      return `string${ordinal}_${lane === 0 ? 'utf8_address' : 'byte_length'}`
    if (SilkType.isSlice(semantic) && SilkType.equals(semantic.element, 'u8'))
      return `bytes${ordinal}_${lane === 0 ? 'address' : 'length'}`
    return `local${ordinal}_${lane}`
  }
  const logicalLanes = (type: Mir.Type): ReadonlyArray<LayoutPlan.CallingLane> => {
    if (type._tag === 'EffectBorrow') {
      const shape = LayoutPlan.callingShape(plan, type.type)
      if (shape === undefined) throw new RangeError('Wasm backend lost a borrowed calling shape')
      return shape.lanes
    }
    if (type._tag === 'EffectValue')
      return LayoutPlan.effectEnvironmentLanes(plan, type.environment)
    if (type._tag === 'CallableValue' && type.storage !== undefined) {
      const shape = LayoutPlan.callingShape(plan, type.storage.type)
      if (shape === undefined) throw new RangeError('Wasm backend lost a stored callable shape')
      return shape.lanes
    }
    if (type._tag === 'CallableValue')
      return type.environment === undefined
        ? Object.freeze([])
        : LayoutPlan.callableEnvironmentLanes(plan, type.environment)
    const shape = LayoutPlan.callingShape(plan, Mir.semanticType(type))
    if (shape === undefined) throw new RangeError('Wasm backend lost a logical calling shape')
    return shape.lanes
  }
  const lanes = fn.localTypes.map(logicalLanes)
  const signatureLaneCount = (type: Mir.Type): number =>
    type._tag === 'EffectBorrow' ? 1 : logicalLanes(type).length
  const parameterLaneCount = fn.localTypes
    .slice(0, fn.parameterCount)
    .reduce((total, type) => total + signatureLaneCount(type), 0)
  const slots: Array<ReadonlyArray<number>> = []
  const borrowPointers = new Map<number, number>()
  // The declared type of every physical local, in index order. Parameters are not part of
  // `declared`, so they are recorded here as they are assigned and the declared locals append
  // afterwards in the same order the definition emits them.
  const parameterTypes: Array<ValType.ValType> = []
  let parameterPhysical = 0
  for (let ordinal = 0; ordinal < fn.parameterCount; ordinal += 1) {
    const type = fn.localTypes.at(ordinal)
    if (type === undefined) throw new RangeError(`Wasm backend lost parameter %${ordinal}`)
    if (type._tag === 'EffectBorrow') {
      borrowPointers.set(ordinal, parameterPhysical)
      parameterPhysical += 1
      parameterTypes.push(i32)
      slots.push(Object.freeze([]))
    } else {
      const logical = logicalLanes(type)
      for (const logicalLane of logical) parameterTypes.push(laneValueType(plan, logicalLane))
      slots.push(Object.freeze(logical.map(() => parameterPhysical++)))
    }
  }
  let physical = parameterLaneCount
  const declared: Array<FuncActor.Local> = []
  for (const [ordinal] of borrowPointers) {
    const type = fn.localTypes.at(ordinal)
    if (type === undefined) throw new RangeError(`Wasm backend lost borrow %${ordinal}`)
    const logical = logicalLanes(type)
    const localSlots = logical.map(() => physical++)
    slots[ordinal] = Object.freeze(localSlots)
    for (const [lane, logicalLane] of logical.entries())
      declared.push(named(laneValueType(plan, logicalLane), `borrow${ordinal}_${lane}`))
  }
  for (let ordinal = fn.parameterCount; ordinal < fn.localTypes.length; ordinal += 1) {
    const type = fn.localTypes.at(ordinal)
    if (type === undefined) throw new RangeError(`Wasm backend lost local %${ordinal}`)
    const logical = logicalLanes(type)
    const localSlots = logical.map(() => physical++)
    slots.push(Object.freeze(localSlots))
    for (const [lane, logicalLane] of logical.entries()) {
      declared.push(named(laneValueType(plan, logicalLane), localLaneName(type, ordinal, lane)))
    }
  }
  const scratch = physical
  declared.push(named(i32, 'scratch'))
  const needsScratch64 =
    lanes.some((localLanes) =>
      localLanes.some(
        (lane) =>
          typeof lane.type === 'string' &&
          Scalar.bits(Scalar.find(lane.type) ?? Scalar.defaultInteger, 32) === 64,
      ),
    ) || Mir.operations(fn).some((operation) => operation._tag === 'FloatTranscendental')
  let nextInternal = physical + 1
  const scratch64 = needsScratch64 ? nextInternal : scratch
  if (needsScratch64) declared.push(named(i64, 'scratch64'))
  if (needsScratch64) nextInternal += 1
  const needsScratchF32 = Mir.operations(fn).some(
    (operation) => operation._tag === 'FloatTranscendental' && operation.sourceType._tag === 'f32',
  )
  const scratchF32 = needsScratchF32 ? ([nextInternal, nextInternal + 1] as const) : undefined
  if (scratchF32 !== undefined) {
    declared.push(named(f32, 'scratch_f32_a'), named(f32, 'scratch_f32_b'))
    nextInternal += 2
  }
  const needsScratchF64 = Mir.operations(fn).some(
    (operation) => operation._tag === 'FloatTranscendental' && operation.sourceType._tag === 'f64',
  )
  const scratchF64 = needsScratchF64 ? ([nextInternal, nextInternal + 1] as const) : undefined
  if (scratchF64 !== undefined) {
    declared.push(named(f64, 'scratch_f64_a'), named(f64, 'scratch_f64_b'))
    nextInternal += 2
  }
  const internalCount = nextInternal - physical
  const frameBase = frame.roots.size === 0 ? undefined : physical + internalCount
  const frameEnd = frame.roots.size === 0 ? undefined : physical + internalCount + 1
  const framePages = frame.roots.size === 0 ? undefined : physical + internalCount + 2
  if (frameBase !== undefined && frameEnd !== undefined && framePages !== undefined) {
    declared.push(named(i32, 'frame_base'), named(i32, 'frame_end'), named(i32, 'frame_pages'))
  }
  const suspensionScratch = suspension
    ? (() => {
        const first = parameterLaneCount + declared.length
        return Object.freeze({
          frame: first,
          append: first + 1,
          next: first + 2,
          allocation: Object.freeze([
            first + 3,
            first + 4,
            first + 5,
            first + 6,
            first + 7,
            first + 8,
            first + 9,
          ] as const),
        })
      })()
    : undefined
  if (suspensionScratch !== undefined)
    declared.push(
      named(i32, 'suspend_frame'),
      named(i32, 'suspend_append'),
      named(i32, 'suspend_next'),
      named(i32, 'suspend_allocation_tag'),
      named(i32, 'suspend_allocation_base'),
      named(i32, 'suspend_allocation_bytes'),
      named(i32, 'suspend_allocation_alignment'),
      named(i32, 'suspend_allocation_reclaim'),
      named(i32, 'suspend_allocation_context'),
      named(i32, 'suspend_allocation_active'),
    )
  return {
    scratch,
    scratch64,
    ...(scratchF32 === undefined ? {} : { scratchF32 }),
    ...(scratchF64 === undefined ? {} : { scratchF64 }),
    declared: Object.freeze(declared),
    slots: Object.freeze(slots),
    lanes: Object.freeze(lanes),
    physicalTypes: Object.freeze([...parameterTypes, ...declared.map((local) => local.type)]),
    borrowPointers,
    types: fn.localTypes,
    ...(frameBase === undefined ? {} : { frameBase }),
    ...(frameEnd === undefined ? {} : { frameEnd }),
    ...(framePages === undefined ? {} : { framePages }),
    ...(suspensionScratch === undefined ? {} : { suspensionScratch }),
  }
}

const suspensionPointKey = (point: Mir.SuspensionPointId): string =>
  `${Instances.keyText(point.owner)}\u0000${point.sourceId}\u0000${point.spanStart}\u0000${point.spanEnd}\u0000${point.ordinal}`

const suspensionOperationInputs = (
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
  >,
): ReadonlyArray<Mir.LocalId> =>
  operation._tag === 'RunEffect'
    ? operation.arguments
    : Object.freeze([operation.effect, ...operation.arguments])

const matchesSuspensionOperation = (
  candidate: Mir.Operation,
  expected: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
  >,
): boolean =>
  candidate === expected ||
  ((candidate._tag === 'RunEffect' ||
    candidate._tag === 'RunEffectValue' ||
    candidate._tag === 'ReifyEffect') &&
    candidate._tag === expected._tag &&
    candidate.destination.ordinal === expected.destination.ordinal &&
    candidate.outcome.ordinal === expected.outcome.ordinal)

interface PackedWasmLane {
  readonly offset: number
  readonly type: ValType.ValType
}

const packWasmLanes = (
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

interface WasmSuspensionFunctionContext {
  readonly regions: ReadonlyMap<Mir.Operation, Mir.SuspensionRegion>
  readonly originate: (
    region: Extract<Mir.SuspensionRegion, { readonly _tag: 'SuspendEffectRegion' }>,
  ) => ReadonlyArray<Instr.Instr>
  readonly relay: (
    region: Extract<Mir.SuspensionRegion, { readonly _tag: 'RunSuspendableEffectRegion' }>,
  ) => ReadonlyArray<Instr.Instr>
}

/** Emits one MIR operation as a wasm instruction sequence writing its destination local. */
const emitOperation = (
  operation: Mir.Operation,
  layout: Layout,
  plan: LayoutPlan.Plan,
  resolve: (
    target: DeclarationIndex.CanonicalId,
    typeArguments: ReadonlyArray<SilkType.GenericArgument>,
  ) => FuncActor.Func,
  memory: MemoryContext | undefined,
  suspension?: WasmSuspensionFunctionContext,
  skipInvocation = false,
): ReadonlyArray<Instr.Instr> => {
  const slots = (local: Mir.LocalId): ReadonlyArray<number> => layout.slots.at(local.ordinal) ?? []
  const scalar = (local: Mir.LocalId): number => {
    const selected = slots(local)
    const first = selected.at(0)
    if (selected.length !== 1 || first === undefined) {
      throw new RangeError(`Wasm backend expected scalar local %${local.ordinal}`)
    }
    return first
  }
  // A union payload slot is as wide as its widest member, so a narrower member's lane and the slot
  // holding it can be different wasm value types. The bits are the same bits either way — only the
  // container differs — so a transfer between them normalizes to the integer of its own width,
  // adjusts the width, and reinterprets into the target. Widening is unsigned so that narrowing
  // back yields exactly the bits that went in, whatever the member's own signedness.
  const laneBridge = (from: ValType.ValType, to: ValType.ValType): ReadonlyArray<Instr.Instr> => {
    if (from === to) return []
    const toInteger =
      from === f32
        ? [Instr.op('i32.reinterpret_f32')]
        : from === f64
          ? [Instr.op('i64.reinterpret_f64')]
          : []
    const sourceBits = from === i64 || from === f64 ? 64 : 32
    const targetBits = to === i64 || to === f64 ? 64 : 32
    const resize =
      sourceBits === targetBits
        ? []
        : targetBits === 64
          ? [Instr.op('i64.extend_i32_u')]
          : [Instr.op('i32.wrap_i64')]
    const fromInteger =
      to === f32
        ? [Instr.op('f32.reinterpret_i32')]
        : to === f64
          ? [Instr.op('f64.reinterpret_i64')]
          : []
    return [...toInteger, ...resize, ...fromInteger]
  }
  /** Moves one physical local into another, bridging the value types when they differ. */
  const transfer = (source: number, target: number): ReadonlyArray<Instr.Instr> => {
    const from = layout.physicalTypes.at(source)
    const to = layout.physicalTypes.at(target)
    return [
      Instr.localGet(source),
      ...(from === undefined || to === undefined ? [] : laneBridge(from, to)),
      Instr.localSet(target),
    ]
  }
  /** The zero one lane's own value type spells, for a union slot no member of this arm fills. */
  const zeroFor = (target: number): Instr.Instr => {
    const type = layout.physicalTypes.at(target)
    if (type === i64) return Instr.i64Const(0n)
    if (type === f32) return Instr.f32Const(0)
    if (type === f64) return Instr.f64Const(0)
    return Instr.i32Const(0)
  }
  const copy = (source: ReadonlyArray<number>, destination: ReadonlyArray<number>) => {
    if (source.length !== destination.length) {
      throw new RangeError('Wasm backend cannot copy mismatched logical lane bundles')
    }
    return source.flatMap((value, index) => {
      const target = destination.at(index)
      return target === undefined ? [] : [...transfer(value, target)]
    })
  }
  const frameAddress = (offset: number): ReadonlyArray<Instr.Instr> => {
    if (layout.frameBase === undefined) throw new RangeError('Wasm frame has no base local')
    return [Instr.localGet(layout.frameBase), Instr.i32Const(offset), Instr.op('i32.add')]
  }
  const requireMemory = (): MemoryContext => {
    if (memory === undefined) throw new RangeError('Wasm raw storage requires private memory')
    return memory
  }
  const aggregateFieldOffset = (type: SilkType.Type, name: string): number => {
    const entry = LayoutPlan.entry(plan, type)
    if (entry?.representation._tag !== 'Aggregate') {
      throw new RangeError(`Wasm raw storage lost aggregate layout ${SilkType.encode(type)}`)
    }
    const field = entry.representation.fields.find((candidate) => candidate.name === name)
    if (field === undefined) throw new RangeError(`Wasm raw storage lost field ${name}`)
    return field.offset
  }
  /**
   * A lane-less load reads a four-byte compiler field — a count, a base pointer, a union tag.
   * A load that lands in an element lane must pass that lane, so the access is exactly as wide
   * as the element: a narrow lane shares its four-byte window with its neighbours, and a fixed
   * `i32.load` would read theirs along with its own.
   */
  const loadAt = (
    address: number,
    offset = 0,
    lane?: LayoutPlan.CallingLane,
  ): ReadonlyArray<Instr.Instr> => {
    const context = requireMemory()
    return [
      Instr.localGet(address),
      Instr.memoryAccess(
        lane === undefined ? 'i32.load' : laneLoadMnemonic(plan, lane),
        context.memory,
        { offset },
      ),
    ]
  }
  const storeAt = (
    address: number,
    value: number,
    offset = 0,
    lane?: LayoutPlan.CallingLane,
  ): ReadonlyArray<Instr.Instr> => {
    const context = requireMemory()
    return [
      Instr.localGet(address),
      Instr.localGet(value),
      Instr.memoryAccess(
        lane === undefined ? 'i32.store' : laneStoreMnemonic(plan, lane),
        context.memory,
        { offset },
      ),
    ]
  }
  const materializeRoot = (root: Mir.LocalId): ReadonlyArray<Instr.Instr> => {
    if (memory === undefined) throw new RangeError('Wasm slice has no private memory')
    const planned = memory.frame.roots.get(root.ordinal)
    if (planned === undefined) throw new RangeError(`Wasm frame lost root %${root.ordinal}`)
    const rootSlots = slots(root)
    const rootLanes = layout.lanes.at(root.ordinal) ?? []
    return rootLanes.flatMap((lane, ordinal) => {
      const offset = LayoutPlan.laneOffset(memory.plan, Mir.semanticType(planned.type), lane.path)
      const source = rootSlots.at(ordinal)
      if (offset === undefined || source === undefined) {
        throw new RangeError(`Wasm frame lost lane ${ordinal} of %${root.ordinal}`)
      }
      return [
        ...frameAddress(planned.offset + offset),
        Instr.localGet(source),
        Instr.memoryAccess(laneStoreMnemonic(memory.plan, lane), memory.memory),
      ]
    })
  }
  const reloadRoot = (root: number): ReadonlyArray<Instr.Instr> => {
    if (memory === undefined) return []
    const planned = memory.frame.roots.get(root)
    const rootLanes = layout.lanes.at(root) ?? []
    const rootSlots = layout.slots.at(root) ?? []
    if (planned === undefined) return []
    return rootLanes.flatMap((lane, ordinal) => {
      const offset = LayoutPlan.laneOffset(memory.plan, Mir.semanticType(planned.type), lane.path)
      const destination = rootSlots.at(ordinal)
      if (offset === undefined || destination === undefined) {
        throw new RangeError(`Wasm frame lost reload lane ${ordinal} of %${root}`)
      }
      return [
        ...frameAddress(planned.offset + offset),
        Instr.memoryAccess(laneLoadMnemonic(memory.plan, lane), memory.memory),
        Instr.localSet(destination),
      ]
    })
  }
  /**
   * Reloads the roots a call could have written through the pointer it was handed. Roots that
   * never gave their address away keep their authoritative value in slots, so they stay put.
   */
  const reloadEscapingRoots = (): ReadonlyArray<Instr.Instr> =>
    [...(memory?.frame.escaping ?? [])].sort((left, right) => left - right).flatMap(reloadRoot)
  const callableCaptureRange = (
    cleanup: Extract<Ownership.CleanupPlan, { readonly _tag: 'CallableCleanup' }>,
    capture: number,
  ): LayoutPlan.CallableCaptureRange => {
    if (cleanup.environment._tag !== 'CallableEnvironmentIdentity')
      throw new RangeError('Wasm callable cleanup lost its specialized environment')
    const range = LayoutPlan.callableCaptureRange(plan, cleanup.environment.identity, capture)
    if (range !== undefined) return range
    throw new RangeError('Wasm callable cleanup lost an owned capture lane')
  }
  /**
   * Runs every Drop hook one cleanup plan invokes: the owner materializes to its frame root,
   * each hook receives the address of its (possibly nested) value, and the owner's slots
   * reload afterward. The blocks the plan still owns are reclaimed separately, after the hooks.
   */
  const hookReleaseInstructions = (
    cleanup: Ownership.CleanupPlan,
    local: Mir.LocalId,
  ): ReadonlyArray<Instr.Instr> => {
    if (!Ownership.cleanupHasHook(cleanup)) return []
    if (memory === undefined) throw new RangeError('Wasm hook cleanup has no private memory')
    const planned = memory.frame.roots.get(local.ordinal)
    if (planned === undefined) throw new RangeError('Wasm hook cleanup lost its frame root')
    const walk = (plan_: Ownership.CleanupPlan, byteOffset: number): ReadonlyArray<Instr.Instr> => {
      switch (plan_._tag) {
        case 'HookCleanup':
          return [
            ...frameAddress(planned.offset + byteOffset),
            Instr.call(resolve(plan_.hook, plan_.typeArguments)),
            ...walk(plan_.inner, byteOffset),
          ]
        case 'CallableCleanup':
          return plan_.slots.flatMap((slot) =>
            walk(slot.cleanup, byteOffset + callableCaptureRange(plan_, slot.ordinal).byteOffset),
          )
        case 'StructCleanup': {
          const entry = LayoutPlan.entry(memory.plan, plan_.type)
          const representation = entry?.representation
          if (representation?._tag !== 'Aggregate') return []
          return plan_.fields.flatMap((field) => {
            if (!Ownership.cleanupHasHook(field.cleanup)) return []
            const layoutField = representation.fields.find(
              (candidate) =>
                candidate.id.ordinal === field.field.ordinal &&
                candidate.id.struct.ordinal === field.field.struct.ordinal &&
                candidate.id.struct.sourceId === field.field.struct.sourceId,
            )
            return layoutField === undefined
              ? []
              : walk(field.cleanup, byteOffset + layoutField.offset)
          })
        }
        case 'ArrayCleanup': {
          if (!Ownership.cleanupHasHook(plan_.element)) return []
          const entry = LayoutPlan.entry(memory.plan, plan_.type)
          const representation = entry?.representation
          if (representation?._tag !== 'Repeated') return []
          return Array.from({ length: plan_.length }, (_, index) =>
            walk(plan_.element, byteOffset + index * representation.stride),
          ).flat()
        }
        case 'UnionCleanup': {
          if (plan_.cases.every((entry) => !Ownership.cleanupHasHook(entry.cleanup))) return []
          const entry = LayoutPlan.entry(memory.plan, plan_.type)
          const representation = entry?.representation
          if (representation?._tag !== 'Union') return []
          // Only the live case owns the payload, so each hook-bearing case runs guarded on the
          // union's own tag. The tag sits at the union's base and the payload at its offset.
          return plan_.cases.flatMap((caseEntry) =>
            Ownership.cleanupHasHook(caseEntry.cleanup)
              ? [
                  ...frameAddress(planned.offset + byteOffset),
                  Instr.memoryAccess('i32.load', memory.memory),
                  Instr.i32Const(caseEntry.ordinal),
                  Instr.op('i32.eq'),
                  Instr.ifElse(
                    Instr.emptyBlockType,
                    walk(caseEntry.cleanup, byteOffset + representation.payloadOffset),
                    [],
                  ),
                ]
              : [],
          )
        }
        default:
          return []
      }
    }
    const localType = layout.types.at(local.ordinal)
    if (cleanup._tag === 'EffectCleanup' && localType?._tag === 'EffectValue') {
      const environmentOffsets = (
        environment: Extract<LayoutPlan.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }>,
        base = 0,
      ): ReadonlyArray<number> =>
        environment.fields.flatMap((field) => {
          if (field.representation === 'Borrow') return [base + field.offset]
          if (field.effectIdentity !== undefined) {
            const nested = memory.plan.effectEnvironments.find(
              (candidate) =>
                candidate._tag === 'EffectEnvironment' &&
                Instances.effectIdentity(candidate.instance, candidate.site) ===
                  field.effectIdentity,
            )
            return nested?._tag === 'EffectEnvironment'
              ? environmentOffsets(nested, base + field.offset)
              : []
          }
          const shape = LayoutPlan.callingShape(memory.plan, field.type)
          return (
            shape?.lanes.flatMap((lane) => {
              const offset = LayoutPlan.laneOffset(memory.plan, field.type, lane.path)
              return offset === undefined ? [] : [base + field.offset + offset]
            }) ?? []
          )
        })
      const offsets = environmentOffsets(localType.environment)
      const rootSlots = slots(local)
      const rootLanes = layout.lanes.at(local.ordinal) ?? []
      const stores = rootLanes.flatMap((lane, ordinal) => {
        const offset = offsets.at(ordinal)
        const source = rootSlots.at(ordinal)
        if (offset === undefined || source === undefined)
          throw new RangeError('Wasm Effect cleanup lost an environment lane')
        return [
          ...frameAddress(planned.offset + offset),
          Instr.localGet(source),
          Instr.memoryAccess(laneStoreMnemonic(memory.plan, lane), memory.memory),
        ]
      })
      const hooks = cleanup.slots.flatMap((slot) => {
        const field = localType.environment.fields.at(slot.ordinal)
        return field === undefined ? [] : walk(slot.cleanup, field.offset)
      })
      return [...stores, ...hooks]
    }
    return [...materializeRoot(local), ...walk(cleanup, 0), ...reloadRoot(local.ordinal)]
  }
  const hookReleaseAtAddress = (
    cleanup: Ownership.CleanupPlan,
    address: number,
  ): ReadonlyArray<Instr.Instr> => {
    if (!Ownership.cleanupHasHook(cleanup)) return []
    if (memory === undefined) throw new RangeError('Wasm slot cleanup has no private memory')
    const addressAt = (byteOffset: number): ReadonlyArray<Instr.Instr> => [
      Instr.localGet(address),
      ...(byteOffset === 0 ? [] : [Instr.i32Const(byteOffset), Instr.op('i32.add')]),
    ]
    const walk = (plan_: Ownership.CleanupPlan, byteOffset: number): ReadonlyArray<Instr.Instr> => {
      switch (plan_._tag) {
        case 'HookCleanup':
          return [
            ...addressAt(byteOffset),
            Instr.call(resolve(plan_.hook, plan_.typeArguments)),
            ...walk(plan_.inner, byteOffset),
          ]
        case 'CallableCleanup':
          return plan_.slots.flatMap((slot) =>
            walk(slot.cleanup, byteOffset + callableCaptureRange(plan_, slot.ordinal).byteOffset),
          )
        case 'StructCleanup': {
          const entry = LayoutPlan.entry(memory.plan, plan_.type)
          const representation = entry?.representation
          if (representation?._tag !== 'Aggregate') return []
          return plan_.fields.flatMap((field) => {
            if (!Ownership.cleanupHasHook(field.cleanup)) return []
            const layoutField = representation.fields.find(
              (candidate) =>
                candidate.id.ordinal === field.field.ordinal &&
                candidate.id.struct.ordinal === field.field.struct.ordinal &&
                candidate.id.struct.sourceId === field.field.struct.sourceId,
            )
            return layoutField === undefined
              ? []
              : walk(field.cleanup, byteOffset + layoutField.offset)
          })
        }
        case 'ArrayCleanup': {
          if (!Ownership.cleanupHasHook(plan_.element)) return []
          const entry = LayoutPlan.entry(memory.plan, plan_.type)
          const representation = entry?.representation
          if (representation?._tag !== 'Repeated') return []
          return Array.from({ length: plan_.length }, (_, index) =>
            walk(plan_.element, byteOffset + index * representation.stride),
          ).flat()
        }
        case 'UnionCleanup': {
          if (plan_.cases.every((entry) => !Ownership.cleanupHasHook(entry.cleanup))) return []
          const entry = LayoutPlan.entry(memory.plan, plan_.type)
          const representation = entry?.representation
          if (representation?._tag !== 'Union') return []
          // Only the live case owns the payload, so each hook-bearing case runs guarded on the
          // union's own tag. The tag sits at the union's base and the payload at its offset.
          return plan_.cases.flatMap((caseEntry) =>
            Ownership.cleanupHasHook(caseEntry.cleanup)
              ? [
                  ...addressAt(byteOffset),
                  Instr.memoryAccess('i32.load', memory.memory),
                  Instr.i32Const(caseEntry.ordinal),
                  Instr.op('i32.eq'),
                  Instr.ifElse(
                    Instr.emptyBlockType,
                    walk(caseEntry.cleanup, byteOffset + representation.payloadOffset),
                    [],
                  ),
                ]
              : [],
          )
        }
        default:
          return []
      }
    }
    return walk(cleanup, 0)
  }
  const semanticLanesOf = (type: SilkType.Type): ReadonlyArray<LayoutPlan.CallingLane> => {
    const shape = LayoutPlan.callingShape(plan, type)
    if (shape === undefined) {
      throw new RangeError(`Wasm cleanup lost calling shape for ${SilkType.encode(type)}`)
    }
    return shape.lanes
  }
  const requireRelease = (): FuncActor.Func => {
    const release = memory?.heapRelease
    if (release === undefined) throw new RangeError('Wasm reclaim lost its heap release helper')
    return release
  }
  /**
   * Returns every block one cleanup plan still owns, reading each `$context` lane out of the
   * owner's own slots. Hooks are not this walk's business — `hookReleaseInstructions` has already
   * run them and reloaded the slots, so a context read here is the post-hook one, which is the
   * order the LLVM backend's single lane-driven walk produces too.
   */
  const reclaimReleaseInstructions = (
    cleanup: Ownership.CleanupPlan,
    values: ReadonlyArray<number>,
  ): ReadonlyArray<Instr.Instr> => {
    if (!Ownership.cleanupReclaims(cleanup)) return []
    switch (cleanup._tag) {
      case 'AllocationCleanup':
      case 'RawBufferCleanup': {
        const context = values.at(4)
        if (context === undefined) {
          throw new RangeError('Wasm allocation cleanup lost its reclaim context')
        }
        return [Instr.localGet(context), Instr.call(requireRelease())]
      }
      case 'HookCleanup':
        return reclaimReleaseInstructions(cleanup.inner, values)
      case 'CallableCleanup':
        return cleanup.slots.flatMap((slot) => {
          const range = callableCaptureRange(cleanup, slot.ordinal)
          return reclaimReleaseInstructions(
            slot.cleanup,
            values.slice(range.laneOffset, range.laneOffset + range.laneCount),
          )
        })
      case 'EffectCleanup':
        return cleanup.slots.flatMap((slot) =>
          reclaimReleaseInstructions(
            slot.cleanup,
            values.slice(slot.laneOffset, slot.laneOffset + slot.laneCount),
          ),
        )
      case 'StructCleanup': {
        const lanes = semanticLanesOf(cleanup.type)
        return cleanup.fields.flatMap((field) =>
          reclaimReleaseInstructions(
            field.cleanup,
            lanes.flatMap((lane, ordinal) => {
              const first = lane.path.at(0)
              const value = values.at(ordinal)
              return first !== undefined &&
                first._tag === 'FieldId' &&
                value !== undefined &&
                first.ordinal === field.field.ordinal &&
                first.struct.ordinal === field.field.struct.ordinal &&
                first.struct.sourceId === field.field.struct.sourceId
                ? [value]
                : []
            }),
          ),
        )
      }
      case 'ArrayCleanup': {
        const lanes = semanticLanesOf(cleanup.type)
        return Array.from({ length: cleanup.length }, (_, index) =>
          reclaimReleaseInstructions(
            cleanup.element,
            lanes.flatMap((lane, ordinal) => {
              const first = lane.path.at(0)
              const value = values.at(ordinal)
              return first !== undefined &&
                first._tag === 'ElementSelector' &&
                first.index === index &&
                value !== undefined
                ? [value]
                : []
            }),
          ),
        ).flat()
      }
      case 'UnionCleanup': {
        const shape = LayoutPlan.callingShape(plan, cleanup.type)
        const tag = values.at(0)
        if (shape === undefined || tag === undefined) {
          throw new RangeError('Wasm union cleanup lost its shape')
        }
        // Only the live case owns the payload, so each reclaiming case runs guarded on the tag.
        return cleanup.cases.flatMap((caseEntry) => {
          const physical = LayoutPlan.memberFieldSlots(shape, caseEntry.member, [])
          const memberLanes = semanticLanesOf(caseEntry.member)
          if (physical === undefined || physical.length !== memberLanes.length) return []
          // A member's lane can sit in a physically wider union slot. Reclaim reads slots directly
          // and has no local of the member's own type to bridge into, so a widened lane cannot be
          // released here. Refusing to emit keeps that loud: releasing nothing would leak the
          // member's blocks silently, and this is the only shape for which the release is missing.
          const selected = physical.flatMap((ordinal, index) => {
            const value = values.at(ordinal)
            const physicalLane = shape.lanes.at(ordinal)
            const memberLane = memberLanes.at(index)
            if (value === undefined || physicalLane === undefined || memberLane === undefined) {
              return []
            }
            if (laneValueType(plan, physicalLane) !== laneValueType(plan, memberLane)) {
              throw new RangeError(
                'Wasm union cleanup cannot release a member whose lane is narrower than its union slot',
              )
            }
            return [value]
          })
          if (selected.length !== memberLanes.length) return []
          const inner = reclaimReleaseInstructions(caseEntry.cleanup, selected)
          return inner.length === 0
            ? []
            : [
                Instr.localGet(tag),
                Instr.i32Const(caseEntry.ordinal),
                Instr.op('i32.eq'),
                Instr.ifElse(Instr.emptyBlockType, inner, []),
              ]
        })
      }
      default:
        return []
    }
  }
  /** The same reclaim walk for a value the backend only holds the address of. */
  const reclaimReleaseAtAddress = (
    cleanup: Ownership.CleanupPlan,
    address: number,
    byteOffset = 0,
  ): ReadonlyArray<Instr.Instr> => {
    if (!Ownership.cleanupReclaims(cleanup)) return []
    if (memory === undefined) throw new RangeError('Wasm slot reclaim has no private memory')
    switch (cleanup._tag) {
      case 'AllocationCleanup':
      case 'RawBufferCleanup': {
        const contextOffset = SilkType.isRawBuffer(cleanup.type)
          ? aggregateFieldOffset(cleanup.type, '$allocation') +
            aggregateFieldOffset(SilkType.allocation, '$context')
          : aggregateFieldOffset(SilkType.allocation, '$context')
        return [...loadAt(address, byteOffset + contextOffset), Instr.call(requireRelease())]
      }
      case 'HookCleanup':
        return reclaimReleaseAtAddress(cleanup.inner, address, byteOffset)
      case 'CallableCleanup':
        return cleanup.slots.flatMap((slot) =>
          reclaimReleaseAtAddress(
            slot.cleanup,
            address,
            byteOffset + callableCaptureRange(cleanup, slot.ordinal).byteOffset,
          ),
        )
      case 'StructCleanup': {
        const representation = LayoutPlan.entry(memory.plan, cleanup.type)?.representation
        if (representation?._tag !== 'Aggregate') return []
        return cleanup.fields.flatMap((field) => {
          const layoutField = representation.fields.find(
            (candidate) =>
              candidate.id.ordinal === field.field.ordinal &&
              candidate.id.struct.ordinal === field.field.struct.ordinal &&
              candidate.id.struct.sourceId === field.field.struct.sourceId,
          )
          return layoutField === undefined
            ? []
            : reclaimReleaseAtAddress(field.cleanup, address, byteOffset + layoutField.offset)
        })
      }
      case 'ArrayCleanup': {
        const representation = LayoutPlan.entry(memory.plan, cleanup.type)?.representation
        if (representation?._tag !== 'Repeated') return []
        return Array.from({ length: cleanup.length }, (_, index) =>
          reclaimReleaseAtAddress(
            cleanup.element,
            address,
            byteOffset + index * representation.stride,
          ),
        ).flat()
      }
      case 'UnionCleanup': {
        const representation = LayoutPlan.entry(memory.plan, cleanup.type)?.representation
        if (representation?._tag !== 'Union') return []
        return cleanup.cases.flatMap((caseEntry) => {
          const inner = reclaimReleaseAtAddress(
            caseEntry.cleanup,
            address,
            byteOffset + representation.payloadOffset,
          )
          return inner.length === 0
            ? []
            : [
                ...loadAt(address, byteOffset),
                Instr.i32Const(caseEntry.ordinal),
                Instr.op('i32.eq'),
                Instr.ifElse(Instr.emptyBlockType, inner, []),
              ]
        })
      }
      default:
        return []
    }
  }
  /**
   * One owned value's complete release: its Drop hooks, then the blocks its reclaim tickets still
   * hold. Both halves are conditional on the plan actually carrying them, so a bare allocation
   * drop — which invokes no hook at all — still emits its reclaim.
   */
  const releaseInstructions = (
    cleanup: Ownership.CleanupPlan,
    local: Mir.LocalId,
  ): ReadonlyArray<Instr.Instr> => [
    ...hookReleaseInstructions(cleanup, local),
    ...reclaimReleaseInstructions(cleanup, slots(local)),
  ]
  const releaseAtAddress = (
    cleanup: Ownership.CleanupPlan,
    address: number,
  ): ReadonlyArray<Instr.Instr> => [
    ...hookReleaseAtAddress(cleanup, address),
    ...reclaimReleaseAtAddress(cleanup, address),
  ]
  const flushBorrowRoot = (root: Mir.LocalId): ReadonlyArray<Instr.Instr> => {
    const pointer = layout.borrowPointers.get(root.ordinal)
    if (pointer === undefined) return []
    if (memory === undefined) throw new RangeError('Wasm Effect borrow has no private memory')
    const type = layout.types.at(root.ordinal)
    if (type?._tag !== 'EffectBorrow') throw new RangeError('Wasm Effect borrow lost its type')
    const rootLanes = layout.lanes.at(root.ordinal) ?? []
    const rootSlots = slots(root)
    return rootLanes.flatMap((lane, ordinal) => {
      const offset = LayoutPlan.laneOffset(memory.plan, type.type, lane.path)
      const source = rootSlots.at(ordinal)
      if (offset === undefined || source === undefined)
        throw new RangeError(`Wasm Effect borrow lost lane ${ordinal}`)
      return [
        Instr.localGet(pointer),
        ...(offset === 0 ? [] : [Instr.i32Const(offset), Instr.op('i32.add')]),
        Instr.localGet(source),
        Instr.memoryAccess(laneStoreMnemonic(memory.plan, lane), memory.memory),
      ]
    })
  }
  switch (operation._tag) {
    case 'StaticString': {
      const context = requireMemory()
      const offset = context.staticOffsets.get(operation.data)
      const [address, length] = slots(operation.destination)
      if (offset === undefined || address === undefined || length === undefined) {
        throw new RangeError('Wasm static string lost its data placement or logical lanes')
      }
      return [
        Instr.i32Const(offset),
        Instr.localSet(address),
        Instr.i32Const(operation.byteLength),
        Instr.localSet(length),
      ]
    }
    case 'StringFromUtf8Unchecked':
      return copy(slots(operation.bytes), slots(operation.destination))
    case 'StringUtf8Bytes':
      return copy(slots(operation.string), slots(operation.destination))
    case 'StringByteLength': {
      const length = slots(operation.string).at(1)
      if (length === undefined) throw new RangeError('Wasm string lost its byte-length lane')
      return [Instr.localGet(length), Instr.localSet(scalar(operation.destination))]
    }
    case 'StringEqualsExact': {
      const context = requireMemory()
      const [leftAddress, leftLength] = slots(operation.left)
      const [rightAddress, rightLength] = slots(operation.right)
      const destination = scalar(operation.destination)
      if (
        leftAddress === undefined ||
        leftLength === undefined ||
        rightAddress === undefined ||
        rightLength === undefined
      ) {
        throw new RangeError('Wasm string equality lost its logical lanes')
      }
      const loop = [
        Instr.localGet(layout.scratch),
        Instr.localGet(leftLength),
        Instr.op('i32.ge_u'),
        Instr.brIf(1),
        Instr.localGet(destination),
        Instr.localGet(leftAddress),
        Instr.localGet(layout.scratch),
        Instr.op('i32.add'),
        Instr.memoryAccess('i32.load8_u', context.memory),
        Instr.localGet(rightAddress),
        Instr.localGet(layout.scratch),
        Instr.op('i32.add'),
        Instr.memoryAccess('i32.load8_u', context.memory),
        Instr.op('i32.eq'),
        Instr.op('i32.and'),
        Instr.localSet(destination),
        Instr.localGet(layout.scratch),
        Instr.i32Const(1),
        Instr.op('i32.add'),
        Instr.localSet(layout.scratch),
        Instr.br(0),
      ]
      return [
        Instr.localGet(leftLength),
        Instr.localGet(rightLength),
        Instr.op('i32.eq'),
        Instr.localTee(destination),
        Instr.ifElse(
          Instr.emptyBlockType,
          [
            Instr.i32Const(0),
            Instr.localSet(layout.scratch),
            Instr.block(Instr.emptyBlockType, [Instr.loop(Instr.emptyBlockType, loop)]),
          ],
          [],
        ),
        ...(operation.negated
          ? [Instr.localGet(destination), Instr.op('i32.eqz'), Instr.localSet(destination)]
          : []),
      ]
    }
    case 'ValidateLayout': {
      const bytes = scalar(operation.bytes)
      const alignment = scalar(operation.alignment)
      const destination = slots(operation.destination)
      const tag = destination.at(0)
      const first = destination.at(1)
      const second = destination.at(2)
      if (tag === undefined || first === undefined || second === undefined) {
        throw new RangeError('Wasm layout validation lost its destination lanes')
      }
      const members = operation.type.type.members
      const layoutOrdinal = members.findIndex((member) => SilkType.equals(member, SilkType.layout))
      const invalidOrdinal = members.findIndex((member) =>
        SilkType.equals(member, SilkType.invalidAlignment),
      )
      if (layoutOrdinal < 0 || invalidOrdinal < 0) {
        throw new RangeError('Wasm layout validation lost its union members')
      }
      return [
        // valid = (alignment != 0) & ((alignment & (alignment - 1)) == 0), kept in scratch
        Instr.localGet(alignment),
        Instr.op('i32.eqz'),
        Instr.op('i32.eqz'),
        Instr.localGet(alignment),
        Instr.localGet(alignment),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.op('i32.and'),
        Instr.op('i32.eqz'),
        Instr.op('i32.and'),
        Instr.localSet(layout.scratch),
        // tag = valid ? Layout : InvalidAlignment
        Instr.i32Const(layoutOrdinal),
        Instr.i32Const(invalidOrdinal),
        Instr.localGet(layout.scratch),
        Instr.op('select'),
        Instr.localSet(tag),
        // Layout packs {bytes, alignment}; InvalidAlignment packs {alignment} at slot 0.
        Instr.localGet(bytes),
        Instr.localGet(alignment),
        Instr.localGet(layout.scratch),
        Instr.op('select'),
        Instr.localSet(first),
        Instr.localGet(alignment),
        Instr.i32Const(0),
        Instr.localGet(layout.scratch),
        Instr.op('select'),
        Instr.localSet(second),
        ...destination.slice(3).flatMap((slot) => [Instr.i32Const(0), Instr.localSet(slot)]),
      ]
    }
    case 'RepeatLayout': {
      const layoutSlots = slots(operation.layout)
      const bytes = layoutSlots.at(0)
      const alignment = layoutSlots.at(1)
      const count = scalar(operation.count)
      const destination = slots(operation.destination)
      const tag = destination.at(0)
      const first = destination.at(1)
      const second = destination.at(2)
      if (
        bytes === undefined ||
        alignment === undefined ||
        tag === undefined ||
        first === undefined ||
        second === undefined
      ) {
        throw new RangeError('Wasm repeated layout lost its lanes')
      }
      const members = operation.type.type.members
      const layoutOrdinal = members.findIndex((member) => SilkType.equals(member, SilkType.layout))
      const overflowOrdinal = members.findIndex((member) =>
        SilkType.equals(member, SilkType.layoutOverflow),
      )
      if (layoutOrdinal < 0 || overflowOrdinal < 0) {
        throw new RangeError('Wasm repeated layout lost its union members')
      }
      const maximum = -1 // 0xFFFFFFFF as a signed i32 constant
      return [
        // safeAlignment (temporarily in the tag slot) = alignment == 0 ? 1 : alignment
        Instr.i32Const(1),
        Instr.localGet(alignment),
        Instr.localGet(alignment),
        Instr.op('i32.eqz'),
        Instr.op('select'),
        Instr.localSet(tag),
        // stride (temporarily in the bytes slot): roundUp(bytes, safeAlignment), 0 when alignment == 0
        Instr.i32Const(0),
        Instr.localGet(bytes),
        Instr.localGet(tag),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.op('i32.add'),
        Instr.localGet(tag),
        Instr.op('i32.div_u'),
        Instr.localGet(tag),
        Instr.op('i32.mul'),
        Instr.localGet(alignment),
        Instr.op('i32.eqz'),
        Instr.op('select'),
        Instr.localSet(first),
        // overflow (in scratch) = count != 0 & (bytes > max - (safeAlignment-1) | stride > max / safeCount)
        Instr.i32Const(maximum),
        Instr.localGet(tag),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.op('i32.sub'),
        Instr.localGet(bytes),
        Instr.op('i32.lt_u'),
        Instr.localSet(layout.scratch),
        // safeCount reuses the tag slot now that safeAlignment is no longer needed
        Instr.i32Const(1),
        Instr.localGet(count),
        Instr.localGet(count),
        Instr.op('i32.eqz'),
        Instr.op('select'),
        Instr.localSet(tag),
        Instr.localGet(first),
        Instr.i32Const(maximum),
        Instr.localGet(tag),
        Instr.op('i32.div_u'),
        Instr.op('i32.gt_u'),
        Instr.localGet(layout.scratch),
        Instr.op('i32.or'),
        Instr.localGet(count),
        Instr.op('i32.eqz'),
        Instr.op('i32.eqz'),
        Instr.op('i32.and'),
        Instr.localSet(layout.scratch),
        // bytes out = overflow ? 0 : stride * count
        Instr.i32Const(0),
        Instr.localGet(first),
        Instr.localGet(count),
        Instr.op('i32.mul'),
        Instr.localGet(layout.scratch),
        Instr.op('select'),
        Instr.localSet(first),
        // final tag and passthrough alignment
        Instr.i32Const(overflowOrdinal),
        Instr.i32Const(layoutOrdinal),
        Instr.localGet(layout.scratch),
        Instr.op('select'),
        Instr.localSet(tag),
        Instr.localGet(alignment),
        Instr.localSet(second),
        ...destination.slice(3).flatMap((slot) => [Instr.i32Const(0), Instr.localSet(slot)]),
      ]
    }
    case 'Allocate': {
      const context = requireMemory()
      const [bytes, alignment] = slots(operation.layout)
      const [base, destinationBytes, destinationAlignment, reclaim, rawContext, active] = slots(
        operation.destination,
      )
      if (
        bytes === undefined ||
        alignment === undefined ||
        base === undefined ||
        destinationBytes === undefined ||
        destinationAlignment === undefined ||
        reclaim === undefined ||
        rawContext === undefined ||
        active === undefined
      ) {
        throw new RangeError('Wasm allocation lost its compiler-planned lanes')
      }
      const propagationShape = LayoutPlan.callingShape(plan, operation.propagationType.type)
      if (propagationShape === undefined) {
        throw new RangeError('Wasm allocation lost its failure calling shape')
      }
      const fail = [
        Instr.i32Const(operation.failureTag),
        ...Array.from({ length: propagationShape.laneCount - 1 }, () => Instr.i32Const(0)),
        Instr.op('return'),
      ]
      if (context.heapAllocate === undefined) {
        throw new RangeError('Wasm allocation lost its heap allocator')
      }
      return [
        Instr.localGet(bytes),
        Instr.localGet(alignment),
        Instr.call(context.heapAllocate),
        Instr.localTee(base),
        // No block payload can sit at address zero, so zero is the allocator's whole refusal
        // vocabulary: an unserviceable request leaves through the same typed failure as before.
        Instr.op('i32.eqz'),
        Instr.ifElse(Instr.emptyBlockType, fail, []),
        Instr.localGet(bytes),
        Instr.localSet(destinationBytes),
        Instr.localGet(alignment),
        Instr.localSet(destinationAlignment),
        Instr.i32Const(1),
        Instr.localSet(reclaim),
        // The reclaim-authority lane carries this block's header address. A payload always begins
        // one header past its block, so release recovers the block with a subtraction the backend
        // already knows, and needs no side table keyed by base pointer.
        Instr.localGet(base),
        Instr.i32Const(heapHeaderBytes),
        Instr.op('i32.sub'),
        Instr.localSet(rawContext),
        Instr.i32Const(1),
        Instr.localSet(active),
      ]
    }
    case 'HostWrite': {
      if (memory?.standardWrite === undefined) {
        throw new RangeError('Wasm standard-stream write lost its host import or private memory')
      }
      const [address, length] = slots(operation.bytes)
      if (address === undefined || length === undefined) {
        throw new RangeError('Wasm standard-stream write lost its byte-view lanes')
      }
      const propagationShape = LayoutPlan.callingShape(plan, operation.propagationType.type)
      if (propagationShape === undefined) {
        throw new RangeError('Wasm standard-stream write lost its failure calling shape')
      }
      return [
        Instr.localGet(scalar(operation.stream)),
        Instr.localGet(address),
        Instr.localGet(length),
        Instr.call(memory.standardWrite),
        Instr.i32Const(0),
        Instr.op('i32.ne'),
        Instr.ifElse(
          Instr.emptyBlockType,
          [
            Instr.i32Const(operation.failureTag),
            ...Array.from({ length: propagationShape.laneCount - 1 }, () => Instr.i32Const(0)),
            Instr.op('return'),
          ],
          [],
        ),
      ]
    }
    case 'OsCall':
      throw new RangeError('Target validation allowed a native-only OS operation into Wasm')
    case 'RawBufferFrom': {
      const allocation = slots(operation.allocation)
      const destination = slots(operation.destination)
      const count = scalar(operation.count)
      const bytes = allocation.at(1)
      const alignment = allocation.at(2)
      const destinationCount = destination.at(-1)
      if (
        bytes === undefined ||
        alignment === undefined ||
        destinationCount === undefined ||
        destination.length !== allocation.length + 1
      ) {
        throw new RangeError('Wasm RawBuffer construction lost its planned lanes')
      }
      return [
        Instr.localGet(count),
        Instr.i32Const(operation.stride),
        Instr.op('i32.mul'),
        Instr.localGet(bytes),
        Instr.op('i32.ne'),
        Instr.localGet(alignment),
        Instr.i32Const(operation.elementAlignment),
        Instr.op('i32.ne'),
        Instr.op('i32.or'),
        Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        ...copy(allocation, destination.slice(0, allocation.length)),
        Instr.localGet(count),
        Instr.localSet(destinationCount),
      ]
    }
    case 'RawBufferCount': {
      const address = scalar(operation.buffer)
      const reference = layout.types.at(operation.buffer.ordinal)
      if (reference?._tag !== 'Reference' || !SilkType.isRawBuffer(reference.type.target)) {
        throw new RangeError('Wasm RawBuffer.count lost its referenced buffer type')
      }
      return [
        ...loadAt(address, aggregateFieldOffset(reference.type.target, 'count')),
        Instr.localSet(scalar(operation.destination)),
      ]
    }
    case 'RawBufferSlot': {
      const address = scalar(operation.buffer)
      const index = scalar(operation.index)
      const rawBuffer = SilkType.rawBuffer(operation.element)
      const allocationOffset = aggregateFieldOffset(rawBuffer, '$allocation')
      const baseOffset = allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base')
      const countOffset = aggregateFieldOffset(rawBuffer, 'count')
      const elementLayout = LayoutPlan.entry(plan, operation.element)
      if (elementLayout === undefined)
        throw new RangeError('Wasm RawBuffer.slot lost element layout')
      const stride = alignUp(elementLayout.size, elementLayout.alignment)
      return [
        Instr.localGet(index),
        ...loadAt(address, countOffset),
        Instr.op('i32.ge_u'),
        Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        ...loadAt(address, baseOffset),
        Instr.localGet(index),
        Instr.i32Const(stride),
        Instr.op('i32.mul'),
        Instr.op('i32.add'),
        Instr.localSet(scalar(operation.destination)),
      ]
    }
    case 'RawBufferRead': {
      const address = scalar(operation.buffer)
      const index = scalar(operation.index)
      const rawBuffer = SilkType.rawBuffer(operation.element)
      const allocationOffset = aggregateFieldOffset(rawBuffer, '$allocation')
      const baseOffset = allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base')
      const countOffset = aggregateFieldOffset(rawBuffer, 'count')
      const elementLayout = LayoutPlan.entry(plan, operation.element)
      const shape = LayoutPlan.callingShape(plan, operation.element)
      if (elementLayout === undefined || shape === undefined) {
        throw new RangeError('Wasm RawBuffer.read lost element layout')
      }
      const stride = alignUp(elementLayout.size, elementLayout.alignment)
      const context = requireMemory()
      return [
        Instr.localGet(index),
        ...loadAt(address, countOffset),
        Instr.op('i32.ge_u'),
        Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        ...shape.lanes.flatMap((lane, ordinal) => {
          const destination = slots(operation.destination).at(ordinal)
          const offset = LayoutPlan.laneOffset(plan, operation.element, lane.path)
          if (destination === undefined || offset === undefined) {
            throw new RangeError('Wasm RawBuffer.read lost an element lane')
          }
          return [
            ...loadAt(address, baseOffset),
            Instr.localGet(index),
            Instr.i32Const(stride),
            Instr.op('i32.mul'),
            Instr.op('i32.add'),
            Instr.memoryAccess(laneLoadMnemonic(plan, lane), context.memory, { offset }),
            Instr.localSet(destination),
          ]
        }),
      ]
    }
    case 'RawBufferView': {
      const address = scalar(operation.buffer)
      const offset = scalar(operation.offset)
      const length = scalar(operation.length)
      const destination = slots(operation.destination)
      const destinationAddress = destination.at(0)
      const destinationLength = destination.at(1)
      const rawBuffer = SilkType.rawBuffer(operation.element)
      const allocationOffset = aggregateFieldOffset(rawBuffer, '$allocation')
      const baseOffset = allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base')
      const countOffset = aggregateFieldOffset(rawBuffer, 'count')
      if (destinationAddress === undefined || destinationLength === undefined) {
        throw new RangeError('Wasm RawBuffer view lost its slice lanes')
      }
      return [
        Instr.localGet(offset),
        ...loadAt(address, countOffset),
        Instr.op('i32.gt_u'),
        Instr.localGet(length),
        ...loadAt(address, countOffset),
        Instr.localGet(offset),
        Instr.op('i32.sub'),
        Instr.op('i32.gt_u'),
        Instr.op('i32.or'),
        Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        ...loadAt(address, baseOffset),
        Instr.localGet(offset),
        Instr.i32Const(operation.stride),
        Instr.op('i32.mul'),
        Instr.op('i32.add'),
        Instr.localSet(destinationAddress),
        Instr.localGet(length),
        Instr.localSet(destinationLength),
      ]
    }
    case 'RawBufferCopy': {
      const address = scalar(operation.buffer)
      const offset = scalar(operation.offset)
      const length = scalar(operation.length)
      const sourceLanes = slots(operation.source)
      const sourceAddress = sourceLanes.at(0)
      const sourceLength = sourceLanes.at(1)
      const rawBuffer = SilkType.rawBuffer(operation.element)
      const allocationOffset = aggregateFieldOffset(rawBuffer, '$allocation')
      const baseOffset = allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base')
      const countOffset = aggregateFieldOffset(rawBuffer, 'count')
      if (sourceAddress === undefined || sourceLength === undefined) {
        throw new RangeError('Wasm RawBuffer.copy lost its source slice lanes')
      }
      const context = requireMemory()
      return [
        Instr.localGet(offset),
        ...loadAt(address, countOffset),
        Instr.op('i32.gt_u'),
        Instr.localGet(length),
        ...loadAt(address, countOffset),
        Instr.localGet(offset),
        Instr.op('i32.sub'),
        Instr.op('i32.gt_u'),
        Instr.op('i32.or'),
        Instr.localGet(length),
        Instr.localGet(sourceLength),
        Instr.op('i32.gt_u'),
        Instr.op('i32.or'),
        Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        ...loadAt(address, baseOffset),
        Instr.localGet(offset),
        Instr.i32Const(operation.stride),
        Instr.op('i32.mul'),
        Instr.op('i32.add'),
        Instr.localGet(sourceAddress),
        Instr.localGet(length),
        Instr.i32Const(operation.stride),
        Instr.op('i32.mul'),
        // memory.copy is defined for overlapping ranges, which is the copy intrinsic's contract.
        Instr.memoryCopy(context.memory, context.memory),
      ]
    }
    case 'RawBufferFill': {
      const address = scalar(operation.buffer)
      const offset = scalar(operation.offset)
      const length = scalar(operation.length)
      const value = scalar(operation.value)
      const rawBuffer = SilkType.rawBuffer('u8')
      const allocationOffset = aggregateFieldOffset(rawBuffer, '$allocation')
      const baseOffset = allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base')
      const countOffset = aggregateFieldOffset(rawBuffer, 'count')
      const context = requireMemory()
      return [
        Instr.localGet(offset),
        ...loadAt(address, countOffset),
        Instr.op('i32.gt_u'),
        Instr.localGet(length),
        ...loadAt(address, countOffset),
        Instr.localGet(offset),
        Instr.op('i32.sub'),
        Instr.op('i32.gt_u'),
        Instr.op('i32.or'),
        Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        ...loadAt(address, baseOffset),
        Instr.localGet(offset),
        Instr.op('i32.add'),
        Instr.localGet(value),
        Instr.localGet(length),
        Instr.memoryFill(context.memory),
      ]
    }
    case 'SlotWrite': {
      const address = scalar(operation.slot)
      const shape = LayoutPlan.callingShape(plan, operation.element)
      if (shape === undefined) throw new RangeError('Wasm Slot.write lost its element shape')
      return shape.lanes.flatMap((lane, ordinal) => {
        const value = slots(operation.value).at(ordinal)
        const offset = LayoutPlan.laneOffset(plan, operation.element, lane.path)
        if (value === undefined || offset === undefined) {
          throw new RangeError('Wasm Slot.write lost an element lane')
        }
        return storeAt(address, value, offset, lane)
      })
    }

    case 'SlotTake':
    case 'SlotCopy': {
      const address = scalar(operation.slot)
      const shape = LayoutPlan.callingShape(plan, operation.element)
      if (shape === undefined) throw new RangeError('Wasm Slot.take lost its element shape')
      return shape.lanes.flatMap((lane, ordinal) => {
        const destination = slots(operation.destination).at(ordinal)
        const offset = LayoutPlan.laneOffset(plan, operation.element, lane.path)
        if (destination === undefined || offset === undefined) {
          throw new RangeError('Wasm Slot.take lost an element lane')
        }
        return [...loadAt(address, offset, lane), Instr.localSet(destination)]
      })
    }
    case 'SlotDrop':
      return releaseAtAddress(operation.cleanup, scalar(operation.slot))
    case 'ShortCircuit': {
      const right = [
        ...operation.right.operations.flatMap((nested) =>
          emitOperation(nested, layout, plan, resolve, memory, suspension),
        ),
        ...copy(slots(operation.right.result), slots(operation.destination)),
      ]
      const decided = [
        Instr.i32Const(operation.operator === 'Or' ? 1 : 0),
        Instr.localSet(scalar(operation.destination)),
      ]
      return [
        Instr.localGet(scalar(operation.left)),
        // `&&` takes the right operand on a true left operand; `||` takes it on a false one.
        Instr.ifElse(
          Instr.emptyBlockType,
          operation.operator === 'And' ? right : decided,
          operation.operator === 'And' ? decided : right,
        ),
      ]
    }
    case 'Match': {
      const emitMany = (operations: ReadonlyArray<Mir.Operation>): ReadonlyArray<Instr.Instr> =>
        operations.flatMap((nested) =>
          emitOperation(nested, layout, plan, resolve, memory, suspension),
        )
      const emitCandidates = (
        member: (typeof operation.members)[number],
        candidates: ReadonlyArray<Match.ArmId>,
        ordinal = 0,
      ): ReadonlyArray<Instr.Instr> => {
        const candidate = candidates.at(ordinal)
        if (candidate === undefined) return [Instr.op('unreachable')]
        const arm = operation.arms.find((entry) => entry.id.ordinal === candidate.ordinal)
        if (arm === undefined) throw new RangeError('Wasm match lost a candidate arm')
        const bindings = arm.bindings.flatMap((binding) => {
          const physical = LayoutPlan.memberFieldSlots(
            operation.scrutineeShape,
            member,
            binding.path,
          )
          if (physical === undefined) {
            throw new RangeError('Wasm match lost a pattern payload path')
          }
          return copy(
            physical.flatMap((lane) => {
              const source = slots(operation.scrutinee).at(lane)
              return source === undefined ? [] : [source]
            }),
            slots(binding.destination),
          )
        })
        const selected = [
          ...emitMany(arm.selected.operations),
          ...(layout.types.at(arm.selected.result.ordinal)?._tag === 'Bottom'
            ? []
            : copy(slots(arm.selected.result), slots(operation.destination))),
        ]
        if (arm.guard === undefined) return [...bindings, ...selected]
        return [
          ...bindings,
          ...emitMany(arm.guard.operations),
          Instr.localGet(scalar(arm.guard.result)),
          Instr.ifElse(
            Instr.emptyBlockType,
            selected,
            emitCandidates(member, candidates, ordinal + 1),
          ),
        ]
      }
      const emitDecisions = (ordinal = 0): ReadonlyArray<Instr.Instr> => {
        const decision = operation.decisions.at(ordinal)
        if (decision === undefined) return [Instr.op('unreachable')]
        const selected = emitCandidates(decision.member, decision.candidates)
        if (operation.scrutineeType._tag === 'Nominal') return selected
        const tag = slots(operation.scrutinee).at(0)
        if (tag === undefined) throw new RangeError('Wasm union match has no tag lane')
        return [
          Instr.localGet(tag),
          Instr.i32Const(ordinal),
          Instr.op('i32.eq'),
          Instr.ifElse(Instr.emptyBlockType, selected, emitDecisions(ordinal + 1)),
        ]
      }
      return emitDecisions()
    }
    case 'Literal': {
      const lane = layout.lanes.at(operation.destination.ordinal)?.at(0)
      const semantic = Mir.semanticType(operation.type)
      if (Scalar.isFloatSpelling(semantic)) {
        const number = FloatingPoint.toNumber({
          width: semantic === 'f32' ? 32 : 64,
          bits: BigInt(operation.value),
        })
        return [
          semantic === 'f32' ? Instr.f32Const(number) : Instr.f64Const(number),
          Instr.localSet(scalar(operation.destination)),
        ]
      }
      const exact = BigInt(operation.value)
      return [
        lane !== undefined && laneValueType(plan, lane) === i64
          ? Instr.i64Const(BigInt.asIntN(64, exact))
          : Instr.i32Const(Number(BigInt.asIntN(32, exact))),
        Instr.localSet(scalar(operation.destination)),
      ]
    }
    case 'StaticView': {
      const context = requireMemory()
      const offset = context.staticOffsets.get(operation.data)
      const [address, length] = slots(operation.destination)
      if (offset === undefined || address === undefined || length === undefined) {
        throw new RangeError('Wasm static view lost its data placement or logical lanes')
      }
      return [
        Instr.i32Const(offset),
        Instr.localSet(address),
        Instr.i32Const(operation.length),
        Instr.localSet(length),
      ]
    }
    case 'Move':
      return copy(slots(operation.source), slots(operation.destination))
    case 'BeginLoan':
      if (operation.sourceType._tag === 'Slice') {
        return copy(slots(operation.root), slots(operation.destination))
      } else {
        const rootType = layout.types.at(operation.root.ordinal)
        const rootSemantic = rootType === undefined ? undefined : Mir.semanticType(rootType)
        const staticSelectors = operation.selectors.map((selector) => selector.field)
        if (rootSemantic !== undefined && SilkType.isReference(rootSemantic)) {
          const rootAddress = scalar(operation.root)
          const [address, length] = slots(operation.destination)
          const offset = LayoutPlan.laneOffset(plan, rootSemantic.target, staticSelectors)
          if (address === undefined || offset === undefined) {
            throw new RangeError('Wasm projected borrow lost its reference address')
          }
          if (operation.type._tag !== 'Reference') {
            throw new RangeError('Wasm projected reference borrow produced a non-reference')
          }
          return [
            Instr.localGet(rootAddress),
            Instr.i32Const(offset),
            Instr.op('i32.add'),
            Instr.localSet(address),
            ...(length === undefined ? [] : [Instr.i32Const(0), Instr.localSet(length)]),
          ]
        }
        const planned = memory?.frame.roots.get(operation.root.ordinal)
        const [address, length] = slots(operation.destination)
        const rootOffset =
          rootSemantic === undefined
            ? undefined
            : LayoutPlan.laneOffset(plan, rootSemantic, staticSelectors)
        if (planned === undefined || address === undefined) {
          throw new RangeError('Wasm borrow formation lost its frame root or address lane')
        }
        if (rootOffset === undefined) {
          throw new RangeError('Wasm borrow formation lost its projected root offset')
        }
        if (operation.type._tag === 'Reference') {
          return [
            ...materializeRoot(operation.root),
            ...frameAddress(planned.offset + rootOffset),
            Instr.localSet(address),
          ]
        }
        if (length === undefined || operation.sourceType._tag !== 'FixedArray') {
          throw new RangeError('Wasm slice formation lost its length lane or array root')
        }
        return [
          ...materializeRoot(operation.root),
          ...frameAddress(planned.offset + rootOffset),
          Instr.localSet(address),
          Instr.i32Const(operation.sourceType.type.length),
          Instr.localSet(length),
        ]
      }
    case 'EndLoan':
      return []
    case 'SliceLength': {
      const length = slots(operation.slice).at(1)
      return length === undefined
        ? [Instr.op('unreachable')]
        : [Instr.localGet(length), Instr.localSet(scalar(operation.destination))]
    }
    case 'ConvertUnion': {
      const source = slots(operation.source)
      const destination = slots(operation.destination)
      const tag = destination.at(0)
      if (tag === undefined) throw new RangeError('Wasm union destination has no tag lane')
      const instructions: Array<Instr.Instr> = []
      if (operation.conversion === 'Inject') {
        const mapping = operation.mappings.at(0)
        if (mapping === undefined) throw new RangeError('Wasm union injection has no member map')
        instructions.push(Instr.i32Const(mapping.targetOrdinal), Instr.localSet(tag))
        for (let slot = 1; slot < destination.length; slot += 1) {
          const target = destination.at(slot)
          const value = source.at(slot - 1)
          if (target === undefined) continue
          instructions.push(
            ...(value === undefined
              ? [zeroFor(target), Instr.localSet(target)]
              : transfer(value, target)),
          )
        }
        return instructions
      }
      const sourceTag = source.at(0)
      if (sourceTag === undefined) throw new RangeError('Wasm union source has no tag lane')
      instructions.push(Instr.i32Const(0), Instr.localSet(layout.scratch))
      for (const mapping of operation.mappings) {
        instructions.push(
          Instr.i32Const(mapping.targetOrdinal),
          Instr.localGet(layout.scratch),
          Instr.localGet(sourceTag),
          Instr.i32Const(mapping.sourceOrdinal),
          Instr.op('i32.eq'),
          Instr.op('select'),
          Instr.localSet(layout.scratch),
        )
      }
      instructions.push(Instr.localGet(layout.scratch), Instr.localSet(tag))
      for (let slot = 1; slot < destination.length; slot += 1) {
        const target = destination.at(slot)
        const value = source.at(slot)
        if (target === undefined) continue
        instructions.push(
          ...(value === undefined
            ? [zeroFor(target), Instr.localSet(target)]
            : transfer(value, target)),
        )
      }
      return instructions
    }
    case 'Construct':
      return copy(
        operation.fields.flatMap((field) => [...slots(field.value)]),
        slots(operation.destination),
      )
    case 'ConstructArray':
      return copy(
        operation.elements.flatMap((element) => [...slots(element)]),
        slots(operation.destination),
      )
    case 'Project': {
      const sourceLanes = layout.lanes.at(operation.source.ordinal) ?? []
      const sourceSlots = slots(operation.source)
      const projected = sourceLanes.flatMap((lane, index) => {
        const field = lane.path.at(0)
        const source = sourceSlots.at(index)
        return field !== undefined &&
          field._tag === 'FieldId' &&
          source !== undefined &&
          field.ordinal === operation.field.ordinal &&
          field.struct.sourceId === operation.field.struct.sourceId &&
          field.struct.ordinal === operation.field.struct.ordinal
          ? [source]
          : []
      })
      return copy(projected, slots(operation.destination))
    }
    case 'ReadPlace': {
      const rootType = layout.types.at(operation.root.ordinal)
      const rootSemantic = rootType === undefined ? undefined : Mir.semanticType(rootType)
      if (rootSemantic !== undefined && SilkType.isReference(rootSemantic)) {
        // The place lives on the referenced target: static field offsets off the address.
        const address = scalar(operation.root)
        const target = rootSemantic.target
        const staticSelectors: Array<LayoutPlan.Selector> = []
        for (const candidate of operation.selectors) {
          if (candidate._tag !== 'FieldSelector')
            throw new RangeError('Wasm reference place supports only field selectors')
          staticSelectors.push(candidate.field)
        }
        const destinationSlots = slots(operation.destination)
        const destinationLanes = layout.lanes.at(operation.destination.ordinal) ?? []
        return destinationLanes.flatMap((lane, ordinal) => {
          const destination = destinationSlots.at(ordinal)
          const offset = LayoutPlan.laneOffset(plan, target, [...staticSelectors, ...lane.path])
          if (destination === undefined || offset === undefined)
            throw new RangeError('Wasm reference read lost a lane offset')
          return [...loadAt(address, offset, lane), Instr.localSet(destination)]
        })
      }
      if (rootSemantic !== undefined && SilkType.isSlice(rootSemantic)) {
        if (memory === undefined) throw new RangeError('Wasm slice read has no private memory')
        const [selector, ...suffixSelectors] = operation.selectors
        const [base, length] = slots(operation.root)
        if (
          selector?._tag !== 'SliceElementSelector' ||
          base === undefined ||
          length === undefined
        ) {
          throw new RangeError('Wasm slice read lost its canonical lanes')
        }
        const sliceLayout = LayoutPlan.entry(memory.plan, rootSemantic)
        if (sliceLayout?.representation._tag !== 'Slice') {
          throw new RangeError('Wasm slice read lost its compiler layout')
        }
        const staticSelectors: Array<LayoutPlan.Selector> = []
        for (const candidate of suffixSelectors) {
          if (candidate._tag === 'FieldSelector') {
            staticSelectors.push(candidate.field)
          } else if (candidate._tag === 'ElementSelector' && candidate.index._tag === 'Proven') {
            staticSelectors.push(
              Object.freeze({ _tag: 'ElementSelector', index: candidate.index.value }),
            )
          } else {
            throw new RangeError('Wasm nested runtime slice place is not canonical')
          }
        }
        const instructions: Array<Instr.Instr> = [
          Instr.localGet(scalar(selector.index)),
          Instr.localGet(length),
          Instr.op('i32.lt_u'),
          Instr.ifElse(Instr.emptyBlockType, [], [Instr.op('unreachable')]),
        ]
        const destinationLanes = layout.lanes.at(operation.destination.ordinal) ?? []
        const destinationSlots = slots(operation.destination)
        for (const [ordinal, lane] of destinationLanes.entries()) {
          const staticOffset = LayoutPlan.laneOffset(
            memory.plan,
            rootSemantic.element,
            Object.freeze([...staticSelectors, ...lane.path]),
          )
          const destination = destinationSlots.at(ordinal)
          if (staticOffset === undefined || destination === undefined) {
            throw new RangeError(`Wasm slice read lost lane ${ordinal}`)
          }
          instructions.push(
            Instr.localGet(base),
            Instr.localGet(scalar(selector.index)),
            Instr.i32Const(sliceLayout.representation.stride),
            Instr.op('i32.mul'),
            Instr.op('i32.add'),
            ...(staticOffset === 0 ? [] : [Instr.i32Const(staticOffset), Instr.op('i32.add')]),
            Instr.memoryAccess(laneLoadMnemonic(memory.plan, lane), memory.memory),
            Instr.localSet(destination),
          )
        }
        return instructions
      }
      const sourceLanes = layout.lanes.at(operation.root.ordinal) ?? []
      const sourceSlots = slots(operation.root)
      const destinationLanes = layout.lanes.at(operation.destination.ordinal) ?? []
      const destinationSlots = slots(operation.destination)
      const instructions: Array<Instr.Instr> = []
      for (const selector of operation.selectors) {
        if (selector._tag !== 'ElementSelector' || selector.index._tag !== 'Runtime') continue
        instructions.push(
          Instr.localGet(scalar(selector.index.local)),
          Instr.i32Const(selector.length),
          Instr.op('i32.lt_u'),
          Instr.ifElse(Instr.emptyBlockType, [], [Instr.op('unreachable')]),
        )
      }
      for (const [destinationOrdinal, destinationLane] of destinationLanes.entries()) {
        const candidates = sourceLanes.flatMap((sourceLane, sourceOrdinal) => {
          if (sourceLane.path.length !== operation.selectors.length + destinationLane.path.length) {
            return []
          }
          const conditions: Array<{ readonly local: Mir.LocalId; readonly element: number }> = []
          for (const [selectorOrdinal, selector] of operation.selectors.entries()) {
            const physical = sourceLane.path.at(selectorOrdinal)
            if (physical === undefined) return []
            if (selector._tag === 'FieldSelector') {
              if (
                physical._tag !== 'FieldId' ||
                physical.ordinal !== selector.field.ordinal ||
                physical.struct.sourceId !== selector.field.struct.sourceId ||
                physical.struct.ordinal !== selector.field.struct.ordinal
              ) {
                return []
              }
            } else {
              if (physical._tag !== 'ElementSelector') return []
              if (selector.index._tag === 'Proven' && physical.index !== selector.index.value) {
                return []
              }
              if (selector.index._tag === 'Runtime') {
                conditions.push(
                  Object.freeze({ local: selector.index.local, element: physical.index }),
                )
              }
            }
          }
          const suffix = sourceLane.path.slice(operation.selectors.length)
          const sameSuffix = suffix.every((physical, ordinal) => {
            const expected = destinationLane.path.at(ordinal)
            return expected !== undefined && LayoutPlan.selectorEquals(physical, expected)
          })
          const source = sourceSlots.at(sourceOrdinal)
          return sameSuffix && source !== undefined ? [Object.freeze({ source, conditions })] : []
        })
        const first = candidates.at(0)
        const destination = destinationSlots.at(destinationOrdinal)
        if (
          first === undefined &&
          destination !== undefined &&
          operation.selectors.some(
            (selector) => selector._tag === 'ElementSelector' && selector.length === 0,
          )
        ) {
          instructions.push(Instr.i32Const(0), Instr.localSet(destination))
          continue
        }
        if (first === undefined || destination === undefined) {
          throw new RangeError(
            `Wasm backend could not realize place-read lane ${destinationOrdinal}`,
          )
        }
        let selection: ReadonlyArray<Instr.Instr> = [Instr.localGet(first.source)]
        for (const candidate of candidates.slice(1)) {
          const condition = candidate.conditions.flatMap((element, ordinal) => [
            Instr.localGet(scalar(element.local)),
            Instr.i32Const(element.element),
            Instr.op('i32.eq'),
            ...(ordinal === 0 ? [] : [Instr.op('i32.and')]),
          ])
          if (condition.length === 0) continue
          selection = [
            Instr.localGet(candidate.source),
            ...selection,
            ...condition,
            Instr.op('select'),
          ]
        }
        instructions.push(...selection, Instr.localSet(destination))
      }
      return instructions
    }
    case 'CheckPlace': {
      if (layout.types.at(operation.root.ordinal)?._tag === 'Slice') {
        const selector = operation.selectors.at(0)
        const length = slots(operation.root).at(1)
        if (selector?._tag !== 'SliceElementSelector' || length === undefined) {
          throw new RangeError('Wasm slice write check lost its canonical lanes')
        }
        return [
          Instr.localGet(scalar(selector.index)),
          Instr.localGet(length),
          Instr.op('i32.lt_u'),
          Instr.ifElse(Instr.emptyBlockType, [], [Instr.op('unreachable')]),
        ]
      }
      const instructions: Array<Instr.Instr> = []
      for (const selector of operation.selectors) {
        if (selector._tag !== 'ElementSelector' || selector.index._tag !== 'Runtime') continue
        instructions.push(
          Instr.localGet(scalar(selector.index.local)),
          Instr.i32Const(selector.length),
          Instr.op('i32.lt_u'),
          Instr.ifElse(Instr.emptyBlockType, [], [Instr.op('unreachable')]),
        )
      }
      return instructions
    }
    case 'WritePlace': {
      if (operation.rootType._tag === 'Reference') {
        // Writing through the borrow stores each value lane at its offset on the target.
        const address = scalar(operation.root)
        const target = operation.rootType.type.target
        const staticSelectors: Array<LayoutPlan.Selector> = []
        for (const candidate of operation.selectors) {
          if (candidate._tag !== 'FieldSelector')
            throw new RangeError('Wasm reference place supports only field selectors')
          staticSelectors.push(candidate.field)
        }
        const sourceSlots = slots(operation.source)
        const sourceLanes = layout.lanes.at(operation.source.ordinal) ?? []
        return sourceLanes.flatMap((lane, ordinal) => {
          const value = sourceSlots.at(ordinal)
          const offset = LayoutPlan.laneOffset(plan, target, [...staticSelectors, ...lane.path])
          if (value === undefined || offset === undefined)
            throw new RangeError('Wasm reference write lost a lane offset')
          return storeAt(address, value, offset, lane)
        })
      }
      if (operation.rootType._tag === 'Slice') {
        if (memory === undefined) throw new RangeError('Wasm slice write has no private memory')
        const [selector, ...suffixSelectors] = operation.selectors
        const base = slots(operation.root).at(0)
        if (selector?._tag !== 'SliceElementSelector' || base === undefined) {
          throw new RangeError('Wasm slice write lost its canonical address lane')
        }
        const sliceLayout = LayoutPlan.entry(memory.plan, operation.rootType.type)
        if (sliceLayout?.representation._tag !== 'Slice') {
          throw new RangeError('Wasm slice write lost its compiler layout')
        }
        const sliceType = operation.rootType.type
        const sliceRepresentation = sliceLayout.representation
        const staticSelectors: Array<LayoutPlan.Selector> = []
        for (const candidate of suffixSelectors) {
          if (candidate._tag === 'FieldSelector') {
            staticSelectors.push(candidate.field)
          } else if (candidate._tag === 'ElementSelector' && candidate.index._tag === 'Proven') {
            staticSelectors.push(
              Object.freeze({ _tag: 'ElementSelector', index: candidate.index.value }),
            )
          } else {
            throw new RangeError('Wasm nested runtime slice write is not canonical')
          }
        }
        const sourceLanes = layout.lanes.at(operation.source.ordinal) ?? []
        const sourceSlots = slots(operation.source)
        return sourceLanes.flatMap((lane, ordinal) => {
          const staticOffset = LayoutPlan.laneOffset(
            memory.plan,
            sliceType.element,
            Object.freeze([...staticSelectors, ...lane.path]),
          )
          const source = sourceSlots.at(ordinal)
          if (staticOffset === undefined || source === undefined) {
            throw new RangeError(`Wasm slice write lost lane ${ordinal}`)
          }
          return [
            Instr.localGet(base),
            Instr.localGet(scalar(selector.index)),
            Instr.i32Const(sliceRepresentation.stride),
            Instr.op('i32.mul'),
            Instr.op('i32.add'),
            ...(staticOffset === 0 ? [] : [Instr.i32Const(staticOffset), Instr.op('i32.add')]),
            Instr.localGet(source),
            Instr.memoryAccess(laneStoreMnemonic(memory.plan, lane), memory.memory),
          ]
        })
      }
      if (operation.selectors.length === 0) {
        return [
          ...copy(slots(operation.source), slots(operation.root)),
          ...flushBorrowRoot(operation.root),
        ]
      }
      const rootLanes = layout.lanes.at(operation.root.ordinal) ?? []
      const rootSlots = slots(operation.root)
      const sourceLanes = layout.lanes.at(operation.source.ordinal) ?? []
      const sourceSlots = slots(operation.source)
      const instructions: Array<Instr.Instr> = []
      for (const [rootOrdinal, rootLane] of rootLanes.entries()) {
        const conditions: Array<{ readonly local: Mir.LocalId; readonly element: number }> = []
        let matches = true
        for (const [selectorOrdinal, selector] of operation.selectors.entries()) {
          const physical = rootLane.path.at(selectorOrdinal)
          if (physical === undefined) {
            matches = false
            break
          }
          if (selector._tag === 'FieldSelector') {
            if (
              physical._tag !== 'FieldId' ||
              physical.ordinal !== selector.field.ordinal ||
              physical.struct.sourceId !== selector.field.struct.sourceId ||
              physical.struct.ordinal !== selector.field.struct.ordinal
            ) {
              matches = false
              break
            }
          } else if (selector._tag === 'SliceElementSelector') {
            matches = false
            break
          } else if (physical._tag !== 'ElementSelector') {
            matches = false
            break
          } else if (selector.index._tag === 'Proven') {
            if (physical.index !== selector.index.value) {
              matches = false
              break
            }
          } else {
            conditions.push(Object.freeze({ local: selector.index.local, element: physical.index }))
          }
        }
        if (!matches) continue
        const suffix = rootLane.path.slice(operation.selectors.length)
        const sourceOrdinal = sourceLanes.findIndex(
          (lane) =>
            lane.path.length === suffix.length &&
            lane.path.every((physical, ordinal) => {
              const expected = suffix.at(ordinal)
              return expected !== undefined && LayoutPlan.selectorEquals(physical, expected)
            }),
        )
        const source = sourceSlots.at(sourceOrdinal)
        const destination = rootSlots.at(rootOrdinal)
        if (source === undefined || destination === undefined) {
          throw new RangeError(`Wasm backend could not realize place-write lane ${rootOrdinal}`)
        }
        const assignment = [Instr.localGet(source), Instr.localSet(destination)]
        if (conditions.length === 0) {
          instructions.push(...assignment)
          continue
        }
        const condition = conditions.flatMap((element, ordinal) => [
          Instr.localGet(scalar(element.local)),
          Instr.i32Const(element.element),
          Instr.op('i32.eq'),
          ...(ordinal === 0 ? [] : [Instr.op('i32.and')]),
        ])
        instructions.push(...condition, Instr.ifElse(Instr.emptyBlockType, assignment, []))
      }
      return [...instructions, ...flushBorrowRoot(operation.root)]
    }
    case 'Drop':
      return releaseInstructions(operation.cleanup, operation.local)
    case 'MakeEffect':
    case 'MakeCallable': {
      const destination = slots(operation.destination)
      const instructions: Array<Instr.Instr> = []
      let cursor = 0
      const fields =
        operation._tag === 'MakeEffect'
          ? operation.type.environment.fields
          : (operation.type.environment?.fields ?? Object.freeze([]))
      for (const [ordinal, capture] of operation.captures.entries()) {
        const field = fields.at(ordinal)
        if (field === undefined) throw new RangeError('Wasm Effect capture lost its field')
        if (field.representation !== 'Borrow') {
          const source = slots(capture.source)
          instructions.push(...copy(source, destination.slice(cursor, cursor + source.length)))
          cursor += source.length
          continue
        }
        const target = destination.at(cursor)
        if (target === undefined) throw new RangeError('Wasm Effect capture lost its lane')
        const inherited = layout.borrowPointers.get(capture.source.ordinal)
        if (inherited !== undefined) {
          instructions.push(Instr.localGet(inherited), Instr.localSet(target))
        } else {
          const planned = memory?.frame.roots.get(capture.source.ordinal)
          if (planned === undefined) throw new RangeError('Wasm Effect capture lost its frame root')
          instructions.push(
            ...materializeRoot(capture.source),
            ...frameAddress(planned.offset),
            Instr.localSet(target),
          )
        }
        cursor += 1
      }
      if (cursor !== destination.length)
        throw new RangeError('Wasm Effect environment capture lanes do not match its plan')
      return instructions
    }
    case 'PackEffectOutcome': {
      const source = slots(operation.source)
      const destination = slots(operation.destination)
      const tag = destination.at(0)
      if (tag === undefined) throw new RangeError('Wasm effect outcome lost its tag lane')
      return [
        Instr.i32Const(operation.tag),
        Instr.localSet(tag),
        ...destination.slice(1).flatMap((target, index) => {
          const value = source.at(index)
          return value === undefined
            ? [Instr.i32Const(0), Instr.localSet(target)]
            : [Instr.localGet(value), Instr.localSet(target)]
        }),
      ]
    }
    case 'PackEffectFailureUnion': {
      const source = slots(operation.source)
      const destination = slots(operation.destination)
      const sourceTag = source.at(0)
      const destinationTag = destination.at(0)
      if (sourceTag === undefined || destinationTag === undefined)
        throw new RangeError('Wasm Effect failure union lost a tag lane')
      return [
        Instr.i32Const(-1),
        Instr.localSet(layout.scratch),
        ...operation.mappings.flatMap((mapping) => [
          Instr.localGet(sourceTag),
          Instr.i32Const(mapping.source),
          Instr.op('i32.eq'),
          Instr.ifElse(
            Instr.emptyBlockType,
            [Instr.i32Const(mapping.target), Instr.localSet(layout.scratch)],
            [],
          ),
        ]),
        Instr.localGet(layout.scratch),
        Instr.localSet(destinationTag),
        ...destination.slice(1).flatMap((target, index) => {
          const value = source.at(index + 1)
          return value === undefined
            ? [zeroFor(target), Instr.localSet(target)]
            : [...transfer(value, target)]
        }),
      ]
    }
    case 'UnpackEffectSuccess':
      return copy(
        slots(operation.source).slice(1, 1 + slots(operation.destination).length),
        slots(operation.destination),
      )
    case 'RunEffect': {
      const suspensionRegion = suspension?.regions.get(operation)
      if (suspensionRegion?._tag === 'SuspendEffectRegion')
        return suspension?.originate(suspensionRegion) ?? []
      const outcomeSlots = slots(operation.outcome)
      const destinationSlots = slots(operation.destination)
      const tag = outcomeSlots.at(0)
      if (tag === undefined) throw new RangeError('Wasm propagated effect lost its tag lane')
      const mapTag = [
        Instr.i32Const(-1),
        Instr.localSet(layout.scratch),
        ...operation.tagMappings.flatMap((mapping) => [
          Instr.localGet(tag),
          Instr.i32Const(mapping.source),
          Instr.op('i32.eq'),
          Instr.ifElse(
            Instr.emptyBlockType,
            [Instr.i32Const(mapping.target), Instr.localSet(layout.scratch)],
            [],
          ),
        ]),
      ]
      const propagationPayloadCount = operation.propagationLaneCount - 1
      const failure = [
        // Owners still live at this site release before the failure leaves.
        ...(operation.releases ?? []).flatMap((release) =>
          releaseInstructions(release.cleanup, release.local),
        ),
        ...mapTag,
        Instr.localGet(layout.scratch),
        ...Array.from({ length: propagationPayloadCount }, (_, index) => {
          const source = outcomeSlots.at(index + 1)
          return source === undefined ? Instr.i32Const(0) : Instr.localGet(source)
        }),
        Instr.op('return'),
      ]
      const invoke = [
        ...operation.arguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        Instr.call(resolve(operation.target, operation.typeArguments)),
        ...[...outcomeSlots].reverse().map((slot) => Instr.localSet(slot)),
      ]
      const completion = [
        Instr.localGet(tag),
        Instr.op('i32.eqz'),
        Instr.ifElse(
          Instr.emptyBlockType,
          copy(outcomeSlots.slice(1, 1 + destinationSlots.length), destinationSlots),
          failure,
        ),
      ]
      return [
        ...(skipInvocation ? [] : invoke),
        ...(skipInvocation || suspensionRegion?._tag !== 'RunSuspendableEffectRegion'
          ? []
          : (suspension?.relay(suspensionRegion) ?? [])),
        ...completion,
      ]
    }
    case 'RunEffectValue':
    case 'RunStaticEffect': {
      const suspensionRegion = suspension?.regions.get(operation)
      if (suspensionRegion?._tag === 'SuspendEffectRegion')
        return suspension?.originate(suspensionRegion) ?? []
      const outcomeSlots = slots(operation.outcome)
      const destinationSlots = slots(operation.destination)
      const tag = outcomeSlots.at(0)
      if (tag === undefined) throw new RangeError('Wasm Effect value lost its outcome tag lane')
      const invoke = [
        ...(operation._tag === 'RunEffectValue'
          ? slots(operation.effect)
          : operation.captures.flatMap((capture) => slots(capture.source))
        ).map((slot) => Instr.localGet(slot)),
        ...operation.arguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        Instr.call(resolve(operation.runner, operation.runnerTypeArguments)),
        ...[...outcomeSlots].reverse().map((slot) => Instr.localSet(slot)),
        ...reloadEscapingRoots(),
      ]
      const success = copy(outcomeSlots.slice(1, 1 + destinationSlots.length), destinationSlots)
      if (operation.propagationType === undefined)
        return [
          ...(skipInvocation ? [] : invoke),
          ...(skipInvocation || suspensionRegion?._tag !== 'RunSuspendableEffectRegion'
            ? []
            : (suspension?.relay(suspensionRegion) ?? [])),
          ...success,
        ]
      const mapTag = [
        Instr.i32Const(-1),
        Instr.localSet(layout.scratch),
        ...operation.tagMappings.flatMap((mapping) => [
          Instr.localGet(tag),
          Instr.i32Const(mapping.source),
          Instr.op('i32.eq'),
          Instr.ifElse(
            Instr.emptyBlockType,
            [Instr.i32Const(mapping.target), Instr.localSet(layout.scratch)],
            [],
          ),
        ]),
      ]
      const propagationPayloadCount = operation.propagationLaneCount - 1
      const failure = [
        // Owners still live at this site release before the failure leaves.
        ...(operation.releases ?? []).flatMap((release) =>
          releaseInstructions(release.cleanup, release.local),
        ),
        ...mapTag,
        Instr.localGet(layout.scratch),
        ...Array.from({ length: propagationPayloadCount }, (_, index) => {
          const source = outcomeSlots.at(index + 1)
          return source === undefined ? Instr.i32Const(0) : Instr.localGet(source)
        }),
        Instr.op('return'),
      ]
      return [
        ...(skipInvocation ? [] : invoke),
        ...(skipInvocation || suspensionRegion?._tag !== 'RunSuspendableEffectRegion'
          ? []
          : (suspension?.relay(suspensionRegion) ?? [])),
        Instr.localGet(tag),
        Instr.op('i32.eqz'),
        Instr.ifElse(Instr.emptyBlockType, success, failure),
      ]
    }
    case 'ReifyEffect': {
      const suspensionRegion = suspension?.regions.get(operation)
      if (suspensionRegion?._tag === 'SuspendEffectRegion')
        return suspension?.originate(suspensionRegion) ?? []
      const outcomeSlots = slots(operation.outcome)
      const destinationSlots = slots(operation.destination)
      const outcomeTag = outcomeSlots.at(0)
      const resultTag = destinationSlots.at(0)
      if (outcomeTag === undefined || resultTag === undefined)
        throw new RangeError('Wasm Effect result lost an outcome or Result tag lane')
      const invoke = [
        ...slots(operation.effect).map((slot) => Instr.localGet(slot)),
        ...operation.arguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        Instr.call(resolve(operation.runner, operation.runnerTypeArguments)),
        ...[...outcomeSlots].reverse().map((slot) => Instr.localSet(slot)),
        ...reloadEscapingRoots(),
      ]
      const writePayload = (
        source: ReadonlyArray<number>,
        destination: ReadonlyArray<number>,
      ): ReadonlyArray<Instr.Instr> =>
        destination.flatMap((target, index) => {
          const value = source.at(index)
          return [
            ...(value === undefined ? [Instr.i32Const(0)] : [Instr.localGet(value)]),
            Instr.localSet(target),
          ]
        })
      const resultPayload = destinationSlots.slice(1)
      const successLaneCount =
        operation.outcomeShape.tree._tag === 'OutcomeShape'
          ? operation.outcomeShape.tree.success.laneCount
          : 0
      const success = [
        Instr.i32Const(operation.successTag),
        Instr.localSet(resultTag),
        ...writePayload(outcomeSlots.slice(1, 1 + successLaneCount), resultPayload),
      ]
      const failure =
        operation.outcomeType.type.failures.length === 0
          ? [Instr.op('unreachable')]
          : [
              Instr.i32Const(operation.failureTag),
              Instr.localSet(resultTag),
              ...(SilkType.isUnion(operation.failureValueType)
                ? (() => {
                    const innerTag = resultPayload.at(0)
                    if (innerTag === undefined)
                      throw new RangeError('Wasm Effect Result failure lost its nested tag lane')
                    return [
                      Instr.localGet(outcomeTag),
                      Instr.i32Const(1),
                      Instr.op('i32.sub'),
                      Instr.localSet(innerTag),
                      ...writePayload(outcomeSlots.slice(1), resultPayload.slice(1)),
                    ]
                  })()
                : writePayload(outcomeSlots.slice(1), resultPayload)),
            ]
      return [
        ...(skipInvocation ? [] : invoke),
        ...(skipInvocation || suspensionRegion?._tag !== 'RunSuspendableEffectRegion'
          ? []
          : (suspension?.relay(suspensionRegion) ?? [])),
        Instr.localGet(outcomeTag),
        Instr.op('i32.eqz'),
        Instr.ifElse(Instr.emptyBlockType, success, failure),
      ]
    }
    case 'CloseEffectEntry': {
      const outcomeSlots = slots(operation.outcome)
      const tag = outcomeSlots.at(0)
      const destination = scalar(operation.destination)
      if (tag === undefined) throw new RangeError('Wasm effect entry lost its outcome tag')
      const failureBranch = (ordinal: number): ReadonlyArray<Instr.Instr> => {
        const failure = operation.failures.at(ordinal)
        if (failure === undefined) return [Instr.op('unreachable')]
        const payload = slots(failure.payload)
        const selected = outcomeSlots.slice(1, 1 + payload.length)
        if (selected.length !== payload.length)
          throw new RangeError('Wasm effect entry failure payload shape is inconsistent')
        return [
          Instr.localGet(tag),
          Instr.i32Const(failure.tag),
          Instr.op('i32.eq'),
          Instr.ifElse(
            Instr.emptyBlockType,
            [
              ...copy(selected, payload),
              ...releaseInstructions(failure.cleanup, failure.payload),
              Instr.i32Const(failure.tag),
              Instr.localSet(destination),
            ],
            failureBranch(ordinal + 1),
          ),
        ]
      }
      return [
        Instr.call(resolve(operation.target, operation.typeArguments)),
        ...[...slots(operation.effect)].reverse().map((slot) => Instr.localSet(slot)),
        ...slots(operation.effect).map((slot) => Instr.localGet(slot)),
        Instr.call(resolve(operation.runner, operation.typeArguments)),
        ...[...outcomeSlots].reverse().map((slot) => Instr.localSet(slot)),
        Instr.localGet(tag),
        Instr.op('i32.eqz'),
        Instr.ifElse(
          Instr.emptyBlockType,
          [Instr.i32Const(0), Instr.localSet(destination)],
          failureBranch(0),
        ),
      ]
    }
    case 'Call':
      return [
        ...operation.arguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        Instr.call(resolve(operation.target, operation.typeArguments)),
        ...[...slots(operation.destination)].reverse().map((slot) => Instr.localSet(slot)),
        ...[
          ...new Set(
            operation.arguments.flatMap((argument) => {
              const root = memory?.frame.sliceRoots.get(argument.ordinal)
              return root !== undefined && memory?.frame.roots.has(root) ? [root] : []
            }),
          ),
        ]
          .sort((left, right) => left - right)
          .flatMap(reloadRoot),
      ]
    case 'ApplyCallable': {
      const sourceType =
        operation.callable === undefined ? undefined : layout.types.at(operation.callable.ordinal)
      const target =
        operation.target ?? (sourceType?._tag === 'CallableValue' ? sourceType.target : undefined)
      if (target === undefined)
        throw new RangeError('Wasm callable application lost its hidden identity')
      const captureOperands: Array<Instr.Instr> = []
      if (operation.callable !== undefined) {
        if (sourceType?._tag !== 'CallableValue')
          throw new RangeError('Wasm stored callable lost its identity')
        const environmentSlots = slots(operation.callable)
        let cursor = 0
        for (const field of sourceType.environment?.fields ?? []) {
          if (memory === undefined && field.representation === 'Borrow')
            throw new RangeError('Wasm borrowed callable capture requires private memory')
          const shape = LayoutPlan.callingShape(plan, field.type)
          if (shape === undefined)
            throw new RangeError('Wasm callable capture lost its calling shape')
          if (field.representation === 'Value') {
            captureOperands.push(
              ...environmentSlots
                .slice(cursor, cursor + shape.laneCount)
                .map((slot) => Instr.localGet(slot)),
            )
            cursor += shape.laneCount
            continue
          }
          const pointer = environmentSlots.at(cursor)
          if (pointer === undefined || memory === undefined)
            throw new RangeError('Wasm borrowed callable capture lost its pointer')
          cursor += 1
          for (const lane of shape.lanes) {
            const offset = LayoutPlan.laneOffset(memory.plan, field.type, lane.path)
            if (offset === undefined)
              throw new RangeError('Wasm borrowed callable capture lost its lane offset')
            captureOperands.push(
              Instr.localGet(pointer),
              Instr.i32Const(offset),
              Instr.op('i32.add'),
              Instr.memoryAccess(laneLoadMnemonic(memory.plan, lane), memory.memory),
            )
          }
        }
      } else {
        for (const capture of operation.captures) {
          if (capture.access === 'Copy' || capture.access === 'Take') {
            captureOperands.push(...slots(capture.source).map((slot) => Instr.localGet(slot)))
            continue
          }
          captureOperands.push(...slots(capture.source).map((slot) => Instr.localGet(slot)))
        }
      }
      if (target._tag === 'BuiltinCallableTarget') {
        const operandSlots = [
          ...operation.arguments.flatMap((argument) => [...slots(argument)]),
          ...(operation.callable === undefined
            ? operation.captures.flatMap((capture) => [...slots(capture.source)])
            : [...slots(operation.callable)]),
        ]
        const left = operandSlots.at(0)
        if (left === undefined) throw new RangeError('Wasm callable builtin lost its first operand')
        const scalarActor = Scalar.find(target.actor)
        const conversionTarget = Scalar.conversionTarget(target.operation)
        if (conversionTarget !== undefined) {
          const source = scalarActor
          if (source?.category === 'Floating') {
            const bits = Scalar.bits(conversionTarget, plan.target.pointerSize === 4 ? 32 : 64)
            const mnemonic: Instr.PlainMnemonic =
              bits === 64
                ? source.spelling === 'f32'
                  ? conversionTarget.signedness === 'Signed'
                    ? 'i64.trunc_f32_s'
                    : 'i64.trunc_f32_u'
                  : conversionTarget.signedness === 'Signed'
                    ? 'i64.trunc_f64_s'
                    : 'i64.trunc_f64_u'
                : source.spelling === 'f32'
                  ? conversionTarget.signedness === 'Signed'
                    ? 'i32.trunc_f32_s'
                    : 'i32.trunc_f32_u'
                  : conversionTarget.signedness === 'Signed'
                    ? 'i32.trunc_f64_s'
                    : 'i32.trunc_f64_u'
            return [
              Instr.localGet(left),
              Instr.op(mnemonic),
              ...normalizeSubword(bits, conversionTarget.signedness === 'Signed'),
              Instr.localSet(scalar(operation.destination)),
            ]
          }
          if (source === undefined || source.category !== 'Integer')
            throw new RangeError('Wasm callable conversion lost its source actor')
          return [
            ...emitIntegerConversionValue(
              source,
              conversionTarget,
              left,
              plan.target.pointerSize === 4 ? 32 : 64,
            ),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        const floatTarget = Scalar.floatConversionTarget(target.operation)
        if (floatTarget !== undefined && scalarActor?.category === 'Integer') {
          const bits = Scalar.bits(scalarActor, plan.target.pointerSize === 4 ? 32 : 64)
          const mnemonic: Instr.PlainMnemonic =
            floatTarget.spelling === 'f32'
              ? bits === 64
                ? scalarActor.signedness === 'Signed'
                  ? 'f32.convert_i64_s'
                  : 'f32.convert_i64_u'
                : scalarActor.signedness === 'Signed'
                  ? 'f32.convert_i32_s'
                  : 'f32.convert_i32_u'
              : bits === 64
                ? scalarActor.signedness === 'Signed'
                  ? 'f64.convert_i64_s'
                  : 'f64.convert_i64_u'
                : scalarActor.signedness === 'Signed'
                  ? 'f64.convert_i32_s'
                  : 'f64.convert_i32_u'
          return [
            Instr.localGet(left),
            Instr.op(mnemonic),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        if (floatTarget !== undefined && scalarActor?.category === 'Floating') {
          const instruction: Instr.PlainMnemonic | undefined =
            scalarActor.spelling === floatTarget.spelling
              ? undefined
              : scalarActor.spelling === 'f64'
                ? 'f32.demote_f64'
                : 'f64.promote_f32'
          return [
            Instr.localGet(left),
            ...(instruction === undefined ? [] : [Instr.op(instruction)]),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        if (target.operation === 'ToBits' && scalarActor?.category === 'Floating')
          return [
            Instr.localGet(left),
            Instr.op(
              scalarActor.spelling === 'f32' ? 'i32.reinterpret_f32' : 'i64.reinterpret_f64',
            ),
            Instr.localSet(scalar(operation.destination)),
          ]
        if (target.operation === 'FromBits' && scalarActor?.category === 'Floating')
          return [
            Instr.localGet(left),
            Instr.op(
              scalarActor.spelling === 'f32' ? 'f32.reinterpret_i32' : 'f64.reinterpret_i64',
            ),
            Instr.localSet(scalar(operation.destination)),
          ]
        if (target.operation === 'Negate' && scalarActor?.category === 'Floating')
          return [
            Instr.localGet(left),
            Instr.op(`${scalarActor.spelling}.neg`),
            Instr.localSet(scalar(operation.destination)),
          ]
        if (target.operation === 'Not') {
          return [
            Instr.localGet(left),
            Instr.op('i32.eqz'),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        if (
          target.operation === 'Negate' ||
          target.operation === 'WrappingNegate' ||
          target.operation === 'SaturatingNegate' ||
          target.operation === 'BitNot'
        ) {
          const integer = Scalar.find(target.actor)
          if (integer === undefined || integer.category !== 'Integer') {
            throw new RangeError('Wasm callable unary operation lost its integer actor')
          }
          const bits = Scalar.bits(integer, plan.target.pointerSize === 4 ? 32 : 64)
          const range = Scalar.range(integer, plan.target.pointerSize === 4 ? 32 : 64)
          const constant = (value: bigint): Instr.Instr =>
            bits === 64 ? Instr.i64Const(value) : Instr.i32Const(Number(value))
          const prefix = bits === 64 ? 'i64' : 'i32'
          if (target.operation === 'BitNot') {
            return [
              Instr.localGet(left),
              constant(-1n),
              Instr.op(`${prefix}.xor`),
              ...normalizeSubword(bits, integer.signedness === 'Signed'),
              Instr.localSet(scalar(operation.destination)),
            ]
          }
          if (target.operation === 'SaturatingNegate') {
            return [
              constant(range.maximum),
              constant(0n),
              Instr.localGet(left),
              Instr.op(`${prefix}.sub`),
              Instr.localGet(left),
              constant(range.minimum),
              Instr.op(`${prefix}.eq`),
              Instr.op('select'),
              Instr.localSet(scalar(operation.destination)),
            ]
          }
          return [
            ...(target.operation === 'Negate'
              ? [
                  Instr.localGet(left),
                  constant(range.minimum),
                  Instr.op(`${prefix}.eq`),
                  Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
                ]
              : []),
            constant(0n),
            Instr.localGet(left),
            Instr.op(`${prefix}.sub`),
            ...normalizeSubword(bits, true),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        const right = operandSlots.at(1)
        if (
          right === undefined ||
          target.operation === 'StorageAcquire' ||
          !Mir.isBinaryOperator(target.operation)
        ) {
          throw new RangeError(
            `Wasm callable builtin ${target.actor}.${target.operation} is unavailable`,
          )
        }
        if (
          scalarActor?.category === 'Boolean' &&
          (target.operation === 'Equals' || target.operation === 'NotEquals')
        ) {
          return [
            Instr.localGet(left),
            Instr.localGet(right),
            Instr.op(target.operation === 'Equals' ? 'i32.eq' : 'i32.ne'),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        if (scalarActor?.category === 'Floating') {
          const prefix = scalarActor.spelling
          const mnemonic: Instr.PlainMnemonic | undefined =
            target.operation === 'Add'
              ? `${prefix}.add`
              : target.operation === 'Subtract'
                ? `${prefix}.sub`
                : target.operation === 'Multiply'
                  ? `${prefix}.mul`
                  : target.operation === 'Divide'
                    ? `${prefix}.div`
                    : target.operation === 'Equals'
                      ? `${prefix}.eq`
                      : target.operation === 'NotEquals'
                        ? `${prefix}.ne`
                        : target.operation === 'LessThan'
                          ? `${prefix}.lt`
                          : target.operation === 'LessOrEqual'
                            ? `${prefix}.le`
                            : target.operation === 'GreaterThan'
                              ? `${prefix}.gt`
                              : target.operation === 'GreaterOrEqual'
                                ? `${prefix}.ge`
                                : undefined
          if (mnemonic !== undefined)
            return [
              Instr.localGet(left),
              Instr.localGet(right),
              Instr.op(mnemonic),
              Instr.localSet(scalar(operation.destination)),
            ]
          if (target.operation === 'Remainder')
            return [
              Instr.localGet(left),
              Instr.localGet(left),
              Instr.localGet(right),
              Instr.op(`${prefix}.div`),
              Instr.op(`${prefix}.trunc`),
              Instr.localGet(right),
              Instr.op(`${prefix}.mul`),
              Instr.op(`${prefix}.sub`),
              Instr.localSet(scalar(operation.destination)),
            ]
          throw new RangeError(`Wasm callable float ${target.operation} is unavailable`)
        }
        const integer = scalarActor
        if (integer === undefined || integer.category !== 'Integer') {
          throw new RangeError('Wasm callable binary lost its integer actor')
        }
        return [
          ...emitIntegerBinaryValue(
            target.operation,
            integer,
            left,
            right,
            layout,
            plan.target.pointerSize === 4 ? 32 : 64,
          ),
          Instr.localSet(scalar(operation.destination)),
        ]
      }
      return [
        ...operation.arguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        ...captureOperands,
        Instr.call(resolve(target.declaration, operation.typeArguments)),
        ...[...slots(operation.destination)].reverse().map((slot) => Instr.localSet(slot)),
        ...reloadEscapingRoots(),
      ]
    }
    case 'ConvertInteger': {
      const source = Scalar.find(operation.sourceType._tag)
      const target = Scalar.find(operation.type._tag)
      if (
        source === undefined ||
        source.category !== 'Integer' ||
        target === undefined ||
        target.category !== 'Integer'
      )
        throw new RangeError('Wasm integer conversion lost its scalar types')
      return [
        ...emitIntegerConversionValue(
          source,
          target,
          scalar(operation.source),
          plan.target.pointerSize === 4 ? 32 : 64,
        ),
        Instr.localSet(scalar(operation.destination)),
      ]
    }
    case 'ConvertScalar': {
      const source = Scalar.find(operation.sourceType._tag)
      const target = Scalar.find(operation.type._tag)
      if (
        source === undefined ||
        target === undefined ||
        source.category === 'Boolean' ||
        target.category === 'Boolean'
      )
        throw new RangeError('Wasm scalar conversion lost its types')
      const sourceSlot = scalar(operation.source)
      const targetSlot = scalar(operation.destination)
      if (source.category === 'Floating' && target.category === 'Floating') {
        if (source.spelling === target.spelling)
          return [Instr.localGet(sourceSlot), Instr.localSet(targetSlot)]
        return [
          Instr.localGet(sourceSlot),
          Instr.op(source.spelling === 'f64' ? 'f32.demote_f64' : 'f64.promote_f32'),
          Instr.localSet(targetSlot),
        ]
      }
      if (source.category === 'Integer' && target.category === 'Floating') {
        const bits = Scalar.bits(source, plan.target.pointerSize === 4 ? 32 : 64)
        const mnemonic: Instr.PlainMnemonic =
          target.spelling === 'f32'
            ? bits === 64
              ? source.signedness === 'Signed'
                ? 'f32.convert_i64_s'
                : 'f32.convert_i64_u'
              : source.signedness === 'Signed'
                ? 'f32.convert_i32_s'
                : 'f32.convert_i32_u'
            : bits === 64
              ? source.signedness === 'Signed'
                ? 'f64.convert_i64_s'
                : 'f64.convert_i64_u'
              : source.signedness === 'Signed'
                ? 'f64.convert_i32_s'
                : 'f64.convert_i32_u'
        return [Instr.localGet(sourceSlot), Instr.op(mnemonic), Instr.localSet(targetSlot)]
      }
      if (source.category === 'Floating' && target.category === 'Integer') {
        const bits = Scalar.bits(target, plan.target.pointerSize === 4 ? 32 : 64)
        const mnemonic: Instr.PlainMnemonic =
          bits === 64
            ? source.spelling === 'f32'
              ? target.signedness === 'Signed'
                ? 'i64.trunc_f32_s'
                : 'i64.trunc_f32_u'
              : target.signedness === 'Signed'
                ? 'i64.trunc_f64_s'
                : 'i64.trunc_f64_u'
            : source.spelling === 'f32'
              ? target.signedness === 'Signed'
                ? 'i32.trunc_f32_s'
                : 'i32.trunc_f32_u'
              : target.signedness === 'Signed'
                ? 'i32.trunc_f64_s'
                : 'i32.trunc_f64_u'
        return [
          Instr.localGet(sourceSlot),
          Instr.op(mnemonic),
          ...normalizeSubword(bits, target.signedness === 'Signed'),
          Instr.localSet(targetSlot),
        ]
      }
      throw new RangeError('Wasm scalar conversion was not numeric')
    }
    case 'ReinterpretScalar': {
      const source = Scalar.find(operation.sourceType._tag)
      const target = Scalar.find(operation.type._tag)
      if (source === undefined || target === undefined)
        throw new RangeError('Wasm reinterpretation lost its types')
      const mnemonic: Instr.PlainMnemonic =
        source.spelling === 'f32' && target.spelling === 'u32'
          ? 'i32.reinterpret_f32'
          : source.spelling === 'f64' && target.spelling === 'u64'
            ? 'i64.reinterpret_f64'
            : source.spelling === 'u32' && target.spelling === 'f32'
              ? 'f32.reinterpret_i32'
              : source.spelling === 'u64' && target.spelling === 'f64'
                ? 'f64.reinterpret_i64'
                : (() => {
                    throw new RangeError('Wasm reinterpretation widths do not match')
                  })()
      return [
        Instr.localGet(scalar(operation.source)),
        Instr.op(mnemonic),
        Instr.localSet(scalar(operation.destination)),
      ]
    }
    case 'FloatUnary': {
      const source = Scalar.find(operation.sourceType._tag)
      if (source?.category !== 'Floating')
        throw new RangeError('Wasm float unary lost its source type')
      const input = scalar(operation.source)
      const destination = scalar(operation.destination)
      const prefix = source.spelling
      if (operation.operation === 'Negate')
        return [Instr.localGet(input), Instr.op(`${prefix}.neg`), Instr.localSet(destination)]
      // IEEE-754 mandates a correctly rounded square root, so the native opcode is bit-exact.
      if (operation.operation === 'Sqrt')
        return [Instr.localGet(input), Instr.op(`${prefix}.sqrt`), Instr.localSet(destination)]
      if (operation.operation === 'IsSignNegative') {
        return prefix === 'f32'
          ? [
              Instr.localGet(input),
              Instr.op('i32.reinterpret_f32'),
              Instr.i32Const(0),
              Instr.op('i32.lt_s'),
              Instr.localSet(destination),
            ]
          : [
              Instr.localGet(input),
              Instr.op('i64.reinterpret_f64'),
              Instr.i64Const(0n),
              Instr.op('i64.lt_s'),
              Instr.localSet(destination),
            ]
      }
      if (operation.operation === 'IsNaN')
        return [
          Instr.localGet(input),
          Instr.localGet(input),
          Instr.op(`${prefix}.ne`),
          Instr.localSet(destination),
        ]
      const infinity =
        prefix === 'f32'
          ? Instr.f32Const(Number.POSITIVE_INFINITY)
          : Instr.f64Const(Number.POSITIVE_INFINITY)
      if (operation.operation === 'IsInfinite')
        return [
          Instr.localGet(input),
          Instr.op(`${prefix}.abs`),
          infinity,
          Instr.op(`${prefix}.eq`),
          Instr.localSet(destination),
        ]
      if (operation.operation === 'IsFinite')
        return [
          Instr.localGet(input),
          Instr.op(`${prefix}.abs`),
          infinity,
          Instr.op(`${prefix}.lt`),
          Instr.localSet(destination),
        ]
      const minimum = prefix === 'f32' ? Instr.f32Const(2 ** -126) : Instr.f64Const(2 ** -1022)
      if (operation.operation === 'IsNormal')
        return [
          Instr.localGet(input),
          Instr.op(`${prefix}.abs`),
          minimum,
          Instr.op(`${prefix}.ge`),
          Instr.localGet(input),
          Instr.op(`${prefix}.abs`),
          infinity,
          Instr.op(`${prefix}.lt`),
          Instr.op('i32.and'),
          Instr.localSet(destination),
        ]
      return [
        Instr.localGet(input),
        Instr.op(`${prefix}.abs`),
        prefix === 'f32' ? Instr.f32Const(0) : Instr.f64Const(0),
        Instr.op(`${prefix}.gt`),
        Instr.localGet(input),
        Instr.op(`${prefix}.abs`),
        minimum,
        Instr.op(`${prefix}.lt`),
        Instr.op('i32.and'),
        Instr.localSet(destination),
      ]
    }
    case 'FloatTranscendental': {
      const source = Scalar.find(operation.sourceType._tag)
      if (source?.category !== 'Floating')
        throw new RangeError('Wasm transcendental lost its source type')
      const width = source.spelling === 'f32' ? 32 : 64
      const prefix = source.spelling
      const self = Transcendental.plan(width)
      const input = scalar(operation.source)
      const destination = scalar(operation.destination)
      const scratchFloat = source.spelling === 'f32' ? layout.scratchF32 : layout.scratchF64
      if (scratchFloat === undefined)
        throw new RangeError('Wasm transcendental lost its floating scratch lanes')
      const [scratchA, scratchB] = scratchFloat
      const floatType = source.spelling === 'f32' ? f32 : f64
      const constant = (bits: bigint): Instr.Instr => {
        const value = FloatingPoint.toNumber({ width, bits })
        return source.spelling === 'f32' ? Instr.f32Const(value) : Instr.f64Const(value)
      }
      const horner = (coefficients: ReadonlyArray<bigint>): ReadonlyArray<Instr.Instr> => {
        const instructions: Array<Instr.Instr> = [constant(coefficients.at(-1) ?? 0n)]
        for (let index = coefficients.length - 2; index >= 0; index -= 1) {
          instructions.push(
            Instr.localGet(scratchB),
            Instr.op(`${prefix}.mul`),
            constant(coefficients[index] ?? 0n),
            Instr.op(`${prefix}.add`),
          )
        }
        return instructions
      }
      const sine = [
        Instr.localGet(destination),
        Instr.localGet(destination),
        Instr.localGet(scratchB),
        Instr.op(`${prefix}.mul`),
        ...horner(self.sine),
        Instr.op(`${prefix}.mul`),
        Instr.op(`${prefix}.add`),
      ]
      const cosine = [
        constant(self.one),
        constant(self.half),
        Instr.localGet(scratchB),
        Instr.op(`${prefix}.mul`),
        Instr.op(`${prefix}.sub`),
        Instr.localGet(scratchB),
        Instr.localGet(scratchB),
        Instr.op(`${prefix}.mul`),
        ...horner(self.cosine),
        Instr.op(`${prefix}.mul`),
        Instr.op(`${prefix}.add`),
      ]
      const quadrantCase = (quadrant: number): ReadonlyArray<Instr.Instr> => {
        const sineResult =
          quadrant === 0
            ? [Instr.localGet(scratchA)]
            : quadrant === 1
              ? [Instr.localGet(scratchB)]
              : quadrant === 2
                ? [Instr.localGet(scratchA), Instr.op(`${prefix}.neg`)]
                : [Instr.localGet(scratchB), Instr.op(`${prefix}.neg`)]
        const cosineResult =
          quadrant === 0
            ? [Instr.localGet(scratchB)]
            : quadrant === 1
              ? [Instr.localGet(scratchA), Instr.op(`${prefix}.neg`)]
              : quadrant === 2
                ? [Instr.localGet(scratchB), Instr.op(`${prefix}.neg`)]
                : [Instr.localGet(scratchA)]
        return operation.operation === 'Sin' ? sineResult : cosineResult
      }
      const selectQuadrant = (quadrant = 0): ReadonlyArray<Instr.Instr> =>
        quadrant === 3
          ? quadrantCase(3)
          : [
              Instr.localGet(layout.scratch64),
              Instr.i64Const(BigInt(quadrant)),
              Instr.op('i64.eq'),
              Instr.ifElse(
                Instr.valueBlockType(floatType),
                quadrantCase(quadrant),
                selectQuadrant(quadrant + 1),
              ),
            ]
      const finite = [
        Instr.localGet(input),
        constant(self.inverseHalfPi),
        Instr.op(`${prefix}.mul`),
        constant(self.half),
        Instr.op(`${prefix}.neg`),
        constant(self.half),
        Instr.localGet(input),
        constant(0n),
        Instr.op(`${prefix}.lt`),
        Instr.op('select'),
        Instr.op(`${prefix}.add`),
        Instr.op(`i64.trunc_${prefix}_s`),
        Instr.localSet(layout.scratch64),
        Instr.localGet(input),
        Instr.localSet(destination),
        ...self.halfPi.flatMap((part) => [
          Instr.localGet(destination),
          Instr.localGet(layout.scratch64),
          Instr.op(`${prefix}.convert_i64_s`),
          constant(part),
          Instr.op(`${prefix}.mul`),
          Instr.op(`${prefix}.sub`),
          Instr.localSet(destination),
        ]),
        Instr.localGet(destination),
        Instr.localTee(destination),
        Instr.localGet(destination),
        Instr.op(`${prefix}.mul`),
        Instr.localSet(scratchB),
        ...sine,
        Instr.localSet(scratchA),
        ...cosine,
        Instr.localSet(scratchB),
        Instr.localGet(layout.scratch64),
        Instr.i64Const(3n),
        Instr.op('i64.and'),
        Instr.localSet(layout.scratch64),
        ...selectQuadrant(),
      ]
      return [
        Instr.localGet(input),
        Instr.localGet(input),
        Instr.op(`${prefix}.ne`),
        Instr.localGet(input),
        Instr.op(`${prefix}.abs`),
        source.spelling === 'f32'
          ? Instr.f32Const(Number.POSITIVE_INFINITY)
          : Instr.f64Const(Number.POSITIVE_INFINITY),
        Instr.op(`${prefix}.eq`),
        Instr.op('i32.or'),
        Instr.ifElse(
          Instr.valueBlockType(floatType),
          [constant(self.canonicalNaN)],
          [
            Instr.localGet(input),
            constant(0n),
            Instr.op(`${prefix}.eq`),
            Instr.ifElse(
              Instr.valueBlockType(floatType),
              operation.operation === 'Sin' ? [Instr.localGet(input)] : [constant(self.one)],
              finite,
            ),
          ],
        ),
        Instr.localSet(destination),
      ]
    }
    case 'CheckedInteger': {
      const destination = slots(operation.destination)
      const tag = destination.at(0)
      const payload = destination.at(1)
      const left = operation.operands.at(0)
      const right = operation.operands.at(1)
      const source = Scalar.find(operation.sourceType._tag)
      const target = Scalar.find(operation.valueType._tag)
      if (
        tag === undefined ||
        payload === undefined ||
        left === undefined ||
        source?.category !== 'Integer' ||
        target?.category !== 'Integer'
      )
        throw new RangeError('Wasm checked integer operation lost its Option lanes')
      const leftSlot = scalar(left)
      const rightSlot = right === undefined ? undefined : scalar(right)
      const pointerBits = plan.target.pointerSize === 4 ? 32 : 64
      const sourceBits = Scalar.bits(source, pointerBits)
      const targetBits = Scalar.bits(target, pointerBits)
      const sourcePrefix = sourceBits === 64 ? 'i64' : 'i32'
      const targetPrefix = targetBits === 64 ? 'i64' : 'i32'
      const sourceConstant = (value: bigint): Instr.Instr =>
        sourceBits === 64 ? Instr.i64Const(value) : Instr.i32Const(Number(value))
      const targetConstant = (value: bigint): Instr.Instr =>
        targetBits === 64 ? Instr.i64Const(value) : Instr.i32Const(Number(value))
      const successOrdinal = operation.type.type.members.findIndex((member) =>
        SilkType.equals(member, operation.success),
      )
      const failureOrdinal = operation.type.type.members.findIndex((member) =>
        SilkType.equals(member, operation.failure),
      )
      if (successOrdinal < 0 || failureOrdinal < 0)
        throw new RangeError('Wasm checked integer operation lost its Option members')
      const setTag = [
        Instr.i32Const(failureOrdinal),
        Instr.i32Const(successOrdinal),
        Instr.localGet(tag),
        Instr.op('select'),
        Instr.localSet(tag),
      ]
      if (operation.operation.startsWith('CheckedConvertTo')) {
        const sourceRange = Scalar.range(source, pointerBits)
        const targetRange = Scalar.range(target, pointerBits)
        const invalid: Array<Instr.Instr> = []
        if (targetRange.minimum > sourceRange.minimum)
          invalid.push(
            Instr.localGet(leftSlot),
            sourceConstant(targetRange.minimum),
            Instr.op(`${sourcePrefix}.lt_${source.signedness === 'Signed' ? 's' : 'u'}`),
          )
        if (targetRange.maximum < sourceRange.maximum) {
          invalid.push(
            Instr.localGet(leftSlot),
            sourceConstant(targetRange.maximum),
            Instr.op(`${sourcePrefix}.gt_${source.signedness === 'Signed' ? 's' : 'u'}`),
          )
          if (targetRange.minimum > sourceRange.minimum) invalid.push(Instr.op('i32.or'))
        }
        const conversion: ReadonlyArray<Instr.Instr> =
          sourceBits < 64 && targetBits === 64
            ? [Instr.op(`i64.extend_i32_${source.signedness === 'Signed' ? 's' : 'u'}`)]
            : sourceBits === 64 && targetBits < 64
              ? [Instr.op('i32.wrap_i64')]
              : []
        return [
          ...(invalid.length === 0 ? [Instr.i32Const(0)] : invalid),
          Instr.localSet(tag),
          ...setTag,
          Instr.localGet(leftSlot),
          ...conversion,
          ...normalizeSubword(targetBits, target.signedness === 'Signed'),
          Instr.localSet(payload),
        ]
      }
      if (rightSlot === undefined)
        throw new RangeError('Wasm checked arithmetic lost its right operand')
      if (
        operation.operation === 'CheckedAdd' ||
        operation.operation === 'CheckedSubtract' ||
        operation.operation === 'CheckedMultiply'
      ) {
        const shape: OverflowShape =
          operation.operation === 'CheckedAdd'
            ? 'Add'
            : operation.operation === 'CheckedSubtract'
              ? 'Subtract'
              : 'Multiply'
        const resultScratch = targetBits === 64 ? layout.scratch64 : layout.scratch
        return [
          ...checkedArithmeticOutcome(
            shape,
            target,
            leftSlot,
            rightSlot,
            resultScratch,
            pointerBits,
          ),
          Instr.localSet(tag),
          ...setTag,
          Instr.localGet(resultScratch),
          ...normalizeSubword(targetBits, target.signedness === 'Signed'),
          Instr.localSet(payload),
        ]
      }
      const minimum = Scalar.range(target, pointerBits).minimum
      const signedOverflow =
        target.signedness === 'Signed' && operation.operation === 'CheckedDivide'
          ? [
              Instr.localGet(leftSlot),
              targetConstant(minimum),
              Instr.op(`${targetPrefix}.eq`),
              Instr.localGet(rightSlot),
              targetConstant(-1n),
              Instr.op(`${targetPrefix}.eq`),
              Instr.op('i32.and'),
            ]
          : [Instr.i32Const(0)]
      const division: Instr.PlainMnemonic =
        operation.operation === 'CheckedDivide'
          ? `${targetPrefix}.div_${target.signedness === 'Signed' ? 's' : 'u'}`
          : `${targetPrefix}.rem_${target.signedness === 'Signed' ? 's' : 'u'}`
      return [
        Instr.localGet(rightSlot),
        Instr.op(`${targetPrefix}.eqz`),
        ...signedOverflow,
        Instr.op('i32.or'),
        Instr.localSet(tag),
        Instr.localGet(tag),
        Instr.ifElse(
          Instr.valueBlockType(targetBits === 64 ? i64 : i32),
          [targetConstant(0n)],
          [Instr.localGet(leftSlot), Instr.localGet(rightSlot), Instr.op(division)],
        ),
        ...normalizeSubword(targetBits, target.signedness === 'Signed'),
        Instr.localSet(payload),
        ...setTag,
      ]
    }
    case 'Binary': {
      const leftType = layout.types.at(operation.left.ordinal)
      const semantic = leftType === undefined ? undefined : Mir.semanticType(leftType)
      const scalarType = typeof semantic === 'string' ? Scalar.find(semantic) : undefined
      if (scalarType?.category === 'Floating') {
        const prefix = scalarType.spelling
        if (operation.operator === 'TotalOrder') {
          const temporary = prefix === 'f32' ? layout.scratch : layout.scratch64
          if (temporary === undefined)
            throw new RangeError('Wasm total order lost its scratch local')
          const integer = prefix === 'f32' ? 'i32' : 'i64'
          const all = prefix === 'f32' ? Instr.i32Const(-1) : Instr.i64Const(-1n)
          const sign =
            prefix === 'f32' ? Instr.i32Const(-2147483648) : Instr.i64Const(-9223372036854775808n)
          const zero = prefix === 'f32' ? Instr.i32Const(0) : Instr.i64Const(0n)
          const reinterpret: Instr.PlainMnemonic =
            prefix === 'f32' ? 'i32.reinterpret_f32' : 'i64.reinterpret_f64'
          const key = (local: number): ReadonlyArray<Instr.Instr> => [
            Instr.localGet(local),
            Instr.op(reinterpret),
            all,
            sign,
            Instr.localGet(local),
            Instr.op(reinterpret),
            zero,
            Instr.op(`${integer}.lt_s`),
            Instr.op('select'),
            Instr.op(`${integer}.xor`),
          ]
          return [
            ...key(scalar(operation.left)),
            Instr.localSet(temporary),
            Instr.localGet(temporary),
            ...key(scalar(operation.right)),
            Instr.op(`${integer}.le_u`),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        const comparison: Instr.PlainMnemonic | undefined =
          operation.operator === 'Equals'
            ? `${prefix}.eq`
            : operation.operator === 'NotEquals'
              ? `${prefix}.ne`
              : operation.operator === 'LessThan'
                ? `${prefix}.lt`
                : operation.operator === 'LessOrEqual'
                  ? `${prefix}.le`
                  : operation.operator === 'GreaterThan'
                    ? `${prefix}.gt`
                    : operation.operator === 'GreaterOrEqual'
                      ? `${prefix}.ge`
                      : undefined
        if (comparison !== undefined)
          return [
            Instr.localGet(scalar(operation.left)),
            Instr.localGet(scalar(operation.right)),
            Instr.op(comparison),
            Instr.localSet(scalar(operation.destination)),
          ]
        const arithmetic: Instr.PlainMnemonic | undefined =
          operation.operator === 'Add'
            ? `${prefix}.add`
            : operation.operator === 'Subtract'
              ? `${prefix}.sub`
              : operation.operator === 'Multiply'
                ? `${prefix}.mul`
                : operation.operator === 'Divide'
                  ? `${prefix}.div`
                  : undefined
        if (arithmetic !== undefined)
          return [
            Instr.localGet(scalar(operation.left)),
            Instr.localGet(scalar(operation.right)),
            Instr.op(arithmetic),
            Instr.localSet(scalar(operation.destination)),
          ]
        if (operation.operator === 'Remainder')
          return [
            Instr.localGet(scalar(operation.left)),
            Instr.localGet(scalar(operation.left)),
            Instr.localGet(scalar(operation.right)),
            Instr.op(`${prefix}.div`),
            Instr.op(`${prefix}.trunc`),
            Instr.localGet(scalar(operation.right)),
            Instr.op(`${prefix}.mul`),
            Instr.op(`${prefix}.sub`),
            Instr.localSet(scalar(operation.destination)),
          ]
        throw new RangeError(`Wasm float operation ${operation.operator} is unavailable`)
      }
      if (
        scalarType?.category === 'Boolean' &&
        (operation.operator === 'Equals' || operation.operator === 'NotEquals')
      ) {
        return [
          Instr.localGet(scalar(operation.left)),
          Instr.localGet(scalar(operation.right)),
          Instr.op(operation.operator === 'Equals' ? 'i32.eq' : 'i32.ne'),
          Instr.localSet(scalar(operation.destination)),
        ]
      }
      // A Unicode scalar value occupies one 32-bit lane and orders by that value, so its
      // comparisons are the unsigned 32-bit comparisons and it declares nothing else.
      if (scalarType?.category === 'Character') {
        const comparison: Instr.PlainMnemonic | undefined =
          operation.operator === 'Equals'
            ? 'i32.eq'
            : operation.operator === 'NotEquals'
              ? 'i32.ne'
              : operation.operator === 'LessThan'
                ? 'i32.lt_u'
                : operation.operator === 'LessOrEqual'
                  ? 'i32.le_u'
                  : operation.operator === 'GreaterThan'
                    ? 'i32.gt_u'
                    : operation.operator === 'GreaterOrEqual'
                      ? 'i32.ge_u'
                      : undefined
        if (comparison === undefined)
          throw new RangeError(`Wasm char operation ${operation.operator} is unavailable`)
        return [
          Instr.localGet(scalar(operation.left)),
          Instr.localGet(scalar(operation.right)),
          Instr.op(comparison),
          Instr.localSet(scalar(operation.destination)),
        ]
      }
      const integer = scalarType
      if (integer === undefined || integer.category !== 'Integer')
        throw new RangeError(`Wasm binary operation ${operation.operator} lost its integer type`)
      return [
        ...emitIntegerBinaryValue(
          operation.operator,
          integer,
          scalar(operation.left),
          scalar(operation.right),
          layout,
          plan.target.pointerSize === 4 ? 32 : 64,
        ),
        Instr.localSet(scalar(operation.destination)),
      ]
    }
  }
}

type Label =
  | { readonly _tag: 'If' }
  | { readonly _tag: 'Repeat'; readonly loop: number }
  | { readonly _tag: 'Exit'; readonly loop: number }

interface WasmSuspensionRuntime {
  readonly status: Global.Global
  readonly resumePath: Global.Global
  readonly resumeFrame: Global.Global
  readonly transferAddress: number
  readonly transferHeaderSize: number
  readonly transferResultOffset: number
  readonly origins: ReadonlyMap<string, number>
  readonly resumes: ReadonlyMap<string, number>
  readonly layouts: ReadonlyMap<string, Mir.ContinuationTargetLayout>
  readonly heapAllocate: FuncActor.Func
  readonly heapRelease: FuncActor.Func
  readonly rollback: FuncActor.Func
  readonly memory: Memory.Memory
}

const branchDepth = (
  labels: ReadonlyArray<Label>,
  tag: 'Repeat' | 'Exit',
  loop: number,
): number => {
  const depth = labels.findIndex((label) => label._tag === tag && label.loop === loop)
  if (depth < 0)
    throw new RangeError(`Wasm backend lost ${tag.toLowerCase()} label for loop${loop}`)
  return depth
}

/** Direct structured emission from canonical regions; no CFG recovery or dispatch loop exists. */
const emitBody = (
  fn: Mir.MirFunction,
  functions: ReadonlyArray<Mir.MirFunction>,
  layout: Layout,
  plan: LayoutPlan.Plan,
  resolve: (
    target: DeclarationIndex.CanonicalId,
    typeArguments: ReadonlyArray<SilkType.GenericArgument>,
  ) => FuncActor.Func,
  memory: MemoryContext | undefined,
  suspensionRuntime?: WasmSuspensionRuntime,
): ReadonlyArray<Instr.Instr> => {
  const regions = new Map(fn.regions.map((region) => [region.id.ordinal, region] as const))
  const slots = (local: Mir.LocalId): ReadonlyArray<number> => layout.slots.at(local.ordinal) ?? []
  const copySlots = (
    source: ReadonlyArray<number>,
    destination: ReadonlyArray<number>,
  ): ReadonlyArray<Instr.Instr> => {
    if (source.length !== destination.length)
      throw new RangeError('Wasm continuation cannot copy mismatched logical lane bundles')
    return source.flatMap((value, ordinal) => {
      const target = destination.at(ordinal)
      return target === undefined ? [] : [Instr.localGet(value), Instr.localSet(target)]
    })
  }
  const scalar = (local: Mir.LocalId): number => {
    const selected = slots(local)
    const first = selected.at(0)
    if (selected.length !== 1 || first === undefined) {
      throw new RangeError(`Wasm backend expected scalar local %${local.ordinal}`)
    }
    return first
  }
  const restoreFrame = (): ReadonlyArray<Instr.Instr> =>
    memory === undefined || memory.frame.roots.size === 0 || layout.frameBase === undefined
      ? []
      : [Instr.localGet(layout.frameBase), Instr.globalSet(memory.stackPointer)]
  const reserveFrame = (): ReadonlyArray<Instr.Instr> => {
    if (
      memory === undefined ||
      memory.frame.roots.size === 0 ||
      layout.frameBase === undefined ||
      layout.frameEnd === undefined ||
      layout.framePages === undefined
    ) {
      return []
    }
    if (memory.frame.size === 0) {
      return [Instr.globalGet(memory.stackPointer), Instr.localSet(layout.frameBase)]
    }
    /**
     * Report a deliberate trap rather than just taking one: name the reason in the status word, and
     * rewind the stack pointer to the base so the trap is a single legible event instead of a
     * module that answers every later call with the same trap. Whatever the abandoned frames owned
     * is leaked — no cleanup ran — but the allocator's own structures are untouched, so a host that
     * catches this can still read the heap back and get true answers out of it.
     */
    const report = (reason: number): ReadonlyArray<Instr.Instr> => [
      Instr.i32Const(statusAddress),
      Instr.i32Const(reason),
      Instr.memoryAccess('i32.store', memory.memory),
      Instr.i32Const(memory.stackBase),
      Instr.globalSet(memory.stackPointer),
      Instr.op('unreachable'),
    ]
    /**
     * One comparison against an address known at emission, on the path that already computed
     * `frameEnd`. A reservation that would cross into the heap reports here, before the stack
     * pointer moves, instead of being noticed downstream as corrupted memory.
     */
    const boundCheck =
      memory.stackLimit === undefined
        ? []
        : [
            Instr.localGet(layout.frameEnd),
            Instr.i32Const(memory.stackLimit),
            Instr.op('i32.gt_u'),
            Instr.ifElse(Instr.emptyBlockType, report(statusStackOverflow), []),
          ]
    return [
      Instr.globalGet(memory.stackPointer),
      Instr.localSet(layout.frameBase),
      Instr.localGet(layout.frameBase),
      Instr.i32Const(memory.frame.size),
      Instr.op('i32.add'),
      Instr.localTee(layout.frameEnd),
      Instr.localGet(layout.frameBase),
      Instr.op('i32.lt_u'),
      Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
      ...boundCheck,
      Instr.localGet(layout.frameEnd),
      Instr.i32Const(1),
      Instr.op('i32.sub'),
      Instr.i32Const(16),
      Instr.op('i32.shr_u'),
      Instr.i32Const(1),
      Instr.op('i32.add'),
      Instr.localSet(layout.framePages),
      Instr.localGet(layout.framePages),
      Instr.memorySize(memory.memory),
      Instr.op('i32.gt_u'),
      Instr.ifElse(
        Instr.emptyBlockType,
        [
          Instr.localGet(layout.framePages),
          Instr.memorySize(memory.memory),
          Instr.op('i32.sub'),
          Instr.memoryGrow(memory.memory),
          Instr.i32Const(-1),
          Instr.op('i32.eq'),
          Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        ],
        [],
      ),
      Instr.localGet(layout.frameEnd),
      Instr.globalSet(memory.stackPointer),
    ]
  }
  const loadBorrowedParameters = (): ReadonlyArray<Instr.Instr> => {
    if (layout.borrowPointers.size === 0) return []
    if (memory === undefined) throw new RangeError('Wasm Effect borrow has no private memory')
    return [...layout.borrowPointers].flatMap(([ordinal, pointer]) => {
      const type = layout.types.at(ordinal)
      if (type?._tag !== 'EffectBorrow')
        throw new RangeError(`Wasm Effect borrow %${ordinal} lost its type`)
      const lanes = layout.lanes.at(ordinal) ?? []
      const slots = layout.slots.at(ordinal) ?? []
      return lanes.flatMap((lane, laneOrdinal) => {
        const offset = LayoutPlan.laneOffset(memory.plan, type.type, lane.path)
        const destination = slots.at(laneOrdinal)
        if (offset === undefined || destination === undefined)
          throw new RangeError(`Wasm Effect borrow %${ordinal} lost lane ${laneOrdinal}`)
        return [
          Instr.localGet(pointer),
          ...(offset === 0 ? [] : [Instr.i32Const(offset), Instr.op('i32.add')]),
          Instr.memoryAccess(laneLoadMnemonic(memory.plan, lane), memory.memory),
          Instr.localSet(destination),
        ]
      })
    })
  }

  const resultLanes = (() => {
    if (fn.result._tag === 'EffectBorrow') {
      const shape = LayoutPlan.callingShape(plan, fn.result.type)
      return shape?.lanes ?? Object.freeze([])
    }
    if (fn.result._tag === 'EffectValue')
      return LayoutPlan.effectEnvironmentLanes(plan, fn.result.environment)
    if (fn.result._tag === 'CallableValue')
      return fn.result.environment === undefined
        ? Object.freeze([])
        : LayoutPlan.callableEnvironmentLanes(plan, fn.result.environment)
    return LayoutPlan.callingShape(plan, Mir.semanticType(fn.result))?.lanes ?? Object.freeze([])
  })()
  const zeroOf = (type: ValType.ValType): Instr.Instr =>
    type === i64
      ? Instr.i64Const(0n)
      : type === f32
        ? Instr.f32Const(0)
        : type === f64
          ? Instr.f64Const(0)
          : Instr.i32Const(0)
  const returnTransfer = (): ReadonlyArray<Instr.Instr> => [
    ...restoreFrame(),
    ...resultLanes.map((lane) => zeroOf(laneValueType(plan, lane))),
    Instr.op('return'),
  ]
  const storeI32 = (address: number, value: number): ReadonlyArray<Instr.Instr> => {
    if (suspensionRuntime === undefined) return []
    return [
      Instr.i32Const(address),
      Instr.i32Const(value),
      Instr.memoryAccess('i32.store', suspensionRuntime.memory),
    ]
  }
  const storeLocalLane = (
    base: ReadonlyArray<Instr.Instr>,
    offset: number,
    slot: number,
    type: ValType.ValType,
  ): ReadonlyArray<Instr.Instr> => [
    ...base,
    Instr.localGet(slot),
    Instr.memoryAccess(
      type === i64
        ? 'i64.store'
        : type === f32
          ? 'f32.store'
          : type === f64
            ? 'f64.store'
            : 'i32.store',
      suspensionRuntime?.memory ??
        (() => {
          throw new RangeError('Wasm suspension storage lost its memory')
        })(),
      { offset },
    ),
  ]
  const returnAllocationRefusal = (
    releases: ReadonlyArray<Mir.ContinuationRelease>,
    provenance: Mir.Provenance,
  ): ReadonlyArray<Instr.Instr> => {
    if (suspensionRuntime === undefined) return [Instr.op('unreachable')]
    if (fn.result._tag !== 'EffectOutcome') return [Instr.op('unreachable')]
    const failure = fn.result.type.failures.findIndex(
      (member) => member.module === 'silk/core' && member.name === 'OutOfMemory',
    )
    if (failure < 0) return [Instr.op('unreachable')]
    return [
      ...releases.flatMap((release) =>
        emitOperation(
          Object.freeze({
            _tag: 'Drop',
            local: release.local,
            cleanup: release.cleanup,
            provenance,
          }),
          layout,
          plan,
          resolve,
          memory,
        ),
      ),
      Instr.i32Const(0),
      Instr.globalSet(suspensionRuntime.status),
      Instr.call(suspensionRuntime.rollback),
      ...restoreFrame(),
      Instr.i32Const(failure + 1),
      ...resultLanes.slice(1).map((lane) => zeroOf(laneValueType(plan, lane))),
      Instr.op('return'),
    ]
  }
  const suspensionRegions = new Map(
    (fn.suspension?.regions ?? []).map((region) => [region.operation, region] as const),
  )
  const suspensionContext: WasmSuspensionFunctionContext | undefined =
    suspensionRuntime === undefined || suspensionRegions.size === 0
      ? undefined
      : Object.freeze({
          regions: suspensionRegions,
          originate: (
            region: Extract<Mir.SuspensionRegion, { readonly _tag: 'SuspendEffectRegion' }>,
          ) => {
            const child = suspensionRuntime.origins.get(suspensionPointKey(region.point))
            if (child === undefined) throw new RangeError('Wasm suspension origin lost child id')
            const inputs = suspensionOperationInputs(region.operation)
            const lanes = inputs.flatMap((local) => layout.lanes.at(local.ordinal) ?? [])
            const values = inputs.flatMap((local) => layout.slots.at(local.ordinal) ?? [])
            const packed = packWasmLanes(lanes, plan, suspensionRuntime.transferHeaderSize)
            if (packed.lanes.length !== values.length)
              throw new RangeError('Wasm suspension origin argument lanes disagree')
            return [
              ...storeI32(suspensionRuntime.transferAddress, child),
              ...storeI32(
                suspensionRuntime.transferAddress + plan.target.pointerSize * 2,
                suspensionRuntime.transferAddress + plan.target.pointerSize,
              ),
              ...packed.lanes.flatMap((lane, ordinal) => {
                const value = values.at(ordinal)
                if (value === undefined) return []
                return storeLocalLane(
                  [Instr.i32Const(suspensionRuntime.transferAddress)],
                  lane.offset,
                  value,
                  lane.type,
                )
              }),
              Instr.i32Const(1),
              Instr.globalSet(suspensionRuntime.status),
              ...returnTransfer(),
            ]
          },
          relay: (
            region: Extract<Mir.SuspensionRegion, { readonly _tag: 'RunSuspendableEffectRegion' }>,
          ) => {
            const descriptor = region.relay.continuation
            const transfer = [
              Instr.globalGet(suspensionRuntime.status),
              Instr.ifElse(Instr.emptyBlockType, returnTransfer(), []),
            ]
            if (descriptor === undefined) return transfer
            const targetLayout = suspensionRuntime.layouts.get(suspensionPointKey(descriptor.point))
            const resume = suspensionRuntime.resumes.get(suspensionPointKey(descriptor.point))
            const scratch = layout.suspensionScratch
            if (targetLayout === undefined || resume === undefined || scratch === undefined)
              throw new RangeError('Wasm stateful relay lost its layout or dispatch identity')
            const { frame, append, next, allocation } = scratch
            const [allocationTag, allocationBase, , , , allocationContext] = allocation
            if (
              allocationTag === undefined ||
              allocationBase === undefined ||
              allocationContext === undefined
            )
              throw new RangeError('Wasm stateful relay lost its allocation result scratch')
            const allocator = region.runner.providers.find(
              (provider) => provider.purposes.length === 2 && provider.argument !== undefined,
            )
            const implementation =
              allocator?.witness?._tag === 'SourceConformanceWitness'
                ? allocator.witness.operations.find((operation) => operation.name === 'allocate')
                    ?.implementation
                : undefined
            const allocatorConstructor =
              implementation === undefined || allocator?.argument === undefined
                ? undefined
                : functions.find((candidate) => {
                    const parameter = candidate.localTypes.at(0)
                    return (
                      candidate.id.module === implementation.module &&
                      candidate.id.name === implementation.name &&
                      parameter?._tag === 'Reference' &&
                      SilkType.equals(parameter.type.target, allocator.providerType) &&
                      candidate.result._tag === 'EffectValue'
                    )
                  })
            const effectType = allocatorConstructor?.result
            const runnerDeclaration =
              effectType?._tag === 'EffectValue'
                ? Hir.effectRunnerId(effectType.environment.instance.declaration, effectType.site)
                : undefined
            const allocatorRunner =
              runnerDeclaration === undefined || effectType?._tag !== 'EffectValue'
                ? undefined
                : functions.find((candidate) =>
                    Mir.matchesInstance(
                      candidate,
                      runnerDeclaration,
                      effectType.environment.instance.typeArguments,
                    ),
                  )
            const allocatorSlot =
              allocator?.argument === undefined
                ? undefined
                : layout.slots.at(allocator.argument.ordinal)?.at(allocator.argumentLane ?? 0)
            if (
              allocator === undefined ||
              allocatorConstructor === undefined ||
              allocatorRunner === undefined ||
              (allocatorRunner.suspension?.classification ?? 'Synchronous') !== 'Synchronous' ||
              allocatorSlot === undefined
            )
              throw new RangeError(
                'Wasm continuation allocator did not resolve to a closed synchronous witness',
              )
            const allocate = [
              Instr.localGet(allocatorSlot),
              Instr.i32Const(targetLayout.size),
              Instr.i32Const(targetLayout.alignment),
              Instr.call(
                resolve(allocatorConstructor.id, allocatorConstructor.instance.typeArguments),
              ),
              Instr.call(resolve(allocatorRunner.id, allocatorRunner.instance.typeArguments)),
              ...[...allocation].reverse().map((local) => Instr.localSet(local)),
              Instr.localGet(allocationTag),
              Instr.op('i32.eqz'),
              Instr.ifElse(
                Instr.emptyBlockType,
                [],
                returnAllocationRefusal(descriptor.allocationRefusal.releases, region.provenance),
              ),
              Instr.localGet(allocationBase),
              Instr.localTee(frame),
              Instr.op('i32.eqz'),
              Instr.ifElse(
                Instr.emptyBlockType,
                returnAllocationRefusal(descriptor.allocationRefusal.releases, region.provenance),
                [],
              ),
            ]
            const initialize = [
              ...allocate,
              Instr.i32Const(suspensionRuntime.transferAddress + plan.target.pointerSize * 2),
              Instr.memoryAccess('i32.load', suspensionRuntime.memory),
              Instr.localSet(append),
              Instr.localGet(append),
              Instr.memoryAccess('i32.load', suspensionRuntime.memory),
              Instr.localSet(next),
              Instr.localGet(frame),
              Instr.localGet(next),
              Instr.memoryAccess('i32.store', suspensionRuntime.memory),
              Instr.localGet(frame),
              Instr.i32Const(resume),
              Instr.memoryAccess('i32.store', suspensionRuntime.memory, {
                offset: plan.target.pointerSize,
              }),
              Instr.localGet(frame),
              Instr.i32Const(resume),
              Instr.memoryAccess('i32.store', suspensionRuntime.memory, {
                offset: plan.target.pointerSize * 2,
              }),
              Instr.localGet(frame),
              Instr.localGet(allocationContext),
              Instr.memoryAccess('i32.store', suspensionRuntime.memory, {
                offset: plan.target.pointerSize * 3,
              }),
              ...targetLayout.payload.flatMap((field) => {
                const fieldLanes = layout.lanes.at(field.local.ordinal) ?? []
                const fieldSlots = layout.slots.at(field.local.ordinal) ?? []
                const packed = packWasmLanes(fieldLanes, plan, field.offset)
                return packed.lanes.flatMap((lane, ordinal) => {
                  const value = fieldSlots.at(ordinal)
                  return value === undefined
                    ? []
                    : storeLocalLane([Instr.localGet(frame)], lane.offset, value, lane.type)
                })
              }),
              Instr.localGet(append),
              Instr.localGet(frame),
              Instr.memoryAccess('i32.store', suspensionRuntime.memory),
              Instr.i32Const(suspensionRuntime.transferAddress + plan.target.pointerSize * 2),
              Instr.localGet(frame),
              Instr.memoryAccess('i32.store', suspensionRuntime.memory),
              ...returnTransfer(),
            ]
            return [
              Instr.globalGet(suspensionRuntime.status),
              Instr.ifElse(Instr.emptyBlockType, initialize, []),
            ]
          },
        })

  const emitOutcome = (
    outcome: Mir.Outcome,
    labels: ReadonlyArray<Label>,
    stop: Mir.RegionId | undefined,
  ): ReadonlyArray<Instr.Instr> => {
    switch (outcome._tag) {
      case 'Forward':
        return stop?.ordinal === outcome.target.ordinal
          ? []
          : emitRegion(outcome.target, labels, stop)
      case 'Return':
        return [
          ...restoreFrame(),
          ...(layout.slots.at(outcome.value.ordinal) ?? []).map((slot) => Instr.localGet(slot)),
          Instr.op('return'),
        ]
      case 'Trap':
        return [Instr.op('unreachable')]
      case 'Repeat':
        return [Instr.br(branchDepth(labels, 'Repeat', outcome.loop.ordinal))]
      case 'Exit':
        return [Instr.br(branchDepth(labels, 'Exit', outcome.loop.ordinal))]
      case 'Yield':
        throw new RangeError('Wasm backend reached loop condition outside its loop region')
    }
  }

  const emitRegion = (
    id: Mir.RegionId,
    labels: ReadonlyArray<Label>,
    stop?: Mir.RegionId,
  ): ReadonlyArray<Instr.Instr> => {
    if (stop?.ordinal === id.ordinal) return []
    const region = regions.get(id.ordinal)
    if (region === undefined)
      throw new RangeError(`Wasm backend reached missing region r${id.ordinal}`)
    if (region._tag === 'OperationRegion' || region._tag === 'CleanupRegion') {
      const operations = region._tag === 'OperationRegion' ? region.operations : region.releases
      return [
        ...operations.flatMap((operation) =>
          emitOperation(operation, layout, plan, resolve, memory, suspensionContext),
        ),
        ...emitOutcome(region.outcome, labels, stop),
      ]
    }
    if (region._tag === 'ConditionalRegion') {
      const innerLabels = Object.freeze([{ _tag: 'If' as const }, ...labels])
      return [
        Instr.localGet(scalar(region.condition)),
        Instr.ifElse(
          Instr.emptyBlockType,
          emitRegion(region.taken, innerLabels, region.following),
          emitRegion(region.otherwise, innerLabels, region.following),
        ),
        ...(region.following === undefined ? [] : emitRegion(region.following, labels, stop)),
      ]
    }
    const condition = regions.get(region.condition.ordinal)
    if (condition?._tag !== 'OperationRegion' || condition.outcome._tag !== 'Yield') {
      throw new RangeError('Wasm loop condition is not one yielding operation region')
    }
    const loopLabels: ReadonlyArray<Label> = Object.freeze([
      { _tag: 'Repeat', loop: region.loop.ordinal },
      { _tag: 'Exit', loop: region.loop.ordinal },
      ...labels,
    ])
    const loopBody = [
      ...condition.operations.flatMap((operation) =>
        emitOperation(operation, layout, plan, resolve, memory, suspensionContext),
      ),
      Instr.localGet(scalar(region.conditionValue)),
      Instr.op('i32.eqz'),
      Instr.brIf(branchDepth(loopLabels, 'Exit', region.loop.ordinal)),
      ...emitRegion(region.body, loopLabels),
    ]
    return [
      Instr.block(Instr.emptyBlockType, [Instr.loop(Instr.emptyBlockType, loopBody)]),
      ...emitRegion(region.following, labels, stop),
    ]
  }

  const resumeDispatch = (): ReadonlyArray<Instr.Instr> => {
    if (suspensionRuntime === undefined || suspensionContext === undefined) return []
    return (fn.suspension?.regions ?? []).flatMap((region) => {
      if (region._tag !== 'RunSuspendableEffectRegion' || region.relay.continuation === undefined)
        return []
      const id = suspensionRuntime.resumes.get(suspensionPointKey(region.point))
      const targetLayout = suspensionRuntime.layouts.get(suspensionPointKey(region.point))
      const owner = regions.get(region.ownerRegion.ordinal)
      if (id === undefined || targetLayout === undefined || owner?._tag !== 'OperationRegion')
        throw new RangeError('Wasm resume dispatch lost its continuation region')
      const restorePayload = targetLayout.payload.flatMap((field) => {
        const lanes = layout.lanes.at(field.local.ordinal) ?? []
        const destinations = layout.slots.at(field.local.ordinal) ?? []
        const packed = packWasmLanes(lanes, plan, field.offset)
        return packed.lanes.flatMap((lane, ordinal) => {
          const destination = destinations.at(ordinal)
          if (destination === undefined) return []
          return [
            Instr.globalGet(suspensionRuntime.resumeFrame),
            Instr.memoryAccess(
              laneLoadMnemonic(
                plan,
                lanes.at(ordinal) ??
                  (() => {
                    throw new RangeError('Wasm continuation payload lost its lane')
                  })(),
              ),
              suspensionRuntime.memory,
              { offset: lane.offset },
            ),
            Instr.localSet(destination),
          ]
        })
      })
      const outcomeLanes = layout.lanes.at(region.operation.outcome.ordinal) ?? []
      const outcomeSlots = layout.slots.at(region.operation.outcome.ordinal) ?? []
      const packedOutcome = packWasmLanes(
        outcomeLanes,
        plan,
        suspensionRuntime.transferResultOffset,
      )
      const restoreOutcome = packedOutcome.lanes.flatMap((lane, ordinal) => {
        const destination = outcomeSlots.at(ordinal)
        const callingLane = outcomeLanes.at(ordinal)
        if (destination === undefined || callingLane === undefined) return []
        return [
          Instr.i32Const(suspensionRuntime.transferAddress),
          Instr.memoryAccess(laneLoadMnemonic(plan, callingLane), suspensionRuntime.memory, {
            offset: lane.offset,
          }),
          Instr.localSet(destination),
        ]
      })
      const completeSuspendedOperation = emitOperation(
        region.operation,
        layout,
        plan,
        resolve,
        memory,
        suspensionContext,
        true,
      )
      const resumeWithin = (
        operations: ReadonlyArray<Mir.Operation>,
        continuation: ReadonlyArray<Instr.Instr>,
      ): ReadonlyArray<Instr.Instr> | undefined => {
        for (const [operationOrdinal, operation] of operations.entries()) {
          const following = [
            ...operations
              .slice(operationOrdinal + 1)
              .flatMap((entry) =>
                emitOperation(entry, layout, plan, resolve, memory, suspensionContext),
              ),
            ...continuation,
          ]
          if (matchesSuspensionOperation(operation, region.operation)) {
            return [...completeSuspendedOperation, ...following]
          }
          if (operation._tag === 'ShortCircuit') {
            const nested = resumeWithin(operation.right.operations, [
              ...copySlots(slots(operation.right.result), slots(operation.destination)),
              ...following,
            ])
            if (nested !== undefined) return nested
          }
          if (operation._tag !== 'Match') continue
          for (const arm of operation.arms) {
            const selected = resumeWithin(arm.selected.operations, [
              ...(layout.types.at(arm.selected.result.ordinal)?._tag === 'Bottom'
                ? []
                : copySlots(slots(arm.selected.result), slots(operation.destination))),
              ...following,
            ])
            if (selected !== undefined) return selected
            if (arm.guard !== undefined) {
              const guarded = resumeWithin(arm.guard.operations, [])
              if (guarded !== undefined) {
                throw new RangeError(
                  'Wasm suspension inside a match guard requires finalized guard continuation control',
                )
              }
            }
          }
        }
        return undefined
      }
      const continuation = resumeWithin(owner.operations, [
        ...emitOutcome(owner.outcome, Object.freeze([]), undefined),
        Instr.op('unreachable'),
      ])
      if (continuation === undefined)
        throw new RangeError('Wasm resume dispatch cannot locate its suspended operation')
      const resumed = [
        Instr.i32Const(0),
        Instr.globalSet(suspensionRuntime.resumePath),
        ...restorePayload,
        ...restoreOutcome,
        ...continuation,
      ]
      return [
        Instr.globalGet(suspensionRuntime.resumePath),
        Instr.i32Const(id),
        Instr.op('i32.eq'),
        Instr.ifElse(Instr.emptyBlockType, resumed, []),
      ]
    })
  }

  return [
    ...reserveFrame(),
    ...loadBorrowedParameters(),
    ...(suspensionRuntime === undefined
      ? []
      : [Instr.i32Const(0), Instr.globalSet(suspensionRuntime.status)]),
    ...resumeDispatch(),
    ...emitRegion(fn.entry, Object.freeze([])),
    Instr.op('unreachable'),
  ]
}

const emitProgram = (program: Mir.Module, request: Backend.CodegenRequest) =>
  Effect.gen(function* () {
    const i32Layout = LayoutPlan.entry(program.layout, 'i32')
    if (i32Layout === undefined) {
      return yield* new Backend.BackendError({
        operation: 'Backend.emit',
        backend: 'WebAssembly',
        message: 'WebAssembly requires the planned i32 representation',
        reason: { _tag: 'InvalidMir', violations: Mir.verify(program) },
      })
    }
    // Boolean retains the canonical i32 lane; integer entries keep their logical width while
    // subword values are normalized in i32 lanes and 64-bit values use i64 lanes.
    if (
      program.layout.entries.some(
        (entry) => entry.representation._tag === 'Boolean' && entry.representation.bits !== 32,
      )
    ) {
      return yield* new Backend.BackendError({
        operation: 'Backend.emit',
        backend: 'WebAssembly',
        message: 'WebAssembly requires the canonical 32-bit i32 representation',
        reason: { _tag: 'InvalidMir', violations: Mir.verify(program) },
      })
    }
    // WebAssembly's debug-information equivalent of the LLVM backend's native debug metadata is
    // the `name` custom section, which the builder emits from the names given here. Debug builds
    // name the module, its functions, and their locals; release builds omit every name, which is
    // what the LLVM backend's `strip` flag does with its own metadata.
    const debug = request.mode === 'debug'
    const builder = yield* Builder.make(debug ? { moduleName: program.module } : {})
    const suspensionEnabled = program.functions.some(
      (fn) => (fn.suspension?.regions.length ?? 0) > 0,
    )
    const lanesFor = (type: Mir.Type): ReadonlyArray<LayoutPlan.CallingLane> => {
      if (type._tag === 'EffectBorrow')
        return Object.freeze([
          Object.freeze({
            _tag: 'CallingLane' as const,
            path: Object.freeze([]),
            type: Object.freeze({
              _tag: 'Address' as const,
              element: type.type,
              bits: program.layout.target.pointerSize === 4 ? 32 : 64,
            }),
          }),
        ])
      if (type._tag === 'EffectValue')
        return LayoutPlan.effectEnvironmentLanes(program.layout, type.environment)
      if (type._tag === 'CallableValue')
        return type.environment === undefined
          ? Object.freeze([])
          : LayoutPlan.callableEnvironmentLanes(program.layout, type.environment)
      const shape = LayoutPlan.callingShape(program.layout, Mir.semanticType(type))
      if (shape === undefined) throw new RangeError('Wasm declaration lost a calling shape')
      return shape.lanes
    }
    const originRecords = program.functions
      .flatMap((fn) =>
        (fn.suspension?.regions ?? []).flatMap((region) =>
          region._tag === 'SuspendEffectRegion' ? [Object.freeze({ fn, region })] : [],
        ),
      )
      .sort((left, right) =>
        suspensionPointKey(left.region.point).localeCompare(suspensionPointKey(right.region.point)),
      )
    const resumeRecords = program.functions
      .flatMap((fn) =>
        (fn.suspension?.regions ?? []).flatMap((region) =>
          region._tag === 'RunSuspendableEffectRegion' && region.relay.continuation !== undefined
            ? [Object.freeze({ fn, region })]
            : [],
        ),
      )
      .sort((left, right) =>
        suspensionPointKey(left.region.point).localeCompare(suspensionPointKey(right.region.point)),
      )
    const originIds = new Map(
      originRecords.map((record, ordinal) => [
        suspensionPointKey(record.region.point),
        ordinal + 1,
      ]),
    )
    const resumeIds = new Map(
      resumeRecords.map((record, ordinal) => [
        suspensionPointKey(record.region.point),
        ordinal + 1,
      ]),
    )
    const continuationLayouts = new Map(
      (program.continuations?.entries ?? []).map((entry) => [
        suspensionPointKey(entry.point),
        entry,
      ]),
    )
    const transferHeaderSize = program.layout.target.pointerSize * 3
    const originArgumentSize = program.functions.reduce((maximum, fn) => {
      const size = (fn.suspension?.regions ?? []).reduce((innerMaximum, region) => {
        if (region._tag !== 'SuspendEffectRegion') return innerMaximum
        const lanes = suspensionOperationInputs(region.operation).flatMap((local) => {
          const type = fn.localTypes.at(local.ordinal)
          return type === undefined ? [] : [...lanesFor(type)]
        })
        return Math.max(innerMaximum, packWasmLanes(lanes, program.layout).end)
      }, 0)
      return Math.max(maximum, size)
    }, 0)
    const transferResultOffset = alignUp(
      transferHeaderSize + originArgumentSize,
      program.layout.target.pointerAlignment,
    )
    const transferResultSize = program.functions.reduce(
      (maximum, fn) => Math.max(maximum, packWasmLanes(lanesFor(fn.result), program.layout).end),
      0,
    )
    const transferStorageSize = alignUp(
      transferResultOffset + transferResultSize,
      program.layout.target.pointerAlignment,
    )
    const frames = new Map(
      program.functions.map((fn) => [fn, framePlan(fn, program.layout)] as const),
    )
    const staticOffsets = new Map<string, number>()
    let staticEnd = 16
    for (const data of program.staticData ?? []) {
      staticOffsets.set(data.id, staticEnd)
      staticEnd += data.bytes.length
    }
    staticEnd = alignUp(staticEnd, 16)
    const transferAddress = suspensionEnabled ? staticEnd : undefined
    if (transferAddress !== undefined)
      staticEnd = alignUp(staticEnd + Math.max(transferStorageSize, 16), 16)
    // A cleanup plan can reclaim a block this module never allocated itself — a caller's owner
    // dropped here — so the release helper is needed wherever a reclaim ticket is consumed too.
    const releasesBlocks = (plan: Ownership.CleanupPlan): boolean => Ownership.cleanupReclaims(plan)
    const needsHeap =
      suspensionEnabled ||
      program.functions.some((fn) =>
        Mir.operations(fn).some(
          (operation) =>
            operation._tag === 'Allocate' ||
            operation._tag === 'RawBufferFrom' ||
            operation._tag === 'RawBufferCount' ||
            operation._tag === 'RawBufferSlot' ||
            operation._tag === 'RawBufferRead' ||
            operation._tag === 'RawBufferView' ||
            operation._tag === 'RawBufferCopy' ||
            operation._tag === 'RawBufferFill' ||
            operation._tag === 'SlotWrite' ||
            operation._tag === 'SlotTake' ||
            operation._tag === 'SlotCopy' ||
            operation._tag === 'SlotDrop' ||
            (operation._tag === 'Drop' && releasesBlocks(operation.cleanup)) ||
            ((operation._tag === 'RunEffect' ||
              operation._tag === 'RunEffectValue' ||
              operation._tag === 'RunStaticEffect') &&
              (operation.releases ?? []).some((release) => releasesBlocks(release.cleanup))) ||
            (operation._tag === 'CloseEffectEntry' &&
              operation.failures.some((failure) => releasesBlocks(failure.cleanup))),
        ),
      )
    const needsHostWrite = program.functions.some((fn) =>
      Mir.operations(fn).some((operation) => operation._tag === 'HostWrite'),
    )
    const needsMemory =
      staticOffsets.size > 0 ||
      needsHeap ||
      suspensionEnabled ||
      needsHostWrite ||
      [...frames.values()].some((frame) => frame.roots.size > 0)
    const privateMemory = needsMemory
      ? yield* Memory.make(
          builder,
          { min: needsHeap ? 2 : 1, max: 65536 },
          debug ? { name: 'silk_memory' } : {},
        )
      : undefined
    // Every function is exported so the artifact is directly instantiable for inspection; the
    // private memory is exported for the same reason, and because a host that only wants to watch
    // the heap should not have to import a standard-stream write to see it.
    if (privateMemory !== undefined) {
      yield* ExportActor.memory(builder, StandardStreams.wasmMemoryExport, privateMemory)
    }
    const stackPointer = needsMemory
      ? yield* Global.make(
          builder,
          i32,
          true,
          [Instr.i32Const(staticEnd)],
          debug ? { name: 'silk_stack_pointer' } : {},
        )
      : undefined
    if (privateMemory !== undefined) {
      for (const data of program.staticData ?? []) {
        const offset = staticOffsets.get(data.id)
        if (offset === undefined) throw new RangeError('Wasm static data lost its offset')
        if (data.bytes.length > 0)
          yield* Data.active(
            builder,
            privateMemory,
            [Instr.i32Const(offset)],
            Uint8Array.from(data.bytes),
            debug ? { name: `${data.kind === 'Text' ? 'string_utf8' : 'bytes'}_${offset}` } : {},
          )
      }
    }
    // The bump region begins past the free-list head table, which wasm memory already zeroes.
    const heapPointer = needsMemory
      ? yield* Global.make(
          builder,
          i32,
          true,
          [Instr.i32Const(heapBase)],
          debug ? { name: 'silk_heap_pointer' } : {},
        )
      : undefined
    const suspendStatus = suspensionEnabled
      ? yield* Global.make(
          builder,
          i32,
          true,
          [Instr.i32Const(0)],
          debug ? { name: 'silk_suspend_status' } : {},
        )
      : undefined
    const suspendResumePath = suspensionEnabled
      ? yield* Global.make(
          builder,
          i32,
          true,
          [Instr.i32Const(0)],
          debug ? { name: 'silk_suspend_resume_path' } : {},
        )
      : undefined
    const suspendResumeFrame = suspensionEnabled
      ? yield* Global.make(
          builder,
          i32,
          true,
          [Instr.i32Const(0)],
          debug ? { name: 'silk_suspend_resume_frame' } : {},
        )
      : undefined
    const heapAllocate =
      needsHeap && privateMemory !== undefined && heapPointer !== undefined
        ? yield* FuncActor.declare(
            builder,
            yield* WasmType.func(builder, [i32, i32], [i32]),
            debug ? { name: 'silk_heap_allocate' } : {},
          )
        : undefined
    const heapRelease =
      needsHeap && privateMemory !== undefined
        ? yield* FuncActor.declare(
            builder,
            yield* WasmType.func(builder, [i32], []),
            debug ? { name: 'silk_heap_release' } : {},
          )
        : undefined
    const suspendRollback =
      suspensionEnabled && privateMemory !== undefined && heapRelease !== undefined
        ? yield* FuncActor.declare(
            builder,
            yield* WasmType.func(builder, [], []),
            debug ? { name: 'silk_suspend_rollback' } : {},
          )
        : undefined
    const standardWrite = needsHostWrite
      ? yield* Import.func(
          builder,
          StandardStreams.wasmModule,
          StandardStreams.wasmWriteAll,
          yield* WasmType.func(builder, [i32, i32, i32], [i32]),
          debug ? { name: 'silk_standard_stream_write_v1' } : {},
        )
      : undefined
    if (heapAllocate !== undefined && privateMemory !== undefined && heapPointer !== undefined) {
      const named = (name: string): FuncActor.Local => (debug ? { type: i32, name } : { type: i32 })
      yield* FuncActor.define(builder, heapAllocate, {
        locals: ['align', 'class', 'capacity', 'list', 'block', 'payload', 'cursor'].map(named),
        body: heapAllocateBody(privateMemory, heapPointer),
      })
    }
    if (heapRelease !== undefined && privateMemory !== undefined) {
      yield* FuncActor.define(builder, heapRelease, {
        locals: [debug ? { type: i32, name: 'list' } : { type: i32 }],
        body: heapReleaseBody(privateMemory),
      })
    }
    if (
      suspendRollback !== undefined &&
      privateMemory !== undefined &&
      heapRelease !== undefined &&
      transferAddress !== undefined
    ) {
      yield* FuncActor.define(builder, suspendRollback, {
        locals: [
          debug ? { type: i32, name: 'frame' } : { type: i32 },
          debug ? { type: i32, name: 'next' } : { type: i32 },
        ],
        body: [
          Instr.block(Instr.emptyBlockType, [
            Instr.loop(Instr.emptyBlockType, [
              Instr.i32Const(transferAddress + program.layout.target.pointerSize),
              Instr.memoryAccess('i32.load', privateMemory),
              Instr.localTee(0),
              Instr.op('i32.eqz'),
              Instr.brIf(1),
              Instr.localGet(0),
              Instr.memoryAccess('i32.load', privateMemory),
              Instr.localSet(1),
              Instr.i32Const(transferAddress + program.layout.target.pointerSize),
              Instr.localGet(1),
              Instr.memoryAccess('i32.store', privateMemory),
              Instr.localGet(0),
              Instr.memoryAccess('i32.load', privateMemory, {
                offset: program.layout.target.pointerSize * 3,
              }),
              Instr.call(heapRelease),
              Instr.br(0),
            ]),
          ]),
        ],
      })
    }

    // Declare every function first so calls resolve regardless of definition order, mirroring the
    // LLVM backend's declare-then-define pass structure.
    const declared: Array<{
      readonly fn: Mir.MirFunction
      readonly symbol: string
      readonly handle: FuncActor.Func
      readonly suspendable: boolean
    }> = []
    for (const fn of program.functions) {
      const signature = yield* WasmType.func(
        builder,
        fn.regions.length === 0
          ? []
          : fn.localTypes
              .slice(0, fn.parameterCount)
              .flatMap((type) => lanesFor(type).map((lane) => laneValueType(program.layout, lane))),
        lanesFor(fn.result).map((lane) => laneValueType(program.layout, lane)),
      )
      const suspendable = (fn.suspension?.classification ?? 'Synchronous') !== 'Synchronous'
      const publicSymbol = symbolFor(fn, Mir.machineEntry(program))
      const symbol = suspendable ? `${publicSymbol}$suspend_step` : publicSymbol
      declared.push({
        fn,
        symbol,
        suspendable,
        // The export name carries the symbol regardless, so a release module is still callable
        // by name even with the name section stripped.
        handle: yield* FuncActor.declare(builder, signature, debug ? { name: symbol } : {}),
      })
    }
    const thunkType = suspensionEnabled ? yield* WasmType.func(builder, [], [i32]) : undefined
    const originThunks = new Map<number, FuncActor.Func>()
    if (thunkType !== undefined)
      for (const [ordinal, record] of originRecords.entries()) {
        const id = ordinal + 1
        originThunks.set(
          id,
          yield* FuncActor.declare(
            builder,
            thunkType,
            debug ? { name: `silk_suspend_child_${id}_${record.region.point.ordinal}` } : {},
          ),
        )
      }
    const resumeThunks = new Map<number, FuncActor.Func>()
    if (thunkType !== undefined)
      for (const [ordinal, record] of resumeRecords.entries()) {
        const id = ordinal + 1
        resumeThunks.set(
          id,
          yield* FuncActor.declare(
            builder,
            thunkType,
            debug ? { name: `silk_suspend_resume_${id}_${record.region.point.ordinal}` } : {},
          ),
        )
      }
    const machine = declared.find((entry) =>
      Mir.matchesInstanceKey(entry.fn, Mir.machineEntry(program)),
    )
    const driver =
      suspensionEnabled && machine !== undefined
        ? yield* FuncActor.declare(
            builder,
            yield* WasmType.func(
              builder,
              machine.fn.regions.length === 0
                ? []
                : machine.fn.localTypes
                    .slice(0, machine.fn.parameterCount)
                    .flatMap((type) =>
                      lanesFor(type).map((lane) => laneValueType(program.layout, lane)),
                    ),
              lanesFor(machine.fn.result).map((lane) => laneValueType(program.layout, lane)),
            ),
            debug ? { name: symbolFor(machine.fn, Mir.machineEntry(program)) } : {},
          )
        : undefined
    const suspensionRuntime: WasmSuspensionRuntime | undefined =
      suspendStatus === undefined ||
      suspendResumePath === undefined ||
      suspendResumeFrame === undefined ||
      transferAddress === undefined ||
      heapAllocate === undefined ||
      heapRelease === undefined ||
      suspendRollback === undefined ||
      privateMemory === undefined
        ? undefined
        : Object.freeze({
            status: suspendStatus,
            resumePath: suspendResumePath,
            resumeFrame: suspendResumeFrame,
            transferAddress,
            transferHeaderSize,
            transferResultOffset,
            origins: originIds,
            resumes: resumeIds,
            layouts: continuationLayouts,
            heapAllocate,
            heapRelease,
            rollback: suspendRollback,
            memory: privateMemory,
          })

    const resolve = (
      targetId: DeclarationIndex.CanonicalId,
      typeArguments: ReadonlyArray<SilkType.GenericArgument>,
    ): FuncActor.Func => {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, targetId, typeArguments),
      )
      if (target === undefined) {
        const requested = typeArguments.map(SilkType.genericArgumentKey).join(', ')
        const candidates = declared
          .filter(
            (candidate) =>
              candidate.fn.id.module === targetId.module && candidate.fn.id.name === targetId.name,
          )
          .map((candidate) =>
            candidate.fn.instance.typeArguments.map(SilkType.genericArgumentKey).join(', '),
          )
        throw new RangeError(
          `Backend cannot resolve call target ${targetId.name}<${requested}>; candidates: ${candidates.join(' | ') || 'none'}`,
        )
      }
      return target.handle
    }

    for (const entry of declared) {
      const frame = frames.get(entry.fn)
      if (frame === undefined) throw new RangeError('Wasm declaration lost its frame plan')
      const layout = layoutOf(entry.fn, program.layout, frame, debug, entry.suspendable)
      const memory: MemoryContext | undefined =
        privateMemory === undefined || stackPointer === undefined || heapPointer === undefined
          ? undefined
          : Object.freeze({
              memory: privateMemory,
              stackPointer,
              stackBase: staticEnd,
              // Only a module with a heap has something above the shadow stack to run into.
              stackLimit: needsHeap ? stackLimit : undefined,
              heapPointer,
              frame,
              plan: program.layout,
              staticOffsets,
              ...(standardWrite === undefined ? {} : { standardWrite }),
              ...(heapAllocate === undefined ? {} : { heapAllocate }),
              ...(heapRelease === undefined ? {} : { heapRelease }),
            })
      // A body-less function is a declaration the frontend could not resolve; the LLVM backend
      // leaves it undefined, but wasm rejects an undefined function at emission, so it becomes a
      // trapping stub with the same observable behaviour.
      const body =
        entry.fn.regions.length === 0
          ? [Instr.op('unreachable')]
          : emitBody(
              entry.fn,
              program.functions,
              layout,
              program.layout,
              resolve,
              memory,
              entry.suspendable ? suspensionRuntime : undefined,
            )
      // Body validation happens here, inside the wasm builder, and its failure names only the
      // operation. Naming the function turns "expected i64, found i32" into a report that points
      // at the one body that produced it.
      yield* FuncActor.define(builder, entry.handle, {
        locals: entry.fn.regions.length === 0 ? [] : layout.declared,
        body,
      }).pipe(
        Effect.mapError(
          (cause) =>
            new Backend.BackendError({
              operation: 'Backend.emit',
              backend: 'WebAssembly',
              message: `WebAssembly emitted an invalid body for ${program.module} (1 violation(s)):\n${Backend.formatModuleViolations(
                [{ function: entry.symbol, message: cause.message, detail: [] }],
              )}`,
              reason: {
                _tag: 'InvalidModule',
                violations: [
                  Object.freeze({
                    function: entry.symbol,
                    message: cause.message,
                    detail: Object.freeze([]),
                  }),
                ],
              },
            }),
        ),
      )
      // Every function is exported so the artifact is directly instantiable for inspection.
      yield* ExportActor.func(builder, entry.symbol, entry.handle)
    }

    const loadTransferLane = (
      lane: LayoutPlan.CallingLane,
      offset: number,
    ): ReadonlyArray<Instr.Instr> => {
      if (privateMemory === undefined || transferAddress === undefined)
        return [Instr.op('unreachable')]
      return [
        Instr.i32Const(transferAddress),
        Instr.memoryAccess(laneLoadMnemonic(program.layout, lane), privateMemory, { offset }),
      ]
    }
    const storeTransferLocal = (
      local: number,
      lane: LayoutPlan.CallingLane,
      offset: number,
    ): ReadonlyArray<Instr.Instr> => {
      if (privateMemory === undefined || transferAddress === undefined)
        return [Instr.op('unreachable')]
      return [
        Instr.i32Const(transferAddress),
        Instr.localGet(local),
        Instr.memoryAccess(laneStoreMnemonic(program.layout, lane), privateMemory, { offset }),
      ]
    }
    for (const [ordinal, record] of originRecords.entries()) {
      const id = ordinal + 1
      const thunk = originThunks.get(id)
      const target = declared.find((candidate) =>
        record.region.deferred.instance !== undefined
          ? Mir.matchesInstanceKey(candidate.fn, record.region.deferred.instance)
          : record.region.deferred.declaration !== undefined &&
            Mir.matchesInstance(
              candidate.fn,
              record.region.deferred.declaration,
              record.region.deferred.typeArguments,
            ),
      )
      if (
        thunk === undefined ||
        target === undefined ||
        suspendStatus === undefined ||
        transferAddress === undefined
      )
        throw new RangeError('Wasm suspension child thunk lost its target or runtime')
      const inputLocals = suspensionOperationInputs(record.region.operation)
      const inputLanes = inputLocals.flatMap((local) => {
        const type = record.fn.localTypes.at(local.ordinal)
        return type === undefined ? [] : [...lanesFor(type)]
      })
      const inputPacked = packWasmLanes(inputLanes, program.layout, transferHeaderSize)
      const targetResultLanes = lanesFor(target.fn.result)
      const targetResultPacked = packWasmLanes(
        targetResultLanes,
        program.layout,
        transferResultOffset,
      )
      yield* FuncActor.define(builder, thunk, {
        locals: targetResultLanes.map((lane, laneOrdinal) =>
          debug
            ? { type: laneValueType(program.layout, lane), name: `result${laneOrdinal}` }
            : { type: laneValueType(program.layout, lane) },
        ),
        body: [
          Instr.i32Const(0),
          Instr.globalSet(suspendStatus),
          ...inputPacked.lanes.flatMap((lane, laneOrdinal) => {
            const callingLane = inputLanes.at(laneOrdinal)
            return callingLane === undefined ? [] : loadTransferLane(callingLane, lane.offset)
          }),
          Instr.call(target.handle),
          ...targetResultLanes
            .map((_lane, laneOrdinal) => laneOrdinal)
            .reverse()
            .map((laneOrdinal) => Instr.localSet(laneOrdinal)),
          ...targetResultPacked.lanes.flatMap((lane, laneOrdinal) => {
            const callingLane = targetResultLanes.at(laneOrdinal)
            return callingLane === undefined
              ? []
              : storeTransferLocal(laneOrdinal, callingLane, lane.offset)
          }),
          Instr.globalGet(suspendStatus),
        ],
      })
    }
    for (const [ordinal, record] of resumeRecords.entries()) {
      const id = ordinal + 1
      const thunk = resumeThunks.get(id)
      const owner = declared.find((candidate) => candidate.fn === record.fn)
      if (
        thunk === undefined ||
        owner === undefined ||
        suspendStatus === undefined ||
        suspendResumePath === undefined ||
        suspendResumeFrame === undefined ||
        privateMemory === undefined ||
        heapRelease === undefined
      )
        throw new RangeError('Wasm suspension resume thunk lost its owner or runtime')
      const ownerResultLanes = lanesFor(owner.fn.result)
      const ownerResultPacked = packWasmLanes(
        ownerResultLanes,
        program.layout,
        transferResultOffset,
      )
      const parameterLanes = owner.fn.localTypes
        .slice(0, owner.fn.parameterCount)
        .flatMap((type) => lanesFor(type))
      const frameLocal = ownerResultLanes.length
      yield* FuncActor.define(builder, thunk, {
        locals: [
          ...ownerResultLanes.map((lane, laneOrdinal) =>
            debug
              ? { type: laneValueType(program.layout, lane), name: `result${laneOrdinal}` }
              : { type: laneValueType(program.layout, lane) },
          ),
          debug ? { type: i32, name: 'frame' } : { type: i32 },
        ],
        body: [
          Instr.globalGet(suspendResumeFrame),
          Instr.localSet(frameLocal),
          Instr.i32Const(0),
          Instr.globalSet(suspendStatus),
          Instr.i32Const(id),
          Instr.globalSet(suspendResumePath),
          ...parameterLanes.map((lane) =>
            laneValueType(program.layout, lane) === i64
              ? Instr.i64Const(0n)
              : laneValueType(program.layout, lane) === f32
                ? Instr.f32Const(0)
                : laneValueType(program.layout, lane) === f64
                  ? Instr.f64Const(0)
                  : Instr.i32Const(0),
          ),
          Instr.call(owner.handle),
          ...ownerResultLanes
            .map((_lane, laneOrdinal) => laneOrdinal)
            .reverse()
            .map((laneOrdinal) => Instr.localSet(laneOrdinal)),
          ...ownerResultPacked.lanes.flatMap((lane, laneOrdinal) => {
            const callingLane = ownerResultLanes.at(laneOrdinal)
            return callingLane === undefined
              ? []
              : storeTransferLocal(laneOrdinal, callingLane, lane.offset)
          }),
          Instr.localGet(frameLocal),
          Instr.memoryAccess('i32.load', privateMemory, {
            offset: program.layout.target.pointerSize * 3,
          }),
          Instr.call(heapRelease),
          Instr.globalGet(suspendStatus),
        ],
      })
    }
    if (
      driver !== undefined &&
      machine !== undefined &&
      privateMemory !== undefined &&
      transferAddress !== undefined &&
      suspendStatus !== undefined &&
      suspendResumeFrame !== undefined
    ) {
      const parameterLanes = machine.fn.localTypes
        .slice(0, machine.fn.parameterCount)
        .flatMap((type) => lanesFor(type))
      const resultLanes = lanesFor(machine.fn.result)
      const resultPacked = packWasmLanes(resultLanes, program.layout, transferResultOffset)
      const resultBase = parameterLanes.length
      const statusLocal = resultBase + resultLanes.length
      const headLocal = statusLocal + 1
      const idLocal = headLocal + 1
      const dispatch = (
        entries: ReadonlyArray<readonly [number, FuncActor.Func]>,
        ordinal = 0,
      ): ReadonlyArray<Instr.Instr> => {
        const entry = entries.at(ordinal)
        if (entry === undefined) return [Instr.op('unreachable')]
        return [
          Instr.localGet(idLocal),
          Instr.i32Const(entry[0]),
          Instr.op('i32.eq'),
          Instr.ifElse(
            Instr.valueBlockType(i32),
            [Instr.call(entry[1])],
            dispatch(entries, ordinal + 1),
          ),
        ]
      }
      const childDispatch = dispatch([...originThunks.entries()])
      const resumeDispatch = dispatch([...resumeThunks.entries()])
      const storeMachineResults = resultPacked.lanes.flatMap((lane, laneOrdinal) => {
        const callingLane = resultLanes.at(laneOrdinal)
        return callingLane === undefined
          ? []
          : storeTransferLocal(resultBase + laneOrdinal, callingLane, lane.offset)
      })
      const finish = resultPacked.lanes.flatMap((lane, laneOrdinal) => {
        const callingLane = resultLanes.at(laneOrdinal)
        return callingLane === undefined ? [] : loadTransferLane(callingLane, lane.offset)
      })
      yield* FuncActor.define(builder, driver, {
        locals: [
          ...resultLanes.map((lane, laneOrdinal) =>
            debug
              ? { type: laneValueType(program.layout, lane), name: `result${laneOrdinal}` }
              : { type: laneValueType(program.layout, lane) },
          ),
          debug ? { type: i32, name: 'status' } : { type: i32 },
          debug ? { type: i32, name: 'head' } : { type: i32 },
          debug ? { type: i32, name: 'dispatch' } : { type: i32 },
        ],
        body: [
          Instr.i32Const(transferAddress + program.layout.target.pointerSize),
          Instr.i32Const(0),
          Instr.memoryAccess('i32.store', privateMemory),
          Instr.i32Const(0),
          Instr.globalSet(suspendStatus),
          ...parameterLanes.map((_lane, ordinal) => Instr.localGet(ordinal)),
          Instr.call(machine.handle),
          ...resultLanes
            .map((_lane, laneOrdinal) => resultBase + laneOrdinal)
            .reverse()
            .map((local) => Instr.localSet(local)),
          ...storeMachineResults,
          Instr.globalGet(suspendStatus),
          Instr.localSet(statusLocal),
          Instr.loop(Instr.emptyBlockType, [
            Instr.localGet(statusLocal),
            Instr.ifElse(
              Instr.emptyBlockType,
              [
                Instr.i32Const(transferAddress),
                Instr.memoryAccess('i32.load', privateMemory),
                Instr.localSet(idLocal),
                ...childDispatch,
                Instr.localSet(statusLocal),
              ],
              [],
            ),
            Instr.localGet(statusLocal),
            Instr.op('i32.eqz'),
            Instr.ifElse(
              Instr.emptyBlockType,
              [
                Instr.i32Const(transferAddress + program.layout.target.pointerSize),
                Instr.memoryAccess('i32.load', privateMemory),
                Instr.localTee(headLocal),
                Instr.op('i32.eqz'),
                Instr.ifElse(
                  Instr.emptyBlockType,
                  [...finish, Instr.op('return')],
                  [
                    Instr.i32Const(transferAddress + program.layout.target.pointerSize),
                    Instr.localGet(headLocal),
                    Instr.memoryAccess('i32.load', privateMemory),
                    Instr.memoryAccess('i32.store', privateMemory),
                    Instr.localGet(headLocal),
                    Instr.globalSet(suspendResumeFrame),
                    Instr.localGet(headLocal),
                    Instr.memoryAccess('i32.load', privateMemory, {
                      offset: program.layout.target.pointerSize,
                    }),
                    Instr.localSet(idLocal),
                    ...resumeDispatch,
                    Instr.localSet(statusLocal),
                  ],
                ),
              ],
              [],
            ),
            Instr.br(0),
          ]),
          Instr.op('unreachable'),
        ],
      })
      yield* ExportActor.func(builder, symbolFor(machine.fn, Mir.machineEntry(program)), driver)
    }

    const bitcode = yield* Binary.encode(builder)
    // The host validates what was just encoded, so a module that could not be instantiated fails
    // at the emission that produced it rather than wherever it is finally loaded.
    const violations = yield* Validate.validate(bitcode)
    if (violations.length > 0) {
      return yield* new Backend.BackendError({
        operation: 'Backend.emit',
        backend: 'WebAssembly',
        message: `WebAssembly emitted an invalid module for ${program.module} (${violations.length} violation(s)):\n${Backend.formatModuleViolations(violations)}`,
        reason: { _tag: 'InvalidModule', violations },
      })
    }

    return {
      symbols: declared.map((entry) =>
        Object.freeze({
          declaration: entry.fn.id,
          instance: entry.fn.instance,
          symbol: symbolFor(entry.fn, Mir.machineEntry(program)),
        }),
      ),
      ir: yield* WatText.render(builder),
      bitcode,
    }
  })

const controlProvenance = (program: Mir.Module): ReadonlyArray<Backend.ControlProvenance> =>
  Object.freeze(
    program.functions.flatMap((fn) => {
      const loops = new Map(
        fn.regions.flatMap((region) =>
          region._tag === 'LoopRegion' ? [[region.loop.ordinal, region] as const] : [],
        ),
      )
      const conditions = new Map(
        [...loops.values()].map((loop) => [loop.condition.ordinal, loop] as const),
      )
      return Mir.topologicalRegions(fn).flatMap(
        (region): ReadonlyArray<Backend.ControlProvenance> => {
          if (region._tag === 'ConditionalRegion') {
            return [
              Object.freeze({
                _tag: 'BackendControlProvenance' as const,
                backend: 'WebAssembly' as const,
                function: fn.id,
                instance: fn.instance,
                region: region.id,
                construct: 'WasmIf' as const,
                targets: Object.freeze([
                  region.taken,
                  region.otherwise,
                  ...(region.following === undefined ? [] : [region.following]),
                ]),
                span: region.provenance.span,
              }),
            ]
          }
          if (region._tag === 'LoopRegion') {
            return [
              Object.freeze({
                _tag: 'BackendControlProvenance' as const,
                backend: 'WebAssembly' as const,
                function: fn.id,
                instance: fn.instance,
                region: region.id,
                construct: 'WasmLoop' as const,
                targets: Object.freeze([region.condition, region.body, region.following]),
                loop: region.loop,
                span: region.provenance.span,
              }),
            ]
          }
          const outcome = region.outcome
          const loop =
            outcome._tag === 'Repeat' || outcome._tag === 'Exit'
              ? loops.get(outcome.loop.ordinal)
              : outcome._tag === 'Yield'
                ? conditions.get(region.id.ordinal)
                : undefined
          const construct =
            outcome._tag === 'Repeat' || outcome._tag === 'Exit' || outcome._tag === 'Yield'
              ? 'WasmBr'
              : outcome._tag === 'Return'
                ? 'WasmReturn'
                : outcome._tag === 'Trap'
                  ? 'WasmTrap'
                  : undefined
          if (construct === undefined) return []
          const target =
            outcome._tag === 'Repeat'
              ? loop?.id
              : outcome._tag === 'Exit' || outcome._tag === 'Yield'
                ? loop?.following
                : undefined
          return [
            Object.freeze({
              _tag: 'BackendControlProvenance' as const,
              backend: 'WebAssembly' as const,
              function: fn.id,
              instance: fn.instance,
              region: region.id,
              construct,
              targets: target === undefined ? Object.freeze([]) : Object.freeze([target]),
              ...(loop === undefined ? {} : { loop: loop.loop }),
              span: outcome.provenance.span,
            }),
          ]
        },
      )
    }),
  )

/**
 * The WebAssembly backend over the Silk wasm builder. It satisfies the same nominal `Backend`
 * contract as `Backend.LlvmBackend`, while its artifact kind selects a different finalizer.
 */
export const WasmBackend: Backend.Backend<Backend.WebAssemblyModuleArtifact> = Object.freeze({
  _tag: 'Backend',
  id: 'wasm',
  name: 'WebAssembly',
  targets: Object.freeze([Target.wasm32UnknownUnknown.id]),
  emit: Effect.fn('Backend.WebAssembly.emit')(function* (
    program: Mir.Module,
    request: Backend.CodegenRequest,
  ): Effect.fn.Return<Backend.WebAssemblyModuleArtifact, Backend.BackendError> {
    const output = yield* emitProgram(program, request).pipe(
      Effect.mapError((cause) =>
        cause._tag === 'BackendError'
          ? cause
          : new Backend.BackendError({
              operation: 'Backend.emit',
              backend: 'WebAssembly',
              message: `WebAssembly emission failed for ${program.module}`,
              reason: { _tag: 'WrappedFailure', cause },
            }),
      ),
    )
    return Object.freeze({
      _tag: 'WebAssemblyModuleArtifact',
      backend: 'wasm',
      module: program.module,
      target: program.layout.target,
      symbols: Object.freeze(output.symbols),
      termination: Backend.terminationOf(program),
      nativeRuntimeSymbols: Object.freeze([]),
      control: controlProvenance(program),
      bytes: output.bitcode,
      wat: output.ir,
      hostImports: program.functions.some((fn) =>
        Mir.operations(fn).some((operation) => operation._tag === 'HostWrite'),
      )
        ? Object.freeze([
            Object.freeze({
              module: StandardStreams.wasmModule,
              name: StandardStreams.wasmWriteAll,
            }),
          ])
        : Object.freeze([]),
    })
  }),
})
