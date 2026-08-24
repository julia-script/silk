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
import type * as ValType from '@silk-effect/wasm/ValType'
import * as WatText from '@silk-effect/wasm/WatText'
import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import { symbolFor } from './Backend.js'
import * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import * as ExecutionPackage from './ExecutionPackage.js'
import * as ExecutionTransition from './ExecutionTransition.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import { alignUp } from './internal/Align.js'
import * as LayoutPlan from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import * as LocalSharedControlBlock from './LocalSharedControlBlock.js'
import * as LocalSharedPayloadCleanup from './LocalSharedPayloadCleanup.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as MirVerification from './MirVerification.js'
import * as Scalar from './Scalar.js'
import * as StandardStreams from './StandardStreams.js'
import * as Target from './Target.js'
import * as Transcendental from './Transcendental.js'
import * as SilkType from './Type.js'
import * as WasmCleanup from './WasmCleanup.js'
import type * as WasmEmitContext from './WasmEmitContext.js'
import {
  f32,
  f64,
  i32,
  i64,
  laneKindsOf,
  laneLoadMnemonic,
  laneStoreMnemonic,
  laneValueType,
  packWasmLanes,
  zeroConst,
} from './WasmLanes.js'
import type { FramePlan, MemoryContext } from './WasmMemory.js'
import {
  framePlan,
  heapAllocateBody,
  heapBase,
  heapHeaderBytes,
  heapReleaseBody,
  stackLimit,
} from './WasmMemory.js'
import * as WasmSuspension from './WasmSuspension.js'

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
  /** One preserved control-block address per statically nested local-shared cleanup. */
  readonly localSharedCleanupScratch: ReadonlyArray<number>
  /** Distinct package authorities and continuation cursors for recursive execution cleanup. */
  readonly executionCleanupScratch: ReadonlyArray<{
    readonly package: number
    readonly frame: number
  }>
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

const localSharedCleanupDepth = (cleanup: CleanupPlan.CleanupPlan): number => {
  switch (cleanup._tag) {
    case 'LocalSharedCoreCleanup':
      return 1
    case 'RawBufferCleanup':
      return localSharedCleanupDepth(cleanup.allocation)
    case 'HookCleanup':
      return localSharedCleanupDepth(cleanup.inner)
    case 'StructCleanup':
      return Math.max(0, ...cleanup.fields.map((field) => localSharedCleanupDepth(field.cleanup)))
    case 'ArrayCleanup':
      return localSharedCleanupDepth(cleanup.element)
    case 'UnionCleanup':
      return Math.max(0, ...cleanup.cases.map((entry) => localSharedCleanupDepth(entry.cleanup)))
    case 'CallableCleanup':
    case 'EffectCleanup':
      return Math.max(0, ...cleanup.slots.map((slot) => localSharedCleanupDepth(slot.cleanup)))
    case 'EffectCompositeCleanup':
      return Math.max(0, ...cleanup.alternatives.map(localSharedCleanupDepth))
    default:
      return 0
  }
}

const operationCleanupPlans = (
  operation: Mir.Operation,
): ReadonlyArray<CleanupPlan.CleanupPlan> => {
  switch (operation._tag) {
    case 'SlotDrop':
    case 'Drop':
      return Object.freeze([operation.cleanup])
    case 'SharedWithMut':
      return Object.freeze([operation.useCleanup, operation.conflictCleanup])
    case 'ExecutionPark':
      return Object.freeze([operation.guardCleanup, operation.registerCleanup])
    case 'PropagateEffectFailure':
    case 'RunEffect':
    case 'RunEffectValue':
    case 'RunEffectComposite':
    case 'RunStaticEffect':
      return Object.freeze((operation.releases ?? []).map((release) => release.cleanup))
    case 'CloseEffectEntry':
      return Object.freeze(operation.failures.map((failure) => failure.cleanup))
    case 'Match':
      return Object.freeze(
        operation.arms.flatMap((arm) => arm.selected.cleanup.map((entry) => entry.cleanup)),
      )
    default:
      return Object.freeze([])
  }
}

const containsExecutionCleanup = (cleanup: CleanupPlan.CleanupPlan): boolean => {
  switch (cleanup._tag) {
    case 'ExecutionCleanup':
    case 'WakeCleanup':
      return true
    case 'RawBufferCleanup':
      return containsExecutionCleanup(cleanup.allocation)
    case 'HookCleanup':
      return containsExecutionCleanup(cleanup.inner)
    case 'StructCleanup':
      return cleanup.fields.some((field) => containsExecutionCleanup(field.cleanup))
    case 'ArrayCleanup':
      return containsExecutionCleanup(cleanup.element)
    case 'UnionCleanup':
      return cleanup.cases.some((entry) => containsExecutionCleanup(entry.cleanup))
    case 'CallableCleanup':
    case 'EffectCleanup':
      return cleanup.slots.some((slot) => containsExecutionCleanup(slot.cleanup))
    case 'EffectCompositeCleanup':
      return cleanup.alternatives.some(containsExecutionCleanup)
    default:
      return false
  }
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
    return laneKindsOf(plan, type)
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
    ) ||
    MirVerification.operations(fn).some((operation) => operation._tag === 'FloatTranscendental')
  let nextInternal = physical + 1
  const scratch64 = needsScratch64 ? nextInternal : scratch
  if (needsScratch64) declared.push(named(i64, 'scratch64'))
  if (needsScratch64) nextInternal += 1
  const localSharedCleanupDepthMaximum = Math.max(
    0,
    ...MirVerification.operations(fn).flatMap(operationCleanupPlans).map(localSharedCleanupDepth),
  )
  const localSharedCleanupScratch = Object.freeze(
    Array.from({ length: localSharedCleanupDepthMaximum }, (_, depth) => nextInternal + depth),
  )
  for (const [depth] of localSharedCleanupScratch.entries()) {
    declared.push(named(i32, `local_shared_cleanup_${depth}`))
  }
  nextInternal += localSharedCleanupScratch.length
  const operations = MirVerification.operations(fn)
  const needsExecutionCleanup =
    operations.some(
      (operation) =>
        operation._tag === 'ExecutionFromAllocation' ||
        operation._tag === 'ExecutionDrive' ||
        operation._tag === 'ExecutionWake' ||
        operation._tag === 'ExecutionPark',
    ) || operations.flatMap(operationCleanupPlans).some(containsExecutionCleanup)
  const executionCleanupDepth = needsExecutionCleanup
    ? Math.max(1, plan.executionPackages.plans.length + 1)
    : 0
  const executionCleanupScratch = Object.freeze(
    Array.from({ length: executionCleanupDepth }, (_, depth) =>
      Object.freeze({
        package: nextInternal + depth * 2,
        frame: nextInternal + depth * 2 + 1,
      }),
    ),
  )
  for (const [depth] of executionCleanupScratch.entries()) {
    declared.push(
      named(i32, `execution_cleanup_package_${depth}`),
      named(i32, `execution_cleanup_frame_${depth}`),
    )
  }
  nextInternal += executionCleanupScratch.length * 2
  const needsScratchF32 = MirVerification.operations(fn).some(
    (operation) => operation._tag === 'FloatTranscendental' && operation.sourceType._tag === 'f32',
  )
  const scratchF32 = needsScratchF32 ? ([nextInternal, nextInternal + 1] as const) : undefined
  if (scratchF32 !== undefined) {
    declared.push(named(f32, 'scratch_f32_a'), named(f32, 'scratch_f32_b'))
    nextInternal += 2
  }
  const needsScratchF64 = MirVerification.operations(fn).some(
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
        })
      })()
    : undefined
  if (suspensionScratch !== undefined)
    declared.push(
      named(i32, 'suspend_frame'),
      named(i32, 'suspend_append'),
      named(i32, 'suspend_next'),
    )
  return {
    scratch,
    scratch64,
    localSharedCleanupScratch,
    executionCleanupScratch,
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

const suspensionOperationInputs = (
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' | 'ExecutionPark' }
  >,
): ReadonlyArray<Mir.LocalId> =>
  operation._tag === 'ExecutionPark'
    ? Object.freeze([operation.register])
    : operation._tag === 'RunEffect'
      ? operation.arguments
      : Object.freeze([operation.effect, ...operation.arguments])

const matchesSuspensionOperation = (
  candidate: Mir.Operation,
  expected: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' | 'ExecutionPark' }
  >,
): boolean =>
  candidate === expected ||
  ((candidate._tag === 'RunEffect' ||
    candidate._tag === 'RunEffectValue' ||
    candidate._tag === 'ReifyEffect' ||
    candidate._tag === 'ExecutionPark') &&
    candidate._tag === expected._tag &&
    candidate.destination.ordinal === expected.destination.ordinal &&
    (candidate._tag === 'ExecutionPark' ||
      expected._tag === 'ExecutionPark' ||
      candidate.outcome.ordinal === expected.outcome.ordinal))

interface WasmSuspensionFunctionContext {
  readonly regions: ReadonlyMap<Mir.Operation, Mir.SuspensionRegion>
  readonly originate: (
    region: Extract<Mir.SuspensionRegion, { readonly _tag: 'SuspendEffectRegion' }>,
  ) => ReadonlyArray<Instr.Instr>
  readonly relay: (
    region: Extract<Mir.SuspensionRegion, { readonly _tag: 'RunSuspendableEffectRegion' }>,
  ) => ReadonlyArray<Instr.Instr>
}

const makeOperationContext = (
  emitter: WasmEmitContext.WasmEmitContext<Layout, WasmSuspensionRuntime>,
  suspension?: WasmSuspensionFunctionContext,
  skipInvocation = false,
) => {
  const { layout, plan, resolve, resolveIndependent, memory, executionPackageCleanups } = emitter
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
    return zeroConst(type ?? i32)
  }
  const failurePayload = (
    source: ReadonlyArray<number>,
    sourceType: DeclarationFacts.SemanticType,
    sourceTag: number | undefined,
    targetType: SilkType.Effect,
    mappings: ReadonlyArray<{ readonly source: number; readonly target: number }>,
  ): ReadonlyArray<ReadonlyArray<Instr.Instr>> => {
    const targetShape = LayoutPlan.callingShape(plan, targetType)
    if (targetShape?.tree._tag !== 'OutcomeShape')
      throw new RangeError('Wasm failure propagation lost its target calling shape')
    return Object.freeze(
      targetShape.lanes.slice(1).map((targetLane, targetOrdinal) => {
        const targetValueType = laneValueType(plan, targetLane)
        const payloadFor = (mapping: {
          readonly source: number
          readonly target: number
        }): ReadonlyArray<Instr.Instr> => {
          const repacking = LayoutPlan.failurePayloadRepacking(
            plan,
            sourceType,
            mapping.source,
            targetType,
            mapping.target,
          )
          if (repacking === undefined)
            throw new RangeError('Wasm failure propagation has an invalid member mapping')
          const lane = repacking.lanes.find(
            (candidate) => candidate.targetOrdinal === targetOrdinal,
          )
          if (lane === undefined) return [zeroConst(targetValueType)]
          const sourceSlot = source.at(lane.sourceOrdinal)
          const sourceValueType =
            sourceSlot === undefined ? undefined : layout.physicalTypes.at(sourceSlot)
          if (sourceSlot === undefined || sourceValueType === undefined)
            throw new RangeError('Wasm failure propagation lost a source payload lane')
          const memberValueType = laneValueType(plan, lane.member)
          return [
            Instr.localGet(sourceSlot),
            ...laneBridge(sourceValueType, memberValueType),
            ...laneBridge(memberValueType, targetValueType),
          ]
        }
        if (sourceTag === undefined) {
          const mapping = mappings.at(0)
          return mapping === undefined ? [zeroConst(targetValueType)] : payloadFor(mapping)
        }
        let selected: ReadonlyArray<Instr.Instr> = [zeroConst(targetValueType)]
        for (const mapping of [...mappings].reverse()) {
          selected = [
            Instr.localGet(sourceTag),
            Instr.i32Const(mapping.source),
            Instr.op('i32.eq'),
            Instr.ifElse(Instr.valueBlockType(targetValueType), payloadFor(mapping), selected),
          ]
        }
        return selected
      }),
    )
  }
  const outcomePayload = (
    source: ReadonlyArray<number>,
    sourceType: DeclarationFacts.SemanticType,
    targetType: SilkType.Effect,
  ): ReadonlyArray<ReadonlyArray<Instr.Instr>> => {
    const sourceShape = LayoutPlan.callingShape(plan, sourceType)
    const targetShape = LayoutPlan.callingShape(plan, targetType)
    if (sourceShape === undefined || targetShape?.tree._tag !== 'OutcomeShape')
      throw new RangeError('Wasm effect outcome packing lost its calling shape')
    return Object.freeze(
      targetShape.lanes.slice(1).map((targetLane, ordinal) => {
        const targetValueType = laneValueType(plan, targetLane)
        const sourceSlot = source.at(ordinal)
        const sourceLane = sourceShape.lanes.at(ordinal)
        const sourceValueType =
          sourceSlot === undefined ? undefined : layout.physicalTypes.at(sourceSlot)
        if (sourceSlot === undefined || sourceLane === undefined || sourceValueType === undefined)
          return [zeroConst(targetValueType)]
        const memberValueType = laneValueType(plan, sourceLane)
        return [
          Instr.localGet(sourceSlot),
          ...laneBridge(sourceValueType, memberValueType),
          ...laneBridge(memberValueType, targetValueType),
        ]
      }),
    )
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
      const offset = LayoutVerify.laneOffset(memory.plan, Mir.semanticType(planned.type), lane.path)
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
      const offset = LayoutVerify.laneOffset(memory.plan, Mir.semanticType(planned.type), lane.path)
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
  /** Reloads exactly the frame roots reachable through the pointers handed to one call. */
  const reloadReachableRoots = (inputs: ReadonlyArray<Mir.LocalId>): ReadonlyArray<Instr.Instr> =>
    [
      ...new Set(
        inputs.flatMap((input) => [...(memory?.frame.localRoots.get(input.ordinal) ?? [])]),
      ),
    ]
      .sort((left, right) => left - right)
      .flatMap(reloadRoot)
  const callableCaptureRange = (
    cleanup: Extract<CleanupPlan.CleanupPlan, { readonly _tag: 'CallableCleanup' }>,
    capture: number,
  ): LayoutPlan.CallableCaptureRange => {
    if (cleanup.environment._tag !== 'CallableEnvironmentIdentity')
      throw new RangeError('Wasm callable cleanup lost its specialized environment')
    const range = LayoutPlan.callableCaptureRange(plan, cleanup.environment.identity, capture)
    if (range !== undefined) return range
    throw new RangeError('Wasm callable cleanup lost an owned capture lane')
  }
  const effectEnvironmentForCleanup = (
    cleanup: Extract<CleanupPlan.CleanupPlan, { readonly _tag: 'EffectCleanup' }>,
  ): Extract<LayoutPlan.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }> => {
    const environment = plan.effectEnvironments.find(
      (
        candidate,
      ): candidate is Extract<
        LayoutPlan.EffectEnvironment,
        { readonly _tag: 'EffectEnvironment' }
      > =>
        candidate._tag === 'EffectEnvironment' &&
        Hir.sameExecutableSite(candidate.site, cleanup.site) &&
        SilkType.equals(candidate.effect, cleanup.type),
    )
    if (environment !== undefined) return environment
    throw new RangeError('Wasm Effect cleanup lost its exact stored environment')
  }
  const hookReleaseWalk = (
    cleanup: CleanupPlan.CleanupPlan,
    addressAt: (byteOffset: number) => ReadonlyArray<Instr.Instr>,
  ): ReadonlyArray<Instr.Instr> => {
    if (!CleanupPlan.hasHook(cleanup)) return []
    if (memory === undefined) throw new RangeError('Wasm hook cleanup has no private memory')
    return WasmCleanup.emitCleanupWalk(cleanup, 0, (plan_, byteOffset) => {
      switch (plan_._tag) {
        case 'HookCleanup':
          return Object.freeze({
            before: Object.freeze([
              ...addressAt(byteOffset),
              Instr.call(resolve(plan_.hook, plan_.typeArguments)),
            ]),
            children: Object.freeze([Object.freeze({ cleanup: plan_.inner, state: byteOffset })]),
          })
        case 'CallableCleanup':
          return Object.freeze({
            children: Object.freeze(
              plan_.slots.map((slot) =>
                Object.freeze({
                  cleanup: slot.cleanup,
                  state: byteOffset + callableCaptureRange(plan_, slot.ordinal).byteOffset,
                }),
              ),
            ),
          })
        case 'EffectCleanup': {
          const environment = effectEnvironmentForCleanup(plan_)
          return Object.freeze({
            children: Object.freeze(
              plan_.slots.flatMap((slot) => {
                const field = environment.fields.at(slot.ordinal)
                return field === undefined
                  ? []
                  : [
                      Object.freeze({
                        cleanup: slot.cleanup,
                        state: byteOffset + field.offset,
                      }),
                    ]
              }),
            ),
          })
        }
        case 'StructCleanup': {
          const representation = LayoutPlan.entry(memory.plan, plan_.type)?.representation
          if (representation?._tag !== 'Aggregate') return Object.freeze({})
          return Object.freeze({
            children: Object.freeze(
              plan_.fields.flatMap((field) => {
                if (!CleanupPlan.hasHook(field.cleanup)) return []
                const layoutField = representation.fields.find(
                  (candidate) =>
                    candidate.id.ordinal === field.field.ordinal &&
                    candidate.id.struct.ordinal === field.field.struct.ordinal &&
                    candidate.id.struct.sourceId === field.field.struct.sourceId,
                )
                return layoutField === undefined
                  ? []
                  : [
                      Object.freeze({
                        cleanup: field.cleanup,
                        state: byteOffset + layoutField.offset,
                      }),
                    ]
              }),
            ),
          })
        }
        case 'ArrayCleanup': {
          if (!CleanupPlan.hasHook(plan_.element)) return Object.freeze({})
          const representation = LayoutPlan.entry(memory.plan, plan_.type)?.representation
          if (representation?._tag !== 'Repeated') return Object.freeze({})
          return Object.freeze({
            children: Object.freeze(
              Array.from({ length: plan_.length }, (_, index) =>
                Object.freeze({
                  cleanup: plan_.element,
                  state: byteOffset + index * representation.stride,
                }),
              ),
            ),
          })
        }
        case 'UnionCleanup': {
          const representation = LayoutPlan.entry(memory.plan, plan_.type)?.representation
          if (representation?._tag !== 'Union') return Object.freeze({})
          return Object.freeze({
            children: Object.freeze(
              plan_.cases.flatMap((caseEntry) =>
                CleanupPlan.hasHook(caseEntry.cleanup)
                  ? [
                      Object.freeze({
                        cleanup: caseEntry.cleanup,
                        state: byteOffset + representation.payloadOffset,
                        wrap: (instructions: ReadonlyArray<Instr.Instr>) =>
                          Object.freeze([
                            ...addressAt(byteOffset),
                            Instr.memoryAccess('i32.load', memory.memory),
                            Instr.i32Const(caseEntry.ordinal),
                            Instr.op('i32.eq'),
                            Instr.ifElse(Instr.emptyBlockType, instructions, []),
                          ]),
                      }),
                    ]
                  : [],
              ),
            ),
          })
        }
        default:
          return Object.freeze({})
      }
    })
  }
  /**
   * Runs every Drop hook one cleanup plan invokes: the owner materializes to its frame root,
   * each hook receives the address of its (possibly nested) value, and the owner's slots
   * reload afterward. The blocks the plan still owns are reclaimed separately, after the hooks.
   */
  const hookReleaseInstructions = (
    cleanup: CleanupPlan.CleanupPlan,
    local: Mir.LocalId,
  ): ReadonlyArray<Instr.Instr> => {
    if (!CleanupPlan.hasHook(cleanup)) return []
    if (memory === undefined) throw new RangeError('Wasm hook cleanup has no private memory')
    const localType = layout.types.at(local.ordinal)
    if (localType?._tag === 'EffectBorrow') {
      const pointer = layout.borrowPointers.get(local.ordinal)
      if (pointer === undefined) throw new RangeError('Wasm hook cleanup lost its borrow pointer')
      const rootLanes = layout.lanes.at(local.ordinal) ?? []
      const rootSlots = slots(local)
      const laneInstructions = (instruction: 'load' | 'store'): ReadonlyArray<Instr.Instr> =>
        rootLanes.flatMap((lane, ordinal) => {
          const offset = LayoutVerify.laneOffset(memory.plan, localType.type, lane.path)
          const slot = rootSlots.at(ordinal)
          if (offset === undefined || slot === undefined)
            throw new RangeError('Wasm hook cleanup lost a borrowed lane')
          const address = [
            Instr.localGet(pointer),
            ...(offset === 0 ? [] : [Instr.i32Const(offset), Instr.op('i32.add')]),
          ]
          return instruction === 'store'
            ? [
                ...address,
                Instr.localGet(slot),
                Instr.memoryAccess(laneStoreMnemonic(memory.plan, lane), memory.memory),
              ]
            : [
                ...address,
                Instr.memoryAccess(laneLoadMnemonic(memory.plan, lane), memory.memory),
                Instr.localSet(slot),
              ]
        })
      const hooks = hookReleaseWalk(cleanup, (nestedOffset) => [
        Instr.localGet(pointer),
        ...(nestedOffset === 0 ? [] : [Instr.i32Const(nestedOffset), Instr.op('i32.add')]),
      ])
      return [...laneInstructions('store'), ...hooks, ...laneInstructions('load')]
    }
    const planned = memory.frame.roots.get(local.ordinal)
    if (planned === undefined) throw new RangeError('Wasm hook cleanup lost its frame root')
    const walk = (plan_: CleanupPlan.CleanupPlan, byteOffset: number) =>
      hookReleaseWalk(plan_, (nestedOffset) =>
        frameAddress(planned.offset + byteOffset + nestedOffset),
      )
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
              Instances.effectIdentity(candidate.instance, candidate.site) === field.effectIdentity,
          )
          return nested?._tag === 'EffectEnvironment'
            ? environmentOffsets(nested, base + field.offset)
            : []
        }
        const shape = LayoutPlan.callingShape(memory.plan, field.type)
        return (
          shape?.lanes.flatMap((lane) => {
            const offset = LayoutVerify.laneOffset(memory.plan, field.type, lane.path)
            return offset === undefined ? [] : [base + field.offset + offset]
          }) ?? []
        )
      })
    const callableEnvironmentOffsets = (
      environment: Extract<
        LayoutPlan.CallableEnvironment,
        { readonly _tag: 'CallableEnvironment' }
      >,
      base = 0,
    ): ReadonlyArray<number> =>
      environment.fields.flatMap((field) => {
        if (field.representation === 'Borrow') return [base + field.offset]
        if (field.callableIdentity?.environment !== undefined) {
          const nested = LayoutPlan.callableEnvironmentByIdentity(
            memory.plan,
            field.callableIdentity.environment,
          )
          return nested?._tag === 'CallableEnvironment'
            ? callableEnvironmentOffsets(nested, base + field.offset)
            : []
        }
        const shape = LayoutPlan.callingShape(memory.plan, field.type)
        return (
          shape?.lanes.flatMap((lane) => {
            const offset = LayoutVerify.laneOffset(memory.plan, field.type, lane.path)
            return offset === undefined ? [] : [base + field.offset + offset]
          }) ?? []
        )
      })
    if (
      cleanup._tag === 'CallableCleanup' &&
      localType?._tag === 'CallableValue' &&
      localType.environment !== undefined
    ) {
      const offsets = callableEnvironmentOffsets(localType.environment)
      const rootSlots = slots(local)
      const rootLanes = layout.lanes.at(local.ordinal) ?? []
      const stores = rootLanes.flatMap((lane, ordinal) => {
        const offset = offsets.at(ordinal)
        const source = rootSlots.at(ordinal)
        if (offset === undefined || source === undefined)
          throw new RangeError('Wasm callable cleanup lost an environment lane')
        return [
          ...frameAddress(planned.offset + offset),
          Instr.localGet(source),
          Instr.memoryAccess(laneStoreMnemonic(memory.plan, lane), memory.memory),
        ]
      })
      const hooks = cleanup.slots.flatMap((slot) => {
        const field = localType.environment?.fields.at(slot.ordinal)
        return field === undefined ? [] : walk(slot.cleanup, field.offset)
      })
      const reloads = rootLanes.flatMap((lane, ordinal) => {
        const offset = offsets.at(ordinal)
        const destination = rootSlots.at(ordinal)
        if (offset === undefined || destination === undefined)
          throw new RangeError('Wasm callable cleanup lost an environment lane')
        return [
          ...frameAddress(planned.offset + offset),
          Instr.memoryAccess(laneLoadMnemonic(memory.plan, lane), memory.memory),
          Instr.localSet(destination),
        ]
      })
      return [...stores, ...hooks, ...reloads]
    }
    if (cleanup._tag === 'EffectCleanup' && localType?._tag === 'EffectValue') {
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
    if (cleanup._tag === 'EffectCompositeCleanup' && localType?._tag === 'EffectComposite') {
      const rootSlots = slots(local)
      const choice = rootSlots.at(0)
      if (choice === undefined) throw new RangeError('Wasm Effect composite hook lost its tag')
      return cleanup.alternatives.flatMap((alternativeCleanup, ordinal) => {
        if (!CleanupPlan.hasHook(alternativeCleanup)) return []
        if (alternativeCleanup._tag !== 'EffectCleanup') {
          throw new RangeError('Wasm Effect composite hook lost its alternative environment')
        }
        const alternative = localType.alternatives.at(ordinal)
        if (alternative === undefined)
          throw new RangeError('Wasm Effect composite hook lost its alternative type')
        const offsets = environmentOffsets(alternative.environment)
        const lanes = laneKindsOf(plan, alternative)
        const payloadSlots = rootSlots.slice(1)
        const stores = lanes.flatMap((lane, laneOrdinal) => {
          const offset = offsets.at(laneOrdinal)
          const source = payloadSlots.at(laneOrdinal)
          if (offset === undefined || source === undefined) {
            throw new RangeError('Wasm Effect composite hook lost an environment lane')
          }
          const physical = layout.physicalTypes.at(source)
          return [
            ...frameAddress(planned.offset + offset),
            Instr.localGet(source),
            ...(physical === undefined ? [] : laneBridge(physical, laneValueType(plan, lane))),
            Instr.memoryAccess(laneStoreMnemonic(memory.plan, lane), memory.memory),
          ]
        })
        const hooks = alternativeCleanup.slots.flatMap((slot) => {
          const field = alternative.environment.fields.at(slot.ordinal)
          return field === undefined ? [] : walk(slot.cleanup, field.offset)
        })
        const reloads = lanes.flatMap((lane, laneOrdinal) => {
          const offset = offsets.at(laneOrdinal)
          const destination = payloadSlots.at(laneOrdinal)
          if (offset === undefined || destination === undefined) {
            throw new RangeError('Wasm Effect composite hook lost a reload lane')
          }
          const physical = layout.physicalTypes.at(destination)
          return [
            ...frameAddress(planned.offset + offset),
            Instr.memoryAccess(laneLoadMnemonic(memory.plan, lane), memory.memory),
            ...(physical === undefined ? [] : laneBridge(laneValueType(plan, lane), physical)),
            Instr.localSet(destination),
          ]
        })
        return [
          Instr.localGet(choice),
          Instr.i32Const(ordinal),
          Instr.op('i32.eq'),
          Instr.ifElse(Instr.emptyBlockType, [...stores, ...hooks, ...reloads], []),
        ]
      })
    }
    return [...materializeRoot(local), ...walk(cleanup, 0), ...reloadRoot(local.ordinal)]
  }
  const hookReleaseAtAddress = (
    cleanup: CleanupPlan.CleanupPlan,
    address: number,
    rootOffset = 0,
  ): ReadonlyArray<Instr.Instr> => {
    const addressAt = (byteOffset: number): ReadonlyArray<Instr.Instr> => [
      Instr.localGet(address),
      ...(rootOffset + byteOffset === 0
        ? []
        : [Instr.i32Const(rootOffset + byteOffset), Instr.op('i32.add')]),
    ]
    return hookReleaseWalk(cleanup, addressAt)
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
    cleanup: CleanupPlan.CleanupPlan,
    values: ReadonlyArray<ReadonlyArray<Instr.Instr>>,
  ): ReadonlyArray<Instr.Instr> => {
    if (!CleanupPlan.reclaims(cleanup)) return []
    return WasmCleanup.emitCleanupWalk(cleanup, values, (plan_, currentValues) => {
      if (!CleanupPlan.reclaims(plan_)) return Object.freeze({})
      switch (plan_._tag) {
        case 'ExecutionCleanup': {
          const value = currentValues.at(0)
          const scratch = layout.executionCleanupScratch.at(0)
          if (value === undefined) {
            throw new RangeError('Wasm Execution cleanup lost its package pointer')
          }
          if (scratch === undefined) {
            throw new RangeError('Wasm Execution cleanup lost its package authority local')
          }
          return Object.freeze({
            before: Object.freeze([
              ...value,
              Instr.localSet(scratch.package),
              ...releaseExecutionBase(scratch.package),
            ]),
          })
        }
        case 'WakeCleanup': {
          const value = currentValues.at(0)
          const scratch = layout.executionCleanupScratch.at(0)
          if (value === undefined)
            throw new RangeError('Wasm Wake cleanup lost its package pointer')
          if (scratch === undefined)
            throw new RangeError('Wasm Wake cleanup lost its package authority local')
          return Object.freeze({
            before: Object.freeze([
              ...value,
              Instr.localSet(scratch.package),
              ...releaseWakeBase(scratch.package),
            ]),
          })
        }
        case 'LocalSharedCoreCleanup': {
          const base = currentValues.at(0)
          const elementLayout = LayoutPlan.entry(plan, plan_.element)
          const block =
            elementLayout === undefined
              ? undefined
              : LocalSharedControlBlock.plan(plan.target, plan_.element, elementLayout)
          if (base === undefined || block?._tag !== 'LocalSharedControlBlockPlan')
            throw new RangeError('Wasm local-shared cleanup lost its control-block lanes')
          const context = requireMemory()
          const decrement = [
            ...base,
            ...base,
            Instr.memoryAccess('i32.load', context.memory, { offset: block.strongOffset }),
            Instr.i32Const(1),
            Instr.op('i32.sub'),
            Instr.memoryAccess('i32.store', context.memory, { offset: block.strongOffset }),
          ]
          const last = [
            ...semanticLanesOf(plan_.element).flatMap((lane) => {
              const offset = LayoutVerify.laneOffset(plan, plan_.element, lane.path)
              if (offset === undefined)
                throw new RangeError('Wasm local-shared cleanup lost a payload lane')
              return [
                ...base,
                Instr.memoryAccess(laneLoadMnemonic(plan, lane), context.memory, {
                  offset: block.valueOffset + offset,
                }),
              ]
            }),
            Instr.call(resolve(LocalSharedPayloadCleanup.declaration, [plan_.element])),
            Instr.op('drop'),
            ...base,
            Instr.memoryAccess('i32.load', context.memory, {
              offset:
                block.allocationOffset + aggregateFieldOffset(SilkType.allocation, '$context'),
            }),
            Instr.call(requireRelease()),
          ]
          return Object.freeze({
            before: Object.freeze([
              ...base,
              Instr.memoryAccess('i32.load', context.memory, { offset: block.strongOffset }),
              Instr.i32Const(1),
              Instr.op('i32.gt_u'),
              Instr.ifElse(Instr.emptyBlockType, decrement, last),
            ]),
          })
        }
        case 'AllocationCleanup':
        case 'RawBufferCleanup': {
          const context = currentValues.at(4)
          if (context === undefined) {
            throw new RangeError('Wasm allocation cleanup lost its reclaim context')
          }
          return Object.freeze({
            before: Object.freeze([...context, Instr.call(requireRelease())]),
          })
        }
        case 'HookCleanup':
          return Object.freeze({
            children: Object.freeze([
              Object.freeze({ cleanup: plan_.inner, state: currentValues }),
            ]),
          })
        case 'CallableCleanup':
          return Object.freeze({
            children: Object.freeze(
              plan_.slots.map((slot) => {
                const range = callableCaptureRange(plan_, slot.ordinal)
                return Object.freeze({
                  cleanup: slot.cleanup,
                  state: currentValues.slice(range.laneOffset, range.laneOffset + range.laneCount),
                })
              }),
            ),
          })
        case 'EffectCleanup':
          return Object.freeze({
            children: Object.freeze(
              plan_.slots.map((slot) =>
                Object.freeze({
                  cleanup: slot.cleanup,
                  state: currentValues.slice(slot.laneOffset, slot.laneOffset + slot.laneCount),
                }),
              ),
            ),
          })
        case 'EffectCompositeCleanup': {
          const tag = currentValues.at(0)
          if (tag === undefined) throw new RangeError('Wasm Effect composite cleanup lost its tag')
          return Object.freeze({
            children: Object.freeze(
              plan_.alternatives.map((alternative, ordinal) =>
                Object.freeze({
                  cleanup: alternative,
                  state: currentValues.slice(1),
                  wrap: (instructions: ReadonlyArray<Instr.Instr>) =>
                    instructions.length === 0
                      ? Object.freeze([])
                      : Object.freeze([
                          ...tag,
                          Instr.i32Const(ordinal),
                          Instr.op('i32.eq'),
                          Instr.ifElse(Instr.emptyBlockType, instructions, []),
                        ]),
                }),
              ),
            ),
          })
        }
        case 'StructCleanup': {
          const lanes = semanticLanesOf(plan_.type)
          return Object.freeze({
            children: Object.freeze(
              plan_.fields.map((field) =>
                Object.freeze({
                  cleanup: field.cleanup,
                  state: lanes.flatMap((lane, ordinal) => {
                    const first = lane.path.at(0)
                    const value = currentValues.at(ordinal)
                    return first !== undefined &&
                      first._tag === 'FieldId' &&
                      value !== undefined &&
                      first.ordinal === field.field.ordinal &&
                      first.struct.ordinal === field.field.struct.ordinal &&
                      first.struct.sourceId === field.field.struct.sourceId
                      ? [value]
                      : []
                  }),
                }),
              ),
            ),
          })
        }
        case 'ArrayCleanup': {
          const lanes = semanticLanesOf(plan_.type)
          return Object.freeze({
            children: Object.freeze(
              Array.from({ length: plan_.length }, (_, index) =>
                Object.freeze({
                  cleanup: plan_.element,
                  state: lanes.flatMap((lane, ordinal) => {
                    const first = lane.path.at(0)
                    const value = currentValues.at(ordinal)
                    return first !== undefined &&
                      first._tag === 'ElementSelector' &&
                      first.index === index &&
                      value !== undefined
                      ? [value]
                      : []
                  }),
                }),
              ),
            ),
          })
        }
        case 'UnionCleanup': {
          const shape = LayoutPlan.callingShape(plan, plan_.type)
          const tag = currentValues.at(0)
          if (shape === undefined || tag === undefined) {
            throw new RangeError('Wasm union cleanup lost its shape')
          }
          return Object.freeze({
            children: Object.freeze(
              plan_.cases.flatMap((caseEntry) => {
                const physical = LayoutPlan.memberFieldSlots(shape, caseEntry.member, [])
                const memberLanes = semanticLanesOf(caseEntry.member)
                if (physical === undefined || physical.length !== memberLanes.length) return []
                const selected = physical.flatMap((ordinal, index) => {
                  const value = currentValues.at(ordinal)
                  const physicalLane = shape.lanes.at(ordinal)
                  const memberLane = memberLanes.at(index)
                  return value === undefined ||
                    physicalLane === undefined ||
                    memberLane === undefined
                    ? []
                    : [
                        Object.freeze([
                          ...value,
                          ...laneBridge(
                            laneValueType(plan, physicalLane),
                            laneValueType(plan, memberLane),
                          ),
                        ]),
                      ]
                })
                return selected.length !== memberLanes.length
                  ? []
                  : [
                      Object.freeze({
                        cleanup: caseEntry.cleanup,
                        state: selected,
                        wrap: (instructions: ReadonlyArray<Instr.Instr>) =>
                          instructions.length === 0
                            ? Object.freeze([])
                            : Object.freeze([
                                ...tag,
                                Instr.i32Const(caseEntry.ordinal),
                                Instr.op('i32.eq'),
                                Instr.ifElse(Instr.emptyBlockType, instructions, []),
                              ]),
                      }),
                    ]
              }),
            ),
          })
        }
        default:
          return Object.freeze({})
      }
    })
  }
  /** The same reclaim walk for a value the backend only holds the address of. */
  const reclaimReleaseAtAddress = (
    cleanup: CleanupPlan.CleanupPlan,
    address: number,
    byteOffset = 0,
  ): ReadonlyArray<Instr.Instr> => {
    if (!CleanupPlan.reclaims(cleanup)) return []
    if (memory === undefined) throw new RangeError('Wasm slot reclaim has no private memory')
    return WasmCleanup.emitCleanupWalk(cleanup, byteOffset, (plan_, currentOffset) => {
      if (!CleanupPlan.reclaims(plan_)) return Object.freeze({})
      switch (plan_._tag) {
        case 'ExecutionCleanup': {
          const scratch = layout.executionCleanupScratch.at(0)
          if (scratch === undefined) {
            throw new RangeError('Wasm Execution cleanup lost its package authority local')
          }
          return Object.freeze({
            before: Object.freeze([
              ...loadAt(address, currentOffset),
              Instr.localSet(scratch.package),
              ...releaseExecutionBase(scratch.package),
            ]),
          })
        }
        case 'WakeCleanup': {
          const scratch = layout.executionCleanupScratch.at(0)
          if (scratch === undefined) {
            throw new RangeError('Wasm Wake cleanup lost its package authority local')
          }
          return Object.freeze({
            before: Object.freeze([
              ...loadAt(address, currentOffset),
              Instr.localSet(scratch.package),
              ...releaseWakeBase(scratch.package),
            ]),
          })
        }
        case 'AllocationCleanup':
        case 'RawBufferCleanup': {
          const contextOffset = SilkType.isRawBuffer(plan_.type)
            ? aggregateFieldOffset(plan_.type, '$allocation') +
              aggregateFieldOffset(SilkType.allocation, '$context')
            : aggregateFieldOffset(SilkType.allocation, '$context')
          return Object.freeze({
            before: Object.freeze([
              ...loadAt(address, currentOffset + contextOffset),
              Instr.call(requireRelease()),
            ]),
          })
        }
        case 'HookCleanup':
          return Object.freeze({
            children: Object.freeze([
              Object.freeze({ cleanup: plan_.inner, state: currentOffset }),
            ]),
          })
        case 'CallableCleanup':
          return Object.freeze({
            children: Object.freeze(
              plan_.slots.map((slot) =>
                Object.freeze({
                  cleanup: slot.cleanup,
                  state: currentOffset + callableCaptureRange(plan_, slot.ordinal).byteOffset,
                }),
              ),
            ),
          })
        case 'EffectCleanup': {
          const environment = effectEnvironmentForCleanup(plan_)
          return Object.freeze({
            children: Object.freeze(
              plan_.slots.flatMap((slot) => {
                const field = environment.fields.at(slot.ordinal)
                return field === undefined
                  ? []
                  : [
                      Object.freeze({
                        cleanup: slot.cleanup,
                        state: currentOffset + field.offset,
                      }),
                    ]
              }),
            ),
          })
        }
        case 'StructCleanup': {
          const representation = LayoutPlan.entry(memory.plan, plan_.type)?.representation
          if (representation?._tag !== 'Aggregate') return Object.freeze({})
          return Object.freeze({
            children: Object.freeze(
              plan_.fields.flatMap((field) => {
                const layoutField = representation.fields.find(
                  (candidate) =>
                    candidate.id.ordinal === field.field.ordinal &&
                    candidate.id.struct.ordinal === field.field.struct.ordinal &&
                    candidate.id.struct.sourceId === field.field.struct.sourceId,
                )
                return layoutField === undefined
                  ? []
                  : [
                      Object.freeze({
                        cleanup: field.cleanup,
                        state: currentOffset + layoutField.offset,
                      }),
                    ]
              }),
            ),
          })
        }
        case 'ArrayCleanup': {
          const representation = LayoutPlan.entry(memory.plan, plan_.type)?.representation
          if (representation?._tag !== 'Repeated') return Object.freeze({})
          return Object.freeze({
            children: Object.freeze(
              Array.from({ length: plan_.length }, (_, index) =>
                Object.freeze({
                  cleanup: plan_.element,
                  state: currentOffset + index * representation.stride,
                }),
              ),
            ),
          })
        }
        case 'UnionCleanup': {
          const representation = LayoutPlan.entry(memory.plan, plan_.type)?.representation
          if (representation?._tag !== 'Union') return Object.freeze({})
          return Object.freeze({
            children: Object.freeze(
              plan_.cases.map((caseEntry) =>
                Object.freeze({
                  cleanup: caseEntry.cleanup,
                  state: currentOffset + representation.payloadOffset,
                  wrap: (instructions: ReadonlyArray<Instr.Instr>) =>
                    instructions.length === 0
                      ? Object.freeze([])
                      : Object.freeze([
                          ...loadAt(address, currentOffset),
                          Instr.i32Const(caseEntry.ordinal),
                          Instr.op('i32.eq'),
                          Instr.ifElse(Instr.emptyBlockType, instructions, []),
                        ]),
                }),
              ),
            ),
          })
        }
        default:
          return Object.freeze({})
      }
    })
  }
  /**
   * One owned value's complete release: its Drop hooks, then the blocks its reclaim tickets still
   * hold. Both halves are conditional on the plan actually carrying them, so a bare allocation
   * drop — which invokes no hook at all — still emits its reclaim.
   */
  const releaseInstructions = (
    cleanup: CleanupPlan.CleanupPlan,
    local: Mir.LocalId,
  ): ReadonlyArray<Instr.Instr> =>
    cleanup._tag === 'ExecutionCleanup'
      ? releaseExecutionBase(scalar(local))
      : cleanup._tag === 'WakeCleanup'
        ? releaseWakeBase(scalar(local))
        : WasmCleanup.release(
            hookReleaseInstructions(cleanup, local),
            reclaimReleaseInstructions(
              cleanup,
              slots(local).map((slot) => Object.freeze([Instr.localGet(slot)])),
            ),
          )
  function releaseWakeBase(base: number): ReadonlyArray<Instr.Instr> {
    const releasePackage = (ordinal: number): ReadonlyArray<Instr.Instr> => {
      const package_ = plan.executionPackages.plans.at(ordinal)
      const controlOffset =
        package_ === undefined ? undefined : executionComponentOffset(package_, 'WakeControl')
      const allocationOffset =
        package_ === undefined
          ? undefined
          : executionComponentOffset(package_, 'AllocationAuthority')
      if (package_ === undefined || controlOffset === undefined || allocationOffset === undefined)
        return [Instr.op('unreachable')]
      const allocationCleanup: Extract<
        CleanupPlan.CleanupPlan,
        { readonly _tag: 'AllocationCleanup' }
      > = Object.freeze({
        _tag: 'AllocationCleanup',
        type: SilkType.allocation,
        ticket: 'ActiveReclaimTicket',
      })
      return [
        Instr.localGet(base),
        Instr.memoryAccess('i32.load', requireMemory().memory, { offset: controlOffset }),
        Instr.i32Const(6),
        Instr.op('i32.eq'),
        Instr.ifElse(
          Instr.emptyBlockType,
          releaseAtAddress(allocationCleanup, base, allocationOffset),
          [
            Instr.localGet(base),
            Instr.i32Const(6),
            Instr.memoryAccess('i32.store', requireMemory().memory, { offset: controlOffset }),
          ],
        ),
      ]
    }
    const select = (ordinal: number): ReadonlyArray<Instr.Instr> =>
      ordinal >= plan.executionPackages.plans.length
        ? [Instr.op('unreachable')]
        : [
            Instr.localGet(base),
            Instr.memoryAccess('i32.load', requireMemory().memory, {
              offset: plan.target.pointerSize,
            }),
            Instr.i32Const(ordinal),
            Instr.op('i32.eq'),
            Instr.ifElse(Instr.emptyBlockType, releasePackage(ordinal), select(ordinal + 1)),
          ]
    return select(0)
  }
  function releaseExecutionBase(base: number, cleanupDepth = 0): ReadonlyArray<Instr.Instr> {
    const cleanupScratch = layout.executionCleanupScratch.at(cleanupDepth)
    if (cleanupScratch === undefined) {
      throw new RangeError(`Wasm Execution cleanup exceeded depth ${cleanupDepth}`)
    }
    const releasePackage = (ordinal: number): ReadonlyArray<Instr.Instr> => {
      const package_ = plan.executionPackages.plans.at(ordinal)
      const cleanup =
        package_ === undefined ? undefined : executionPackageCleanups.get(package_.provenance)
      const bodyOffset =
        package_ === undefined ? undefined : executionComponentOffset(package_, 'BodyEnvironment')
      const allocationOffset =
        package_ === undefined
          ? undefined
          : executionComponentOffset(package_, 'AllocationAuthority')
      if (
        package_ === undefined ||
        cleanup === undefined ||
        bodyOffset === undefined ||
        allocationOffset === undefined
      )
        return [Instr.op('unreachable')]
      const callbackOffset = executionComponentOffset(package_, 'EndpointCallback')
      const endpointOffset = executionComponentOffset(package_, 'EndpointState')
      const allocationCleanup: Extract<
        CleanupPlan.CleanupPlan,
        { readonly _tag: 'AllocationCleanup' }
      > = Object.freeze({
        _tag: 'AllocationCleanup',
        type: SilkType.allocation,
        ticket: 'ActiveReclaimTicket',
      })
      const cleanupEndpoint = [
        ...(callbackOffset === undefined
          ? []
          : releaseAtAddress(cleanup.callback, base, callbackOffset, 0, cleanupDepth + 1)),
        ...(endpointOffset === undefined
          ? []
          : releaseAtAddress(cleanup.endpoint, base, endpointOffset, 0, cleanupDepth + 1)),
      ]
      const cleanupBody = releaseAtAddress(cleanup.body, base, bodyOffset, 0, cleanupDepth + 1)
      const controlOffset = executionComponentOffset(package_, 'WakeControl')
      const continuationOffset = executionComponentOffset(package_, 'InitialContinuationSegment')
      const runtime = emitter.suspensionRuntime
      const frame = cleanupScratch.frame
      const cleanupCurrentFrame = (ordinal: number): ReadonlyArray<Instr.Instr> => {
        if (runtime === undefined) return [Instr.op('unreachable')]
        const entries = [...runtime.frameCleanups.entries()].sort(([left], [right]) => left - right)
        const selected = entries.at(ordinal)
        if (selected === undefined) return [Instr.op('unreachable')]
        const [id, cleanup] = selected
        return [
          Instr.localGet(frame),
          Instr.memoryAccess('i32.load', runtime.frameMemory, {
            offset: plan.target.pointerSize,
          }),
          Instr.i32Const(id),
          Instr.op('i32.eq'),
          Instr.ifElse(
            Instr.emptyBlockType,
            [Instr.localGet(frame), Instr.call(cleanup)],
            cleanupCurrentFrame(ordinal + 1),
          ),
        ]
      }
      const cleanupFrames =
        continuationOffset === undefined || runtime === undefined
          ? []
          : [
              Instr.localGet(base),
              Instr.memoryAccess('i32.load', runtime.memory, { offset: continuationOffset }),
              Instr.localSet(frame),
              Instr.block(Instr.emptyBlockType, [
                Instr.loop(Instr.emptyBlockType, [
                  Instr.localGet(frame),
                  Instr.op('i32.eqz'),
                  Instr.brIf(1),
                  ...cleanupCurrentFrame(0),
                  Instr.localGet(base),
                  Instr.localGet(frame),
                  Instr.memoryAccess('i32.load', runtime.frameMemory),
                  Instr.memoryAccess('i32.store', runtime.memory, { offset: continuationOffset }),
                  Instr.localGet(frame),
                  Instr.globalGet(runtime.freeFrameHead),
                  Instr.memoryAccess('i32.store', runtime.frameMemory),
                  Instr.localGet(frame),
                  Instr.globalSet(runtime.freeFrameHead),
                  Instr.localGet(base),
                  Instr.memoryAccess('i32.load', runtime.memory, { offset: continuationOffset }),
                  Instr.localSet(frame),
                  Instr.br(0),
                ]),
              ]),
            ]
      const cleanupInitial = [
        ...cleanupEndpoint,
        ...cleanupBody,
        ...releaseAtAddress(allocationCleanup, base, allocationOffset, 0, cleanupDepth + 1),
      ]
      const cleanupActivated = [
        ...cleanupEndpoint,
        ...cleanupFrames,
        ...releaseAtAddress(allocationCleanup, base, allocationOffset, 0, cleanupDepth + 1),
      ]
      const cleanupPackage = [
        Instr.localGet(base),
        Instr.memoryAccess('i32.load', requireMemory().memory),
        Instr.i32Const(ExecutionTransition.tagOf('Initial')),
        Instr.op('i32.eq'),
        Instr.ifElse(Instr.emptyBlockType, cleanupInitial, cleanupActivated),
      ]
      const cancelDormant =
        controlOffset === undefined
          ? [Instr.op('unreachable')]
          : [
              Instr.localGet(base),
              Instr.i32Const(ExecutionTransition.tagOf('Destroyed')),
              Instr.memoryAccess('i32.store', requireMemory().memory),
              Instr.localGet(base),
              Instr.i32Const(6),
              Instr.memoryAccess('i32.store', requireMemory().memory, {
                offset: controlOffset,
              }),
              ...cleanupEndpoint,
              ...cleanupFrames,
            ]
      const cancelRegistration =
        controlOffset === undefined
          ? [Instr.op('unreachable')]
          : [
              Instr.localGet(base),
              Instr.memoryAccess('i32.load', requireMemory().memory, { offset: controlOffset }),
              Instr.i32Const(2),
              Instr.op('i32.eq'),
              Instr.ifElse(
                Instr.emptyBlockType,
                [
                  Instr.localGet(base),
                  Instr.i32Const(ExecutionTransition.tagOf('DestroyPending')),
                  Instr.memoryAccess('i32.store', requireMemory().memory),
                  Instr.localGet(base),
                  Instr.i32Const(8),
                  Instr.memoryAccess('i32.store', requireMemory().memory, {
                    offset: controlOffset,
                  }),
                ],
                cancelDormant,
              ),
            ]
      return [
        Instr.localGet(base),
        Instr.memoryAccess('i32.load', requireMemory().memory),
        Instr.i32Const(ExecutionTransition.tagOf('Notifying')),
        Instr.op('i32.eq'),
        Instr.ifElse(
          Instr.emptyBlockType,
          [
            Instr.localGet(base),
            Instr.i32Const(ExecutionTransition.tagOf('DestroyPending')),
            Instr.memoryAccess('i32.store', requireMemory().memory),
            ...(controlOffset === undefined
              ? []
              : [
                  Instr.localGet(base),
                  Instr.i32Const(8),
                  Instr.memoryAccess('i32.store', requireMemory().memory, {
                    offset: controlOffset,
                  }),
                ]),
          ],
          [
            Instr.localGet(base),
            Instr.memoryAccess('i32.load', requireMemory().memory),
            Instr.i32Const(ExecutionTransition.tagOf('Running')),
            Instr.op('i32.eq'),
            Instr.ifElse(Instr.emptyBlockType, cancelRegistration, [
              Instr.localGet(base),
              Instr.memoryAccess('i32.load', requireMemory().memory),
              Instr.i32Const(ExecutionTransition.tagOf('Dormant')),
              Instr.op('i32.eq'),
              Instr.ifElse(Instr.emptyBlockType, cancelDormant, cleanupPackage),
            ]),
          ],
        ),
      ]
    }
    const select = (ordinal: number): ReadonlyArray<Instr.Instr> =>
      ordinal >= plan.executionPackages.plans.length
        ? [Instr.op('unreachable')]
        : [
            Instr.localGet(base),
            Instr.memoryAccess('i32.load', requireMemory().memory, {
              offset: plan.target.pointerSize,
            }),
            Instr.i32Const(ordinal),
            Instr.op('i32.eq'),
            Instr.ifElse(Instr.emptyBlockType, releasePackage(ordinal), select(ordinal + 1)),
          ]
    return select(0)
  }
  function releaseAtAddress(
    cleanup: CleanupPlan.CleanupPlan,
    address: number,
    byteOffset = 0,
    localSharedDepth = 0,
    executionCleanupDepth = 0,
  ): ReadonlyArray<Instr.Instr> {
    if (cleanup._tag === 'ExecutionCleanup') {
      const scratch = layout.executionCleanupScratch.at(executionCleanupDepth)
      if (scratch === undefined) {
        throw new RangeError(`Wasm Execution cleanup exceeded depth ${executionCleanupDepth}`)
      }
      return [
        ...loadAt(address, byteOffset),
        Instr.localSet(scratch.package),
        ...releaseExecutionBase(scratch.package, executionCleanupDepth),
      ]
    }
    if (cleanup._tag === 'WakeCleanup') {
      const scratch = layout.executionCleanupScratch.at(executionCleanupDepth)
      if (scratch === undefined) {
        throw new RangeError(`Wasm Wake cleanup exceeded depth ${executionCleanupDepth}`)
      }
      return [
        ...loadAt(address, byteOffset),
        Instr.localSet(scratch.package),
        ...releaseWakeBase(scratch.package),
      ]
    }
    if (cleanup._tag === 'LocalSharedCoreCleanup') {
      const base = layout.localSharedCleanupScratch.at(localSharedDepth)
      const elementLayout = LayoutPlan.entry(plan, cleanup.element)
      const block =
        elementLayout === undefined
          ? undefined
          : LocalSharedControlBlock.plan(plan.target, cleanup.element, elementLayout)
      if (base === undefined || block?._tag !== 'LocalSharedControlBlockPlan') {
        throw new RangeError('Wasm nested local-shared cleanup lost its target cleanup scratch')
      }
      const context = requireMemory()
      const decrement = [
        Instr.localGet(base),
        Instr.localGet(base),
        Instr.memoryAccess('i32.load', context.memory, { offset: block.strongOffset }),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.memoryAccess('i32.store', context.memory, { offset: block.strongOffset }),
      ]
      const last = [
        ...semanticLanesOf(cleanup.element).flatMap((lane) => {
          const offset = LayoutVerify.laneOffset(plan, cleanup.element, lane.path)
          if (offset === undefined)
            throw new RangeError('Wasm local-shared cleanup lost a payload lane')
          return loadAt(base, block.valueOffset + offset, lane)
        }),
        Instr.call(resolve(LocalSharedPayloadCleanup.declaration, [cleanup.element])),
        Instr.op('drop'),
        ...releaseAtAddress(
          cleanup.allocation,
          base,
          block.allocationOffset,
          localSharedDepth,
          executionCleanupDepth,
        ),
      ]
      return [
        ...loadAt(address, byteOffset),
        Instr.localSet(base),
        Instr.localGet(base),
        Instr.memoryAccess('i32.load', context.memory, { offset: block.strongOffset }),
        Instr.i32Const(1),
        Instr.op('i32.gt_u'),
        Instr.ifElse(Instr.emptyBlockType, decrement, last),
      ]
    }
    return WasmCleanup.release(
      hookReleaseAtAddress(cleanup, address, byteOffset),
      reclaimReleaseAtAddress(cleanup, address, byteOffset),
    )
  }
  const flushBorrowRoot = (root: Mir.LocalId): ReadonlyArray<Instr.Instr> => {
    const pointer = layout.borrowPointers.get(root.ordinal)
    if (pointer === undefined) return []
    if (memory === undefined) throw new RangeError('Wasm Effect borrow has no private memory')
    const type = layout.types.at(root.ordinal)
    if (type?._tag !== 'EffectBorrow') throw new RangeError('Wasm Effect borrow lost its type')
    const rootLanes = layout.lanes.at(root.ordinal) ?? []
    const rootSlots = slots(root)
    return rootLanes.flatMap((lane, ordinal) => {
      const offset = LayoutVerify.laneOffset(memory.plan, type.type, lane.path)
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
  return {
    emitter,
    layout,
    plan,
    resolve,
    resolveIndependent,
    memory,
    suspension,
    skipInvocation,
    slots,
    scalar,
    laneBridge,
    transfer,
    zeroFor,
    failurePayload,
    outcomePayload,
    copy,
    frameAddress,
    requireMemory,
    aggregateFieldOffset,
    loadAt,
    storeAt,
    materializeRoot,
    reloadRoot,
    reloadReachableRoots,
    callableCaptureRange,
    hookReleaseInstructions,
    hookReleaseAtAddress,
    semanticLanesOf,
    requireRelease,
    reclaimReleaseInstructions,
    reclaimReleaseAtAddress,
    releaseInstructions,
    releaseExecutionBase,
    releaseAtAddress,
    flushBorrowRoot,
  }
}

type WasmOperationContext = ReturnType<typeof makeOperationContext>

const emitStaticStringOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'StaticString' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, requireMemory } = state

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

const emitStringFromUtf8UncheckedOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'StringFromUtf8Unchecked' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, copy } = state
  return copy(slots(operation.bytes), slots(operation.destination))
}

const emitStringUtf8BytesOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'StringUtf8Bytes' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, copy } = state
  return copy(slots(operation.string), slots(operation.destination))
}

const emitStringByteLengthOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'StringByteLength' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, scalar } = state

  const length = slots(operation.string).at(1)
  if (length === undefined) throw new RangeError('Wasm string lost its byte-length lane')
  return [Instr.localGet(length), Instr.localSet(scalar(operation.destination))]
}

const emitStringEqualsExactOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'StringEqualsExact' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, scalar, requireMemory } = state

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

const emitValidateLayoutOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ValidateLayout' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, scalar } = state

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

const emitRepeatLayoutOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RepeatLayout' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, scalar } = state

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

const emitAllocateOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Allocate' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, slots, requireMemory } = state

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

const emitHostWriteOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'HostWrite' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, memory, slots, scalar } = state

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

const emitOsCallOperation = (
  _operation: Extract<Mir.Operation, { readonly _tag: 'OsCall' }>,
  _state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  throw new RangeError('Target validation allowed a native-only OS operation into Wasm')
}

const emitRawBufferFromOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RawBufferFrom' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, scalar, copy } = state

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

const emitSharedFromAllocationOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'SharedFromAllocation' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, slots, storeAt } = state
  const allocation = slots(operation.allocation)
  const base = allocation.at(0)
  const bytes = allocation.at(1)
  const alignment = allocation.at(2)
  const destination = slots(operation.destination).at(0)
  const allocationShape = LayoutPlan.callingShape(plan, SilkType.allocation)
  const valueShape = LayoutPlan.callingShape(plan, operation.element)
  if (
    base === undefined ||
    bytes === undefined ||
    alignment === undefined ||
    destination === undefined ||
    allocationShape === undefined ||
    valueShape === undefined ||
    allocationShape.lanes.length !== allocation.length
  )
    throw new RangeError('Wasm local-shared initialization lost its planned lanes')
  const instructions: Array<Instr.Instr> = [
    Instr.localGet(bytes),
    Instr.i32Const(operation.block.size),
    Instr.op('i32.ne'),
    Instr.localGet(alignment),
    Instr.i32Const(operation.block.alignment),
    Instr.op('i32.ne'),
    Instr.op('i32.or'),
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    Instr.localGet(base),
    Instr.localSet(destination),
    Instr.localGet(base),
    Instr.i32Const(1),
    Instr.memoryAccess('i32.store', state.requireMemory().memory, {
      offset: operation.block.strongOffset,
    }),
    Instr.localGet(base),
    Instr.i32Const(0),
    Instr.memoryAccess('i32.store', state.requireMemory().memory, {
      offset: operation.block.accessOffset,
    }),
  ]
  for (const [ordinal, lane] of allocationShape.lanes.entries()) {
    const value = allocation.at(ordinal)
    const offset = LayoutVerify.laneOffset(plan, SilkType.allocation, lane.path)
    if (value === undefined || offset === undefined)
      throw new RangeError('Wasm local-shared initialization lost reclaim provenance')
    instructions.push(...storeAt(base, value, operation.block.allocationOffset + offset, lane))
  }
  const payload = slots(operation.value)
  for (const [ordinal, lane] of valueShape.lanes.entries()) {
    const value = payload.at(ordinal)
    const offset = LayoutVerify.laneOffset(plan, operation.element, lane.path)
    if (value === undefined || offset === undefined)
      throw new RangeError('Wasm local-shared initialization lost its payload')
    instructions.push(...storeAt(base, value, operation.block.valueOffset + offset, lane))
  }
  return instructions
}

/** Reconstructs one target-private component address without publishing it in the common plan. */
const executionComponentOffset = (
  plan: ExecutionPackage.Plan,
  role: ExecutionPackage.Component['role'],
): number | undefined => {
  let cursor = 0
  for (const component of plan.components) {
    cursor = alignUp(cursor, component.alignment)
    if (component.role === role) return cursor
    cursor += component.size
  }
  return undefined
}

const emitExecutionFromAllocationOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ExecutionFromAllocation' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, plan, slots, scalar, storeAt, requireMemory } = state
  const allocation = slots(operation.allocation)
  const base = allocation.at(0)
  const bytes = allocation.at(1)
  const alignment = allocation.at(2)
  const destination = scalar(operation.destination)
  const allocationOffset = executionComponentOffset(operation.plan, 'AllocationAuthority')
  const bodyOffset = executionComponentOffset(operation.plan, 'BodyEnvironment')
  if (
    base === undefined ||
    bytes === undefined ||
    alignment === undefined ||
    allocationOffset === undefined ||
    bodyOffset === undefined
  )
    throw new RangeError('Wasm execution initialization lost its package placement')
  const instructions: Array<Instr.Instr> = [
    Instr.localGet(bytes),
    Instr.i32Const(operation.plan.size),
    Instr.op('i32.ne'),
    Instr.localGet(alignment),
    Instr.i32Const(operation.plan.alignment),
    Instr.op('i32.ne'),
    Instr.op('i32.or'),
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    Instr.localGet(base),
    Instr.localSet(destination),
    Instr.localGet(base),
    Instr.i32Const(0),
    Instr.memoryAccess('i32.store', requireMemory().memory),
  ]
  const packageOrdinal = plan.executionPackages.plans.findIndex((candidate) =>
    ExecutionPackage.equals(candidate, operation.plan),
  )
  if (packageOrdinal < 0)
    throw new RangeError('Wasm execution initialization lost its canonical package ordinal')
  instructions.push(
    Instr.localGet(base),
    Instr.i32Const(packageOrdinal),
    Instr.memoryAccess('i32.store', requireMemory().memory, {
      offset: plan.target.pointerSize,
    }),
  )
  for (const role of ['WakeControl', 'InitialContinuationSegment'] as const) {
    const offset = executionComponentOffset(operation.plan, role)
    if (offset === undefined) continue
    const words = role === 'WakeControl' ? 4 : 1
    for (let ordinal = 0; ordinal < words; ordinal += 1)
      instructions.push(
        Instr.localGet(base),
        Instr.i32Const(0),
        Instr.memoryAccess('i32.store', requireMemory().memory, {
          offset: offset + ordinal * plan.target.pointerSize,
        }),
      )
  }
  const storeValue = (
    local: Mir.LocalId,
    type: DeclarationFacts.SemanticType,
    byteOffset: number,
  ): void => {
    const values = slots(local)
    const lanes = layout.lanes.at(local.ordinal) ?? []
    if (values.length !== lanes.length)
      throw new RangeError('Wasm execution initialization lost an exact value lane')
    lanes.forEach((lane, ordinal) => {
      const value = values.at(ordinal)
      const offset = LayoutVerify.laneOffset(plan, type, lane.path)
      if (value === undefined || offset === undefined)
        throw new RangeError('Wasm execution initialization lost a package lane offset')
      instructions.push(...storeAt(base, value, byteOffset + offset, lane))
    })
  }
  const storeExecutableValue = (
    local: Mir.LocalId,
    fallback: DeclarationFacts.SemanticType,
    byteOffset: number,
  ): void => {
    const localType = layout.types.at(local.ordinal)
    if (localType?._tag !== 'EffectValue' && localType?._tag !== 'CallableValue') {
      storeValue(local, fallback, byteOffset)
      return
    }
    if (localType.environment === undefined) {
      storeValue(local, fallback, byteOffset)
      return
    }
    const values = slots(local)
    let ordinal = 0
    if (localType._tag === 'EffectValue') {
      for (const field of localType.environment.fields) {
        for (const lane of LayoutPlan.effectFieldLanes(plan, field)) {
          const value = values.at(ordinal)
          const offset =
            field.representation === 'Borrow'
              ? 0
              : LayoutVerify.laneOffset(plan, field.type, lane.path)
          if (value === undefined || offset === undefined)
            throw new RangeError('Wasm execution initialization lost an executable package lane')
          instructions.push(...storeAt(base, value, byteOffset + field.offset + offset, lane))
          ordinal += 1
        }
      }
    } else {
      for (const field of localType.environment.fields) {
        for (const lane of LayoutPlan.callableFieldLanes(plan, field)) {
          const value = values.at(ordinal)
          const offset =
            field.representation === 'Borrow'
              ? 0
              : LayoutVerify.laneOffset(plan, field.type, lane.path)
          if (value === undefined || offset === undefined)
            throw new RangeError('Wasm execution initialization lost an executable package lane')
          instructions.push(...storeAt(base, value, byteOffset + field.offset + offset, lane))
          ordinal += 1
        }
      }
    }
    if (ordinal !== values.length)
      throw new RangeError('Wasm execution initialization retained a stale executable lane')
  }
  const allocationShape = LayoutPlan.callingShape(plan, SilkType.allocation)
  if (allocationShape === undefined || allocationShape.lanes.length !== allocation.length)
    throw new RangeError('Wasm execution initialization lost its reclaim authority lanes')
  allocationShape.lanes.forEach((lane, ordinal) => {
    const value = allocation.at(ordinal)
    const offset = LayoutVerify.laneOffset(plan, SilkType.allocation, lane.path)
    if (value === undefined || offset === undefined)
      throw new RangeError('Wasm execution initialization lost reclaim provenance')
    instructions.push(...storeAt(base, value, allocationOffset + offset, lane))
  })
  storeExecutableValue(operation.body, operation.plan.specialization.body, bodyOffset)
  const endpointOffset = executionComponentOffset(operation.plan, 'EndpointState')
  if (endpointOffset !== undefined)
    storeValue(operation.endpoint, operation.plan.specialization.endpoint, endpointOffset)
  const callbackOffset = executionComponentOffset(operation.plan, 'EndpointCallback')
  if (callbackOffset !== undefined)
    storeExecutableValue(operation.callback, operation.plan.specialization.callback, callbackOffset)
  return instructions
}

const emitExecutionDriveOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ExecutionDrive' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const {
    emitter,
    layout,
    plan,
    resolveIndependent,
    slots,
    scalar,
    loadAt,
    releaseAtAddress,
    releaseInstructions,
  } = state
  const runtime = emitter.suspensionRuntime
  const base = scalar(operation.execution)
  const result = slots(operation.result)
  const completeType = layout.types.at(operation.onComplete.ordinal)
  const suspendType = layout.types.at(operation.onSuspend.ordinal)
  if (completeType?._tag !== 'CallableValue')
    throw new RangeError('Wasm execution drive lost its completion callable identity')
  const completion = emitApplyCallableOperation(
    Object.freeze({
      _tag: 'ApplyCallable' as const,
      destination: operation.destination,
      callable: operation.onComplete,
      typeArguments: operation.completionTypeArguments,
      captures: Object.freeze([]),
      arguments: Object.freeze([operation.branch, operation.result]),
      callableType: completeType.type,
      access: completeType.type.mode,
      evaluation: 'CalleeThenArguments' as const,
      realization: 'Environment' as const,
      type: operation.type,
      provenance: operation.provenance,
    }),
    state,
  )
  const suspension = emitApplyCallableOperation(
    Object.freeze({
      _tag: 'ApplyCallable' as const,
      destination: operation.destination,
      callable: operation.onSuspend,
      typeArguments: operation.suspensionTypeArguments,
      captures: Object.freeze([]),
      arguments: Object.freeze([operation.branch, operation.execution]),
      callableType:
        suspendType?._tag === 'CallableValue'
          ? suspendType.type
          : (() => {
              throw new RangeError('Wasm execution drive lost its suspension callable identity')
            })(),
      access: 'Take' as const,
      evaluation: 'CalleeThenArguments' as const,
      realization: 'Environment' as const,
      type: operation.type,
      provenance: operation.provenance,
    }),
    state,
  )
  const branchFor = (packageOrdinal: number): ReadonlyArray<Instr.Instr> => {
    const package_ = plan.executionPackages.plans.at(packageOrdinal)
    if (package_ === undefined) return [Instr.op('unreachable')]
    const represented = package_.specialization.body
    const representation = SilkType.isRepresented(represented)
      ? represented.representation.argument
      : undefined
    const identity =
      representation !== undefined && SilkType.isExactRepresentationArgument(representation)
        ? representation.identity
        : undefined
    const environment =
      identity !== undefined && SilkType.isEffectIdentityArgument(identity)
        ? (LayoutPlan.effectEnvironmentByFieldIdentity(plan, identity.identity) ??
          plan.effectEnvironments.find(
            (
              candidate,
            ): candidate is Extract<
              LayoutPlan.EffectEnvironment,
              { readonly _tag: 'EffectEnvironment' }
            > =>
              candidate._tag === 'EffectEnvironment' &&
              Hir.effectRepresentationIdentity(candidate.site) === identity.identity &&
              identity.owner !== undefined &&
              candidate.instance.declaration.module === identity.owner.declaration.module &&
              candidate.instance.declaration.name === identity.owner.declaration.name &&
              candidate.instance.typeArguments.length === identity.owner.typeArguments.length &&
              candidate.instance.typeArguments.every((argument, ordinal) => {
                const expected = identity.owner?.typeArguments.at(ordinal)
                return (
                  expected !== undefined &&
                  SilkType.genericArgumentKey(argument) === SilkType.genericArgumentKey(expected)
                )
              }),
          ))
        : undefined
    const bodyOffset = executionComponentOffset(package_, 'BodyEnvironment')
    const allocationOffset = executionComponentOffset(package_, 'AllocationAuthority')
    const packageCleanup = state.emitter.executionPackageCleanups.get(package_.provenance)
    if (
      environment === undefined ||
      bodyOffset === undefined ||
      allocationOffset === undefined ||
      packageCleanup === undefined
    )
      throw new RangeError(
        `Wasm execution drive lost package authority: environment=${environment !== undefined}, body=${bodyOffset !== undefined}, allocation=${allocationOffset !== undefined}, cleanup=${packageCleanup !== undefined}`,
      )
    const callbackOffset = executionComponentOffset(package_, 'EndpointCallback')
    const endpointOffset = executionComponentOffset(package_, 'EndpointState')
    const controlOffset = executionComponentOffset(package_, 'WakeControl')
    const continuationOffset = executionComponentOffset(package_, 'InitialContinuationSegment')
    const bodyOperands = environment.fields.flatMap((field) =>
      LayoutPlan.effectFieldLanes(plan, field).flatMap((lane) => {
        const offset =
          field.representation === 'Borrow'
            ? 0
            : LayoutVerify.laneOffset(plan, field.type, lane.path)
        return offset === undefined
          ? [Instr.op('unreachable')]
          : loadAt(base, bodyOffset + field.offset + offset, lane)
      }),
    )
    const outcomeShape = LayoutPlan.callingShape(plan, environment.effect)
    if (outcomeShape === undefined || outcomeShape.lanes.length !== result.length + 1)
      return [Instr.op('unreachable')]
    const allocationCleanup: Extract<
      CleanupPlan.CleanupPlan,
      { readonly _tag: 'AllocationCleanup' }
    > = Object.freeze({
      _tag: 'AllocationCleanup',
      type: SilkType.allocation,
      ticket: 'ActiveReclaimTicket',
    })
    if (continuationOffset === undefined)
      return [
        Instr.localGet(base),
        Instr.memoryAccess('i32.load', state.requireMemory().memory),
        Instr.i32Const(0),
        Instr.op('i32.ne'),
        Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        Instr.localGet(base),
        Instr.i32Const(1),
        Instr.memoryAccess('i32.store', state.requireMemory().memory),
        ...bodyOperands,
        Instr.call(
          resolveIndependent(
            Hir.effectRunnerId(environment.instance.declaration, environment.site),
            environment.instance.typeArguments,
          ),
        ),
        ...[layout.scratch, ...result].reverse().map((slot) => Instr.localSet(slot)),
        Instr.localGet(layout.scratch),
        Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
        ...releaseInstructions(operation.suspensionCleanup, operation.onSuspend),
        Instr.localGet(base),
        Instr.i32Const(5),
        Instr.memoryAccess('i32.store', state.requireMemory().memory),
        ...(callbackOffset === undefined
          ? []
          : releaseAtAddress(packageCleanup.callback, base, callbackOffset)),
        ...(endpointOffset === undefined
          ? []
          : releaseAtAddress(packageCleanup.endpoint, base, endpointOffset)),
        ...releaseAtAddress(allocationCleanup, base, allocationOffset),
        ...completion,
      ]
    if (runtime === undefined)
      throw new RangeError('Wasm execution drive lost its suspension runtime')
    const notification =
      controlOffset === undefined
        ? [Instr.op('unreachable')]
        : [
            Instr.localGet(base),
            Instr.i32Const(3),
            Instr.memoryAccess('i32.store', runtime.memory),
            Instr.localGet(base),
            Instr.i32Const(4),
            Instr.memoryAccess('i32.store', runtime.memory, { offset: controlOffset }),
            ...emitExecutionReadyNotification(package_, base, state),
            Instr.localGet(base),
            Instr.memoryAccess('i32.load', runtime.memory),
            Instr.i32Const(7),
            Instr.op('i32.eq'),
            Instr.ifElse(Instr.emptyBlockType, state.releaseExecutionBase(base), [
              Instr.localGet(base),
              Instr.i32Const(5),
              Instr.memoryAccess('i32.store', runtime.memory, { offset: controlOffset }),
              Instr.localGet(base),
              Instr.i32Const(4),
              Instr.memoryAccess('i32.store', runtime.memory),
            ]),
          ]
    const suspended = [
      ...releaseInstructions(operation.completionCleanup, operation.onComplete),
      Instr.i32Const(runtime.transferAddress + plan.target.pointerSize),
      Instr.memoryAccess('i32.load', runtime.memory),
      Instr.localSet(layout.scratch),
      Instr.localGet(base),
      Instr.localGet(layout.scratch),
      Instr.memoryAccess('i32.store', runtime.memory, { offset: continuationOffset }),
      Instr.i32Const(runtime.transferAddress + plan.target.pointerSize),
      Instr.i32Const(0),
      Instr.memoryAccess('i32.store', runtime.memory),
      ...suspension,
      ...(controlOffset === undefined
        ? [Instr.localGet(base), Instr.i32Const(2), Instr.memoryAccess('i32.store', runtime.memory)]
        : [
            Instr.localGet(base),
            Instr.memoryAccess('i32.load', runtime.memory),
            Instr.i32Const(7),
            Instr.op('i32.eq'),
            Instr.ifElse(Instr.emptyBlockType, state.releaseExecutionBase(base), [
              Instr.localGet(base),
              Instr.memoryAccess('i32.load', runtime.memory),
              Instr.i32Const(6),
              Instr.op('i32.eq'),
              Instr.ifElse(
                Instr.emptyBlockType,
                [],
                [
                  Instr.localGet(base),
                  Instr.memoryAccess('i32.load', runtime.memory, { offset: controlOffset }),
                  Instr.i32Const(2),
                  Instr.op('i32.eq'),
                  Instr.ifElse(Instr.emptyBlockType, notification, [
                    Instr.localGet(base),
                    Instr.i32Const(2),
                    Instr.memoryAccess('i32.store', runtime.memory),
                  ]),
                ],
              ),
            ]),
          ]),
      Instr.i32Const(0),
      Instr.globalSet(runtime.status),
      Instr.i32Const(0),
      Instr.globalSet(runtime.activeExecution),
    ]
    const completed = [
      Instr.localGet(layout.scratch),
      Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
      ...releaseInstructions(operation.suspensionCleanup, operation.onSuspend),
      Instr.localGet(base),
      Instr.i32Const(2),
      Instr.memoryAccess('i32.store', state.requireMemory().memory),
      ...(callbackOffset === undefined
        ? []
        : releaseAtAddress(packageCleanup.callback, base, callbackOffset)),
      ...(endpointOffset === undefined
        ? []
        : releaseAtAddress(packageCleanup.endpoint, base, endpointOffset)),
      ...releaseAtAddress(allocationCleanup, base, allocationOffset),
      ...completion,
      Instr.i32Const(0),
      Instr.globalSet(runtime.activeExecution),
    ]
    return [
      Instr.localGet(base),
      Instr.memoryAccess('i32.load', state.requireMemory().memory),
      Instr.localTee(layout.scratch),
      Instr.i32Const(0),
      Instr.op('i32.eq'),
      Instr.localGet(layout.scratch),
      Instr.i32Const(4),
      Instr.op('i32.eq'),
      Instr.op('i32.or'),
      Instr.op('i32.eqz'),
      Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
      Instr.localGet(layout.scratch),
      Instr.i32Const(4),
      Instr.op('i32.eq'),
      Instr.ifElse(
        Instr.emptyBlockType,
        [
          Instr.localGet(base),
          Instr.memoryAccess('i32.load', runtime.memory, { offset: continuationOffset }),
          Instr.globalSet(runtime.externalResumeHead),
          ...(controlOffset === undefined
            ? []
            : [
                Instr.localGet(base),
                Instr.i32Const(0),
                Instr.memoryAccess('i32.store', runtime.memory, { offset: controlOffset }),
              ]),
        ],
        [Instr.i32Const(0), Instr.globalSet(runtime.externalResumeHead)],
      ),
      Instr.localGet(base),
      Instr.globalSet(runtime.activeExecution),
      Instr.localGet(base),
      Instr.i32Const(1),
      Instr.memoryAccess('i32.store', state.requireMemory().memory),
      ...bodyOperands,
      Instr.call(
        resolveIndependent(
          Hir.effectRunnerId(environment.instance.declaration, environment.site),
          environment.instance.typeArguments,
        ),
      ),
      ...[layout.scratch, ...result].reverse().map((slot) => Instr.localSet(slot)),
      Instr.globalGet(runtime.status),
      Instr.i32Const(2),
      Instr.op('i32.eq'),
      Instr.ifElse(Instr.emptyBlockType, suspended, completed),
    ]
  }
  const select = (ordinal: number): ReadonlyArray<Instr.Instr> =>
    ordinal >= plan.executionPackages.plans.length
      ? [Instr.op('unreachable')]
      : [
          Instr.localGet(base),
          Instr.memoryAccess('i32.load', state.requireMemory().memory, {
            offset: plan.target.pointerSize,
          }),
          Instr.i32Const(ordinal),
          Instr.op('i32.eq'),
          Instr.ifElse(Instr.emptyBlockType, branchFor(ordinal), select(ordinal + 1)),
        ]
  const executionType = layout.types.at(operation.execution.ordinal)
  const executionResult =
    executionType?._tag === 'Nominal' && SilkType.isExecution(executionType.type)
      ? SilkType.typeArgumentAt(executionType.type, 0)
      : undefined
  const matching = plan.executionPackages.plans.flatMap((package_, ordinal) =>
    executionResult !== undefined &&
    SilkType.equals(package_.specialization.result, executionResult)
      ? [ordinal]
      : [],
  )
  return matching.length === 1 ? branchFor(matching.at(0) ?? -1) : select(0)
}

/** Invokes the package's fixed readiness endpoint from its exact represented callable identity. */
const emitExecutionReadyNotification = (
  package_: ExecutionPackage.Plan,
  base: number,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, resolve, loadAt, requireMemory } = state
  const callbackOffset = executionComponentOffset(package_, 'EndpointCallback')
  const endpointOffset = executionComponentOffset(package_, 'EndpointState')
  const callback = package_.specialization.callback
  const representation = SilkType.isRepresented(callback)
    ? callback.representation.argument
    : undefined
  const identity =
    representation !== undefined &&
    SilkType.isExactRepresentationArgument(representation) &&
    SilkType.isCallableIdentityArgument(representation.identity)
      ? representation.identity
      : undefined
  const target =
    identity === undefined ? undefined : Hir.callableTargetFromIdentity(identity.target)
  const environment =
    identity?.environment === undefined
      ? undefined
      : LayoutPlan.callableEnvironmentByIdentity(plan, identity.environment)
  if (
    callbackOffset === undefined ||
    endpointOffset === undefined ||
    identity === undefined ||
    target?._tag !== 'DeclarationCallableTarget' ||
    (identity.environment !== undefined && environment === undefined)
  )
    throw new RangeError('Wasm readiness notification lost its exact package callback authority')
  const captureOperands = (environment?.fields ?? [])
    .flatMap((field) => {
      const lanes = LayoutPlan.callableFieldLanes(plan, field)
      if (field.representation !== 'Borrow')
        return [
          Object.freeze({
            parameterOrdinal: field.parameterOrdinal,
            instructions: lanes.flatMap((lane) => {
              const offset = LayoutVerify.laneOffset(plan, field.type, lane.path)
              if (offset === undefined)
                throw new RangeError('Wasm readiness callback lost a capture lane')
              return loadAt(base, callbackOffset + field.offset + offset, lane)
            }),
          }),
        ]
      const shape = LayoutPlan.callingShape(plan, field.type)
      if (shape === undefined)
        throw new RangeError('Wasm readiness callback lost a borrowed capture shape')
      return [
        Object.freeze({
          parameterOrdinal: field.parameterOrdinal,
          instructions: shape.lanes.flatMap((lane) => {
            const offset = LayoutVerify.laneOffset(plan, field.type, lane.path)
            if (offset === undefined)
              throw new RangeError('Wasm readiness callback lost a borrowed capture lane')
            return [
              ...loadAt(base, callbackOffset + field.offset),
              Instr.memoryAccess(laneLoadMnemonic(plan, lane), requireMemory().memory, { offset }),
            ]
          }),
        }),
      ]
    })
    .sort((left, right) => left.parameterOrdinal - right.parameterOrdinal)
    .flatMap((capture) => capture.instructions)
  return [
    Instr.localGet(base),
    ...(endpointOffset === 0 ? [] : [Instr.i32Const(endpointOffset), Instr.op('i32.add')]),
    ...captureOperands,
    Instr.call(
      resolve(
        target.declaration,
        environment === undefined
          ? identity.typeArguments
          : LayoutPlan.callableTargetArguments(environment),
      ),
    ),
  ]
}

const emitExecutionWakeOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ExecutionWake' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { scalar } = state
  const memory = state.requireMemory().memory
  const base = scalar(operation.wake)
  const branches = (ordinal: number): ReadonlyArray<Instr.Instr> => {
    const package_ = state.plan.executionPackages.plans.at(ordinal)
    if (package_ === undefined) return [Instr.op('unreachable')]
    const control = executionComponentOffset(package_, 'WakeControl')
    const selected =
      control === undefined
        ? [Instr.op('unreachable')]
        : (() => {
            const phase = (expected: number): ReadonlyArray<Instr.Instr> => [
              Instr.localGet(base),
              Instr.memoryAccess('i32.load', memory, { offset: control }),
              Instr.i32Const(expected),
              Instr.op('i32.eq'),
            ]
            const notify = [
              Instr.localGet(base),
              Instr.i32Const(3),
              Instr.memoryAccess('i32.store', memory),
              Instr.localGet(base),
              Instr.i32Const(4),
              Instr.memoryAccess('i32.store', memory, { offset: control }),
              ...emitExecutionReadyNotification(package_, base, state),
              Instr.localGet(base),
              Instr.memoryAccess('i32.load', memory),
              Instr.i32Const(7),
              Instr.op('i32.eq'),
              Instr.ifElse(Instr.emptyBlockType, state.releaseExecutionBase(base), [
                Instr.localGet(base),
                Instr.i32Const(5),
                Instr.memoryAccess('i32.store', memory, { offset: control }),
                Instr.localGet(base),
                Instr.i32Const(4),
                Instr.memoryAccess('i32.store', memory),
              ]),
            ]
            return [
              ...phase(1),
              Instr.ifElse(
                Instr.emptyBlockType,
                [
                  Instr.localGet(base),
                  Instr.i32Const(2),
                  Instr.memoryAccess('i32.store', memory, { offset: control }),
                ],
                [
                  ...phase(3),
                  Instr.ifElse(Instr.emptyBlockType, notify, [
                    ...phase(6),
                    Instr.ifElse(Instr.emptyBlockType, [], [Instr.op('unreachable')]),
                  ]),
                ],
              ),
            ]
          })()
    return [
      Instr.localGet(base),
      Instr.memoryAccess('i32.load', memory, { offset: state.plan.target.pointerSize }),
      Instr.i32Const(ordinal),
      Instr.op('i32.eq'),
      Instr.ifElse(Instr.emptyBlockType, selected, branches(ordinal + 1)),
    ]
  }
  return branches(0)
}

const emitExecutionParkOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ExecutionPark' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { emitter, layout, suspension, skipInvocation } = state
  // The execution package, not the resumed coroutine frame, owns the registration guard after
  // parking. Eligibility consumes it before this resume label is entered, so the skipped
  // invocation must not materialize or release the transferred local a second time.
  if (skipInvocation) return []
  const runtime = emitter.suspensionRuntime
  const region = suspension?.regions.get(operation)
  const registerType = layout.types.at(operation.register.ordinal)
  const guardType = layout.types.at(operation.guard.ordinal)
  if (
    runtime === undefined ||
    region?._tag !== 'RunSuspendableEffectRegion' ||
    registerType?._tag !== 'CallableValue' ||
    guardType === undefined
  )
    return [Instr.op('unreachable')]
  const registration = emitApplyCallableOperation(
    Object.freeze({
      _tag: 'ApplyCallable' as const,
      destination: operation.guard,
      callable: operation.register,
      typeArguments: operation.registrationTypeArguments,
      captures: Object.freeze([]),
      arguments: Object.freeze([]),
      callableType: registerType.type,
      access: 'Take' as const,
      evaluation: 'CalleeThenArguments' as const,
      realization: 'Environment' as const,
      type: guardType,
      provenance: operation.provenance,
    }),
    state,
    Object.freeze([Object.freeze([Instr.globalGet(runtime.activeExecution)])]),
  )
  const select = (ordinal: number): ReadonlyArray<Instr.Instr> => {
    const package_ = state.plan.executionPackages.plans.at(ordinal)
    if (package_ === undefined) return [Instr.op('unreachable')]
    const control = executionComponentOffset(package_, 'WakeControl')
    const selected =
      control === undefined
        ? [Instr.op('unreachable')]
        : [
            Instr.globalGet(runtime.activeExecution),
            Instr.memoryAccess('i32.load', runtime.memory, { offset: control }),
            Instr.i32Const(0),
            Instr.op('i32.ne'),
            Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
            Instr.globalGet(runtime.activeExecution),
            Instr.i32Const(1),
            Instr.memoryAccess('i32.store', runtime.memory, { offset: control }),
            Instr.globalGet(runtime.activeExecution),
            Instr.globalGet(runtime.activeExecution),
            Instr.memoryAccess('i32.load', runtime.memory, {
              offset: control + state.plan.target.pointerSize,
            }),
            Instr.i32Const(1),
            Instr.op('i32.add'),
            Instr.memoryAccess('i32.store', runtime.memory, {
              offset: control + state.plan.target.pointerSize,
            }),
            ...registration,
            Instr.globalGet(runtime.activeExecution),
            Instr.memoryAccess('i32.load', runtime.memory, { offset: control }),
            Instr.i32Const(2),
            Instr.op('i32.ne'),
            Instr.ifElse(
              Instr.emptyBlockType,
              [
                Instr.globalGet(runtime.activeExecution),
                Instr.i32Const(3),
                Instr.memoryAccess('i32.store', runtime.memory, { offset: control }),
              ],
              [],
            ),
            Instr.i32Const(2),
            Instr.globalSet(runtime.status),
            ...(suspension?.relay(region) ?? []),
          ]
    return [
      Instr.globalGet(runtime.activeExecution),
      Instr.memoryAccess('i32.load', runtime.memory, {
        offset: state.plan.target.pointerSize,
      }),
      Instr.i32Const(ordinal),
      Instr.op('i32.eq'),
      Instr.ifElse(Instr.emptyBlockType, selected, select(ordinal + 1)),
    ]
  }
  return select(0)
}

const emitSharedCloneOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'SharedClone' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { scalar, loadAt, requireMemory } = state
  const self = scalar(operation.self)
  const destination = scalar(operation.destination)
  const memory = requireMemory().memory
  return [
    ...loadAt(self, 0),
    Instr.localSet(destination),
    Instr.localGet(destination),
    Instr.memoryAccess('i32.load', memory, { offset: operation.block.strongOffset }),
    Instr.i32Const(-1),
    Instr.op('i32.eq'),
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    Instr.localGet(destination),
    Instr.localGet(destination),
    Instr.memoryAccess('i32.load', memory, { offset: operation.block.strongOffset }),
    Instr.i32Const(1),
    Instr.op('i32.add'),
    Instr.memoryAccess('i32.store', memory, { offset: operation.block.strongOffset }),
  ]
}

const emitSharedWithMutOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'SharedWithMut' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, scalar, loadAt, requireMemory } = state
  const memory = requireMemory().memory
  const self = scalar(operation.self)
  const payload = scalar(operation.payload)
  const callableArguments = (local: Mir.LocalId): ReadonlyArray<SilkType.GenericArgument> => {
    const type = layout.types.at(local.ordinal)
    return type?._tag === 'CallableValue'
      ? ((type.environment === undefined
          ? undefined
          : LayoutPlan.callableTargetArguments(type.environment)) ??
          type.storage?.realization.targetArguments ??
          Object.freeze([]))
      : Object.freeze([])
  }
  const apply = (
    callable: Mir.LocalId,
    callableType: SilkType.Callable,
    arguments_: ReadonlyArray<Mir.LocalId>,
  ): ReadonlyArray<Instr.Instr> =>
    emitApplyCallableOperation(
      Object.freeze({
        _tag: 'ApplyCallable',
        destination: operation.destination,
        callable,
        typeArguments: callableArguments(callable),
        captures: Object.freeze([]),
        arguments: Object.freeze(arguments_),
        callableType,
        access: 'Take',
        evaluation: 'CalleeThenArguments',
        realization: 'Environment',
        type: operation.type,
        provenance: operation.provenance,
      }),
      state,
    )
  const drop = (
    local: Mir.LocalId,
    cleanup: CleanupPlan.CleanupPlan,
  ): ReadonlyArray<Instr.Instr> =>
    cleanup._tag === 'NoCleanup'
      ? Object.freeze([])
      : emitDropOperation(
          Object.freeze({ _tag: 'Drop', local, cleanup, provenance: operation.provenance }),
          state,
        )
  const base = (): ReadonlyArray<Instr.Instr> => loadAt(self, 0)
  const setAccess = (active: boolean): ReadonlyArray<Instr.Instr> => [
    ...base(),
    Instr.i32Const(active ? 1 : 0),
    Instr.memoryAccess('i32.store', memory, { offset: operation.block.accessOffset }),
  ]
  const use = [
    ...setAccess(true),
    ...base(),
    ...(operation.block.valueOffset === 0
      ? []
      : [Instr.i32Const(operation.block.valueOffset), Instr.op('i32.add')]),
    Instr.localSet(payload),
    ...apply(operation.use, operation.useType, [operation.payload]),
    ...setAccess(false),
    ...drop(operation.onConflict, operation.conflictCleanup),
  ]
  const conflict = [
    ...apply(operation.onConflict, operation.conflictType, []),
    ...drop(operation.use, operation.useCleanup),
  ]
  return [
    ...base(),
    Instr.memoryAccess('i32.load', memory, { offset: operation.block.accessOffset }),
    Instr.op('i32.eqz'),
    Instr.ifElse(Instr.emptyBlockType, use, conflict),
  ]
}

const emitRawBufferCountOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RawBufferCount' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, scalar, aggregateFieldOffset, loadAt } = state

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

const emitRawBufferSlotOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RawBufferSlot' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, scalar, aggregateFieldOffset, loadAt } = state

  const address = scalar(operation.buffer)
  const index = scalar(operation.index)
  const rawBuffer = SilkType.rawBuffer(operation.element)
  const allocationOffset = aggregateFieldOffset(rawBuffer, '$allocation')
  const baseOffset = allocationOffset + aggregateFieldOffset(SilkType.allocation, '$base')
  const countOffset = aggregateFieldOffset(rawBuffer, 'count')
  const elementLayout = LayoutPlan.entry(plan, operation.element)
  if (elementLayout === undefined) throw new RangeError('Wasm RawBuffer.slot lost element layout')
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

const emitRawBufferReadOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RawBufferRead' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, slots, scalar, requireMemory, aggregateFieldOffset, loadAt } = state

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
      const offset = LayoutVerify.laneOffset(plan, operation.element, lane.path)
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

const emitRawBufferViewOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RawBufferView' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, scalar, aggregateFieldOffset, loadAt } = state

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

const emitRawBufferCopyOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RawBufferCopy' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, scalar, requireMemory, aggregateFieldOffset, loadAt } = state

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

const emitRawBufferFillOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RawBufferFill' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { scalar, requireMemory, aggregateFieldOffset, loadAt } = state

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

const emitSlotWriteOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'SlotWrite' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, slots, scalar, storeAt } = state

  const address = scalar(operation.slot)
  const shape = LayoutPlan.callingShape(plan, operation.element)
  if (shape === undefined) throw new RangeError('Wasm Slot.write lost its element shape')
  return shape.lanes.flatMap((lane, ordinal) => {
    const value = slots(operation.value).at(ordinal)
    const offset = LayoutVerify.laneOffset(plan, operation.element, lane.path)
    if (value === undefined || offset === undefined) {
      throw new RangeError('Wasm Slot.write lost an element lane')
    }
    return storeAt(address, value, offset, lane)
  })
}

const emitSlotTakeOrSlotCopyOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'SlotTake' | 'SlotCopy' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, slots, scalar, loadAt } = state

  const address = scalar(operation.slot)
  const shape = LayoutPlan.callingShape(plan, operation.element)
  if (shape === undefined) throw new RangeError('Wasm Slot.take lost its element shape')
  return shape.lanes.flatMap((lane, ordinal) => {
    const destination = slots(operation.destination).at(ordinal)
    const offset = LayoutVerify.laneOffset(plan, operation.element, lane.path)
    if (destination === undefined || offset === undefined) {
      throw new RangeError('Wasm Slot.take lost an element lane')
    }
    return [...loadAt(address, offset, lane), Instr.localSet(destination)]
  })
}

const emitSlotDropOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'SlotDrop' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { scalar, releaseAtAddress } = state
  return releaseAtAddress(operation.cleanup, scalar(operation.slot))
}

const emitShortCircuitOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ShortCircuit' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { emitter, suspension, slots, scalar, copy } = state

  const right = [
    ...operation.right.operations.flatMap((nested) => emitOperation(nested, emitter, suspension)),
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

const emitMatchOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Match' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { emitter, layout, suspension, slots, scalar, copy } = state

  const emitMany = (operations: ReadonlyArray<Mir.Operation>): ReadonlyArray<Instr.Instr> =>
    operations.flatMap((nested) => emitOperation(nested, emitter, suspension))
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
      const physical = LayoutPlan.memberFieldSlots(operation.scrutineeShape, member, binding.path)
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
      Instr.ifElse(Instr.emptyBlockType, selected, emitCandidates(member, candidates, ordinal + 1)),
    ]
  }
  const emitDecisions = (ordinal = 0): ReadonlyArray<Instr.Instr> => {
    const decision = operation.decisions.at(ordinal)
    if (decision === undefined) return [Instr.op('unreachable')]
    const selected = emitCandidates(decision.member, decision.candidates)
    if (operation.scrutineeType._tag !== 'Union') return selected
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

const emitLiteralOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Literal' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, plan, scalar } = state

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

const emitStaticViewOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'StaticView' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, requireMemory } = state

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

const emitMoveOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Move' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, copy } = state
  return copy(slots(operation.source), slots(operation.destination))
}

const emitBeginLoanOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'BeginLoan' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, plan, memory, slots, scalar, copy, materializeRoot, frameAddress } = state
  if (operation.sourceType._tag === 'Slice') {
    return copy(slots(operation.root), slots(operation.destination))
  } else {
    const rootType = layout.types.at(operation.root.ordinal)
    const rootSemantic = rootType === undefined ? undefined : Mir.semanticType(rootType)
    const borrowedRoot = rootType?._tag === 'EffectBorrow'
    const borrowedPointer = borrowedRoot
      ? layout.borrowPointers.get(operation.root.ordinal)
      : undefined
    if (rootSemantic !== undefined && SilkType.isSlice(rootSemantic)) {
      const [selector, ...suffixSelectors] = operation.selectors
      const [base, length] = slots(operation.root)
      const [address] = slots(operation.destination)
      if (
        selector?._tag !== 'SliceElementSelector' ||
        base === undefined ||
        length === undefined ||
        address === undefined ||
        operation.type._tag !== 'Reference'
      ) {
        throw new RangeError('Wasm slice borrow lost its canonical lanes')
      }
      const sliceLayout = LayoutPlan.entry(plan, rootSemantic)
      if (sliceLayout?.representation._tag !== 'Slice') {
        throw new RangeError('Wasm slice borrow lost its compiler layout')
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
          throw new RangeError('Wasm nested runtime slice borrow is not canonical')
        }
      }
      const staticOffset = LayoutVerify.laneOffset(plan, rootSemantic.element, staticSelectors)
      if (staticOffset === undefined) {
        throw new RangeError('Wasm slice borrow lost its selected layout')
      }
      return [
        Instr.localGet(scalar(selector.index)),
        Instr.localGet(length),
        Instr.op('i32.lt_u'),
        Instr.ifElse(Instr.emptyBlockType, [], [Instr.op('unreachable')]),
        Instr.localGet(base),
        Instr.localGet(scalar(selector.index)),
        Instr.i32Const(sliceLayout.representation.stride),
        Instr.op('i32.mul'),
        Instr.op('i32.add'),
        ...(staticOffset === 0 ? [] : [Instr.i32Const(staticOffset), Instr.op('i32.add')]),
        Instr.localSet(address),
      ]
    }
    let selected =
      rootSemantic !== undefined && SilkType.isReference(rootSemantic)
        ? rootSemantic.target
        : rootSemantic
    let staticOffset = 0
    const dynamicOffsets: Array<{
      readonly local: Mir.LocalId
      readonly stride: number
      readonly length: number
    }> = []
    for (const selector of operation.selectors) {
      const selectedLayout = selected === undefined ? undefined : LayoutPlan.entry(plan, selected)
      if (selector._tag === 'FieldSelector') {
        if (selectedLayout?.representation._tag !== 'Aggregate')
          throw new RangeError('Wasm borrow field selector lost its aggregate layout')
        const field = selectedLayout.representation.fields.find(
          (candidate) =>
            candidate.id.ordinal === selector.field.ordinal &&
            candidate.id.struct.sourceId === selector.field.struct.sourceId &&
            candidate.id.struct.ordinal === selector.field.struct.ordinal,
        )
        if (field === undefined)
          throw new RangeError('Wasm borrow field selector lost its field layout')
        staticOffset += field.offset
        selected = field.type
        continue
      }
      if (
        selector._tag !== 'ElementSelector' ||
        selectedLayout?.representation._tag !== 'Repeated'
      ) {
        throw new RangeError('Wasm borrow element selector lost its repeated layout')
      }
      if (selector.index._tag === 'Proven') {
        staticOffset += selector.index.value * selectedLayout.representation.stride
      } else {
        dynamicOffsets.push(
          Object.freeze({
            local: selector.index.local,
            stride: selectedLayout.representation.stride,
            length: selector.length,
          }),
        )
      }
      selected = selectedLayout.representation.element
    }
    const planned = memory?.frame.roots.get(operation.root.ordinal)
    const [address, length] = slots(operation.destination)
    if (rootSemantic === undefined || address === undefined) {
      throw new RangeError('Wasm borrow formation lost its frame root or address lane')
    }
    let rootAddress: ReadonlyArray<Instr.Instr>
    if (borrowedRoot) {
      if (borrowedPointer === undefined) {
        throw new RangeError('Wasm borrow formation lost its inherited borrow pointer')
      }
      rootAddress = [Instr.localGet(borrowedPointer)]
    } else if (SilkType.isReference(rootSemantic)) {
      rootAddress = [Instr.localGet(scalar(operation.root))]
    } else {
      if (planned === undefined) {
        throw new RangeError('Wasm borrow formation lost its address-taken frame root')
      }
      rootAddress = [...materializeRoot(operation.root), ...frameAddress(planned.offset)]
    }
    const instructions: Array<Instr.Instr> = []
    for (const offset of dynamicOffsets) {
      instructions.push(
        Instr.localGet(scalar(offset.local)),
        Instr.i32Const(offset.length),
        Instr.op('i32.lt_u'),
        Instr.ifElse(Instr.emptyBlockType, [], [Instr.op('unreachable')]),
      )
    }
    instructions.push(
      ...rootAddress,
      ...(staticOffset === 0 ? [] : [Instr.i32Const(staticOffset), Instr.op('i32.add')]),
    )
    for (const offset of dynamicOffsets) {
      instructions.push(
        Instr.localGet(scalar(offset.local)),
        Instr.i32Const(offset.stride),
        Instr.op('i32.mul'),
        Instr.op('i32.add'),
      )
    }
    instructions.push(Instr.localSet(address))
    if (operation.type._tag === 'Reference') return instructions
    if (length === undefined || operation.sourceType._tag !== 'FixedArray') {
      throw new RangeError('Wasm slice formation lost its length lane or array root')
    }
    return [
      ...instructions,
      Instr.i32Const(operation.sourceType.type.length),
      Instr.localSet(length),
    ]
  }
}

const emitEndLoanOperation = (
  _operation: Extract<Mir.Operation, { readonly _tag: 'EndLoan' }>,
  _state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => []

const emitSliceLengthOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'SliceLength' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, scalar } = state

  const length = slots(operation.slice).at(1)
  return length === undefined
    ? [Instr.op('unreachable')]
    : [Instr.localGet(length), Instr.localSet(scalar(operation.destination))]
}

const emitConvertUnionOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ConvertUnion' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, transfer, zeroFor } = state

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

const emitConstructOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Construct' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, copy } = state
  return copy(
    operation.fields.flatMap((field) => [...slots(field.value)]),
    slots(operation.destination),
  )
}

const emitConstructArrayOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ConstructArray' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, copy } = state
  return copy(
    operation.elements.flatMap((element) => [...slots(element)]),
    slots(operation.destination),
  )
}

const emitProjectOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Project' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, copy } = state

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

const emitReadPlaceOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ReadPlace' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, plan, memory, slots, scalar, loadAt } = state

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
      const offset = LayoutVerify.laneOffset(plan, target, [...staticSelectors, ...lane.path])
      if (destination === undefined || offset === undefined)
        throw new RangeError('Wasm reference read lost a lane offset')
      return [...loadAt(address, offset, lane), Instr.localSet(destination)]
    })
  }
  if (rootSemantic !== undefined && SilkType.isSlice(rootSemantic)) {
    if (memory === undefined) throw new RangeError('Wasm slice read has no private memory')
    const [selector, ...suffixSelectors] = operation.selectors
    const [base, length] = slots(operation.root)
    if (selector?._tag !== 'SliceElementSelector' || base === undefined || length === undefined) {
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
      const staticOffset = LayoutVerify.laneOffset(
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
            conditions.push(Object.freeze({ local: selector.index.local, element: physical.index }))
          }
        }
      }
      const suffix = sourceLane.path.slice(operation.selectors.length)
      const sameSuffix = suffix.every((physical, ordinal) => {
        const expected = destinationLane.path.at(ordinal)
        return expected !== undefined && LayoutVerify.selectorEquals(physical, expected)
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
      throw new RangeError(`Wasm backend could not realize place-read lane ${destinationOrdinal}`)
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
      selection = [Instr.localGet(candidate.source), ...selection, ...condition, Instr.op('select')]
    }
    instructions.push(...selection, Instr.localSet(destination))
  }
  return instructions
}

const emitCheckPlaceOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'CheckPlace' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, scalar } = state

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

const emitWritePlaceOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'WritePlace' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, plan, memory, slots, scalar, copy, storeAt, flushBorrowRoot } = state

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
      const offset = LayoutVerify.laneOffset(plan, target, [...staticSelectors, ...lane.path])
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
      const staticOffset = LayoutVerify.laneOffset(
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
          return expected !== undefined && LayoutVerify.selectorEquals(physical, expected)
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

const emitDropOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Drop' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const {
    releaseInstructions,
    releaseAtAddress,
    semanticLanesOf,
    loadAt,
    resolve,
    scalar,
    requireMemory,
    plan,
  } = state
  if (operation.cleanup._tag === 'LocalSharedCoreCleanup') {
    const cleanup = operation.cleanup
    const block = operation.localShared?.block
    if (block?._tag !== 'LocalSharedControlBlockPlan')
      throw new RangeError('Wasm local-shared cleanup lost its target control-block plan')
    const base = scalar(operation.local)
    const memory = requireMemory().memory
    const decrement = [
      Instr.localGet(base),
      Instr.localGet(base),
      Instr.memoryAccess('i32.load', memory, { offset: block.strongOffset }),
      Instr.i32Const(1),
      Instr.op('i32.sub'),
      Instr.memoryAccess('i32.store', memory, { offset: block.strongOffset }),
    ]
    const last = [
      ...semanticLanesOf(cleanup.element).flatMap((lane) => {
        const offset = LayoutVerify.laneOffset(plan, cleanup.element, lane.path)
        if (offset === undefined)
          throw new RangeError('Wasm local-shared cleanup lost a payload lane')
        return loadAt(base, block.valueOffset + offset, lane)
      }),
      Instr.call(resolve(LocalSharedPayloadCleanup.declaration, [cleanup.element])),
      Instr.op('drop'),
      ...releaseAtAddress(cleanup.allocation, base, block.allocationOffset),
    ]
    return [
      Instr.localGet(base),
      Instr.memoryAccess('i32.load', memory, { offset: block.strongOffset }),
      Instr.i32Const(1),
      Instr.op('i32.gt_u'),
      Instr.ifElse(Instr.emptyBlockType, decrement, last),
    ]
  }
  return releaseInstructions(operation.cleanup, operation.local)
}

const emitMakeEffectOrMakeCallableOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'MakeEffect' | 'MakeCallable' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, memory, slots, copy, materializeRoot, frameAddress } = state

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

const emitPackEffectCompositeOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'PackEffectComposite' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, laneBridge } = state

  const source = slots(operation.source)
  const destination = slots(operation.destination)
  const tag = destination.at(0)
  if (tag === undefined) throw new RangeError('Wasm Effect composite lost its tag lane')
  const payload = destination.slice(1)
  const instructions: Array<Instr.Instr> = [
    Instr.i32Const(operation.alternative),
    Instr.localSet(tag),
  ]
  for (const [ordinal, target] of payload.entries()) {
    const selected = source.at(ordinal)
    instructions.push(
      ...(selected === undefined
        ? [zeroConst(layout.physicalTypes.at(target) ?? i32)]
        : [
            Instr.localGet(selected),
            ...laneBridge(
              layout.physicalTypes.at(selected) ?? i32,
              layout.physicalTypes.at(target) ?? i32,
            ),
          ]),
      Instr.localSet(target),
    )
  }
  return instructions
}

const emitPackEffectOutcomeOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'PackEffectOutcome' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, zeroFor, outcomePayload } = state

  const source = slots(operation.source)
  const destination = slots(operation.destination)
  const tag = destination.at(0)
  if (tag === undefined) throw new RangeError('Wasm effect outcome lost its tag lane')
  const sourceType = layout.types.at(operation.source.ordinal)
  if (sourceType === undefined) throw new RangeError('Wasm effect outcome lost its source type')
  const payload = outcomePayload(source, Mir.semanticType(sourceType), operation.type.type)
  return [
    Instr.i32Const(operation.tag),
    Instr.localSet(tag),
    ...destination
      .slice(1)
      .flatMap((target, index) => [
        ...(payload.at(index) ?? [zeroFor(target)]),
        Instr.localSet(target),
      ]),
  ]
}

const emitPackEffectFailureUnionOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'PackEffectFailureUnion' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, zeroFor, failurePayload } = state

  const source = slots(operation.source)
  const destination = slots(operation.destination)
  const sourceTag = source.at(0)
  const destinationTag = destination.at(0)
  if (sourceTag === undefined || destinationTag === undefined)
    throw new RangeError('Wasm Effect failure union lost a tag lane')
  const payload = failurePayload(
    source,
    operation.sourceType.type,
    sourceTag,
    operation.type.type,
    operation.mappings,
  )
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
    ...destination
      .slice(1)
      .flatMap((target, index) => [
        ...(payload.at(index) ?? [zeroFor(target)]),
        Instr.localSet(target),
      ]),
  ]
}

const emitPropagateEffectFailureOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'PropagateEffectFailure' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, slots, failurePayload, releaseInstructions } = state

  const source = slots(operation.source)
  const sourceTag = operation.sourceType._tag === 'Union' ? source.at(0) : undefined
  const mapTag =
    operation.sourceType._tag === 'Nominal'
      ? [Instr.i32Const(operation.tagMappings.at(0)?.target ?? -1)]
      : sourceTag === undefined
        ? []
        : [
            Instr.i32Const(-1),
            Instr.localSet(layout.scratch),
            ...operation.tagMappings.flatMap((mapping) => [
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
          ]
  return [
    ...(operation.releases ?? []).flatMap((release) =>
      releaseInstructions(release.cleanup, release.local),
    ),
    ...mapTag,
    ...failurePayload(
      source,
      Mir.semanticType(operation.sourceType),
      sourceTag,
      operation.propagationType.type,
      operation.tagMappings,
    ).flat(),
    Instr.op('return'),
  ]
}

const emitUnpackEffectSuccessOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'UnpackEffectSuccess' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { slots, copy } = state
  return copy(
    slots(operation.source).slice(1, 1 + slots(operation.destination).length),
    slots(operation.destination),
  )
}

const emitRunEffectOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RunEffect' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const {
    layout,
    resolve,
    suspension,
    skipInvocation,
    slots,
    copy,
    failurePayload,
    reloadReachableRoots,
    releaseInstructions,
  } = state

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
  const failure = [
    // Owners still live at this site release before the failure leaves.
    ...(operation.releases ?? []).flatMap((release) =>
      releaseInstructions(release.cleanup, release.local),
    ),
    ...mapTag,
    Instr.localGet(layout.scratch),
    ...failurePayload(
      outcomeSlots,
      operation.outcomeType.type,
      tag,
      operation.propagationType.type,
      operation.tagMappings,
    ).flat(),
    Instr.op('return'),
  ]
  const invoke = [
    ...operation.arguments.flatMap((argument) =>
      slots(argument).map((slot) => Instr.localGet(slot)),
    ),
    Instr.call(resolve(operation.target, operation.typeArguments)),
    ...[...outcomeSlots].reverse().map((slot) => Instr.localSet(slot)),
    ...reloadReachableRoots(operation.arguments),
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

const emitRunEffectValueOrRunStaticEffectOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RunEffectValue' | 'RunStaticEffect' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const {
    layout,
    resolve,
    suspension,
    skipInvocation,
    slots,
    copy,
    failurePayload,
    reloadReachableRoots,
    releaseInstructions,
  } = state

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
    ...reloadReachableRoots(
      operation._tag === 'RunEffectValue'
        ? [operation.effect, ...operation.arguments]
        : [...operation.captures.map((capture) => capture.source), ...operation.arguments],
    ),
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
  const failure = [
    // Owners still live at this site release before the failure leaves.
    ...(operation.releases ?? []).flatMap((release) =>
      releaseInstructions(release.cleanup, release.local),
    ),
    ...mapTag,
    Instr.localGet(layout.scratch),
    ...failurePayload(
      outcomeSlots,
      operation.outcomeType.type,
      tag,
      operation.propagationType.type,
      operation.tagMappings,
    ).flat(),
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

const emitRunEffectCompositeOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'RunEffectComposite' }>,
  context: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const {
    layout,
    plan,
    resolve,
    suspension,
    skipInvocation,
    slots,
    zeroFor,
    copy,
    laneBridge,
    failurePayload,
    reloadReachableRoots,
    releaseInstructions,
  } = context

  const suspensionRegion = suspension?.regions.get(operation)
  if (suspensionRegion?._tag === 'SuspendEffectRegion')
    return suspension?.originate(suspensionRegion) ?? []
  const compositeSlots = slots(operation.effect)
  const choice = compositeSlots.at(0)
  const outcomeSlots = slots(operation.outcome)
  const destinationSlots = slots(operation.destination)
  const outcomeTag = outcomeSlots.at(0)
  if (choice === undefined || outcomeTag === undefined)
    throw new RangeError('Wasm Effect composite lost its tag lane')
  const invokeAlternative = (
    alternative: (typeof operation.alternatives)[number],
  ): ReadonlyArray<Instr.Instr> => {
    const captureLanes = laneKindsOf(plan, alternative.type)
    const sourceOutcomeLanes = laneKindsOf(
      plan,
      Object.freeze({ _tag: 'EffectOutcome' as const, type: alternative.type.type }),
    )
    const captureInputs = captureLanes.flatMap((lane, ordinal) => {
      const source = compositeSlots.at(ordinal + 1)
      if (source === undefined)
        throw new RangeError('Wasm Effect composite lost an alternative capture lane')
      return [
        Instr.localGet(source),
        ...laneBridge(layout.physicalTypes.at(source) ?? i32, laneValueType(plan, lane)),
      ]
    })
    const clearUnused = outcomeSlots
      .slice(sourceOutcomeLanes.length)
      .flatMap((target) => [zeroFor(target), Instr.localSet(target)])
    const storeOutcome = [...sourceOutcomeLanes]
      .map((lane, ordinal) => ({ lane, ordinal }))
      .reverse()
      .flatMap(({ lane, ordinal }) => {
        const target = outcomeSlots.at(ordinal)
        if (target === undefined)
          throw new RangeError('Wasm Effect composite outcome exceeds its joined carrier')
        return [
          ...laneBridge(laneValueType(plan, lane), layout.physicalTypes.at(target) ?? i32),
          Instr.localSet(target),
        ]
      })
    const remapFailureTag = alternative.tagMappings.flatMap((mapping) => [
      Instr.localGet(outcomeTag),
      Instr.i32Const(mapping.source),
      Instr.op('i32.eq'),
      Instr.ifElse(
        Instr.emptyBlockType,
        [Instr.i32Const(mapping.target), Instr.localSet(outcomeTag)],
        [],
      ),
    ])
    return [
      ...clearUnused,
      ...captureInputs,
      ...alternative.arguments.flatMap((argument) =>
        slots(argument).map((slot) => Instr.localGet(slot)),
      ),
      Instr.call(resolve(alternative.runner, alternative.runnerTypeArguments)),
      ...storeOutcome,
      ...remapFailureTag,
      ...reloadReachableRoots([operation.effect, ...alternative.arguments]),
    ]
  }
  let dispatch: ReadonlyArray<Instr.Instr> = [Instr.op('unreachable')]
  for (let ordinal = operation.alternatives.length - 1; ordinal >= 0; ordinal -= 1) {
    const alternative = operation.alternatives.at(ordinal)
    if (alternative === undefined) continue
    dispatch = [
      Instr.localGet(choice),
      Instr.i32Const(ordinal),
      Instr.op('i32.eq'),
      Instr.ifElse(Instr.emptyBlockType, invokeAlternative(alternative), dispatch),
    ]
  }
  const success = copy(outcomeSlots.slice(1, 1 + destinationSlots.length), destinationSlots)
  const completion = (() => {
    if (operation.propagationType === undefined) return success
    const mapTag = [
      Instr.i32Const(-1),
      Instr.localSet(layout.scratch),
      ...operation.tagMappings.flatMap((mapping) => [
        Instr.localGet(outcomeTag),
        Instr.i32Const(mapping.source),
        Instr.op('i32.eq'),
        Instr.ifElse(
          Instr.emptyBlockType,
          [Instr.i32Const(mapping.target), Instr.localSet(layout.scratch)],
          [],
        ),
      ]),
    ]
    const failure = [
      ...(operation.releases ?? []).flatMap((release) =>
        releaseInstructions(release.cleanup, release.local),
      ),
      ...mapTag,
      Instr.localGet(layout.scratch),
      ...failurePayload(
        outcomeSlots,
        operation.outcomeType.type,
        outcomeTag,
        operation.propagationType.type,
        operation.tagMappings,
      ).flat(),
      Instr.op('return'),
    ]
    return [
      Instr.localGet(outcomeTag),
      Instr.op('i32.eqz'),
      Instr.ifElse(Instr.emptyBlockType, success, failure),
    ]
  })()
  return [
    ...(skipInvocation ? [] : dispatch),
    ...(skipInvocation || suspensionRegion?._tag !== 'RunSuspendableEffectRegion'
      ? []
      : (suspension?.relay(suspensionRegion) ?? [])),
    ...completion,
  ]
}

const emitReifyEffectOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ReifyEffect' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const {
    layout,
    plan,
    resolve,
    suspension,
    skipInvocation,
    slots,
    laneBridge,
    zeroFor,
    reloadReachableRoots,
  } = state

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
    ...reloadReachableRoots([operation.effect, ...operation.arguments]),
  ]
  const writePayload = (
    source: ReadonlyArray<number>,
    destination: ReadonlyArray<number>,
    memberLanes: ReadonlyArray<LayoutPlan.CallingLane> = Object.freeze([]),
  ): ReadonlyArray<Instr.Instr> =>
    destination.flatMap((target, index) => {
      const value = source.at(index)
      const from = value === undefined ? undefined : layout.physicalTypes.at(value)
      const to = layout.physicalTypes.at(target)
      const member = memberLanes.at(index)
      const memberType = member === undefined ? undefined : laneValueType(plan, member)
      return [
        ...(value === undefined
          ? [zeroFor(target)]
          : [
              Instr.localGet(value),
              ...(from === undefined || memberType === undefined
                ? []
                : laneBridge(from, memberType)),
              ...(to === undefined || memberType === undefined
                ? from === undefined || to === undefined
                  ? []
                  : laneBridge(from, to)
                : laneBridge(memberType, to)),
            ]),
        Instr.localSet(target),
      ]
    })
  const resultPayload = destinationSlots.slice(1)
  const successLaneCount =
    operation.outcomeShape.tree._tag === 'OutcomeShape'
      ? operation.outcomeShape.tree.success.laneCount
      : 0
  const successShape = LayoutPlan.callingShape(plan, operation.outcomeType.type.success)
  if (successShape === undefined)
    throw new RangeError('Wasm Effect Result lost its success member shape')
  const success = [
    Instr.i32Const(operation.successTag),
    Instr.localSet(resultTag),
    ...writePayload(outcomeSlots.slice(1, 1 + successLaneCount), resultPayload, successShape.lanes),
  ]
  const failure =
    SilkType.failureMembers(operation.outcomeType.type).length === 0
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

const emitCloseEffectEntryOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'CloseEffectEntry' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { resolve, slots, scalar, copy, releaseInstructions } = state

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
          Instr.i32Const(1),
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

const emitCallOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Call' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { resolve, slots, reloadReachableRoots } = state
  return [
    ...operation.arguments.flatMap((argument) =>
      slots(argument).map((slot) => Instr.localGet(slot)),
    ),
    Instr.call(resolve(operation.target, operation.typeArguments)),
    ...[...slots(operation.destination)].reverse().map((slot) => Instr.localSet(slot)),
    ...reloadReachableRoots(operation.arguments),
  ]
}

const emitApplyCallableOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ApplyCallable' }>,
  state: WasmOperationContext,
  explicitArguments?: ReadonlyArray<ReadonlyArray<Instr.Instr>>,
): ReadonlyArray<Instr.Instr> => {
  const { layout, plan, resolve, memory, slots, scalar, reloadReachableRoots } = state

  const sourceType =
    operation.callable === undefined ? undefined : layout.types.at(operation.callable.ordinal)
  const target =
    operation.target ?? (sourceType?._tag === 'CallableValue' ? sourceType.target : undefined)
  if (target === undefined)
    throw new RangeError('Wasm callable application lost its hidden identity')
  const captureGroups: Array<{
    readonly parameterOrdinal: number
    readonly operands: ReadonlyArray<Instr.Instr>
  }> = []
  if (operation.callable !== undefined) {
    if (sourceType?._tag !== 'CallableValue')
      throw new RangeError('Wasm stored callable lost its identity')
    const environmentSlots = slots(operation.callable)
    let cursor = 0
    for (const field of sourceType.environment?.fields ?? []) {
      if (memory === undefined && field.representation === 'Borrow')
        throw new RangeError('Wasm borrowed callable capture requires private memory')
      const fieldLanes = LayoutPlan.callableFieldLanes(plan, field)
      if (field.representation !== 'Borrow') {
        captureGroups.push(
          Object.freeze({
            parameterOrdinal: field.parameterOrdinal,
            operands: Object.freeze(
              environmentSlots
                .slice(cursor, cursor + fieldLanes.length)
                .map((slot) => Instr.localGet(slot)),
            ),
          }),
        )
        cursor += fieldLanes.length
        continue
      }
      const shape = LayoutPlan.callingShape(plan, field.type)
      if (shape === undefined)
        throw new RangeError('Wasm borrowed callable capture lost its calling shape')
      const pointer = environmentSlots.at(cursor)
      if (pointer === undefined || memory === undefined)
        throw new RangeError('Wasm borrowed callable capture lost its pointer')
      cursor += 1
      const operands: Array<Instr.Instr> = []
      for (const lane of shape.lanes) {
        const offset = LayoutVerify.laneOffset(memory.plan, field.type, lane.path)
        if (offset === undefined)
          throw new RangeError('Wasm borrowed callable capture lost its lane offset')
        operands.push(
          Instr.localGet(pointer),
          Instr.i32Const(offset),
          Instr.op('i32.add'),
          Instr.memoryAccess(laneLoadMnemonic(memory.plan, lane), memory.memory),
        )
      }
      captureGroups.push(
        Object.freeze({
          parameterOrdinal: field.parameterOrdinal,
          operands: Object.freeze(operands),
        }),
      )
    }
  } else {
    for (const capture of operation.captures) {
      captureGroups.push(
        Object.freeze({
          parameterOrdinal: capture.parameterOrdinal,
          operands: Object.freeze(slots(capture.source).map((slot) => Instr.localGet(slot))),
        }),
      )
    }
  }
  const captureOperands = [...captureGroups]
    .sort((left, right) => left.parameterOrdinal - right.parameterOrdinal)
    .flatMap((capture) => [...capture.operands])
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
        Instr.op(scalarActor.spelling === 'f32' ? 'i32.reinterpret_f32' : 'i64.reinterpret_f64'),
        Instr.localSet(scalar(operation.destination)),
      ]
    if (target.operation === 'FromBits' && scalarActor?.category === 'Floating')
      return [
        Instr.localGet(left),
        Instr.op(scalarActor.spelling === 'f32' ? 'f32.reinterpret_i32' : 'f64.reinterpret_i64'),
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
  const diverges = sourceType?._tag === 'CallableValue' && SilkType.isNever(sourceType.type.result)
  return [
    ...(
      explicitArguments ??
      operation.arguments.map((argument) => slots(argument).map((slot) => Instr.localGet(slot)))
    ).flat(),
    ...captureOperands,
    Instr.call(resolve(target.declaration, operation.typeArguments)),
    ...(diverges
      ? [Instr.op('unreachable')]
      : [
          ...[...slots(operation.destination)].reverse().map((slot) => Instr.localSet(slot)),
          ...reloadReachableRoots([
            ...(operation.callable === undefined ? [] : [operation.callable]),
            ...operation.captures.map((capture) => capture.source),
            ...operation.arguments,
          ]),
        ]),
  ]
}

const emitConvertIntegerOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ConvertInteger' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, scalar } = state

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

const emitConvertScalarOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ConvertScalar' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { plan, scalar } = state

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
  if (source.category === 'Character' && target.spelling === 'u32')
    return [Instr.localGet(sourceSlot), Instr.localSet(targetSlot)]
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

const emitReinterpretScalarOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'ReinterpretScalar' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { scalar } = state

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

const emitFloatUnaryOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'FloatUnary' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { scalar } = state

  const source = Scalar.find(operation.sourceType._tag)
  if (source?.category !== 'Floating') throw new RangeError('Wasm float unary lost its source type')
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

const emitFloatTranscendentalOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'FloatTranscendental' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, scalar } = state

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

const emitCheckedScalarOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'CheckedScalar' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, plan, slots, scalar } = state

  const destination = slots(operation.destination)
  const tag = destination.at(0)
  const payload = destination.at(1)
  const left = operation.operands.at(0)
  const right = operation.operands.at(1)
  const source = Scalar.find(operation.sourceType._tag)
  const target = Scalar.find(operation.valueType._tag)
  if (
    operation.operation === 'CheckedConvertToChar' &&
    tag !== undefined &&
    payload !== undefined &&
    left !== undefined &&
    source?.spelling === 'u32' &&
    target?.category === 'Character'
  ) {
    const successOrdinal = operation.type.type.members.findIndex((member) =>
      SilkType.equals(member, operation.success),
    )
    const failureOrdinal = operation.type.type.members.findIndex((member) =>
      SilkType.equals(member, operation.failure),
    )
    if (successOrdinal < 0 || failureOrdinal < 0)
      throw new RangeError('Wasm checked char operation lost its Option members')
    const leftSlot = scalar(left)
    return [
      Instr.localGet(leftSlot),
      Instr.i32Const(0x10ffff),
      Instr.op('i32.gt_u'),
      Instr.localGet(leftSlot),
      Instr.i32Const(0xd800),
      Instr.op('i32.ge_u'),
      Instr.localGet(leftSlot),
      Instr.i32Const(0xdfff),
      Instr.op('i32.le_u'),
      Instr.op('i32.and'),
      Instr.op('i32.or'),
      Instr.localSet(tag),
      Instr.i32Const(failureOrdinal),
      Instr.i32Const(successOrdinal),
      Instr.localGet(tag),
      Instr.op('select'),
      Instr.localSet(tag),
      Instr.localGet(leftSlot),
      Instr.localSet(payload),
    ]
  }
  if (
    tag === undefined ||
    payload === undefined ||
    left === undefined ||
    source?.category !== 'Integer' ||
    target?.category !== 'Integer'
  )
    throw new RangeError('Wasm checked scalar operation lost its Option lanes')
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
    throw new RangeError('Wasm checked scalar operation lost its Option members')
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
      ...checkedArithmeticOutcome(shape, target, leftSlot, rightSlot, resultScratch, pointerBits),
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

const emitBinaryOperation = (
  operation: Extract<Mir.Operation, { readonly _tag: 'Binary' }>,
  state: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  const { layout, plan, scalar } = state

  const leftType = layout.types.at(operation.left.ordinal)
  const semantic = leftType === undefined ? undefined : Mir.semanticType(leftType)
  const scalarType = typeof semantic === 'string' ? Scalar.find(semantic) : undefined
  if (scalarType?.category === 'Floating') {
    const prefix = scalarType.spelling
    if (operation.operator === 'TotalOrder') {
      const temporary = prefix === 'f32' ? layout.scratch : layout.scratch64
      if (temporary === undefined) throw new RangeError('Wasm total order lost its scratch local')
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

/** Emits one MIR operation through its independently addressable sibling emitter. */
const emitOperationWithContext = (
  operation: Mir.Operation,
  context: WasmOperationContext,
): ReadonlyArray<Instr.Instr> => {
  switch (operation._tag) {
    case 'StaticString':
      return emitStaticStringOperation(operation, context)
    case 'StringFromUtf8Unchecked':
      return emitStringFromUtf8UncheckedOperation(operation, context)
    case 'StringUtf8Bytes':
      return emitStringUtf8BytesOperation(operation, context)
    case 'StringByteLength':
      return emitStringByteLengthOperation(operation, context)
    case 'StringEqualsExact':
      return emitStringEqualsExactOperation(operation, context)
    case 'ValidateLayout':
      return emitValidateLayoutOperation(operation, context)
    case 'RepeatLayout':
      return emitRepeatLayoutOperation(operation, context)
    case 'Allocate':
      return emitAllocateOperation(operation, context)
    case 'HostWrite':
      return emitHostWriteOperation(operation, context)
    case 'OsCall':
      return emitOsCallOperation(operation, context)
    case 'RawBufferFrom':
      return emitRawBufferFromOperation(operation, context)
    case 'SharedFromAllocation':
      return emitSharedFromAllocationOperation(operation, context)
    case 'ExecutionFromAllocation':
      return emitExecutionFromAllocationOperation(operation, context)
    case 'ExecutionDrive':
      return emitExecutionDriveOperation(operation, context)
    case 'ExecutionWake':
      return emitExecutionWakeOperation(operation, context)
    case 'ExecutionPark':
      return emitExecutionParkOperation(operation, context)
    case 'SharedClone':
      return emitSharedCloneOperation(operation, context)
    case 'SharedWithMut':
      return emitSharedWithMutOperation(operation, context)
    case 'RawBufferCount':
      return emitRawBufferCountOperation(operation, context)
    case 'RawBufferSlot':
      return emitRawBufferSlotOperation(operation, context)
    case 'RawBufferRead':
      return emitRawBufferReadOperation(operation, context)
    case 'RawBufferView':
      return emitRawBufferViewOperation(operation, context)
    case 'RawBufferCopy':
      return emitRawBufferCopyOperation(operation, context)
    case 'RawBufferFill':
      return emitRawBufferFillOperation(operation, context)
    case 'SlotWrite':
      return emitSlotWriteOperation(operation, context)
    case 'SlotTake':
      return emitSlotTakeOrSlotCopyOperation(operation, context)
    case 'SlotCopy':
      return emitSlotTakeOrSlotCopyOperation(operation, context)
    case 'SlotDrop':
      return emitSlotDropOperation(operation, context)
    case 'ShortCircuit':
      return emitShortCircuitOperation(operation, context)
    case 'Match':
      return emitMatchOperation(operation, context)
    case 'Literal':
      return emitLiteralOperation(operation, context)
    case 'StaticView':
      return emitStaticViewOperation(operation, context)
    case 'Move':
      return emitMoveOperation(operation, context)
    case 'BeginLoan':
      return emitBeginLoanOperation(operation, context)
    case 'EndLoan':
      return emitEndLoanOperation(operation, context)
    case 'SliceLength':
      return emitSliceLengthOperation(operation, context)
    case 'ConvertUnion':
      return emitConvertUnionOperation(operation, context)
    case 'Construct':
      return emitConstructOperation(operation, context)
    case 'ConstructArray':
      return emitConstructArrayOperation(operation, context)
    case 'Project':
      return emitProjectOperation(operation, context)
    case 'ReadPlace':
      return emitReadPlaceOperation(operation, context)
    case 'CheckPlace':
      return emitCheckPlaceOperation(operation, context)
    case 'WritePlace':
      return emitWritePlaceOperation(operation, context)
    case 'Drop':
      return emitDropOperation(operation, context)
    case 'MakeEffect':
      return emitMakeEffectOrMakeCallableOperation(operation, context)
    case 'MakeCallable':
      return emitMakeEffectOrMakeCallableOperation(operation, context)
    case 'PackEffectComposite':
      return emitPackEffectCompositeOperation(operation, context)
    case 'PackEffectOutcome':
      return emitPackEffectOutcomeOperation(operation, context)
    case 'PackEffectFailureUnion':
      return emitPackEffectFailureUnionOperation(operation, context)
    case 'PropagateEffectFailure':
      return emitPropagateEffectFailureOperation(operation, context)
    case 'UnpackEffectSuccess':
      return emitUnpackEffectSuccessOperation(operation, context)
    case 'RunEffect':
      return emitRunEffectOperation(operation, context)
    case 'RunEffectValue':
      return emitRunEffectValueOrRunStaticEffectOperation(operation, context)
    case 'RunStaticEffect':
      return emitRunEffectValueOrRunStaticEffectOperation(operation, context)
    case 'RunEffectComposite':
      return emitRunEffectCompositeOperation(operation, context)
    case 'ReifyEffect':
      return emitReifyEffectOperation(operation, context)
    case 'CloseEffectEntry':
      return emitCloseEffectEntryOperation(operation, context)
    case 'Call':
      return emitCallOperation(operation, context)
    case 'ApplyCallable':
      return emitApplyCallableOperation(operation, context)
    case 'ConvertInteger':
      return emitConvertIntegerOperation(operation, context)
    case 'ConvertScalar':
      return emitConvertScalarOperation(operation, context)
    case 'ReinterpretScalar':
      return emitReinterpretScalarOperation(operation, context)
    case 'FloatUnary':
      return emitFloatUnaryOperation(operation, context)
    case 'FloatTranscendental':
      return emitFloatTranscendentalOperation(operation, context)
    case 'CheckedScalar':
      return emitCheckedScalarOperation(operation, context)
    case 'Binary':
      return emitBinaryOperation(operation, context)
  }
}

const emitOperation = (
  operation: Mir.Operation,
  context: WasmEmitContext.WasmEmitContext<Layout, WasmSuspensionRuntime>,
  suspension?: WasmSuspensionFunctionContext,
  skipInvocation = false,
): ReadonlyArray<Instr.Instr> =>
  emitOperationWithContext(operation, makeOperationContext(context, suspension, skipInvocation))

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
  readonly frames: ReadonlyMap<string, Mir.CoroutineFrameTargetLayout>
  readonly layouts: ReadonlyMap<string, Mir.CoroutineFrameTargetStateLayout>
  /** Typed cleanup thunk selected by the resume id stored in each retained frame. */
  readonly frameCleanups: ReadonlyMap<number, FuncActor.Func>
  readonly frameStackPointer: Global.Global
  /** Non-LIFO-safe free list for fixed-size continuation frame slots. */
  readonly freeFrameHead: Global.Global
  readonly frameSlotSize: number
  /** Package selected by the currently running independent root; zero outside ExecutionDrive. */
  readonly activeExecution: Global.Global
  /** Retained frame head restored by the next owner-selected drive. */
  readonly externalResumeHead: Global.Global
  readonly frameMemory: Memory.Memory
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
  context: WasmEmitContext.WasmEmitContext<Layout, WasmSuspensionRuntime>,
): ReadonlyArray<Instr.Instr> => {
  const { fn, layout, plan, memory, suspensionRuntime } = context
  const regions = new Map(fn.regions.map((region) => [region.id.ordinal, region] as const))
  const slots = (local: Mir.LocalId): ReadonlyArray<number> => layout.slots.at(local.ordinal) ?? []
  const copySlots = (
    source: ReadonlyArray<number>,
    destination: ReadonlyArray<number>,
  ): ReadonlyArray<Instr.Instr> => {
    if (source.length !== destination.length)
      throw new RangeError('Wasm coroutine state cannot copy mismatched logical lane bundles')
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
        const offset = LayoutVerify.laneOffset(memory.plan, type.type, lane.path)
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
    return laneKindsOf(plan, fn.result)
  })()
  const returnTransfer = (): ReadonlyArray<Instr.Instr> => [
    ...restoreFrame(),
    ...resultLanes.map((lane) => zeroConst(laneValueType(plan, lane))),
    Instr.op('return'),
  ]
  const coroutineFrame =
    suspensionRuntime === undefined
      ? undefined
      : suspensionRuntime.frames.get(Instances.keyText(fn.instance))
  const acquireCoroutineFrame = (): ReadonlyArray<Instr.Instr> => {
    const scratch = layout.suspensionScratch
    if (suspensionRuntime === undefined || coroutineFrame === undefined || scratch === undefined)
      return []
    const frameAlignment = Math.max(16, coroutineFrame.alignment)
    const frameSize = suspensionRuntime.frameSlotSize
    const reportStorageExhaustion = [
      Instr.i32Const(statusAddress),
      Instr.i32Const(statusStackOverflow),
      Instr.memoryAccess('i32.store', suspensionRuntime.memory),
      Instr.op('unreachable'),
    ]
    return [
      Instr.localGet(scratch.frame),
      Instr.ifElse(
        Instr.emptyBlockType,
        [],
        [
          Instr.globalGet(suspensionRuntime.freeFrameHead),
          Instr.localTee(scratch.frame),
          Instr.ifElse(
            Instr.emptyBlockType,
            [
              Instr.localGet(scratch.frame),
              Instr.memoryAccess('i32.load', suspensionRuntime.frameMemory),
              Instr.globalSet(suspensionRuntime.freeFrameHead),
            ],
            [
              Instr.globalGet(suspensionRuntime.frameStackPointer),
              Instr.i32Const(frameAlignment - 1),
              Instr.op('i32.add'),
              Instr.i32Const(-frameAlignment),
              Instr.op('i32.and'),
              Instr.localTee(scratch.frame),
              Instr.i32Const(frameSize),
              Instr.op('i32.add'),
              Instr.localTee(scratch.append),
              Instr.localGet(scratch.frame),
              Instr.op('i32.lt_u'),
              Instr.ifElse(Instr.emptyBlockType, reportStorageExhaustion, []),
              Instr.localGet(scratch.append),
              Instr.i32Const(1),
              Instr.op('i32.sub'),
              Instr.i32Const(16),
              Instr.op('i32.shr_u'),
              Instr.i32Const(1),
              Instr.op('i32.add'),
              Instr.localTee(scratch.next),
              Instr.memorySize(suspensionRuntime.frameMemory),
              Instr.op('i32.gt_u'),
              Instr.ifElse(
                Instr.emptyBlockType,
                [
                  Instr.localGet(scratch.next),
                  Instr.memorySize(suspensionRuntime.frameMemory),
                  Instr.op('i32.sub'),
                  Instr.memoryGrow(suspensionRuntime.frameMemory),
                  Instr.i32Const(-1),
                  Instr.op('i32.eq'),
                  Instr.ifElse(Instr.emptyBlockType, reportStorageExhaustion, []),
                ],
                [],
              ),
              Instr.localGet(scratch.append),
              Instr.globalSet(suspensionRuntime.frameStackPointer),
            ],
          ),
        ],
      ),
    ]
  }
  const releaseCoroutineFrame = (): ReadonlyArray<Instr.Instr> => {
    const scratch = layout.suspensionScratch
    return suspensionRuntime === undefined || coroutineFrame === undefined || scratch === undefined
      ? []
      : [
          Instr.localGet(scratch.frame),
          Instr.ifElse(
            Instr.emptyBlockType,
            [
              Instr.localGet(scratch.frame),
              Instr.globalGet(suspensionRuntime.freeFrameHead),
              Instr.memoryAccess('i32.store', suspensionRuntime.frameMemory),
              Instr.localGet(scratch.frame),
              Instr.globalSet(suspensionRuntime.freeFrameHead),
            ],
            [],
          ),
        ]
  }
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
            const child = suspensionRuntime.origins.get(Backend.suspensionPointKey(region.point))
            if (child === undefined) throw new RangeError('Wasm suspension origin lost child id')
            const inputs = suspensionOperationInputs(region.operation)
            const lanes = inputs.flatMap((local) => layout.lanes.at(local.ordinal) ?? [])
            const values = inputs.flatMap((local) => layout.slots.at(local.ordinal) ?? [])
            const packed = packWasmLanes(lanes, plan, suspensionRuntime.transferHeaderSize)
            if (packed.lanes.length !== values.length)
              throw new RangeError('Wasm suspension origin argument lanes disagree')
            return [
              ...storeI32(suspensionRuntime.transferAddress, child),
              ...storeI32(suspensionRuntime.transferAddress + plan.target.pointerSize * 2, 0),
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
            const descriptor = region.relay.state
            const transfer = [
              Instr.globalGet(suspensionRuntime.status),
              Instr.ifElse(Instr.emptyBlockType, returnTransfer(), []),
            ]
            if (descriptor === undefined) return transfer
            const targetLayout = suspensionRuntime.layouts.get(
              Backend.suspensionPointKey(descriptor.point),
            )
            const resume = suspensionRuntime.resumes.get(
              Backend.suspensionPointKey(descriptor.point),
            )
            const scratch = layout.suspensionScratch
            if (targetLayout === undefined || resume === undefined || scratch === undefined)
              throw new RangeError('Wasm stateful relay lost its layout or dispatch identity')
            const { frame, append, next } = scratch
            if (memory === undefined)
              throw new RangeError('Wasm coroutine-frame stack lost its memory context')
            const ownerFrame = coroutineFrame
            if (ownerFrame === undefined)
              throw new RangeError('Wasm coroutine-frame stack lost its invocation layout')
            const initialize = [
              Instr.localGet(frame),
              Instr.op('i32.eqz'),
              Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
              Instr.i32Const(suspensionRuntime.transferAddress + plan.target.pointerSize * 2),
              Instr.memoryAccess('i32.load', suspensionRuntime.memory),
              Instr.localSet(append),
              Instr.localGet(append),
              Instr.ifElse(
                Instr.valueBlockType(i32),
                [
                  Instr.localGet(append),
                  Instr.memoryAccess('i32.load', suspensionRuntime.frameMemory),
                ],
                [
                  Instr.i32Const(suspensionRuntime.transferAddress + plan.target.pointerSize),
                  Instr.memoryAccess('i32.load', suspensionRuntime.memory),
                ],
              ),
              Instr.localSet(next),
              Instr.localGet(frame),
              Instr.localGet(next),
              Instr.memoryAccess('i32.store', suspensionRuntime.frameMemory),
              Instr.localGet(frame),
              Instr.i32Const(resume),
              Instr.memoryAccess('i32.store', suspensionRuntime.frameMemory, {
                offset: plan.target.pointerSize,
              }),
              ...targetLayout.payload.flatMap((field) => {
                const fieldLanes = layout.lanes.at(field.local.ordinal) ?? []
                const fieldSlots = layout.slots.at(field.local.ordinal) ?? []
                const packed = packWasmLanes(fieldLanes, plan, field.offset)
                return packed.lanes.flatMap((lane, ordinal) => {
                  const value = fieldSlots.at(ordinal)
                  return value === undefined
                    ? []
                    : [
                        Instr.localGet(frame),
                        Instr.localGet(value),
                        Instr.memoryAccess(
                          lane.type === i64
                            ? 'i64.store'
                            : lane.type === f32
                              ? 'f32.store'
                              : lane.type === f64
                                ? 'f64.store'
                                : 'i32.store',
                          suspensionRuntime.frameMemory,
                          { offset: lane.offset },
                        ),
                      ]
                })
              }),
              Instr.localGet(append),
              Instr.ifElse(
                Instr.emptyBlockType,
                [
                  Instr.localGet(append),
                  Instr.localGet(frame),
                  Instr.memoryAccess('i32.store', suspensionRuntime.frameMemory),
                ],
                [
                  Instr.i32Const(suspensionRuntime.transferAddress + plan.target.pointerSize),
                  Instr.localGet(frame),
                  Instr.memoryAccess('i32.store', suspensionRuntime.memory),
                ],
              ),
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
          ...releaseCoroutineFrame(),
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
        ...operations.flatMap((operation) => emitOperation(operation, context, suspensionContext)),
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
        emitOperation(operation, context, suspensionContext),
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
    const currentFrame = layout.suspensionScratch?.frame
    if (currentFrame === undefined) return []
    const dispatch = (fn.suspension?.regions ?? []).flatMap((region) => {
      if (region._tag !== 'RunSuspendableEffectRegion' || region.relay.state === undefined)
        return []
      const id = suspensionRuntime.resumes.get(Backend.suspensionPointKey(region.point))
      const targetLayout = suspensionRuntime.layouts.get(Backend.suspensionPointKey(region.point))
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
            Instr.localGet(currentFrame),
            Instr.memoryAccess(
              laneLoadMnemonic(
                plan,
                lanes.at(ordinal) ??
                  (() => {
                    throw new RangeError('Wasm coroutine payload lost its lane')
                  })(),
              ),
              suspensionRuntime.frameMemory,
              { offset: lane.offset },
            ),
            Instr.localSet(destination),
          ]
        })
      })
      const outcomeLanes =
        region.operation._tag === 'ExecutionPark'
          ? []
          : (layout.lanes.at(region.operation.outcome.ordinal) ?? [])
      const outcomeSlots =
        region.operation._tag === 'ExecutionPark'
          ? []
          : (layout.slots.at(region.operation.outcome.ordinal) ?? [])
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
        context,
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
              .flatMap((entry) => emitOperation(entry, context, suspensionContext)),
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
    return [
      Instr.globalGet(suspensionRuntime.resumeFrame),
      Instr.localSet(currentFrame),
      Instr.i32Const(0),
      Instr.globalSet(suspensionRuntime.resumeFrame),
      ...dispatch,
    ]
  }

  return [
    ...reserveFrame(),
    ...loadBorrowedParameters(),
    ...(suspensionRuntime === undefined
      ? []
      : [Instr.i32Const(0), Instr.globalSet(suspensionRuntime.status)]),
    ...resumeDispatch(),
    ...acquireCoroutineFrame(),
    ...emitRegion(fn.entry, Object.freeze([])),
    Instr.op('unreachable'),
  ]
}

interface EmittedProgram {
  readonly symbols: ReadonlyArray<Backend.SymbolEntry>
  readonly ir: string
  readonly bitcode: Uint8Array
}

const emitProgramUnmapped = Effect.fnUntraced(function* (
  program: Mir.Module,
  request: Backend.CodegenRequest,
) {
  const i32Layout = LayoutPlan.entry(program.layout, 'i32')
  if (i32Layout === undefined) {
    return yield* new Backend.BackendError({
      operation: 'Backend.emit',
      backend: 'WebAssembly',
      message: 'WebAssembly requires the planned i32 representation',
      reason: { _tag: 'InvalidMir', violations: MirVerification.verify(program) },
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
      reason: { _tag: 'InvalidMir', violations: MirVerification.verify(program) },
    })
  }
  // WebAssembly's debug-information equivalent of the LLVM backend's native debug metadata is
  // the `name` custom section, which the builder emits from the names given here. Debug builds
  // name the module, its functions, and their locals; release builds omit every name, which is
  // what the LLVM backend's `strip` flag does with its own metadata.
  const debug = request.mode === 'debug'
  const builder = yield* Builder.make(debug ? { moduleName: program.module } : {})
  const executionPackageCleanups = new Map<string, WasmEmitContext.ExecutionPackageCleanup>()
  for (const operation of program.functions.flatMap(MirVerification.operations)) {
    if (operation._tag !== 'ExecutionFromAllocation') continue
    executionPackageCleanups.set(
      operation.plan.provenance,
      Object.freeze({
        body: operation.bodyCleanup,
        endpoint: operation.endpointCleanup,
        callback: operation.callbackCleanup,
      }),
    )
  }
  const suspensionEnabled = program.functions.some((fn) => (fn.suspension?.regions.length ?? 0) > 0)
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
    return laneKindsOf(program.layout, type)
  }
  const suspensionRecords = WasmSuspension.records(program)
  const originRecords = suspensionRecords.origins
  const resumeRecords = suspensionRecords.resumes
  const originIds = suspensionRecords.originIds
  const resumeIds = suspensionRecords.resumeIds
  const coroutineFrames = suspensionRecords.frames
  const coroutineFrameStates = suspensionRecords.layouts
  const coroutineFrameSlotAlignment = Math.max(
    16,
    ...[...coroutineFrames.values()].map((frame) => frame.alignment),
  )
  const coroutineFrameSlotSize = alignUp(
    Math.max(1, ...[...coroutineFrames.values()].map((frame) => frame.size)),
    coroutineFrameSlotAlignment,
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
  const releasesBlocks = (plan: CleanupPlan.CleanupPlan): boolean => CleanupPlan.reclaims(plan)
  const needsHeap = program.functions.some((fn) =>
    MirVerification.operations(fn).some(
      (operation) =>
        operation._tag === 'Allocate' ||
        operation._tag === 'RawBufferFrom' ||
        operation._tag === 'SharedFromAllocation' ||
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
    MirVerification.operations(fn).some((operation) => operation._tag === 'HostWrite'),
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
  const privateExecutionStackPages = request.privateExecutionStackPages ?? 65536
  if (
    !Number.isSafeInteger(privateExecutionStackPages) ||
    privateExecutionStackPages < 1 ||
    privateExecutionStackPages > 65536
  ) {
    return yield* Effect.fail(
      new Backend.BackendError({
        operation: 'Backend.emit',
        backend: 'Wasm',
        message: 'privateExecutionStackPages must be an integer from 1 through 65536',
        reason: {
          _tag: 'UnsupportedMir',
          detail: 'privateExecutionStackPages must be an integer from 1 through 65536',
        },
      }),
    )
  }
  const coroutineFrameMemory = suspensionEnabled
    ? yield* Memory.make(
        builder,
        { min: 1, max: privateExecutionStackPages },
        debug ? { name: 'silk_coroutine_frame_memory' } : {},
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
  const suspendFrameStackPointer = suspensionEnabled
    ? yield* Global.make(
        builder,
        i32,
        true,
        [Instr.i32Const(16)],
        debug ? { name: 'silk_coroutine_frame_stack_pointer' } : {},
      )
    : undefined
  const suspendFreeFrameHead = suspensionEnabled
    ? yield* Global.make(
        builder,
        i32,
        true,
        [Instr.i32Const(0)],
        debug ? { name: 'silk_coroutine_frame_free_head' } : {},
      )
    : undefined
  const activeExecution = suspensionEnabled
    ? yield* Global.make(
        builder,
        i32,
        true,
        [Instr.i32Const(0)],
        debug ? { name: 'silk_active_execution' } : {},
      )
    : undefined
  const externalResumeHead = suspensionEnabled
    ? yield* Global.make(
        builder,
        i32,
        true,
        [Instr.i32Const(0)],
        debug ? { name: 'silk_external_resume_head' } : {},
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
  const frameCleanupThunks = new Map<number, FuncActor.Func>()
  if (suspensionEnabled && executionPackageCleanups.size > 0)
    for (const [ordinal, record] of resumeRecords.entries()) {
      const id = ordinal + 1
      frameCleanupThunks.set(
        id,
        yield* FuncActor.declare(
          builder,
          yield* WasmType.func(builder, [i32], []),
          debug ? { name: `silk_suspend_cleanup_${id}_${record.region.point.ordinal}` } : {},
        ),
      )
    }
  const machine = declared.find((entry) =>
    Mir.matchesInstanceKey(entry.fn, Mir.machineEntry(program)),
  )
  const independentDrivers = new Map<string, FuncActor.Func>()
  for (const entry of declared) {
    if (!entry.suspendable) continue
    const signature = yield* WasmType.func(
      builder,
      entry.fn.regions.length === 0
        ? []
        : entry.fn.localTypes
            .slice(0, entry.fn.parameterCount)
            .flatMap((type) => lanesFor(type).map((lane) => laneValueType(program.layout, lane))),
      lanesFor(entry.fn.result).map((lane) => laneValueType(program.layout, lane)),
    )
    independentDrivers.set(
      Instances.keyText(entry.fn.instance),
      yield* FuncActor.declare(
        builder,
        signature,
        debug ? { name: `${entry.symbol}$independent_root` } : {},
      ),
    )
  }
  const driver =
    machine === undefined
      ? undefined
      : independentDrivers.get(Instances.keyText(machine.fn.instance))
  const suspensionRuntime: WasmSuspensionRuntime | undefined =
    suspendStatus === undefined ||
    suspendResumePath === undefined ||
    suspendResumeFrame === undefined ||
    suspendFrameStackPointer === undefined ||
    suspendFreeFrameHead === undefined ||
    activeExecution === undefined ||
    externalResumeHead === undefined ||
    coroutineFrameMemory === undefined ||
    transferAddress === undefined ||
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
          frames: coroutineFrames,
          layouts: coroutineFrameStates,
          frameCleanups: frameCleanupThunks,
          frameStackPointer: suspendFrameStackPointer,
          freeFrameHead: suspendFreeFrameHead,
          frameSlotSize: coroutineFrameSlotSize,
          activeExecution,
          externalResumeHead,
          frameMemory: coroutineFrameMemory,
          memory: privateMemory,
        })

  const resolve = (
    targetId: DeclarationFacts.CanonicalId,
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
  const resolveIndependent = (
    targetId: DeclarationFacts.CanonicalId,
    typeArguments: ReadonlyArray<SilkType.GenericArgument>,
  ): FuncActor.Func => {
    const target = declared.find((candidate) =>
      Mir.matchesInstance(candidate.fn, targetId, typeArguments),
    )
    if (target === undefined) return resolve(targetId, typeArguments)
    return independentDrivers.get(Instances.keyText(target.fn.instance)) ?? target.handle
  }

  for (const [ordinal, record] of frameCleanupThunks.size === 0 ? [] : resumeRecords.entries()) {
    const id = ordinal + 1
    const handle = frameCleanupThunks.get(id)
    const frameLayout = coroutineFrameStates.get(Backend.suspensionPointKey(record.region.point))
    if (
      handle === undefined ||
      frameLayout === undefined ||
      privateMemory === undefined ||
      coroutineFrameMemory === undefined ||
      stackPointer === undefined ||
      heapPointer === undefined
    )
      throw new RangeError('Wasm frame cleanup thunk lost its typed frame plan')
    const { suspension: _suspension, ...cleanupFunctionBase } = record.fn
    const fields = frameLayout.payload.filter(
      (
        field,
      ): field is Mir.CoroutineFramePayloadField & {
        readonly access: Extract<Mir.CoroutineFrameAccess, { readonly _tag: 'AffineTransfer' }>
      } => field.access._tag === 'AffineTransfer',
    )
    const affineLocals = new Set(fields.map((field) => field.local.ordinal))
    const provenance = record.region.provenance
    const cleanupFunction: Mir.MirFunction = Object.freeze({
      ...cleanupFunctionBase,
      parameterCount: 1,
      localTypes: Object.freeze([
        Object.freeze({ _tag: 'usize' as const }),
        ...record.fn.localTypes.map((type, local) =>
          affineLocals.has(local) ? type : Object.freeze({ _tag: 'usize' as const }),
        ),
      ]),
      entry: Object.freeze({ _tag: 'Region', ordinal: 0 }),
      regions: Object.freeze([
        Object.freeze({
          _tag: 'CleanupRegion' as const,
          id: Object.freeze({ _tag: 'Region' as const, ordinal: 0 }),
          releases: Object.freeze(
            fields.map((field) =>
              Object.freeze({
                _tag: 'Drop' as const,
                local: Object.freeze({ _tag: 'Local' as const, ordinal: field.local.ordinal + 1 }),
                cleanup: field.access.cleanup,
                provenance,
              }),
            ),
          ),
          outcome: Object.freeze({
            _tag: 'Trap' as const,
            reason: 'frame cleanup helper',
            provenance,
          }),
        }),
      ]),
    })
    const cleanupFrame = framePlan(cleanupFunction, program.layout)
    const cleanupLayout = layoutOf(cleanupFunction, program.layout, cleanupFrame, debug, false)
    const memory: MemoryContext = Object.freeze({
      memory: privateMemory,
      stackPointer,
      stackBase: staticEnd,
      stackLimit: needsHeap ? stackLimit : undefined,
      heapPointer,
      frame: cleanupFrame,
      plan: program.layout,
      staticOffsets,
      ...(standardWrite === undefined ? {} : { standardWrite }),
      ...(heapAllocate === undefined ? {} : { heapAllocate }),
      ...(heapRelease === undefined ? {} : { heapRelease }),
    })
    const operation = makeOperationContext(
      Object.freeze({
        fn: cleanupFunction,
        layout: cleanupLayout,
        plan: program.layout,
        resolve,
        resolveIndependent,
        memory,
        executionPackageCleanups,
        ...(suspensionRuntime === undefined ? {} : { suspensionRuntime }),
      }),
    )
    const body = fields.flatMap((field) => {
      const local = Object.freeze({ _tag: 'Local' as const, ordinal: field.local.ordinal + 1 })
      const lanes = cleanupLayout.lanes.at(local.ordinal) ?? []
      const slots = cleanupLayout.slots.at(local.ordinal) ?? []
      const packed = packWasmLanes(lanes, program.layout, field.offset)
      if (packed.lanes.length !== slots.length)
        throw new RangeError('Wasm frame cleanup lost an affine payload lane')
      return [
        ...packed.lanes.flatMap((lane, laneOrdinal) => {
          const target = slots.at(laneOrdinal)
          const callingLane = lanes.at(laneOrdinal)
          if (target === undefined || callingLane === undefined)
            throw new RangeError('Wasm frame cleanup lost its typed local lane')
          return [
            Instr.localGet(0),
            Instr.memoryAccess(
              laneLoadMnemonic(program.layout, callingLane),
              coroutineFrameMemory,
              { offset: lane.offset },
            ),
            Instr.localSet(target),
          ]
        }),
        ...operation.releaseInstructions(field.access.cleanup, local),
      ]
    })
    yield* FuncActor.define(builder, handle, { locals: cleanupLayout.declared, body })
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
            Object.freeze({
              fn: entry.fn,
              layout,
              plan: program.layout,
              resolve,
              resolveIndependent,
              memory,
              executionPackageCleanups,
              ...(suspensionRuntime === undefined ? {} : { suspensionRuntime }),
            }),
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
      suspendResumePath === undefined ||
      suspendResumeFrame === undefined ||
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
        Instr.i32Const(0),
        Instr.globalSet(suspendResumePath),
        Instr.i32Const(0),
        Instr.globalSet(suspendResumeFrame),
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
      coroutineFrameMemory === undefined ||
      privateMemory === undefined
    )
      throw new RangeError('Wasm suspension resume thunk lost its owner or runtime')
    const ownerResultLanes = lanesFor(owner.fn.result)
    const ownerResultPacked = packWasmLanes(ownerResultLanes, program.layout, transferResultOffset)
    const parameterLanes = owner.fn.localTypes
      .slice(0, owner.fn.parameterCount)
      .flatMap((type) => lanesFor(type))
    yield* FuncActor.define(builder, thunk, {
      locals: ownerResultLanes.map((lane, laneOrdinal) =>
        debug
          ? { type: laneValueType(program.layout, lane), name: `result${laneOrdinal}` }
          : { type: laneValueType(program.layout, lane) },
      ),
      body: [
        Instr.i32Const(0),
        Instr.globalSet(suspendStatus),
        Instr.i32Const(id),
        Instr.globalSet(suspendResumePath),
        ...parameterLanes.map((lane) => zeroConst(laneValueType(program.layout, lane))),
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
        Instr.globalGet(suspendStatus),
      ],
    })
  }
  if (
    privateMemory !== undefined &&
    transferAddress !== undefined &&
    suspendStatus !== undefined &&
    suspendResumeFrame !== undefined &&
    suspendFrameStackPointer !== undefined &&
    externalResumeHead !== undefined &&
    coroutineFrameMemory !== undefined
  ) {
    for (const root of declared) {
      const rootDriver = independentDrivers.get(Instances.keyText(root.fn.instance))
      if (rootDriver === undefined) continue
      const parameterLanes = root.fn.localTypes
        .slice(0, root.fn.parameterCount)
        .flatMap((type) => lanesFor(type))
      const resultLanes = lanesFor(root.fn.result)
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
      yield* FuncActor.define(builder, rootDriver, {
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
          Instr.i32Const(0),
          Instr.globalSet(suspendStatus),
          Instr.globalGet(externalResumeHead),
          Instr.localTee(headLocal),
          Instr.op('i32.eqz'),
          Instr.ifElse(
            Instr.emptyBlockType,
            [
              Instr.i32Const(transferAddress + program.layout.target.pointerSize),
              Instr.i32Const(0),
              Instr.memoryAccess('i32.store', privateMemory),
              Instr.i32Const(transferAddress + program.layout.target.pointerSize * 2),
              Instr.i32Const(0),
              Instr.memoryAccess('i32.store', privateMemory),
              ...parameterLanes.map((_lane, ordinal) => Instr.localGet(ordinal)),
              Instr.call(root.handle),
              ...resultLanes
                .map((_lane, laneOrdinal) => resultBase + laneOrdinal)
                .reverse()
                .map((local) => Instr.localSet(local)),
              ...storeMachineResults,
              Instr.globalGet(suspendStatus),
              Instr.localSet(statusLocal),
            ],
            [
              Instr.i32Const(transferAddress + program.layout.target.pointerSize),
              Instr.localGet(headLocal),
              Instr.memoryAccess('i32.store', privateMemory),
              Instr.i32Const(transferAddress + program.layout.target.pointerSize * 2),
              Instr.i32Const(0),
              Instr.memoryAccess('i32.store', privateMemory),
              Instr.i32Const(0),
              Instr.globalSet(externalResumeHead),
              Instr.i32Const(0),
              Instr.localSet(statusLocal),
            ],
          ),
          Instr.loop(Instr.emptyBlockType, [
            Instr.localGet(statusLocal),
            Instr.i32Const(2),
            Instr.op('i32.eq'),
            Instr.ifElse(
              Instr.emptyBlockType,
              [
                ...resultLanes.map((lane) => zeroConst(laneValueType(program.layout, lane))),
                Instr.op('return'),
              ],
              [],
            ),
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
                    Instr.memoryAccess('i32.load', coroutineFrameMemory),
                    Instr.memoryAccess('i32.store', privateMemory),
                    Instr.localGet(headLocal),
                    Instr.globalSet(suspendResumeFrame),
                    Instr.localGet(headLocal),
                    Instr.memoryAccess('i32.load', coroutineFrameMemory, {
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
      if (machine === root && driver === rootDriver)
        yield* ExportActor.func(
          builder,
          symbolFor(machine.fn, Mir.machineEntry(program)),
          rootDriver,
        )
    }
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

const emitProgram = Effect.fnUntraced(function* (
  program: Mir.Module,
  request: Backend.CodegenRequest,
): Effect.fn.Return<EmittedProgram, Backend.BackendError> {
  return yield* emitProgramUnmapped(program, request).pipe(
    Effect.catchTag('WasmError', (cause) =>
      Effect.fail(
        new Backend.BackendError({
          operation: 'Backend.emit',
          backend: 'WebAssembly',
          message: `WebAssembly emission failed for ${program.module}`,
          reason: { _tag: 'WrappedFailure', cause },
        }),
      ),
    ),
  )
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
      const recordedEarlyReturns = new Set<string>()
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
          const earlyReturns =
            region._tag === 'OperationRegion'
              ? region.operations.flatMap(Mir.operationTree).flatMap((operation) => {
                  if (operation._tag !== 'PropagateEffectFailure') return []
                  const span = operation.provenance.span
                  const key = JSON.stringify([
                    region.id.ordinal,
                    span.sourceId,
                    span.start,
                    span.end,
                  ])
                  if (recordedEarlyReturns.has(key)) return []
                  recordedEarlyReturns.add(key)
                  return [
                    Object.freeze({
                      _tag: 'BackendControlProvenance' as const,
                      backend: 'WebAssembly' as const,
                      function: fn.id,
                      instance: fn.instance,
                      region: region.id,
                      construct: 'WasmReturn' as const,
                      targets: Object.freeze([]),
                      span,
                    }),
                  ]
                })
              : Object.freeze([])
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
          if (construct === undefined) return earlyReturns
          const target =
            outcome._tag === 'Repeat'
              ? loop?.id
              : outcome._tag === 'Exit' || outcome._tag === 'Yield'
                ? loop?.following
                : undefined
          return [
            ...earlyReturns,
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
 * contract as the LLVM backend actor, while its artifact kind selects a different finalizer.
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
    const output = yield* emitProgram(program, request)
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
        MirVerification.operations(fn).some((operation) => operation._tag === 'HostWrite'),
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
