import * as Binary from '@silk-effect/wasm/Binary'
import * as Builder from '@silk-effect/wasm/Builder'
import * as ExportActor from '@silk-effect/wasm/Export'
import * as FuncActor from '@silk-effect/wasm/Func'
import * as Global from '@silk-effect/wasm/Global'
import * as Instr from '@silk-effect/wasm/Instr'
import * as Memory from '@silk-effect/wasm/Memory'
import * as WasmType from '@silk-effect/wasm/Type'
import * as ValType from '@silk-effect/wasm/ValType'
import * as WatText from '@silk-effect/wasm/WatText'
import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import { symbolFor } from './Backend.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as LayoutPlan from './Layout.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import type * as Ownership from './Ownership.js'
import * as Target from './Target.js'
import * as SilkType from './Type.js'

/** Tests whether one cleanup plan invokes a Drop hook at any nesting depth. */
const planHasHook = (plan: Ownership.CleanupPlan): boolean =>
  plan._tag === 'HookCleanup' ||
  (plan._tag === 'StructCleanup' && plan.fields.some((field) => planHasHook(field.cleanup))) ||
  (plan._tag === 'ArrayCleanup' && planHasHook(plan.element)) ||
  (plan._tag === 'UnionCleanup' && plan.cases.some((entry) => planHasHook(entry.cleanup))) ||
  (plan._tag === 'CallableCleanup' && plan.slots.some((slot) => planHasHook(slot.cleanup))) ||
  (plan._tag === 'RawBufferCleanup' && planHasHook(plan.allocation))

/**
 * A second `Backend` implementation emitting WebAssembly through the Silk wasm builder, for the
 * same MIR subset the bootstrap LLVM backend covers: logical scalar/aggregate locals, trapping
 * arithmetic, direct calls, checked replacement, and canonical structured control regions.
 *
 * The artifact reuses the nominal `Backend.Artifact` shape: `bitcode` carries the wasm binary and
 * `ir` carries the WAT inspection text, mirroring how the LLVM backend pairs bitcode with IR text.
 */

/**
 * MIR already publishes a backend-neutral structured DAG. This backend consumes its conditional
 * and loop regions directly: a loop becomes an exit `block` containing a repeat `loop`, and
 * lexical `Repeat`/`Exit` outcomes become exact `br` depths through the active label stack. It
 * never reconstructs source structure from a CFG and never introduces a dispatch loop.
 */

/** The wasm value type every MIR `I32` and `Bool` local lowers to. */
const i32 = ValType.i32

const alignUp = (value: number, alignment: number): number =>
  Math.ceil(value / alignment) * alignment

interface FrameRoot {
  readonly local: number
  readonly offset: number
  readonly type: Exclude<Mir.Type, { readonly _tag: 'EffectBorrow' | 'EffectOutcome' }>
}

interface FramePlan {
  readonly roots: ReadonlyMap<number, FrameRoot>
  readonly sliceRoots: ReadonlyMap<number, number>
  readonly size: number
  readonly alignment: number
}

const framePlan = (fn: Mir.MirFunction, plan: LayoutPlan.Plan): FramePlan => {
  const formations = Mir.operations(fn).filter(
    (operation): operation is Extract<Mir.Operation, { readonly _tag: 'BeginLoan' }> =>
      operation._tag === 'BeginLoan',
  )
  const rootOrdinals = new Set([
    ...formations.flatMap((operation) =>
      operation.sourceType._tag === 'Slice' ? [] : [operation.root.ordinal],
    ),
    ...Mir.operations(fn).flatMap((operation) =>
      operation._tag === 'MakeEffect' || operation._tag === 'MakeCallable'
        ? operation.captures.flatMap((capture) =>
            (capture.access === 'Shared' || capture.access === 'Exclusive') &&
            fn.localTypes.at(capture.source.ordinal)?._tag !== 'EffectBorrow'
              ? [capture.source.ordinal]
              : [],
          )
        : [],
    ),
    // A hook-bearing drop passes `&mut self` into its hook, so the owner needs frame storage.
    ...Mir.operations(fn).flatMap((operation) => {
      const dropped =
        operation._tag === 'Drop'
          ? [operation]
          : operation._tag === 'RunEffect' ||
              operation._tag === 'RunEffectValue' ||
              operation._tag === 'RetryEffect'
            ? (operation.releases ?? [])
            : []
      return dropped.flatMap((release) =>
        planHasHook(release.cleanup) ? [release.local.ordinal] : [],
      )
    }),
  ])
  const roots = new Map<number, FrameRoot>()
  let cursor = 0
  let alignment = 1
  for (const local of [...rootOrdinals].sort((left, right) => left - right)) {
    const type = fn.localTypes.at(local)
    const entry = type === undefined ? undefined : LayoutPlan.entry(plan, Mir.semanticType(type))
    if (
      type === undefined ||
      type._tag === 'EffectBorrow' ||
      type._tag === 'EffectOutcome' ||
      entry === undefined
    ) {
      throw new RangeError(`Wasm frame lost address-taken root %${local}`)
    }
    cursor = alignUp(cursor, entry.alignment)
    roots.set(local, Object.freeze({ local, offset: cursor, type }))
    cursor += entry.size
    alignment = Math.max(alignment, entry.alignment)
  }
  return Object.freeze({
    roots,
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
): ReadonlyArray<Instr.Instr> => {
  const wrapped: Instr.PlainMnemonic =
    shape === 'Add' ? 'i32.add' : shape === 'Subtract' ? 'i32.sub' : 'i32.mul'
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
          Instr.op('i32.eqz'),
          Instr.ifElse(
            Instr.valueBlockType(i32),
            [Instr.i32Const(0)],
            [
              Instr.localGet(left),
              Instr.i32Const(-2147483648),
              Instr.op('i32.eq'),
              Instr.localGet(right),
              Instr.i32Const(-1),
              Instr.op('i32.eq'),
              Instr.op('i32.and'),
              Instr.ifElse(
                Instr.valueBlockType(i32),
                [Instr.i32Const(1)],
                [
                  Instr.localGet(scratch),
                  Instr.localGet(right),
                  Instr.op('i32.div_s'),
                  Instr.localGet(left),
                  Instr.op('i32.ne'),
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
          Instr.op('i32.xor'),
          Instr.i32Const(0),
          ...(shape === 'Add' ? [Instr.op('i32.ge_s')] : [Instr.op('i32.lt_s')]),
          Instr.localGet(left),
          Instr.localGet(scratch),
          Instr.op('i32.xor'),
          Instr.i32Const(0),
          Instr.op('i32.lt_s'),
          Instr.op('i32.and'),
        ]

  return [
    ...compute,
    ...overflowed,
    Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
    Instr.localGet(scratch),
  ]
}

/** Emits checked unsigned i32 arithmetic for the target-word `Usize` lane. */
const checkedUnsignedArithmetic = (
  shape: OverflowShape,
  left: number,
  right: number,
  scratch: number,
): ReadonlyArray<Instr.Instr> => {
  const wrapped: Instr.PlainMnemonic =
    shape === 'Add' ? 'i32.add' : shape === 'Subtract' ? 'i32.sub' : 'i32.mul'
  const compute = [
    Instr.localGet(left),
    Instr.localGet(right),
    Instr.op(wrapped),
    Instr.localSet(scratch),
  ]
  const overflowed: ReadonlyArray<Instr.Instr> =
    shape === 'Add'
      ? [Instr.localGet(scratch), Instr.localGet(left), Instr.op('i32.lt_u')]
      : shape === 'Subtract'
        ? [Instr.localGet(left), Instr.localGet(right), Instr.op('i32.lt_u')]
        : [
            Instr.localGet(right),
            Instr.op('i32.eqz'),
            Instr.ifElse(
              Instr.valueBlockType(i32),
              [Instr.i32Const(0)],
              [
                Instr.localGet(scratch),
                Instr.localGet(right),
                Instr.op('i32.div_u'),
                Instr.localGet(left),
                Instr.op('i32.ne'),
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

/**
 * The wasm local layout of one lowered function. MIR locals occupy the first slots — parameters
 * bind to the leading ones, exactly as in MIR — followed by the emission's own scratch slot.
 */
interface Layout {
  /** Holds a checked arithmetic operation's wrapped result while it is being verified. */
  readonly scratch: number
  readonly frameBase?: number
  readonly frameEnd?: number
  readonly framePages?: number
  /** Every local the definition must declare beyond the function's parameters. */
  readonly declared: ReadonlyArray<FuncActor.Local>
  /** Physical wasm locals realizing each logical MIR local's compiler-selected lanes. */
  readonly slots: ReadonlyArray<ReadonlyArray<number>>
  readonly lanes: ReadonlyArray<ReadonlyArray<LayoutPlan.CallingLane>>
  /** Incoming address parameter for each capture represented internally by its loaded value lanes. */
  readonly borrowPointers: ReadonlyMap<number, number>
  readonly types: ReadonlyArray<Mir.Type>
}

interface MemoryContext {
  readonly memory: Memory.Memory
  readonly stackPointer: Global.Global
  readonly heapPointer: Global.Global
  readonly frame: FramePlan
  readonly plan: LayoutPlan.Plan
}

/** Local names reach the `name` custom section, so release builds declare them unnamed. */
const layoutOf = (
  fn: Mir.MirFunction,
  plan: LayoutPlan.Plan,
  frame: FramePlan,
  debug: boolean,
): Layout => {
  const named = (type: ValType.ValType, name: string): FuncActor.Local =>
    debug ? { type, name } : { type }
  const logicalLanes = (type: Mir.Type): ReadonlyArray<LayoutPlan.CallingLane> => {
    if (type._tag === 'EffectBorrow') {
      const shape = LayoutPlan.callingShape(plan, type.type)
      if (shape === undefined) throw new RangeError('Wasm backend lost a borrowed calling shape')
      return shape.lanes
    }
    if (type._tag === 'EffectValue')
      return LayoutPlan.effectEnvironmentLanes(plan, type.environment)
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
  let parameterPhysical = 0
  for (let ordinal = 0; ordinal < fn.parameterCount; ordinal += 1) {
    const type = fn.localTypes.at(ordinal)
    if (type === undefined) throw new RangeError(`Wasm backend lost parameter %${ordinal}`)
    if (type._tag === 'EffectBorrow') {
      borrowPointers.set(ordinal, parameterPhysical)
      parameterPhysical += 1
      slots.push(Object.freeze([]))
    } else {
      slots.push(Object.freeze(logicalLanes(type).map(() => parameterPhysical++)))
    }
  }
  let physical = parameterLaneCount
  const declared: Array<FuncActor.Local> = []
  for (const [ordinal] of borrowPointers) {
    const type = fn.localTypes.at(ordinal)
    if (type === undefined) throw new RangeError(`Wasm backend lost borrow %${ordinal}`)
    const localSlots = logicalLanes(type).map(() => physical++)
    slots[ordinal] = Object.freeze(localSlots)
    for (const [lane] of localSlots.entries()) declared.push(named(i32, `borrow${ordinal}_${lane}`))
  }
  for (let ordinal = fn.parameterCount; ordinal < fn.localTypes.length; ordinal += 1) {
    const type = fn.localTypes.at(ordinal)
    if (type === undefined) throw new RangeError(`Wasm backend lost local %${ordinal}`)
    const localSlots = logicalLanes(type).map(() => physical++)
    slots.push(Object.freeze(localSlots))
    for (const [lane] of localSlots.entries()) {
      declared.push(named(i32, `local${ordinal}_${lane}`))
    }
  }
  const scratch = physical
  declared.push(named(i32, 'scratch'))
  const frameBase = frame.roots.size === 0 ? undefined : physical + 1
  const frameEnd = frame.roots.size === 0 ? undefined : physical + 2
  const framePages = frame.roots.size === 0 ? undefined : physical + 3
  if (frameBase !== undefined && frameEnd !== undefined && framePages !== undefined) {
    declared.push(named(i32, 'frame_base'), named(i32, 'frame_end'), named(i32, 'frame_pages'))
  }
  return {
    scratch,
    declared: Object.freeze(declared),
    slots: Object.freeze(slots),
    lanes: Object.freeze(lanes),
    borrowPointers,
    types: fn.localTypes,
    ...(frameBase === undefined ? {} : { frameBase }),
    ...(frameEnd === undefined ? {} : { frameEnd }),
    ...(framePages === undefined ? {} : { framePages }),
  }
}

/** Emits one MIR operation as a wasm instruction sequence writing its destination local. */
const emitOperation = (
  operation: Mir.Operation,
  layout: Layout,
  plan: LayoutPlan.Plan,
  resolve: (
    target: DeclarationIndex.CanonicalId,
    typeArguments: ReadonlyArray<SilkType.Type>,
  ) => FuncActor.Func,
  memory: MemoryContext | undefined,
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
  const copy = (source: ReadonlyArray<number>, destination: ReadonlyArray<number>) => {
    if (source.length !== destination.length) {
      throw new RangeError('Wasm backend cannot copy mismatched logical lane bundles')
    }
    return source.flatMap((value, index) => {
      const target = destination.at(index)
      return target === undefined ? [] : [Instr.localGet(value), Instr.localSet(target)]
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
  const loadAt = (address: number, offset = 0): ReadonlyArray<Instr.Instr> => {
    const context = requireMemory()
    return [Instr.localGet(address), Instr.memoryAccess('i32.load', context.memory, { offset })]
  }
  const storeAt = (address: number, value: number, offset = 0): ReadonlyArray<Instr.Instr> => {
    const context = requireMemory()
    return [
      Instr.localGet(address),
      Instr.localGet(value),
      Instr.memoryAccess('i32.store', context.memory, { offset }),
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
        Instr.memoryAccess('i32.store', memory.memory),
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
        Instr.memoryAccess('i32.load', memory.memory),
        Instr.localSet(destination),
      ]
    })
  }
  /**
   * Runs every Drop hook one cleanup plan invokes: the owner materializes to its frame root,
   * each hook receives the address of its (possibly nested) value, and the owner's slots
   * reload afterward. Allocation and raw-buffer releases stay no-ops on the bump allocator.
   */
  const hookReleaseInstructions = (
    cleanup: Ownership.CleanupPlan,
    local: Mir.LocalId,
  ): ReadonlyArray<Instr.Instr> => {
    if (!planHasHook(cleanup)) return []
    if (memory === undefined) throw new RangeError('Wasm hook cleanup has no private memory')
    const planned = memory.frame.roots.get(local.ordinal)
    if (planned === undefined) throw new RangeError('Wasm hook cleanup lost its frame root')
    const walk = (
      plan_: Ownership.CleanupPlan,
      byteOffset: number,
    ): ReadonlyArray<Instr.Instr> => {
      switch (plan_._tag) {
        case 'HookCleanup':
          return [
            ...frameAddress(planned.offset + byteOffset),
            Instr.call(resolve(plan_.hook, plan_.typeArguments)),
            ...walk(plan_.inner, byteOffset),
          ]
        case 'StructCleanup': {
          const entry = LayoutPlan.entry(memory.plan, plan_.type)
          const representation = entry?.representation
          if (representation?._tag !== 'Aggregate') return []
          return plan_.fields.flatMap((field) => {
            if (!planHasHook(field.cleanup)) return []
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
          if (!planHasHook(plan_.element)) return []
          const entry = LayoutPlan.entry(memory.plan, plan_.type)
          const representation = entry?.representation
          if (representation?._tag !== 'Repeated') return []
          return Array.from({ length: plan_.length }, (_, index) =>
            walk(plan_.element, byteOffset + index * representation.stride),
          ).flat()
        }
        case 'UnionCleanup':
          if (plan_.cases.every((entry) => !planHasHook(entry.cleanup))) return []
          throw new RangeError('Wasm cleanup does not yet lower hook-bearing union cases')
        default:
          return []
      }
    }
    return [...materializeRoot(local), ...walk(cleanup, 0), ...reloadRoot(local.ordinal)]
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
      const offset = LayoutPlan.laneOffset(memory.plan, type.type, lane.path)
      const source = rootSlots.at(ordinal)
      if (offset === undefined || source === undefined)
        throw new RangeError(`Wasm Effect borrow lost lane ${ordinal}`)
      return [
        Instr.localGet(pointer),
        ...(offset === 0 ? [] : [Instr.i32Const(offset), Instr.op('i32.add')]),
        Instr.localGet(source),
        Instr.memoryAccess('i32.store', memory.memory),
      ]
    })
  }
  switch (operation._tag) {
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
      return [
        Instr.globalGet(context.heapPointer),
        Instr.localGet(alignment),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.op('i32.add'),
        Instr.i32Const(0),
        Instr.localGet(alignment),
        Instr.op('i32.sub'),
        Instr.op('i32.and'),
        Instr.localSet(base),
        Instr.localGet(base),
        Instr.localGet(bytes),
        Instr.op('i32.add'),
        Instr.localTee(layout.scratch),
        Instr.localGet(base),
        Instr.op('i32.lt_u'),
        Instr.ifElse(Instr.emptyBlockType, fail, []),
        Instr.localGet(layout.scratch),
        Instr.i32Const(1),
        Instr.op('i32.sub'),
        Instr.i32Const(16),
        Instr.op('i32.shr_u'),
        Instr.i32Const(1),
        Instr.op('i32.add'),
        Instr.memorySize(context.memory),
        Instr.op('i32.gt_u'),
        Instr.ifElse(
          Instr.emptyBlockType,
          [
            Instr.localGet(layout.scratch),
            Instr.i32Const(1),
            Instr.op('i32.sub'),
            Instr.i32Const(16),
            Instr.op('i32.shr_u'),
            Instr.i32Const(1),
            Instr.op('i32.add'),
            Instr.memorySize(context.memory),
            Instr.op('i32.sub'),
            Instr.memoryGrow(context.memory),
            Instr.i32Const(-1),
            Instr.op('i32.eq'),
            Instr.ifElse(Instr.emptyBlockType, fail, []),
          ],
          [],
        ),
        Instr.localGet(layout.scratch),
        Instr.globalSet(context.heapPointer),
        Instr.localGet(bytes),
        Instr.localSet(destinationBytes),
        Instr.localGet(alignment),
        Instr.localSet(destinationAlignment),
        Instr.i32Const(1),
        Instr.localSet(reclaim),
        Instr.i32Const(0),
        Instr.localSet(rawContext),
        Instr.i32Const(1),
        Instr.localSet(active),
      ]
    }
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
        return storeAt(address, value, offset)
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
        return [...loadAt(address, offset), Instr.localSet(destination)]
      })
    }
    case 'SlotDrop':
      return []
    case 'Match': {
      const emitMany = (operations: ReadonlyArray<Mir.Operation>): ReadonlyArray<Instr.Instr> =>
        operations.flatMap((nested) => emitOperation(nested, layout, plan, resolve, memory))
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
          ...copy(slots(arm.selected.result), slots(operation.destination)),
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
    case 'Literal':
      return [
        Instr.i32Const(
          operation.type._tag === 'Usize'
            ? Number(BigInt.asIntN(32, BigInt(operation.value)))
            : Number(operation.value),
        ),
        Instr.localSet(scalar(operation.destination)),
      ]
    case 'Move':
      return copy(slots(operation.source), slots(operation.destination))
    case 'BeginLoan':
      if (operation.sourceType._tag === 'Slice') {
        return copy(slots(operation.root), slots(operation.destination))
      } else {
        const planned = memory?.frame.roots.get(operation.root.ordinal)
        const [address, length] = slots(operation.destination)
        if (planned === undefined || address === undefined) {
          throw new RangeError('Wasm borrow formation lost its frame root or address lane')
        }
        if (operation.type._tag === 'Reference') {
          return [
            ...materializeRoot(operation.root),
            ...frameAddress(planned.offset),
            Instr.localSet(address),
          ]
        }
        if (length === undefined || operation.sourceType._tag !== 'FixedArray') {
          throw new RangeError('Wasm slice formation lost its length lane or array root')
        }
        return [
          ...materializeRoot(operation.root),
          ...frameAddress(planned.offset),
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
            ...(value === undefined ? [Instr.i32Const(0)] : [Instr.localGet(value)]),
            Instr.localSet(target),
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
          ...(value === undefined ? [Instr.i32Const(0)] : [Instr.localGet(value)]),
          Instr.localSet(target),
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
      if (rootType?._tag === 'Reference') {
        // The place lives on the referenced target: static field offsets off the address.
        const address = scalar(operation.root)
        const target = rootType.type.target
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
          return [...loadAt(address, offset), Instr.localSet(destination)]
        })
      }
      if (rootType?._tag === 'Slice') {
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
        const sliceLayout = LayoutPlan.entry(memory.plan, rootType.type)
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
            rootType.type.element,
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
            Instr.memoryAccess('i32.load', memory.memory),
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
          return storeAt(address, value, offset)
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
            Instr.memoryAccess('i32.store', memory.memory),
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
      // The bump allocator owns no host resources, so only Drop hooks have a Wasm effect.
      return hookReleaseInstructions(operation.cleanup, operation.local)
    case 'MakeEffect':
    case 'MakeCallable': {
      const destination = slots(operation.destination)
      const instructions: Array<Instr.Instr> = []
      let cursor = 0
      for (const capture of operation.captures) {
        if (capture.access === 'Copy' || capture.access === 'Take') {
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
    case 'UnpackEffectSuccess':
      return copy(
        slots(operation.source).slice(1, 1 + slots(operation.destination).length),
        slots(operation.destination),
      )
    case 'CatchEffect': {
      const protectedValueSlots = slots(operation.protectedValue)
      const protectedSlots = slots(operation.protectedOutcome)
      const handlerValueSlots = slots(operation.handlerValue)
      const handlerSlots = slots(operation.handlerOutcome)
      const destinationSlots = slots(operation.destination)
      const tag = protectedSlots.at(0)
      if (tag === undefined) throw new RangeError('Wasm effect catch lost its tag lane')
      const success = copy(protectedSlots.slice(1, 1 + destinationSlots.length), destinationSlots)
      const failure = [
        Instr.localGet(tag),
        Instr.i32Const(operation.handledTag),
        Instr.op('i32.eq'),
        Instr.ifElse(Instr.emptyBlockType, [], [Instr.op('unreachable')]),
        ...protectedSlots
          .slice(1, 1 + operation.handledLaneCount)
          .map((slot) => Instr.localGet(slot)),
        ...slots(operation.handlerCallable).map((slot) => Instr.localGet(slot)),
        Instr.call(resolve(operation.handlerTarget, operation.handlerTypeArguments)),
        ...[...handlerValueSlots].reverse().map((slot) => Instr.localSet(slot)),
        ...handlerValueSlots.map((slot) => Instr.localGet(slot)),
        Instr.call(resolve(operation.handlerRunner, operation.handlerTypeArguments)),
        ...[...handlerSlots].reverse().map((slot) => Instr.localSet(slot)),
        ...copy(handlerSlots.slice(1, 1 + destinationSlots.length), destinationSlots),
      ]
      return [
        ...(operation.protectedTarget === undefined
          ? []
          : [
              ...operation.protectedArguments.flatMap((argument) =>
                slots(argument).map((slot) => Instr.localGet(slot)),
              ),
              Instr.call(resolve(operation.protectedTarget, operation.protectedTypeArguments)),
              ...[...protectedValueSlots].reverse().map((slot) => Instr.localSet(slot)),
            ]),
        ...protectedValueSlots.map((slot) => Instr.localGet(slot)),
        ...operation.protectedRunnerArguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        Instr.call(resolve(operation.protectedRunner, operation.protectedTypeArguments)),
        ...[...protectedSlots].reverse().map((slot) => Instr.localSet(slot)),
        Instr.localGet(tag),
        Instr.op('i32.eqz'),
        Instr.ifElse(Instr.emptyBlockType, success, failure),
      ]
    }
    case 'RetryEffect': {
      const valueSlots = slots(operation.protectedValue)
      const outcomeSlots = slots(operation.protectedOutcome)
      const destinationSlots = slots(operation.destination)
      const tag = outcomeSlots.at(0)
      if (tag === undefined) throw new RangeError('Wasm Effect retry lost its outcome tag')
      const invokeRunner = [
        ...valueSlots.map((slot) => Instr.localGet(slot)),
        Instr.call(resolve(operation.protectedRunner, operation.protectedTypeArguments)),
        ...[...outcomeSlots].reverse().map((slot) => Instr.localSet(slot)),
      ]
      const loop = Instr.block(Instr.emptyBlockType, [
        Instr.loop(Instr.emptyBlockType, [
          ...invokeRunner,
          Instr.localGet(tag),
          Instr.op('i32.eqz'),
          Instr.brIf(1),
          Instr.localGet(layout.scratch),
          Instr.i32Const(0),
          Instr.op('i32.le_s'),
          Instr.brIf(1),
          Instr.localGet(layout.scratch),
          Instr.i32Const(1),
          Instr.op('i32.sub'),
          Instr.localSet(layout.scratch),
          Instr.br(0),
        ]),
      ])
      const success = [
        ...copy(outcomeSlots.slice(1, 1 + destinationSlots.length), destinationSlots),
        ...[...(memory?.frame.roots.keys() ?? [])]
          .sort((left, right) => left - right)
          .flatMap(reloadRoot),
      ]
      const failure =
        operation.propagationType === undefined
          ? [Instr.op('unreachable')]
          : [
              // Owners still live at this site run their Drop hooks before the failure leaves.
              ...(operation.releases ?? []).flatMap((release) =>
                hookReleaseInstructions(release.cleanup, release.local),
              ),
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
              Instr.localGet(layout.scratch),
              ...Array.from({ length: operation.propagationLaneCount - 1 }, (_, index) => {
                const source = outcomeSlots.at(index + 1)
                return source === undefined ? Instr.i32Const(0) : Instr.localGet(source)
              }),
              Instr.op('return'),
            ]
      return [
        ...(operation.protectedTarget === undefined
          ? []
          : [
              ...operation.protectedArguments.flatMap((argument) =>
                slots(argument).map((slot) => Instr.localGet(slot)),
              ),
              Instr.call(resolve(operation.protectedTarget, operation.protectedTypeArguments)),
              ...[...valueSlots].reverse().map((slot) => Instr.localSet(slot)),
            ]),
        Instr.localGet(scalar(operation.retries)),
        Instr.localSet(layout.scratch),
        loop,
        Instr.localGet(tag),
        Instr.op('i32.eqz'),
        Instr.ifElse(Instr.emptyBlockType, success, failure),
      ]
    }
    case 'RunEffect': {
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
        // Owners still live at this site run their Drop hooks before the failure leaves.
        ...(operation.releases ?? []).flatMap((release) =>
          hookReleaseInstructions(release.cleanup, release.local),
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
        ...operation.arguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        Instr.call(resolve(operation.target, operation.typeArguments)),
        ...[...outcomeSlots].reverse().map((slot) => Instr.localSet(slot)),
        Instr.localGet(tag),
        Instr.op('i32.eqz'),
        Instr.ifElse(
          Instr.emptyBlockType,
          copy(outcomeSlots.slice(1, 1 + destinationSlots.length), destinationSlots),
          failure,
        ),
      ]
    }
    case 'RunEffectValue': {
      const outcomeSlots = slots(operation.outcome)
      const destinationSlots = slots(operation.destination)
      const tag = outcomeSlots.at(0)
      if (tag === undefined) throw new RangeError('Wasm Effect value lost its outcome tag lane')
      const invoke = [
        ...slots(operation.effect).map((slot) => Instr.localGet(slot)),
        ...operation.arguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        Instr.call(resolve(operation.runner, operation.runnerTypeArguments)),
        ...[...outcomeSlots].reverse().map((slot) => Instr.localSet(slot)),
        ...[...(memory?.frame.roots.keys() ?? [])]
          .sort((left, right) => left - right)
          .flatMap(reloadRoot),
      ]
      const success = copy(outcomeSlots.slice(1, 1 + destinationSlots.length), destinationSlots)
      if (operation.propagationType === undefined) return [...invoke, ...success]
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
        // Owners still live at this site run their Drop hooks before the failure leaves.
        ...(operation.releases ?? []).flatMap((release) =>
          hookReleaseInstructions(release.cleanup, release.local),
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
        ...invoke,
        Instr.localGet(tag),
        Instr.op('i32.eqz'),
        Instr.ifElse(Instr.emptyBlockType, success, failure),
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
        if (sourceType?._tag !== 'CallableValue' || sourceType.environment === undefined)
          throw new RangeError('Wasm stored callable lost its environment')
        const environmentSlots = slots(operation.callable)
        let cursor = 0
        for (const field of sourceType.environment.fields) {
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
              Instr.memoryAccess('i32.load', memory.memory),
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
        if (target.operation === 'Not') {
          return [
            Instr.localGet(left),
            Instr.op('i32.eqz'),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        if (target.operation === 'Negate') {
          return [
            Instr.localGet(left),
            Instr.i32Const(-2147483648),
            Instr.op('i32.eq'),
            Instr.ifElse(Instr.emptyBlockType, [Instr.op('unreachable')], []),
            Instr.i32Const(0),
            Instr.localGet(left),
            Instr.op('i32.sub'),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        const right = operandSlots.at(1)
        if (
          right === undefined ||
          target.operation === 'LayoutMake' ||
          target.operation === 'LayoutRepeat' ||
          target.operation === 'SystemAllocatorMake' ||
          target.operation === 'AllocatorAllocate' ||
          !Mir.isBinaryOperator(target.operation)
        ) {
          throw new RangeError(
            `Wasm callable builtin ${target.actor}.${target.operation} is unavailable`,
          )
        }
        const firstType = operation.arguments.at(0)
        const unsigned =
          firstType !== undefined && layout.types.at(firstType.ordinal)?._tag === 'Usize'
        const comparison = (unsigned ? unsignedComparisons : comparisons)[target.operation]
        if (comparison !== undefined) {
          return [
            Instr.localGet(left),
            Instr.localGet(right),
            Instr.op(comparison),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        const division = (unsigned ? unsignedDivisions : divisions)[target.operation]
        if (division !== undefined) {
          return [
            Instr.localGet(left),
            Instr.localGet(right),
            Instr.op(division),
            Instr.localSet(scalar(operation.destination)),
          ]
        }
        return [
          ...(unsigned ? checkedUnsignedArithmetic : checkedArithmetic)(
            target.operation === 'Add'
              ? 'Add'
              : target.operation === 'Subtract'
                ? 'Subtract'
                : 'Multiply',
            left,
            right,
            layout.scratch,
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
      ]
    }
    case 'Binary': {
      const leftType = layout.types.at(operation.left.ordinal)
      const unsigned = leftType?._tag === 'Usize'
      const comparison = (unsigned ? unsignedComparisons : comparisons)[operation.operator]
      if (comparison !== undefined) {
        return [
          Instr.localGet(scalar(operation.left)),
          Instr.localGet(scalar(operation.right)),
          Instr.op(comparison),
          Instr.localSet(scalar(operation.destination)),
        ]
      }
      const division = (unsigned ? unsignedDivisions : divisions)[operation.operator]
      if (division !== undefined) {
        return [
          Instr.localGet(scalar(operation.left)),
          Instr.localGet(scalar(operation.right)),
          Instr.op(division),
          Instr.localSet(scalar(operation.destination)),
        ]
      }
      return [
        ...(unsigned ? checkedUnsignedArithmetic : checkedArithmetic)(
          operation.operator === 'Add'
            ? 'Add'
            : operation.operator === 'Subtract'
              ? 'Subtract'
              : 'Multiply',
          scalar(operation.left),
          scalar(operation.right),
          layout.scratch,
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
  layout: Layout,
  plan: LayoutPlan.Plan,
  resolve: (
    target: DeclarationIndex.CanonicalId,
    typeArguments: ReadonlyArray<SilkType.Type>,
  ) => FuncActor.Func,
  memory: MemoryContext | undefined,
): ReadonlyArray<Instr.Instr> => {
  const regions = new Map(fn.regions.map((region) => [region.id.ordinal, region] as const))
  const scalar = (local: Mir.LocalId): number => {
    const slots = layout.slots.at(local.ordinal) ?? []
    const first = slots.at(0)
    if (slots.length !== 1 || first === undefined) {
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
          Instr.memoryAccess('i32.load', memory.memory),
          Instr.localSet(destination),
        ]
      })
    })
  }

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
          emitOperation(operation, layout, plan, resolve, memory),
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
        emitOperation(operation, layout, plan, resolve, memory),
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

  return [
    ...reserveFrame(),
    ...loadBorrowedParameters(),
    ...emitRegion(fn.entry, Object.freeze([])),
    Instr.op('unreachable'),
  ]
}

const emitProgram = (program: Mir.Module, request: Backend.CodegenRequest) =>
  Effect.gen(function* () {
    const i32Layout = LayoutPlan.entry(program.layout, 'I32')
    if (i32Layout === undefined) {
      return yield* new Backend.BackendError({
        operation: 'Backend.emit',
        backend: 'WebAssembly',
        message: 'WebAssembly requires the planned I32 representation',
        reason: { _tag: 'InvalidMir', violations: Mir.verify(program) },
      })
    }
    // WebAssembly realizes both canonical scalar entries as its four-byte i32 value type.
    if (
      program.layout.entries.some(
        (entry) =>
          (entry.representation._tag === 'SignedInteger' ||
            entry.representation._tag === 'Boolean') &&
          entry.representation.bits !== 32,
      )
    ) {
      return yield* new Backend.BackendError({
        operation: 'Backend.emit',
        backend: 'WebAssembly',
        message: 'WebAssembly requires the canonical 32-bit I32 representation',
        reason: { _tag: 'InvalidMir', violations: Mir.verify(program) },
      })
    }
    // WebAssembly's debug-information equivalent of the LLVM backend's native debug metadata is
    // the `name` custom section, which the builder emits from the names given here. Debug builds
    // name the module, its functions, and their locals; release builds omit every name, which is
    // what the LLVM backend's `strip` flag does with its own metadata.
    const debug = request.mode === 'debug'
    const builder = yield* Builder.make(debug ? { moduleName: program.module } : {})
    const frames = new Map(
      program.functions.map((fn) => [fn, framePlan(fn, program.layout)] as const),
    )
    const needsHeap = program.functions.some((fn) =>
      Mir.operations(fn).some(
        (operation) =>
          operation._tag === 'Allocate' ||
          operation._tag === 'RawBufferFrom' ||
          operation._tag === 'RawBufferCount' ||
          operation._tag === 'RawBufferSlot' ||
          operation._tag === 'SlotWrite' ||
          operation._tag === 'SlotTake' ||
          operation._tag === 'SlotCopy' ||
          operation._tag === 'SlotDrop',
      ),
    )
    const needsMemory = needsHeap || [...frames.values()].some((frame) => frame.roots.size > 0)
    const privateMemory = needsMemory
      ? yield* Memory.make(
          builder,
          { min: needsHeap ? 2 : 1, max: 65536 },
          debug ? { name: 'silk_memory' } : {},
        )
      : undefined
    const stackPointer = needsMemory
      ? yield* Global.make(
          builder,
          i32,
          true,
          [Instr.i32Const(16)],
          debug ? { name: 'silk_stack_pointer' } : {},
        )
      : undefined
    const heapPointer = needsMemory
      ? yield* Global.make(
          builder,
          i32,
          true,
          [Instr.i32Const(65536)],
          debug ? { name: 'silk_heap_pointer' } : {},
        )
      : undefined

    // Declare every function first so calls resolve regardless of definition order, mirroring the
    // LLVM backend's declare-then-define pass structure.
    const declared: Array<{
      readonly fn: Mir.MirFunction
      readonly symbol: string
      readonly handle: FuncActor.Func
    }> = []
    for (const [ordinal, fn] of program.functions.entries()) {
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
      const signature = yield* WasmType.func(
        builder,
        fn.regions.length === 0
          ? []
          : fn.localTypes
              .slice(0, fn.parameterCount)
              .flatMap((type) => lanesFor(type).map(() => i32)),
        lanesFor(fn.result).map(() => i32),
      )
      const symbol = symbolFor(fn, ordinal)
      declared.push({
        fn,
        symbol,
        // The export name carries the symbol regardless, so a release module is still callable
        // by name even with the name section stripped.
        handle: yield* FuncActor.declare(builder, signature, debug ? { name: symbol } : {}),
      })
    }

    const resolve = (
      targetId: DeclarationIndex.CanonicalId,
      typeArguments: ReadonlyArray<SilkType.Type>,
    ): FuncActor.Func => {
      const target = declared.find((candidate) =>
        Mir.matchesInstance(candidate.fn, targetId, typeArguments),
      )
      if (target === undefined) {
        throw new RangeError(`Backend cannot resolve call target ${targetId.name}`)
      }
      return target.handle
    }

    for (const entry of declared) {
      const frame = frames.get(entry.fn)
      if (frame === undefined) throw new RangeError('Wasm declaration lost its frame plan')
      const layout = layoutOf(entry.fn, program.layout, frame, debug)
      const memory: MemoryContext | undefined =
        privateMemory === undefined || stackPointer === undefined || heapPointer === undefined
          ? undefined
          : Object.freeze({
              memory: privateMemory,
              stackPointer,
              heapPointer,
              frame,
              plan: program.layout,
            })
      // A body-less function is a declaration the frontend could not resolve; the LLVM backend
      // leaves it undefined, but wasm rejects an undefined function at emission, so it becomes a
      // trapping stub with the same observable behaviour.
      const body =
        entry.fn.regions.length === 0
          ? [Instr.op('unreachable')]
          : emitBody(entry.fn, layout, program.layout, resolve, memory)
      yield* FuncActor.define(builder, entry.handle, {
        locals: entry.fn.regions.length === 0 ? [] : layout.declared,
        body,
      })
      // Every function is exported so the artifact is directly instantiable for inspection.
      yield* ExportActor.func(builder, entry.symbol, entry.handle)
    }

    return {
      symbols: declared.map((entry) =>
        Object.freeze({
          declaration: entry.fn.id,
          instance: entry.fn.instance,
          symbol: entry.symbol,
        }),
      ),
      ir: yield* WatText.render(builder),
      bitcode: yield* Binary.encode(builder),
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
 * contract as `Backend.LlvmBackend`, so `Analysis.codegen` and the driver accept it in place of
 * the LLVM backend with no other change.
 */
export const WasmBackend: Backend.Backend = Object.freeze({
  _tag: 'Backend',
  name: 'WebAssembly',
  targets: Object.freeze([Target.wasm32UnknownUnknown.id]),
  emit: Effect.fn('Backend.WebAssembly.emit')(function* (
    program: Mir.Module,
    request: Backend.CodegenRequest,
  ): Effect.fn.Return<Backend.Artifact, Backend.BackendError> {
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
      _tag: 'BackendArtifact',
      module: program.module,
      target: program.layout.target,
      symbols: Object.freeze(output.symbols),
      control: controlProvenance(program),
      bitcode: output.bitcode,
      ir: output.ir,
    })
  }),
})
