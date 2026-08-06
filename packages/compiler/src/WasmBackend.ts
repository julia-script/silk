import * as Binary from '@silk-effect/wasm/Binary'
import * as Builder from '@silk-effect/wasm/Builder'
import * as ExportActor from '@silk-effect/wasm/Export'
import * as FuncActor from '@silk-effect/wasm/Func'
import * as Instr from '@silk-effect/wasm/Instr'
import * as WasmType from '@silk-effect/wasm/Type'
import * as ValType from '@silk-effect/wasm/ValType'
import * as WatText from '@silk-effect/wasm/WatText'
import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import { symbolFor } from './Backend.js'
import * as LayoutPlan from './Layout.js'
import * as Mir from './Mir.js'
import * as Target from './Target.js'

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

/**
 * The wasm local layout of one lowered function. MIR locals occupy the first slots — parameters
 * bind to the leading ones, exactly as in MIR — followed by the emission's own scratch slot.
 */
interface Layout {
  /** Holds a checked arithmetic operation's wrapped result while it is being verified. */
  readonly scratch: number
  /** Every local the definition must declare beyond the function's parameters. */
  readonly declared: ReadonlyArray<FuncActor.Local>
  /** Physical wasm locals realizing each logical MIR local's compiler-selected lanes. */
  readonly slots: ReadonlyArray<ReadonlyArray<number>>
  readonly lanes: ReadonlyArray<ReadonlyArray<LayoutPlan.CallingLane>>
}

/** Local names reach the `name` custom section, so release builds declare them unnamed. */
const layoutOf = (fn: Mir.MirFunction, plan: LayoutPlan.Plan, debug: boolean): Layout => {
  const named = (type: ValType.ValType, name: string): FuncActor.Local =>
    debug ? { type, name } : { type }
  const lanes = fn.localTypes.map((type) => {
    const shape = LayoutPlan.callingShape(plan, Mir.semanticType(type))
    if (shape === undefined) throw new RangeError('Wasm backend lost a logical calling shape')
    return shape.lanes
  })
  let physical = 0
  const slots = lanes.map((shape) => Object.freeze(shape.map(() => physical++)))
  const parameterLaneCount = lanes
    .slice(0, fn.parameterCount)
    .reduce((total, shape) => total + shape.length, 0)
  const declared: Array<FuncActor.Local> = []
  for (let ordinal = fn.parameterCount; ordinal < fn.localTypes.length; ordinal += 1) {
    const localSlots = slots.at(ordinal) ?? []
    for (const [lane] of localSlots.entries()) {
      declared.push(named(i32, `local${ordinal}_${lane}`))
    }
  }
  const scratch = physical
  declared.push(named(i32, 'scratch'))
  if (parameterLaneCount !== (slots.at(fn.parameterCount)?.at(0) ?? parameterLaneCount)) {
    throw new RangeError('Wasm physical parameter layout is not contiguous')
  }
  return {
    scratch,
    declared: Object.freeze(declared),
    slots: Object.freeze(slots),
    lanes: Object.freeze(lanes),
  }
}

/** Emits one MIR operation as a wasm instruction sequence writing its destination local. */
const emitOperation = (
  operation: Mir.Operation,
  layout: Layout,
  resolve: (target: Mir.Operation & { readonly _tag: 'Call' }) => FuncActor.Func,
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
  switch (operation._tag) {
    case 'Literal':
      return [Instr.i32Const(operation.value), Instr.localSet(scalar(operation.destination))]
    case 'Move':
      return copy(slots(operation.source), slots(operation.destination))
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
      if (operation.selectors.length === 0) {
        return copy(slots(operation.source), slots(operation.root))
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
      return instructions
    }
    case 'Drop':
      // MIR drops are ownership bookkeeping over `I32` locals, which own nothing to release.
      return []
    case 'Call':
      return [
        ...operation.arguments.flatMap((argument) =>
          slots(argument).map((slot) => Instr.localGet(slot)),
        ),
        Instr.call(resolve(operation)),
        ...[...slots(operation.destination)].reverse().map((slot) => Instr.localSet(slot)),
      ]
    case 'Binary': {
      const comparison = comparisons[operation.operator]
      if (comparison !== undefined) {
        return [
          Instr.localGet(scalar(operation.left)),
          Instr.localGet(scalar(operation.right)),
          Instr.op(comparison),
          Instr.localSet(scalar(operation.destination)),
        ]
      }
      const division = divisions[operation.operator]
      if (division !== undefined) {
        return [
          Instr.localGet(scalar(operation.left)),
          Instr.localGet(scalar(operation.right)),
          Instr.op(division),
          Instr.localSet(scalar(operation.destination)),
        ]
      }
      return [
        ...checkedArithmetic(
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
  resolve: (target: Mir.Operation & { readonly _tag: 'Call' }) => FuncActor.Func,
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
        ...operations.flatMap((operation) => emitOperation(operation, layout, resolve)),
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
      ...condition.operations.flatMap((operation) => emitOperation(operation, layout, resolve)),
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

  return [...emitRegion(fn.entry, Object.freeze([])), Instr.op('unreachable')]
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

    // Declare every function first so calls resolve regardless of definition order, mirroring the
    // LLVM backend's declare-then-define pass structure.
    const declared: Array<{
      readonly fn: Mir.MirFunction
      readonly symbol: string
      readonly handle: FuncActor.Func
    }> = []
    for (const [ordinal, fn] of program.functions.entries()) {
      const lanesFor = (type: Mir.Type): ReadonlyArray<LayoutPlan.CallingLane> => {
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

    const resolve = (operation: Mir.Operation & { readonly _tag: 'Call' }): FuncActor.Func => {
      const target = declared.find(
        (candidate) =>
          candidate.fn.id.module === operation.target.module &&
          candidate.fn.id.name === operation.target.name,
      )
      if (target === undefined) {
        throw new RangeError(`Backend cannot resolve call target ${operation.target.name}`)
      }
      return target.handle
    }

    for (const entry of declared) {
      const layout = layoutOf(entry.fn, program.layout, debug)
      // A body-less function is a declaration the frontend could not resolve; the LLVM backend
      // leaves it undefined, but wasm rejects an undefined function at emission, so it becomes a
      // trapping stub with the same observable behaviour.
      const body =
        entry.fn.regions.length === 0
          ? [Instr.op('unreachable')]
          : emitBody(entry.fn, layout, resolve)
      yield* FuncActor.define(builder, entry.handle, {
        locals: entry.fn.regions.length === 0 ? [] : layout.declared,
        body,
      })
      // Every function is exported so the artifact is directly instantiable for inspection.
      yield* ExportActor.func(builder, entry.symbol, entry.handle)
    }

    return {
      symbols: declared.map((entry) =>
        Object.freeze({ declaration: entry.fn.id, symbol: entry.symbol }),
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
