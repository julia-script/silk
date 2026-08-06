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
 * same MIR subset the bootstrap LLVM backend covers: `I32` locals, trapping arithmetic,
 * non-trapping comparisons, direct calls, and arbitrary basic-block control flow.
 *
 * The artifact reuses the nominal `Backend.Artifact` shape: `bitcode` carries the wasm binary and
 * `ir` carries the WAT inspection text, mirroring how the LLVM backend pairs bitcode with IR text.
 */

/**
 * MIR keeps the structure the source had: lowering builds branch diamonds from `if` statements
 * with explicit join blocks, and the language has no loops, so a function's CFG is a DAG whose
 * edges always run forward in block order. That shape is a property of MIR every backend may
 * rely on — MIR is backend-neutral, not structureless. Consuming a DAG is each backend's own
 * job, done the way its target demands: the LLVM backend emits the blocks as-is because LLVM
 * takes an arbitrary CFG, and this backend recovers `if`/`else` because WebAssembly requires
 * structured control flow. Neither shape belongs in MIR, which is why the translation lives
 * here rather than in lowering.
 *
 * A `Branch` becomes a wasm `if`/`else` whose arms are the sub-CFGs reachable from each
 * successor before they reconverge; the reconvergence point — the join block — is emitted after
 * the `if` closes, which is exactly where lowering put it. `Jump` needs no instruction at all
 * when its target is that join, since emission continues there anyway.
 *
 * ponytail: assumes the forward-only DAG lowering produces today, asserted per function before
 * emission. Loops would add back-edges, at which point this backend needs a loop construct and
 * `requireForwardDag` fails loudly instead of emitting something subtly wrong.
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
    case 'Construct':
      return copy(
        operation.fields.flatMap((field) => [...slots(field.value)]),
        slots(operation.destination),
      )
    case 'Project': {
      const sourceLanes = layout.lanes.at(operation.source.ordinal) ?? []
      const sourceSlots = slots(operation.source)
      const projected = sourceLanes.flatMap((lane, index) => {
        const field = lane.path.at(0)
        const source = sourceSlots.at(index)
        return field !== undefined &&
          source !== undefined &&
          field.ordinal === operation.field.ordinal &&
          field.struct.sourceId === operation.field.struct.sourceId &&
          field.struct.ordinal === operation.field.struct.ordinal
          ? [source]
          : []
      })
      return copy(projected, slots(operation.destination))
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
          operation.operator as OverflowShape,
          scalar(operation.left),
          scalar(operation.right),
          layout.scratch,
        ),
        Instr.localSet(scalar(operation.destination)),
      ]
    }
  }
}

/** The successor blocks one terminator can transfer control to, in emission order. */
const successors = (terminator: Mir.Terminator): ReadonlyArray<number> => {
  switch (terminator._tag) {
    case 'Jump':
      return [terminator.target.ordinal]
    case 'Branch':
      return [terminator.taken.ordinal, terminator.otherwise.ordinal]
    default:
      return []
  }
}

/**
 * Rejects any CFG this backend's structured emission does not model: an edge that runs backward
 * or sideways means the language grew loops or arbitrary jumps, and the `if`/`else` recovery
 * below would silently emit the wrong control flow rather than fail.
 */
const forwardDagViolation = (fn: Mir.MirFunction): string | undefined => {
  for (const block of fn.blocks) {
    for (const target of successors(block.terminator)) {
      if (target <= block.id.ordinal) {
        return `WasmBackend requires a forward-only CFG, but ${fn.id.name} jumps from bb${block.id.ordinal} to bb${target}`
      }
    }
  }
  return undefined
}

/**
 * Emits the blocks of one function from `entry` up to but excluding `stop`, as one straight-line
 * instruction sequence with branch diamonds recovered as `if`/`else`.
 *
 * `stop` is the block at which the caller will continue emitting — for a branch's arms it is the
 * join block, so each arm covers exactly the blocks between its successor and the reconvergence
 * point. Because lowering emits a diamond's arms before its join and never reuses a block across
 * two diamonds, the join is simply the first block both arms can reach.
 */
const emitRange = (
  fn: Mir.MirFunction,
  entry: number,
  stop: number,
  layout: Layout,
  resolve: (target: Mir.Operation & { readonly _tag: 'Call' }) => FuncActor.Func,
): ReadonlyArray<Instr.Instr> => {
  const instructions: Array<Instr.Instr> = []
  let ordinal = entry

  while (ordinal < stop) {
    const block = fn.blocks[ordinal]
    if (block === undefined) throw new RangeError(`WasmBackend reached missing block bb${ordinal}`)
    for (const operation of block.operations) {
      instructions.push(...emitOperation(operation, layout, resolve))
    }

    const terminator = block.terminator
    switch (terminator._tag) {
      case 'Return':
        instructions.push(
          ...(layout.slots.at(terminator.value.ordinal) ?? []).map((slot) => Instr.localGet(slot)),
          Instr.op('return'),
        )
        return instructions
      case 'Trap':
        instructions.push(Instr.op('unreachable'))
        return instructions
      case 'Jump':
        // Emission continues at the target, so a jump to the very next block — which is what
        // lowering's join arms produce — costs no instruction at all.
        ordinal = terminator.target.ordinal
        break
      case 'Branch': {
        const taken = terminator.taken.ordinal
        const otherwise = terminator.otherwise.ordinal
        // The arms reconverge at the join, or — when an arm returns outright — the diamond simply
        // ends and emission continues at whichever successor follows it.
        const join = joinOf(fn, taken, otherwise, stop)
        instructions.push(
          Instr.localGet(
            layout.slots.at(terminator.condition.ordinal)?.at(0) ??
              (() => {
                throw new RangeError('Wasm branch condition lost its scalar lane')
              })(),
          ),
          Instr.ifElse(
            Instr.emptyBlockType,
            emitRange(fn, taken, Math.min(join, stop), layout, resolve),
            emitRange(fn, otherwise, Math.min(join, stop), layout, resolve),
          ),
        )
        if (join >= stop) return instructions
        ordinal = join
        break
      }
    }
  }

  return instructions
}

/**
 * The block at which a branch's two arms reconverge, or `bound` when they never do because both
 * arms leave the function. Each arm is followed forward through the blocks it falls into; the
 * first block reachable from both is the join lowering created for the diamond.
 */
const joinOf = (fn: Mir.MirFunction, taken: number, otherwise: number, bound: number): number => {
  const reachable = (from: number): ReadonlySet<number> => {
    const seen = new Set<number>()
    const pending = [from]
    while (pending.length > 0) {
      const ordinal = pending.pop()
      if (ordinal === undefined || ordinal >= bound || seen.has(ordinal)) continue
      seen.add(ordinal)
      const block = fn.blocks[ordinal]
      if (block !== undefined) pending.push(...successors(block.terminator))
    }
    return seen
  }

  const fromTaken = reachable(taken)
  const fromOtherwise = reachable(otherwise)
  // Forward-only edges make the lowest shared ordinal the reconvergence point.
  for (let ordinal = Math.max(taken, otherwise); ordinal < bound; ordinal += 1) {
    if (fromTaken.has(ordinal) && fromOtherwise.has(ordinal)) return ordinal
  }
  return bound
}

/**
 * Builds one function body. Every path through a MIR function ends in a return or a trap, so the
 * emitted body needs no trailing fallthrough value — but a diamond whose arms both return leaves
 * wasm's validator unable to see that, so an `unreachable` closes the body.
 */
const emitBody = (
  fn: Mir.MirFunction,
  layout: Layout,
  resolve: (target: Mir.Operation & { readonly _tag: 'Call' }) => FuncActor.Func,
): ReadonlyArray<Instr.Instr> => {
  return [...emitRange(fn, 0, fn.blocks.length, layout, resolve), Instr.op('unreachable')]
}

const emitProgram = (program: Mir.Module, request: Backend.CodegenRequest) =>
  Effect.gen(function* () {
    for (const fn of program.functions) {
      const violation = forwardDagViolation(fn)
      if (violation !== undefined) {
        return yield* new Backend.BackendError({
          operation: 'Backend.emit',
          backend: 'WebAssembly',
          message: violation,
          reason: { _tag: 'UnsupportedMir', detail: violation },
        })
      }
    }
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
        (entry) => entry.representation._tag !== 'Aggregate' && entry.representation.bits !== 32,
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
        fn.blocks.length === 0
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
        entry.fn.blocks.length === 0
          ? [Instr.op('unreachable')]
          : emitBody(entry.fn, layout, resolve)
      yield* FuncActor.define(builder, entry.handle, {
        locals: entry.fn.blocks.length === 0 ? [] : layout.declared,
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
      bitcode: output.bitcode,
      ir: output.ir,
    })
  }),
})
