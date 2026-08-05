import * as Binary from '@silk-effect/wasm/Binary'
import * as Builder from '@silk-effect/wasm/Builder'
import * as ExportActor from '@silk-effect/wasm/Export'
import * as FuncActor from '@silk-effect/wasm/Func'
import * as Instr from '@silk-effect/wasm/Instr'
import * as WasmType from '@silk-effect/wasm/Type'
import * as ValType from '@silk-effect/wasm/ValType'
import * as WatText from '@silk-effect/wasm/WatText'
import * as Effect from 'effect/Effect'
import type * as Backend from './Backend.js'
import { symbolFor } from './Backend.js'
import type * as Mir from './Mir.js'

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
}

/** Local names reach the `name` custom section, so release builds declare them unnamed. */
const layoutOf = (fn: Mir.MirFunction, debug: boolean): Layout => {
  const named = (type: ValType.ValType, name: string): FuncActor.Local =>
    debug ? { type, name } : { type }
  const declared = [
    ...fn.localTypes
      .slice(fn.parameterCount)
      .map((_, index) => named(i32, `local${fn.parameterCount + index}`)),
    named(i32, 'scratch'),
  ]
  return { scratch: fn.localTypes.length, declared: Object.freeze(declared) }
}

/** Emits one MIR operation as a wasm instruction sequence writing its destination local. */
const emitOperation = (
  operation: Mir.Operation,
  layout: Layout,
  resolve: (target: Mir.Operation & { readonly _tag: 'Call' }) => FuncActor.Func,
): ReadonlyArray<Instr.Instr> => {
  switch (operation._tag) {
    case 'Literal':
      return [Instr.i32Const(operation.value), Instr.localSet(operation.destination.ordinal)]
    case 'Move':
      return [
        Instr.localGet(operation.source.ordinal),
        Instr.localSet(operation.destination.ordinal),
      ]
    case 'Drop':
      // MIR drops are ownership bookkeeping over `I32` locals, which own nothing to release.
      return []
    case 'Call':
      return [
        ...operation.arguments.map((argument) => Instr.localGet(argument.ordinal)),
        Instr.call(resolve(operation)),
        Instr.localSet(operation.destination.ordinal),
      ]
    case 'Binary': {
      const comparison = comparisons[operation.operator]
      if (comparison !== undefined) {
        return [
          Instr.localGet(operation.left.ordinal),
          Instr.localGet(operation.right.ordinal),
          Instr.op(comparison),
          Instr.localSet(operation.destination.ordinal),
        ]
      }
      const division = divisions[operation.operator]
      if (division !== undefined) {
        return [
          Instr.localGet(operation.left.ordinal),
          Instr.localGet(operation.right.ordinal),
          Instr.op(division),
          Instr.localSet(operation.destination.ordinal),
        ]
      }
      return [
        ...checkedArithmetic(
          operation.operator as OverflowShape,
          operation.left.ordinal,
          operation.right.ordinal,
          layout.scratch,
        ),
        Instr.localSet(operation.destination.ordinal),
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
const requireForwardDag = (fn: Mir.MirFunction): void => {
  for (const block of fn.blocks) {
    for (const target of successors(block.terminator)) {
      if (target <= block.id.ordinal) {
        throw new RangeError(
          `WasmBackend requires a forward-only CFG, but ${fn.id.name} jumps from bb${block.id.ordinal} to bb${target}`,
        )
      }
    }
  }
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
        instructions.push(Instr.localGet(terminator.value.ordinal), Instr.op('return'))
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
          Instr.localGet(terminator.condition.ordinal),
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
  requireForwardDag(fn)
  return [...emitRange(fn, 0, fn.blocks.length, layout, resolve), Instr.op('unreachable')]
}

const emitProgram = (
  program: Mir.Module,
  _layout: Mir.TargetLayout,
  request: Backend.CodegenRequest,
) =>
  Effect.gen(function* () {
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
      const signature = yield* WasmType.func(
        builder,
        fn.blocks.length === 0 ? [] : Array.from({ length: fn.parameterCount }, () => i32),
        [i32],
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
      const layout = layoutOf(entry.fn, debug)
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
  emit: (
    program: Mir.Module,
    layout: Mir.TargetLayout,
    request: Backend.CodegenRequest,
  ): Backend.Artifact => {
    const output = Effect.runSync(emitProgram(program, layout, request))
    return Object.freeze({
      _tag: 'BackendArtifact',
      module: program.module,
      symbols: Object.freeze(output.symbols),
      bitcode: output.bitcode,
      ir: output.ir,
    })
  },
})
