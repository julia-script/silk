import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as Block from '@silklang/llvm/Block'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeResult from './NativeResult.js'
import * as Constant from '@silklang/llvm/Constant'
import type * as Mir from './Mir.js'

/** Pending normal exits of one invocation, sharing its diagnostic cleanup epilogue. */
export interface Completion {
  readonly block: Block.Block
  readonly exits: Array<{
    readonly block: Block.Block
    readonly fields: ReadonlyArray<Value.Input>
  }>
}

/** Completes one invocation through its declared synchronous or suspension result ABI. */
export const complete = Effect.fnUntraced(function* (
  context: NativeSuspension.ReturnContext,
  values: ReadonlyArray<Value.Input>,
  name: string,
  source?: Mir.LocalId,
): Effect.fn.Return<FunctionBody.Instruction, LlvmError.LlvmError> {
  if (values.length !== context.entry.resultLaneCount)
    throw new RangeError('Native completion does not match its declared result lanes')
  const outcome =
    source === undefined ? undefined : context.diagnostic?.outcomes.get(source.ordinal)
  if (context.entry.diagnosticResult !== undefined && source !== undefined && outcome === undefined)
    throw new RangeError('Native return lost its diagnostic outcome owner')
  let diagnostic: Value.Input | undefined
  if (context.entry.diagnosticResult !== undefined)
    diagnostic =
      outcome === undefined || context.diagnostic === undefined
        ? yield* Constant.nullValue(context.builder, context.entry.diagnosticResult)
        : yield* NativeDiagnosticOutcome.take(outcome, context.diagnostic)
  return yield* completeResult(
    context,
    { values, ...(diagnostic === undefined ? {} : { diagnostic }) },
    name,
  )
})

/** Consumes an already owned result and releases the invocation's remaining outcome references. */
export const completeResult = Effect.fnUntraced(function* (
  context: NativeSuspension.ReturnContext,
  result: NativeResult.NativeResult,
  name: string,
): Effect.fn.Return<FunctionBody.Instruction, LlvmError.LlvmError> {
  if (
    result.values.length !== context.entry.resultLaneCount ||
    (result.diagnostic !== undefined) !== (context.entry.diagnosticResult !== undefined)
  )
    throw new RangeError('Native completion does not match its declared result ABI')
  if (context.completion !== undefined) {
    // Name an explicit predecessor so the deferred phi never depends on an emitter's
    // internal insertion point. Ownership of the selected result has already moved out
    // of its outcome slot; cleanup can therefore run once after these paths join.
    const exit = yield* Block.make(context.body, 'completion_exit')
    yield* FunctionBody.branch(context.body, exit)
    yield* Block.setInsertionPoint(context.body, exit)
    context.completion.exits.push({
      block: exit,
      fields: NativeResult.fields(result, {
        resultLaneCount: context.entry.resultLaneCount,
        diagnosticResult: context.entry.diagnosticResult !== undefined,
      }),
    })
    return yield* FunctionBody.branch(context.body, context.completion.block)
  }
  return yield* emitResult(context, result, name)
})

/**
 * Seals normal exits after the body is emitted. Repeating all outcome releases at every
 * return made Token.write alone expand to 26,203 LLVM blocks in the self-hosted CLI.
 * Phi nodes preserve the chosen payload and diagnostic ownership through one cleanup.
 * Suspensions and traps keep their separate exits; only completed invocations join here.
 */
export const emitCompletion = Effect.fnUntraced(function* (
  context: NativeSuspension.ReturnContext,
) {
  const completion = context.completion
  if (completion === undefined) return
  yield* Block.setInsertionPoint(context.body, completion.block)
  const first = completion.exits.at(0)
  if (first === undefined) {
    yield* FunctionBody.unreachable(context.body)
    return
  }
  const fields: Array<Value.Input> = []
  for (const [ordinal, input] of first.fields.entries()) {
    const phi = yield* FunctionBody.phi(
      context.body,
      yield* FunctionBody.inputType(context.body, input),
      `completion_lane${ordinal}`,
    )
    for (const exit of completion.exits) {
      const value = exit.fields.at(ordinal)
      if (value === undefined) throw new RangeError('Native completion lost a result lane')
      yield* FunctionBody.addPhiIncoming(context.body, phi, value, exit.block)
    }
    yield* FunctionBody.sealPhi(context.body, phi)
    fields.push(yield* FunctionBody.phiValue(context.body, phi))
  }
  const diagnostic = fields.at(context.entry.resultLaneCount)
  yield* emitResult(
    context,
    {
      values: fields.slice(0, context.entry.resultLaneCount),
      ...(diagnostic === undefined ? {} : { diagnostic }),
    },
    'completion_result',
  )
})

const emitResult = Effect.fnUntraced(function* (
  context: NativeSuspension.ReturnContext,
  result: NativeResult.NativeResult,
  name: string,
) {
  if (context.diagnostic !== undefined)
    for (const outcome of context.diagnostic.outcomes.values())
      yield* NativeDiagnosticOutcome.release(outcome, context.diagnostic)
  if (context.entry.suspendable)
    return yield* NativeSuspension.returnStep(context, 0n, result.values, name, result.diagnostic)
  const packed = yield* NativeResult.pack(
    result,
    context,
    {
      resultLaneCount: context.entry.resultLaneCount,
      diagnosticResult: context.entry.diagnosticResult !== undefined,
    },
    context.entry.resultType,
    name,
  )
  return packed === undefined
    ? yield* FunctionBody.returnVoid(context.body)
    : yield* FunctionBody.returnValue(context.body, packed)
})
