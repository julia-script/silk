import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeResult from './NativeResult.js'
import * as Constant from '@silklang/llvm/Constant'
import type * as Mir from './Mir.js'

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
