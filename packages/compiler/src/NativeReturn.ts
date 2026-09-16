import * as NativeDiagnosticOutcome from './NativeDiagnosticOutcome.js'
import * as Block from '@silklang/llvm/Block'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeResult from './NativeResult.js'
import * as Constant from '@silklang/llvm/Constant'
import type * as Mir from './Mir.js'
import * as NativePlace from './NativePlace.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeValue from './NativeValue.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'

/** Pending normal exits sharing one stored result conversion and diagnostic cleanup. */
export interface Completion {
  readonly block: Block.Block
  readonly place?: NativePlace.NativePlace
  readonly exits: Array<{
    readonly block: Block.Block
    readonly fields: ReadonlyArray<Value.Input>
  }>
}

/** Shares stored result conversion and diagnostic cleanup across normal exits. */
export const makeCompletion = Effect.fnUntraced(function* (
  context: NativePlace.Context,
  entry: NativeLoweringContext.DeclaredFunction,
  hasOutcomes: boolean,
): Effect.fn.Return<Completion | undefined, LlvmError.LlvmError> {
  const stored = NativeValue.classify(context.types.program.layout, entry.fn.result) === 'Place'
  if (!stored && !hasOutcomes) return undefined
  const place = stored
    ? yield* NativePlace.allocate(context, entry.fn.result, 'completion_storage', 'entry')
    : undefined
  return {
    block: yield* Block.make(context.body, 'completion'),
    ...(place === undefined ? {} : { place }),
    exits: [],
  }
})

const takeDiagnostic = Effect.fnUntraced(function* (
  context: NativeSuspension.ReturnContext,
  source?: Mir.LocalId,
) {
  const outcome =
    source === undefined ? undefined : context.diagnostic?.outcomes.get(source.ordinal)
  if (context.entry.diagnosticResult === undefined) return undefined
  if (source !== undefined && outcome === undefined)
    throw new RangeError('Native return lost its diagnostic outcome owner')
  return outcome === undefined || context.diagnostic === undefined
    ? yield* Constant.nullValue(context.builder, context.entry.diagnosticResult)
    : yield* NativeDiagnosticOutcome.take(outcome, context.diagnostic)
})

/** Carries a stored return to the shared boundary without expanding its union at each exit. */
export const completeLocal = Effect.fnUntraced(function* (
  context: NativeSuspension.ReturnContext,
  storage: NativeStorage.Context,
  source: Mir.LocalId,
  name: string,
) {
  const value = NativeStorage.readLocal(storage, source)
  const completion = context.completion
  if (completion?.place !== undefined && value._tag === 'NativePlace') {
    yield* NativePlace.transfer(completion.place, context, value)
    const diagnostic = yield* takeDiagnostic(context, source)
    return yield* enqueue(context, completion, diagnostic === undefined ? [] : [diagnostic])
  }
  return yield* complete(context, yield* NativeStorage.materialize(storage, source), name, source)
})

const enqueue = Effect.fnUntraced(function* (
  context: NativeSuspension.ReturnContext,
  completion: Completion,
  fields: ReadonlyArray<Value.Input>,
) {
  // Emission can move the insertion point internally; name an explicit phi predecessor.
  const exit = yield* Block.make(context.body, 'completion_exit')
  yield* FunctionBody.branch(context.body, exit)
  yield* Block.setInsertionPoint(context.body, exit)
  completion.exits.push({ block: exit, fields })
  return yield* FunctionBody.branch(context.body, completion.block)
})

/** Completes one invocation through its declared synchronous or suspension result ABI. */
export const complete = Effect.fnUntraced(function* (
  context: NativeSuspension.ReturnContext,
  values: ReadonlyArray<Value.Input>,
  name: string,
  source?: Mir.LocalId,
): Effect.fn.Return<FunctionBody.Instruction, LlvmError.LlvmError> {
  if (values.length !== context.entry.resultLaneCount)
    throw new RangeError('Native completion does not match its declared result lanes')
  const diagnostic = yield* takeDiagnostic(context, source)
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
    if (context.completion.place !== undefined) {
      yield* NativePlace.storeLanes(context.completion.place, context, result.values, name)
      return yield* enqueue(
        context,
        context.completion,
        result.diagnostic === undefined ? [] : [result.diagnostic],
      )
    }
    return yield* enqueue(
      context,
      context.completion,
      NativeResult.fields(result, {
        resultLaneCount: context.entry.resultLaneCount,
        diagnosticResult: context.entry.diagnosticResult !== undefined,
      }),
    )
  }
  return yield* emitResult(context, result, name)
})

/**
 * Seals normal exits after the body is emitted. Repeating all outcome releases at every
 * return made Token.write alone expand to 26,203 LLVM blocks in the self-hosted CLI.
 * Canonical storage preserves aggregate payloads; phis join direct values and diagnostic
 * ownership through one cleanup. Stored payloads cross into ABI lanes only after the join.
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
  const values =
    completion.place === undefined
      ? fields.slice(0, context.entry.resultLaneCount)
      : yield* NativePlace.loadLanes(completion.place, context, 'completion_value')
  const diagnostic = fields.at(completion.place === undefined ? context.entry.resultLaneCount : 0)
  yield* emitResult(
    context,
    {
      values,
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
  if (context.entry.resultStorage !== undefined) {
    yield* NativeResult.store(
      context.body,
      context.entry.resultStorage,
      yield* Value.argument(context.body, context.entry.resultStorage.parameter),
      NativeResult.fields(result, {
        resultLaneCount: context.entry.resultLaneCount,
        diagnosticResult: context.entry.diagnosticResult !== undefined,
      }),
      name,
    )
    return yield* FunctionBody.returnVoid(context.body)
  }
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
