import * as LlvmBlock from '@silklang/llvm/Block'
import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import { suspensionPointKey } from './Backend.js'
import type * as Mir from './Mir.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeFrame from './NativeFrame.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeType from './NativeType.js'
import * as NativeDiagnosticContext from './NativeDiagnosticContext.js'
import * as NativeResult from './NativeResult.js'
import * as NativeArgument from './NativeArgument.js'

export interface DeclaredTarget {
  /** Absent only for compiler-synthesized LLVM helpers with already-physical scalar inputs. */
  readonly argumentParameters?: ReadonlyArray<NativeArgument.Parameter>
  readonly handle: FunctionActor.Function
  readonly resultLaneCount: number
  readonly diagnosticResult?: LlvmType.Type
  readonly suspendable: boolean
  readonly diagnosticParameter?: number
}

export interface SynchronousContext {
  readonly body: FunctionBody.FunctionBody
  readonly storage: NativeStorage.Context
  readonly diagnostic?: NativeDiagnosticContext.NativeDiagnosticContext
}

/** Ordinary calls inherit observation; independently owned execution bodies start a fresh root. */
export const argumentsFor = Effect.fnUntraced(function* (
  context: Pick<SynchronousContext, 'diagnostic'>,
  target: Pick<DeclaredTarget, 'diagnosticParameter'>,
  arguments_: ReadonlyArray<Value.Input>,
  observation: 'Inherited' | 'Independent' = 'Inherited',
) {
  if (target.diagnosticParameter === undefined) return arguments_
  if (context.diagnostic === undefined)
    throw new RangeError('Native call lost its invocation diagnostic context')
  if (arguments_.length !== target.diagnosticParameter)
    throw new RangeError('Native call diagnostic argument is not after its source parameters')
  return Object.freeze([
    ...arguments_,
    observation === 'Independent'
      ? yield* Constant.nullValue(context.diagnostic.builder, context.diagnostic.pointer)
      : yield* NativeDiagnosticContext.current(context.diagnostic),
    observation === 'Independent'
      ? yield* Constant.nullValue(context.diagnostic.builder, context.diagnostic.causeType)
      : yield* NativeDiagnosticContext.currentCause(context.diagnostic),
  ])
})

/** Resolves logical arguments before appending the separately borrowed diagnostic context. */
export const lowerArguments = Effect.fnUntraced(function* (
  context: SynchronousContext,
  target: Pick<DeclaredTarget, 'diagnosticParameter' | 'argumentParameters'>,
  inputs: NativeArgument.NativeArgument,
  observation: 'Inherited' | 'Independent' = 'Inherited',
) {
  const arguments_ =
    target.argumentParameters === undefined
      ? yield* NativeArgument.materialize(context.storage, inputs, 'native_helper_arguments')
      : yield* NativeArgument.lower(
          context.storage,
          target.argumentParameters,
          inputs,
          `call_argument${context.storage.sequences.materialize++}`,
        )
  return yield* argumentsFor(context, target, arguments_, observation)
})

/** Calls one synchronous native target and unpacks its ABI result lanes. */
export const callSynchronous = Effect.fnUntraced(function* (
  context: SynchronousContext,
  target: DeclaredTarget,
  arguments_: NativeArgument.NativeArgument,
  name: string,
): Effect.fn.Return<NativeResult.NativeResult, LlvmError.LlvmError> {
  if (target.suspendable)
    throw new RangeError('LLVM synchronous helper selected a suspendable target')
  const result = yield* FunctionBody.callDirect(
    context.body,
    target.handle,
    yield* lowerArguments(context, target, arguments_),
    name,
  )
  for (const root of [...context.storage.addressRoots].sort((left, right) => left - right))
    yield* NativeStorage.reloadAddressRoot(context.storage, root)
  const unpacked = yield* NativeResult.unpack(
    context.body,
    {
      resultLaneCount: target.resultLaneCount,
      diagnosticResult: target.diagnosticResult !== undefined,
    },
    result,
    name,
  )
  return unpacked
})

/** Runtime inputs consumed by an Effect execution operation. */
export const operationInputs = (
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'CatchEffect' | 'ExecutionPark' }
  >,
): ReadonlyArray<Mir.LocalId> => {
  if (operation._tag === 'ExecutionPark') {
    return Object.freeze([operation.register])
  }
  if (operation._tag === 'RunEffect') {
    return Object.freeze(operation.arguments)
  }
  return Object.freeze([operation.effect, ...operation.arguments])
}

export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly program: Mir.Module
  readonly i8: LlvmType.Type
  readonly i32: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly transferPointer?: Value.Input
  readonly invocationFrameStorage?: Value.Input
  readonly resumeThunks: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly layout: Mir.CoroutineFrameTargetStateLayout
    }
  >
  readonly lanePointers: NativeLanePointer.Context
  readonly types: NativeType.LoweringContext
  readonly storage: NativeStorage.Context
  readonly synchronous: SynchronousContext
  readonly returns: NativeSuspension.ReturnContext
}

/** Retains one canonical relay frame in the transfer-owned continuation chain. */
export const retainRelay = Effect.fnUntraced(function* (
  context: Context,
  suspension: Mir.RunSuspendableEffectRegion,
  name: string,
) {
  const {
    body,
    builder,
    entry,
    i8,
    i32,
    invocationFrameStorage,
    lanePointers,
    pointer,
    program,
    resumeThunks,
    storage,
    transferPointer,
  } = context
  const continuation = suspension.relay.state
  if (continuation === undefined) return
  if (transferPointer === undefined || invocationFrameStorage === undefined)
    throw new RangeError('LLVM coroutine relay lost transfer or invocation-frame authority')
  const generated = resumeThunks.get(suspensionPointKey(suspension.point))
  if (generated === undefined)
    throw new RangeError('LLVM coroutine relay lost its native frame plan')
  const frame = yield* FunctionBody.load(
    body,
    pointer,
    invocationFrameStorage,
    `${name}_invocation_frame`,
  )
  const appendPointerPointer = yield* FunctionBody.getElementPtr(
    body,
    i8,
    transferPointer,
    [yield* Constant.integerUnsigned(builder, i32, BigInt(program.layout.target.pointerSize * 2))],
    `${name}_append_ptr_ptr`,
  )
  const appendPointer = yield* FunctionBody.load(
    body,
    pointer,
    appendPointerPointer,
    `${name}_append_ptr`,
  )
  const next = yield* FunctionBody.load(body, pointer, appendPointer, `${name}_next`)
  yield* FunctionBody.store(
    body,
    next,
    yield* NativeLanePointer.lanePointer(lanePointers, body, frame, 0, `${name}_store_parent`),
  )
  yield* FunctionBody.store(
    body,
    yield* Constant.fromGlobal(builder, yield* FunctionActor.global(builder, generated.handle)),
    yield* NativeLanePointer.lanePointer(
      lanePointers,
      body,
      frame,
      program.layout.target.pointerSize,
      `${name}_store_resume`,
    ),
  )
  if (entry.diagnosticParameter !== undefined) {
    const diagnostic = context.synchronous.diagnostic
    if (diagnostic === undefined) throw new RangeError('Relay lost its diagnostic context')
    yield* FunctionBody.store(
      body,
      yield* NativeDiagnosticContext.current(diagnostic),
      yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        frame,
        program.layout.target.pointerSize * 2,
        `${name}_store_observer`,
      ),
    )
    yield* FunctionBody.store(
      body,
      diagnostic.incomingCause,
      yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        frame,
        program.layout.target.pointerSize * 3,
        `${name}_incoming_cause`,
      ),
    )
    yield* FunctionBody.store(
      body,
      yield* NativeDiagnosticContext.currentCause(diagnostic),
      yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        frame,
        program.layout.target.pointerSize * 9,
        `${name}_current_cause`,
      ),
    )
  }
  for (const field of generated.layout.payload) {
    yield* NativeFrame.retain(storage, frame, field, `${name}_payload${field.slot}`)
  }
  yield* FunctionBody.store(body, frame, appendPointer)
  yield* FunctionBody.store(
    body,
    yield* NativeLanePointer.lanePointer(lanePointers, body, frame, 0, `${name}_next_append_ptr`),
    appendPointerPointer,
  )
})

export const callValues = Effect.fnUntraced(function* (
  context: Context,
  target: NativeLoweringContext.DeclaredFunction,
  arguments_: NativeArgument.NativeArgument,
  name: string,
  suspension?: Mir.RunSuspendableEffectRegion,
) {
  const { body, builder, entry, i32, storage, pointer, transferPointer } = context
  if (!target.suspendable)
    return yield* callSynchronous(context.synchronous, target, arguments_, name)
  if (transferPointer === undefined || suspension === undefined)
    throw new RangeError(
      `LLVM suspension-aware call from ${entry.fn.id.module}.${entry.fn.id.name} to ${target.fn.id.module}.${target.fn.id.name} lost transfer control`,
    )
  const nullPointer = yield* Constant.nullValue(builder, pointer)
  const result = yield* FunctionBody.callDirect(
    body,
    target.handle,
    [
      ...(yield* lowerArguments(context.synchronous, target, arguments_)),
      transferPointer,
      nullPointer,
      yield* Constant.integerUnsigned(builder, i32, 0n),
    ],
    name,
  )
  if (result === undefined) throw new RangeError('LLVM suspension step produced no value')
  // The callee may mutate borrowed state before transferring. Refresh it before either
  // consuming a completed result or spilling the caller's continuation payload.
  for (const root of [...storage.addressRoots].sort((left, right) => left - right))
    yield* NativeStorage.reloadAddressRoot(storage, root)
  const status = yield* FunctionBody.extractValue(body, result, [0], `${name}_status`)
  const completed = yield* LlvmBlock.make(body, `${name}_complete`)
  const transferred = yield* LlvmBlock.make(body, `${name}_transfer`)
  yield* FunctionBody.conditionalBranch(
    body,
    yield* FunctionBody.integerCompare(
      body,
      'eq',
      status,
      yield* Constant.integerUnsigned(builder, i32, 0n),
      `${name}_is_complete`,
    ),
    completed,
    transferred,
  )
  yield* LlvmBlock.setInsertionPoint(body, transferred)
  yield* retainRelay(context, suspension, name)
  const external = yield* LlvmBlock.make(body, `${name}_external`)
  const nested = yield* LlvmBlock.make(body, `${name}_nested`)
  yield* FunctionBody.conditionalBranch(
    body,
    yield* FunctionBody.integerCompare(
      body,
      'eq',
      status,
      yield* Constant.integerUnsigned(builder, i32, 2n),
      `${name}_is_external`,
    ),
    external,
    nested,
  )
  yield* LlvmBlock.setInsertionPoint(body, external)
  yield* NativeSuspension.returnStep(context.returns, 2n, Object.freeze([]), `${name}_external`)
  yield* LlvmBlock.setInsertionPoint(body, nested)
  yield* NativeSuspension.returnStep(context.returns, 1n, Object.freeze([]), `${name}_relayed`)
  yield* LlvmBlock.setInsertionPoint(body, completed)
  const unpacked = yield* NativeResult.unpack(
    body,
    {
      resultLaneCount: target.resultLaneCount,
      diagnosticResult: target.diagnosticResult !== undefined,
    },
    result,
    name,
    'SuspensionStep',
  )
  return unpacked
})
