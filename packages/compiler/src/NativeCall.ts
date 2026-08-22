import * as LlvmBlock from '@silk-effect/llvm/Block'
import type * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import { suspensionPointKey } from './Backend.js'
import type * as Mir from './Mir.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeType from './NativeType.js'

export interface DeclaredTarget {
  readonly handle: FunctionActor.Function
  readonly resultLaneCount: number
  readonly suspendable: boolean
}

export interface SynchronousContext {
  readonly body: FunctionBody.FunctionBody
  readonly storage: NativeStorage.Context
}

/** Calls one synchronous native target and unpacks its ABI result lanes. */
export const callSynchronous = Effect.fnUntraced(function* (
  context: SynchronousContext,
  target: DeclaredTarget,
  arguments_: ReadonlyArray<Value.Input>,
  name: string,
): Effect.fn.Return<ReadonlyArray<Value.Input>, LlvmError.LlvmError> {
  if (target.suspendable)
    throw new RangeError('LLVM synchronous helper selected a suspendable target')
  const result = yield* FunctionBody.callDirect(context.body, target.handle, arguments_, name)
  for (const root of [...context.storage.addressRoots].sort((left, right) => left - right))
    yield* NativeStorage.reloadAddressRoot(context.storage, root)
  if (target.resultLaneCount === 0) return Object.freeze([])
  if (result === undefined) throw new RangeError('Backend call produced no value')
  if (target.resultLaneCount === 1) return Object.freeze([result])
  const values: Array<Value.Input> = []
  for (let lane = 0; lane < target.resultLaneCount; lane += 1)
    values.push(yield* FunctionBody.extractValue(context.body, result, [lane], `${name}_${lane}`))
  return Object.freeze(values)
})

/** Runtime inputs consumed by an Effect execution operation. */
export const operationInputs = (
  operation: Extract<
    Mir.Operation,
    { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
  >,
): ReadonlyArray<Mir.LocalId> =>
  operation._tag === 'RunEffect'
    ? Object.freeze(operation.arguments)
    : Object.freeze([operation.effect, ...operation.arguments])

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

export const callValues = Effect.fnUntraced(function* (
  context: Context,
  target: NativeLoweringContext.DeclaredFunction,
  arguments_: ReadonlyArray<Value.Input>,
  name: string,
  suspension?: Mir.RunSuspendableEffectRegion,
) {
  const {
    body,
    builder,
    entry,
    i8,
    i32,
    invocationFrameStorage,
    lanePointers,
    storage,
    types,
    pointer,
    program,
    resumeThunks,
    transferPointer,
  } = context
  const readLocal = (local: Mir.LocalId): ReadonlyArray<Value.Input> => {
    return NativeStorage.readLocal(storage, local)
  }
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
      ...arguments_,
      transferPointer,
      nullPointer,
      yield* Constant.integerUnsigned(builder, i32, 0n),
    ],
    name,
  )
  if (result === undefined) throw new RangeError('LLVM suspension step produced no value')
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
  const continuation = suspension.relay.state
  if (continuation !== undefined) {
    const generated = resumeThunks.get(suspensionPointKey(suspension.point))
    if (generated === undefined)
      throw new RangeError('LLVM coroutine relay lost its native frame plan')
    if (invocationFrameStorage === undefined)
      throw new RangeError('LLVM coroutine relay lost its invocation frame')
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
      [
        yield* Constant.integerUnsigned(
          builder,
          i32,
          BigInt(program.layout.target.pointerSize * 2),
        ),
      ],
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
    for (const field of generated.layout.payload) {
      const values = readLocal(field.local)
      const type = entry.fn.localTypes.at(field.local.ordinal)
      if (type === undefined) throw new RangeError('LLVM frame payload lost its type')
      const packed = NativeType.packLanes(
        program.layout.target,
        NativeType.lanesFor(types, type),
        field.offset,
      )
      for (const [ordinal, lane] of packed.entries.entries()) {
        const value = values.at(ordinal)
        if (value === undefined) throw new RangeError('LLVM frame payload lost a lane')
        yield* FunctionBody.store(
          body,
          value,
          yield* NativeLanePointer.lanePointer(
            lanePointers,
            body,
            frame,
            lane.offset,
            `${name}_payload${field.slot}_${ordinal}`,
          ),
        )
      }
    }
    yield* FunctionBody.store(body, frame, appendPointer)
    yield* FunctionBody.store(
      body,
      yield* NativeLanePointer.lanePointer(lanePointers, body, frame, 0, `${name}_next_append_ptr`),
      appendPointerPointer,
    )
  }
  yield* NativeSuspension.returnStep(context.returns, 1n, Object.freeze([]), `${name}_relayed`)
  yield* LlvmBlock.setInsertionPoint(body, completed)
  for (const root of [...storage.addressRoots].sort((left, right) => left - right)) {
    yield* NativeStorage.reloadAddressRoot(storage, root)
  }
  const values: Array<Value.Input> = []
  for (let lane = 0; lane < target.resultLaneCount; lane += 1) {
    values.push(yield* FunctionBody.extractValue(body, result, [lane + 1], `${name}_${lane}`))
  }
  return Object.freeze(values)
})
