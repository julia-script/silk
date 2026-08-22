import type * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import type * as FunctionActor from '@silk-effect/llvm/Function'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import type * as Layout from './Layout.js'
import type * as Mir from './Mir.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'

export interface ReturnContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly i32: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly invocationFrameStorage?: Value.Input
  readonly coroutineFramePop?: FunctionActor.Function
  readonly lanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly laneType: (lane: Layout.CallingLane) => LlvmType.Type
}

/** Emits one suspension-step ABI return and releases a completed invocation frame. */
export const returnStep = Effect.fnUntraced(function* (
  context: ReturnContext,
  status: bigint,
  values: ReadonlyArray<Value.Input>,
  tag: string,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  if (!context.entry.suspendable)
    throw new RangeError('LLVM synchronous function cannot return a suspension step')
  if (status === 0n && context.invocationFrameStorage !== undefined) {
    if (context.coroutineFramePop === undefined)
      throw new RangeError('LLVM suspension step lost private frame release')
    yield* FunctionBody.callDirect(
      context.body,
      context.coroutineFramePop,
      [
        yield* FunctionBody.load(
          context.body,
          context.pointer,
          context.invocationFrameStorage,
          `${tag}_invocation_frame`,
        ),
      ],
      `${tag}_release_invocation_frame`,
    )
  }
  const padded = [...values]
  const resultLanes = context.lanesFor(context.entry.fn.result)
  while (padded.length < resultLanes.length) {
    const lane = resultLanes.at(padded.length)
    if (lane === undefined) break
    padded.push(yield* Constant.nullValue(context.builder, context.laneType(lane)))
  }
  yield* FunctionBody.returnValue(
    context.body,
    yield* FunctionBody.buildAggregate(
      context.body,
      context.entry.emittedResultType,
      Object.freeze([
        yield* Constant.integerUnsigned(context.builder, context.i32, status),
        ...padded.slice(0, resultLanes.length),
      ]),
      tag,
    ),
  )
})

/** Flattens the logical calling lanes retained across one suspension boundary. */
export const logicalLanes = (
  fn: Mir.MirFunction,
  locals: ReadonlyArray<Mir.LocalId>,
  lanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>,
): ReadonlyArray<Layout.CallingLane> =>
  Object.freeze(
    locals.flatMap((local) => {
      const type = fn.localTypes.at(local.ordinal)
      if (type === undefined) throw new RangeError(`LLVM suspension lost local %${local.ordinal}`)
      return lanesFor(type)
    }),
  )
