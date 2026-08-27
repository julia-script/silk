import type * as Builder from '@silk-lang/llvm/Builder'
import * as Constant from '@silk-lang/llvm/Constant'
import * as FunctionBody from '@silk-lang/llvm/FunctionBody'
import type * as LlvmError from '@silk-lang/llvm/LlvmError'
import type * as Value from '@silk-lang/llvm/Value'
import * as Effect from 'effect/Effect'
import type * as Mir from './Mir.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeSuspension from './NativeSuspension.js'
import * as NativeType from './NativeType.js'

/** Failure-return state for allocation and host-I/O boundaries. */
export interface Context {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly types: NativeType.LoweringContext
  readonly suspension: NativeSuspension.ReturnContext
}

/** Emits one host-boundary failure in the function's synchronous or suspension ABI. */
export const emit = Effect.fnUntraced(function* (
  context: Context,
  operation: Extract<Mir.Operation, { readonly _tag: 'Allocate' | 'HostWrite' }>,
): Effect.fn.Return<void, LlvmError.LlvmError> {
  const lanes = NativeType.lanesFor(context.types, operation.propagationType)
  const values: Array<Value.Input> = []
  for (const [ordinal, lane] of lanes.entries()) {
    values.push(
      yield* Constant.integerUnsigned(
        context.builder,
        NativeType.laneType(context.types, lane),
        ordinal === 0 ? BigInt(operation.failureTag) : 0n,
      ),
    )
  }
  if (context.entry.suspendable) {
    yield* NativeSuspension.returnStep(
      context.suspension,
      0n,
      Object.freeze(values),
      `host_failure${operation.destination.ordinal}`,
    )
    return
  }
  if (values.length === 0) {
    yield* FunctionBody.returnVoid(context.body)
    return
  }
  const single = values.at(0)
  if (values.length === 1 && single !== undefined) {
    yield* FunctionBody.returnValue(context.body, single)
    return
  }
  yield* FunctionBody.returnValue(
    context.body,
    yield* FunctionBody.buildAggregate(
      context.body,
      context.entry.resultType,
      Object.freeze(values),
      `host_failure${operation.destination.ordinal}`,
    ),
  )
})
