import type * as LlvmBlock from '@silk-effect/llvm/Block'
import * as Effect from 'effect/Effect'
import * as CleanupPlan from './CleanupPlan.js'
import type * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeCallOperation from './NativeCallOperation.js'
import * as NativeEffectOperation from './NativeEffectOperation.js'
import * as NativeMemoryOperation from './NativeMemoryOperation.js'
import type * as NativeOperationContext from './NativeOperationContext.js'
import * as NativePlaceOperation from './NativePlaceOperation.js'
import * as NativeScalarOperation from './NativeScalarOperation.js'
import * as NativeValueOperation from './NativeValueOperation.js'
/** Whether one MIR operation requires the native allocation ABI. */
export const needsAllocation = (operation: Mir.Operation): boolean =>
  operation._tag === 'Allocate' ||
  operation._tag === 'RawBufferFrom' ||
  operation._tag === 'SharedFromAllocation' ||
  operation._tag === 'RawBufferCount' ||
  operation._tag === 'RawBufferSlot' ||
  operation._tag === 'RawBufferRead' ||
  operation._tag === 'RawBufferView' ||
  operation._tag === 'RawBufferCopy' ||
  operation._tag === 'RawBufferFill' ||
  operation._tag === 'SlotWrite' ||
  operation._tag === 'SlotTake' ||
  operation._tag === 'SlotCopy' ||
  operation._tag === 'SlotDrop' ||
  (operation._tag === 'CloseEffectEntry' &&
    operation.failures.some((failure) => CleanupPlan.reclaims(failure.cleanup))) ||
  (operation._tag === 'Drop' && CleanupPlan.reclaims(operation.cleanup))

/** Dispatch-only native operation context; each sibling actor owns its lowering behavior. */
export interface LoweringContext {
  readonly value: NativeOperationContext.Context
  readonly memory: NativeOperationContext.Context
  readonly place: NativeOperationContext.Context
  readonly scalar: NativeOperationContext.Context
  readonly effect: NativeOperationContext.Context
  readonly call: NativeOperationContext.Context
}
/** Mutable per-function dispatch state shared by cohesive operation actors. */
export interface State {
  trapBlock: LlvmBlock.Block | undefined
  checkOrdinal: number
}

export const emit = Effect.fnUntraced(function* (
  context: LoweringContext,
  operation: LinearOperation,
) {
  switch (operation._tag) {
    case 'BindMatch':
    case 'Literal':
    case 'StaticView':
    case 'StaticString':
    case 'StringFromUtf8Unchecked':
    case 'StringUtf8Bytes':
    case 'StringByteLength':
    case 'StringEqualsExact':
      return yield* NativeValueOperation.emit(context.value, operation)
    case 'Allocate':
    case 'HostWrite':
    case 'OsCall':
    case 'RawBufferFrom':
    case 'SharedFromAllocation':
    case 'RawBufferCount':
    case 'RawBufferSlot':
    case 'RawBufferRead':
    case 'RawBufferView':
    case 'RawBufferCopy':
    case 'RawBufferFill':
    case 'SlotWrite':
    case 'ValidateLayout':
    case 'RepeatLayout':
    case 'SlotTake':
    case 'SlotCopy':
    case 'SlotDrop':
      return yield* NativeMemoryOperation.emit(context.memory, operation)
    case 'Move':
    case 'BeginLoan':
    case 'EndLoan':
    case 'SliceLength':
    case 'ConvertUnion':
    case 'Construct':
    case 'ConstructArray':
    case 'Project':
    case 'ReadPlace':
    case 'CheckPlace':
    case 'WritePlace':
      return yield* NativePlaceOperation.emit(context.place, operation)
    case 'ConvertInteger':
    case 'ConvertScalar':
    case 'ReinterpretScalar':
    case 'FloatUnary':
    case 'FloatTranscendental':
    case 'CheckedScalar':
    case 'Binary':
      return yield* NativeScalarOperation.emit(context.scalar, operation)
    case 'Drop':
    case 'MakeEffect':
    case 'MakeCallable':
    case 'PackEffectComposite':
    case 'PackEffectOutcome':
    case 'PackEffectFailureUnion':
    case 'UnpackEffectSuccess':
    case 'RunEffect':
    case 'RunEffectComposite':
    case 'RunEffectValue':
    case 'RunStaticEffect':
    case 'ReifyEffect':
    case 'CloseEffectEntry':
      return yield* NativeEffectOperation.emit(context.effect, operation)
    case 'ApplyCallable':
    case 'Call':
      return yield* NativeCallOperation.emit(context.call, operation)
  }
})
