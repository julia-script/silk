import type * as LlvmBlock from '@silk-effect/llvm/Block'
import type * as Builder from '@silk-effect/llvm/Builder'
import type * as Constant from '@silk-effect/llvm/Constant'
import type * as FunctionActor from '@silk-effect/llvm/Function'
import type * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import type * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'
import * as CleanupPlan from './CleanupPlan.js'
import type * as Layout from './Layout.js'
import type * as Mir from './Mir.js'
import type { LinearOperation } from './MirLinearization.js'
import type * as NativeAggregate from './NativeAggregate.js'
import type * as NativeArith from './NativeArith.js'
import type * as NativeCall from './NativeCall.js'
import * as NativeCallOperation from './NativeCallOperation.js'
import * as NativeEffectOperation from './NativeEffectOperation.js'
import type * as NativeLanePointer from './NativeLanePointer.js'
import type * as NativeLoweringContext from './NativeLoweringContext.js'
import * as NativeMemoryOperation from './NativeMemoryOperation.js'
import * as NativePlaceOperation from './NativePlaceOperation.js'
import * as NativeScalarOperation from './NativeScalarOperation.js'
import type * as NativeSuspension from './NativeSuspension.js'
import * as NativeValueOperation from './NativeValueOperation.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as SilkType from './Type.js'

/** Whether one MIR operation requires the native allocation ABI. */
export const needsAllocation = (operation: Mir.Operation): boolean =>
  operation._tag === 'Allocate' ||
  operation._tag === 'RawBufferFrom' ||
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

interface OverflowSignature {
  readonly returnType: LlvmType.Type
  readonly parameters: ReadonlyArray<LlvmType.Type>
}

export interface LoweringContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly program: Mir.Module
  readonly entry: NativeLoweringContext.DeclaredFunction
  readonly declared: ReadonlyArray<NativeLoweringContext.DeclaredFunction>
  readonly locals: Map<number, ReadonlyArray<Value.Input>>
  readonly staticPointers: ReadonlyMap<string, Constant.Constant>
  readonly i32: LlvmType.Type
  readonly f32: LlvmType.Type
  readonly f64: LlvmType.Type
  readonly pointer: LlvmType.Type
  readonly usizeType?: LlvmType.Type
  readonly integerTypes: Map<number, LlvmType.Type>
  readonly signedOverflowSignatures: Map<number, OverflowSignature>
  readonly unsignedOverflowSignatures: Map<number, OverflowSignature>
  readonly malloc?: FunctionActor.Function
  readonly free?: FunctionActor.Function
  readonly memcmp?: FunctionActor.Function
  readonly standardWrite?: FunctionActor.Function
  readonly osRuntimes: ReadonlyMap<
    string,
    {
      readonly handle: FunctionActor.Function
      readonly abi: 'Direct' | 'OpenOut'
      readonly resultLaneCount: number
      readonly symbol: string
    }
  >
  readonly lanePointers: NativeLanePointer.Context
  readonly addressRoots: ReadonlySet<number>
  readonly addressStorage: Map<number, Value.Input>
  readonly mutableStorage: ReadonlyMap<number, ReadonlyArray<Value.Input>>
  readonly suspensionRegions: ReadonlyMap<Mir.Operation, Mir.SuspensionRegion>
  readonly lanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly valueLanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>
  readonly laneType: (lane: Layout.CallingLane) => LlvmType.Type
  readonly coerceLane: (
    input: Value.Input,
    source: Layout.CallingLane,
    target: Layout.CallingLane,
    name: string,
  ) => Effect.Effect<Value.Input, LlvmError.LlvmError>
  readonly locate: (
    span: SourceSpan.SourceSpan,
    instruction: FunctionBody.Instruction | undefined,
  ) => Effect.Effect<void, LlvmError.LlvmError>
  readonly constantBytePointer: (
    base: Value.Input,
    offset: number,
    name: string,
  ) => Effect.Effect<Value.Value, LlvmError.LlvmError>
  readonly aggregateFieldOffset: (type: SilkType.Type, name: string) => number
  readonly emitHostFailure: (
    operation: Extract<Mir.Operation, { readonly _tag: 'Allocate' | 'HostWrite' }>,
  ) => Effect.Effect<void, LlvmError.LlvmError>
  readonly materializeAddressRoot: (root: Mir.LocalId) => Effect.Effect<void, LlvmError.LlvmError>
  readonly ensureAddressRoot: (root: Mir.LocalId) => Effect.Effect<void, LlvmError.LlvmError>
  readonly reloadAddressRoot: (root: number) => Effect.Effect<void, LlvmError.LlvmError>
  readonly reloadMutableRoots: (tag: string) => Effect.Effect<void, LlvmError.LlvmError>
  readonly storeMutable: (
    root: Mir.LocalId,
    values: ReadonlyArray<Value.Input>,
  ) => Effect.Effect<void, LlvmError.LlvmError>
  readonly cleanup: NativeAggregate.Context
  readonly failure: NativeAggregate.FailureContext
  readonly arith: NativeArith.OperationContext
  readonly call: NativeCall.Context
  readonly suspension: NativeSuspension.OperationContext
  readonly state: State
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
      return yield* NativeValueOperation.emit(context, operation)
    case 'Allocate':
    case 'HostWrite':
    case 'OsCall':
    case 'RawBufferFrom':
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
      return yield* NativeMemoryOperation.emit(context, operation)
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
      return yield* NativePlaceOperation.emit(context, operation)
    case 'ConvertInteger':
    case 'ConvertScalar':
    case 'ReinterpretScalar':
    case 'FloatUnary':
    case 'FloatTranscendental':
    case 'CheckedScalar':
    case 'Binary':
      return yield* NativeScalarOperation.emit(context, operation)
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
      return yield* NativeEffectOperation.emit(context, operation)
    case 'ApplyCallable':
    case 'Call':
      return yield* NativeCallOperation.emit(context, operation)
  }
})
