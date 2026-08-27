import * as LlvmBlock from '@silklang/llvm/Block'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as Effect from 'effect/Effect'
import * as Layout from './Layout.js'
import type { LinearOperation } from './MirLinearization.js'
import * as NativeAggregate from './NativeAggregate.js'
import * as NativeCallOperation from './NativeCallOperation.js'
import * as NativeLanePointer from './NativeLanePointer.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'
import * as SilkType from './Type.js'

type Operation = Extract<LinearOperation, { readonly _tag: 'SharedWithMut' }>

const callableArguments = (
  context: Context,
  local: Operation['use'],
): ReadonlyArray<SilkType.GenericArgument> => {
  const type = context.entry.fn.localTypes.at(local.ordinal)
  return type?._tag === 'CallableValue'
    ? ((type.environment === undefined
        ? undefined
        : Layout.callableTargetArguments(type.environment)) ??
        type.storage?.realization.targetArguments ??
        Object.freeze([]))
    : Object.freeze([])
}

/** Emits one closed local-shared access transition without exposing its state word. */
export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  const {
    body,
    builder,
    cleanup,
    lanePointers,
    pointer,
    storage: nativeStorage,
    usizeType,
  } = context
  const self = NativeStorage.readLocal(nativeStorage, operation.self).at(0)
  if (self === undefined || usizeType === undefined)
    throw new RangeError('LLVM local-shared access lost its borrowed handle')
  const baseAddress = yield* FunctionBody.load(
    body,
    usizeType,
    self,
    `shared${operation.destination.ordinal}_base_address`,
  )
  const base = yield* FunctionBody.cast(
    body,
    'inttoptr',
    baseAddress,
    pointer,
    `shared${operation.destination.ordinal}_base`,
  )
  const accessPointer = yield* NativeLanePointer.lanePointer(
    lanePointers,
    body,
    base,
    operation.block.accessOffset,
    `shared${operation.destination.ordinal}_access_ptr`,
  )
  const access = yield* FunctionBody.load(
    body,
    usizeType,
    accessPointer,
    `shared${operation.destination.ordinal}_access`,
  )
  const available = yield* FunctionBody.integerCompare(
    body,
    'eq',
    access,
    yield* Constant.integerUnsigned(builder, usizeType, 0n),
    `shared${operation.destination.ordinal}_available`,
  )
  const useBlock = yield* LlvmBlock.make(body, `shared${operation.destination.ordinal}_use`)
  const conflictBlock = yield* LlvmBlock.make(
    body,
    `shared${operation.destination.ordinal}_conflict`,
  )
  const following = yield* LlvmBlock.make(body, `shared${operation.destination.ordinal}_following`)
  yield* FunctionBody.conditionalBranch(body, available, useBlock, conflictBlock)

  const initialLocals = new Map(nativeStorage.locals)
  const apply = Effect.fnUntraced(function* (
    callable: Operation['use'],
    callableType: SilkType.Callable,
    arguments_: ReadonlyArray<Operation['payload']>,
  ) {
    yield* NativeCallOperation.emit(
      context,
      Object.freeze({
        _tag: 'ApplyCallable' as const,
        destination: operation.destination,
        callable,
        typeArguments: callableArguments(context, callable),
        captures: Object.freeze([]),
        arguments: Object.freeze(arguments_),
        callableType,
        access: 'Take' as const,
        evaluation: 'CalleeThenArguments' as const,
        realization: 'Environment' as const,
        type: operation.type,
        provenance: operation.provenance,
      }),
    )
    const realizedCallable = context.entry.fn.localTypes.at(callable.ordinal)
    const diverges =
      realizedCallable?._tag === 'CallableValue' && SilkType.isNever(realizedCallable.type.result)
    if (!diverges)
      yield* NativeStorage.storeMutable(
        nativeStorage,
        operation.destination,
        NativeStorage.readLocal(nativeStorage, operation.destination),
      )
    return diverges
  })

  yield* LlvmBlock.setInsertionPoint(body, useBlock)
  nativeStorage.locals.clear()
  for (const [ordinal, values] of initialLocals) nativeStorage.locals.set(ordinal, values)
  yield* FunctionBody.store(
    body,
    yield* Constant.integerUnsigned(builder, usizeType, 1n),
    accessPointer,
  )
  nativeStorage.locals.set(
    operation.payload.ordinal,
    Object.freeze([
      yield* NativeLanePointer.lanePointer(
        lanePointers,
        body,
        base,
        operation.block.valueOffset,
        `shared${operation.destination.ordinal}_payload`,
      ),
    ]),
  )
  const useDiverges = yield* apply(
    operation.use,
    operation.useType,
    Object.freeze([operation.payload]),
  )
  if (useDiverges) {
    yield* FunctionBody.unreachable(body)
  } else {
    yield* FunctionBody.store(
      body,
      yield* Constant.integerUnsigned(builder, usizeType, 0n),
      accessPointer,
    )
    yield* NativeAggregate.dropThroughPlan(
      cleanup,
      operation.conflictCleanup,
      NativeStorage.readLocal(nativeStorage, operation.onConflict),
      `shared${operation.destination.ordinal}_unused_conflict`,
    )
    yield* FunctionBody.branch(body, following)
  }

  yield* LlvmBlock.setInsertionPoint(body, conflictBlock)
  nativeStorage.locals.clear()
  for (const [ordinal, values] of initialLocals) nativeStorage.locals.set(ordinal, values)
  const conflictDiverges = yield* apply(
    operation.onConflict,
    operation.conflictType,
    Object.freeze([]),
  )
  if (conflictDiverges) {
    yield* FunctionBody.unreachable(body)
  } else {
    yield* NativeAggregate.dropThroughPlan(
      cleanup,
      operation.useCleanup,
      NativeStorage.readLocal(nativeStorage, operation.use),
      `shared${operation.destination.ordinal}_unused_use`,
    )
    yield* FunctionBody.branch(body, following)
  }

  yield* LlvmBlock.setInsertionPoint(body, following)
  yield* NativeStorage.reloadRoots(
    nativeStorage,
    `shared${operation.destination.ordinal}_following`,
  )
})
