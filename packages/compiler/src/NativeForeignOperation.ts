import type * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as Constant from '@silklang/llvm/Constant'
import * as LlvmType from '@silklang/llvm/Type'
import * as Effect from 'effect/Effect'
import * as CAbi from './CAbi.js'
import type { LinearOperation } from './MirLinearization.js'
import type { Context } from './NativeOperationContext.js'
import * as NativeStorage from './NativeStorage.js'

/** One reachable foreign symbol declared once per module under the C calling convention. */
export interface Declaration {
  readonly handle: FunctionActor.Function
  readonly signature: CAbi.CAbiSignature
}

/** One LLVM global address and value type backing a C data symbol. */
export interface StaticDeclaration {
  readonly address: Constant.Constant
  readonly valueType: LlvmType.Type
}

type Operation = Extract<
  LinearOperation,
  {
    readonly _tag:
      | 'ForeignIndirectCall'
      | 'ForeignCall'
      | 'ForeignStaticLoad'
      | 'ForeignFunctionAddress'
  }
>

/** Emits one direct call to a declared foreign symbol; scalars are single lanes on both sides. */
export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  if (operation._tag === 'ForeignFunctionAddress') {
    const address = context.foreignCallbacks.get(operation.symbol)
    if (address === undefined)
      throw new RangeError(`LLVM C callback ${operation.symbol} was not declared`)
    yield* NativeStorage.writeLocal(
      context.storage,
      operation.destination.ordinal,
      Object.freeze([address]),
    )
    return
  }
  if (operation._tag === 'ForeignStaticLoad') {
    const foreign = context.foreignStatics.get(operation.symbol)
    if (foreign === undefined)
      throw new RangeError(`LLVM foreign static ${operation.symbol} was not declared`)
    const value = yield* FunctionBody.load(
      context.body,
      foreign.valueType,
      foreign.address,
      `foreign_static${operation.destination.ordinal}`,
    )
    yield* NativeStorage.writeLocal(
      context.storage,
      operation.destination.ordinal,
      Object.freeze([value]),
    )
    return
  }
  const { body, foreignFunctions, storage } = context
  const foreign =
    operation._tag === 'ForeignIndirectCall'
      ? context.foreignIndirects.get(CAbi.signatureKey(operation.signature))
      : foreignFunctions.get(CAbi.callKey(operation.symbol, operation.variadicArguments))
  if (foreign === undefined) throw new RangeError('LLVM foreign call was not declared')
  const arguments_ = (yield* NativeStorage.materializeArguments(
    storage,
    operation.arguments,
  )).flat()
  if (operation._tag === 'ForeignIndirectCall') {
    const callee = yield* NativeStorage.materialize(storage, operation.callee)
    if (callee.length !== 1) throw new RangeError('Native function pointer lost its address lane')
    arguments_.push(...callee)
  } else {
    for (const [ordinal, argument] of operation.variadicArguments.entries()) {
      if (argument.source.bits === argument.promoted.bits) continue
      const index = operation.signature.parameters.length + ordinal
      const value = arguments_[index]
      if (value === undefined) throw new RangeError('Missing variadic operand')
      const type = yield* LlvmType.integer(context.builder, argument.promoted.bits)
      arguments_[index] = yield* FunctionBody.cast(
        body,
        argument.source.signed ? 'sext' : 'zext',
        value,
        type,
        `vararg${index}`,
      )
    }
  }
  const result = yield* FunctionBody.callDirect(
    body,
    foreign.handle,
    arguments_,
    `foreign${operation.destination.ordinal}`,
  )
  for (const root of [...storage.addressRoots].sort((left, right) => left - right)) {
    yield* NativeStorage.reloadAddressRoot(storage, root)
  }
  if (foreign.signature.result._tag === 'Void') {
    yield* NativeStorage.writeLocal(storage, operation.destination.ordinal, Object.freeze([]))
    return
  }
  if (result === undefined) throw new RangeError('LLVM foreign call returned no value')
  yield* NativeStorage.writeLocal(storage, operation.destination.ordinal, Object.freeze([result]))
})
