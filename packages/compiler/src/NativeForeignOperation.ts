import type * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as Constant from '@silklang/llvm/Constant'
import type * as LlvmType from '@silklang/llvm/Type'
import * as Effect from 'effect/Effect'
import type * as CAbi from './CAbi.js'
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
  { readonly _tag: 'ForeignCall' | 'ForeignStaticLoad' | 'ForeignFunctionAddress' }
>

/** Emits one direct call to a declared foreign symbol; scalars are single lanes on both sides. */
export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
  if (operation._tag === 'ForeignFunctionAddress') {
    const address = context.foreignCallbacks.get(operation.symbol)
    if (address === undefined)
      throw new RangeError(`LLVM C callback ${operation.symbol} was not declared`)
    context.storage.locals.set(operation.destination.ordinal, Object.freeze([address]))
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
    context.storage.locals.set(operation.destination.ordinal, Object.freeze([value]))
    return
  }
  const { body, foreignFunctions, storage } = context
  const foreign = foreignFunctions.get(operation.symbol)
  if (foreign === undefined)
    throw new RangeError(`LLVM foreign function ${operation.symbol} was not declared`)
  const arguments_ = operation.arguments.flatMap((argument) => [
    ...NativeStorage.readLocal(storage, argument),
  ])
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
    storage.locals.set(operation.destination.ordinal, Object.freeze([]))
    return
  }
  if (result === undefined) throw new RangeError('LLVM foreign call returned no value')
  storage.locals.set(operation.destination.ordinal, Object.freeze([result]))
})
