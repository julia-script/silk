import type * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
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

type Operation = Extract<LinearOperation, { readonly _tag: 'ForeignCall' }>

/** Emits one direct call to a declared foreign symbol; scalars are single lanes on both sides. */
export const emit = Effect.fnUntraced(function* (context: Context, operation: Operation) {
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
