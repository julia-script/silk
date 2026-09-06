import * as Attribute from '@silklang/llvm/Attribute'
import type * as Builder from '@silklang/llvm/Builder'
import type { LlvmError } from '@silklang/llvm/LlvmError'
import * as Effect from 'effect/Effect'
import type * as CAbi from './CAbi.js'

/** Emits the target-classified C integer extensions on declarations, thunks, and their direct calls. */
export const attributes = Effect.fn('NativeCAbi.attributes')(function* (
  builder: Builder.Builder,
  signature: CAbi.CAbiSignature,
): Effect.fn.Return<Attribute.FunctionSet | undefined, LlvmError> {
  const extended = (type: CAbi.CAbiType): boolean =>
    type._tag === 'Integer' && type.extension !== 'None'
  if (![signature.result, ...signature.parameters].some(extended)) return undefined
  const group = Effect.fnUntraced(function* (type: CAbi.CAbiType) {
    const entries =
      type._tag === 'Integer' && type.extension !== 'None'
        ? [yield* Attribute.flag(builder, type.extension === 'Sign' ? 'signext' : 'zeroext')]
        : []
    return yield* Attribute.set(builder, entries)
  })
  return yield* Attribute.functionSet(builder, {
    returnAttributes: yield* group(signature.result),
    parameterAttributes: yield* Effect.forEach(signature.parameters, group),
  })
})
