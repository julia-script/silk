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
  const { contract } = signature
  const functionAttributes: Array<Attribute.Attribute> = []
  if (contract.memory !== 'readwrite' || contract.locality !== 'external') {
    const access = { none: 0, read: 1, write: 2, readwrite: 3 }[contract.memory]
    const effects = contract.locality === 'arguments' ? access : access * 21
    functionAttributes.push(yield* Attribute.integer(builder, 'memory', effects))
  }
  if (contract.noReturn) functionAttributes.push(yield* Attribute.flag(builder, 'noreturn'))
  const group = Effect.fnUntraced(function* (type: CAbi.CAbiType, ordinal: number) {
    const entries: Array<Attribute.Attribute> = []
    if (type._tag === 'Integer' && type.extension !== 'None')
      entries.push(
        yield* Attribute.flag(builder, type.extension === 'Sign' ? 'signext' : 'zeroext'),
      )
    if (ordinal >= 0) {
      if (contract.noCapture.includes(ordinal) || contract.borrow.includes(ordinal))
        entries.push(yield* Attribute.integer(builder, 'captures', 0))
      if (contract.borrow.includes(ordinal)) {
        entries.push(yield* Attribute.flag(builder, 'nofree'))
        if (type._tag === 'Pointer' && !type.type.mutable)
          entries.push(yield* Attribute.flag(builder, 'readonly'))
      }
      if (contract.returned === ordinal) entries.push(yield* Attribute.flag(builder, 'returned'))
    }
    return yield* Attribute.set(builder, entries)
  })
  return yield* Attribute.functionSet(builder, {
    functionAttributes: yield* Attribute.set(builder, functionAttributes),
    returnAttributes: yield* group(signature.result, -1),
    parameterAttributes: yield* Effect.forEach(signature.parameters, group),
  })
})
