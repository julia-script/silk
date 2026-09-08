import * as FunctionBody from '@silklang/llvm/FunctionBody'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import type * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'

/** One invocation's source result lanes and separate private failure metadata. */
export interface NativeResult {
  readonly values: ReadonlyArray<Value.Input>
  readonly diagnostic?: Value.Input
}

/** The private metadata aggregate follows source lanes without changing their layout. */
export interface Shape {
  readonly resultLaneCount: number
  readonly diagnosticResult: boolean
}

/** Reads a result that cannot own failure metadata, rejecting an accidental discard. */
export const sourceValues = (self: NativeResult): ReadonlyArray<Value.Input> => {
  if (self.diagnostic !== undefined)
    throw new RangeError('Native source-only result cannot discard diagnostic ownership')
  return self.values
}

/** Unpacks a call result, optionally after a suspension status field. */
export const unpack = Effect.fnUntraced(function* (
  body: FunctionBody.FunctionBody,
  shape: Shape,
  value: Value.Input | undefined,
  name: string,
  status: 'Synchronous' | 'SuspensionStep' = 'Synchronous',
): Effect.fn.Return<NativeResult, LlvmError.LlvmError> {
  const offset = status === 'SuspensionStep' ? 1 : 0
  const count = shape.resultLaneCount + (shape.diagnosticResult ? 1 : 0)
  if (count === 0) return Object.freeze({ values: Object.freeze([]) })
  if (value === undefined) throw new RangeError('Native result lost its declared value')
  if (count === 1 && offset === 0 && !shape.diagnosticResult)
    return Object.freeze({ values: Object.freeze([value]) })
  const values: Array<Value.Input> = []
  for (let ordinal = 0; ordinal < shape.resultLaneCount; ordinal += 1)
    values.push(
      yield* FunctionBody.extractValue(body, value, [offset + ordinal], `${name}_${ordinal}`),
    )
  const diagnostic = shape.diagnosticResult
    ? yield* FunctionBody.extractValue(
        body,
        value,
        [offset + shape.resultLaneCount],
        `${name}_diagnostic`,
      )
    : undefined
  return Object.freeze({
    values: Object.freeze(values),
    ...(diagnostic === undefined ? {} : { diagnostic }),
  })
})

/** Validates the complete result instead of silently dropping private metadata. */
export const fields = (self: NativeResult, shape: Shape): ReadonlyArray<Value.Input> => {
  if (self.values.length !== shape.resultLaneCount)
    throw new RangeError('Native result does not match its source lane count')
  if ((self.diagnostic !== undefined) !== shape.diagnosticResult)
    throw new RangeError('Native result does not match its diagnostic ownership shape')
  return Object.freeze([
    ...self.values,
    ...(self.diagnostic === undefined ? [] : [self.diagnostic]),
  ])
}

/** Packs a synchronous private result; an empty result denotes a void return. */
export const pack = Effect.fnUntraced(function* (
  self: NativeResult,
  context: { readonly body: FunctionBody.FunctionBody },
  shape: Shape,
  resultType: LlvmType.Type,
  name: string,
): Effect.fn.Return<Value.Input | undefined, LlvmError.LlvmError> {
  const values = fields(self, shape)
  if (values.length === 0) return undefined
  if (values.length === 1 && !shape.diagnosticResult) return values.at(0)
  return yield* FunctionBody.buildAggregate(context.body, resultType, values, name)
})
