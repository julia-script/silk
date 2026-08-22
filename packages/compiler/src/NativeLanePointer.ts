import type * as Builder from '@silk-effect/llvm/Builder'
import * as Constant from '@silk-effect/llvm/Constant'
import * as FunctionBody from '@silk-effect/llvm/FunctionBody'
import type * as LlvmType from '@silk-effect/llvm/Type'
import type * as Value from '@silk-effect/llvm/Value'
import * as Effect from 'effect/Effect'

export interface Context {
  readonly builder: Builder.Builder
  readonly byteType: LlvmType.Type
  readonly offsetType: LlvmType.Type
}

/** Projects one byte-addressed native lane from a base pointer. */
export const lanePointer = Effect.fnUntraced(function* (
  self: Context,
  body: FunctionBody.FunctionBody,
  base: Value.Input,
  offset: number | Value.Input,
  name: string,
) {
  const index =
    typeof offset === 'number'
      ? yield* Constant.integerUnsigned(self.builder, self.offsetType, BigInt(offset))
      : offset
  return yield* FunctionBody.getElementPtr(body, self.byteType, base, [index], name)
})
