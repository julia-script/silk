import type * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as LlvmType from '@silklang/llvm/Type'
import type * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as ContinuationTransfer from './ContinuationTransfer.js'

/** The private transfer's completed-result slot has one producer and one consuming continuation. */
export interface NativeDiagnosticTransfer {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly wordSize: number
  readonly transfer: Value.Input
}

const address = Effect.fnUntraced(function* (self: NativeDiagnosticTransfer) {
  return yield* FunctionBody.getElementPtr(
    self.body,
    yield* LlvmType.integer(self.builder, 8),
    self.transfer,
    [
      yield* Constant.integerUnsigned(
        self.builder,
        yield* LlvmType.integer(self.builder, 32),
        BigInt(ContinuationTransfer.diagnosticResultOffset(self.wordSize)),
      ),
    ],
    'transfer_diagnostic_ptr',
  )
})

/** Publishes a transferred reference after its predecessor has been consumed. */
export const publish = Effect.fnUntraced(function* (
  self: NativeDiagnosticTransfer,
  value: Value.Input,
) {
  yield* FunctionBody.store(self.body, value, yield* address(self))
})

/** Moves completed metadata out before another child or resumed frame can publish a result. */
export const take = Effect.fnUntraced(function* (
  self: NativeDiagnosticTransfer,
  type: LlvmType.Type,
) {
  const storage = yield* address(self)
  const value = yield* FunctionBody.load(self.body, type, storage, 'transfer_diagnostic')
  yield* FunctionBody.store(self.body, yield* Constant.nullValue(self.builder, type), storage)
  return value
})
