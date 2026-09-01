import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as ByteString from '../ByteString.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import { invalidInput, type LlvmError } from '../LlvmError.js'
import type * as Value from '../Value.js'
import { type FastMathInput, type FunctionBody, fastMath } from './_internal.js'

/**
 * Appends floating-point negation after scalar/vector type validation.
 *
 * @category instructions
 * @since 0.0.0
 */
export const unary = Effect.fnUntraced(function* (
  self: FunctionBody,
  kind: 'fneg',
  operand: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
  options: { readonly fastMath?: FastMathInput } = {},
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.unary', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        operand,
        'FunctionBody.unary',
      )
      if (!(yield* FunctionBodyState.isFloatingType(module, resolved.type, 'FunctionBody.unary'))) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.unary',
            message: 'fneg requires a floating-point scalar or vector',
            input: operand,
          }),
        )
      }
      return (yield* FunctionBodyState.appendResult(
        draft,
        resolved.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'Unary',
            kind,
            operand: resolved.operand,
            fastMath: fastMath(options.fastMath),
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})

/**
 * Replaces an undefined or poison operand with an arbitrary stable value of the same type.
 *
 * This is useful at observation boundaries that must safely materialize every lane of a value
 * whose inactive representation lanes are intentionally unspecified.
 *
 * @category instructions
 * @since 0.0.0
 */
export const freeze = Effect.fnUntraced(function* (
  self: FunctionBody,
  operand: Value.Input,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.freeze', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        operand,
        'FunctionBody.freeze',
      )
      return (yield* FunctionBodyState.appendResult(
        draft,
        resolved.type,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'Freeze',
            operand: resolved.operand,
            result,
            name: finalName,
          }),
      )).value
    }),
  )
})
