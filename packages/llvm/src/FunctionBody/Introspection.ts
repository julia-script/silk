import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Builder from '../Builder.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import { invalidState, type LlvmError } from '../LlvmError.js'
import type * as Type from '../Type.js'
import type * as Value from '../Value.js'
import type { FunctionBody, Instruction } from './_internal.js'

/**
 * Returns an instruction's SSA result, or `undefined` for void and terminator instructions.
 *
 * @category instructions
 * @since 0.0.0
 */
export const instructionResult = Effect.fnUntraced(function* (
  self: FunctionBody,
  instruction: Instruction,
): Effect.fn.Return<Value.Value | undefined, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.instructionResult', (draft) =>
    FunctionBodyState.instructionResult(draft, instruction),
  )
})

/**
 * Returns an instruction's zero-based semantic index in the body.
 *
 * @category instructions
 * @since 0.0.0
 */
export const instructionIndex = Effect.fnUntraced(function* (
  self: FunctionBody,
  instruction: Instruction,
): Effect.fn.Return<number, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.instructionIndex', (draft) =>
    FunctionBodyState.resolveInstruction(draft, instruction, 'FunctionBody.instructionIndex'),
  )
})

/**
 * Returns the module builder that owns an open body transaction.
 *
 * @category instructions
 * @since 0.0.0
 */
export const builder = Effect.fnUntraced(function* (
  self: FunctionBody,
): Effect.fn.Return<Builder.Builder, LlvmError> {
  return yield* FunctionBodyState.builder(self)
})

/**
 * Resolves the LLVM type of either a local SSA value or a module constant operand.
 *
 * @category instructions
 * @since 0.0.0
 */
export const inputType = Effect.fnUntraced(function* (
  self: FunctionBody,
  input: Value.Input,
): Effect.fn.Return<Type.Type, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.inputType', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        input,
        'FunctionBody.inputType',
      )
      const type = module.types.handles[resolved.type]
      if (type === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.inputType',
            message: 'Input type handle is missing',
            state: input,
          }),
        )
      }
      return type
    }),
  )
})
