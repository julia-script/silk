import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Block from '../Block.js'
import type * as ByteString from '../ByteString.js'
import * as FastMathActor from '../FastMath.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import * as Handle from '../internal/Handle.js'
import { invalidInput, invalidState, type LlvmError } from '../LlvmError.js'
import type * as Type from '../Type.js'
import type * as Value from '../Value.js'
import { type FastMathInput, type FunctionBody, fastMath } from './_internal.js'

/**
 * Two-phase identity for a phi instruction whose incoming edges are still being assembled.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface Phi extends Handle.Handle<'Phi'> {}

/**
 * Starts a phi instruction whose incoming edges must be completed with {@link addPhiIncoming}.
 *
 * **Details**
 *
 * Use {@link phiValue} when later instructions need the result before the phi is sealed.
 * The final incoming block set must cover every predecessor exactly once.
 *
 * @category instructions
 * @since 0.0.0
 */
export const phi = Effect.fnUntraced(function* (
  self: FunctionBody,
  type: Type.Type,
  name?: ByteString.ByteString | Uint8Array | string,
  options: { readonly fastMath?: FastMathInput } = {},
): Effect.fn.Return<Phi, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.phi', (draft, module) =>
    Result.gen(function* () {
      const typeIndex = yield* Handle.resolve(
        draft.builder,
        draft.moduleOwner,
        type,
        'Type',
        'FunctionBody.phi',
      )
      if (
        FastMathActor.toBitcode(fastMath(options.fastMath)) !== 0 &&
        !(yield* FunctionBodyState.isFloatingType(module, typeIndex, 'FunctionBody.phi'))
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.phi',
            message: 'Fast-math phi requires a floating-point type',
            input: type,
          }),
        )
      }
      const appended = yield* FunctionBodyState.appendResult(
        draft,
        typeIndex,
        name,
        (result, finalName) =>
          Object.freeze({
            _tag: 'Phi',
            type: typeIndex,
            incoming: Object.freeze([]),
            fastMath: fastMath(options.fastMath),
            sealed: false,
            result,
            name: finalName,
          }),
      )
      return yield* FunctionBodyState.makePhiHandle(draft, appended.instruction)
    }),
  )
})

/**
 * Returns the SSA result of an open or sealed phi instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export const phiValue = Effect.fnUntraced(function* (
  self: FunctionBody,
  phiHandle: Phi,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.phiValue', (draft) =>
    Result.gen(function* () {
      const instruction =
        draft.instructions[
          yield* FunctionBodyState.resolvePhi(draft, phiHandle, 'FunctionBody.phiValue')
        ]
      if (instruction?._tag !== 'Phi') {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.phiValue',
            message: 'Phi instruction is missing',
            state: phiHandle,
          }),
        )
      }
      const value = draft.valueHandles[instruction.result]
      if (value === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.phiValue',
            message: 'Phi result value is missing',
            state: phiHandle,
          }),
        )
      }
      return value
    }),
  )
})

/**
 * Adds one same-typed value from a unique predecessor block to an open phi.
 *
 * @category instructions
 * @since 0.0.0
 */
export const addPhiIncoming = Effect.fnUntraced(function* (
  self: FunctionBody,
  phiHandle: Phi,
  value: Value.Input,
  block: Block.Block,
): Effect.fn.Return<void, LlvmError> {
  yield* FunctionBodyState.mutateModule(self, 'FunctionBody.addPhiIncoming', (draft, module) =>
    Result.gen(function* () {
      const index = yield* FunctionBodyState.resolvePhi(
        draft,
        phiHandle,
        'FunctionBody.addPhiIncoming',
      )
      const instruction = draft.instructions[index]
      if (instruction?._tag !== 'Phi' || instruction.sealed) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.addPhiIncoming',
            message: 'Phi is missing or already sealed',
            state: phiHandle,
          }),
        )
      }
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.addPhiIncoming',
      )
      if (resolved.type !== instruction.type) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.addPhiIncoming',
            message: 'Phi incoming value has the wrong type',
            input: value,
          }),
        )
      }
      const blockIndex = yield* FunctionBodyState.resolveBlock(
        draft,
        block,
        'FunctionBody.addPhiIncoming',
      )
      if (instruction.incoming.some((entry) => entry.block === blockIndex)) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.addPhiIncoming',
            message: 'Phi already has an incoming value for this block',
            state: block,
          }),
        )
      }
      draft.instructions[index] = Object.freeze({
        ...instruction,
        incoming: Object.freeze([
          ...instruction.incoming,
          Object.freeze({ value: resolved.operand, block: blockIndex }),
        ]),
      })
    }),
  )
})

/**
 * Validates predecessor coverage, finalizes a phi exactly once, and returns its SSA value.
 *
 * @category instructions
 * @since 0.0.0
 */
export const sealPhi = Effect.fnUntraced(function* (
  self: FunctionBody,
  phiHandle: Phi,
): Effect.fn.Return<Value.Value, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.sealPhi', (draft) =>
    Result.gen(function* () {
      const index = yield* FunctionBodyState.resolvePhi(draft, phiHandle, 'FunctionBody.sealPhi')
      const instruction = draft.instructions[index]
      if (instruction?._tag !== 'Phi' || instruction.sealed) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.sealPhi',
            message: 'Phi is missing or already sealed',
            state: phiHandle,
          }),
        )
      }
      draft.instructions[index] = Object.freeze({ ...instruction, sealed: true })
      const value = draft.valueHandles[instruction.result]
      if (value === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.sealPhi',
            message: 'Phi result value is missing',
            state: phiHandle,
          }),
        )
      }
      return value
    }),
  )
})
