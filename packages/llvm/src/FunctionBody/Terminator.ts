import * as Effect from 'effect/Effect'
import * as Result from 'effect/Result'
import type * as Block from '../Block.js'
import * as ByteString from '../ByteString.js'
import type * as Constant from '../Constant.js'
import * as FunctionBodyState from '../internal/FunctionBodyState.js'
import type * as Handle from '../internal/Handle.js'
import { invalidInput, invalidState, type LlvmError } from '../LlvmError.js'
import * as Type from '../Type.js'
import * as HandleActor from '../internal/Handle.js'
import type * as Value from '../Value.js'
import type { FunctionBody, Instruction } from './_internal.js'
import { setBranchWeights, setUnpredictable } from './Metadata.js'

/**
 * Two-phase identity for a switch terminator whose cases are still being assembled.
 *
 * @category instructions
 * @since 0.0.0
 */
export interface Switch extends Handle.Handle<'Switch'> {}

/**
 * Appends `indirectbr` to a non-empty, de-duplicated list of destination blocks.
 *
 * @category instructions
 * @since 0.0.0
 */
export const indirectBranch = Effect.fnUntraced(function* (
  self: FunctionBody,
  address: Value.Input,
  destinations: ReadonlyArray<Block.Block>,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.indirectBranch',
    (draft, module) =>
      Result.gen(function* () {
        const resolved = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          address,
          'FunctionBody.indirectBranch',
        )
        if (
          !(yield* FunctionBodyState.isPointerType(
            module,
            resolved.type,
            'FunctionBody.indirectBranch',
          ))
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.indirectBranch',
              message: 'indirectbr requires a pointer address',
              input: address,
            }),
          )
        }
        if (destinations.length === 0) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.indirectBranch',
              message: 'indirectbr requires at least one destination',
              input: destinations,
            }),
          )
        }
        const mutableBlocks: Array<number> = []
        for (const block of destinations) {
          mutableBlocks.push(
            yield* FunctionBodyState.resolveBlock(draft, block, 'FunctionBody.indirectBranch'),
          )
        }
        const blocks = Object.freeze(mutableBlocks)
        if (new Set(blocks).size !== blocks.length) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.indirectBranch',
              message: 'indirectbr destinations must be unique',
              input: destinations,
            }),
          )
        }
        const predecessor = draft.cursor
        if (predecessor === undefined) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.indirectBranch',
              message: 'Set an insertion block before adding indirectbr',
              input: address,
            }),
          )
        }
        const instruction = yield* FunctionBodyState.appendInstruction(
          draft,
          Object.freeze({
            _tag: 'IndirectBranch',
            address: resolved.operand,
            destinations: blocks,
            result: undefined,
            name: ByteString.empty,
          }),
        )
        for (const block of blocks)
          yield* FunctionBodyState.addPredecessor(draft, block, predecessor)
        return instruction
      }),
  )
})

/**
 * Terminates the insertion block with an unconditional branch and records its predecessor edge.
 *
 * @category instructions
 * @since 0.0.0
 */
export const branch = Effect.fnUntraced(function* (
  self: FunctionBody,
  destination: Block.Block,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.branch', (draft) =>
    Result.gen(function* () {
      const block = yield* FunctionBodyState.resolveBlock(draft, destination, 'FunctionBody.branch')
      const predecessor = draft.cursor
      if (predecessor === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.branch',
            message: 'Set an insertion block before branching',
            input: destination,
          }),
        )
      }
      const instruction = yield* FunctionBodyState.appendInstruction(
        draft,
        Object.freeze({
          _tag: 'Branch',
          destination: block,
          result: undefined,
          name: ByteString.empty,
        }),
      )
      yield* FunctionBodyState.addPredecessor(draft, block, predecessor)
      return instruction
    }),
  )
})

/**
 * Terminates the insertion block with an `i1` conditional branch and records both edges.
 *
 * @category instructions
 * @since 0.0.0
 */
export const conditionalBranch = Effect.fnUntraced(function* (
  self: FunctionBody,
  condition: Value.Input,
  onTrue: Block.Block,
  onFalse: Block.Block,
  weights: 'none' | 'unpredictable' | 'true-likely' | 'false-likely' = 'none',
): Effect.fn.Return<Instruction, LlvmError> {
  const instruction = yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.conditionalBranch',
    (draft, module) =>
      Result.gen(function* () {
        const resolved = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          condition,
          'FunctionBody.conditionalBranch',
        )
        const type = yield* FunctionBodyState.typeAt(
          module,
          resolved.type,
          'FunctionBody.conditionalBranch',
        )
        if (type._tag !== 'Integer' || type.bitWidth !== 1) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.conditionalBranch',
              message: 'Conditional branches require an i1 condition',
              input: condition,
            }),
          )
        }
        const trueBlock = yield* FunctionBodyState.resolveBlock(
          draft,
          onTrue,
          'FunctionBody.conditionalBranch',
        )
        const falseBlock = yield* FunctionBodyState.resolveBlock(
          draft,
          onFalse,
          'FunctionBody.conditionalBranch',
        )
        const predecessor = draft.cursor
        if (predecessor === undefined) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.conditionalBranch',
              message: 'Set an insertion block before branching',
              input: condition,
            }),
          )
        }
        const instruction = yield* FunctionBodyState.appendInstruction(
          draft,
          Object.freeze({
            _tag: 'ConditionalBranch',
            condition: resolved.operand,
            onTrue: trueBlock,
            onFalse: falseBlock,
            weights,
            result: undefined,
            name: ByteString.empty,
          }),
        )
        yield* FunctionBodyState.addPredecessor(draft, trueBlock, predecessor)
        yield* FunctionBodyState.addPredecessor(draft, falseBlock, predecessor)
        return instruction
      }),
  )
  if (weights === 'unpredictable') {
    yield* setUnpredictable(self, instruction)
  } else if (weights === 'true-likely' || weights === 'false-likely') {
    yield* setBranchWeights(self, instruction, weights === 'true-likely' ? [2000, 1] : [1, 2000])
  }
  return instruction
})

/**
 * Starts a switch terminator whose cases must be added and then finalized with {@link sealSwitch}.
 *
 * **Details**
 *
 * The switch remains intentionally incomplete so callers can assemble cases incrementally.
 *
 * **Gotchas**
 *
 * The body transaction cannot commit while any switch is unsealed.
 *
 * @category instructions
 * @since 0.0.0
 */
export const switchTerminator = Effect.fnUntraced(function* (
  self: FunctionBody,
  value: Value.Input,
  defaultBlock: Block.Block,
  weights: ReadonlyArray<number> = [],
): Effect.fn.Return<Switch, LlvmError> {
  return yield* FunctionBodyState.mutateModule(
    self,
    'FunctionBody.switchTerminator',
    (draft, module) =>
      Result.gen(function* () {
        const resolved = yield* FunctionBodyState.resolveOperand(
          draft,
          module,
          value,
          'FunctionBody.switchTerminator',
        )
        if (
          !(yield* FunctionBodyState.isIntegerType(
            module,
            resolved.type,
            'FunctionBody.switchTerminator',
          ))
        ) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.switchTerminator',
              message: 'switch requires an integer value',
              input: value,
            }),
          )
        }
        const destination = yield* FunctionBodyState.resolveBlock(
          draft,
          defaultBlock,
          'FunctionBody.switchTerminator',
        )
        const predecessor = draft.cursor
        if (predecessor === undefined) {
          return yield* Result.fail(
            invalidInput({
              operation: 'FunctionBody.switchTerminator',
              message: 'Set an insertion block before adding a switch',
              input: value,
            }),
          )
        }
        const instruction = yield* FunctionBodyState.appendInstruction(
          draft,
          Object.freeze({
            _tag: 'Switch',
            value: resolved.operand,
            defaultBlock: destination,
            cases: Object.freeze([]),
            weights: Object.freeze([...weights]),
            sealed: false,
            result: undefined,
            name: ByteString.empty,
          }),
        )
        yield* FunctionBodyState.addPredecessor(draft, destination, predecessor)
        return yield* FunctionBodyState.makeSwitchHandle(draft, instruction)
      }),
  )
})

/**
 * Adds one unique, same-typed integer constant case to an open switch.
 *
 * @category instructions
 * @since 0.0.0
 */
export const addSwitchCase = Effect.fnUntraced(function* (
  self: FunctionBody,
  switchHandle: Switch,
  value: Constant.Constant,
  destination: Block.Block,
): Effect.fn.Return<void, LlvmError> {
  yield* FunctionBodyState.mutateModule(self, 'FunctionBody.addSwitchCase', (draft, module) =>
    Result.gen(function* () {
      const index = yield* FunctionBodyState.resolveSwitch(
        draft,
        switchHandle,
        'FunctionBody.addSwitchCase',
      )
      const instruction = draft.instructions[index]
      if (instruction?._tag !== 'Switch' || instruction.sealed) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.addSwitchCase',
            message: 'Switch is missing or already finalized',
            state: switchHandle,
          }),
        )
      }
      const switchValue =
        instruction.value._tag === 'Constant'
          ? module.constants.descriptions[instruction.value.constant]
          : draft.values[instruction.value.value]
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.addSwitchCase',
      )
      if (switchValue === undefined || switchValue.type !== resolved.type) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.addSwitchCase',
            message: 'Switch case value must match the switch condition type',
            input: value,
          }),
        )
      }
      const constantIndex =
        resolved.operand._tag === 'Constant' ? resolved.operand.constant : undefined
      if (constantIndex === undefined) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.addSwitchCase',
            message: 'Switch cases must be module constants',
            input: value,
          }),
        )
      }
      if (instruction.cases.some((entry) => entry.value === constantIndex)) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.addSwitchCase',
            message: 'Switch case values must be unique',
            input: value,
          }),
        )
      }
      const block = yield* FunctionBodyState.resolveBlock(
        draft,
        destination,
        'FunctionBody.addSwitchCase',
      )
      const predecessor = draft.blocks.findIndex((candidate) =>
        candidate.instructions.includes(index),
      )
      draft.instructions[index] = Object.freeze({
        ...instruction,
        cases: Object.freeze([
          ...instruction.cases,
          Object.freeze({ value: constantIndex, block }),
        ]),
      })
      if (predecessor >= 0) yield* FunctionBodyState.addPredecessor(draft, block, predecessor)
    }),
  )
})

/**
 * Finalizes a switch exactly once and records predecessor edges for every destination.
 *
 * @category instructions
 * @since 0.0.0
 */
export const sealSwitch = Effect.fnUntraced(function* (
  self: FunctionBody,
  switchHandle: Switch,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.sealSwitch', (draft) =>
    Result.gen(function* () {
      const index = yield* FunctionBodyState.resolveSwitch(
        draft,
        switchHandle,
        'FunctionBody.sealSwitch',
      )
      const instruction = draft.instructions[index]
      if (instruction?._tag !== 'Switch' || instruction.sealed) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.sealSwitch',
            message: 'Switch is missing or already finalized',
            state: switchHandle,
          }),
        )
      }
      if (
        instruction.weights.length > 0 &&
        instruction.weights.length !== instruction.cases.length + 1
      ) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.sealSwitch',
            message: 'Switch weights must include the default and every case',
            input: instruction.weights,
          }),
        )
      }
      draft.instructions[index] = Object.freeze({ ...instruction, sealed: true })
      const handle = draft.instructionHandles[index]
      if (handle === undefined) {
        return yield* Result.fail(
          invalidState({
            operation: 'FunctionBody.sealSwitch',
            message: 'Switch instruction handle is missing',
            state: switchHandle,
          }),
        )
      }
      return handle
    }),
  )
})

/**
 * Terminates the insertion block by returning a value matching the function return type.
 *
 * @category instructions
 * @since 0.0.0
 */
export const returnValue = Effect.fnUntraced(function* (
  self: FunctionBody,
  value: Value.Input,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.returnValue', (draft, module) =>
    Result.gen(function* () {
      const resolved = yield* FunctionBodyState.resolveOperand(
        draft,
        module,
        value,
        'FunctionBody.returnValue',
      )
      if (resolved.type !== draft.returnType) {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.returnValue',
            message: 'Return value does not match the function return type',
            input: value,
          }),
        )
      }
      return yield* FunctionBodyState.appendInstruction(
        draft,
        Object.freeze({
          _tag: 'Return',
          value: resolved.operand,
          result: undefined,
          name: ByteString.empty,
        }),
      )
    }),
  )
})

/**
 * Terminates the insertion block with `ret void` after checking the signature.
 *
 * @category instructions
 * @since 0.0.0
 */
export const returnVoid = Effect.fnUntraced(function* (
  self: FunctionBody,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutateModule(self, 'FunctionBody.returnVoid', (draft, module) =>
    Result.gen(function* () {
      const returnType = yield* FunctionBodyState.typeAt(
        module,
        draft.returnType,
        'FunctionBody.returnVoid',
      )
      if (returnType._tag !== 'Simple' || returnType.tag !== 'Void') {
        return yield* Result.fail(
          invalidInput({
            operation: 'FunctionBody.returnVoid',
            message: 'returnVoid requires a void function return type',
            input: draft.returnType,
          }),
        )
      }
      return yield* FunctionBodyState.appendInstruction(
        draft,
        Object.freeze({
          _tag: 'ReturnVoid',
          result: undefined,
          name: ByteString.empty,
        }),
      )
    }),
  )
})

/**
 * Terminates the insertion block with LLVM's `unreachable` instruction.
 *
 * @category instructions
 * @since 0.0.0
 */
export const unreachable = Effect.fnUntraced(function* (
  self: FunctionBody,
): Effect.fn.Return<Instruction, LlvmError> {
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.unreachable', (draft) =>
    FunctionBodyState.appendInstruction(
      draft,
      Object.freeze({
        _tag: 'Unreachable',
        result: undefined,
        name: ByteString.empty,
      }),
    ),
  )
})

/**
 * Begins a cleanup-only Itanium exception handler with the canonical `{ ptr, i32 }` result.
 *
 * The enclosing function needs a personality and this must be its block's first non-PHI
 * instruction. Cleanup handlers must terminate or resume unwinding; this operation does not
 * implement language-level catches.
 *
 * @category instructions
 * @since 0.0.0
 */
export const cleanupLandingPad = Effect.fn('FunctionBody.cleanupLandingPad')(function* (
  self: FunctionBody,
  name?: ByteString.ByteString | Uint8Array | string,
): Effect.fn.Return<Value.Value, LlvmError> {
  const builder = yield* FunctionBodyState.builder(self)
  const type = yield* Type.structure(builder, [
    yield* Type.pointer(builder),
    yield* Type.integer(builder, 32),
  ])
  return yield* FunctionBodyState.mutate(self, 'FunctionBody.cleanupLandingPad', (draft) =>
    Result.gen(function* () {
      const typeIndex = yield* HandleActor.resolve(
        draft.builder,
        draft.moduleOwner,
        type,
        'Type',
        'FunctionBody.cleanupLandingPad',
      )
      return (yield* FunctionBodyState.appendResult(draft, typeIndex, name, (result, finalName) =>
        Object.freeze({ _tag: 'LandingPad', result, name: finalName, type: typeIndex }),
      )).value
    }),
  )
})
