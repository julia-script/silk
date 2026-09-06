import * as Result from 'effect/Result'
import * as ByteString from '../../ByteString.js'
import type * as FunctionBodyActor from '../../FunctionBody.js'
import type { LlvmError } from '../../LlvmError.js'
import type * as ValueActor from '../../Value.js'
import * as FunctionBodyDescription from '../FunctionBodyDescription.js'
import * as Handle from '../Handle.js'
import {
  type Draft,
  fail,
  instructionEntries,
  type MutableBlock,
  valueEntries,
} from './primitives.js'

/** @internal */
const currentBlock = (
  draft: Draft,
  operation: string,
): Result.Result<{ readonly index: number; readonly block: MutableBlock }, LlvmError> => {
  const index = draft.cursor
  const block = index === undefined ? undefined : draft.blocks[index]
  if (index === undefined || block === undefined) {
    return fail(operation, 'Set an insertion block before adding an instruction', draft.cursor)
  }
  const last = block.instructions.at(-1)
  const instruction = last === undefined ? undefined : draft.instructions[last]
  if (instruction !== undefined && FunctionBodyDescription.isTerminator(instruction)) {
    return fail(operation, 'Cannot add an instruction after a terminator', index)
  }
  return Result.succeed({ index, block })
}

/** @internal */
export const appendInstruction = (
  draft: Draft,
  instruction: FunctionBodyDescription.Instruction,
): Result.Result<FunctionBodyActor.Instruction, LlvmError> =>
  Result.gen(function* () {
    const cursor = yield* currentBlock(draft, 'FunctionBody.appendInstruction')
    const index = draft.instructions.length
    const handle = Handle.make('Instruction', draft.owner, index)
    draft.instructions.push(instruction)
    draft.metadata.push([])
    draft.debugLocations.push(undefined)
    draft.instructionHandles.push(handle)
    cursor.block.instructions.push(index)
    instructionEntries.set(handle, { draft, index })
    return handle
  })

/** @internal */
export const appendResult = (
  draft: Draft,
  type: number,
  name: ByteString.ByteString | Uint8Array | string | undefined,
  makeInstruction: (
    result: number,
    name: ByteString.ByteString,
  ) => FunctionBodyDescription.Instruction,
): Result.Result<
  { readonly value: ValueActor.Value; readonly instruction: FunctionBodyActor.Instruction },
  LlvmError
> =>
  Result.gen(function* () {
    yield* currentBlock(draft, 'FunctionBody.appendResult')
    const result = draft.values.length
    const instructionIndex = draft.instructions.length
    const finalName = ByteString.coerceOrEmpty(name)
    const value = Handle.make('Value', draft.owner, result)
    draft.values.push({
      type,
      name: finalName,
      source: { _tag: 'Instruction', instruction: instructionIndex },
    })
    draft.valueHandles.push(value)
    valueEntries.set(value, { draft, index: result })
    const instruction = yield* appendInstruction(draft, makeInstruction(result, finalName))
    return { value, instruction }
  })

/** @internal */
export const freezeInstruction = (
  instruction: FunctionBodyDescription.Instruction,
): FunctionBodyDescription.Instruction => {
  if (instruction._tag === 'Phi') {
    return Object.freeze({
      ...instruction,
      incoming: Object.freeze(instruction.incoming.map((entry) => Object.freeze({ ...entry }))),
    })
  }
  if (instruction._tag === 'Switch') {
    return Object.freeze({
      ...instruction,
      cases: Object.freeze(instruction.cases.map((entry) => Object.freeze({ ...entry }))),
      weights: Object.freeze([...instruction.weights]),
    })
  }
  if (instruction._tag === 'Call' || instruction._tag === 'Invoke') {
    return Object.freeze({
      ...instruction,
      arguments: Object.freeze([...instruction.arguments]),
      operandBundles: Object.freeze(
        instruction.operandBundles.map((bundle) =>
          Object.freeze({ ...bundle, operands: Object.freeze([...bundle.operands]) }),
        ),
      ),
    })
  }
  if (instruction._tag === 'GetElementPtr') {
    return Object.freeze({ ...instruction, indices: Object.freeze([...instruction.indices]) })
  }
  if (instruction._tag === 'IndirectBranch') {
    return Object.freeze({
      ...instruction,
      destinations: Object.freeze([...instruction.destinations]),
    })
  }
  return Object.freeze({ ...instruction })
}

/** @internal */
const resolvedOperand = (
  draft: Draft,
  operand: FunctionBodyDescription.Operand,
  seen: Set<number> = new Set(),
): Result.Result<FunctionBodyDescription.Operand, LlvmError> =>
  Result.gen(function* () {
    if (operand._tag === 'Constant') return operand
    const value = draft.values[operand.value]
    if (value === undefined) {
      return yield* fail('FunctionBody.validate', 'Local operand is missing', operand)
    }
    if (value.source._tag !== 'Forward') return operand
    if (seen.has(operand.value))
      return yield* fail('FunctionBody.validate', 'Forward value cycle detected', operand)
    if (value.source.resolved === undefined) {
      return yield* fail('FunctionBody.validate', 'Forward value was never resolved', operand)
    }
    seen.add(operand.value)
    return yield* resolvedOperand(draft, value.source.resolved, seen)
  })

/** @internal */
const validateInstructionOperands = (
  draft: Draft,
  instruction: FunctionBodyDescription.Instruction,
): Result.Result<void, LlvmError> =>
  Result.gen(function* () {
    const operands: Array<FunctionBodyDescription.Operand> = []
    switch (instruction._tag) {
      case 'Unary':
      case 'Cast':
      case 'Freeze':
        operands.push(instruction.operand)
        break
      case 'Binary':
      case 'Compare':
        operands.push(instruction.left, instruction.right)
        break
      case 'Select':
        operands.push(instruction.condition, instruction.onTrue, instruction.onFalse)
        break
      case 'ExtractValue':
        operands.push(instruction.aggregate)
        break
      case 'InsertValue':
        operands.push(instruction.aggregate, instruction.element)
        break
      case 'Alloca':
        operands.push(instruction.count)
        break
      case 'Load':
        operands.push(instruction.pointer)
        break
      case 'Store':
        operands.push(instruction.value, instruction.pointer)
        break
      case 'GetElementPtr':
        operands.push(instruction.base, ...instruction.indices)
        break
      case 'ExtractElement':
        operands.push(instruction.vector, instruction.index)
        break
      case 'InsertElement':
        operands.push(instruction.vector, instruction.element, instruction.index)
        break
      case 'ShuffleVector':
        operands.push(instruction.left, instruction.right, instruction.mask)
        break
      case 'CompareExchange':
        operands.push(instruction.pointer, instruction.comparison, instruction.replacement)
        break
      case 'AtomicRmw':
        operands.push(instruction.pointer, instruction.value)
        break
      case 'VaArg':
        operands.push(instruction.list)
        break
      case 'IndirectBranch':
        operands.push(instruction.address)
        break
      case 'ConditionalBranch':
        operands.push(instruction.condition)
        break
      case 'Switch':
        operands.push(instruction.value)
        break
      case 'Return':
        operands.push(instruction.value)
        break
      case 'Phi':
        operands.push(...instruction.incoming.map((entry) => entry.value))
        break
      case 'Invoke':
      case 'Call':
        operands.push(
          instruction.callee,
          ...instruction.arguments,
          ...instruction.operandBundles.flatMap((bundle) => bundle.operands),
        )
        break
      case 'LandingPad':
      case 'Branch':
      case 'Fence':
      case 'ReturnVoid':
      case 'Unreachable':
        break
    }
    for (const operand of operands) yield* resolvedOperand(draft, operand)
  })

/** @internal */
export const validateInstructions = (draft: Draft): Result.Result<void, LlvmError> =>
  Result.gen(function* () {
    for (const instruction of draft.instructions) {
      yield* validateInstructionOperands(draft, instruction)
    }
    for (let valueIndex = 0; valueIndex < draft.values.length; valueIndex += 1) {
      const value = draft.values[valueIndex]
      if (value?.source._tag === 'Forward') {
        yield* resolvedOperand(draft, { _tag: 'Local', value: valueIndex })
      }
    }
  })
