import * as Effect from 'effect/Effect'
import type * as BlockActor from '../Block.js'
import type * as Builder from '../Builder.js'
import * as ByteString from '../ByteString.js'
import type * as Constant from '../Constant.js'
import type * as FunctionBodyActor from '../FunctionBody.js'
import { SilkError } from '../SilkError.js'
import type * as ValueActor from '../Value.js'
import * as BuilderState from './BuilderState.js'
import * as CanonicalKey from './CanonicalKey.js'
import * as FunctionBodyDescription from './FunctionBodyDescription.js'
import * as Handle from './Handle.js'
import type * as MetadataDescription from './MetadataDescription.js'
import type * as OwnedHandle from './OwnedHandle.js'
import type * as TypeDescription from './TypeDescription.js'

export type OperandInput = ValueActor.Value | Constant.Constant

interface MutableBlock {
  name: ByteString.ByteString
  instructions: Array<number>
  predecessors: Set<number>
}

interface MutableValue {
  type: number
  name: ByteString.ByteString
  source:
    | { readonly _tag: 'Argument'; readonly index: number }
    | { readonly _tag: 'Instruction'; readonly instruction: number }
    | { _tag: 'Forward'; resolved: FunctionBodyDescription.Operand | undefined }
}

export interface Draft {
  readonly builder: Builder.Builder
  readonly moduleOwner: OwnedHandle.Owner
  readonly owner: OwnedHandle.Owner
  readonly functionIndex: number
  readonly functionType: number
  readonly returnType: number
  readonly creatorFiber: number
  status: 'active' | 'committed' | 'failed'
  cursor: number | undefined
  readonly arguments: Array<number>
  readonly blocks: Array<MutableBlock>
  readonly blockHandles: Array<BlockActor.Block>
  readonly instructions: Array<FunctionBodyDescription.Instruction>
  readonly instructionHandles: Array<FunctionBodyActor.Instruction>
  readonly values: Array<MutableValue>
  readonly valueHandles: Array<ValueActor.Value>
  readonly metadata: Array<Array<MetadataDescription.Attachment>>
  readonly debugLocations: Array<number | undefined>
}

interface LocalEntry {
  readonly draft: Draft
  readonly index: number
}

const drafts = new WeakMap<FunctionBodyActor.FunctionBody, Draft>()
const blockEntries = new WeakMap<BlockActor.Block, LocalEntry>()
const instructionEntries = new WeakMap<FunctionBodyActor.Instruction, LocalEntry>()
const valueEntries = new WeakMap<ValueActor.Value, LocalEntry>()
const phiEntries = new WeakMap<FunctionBodyActor.Phi, LocalEntry>()
const switchEntries = new WeakMap<FunctionBodyActor.Switch, LocalEntry>()

/** @internal */
const bytes = (
  value: ByteString.ByteString | Uint8Array | string | undefined,
): ByteString.ByteString =>
  value === undefined
    ? ByteString.empty
    : typeof value === 'string'
      ? ByteString.fromString(value)
      : value instanceof Uint8Array
        ? ByteString.fromUint8Array(value)
        : value

/** @internal */
const fail = (operation: string, message: string, cause: unknown): never => {
  throw new SilkError({ operation, message, cause })
}

/** @internal */
const lookup = (self: FunctionBodyActor.FunctionBody, operation: string): Draft => {
  const draft = drafts.get(self)
  if (draft === undefined) return fail(operation, 'Unknown function-body draft', self)
  return draft
}

/** @internal */
const assertActive = (draft: Draft, fiber: number, operation: string): void => {
  if (draft.status !== 'active') {
    fail(operation, 'The function-body draft is no longer active', draft.status)
  }
  if (draft.creatorFiber !== fiber) {
    fail(operation, 'The function-body draft cannot be used from another fiber', {
      expected: draft.creatorFiber,
      actual: fiber,
    })
  }
}

/** @internal */
export const create = (
  builder: Builder.Builder,
  moduleOwner: OwnedHandle.Owner,
  functionIndex: number,
  functionType: number,
  signature: Extract<TypeDescription.Description, { readonly _tag: 'Function' }>,
  creatorFiber: number,
): FunctionBodyActor.FunctionBody => {
  const owner: OwnedHandle.Owner = Object.freeze({ token: Symbol('llvm-function-body-owner') })
  const self = Handle.make('FunctionBody', owner, 0)
  const draft: Draft = {
    builder,
    moduleOwner,
    owner,
    functionIndex,
    functionType,
    returnType: signature.returnType,
    creatorFiber,
    status: 'active',
    cursor: undefined,
    arguments: [],
    blocks: [],
    blockHandles: [],
    instructions: [],
    instructionHandles: [],
    values: [],
    valueHandles: [],
    metadata: [],
    debugLocations: [],
  }
  for (let index = 0; index < signature.parameters.length; index += 1) {
    const type = signature.parameters[index]
    if (type === undefined) continue
    const valueIndex = draft.values.length
    const handle = Handle.make('Value', owner, valueIndex)
    draft.values.push({
      type,
      name: ByteString.empty,
      source: { _tag: 'Argument', index },
    })
    draft.valueHandles.push(handle)
    draft.arguments.push(valueIndex)
    valueEntries.set(handle, { draft, index: valueIndex })
  }
  drafts.set(self, draft)
  return self
}

/** @internal */
export const builder = Effect.fn('FunctionBody.builder')(function* (
  self: FunctionBodyActor.FunctionBody,
): Effect.fn.Return<Builder.Builder, SilkError> {
  const fiber = yield* Effect.fiberId
  return yield* Effect.try({
    try: () => {
      const draft = lookup(self, 'FunctionBody.builder')
      assertActive(draft, fiber, 'FunctionBody.builder')
      return draft.builder
    },
    catch: (cause) =>
      cause instanceof SilkError
        ? cause
        : new SilkError({
            operation: 'FunctionBody.builder',
            message: 'Function-body lookup failed',
            cause,
          }),
  })
})

/** @internal */
export const mutate = <A>(
  self: FunctionBodyActor.FunctionBody,
  operation: string,
  transition: (draft: Draft) => A,
): Effect.Effect<A, SilkError> =>
  Effect.flatMap(Effect.fiberId, (fiber) =>
    Effect.try({
      try: () => {
        const draft = lookup(self, operation)
        assertActive(draft, fiber, operation)
        return transition(draft)
      },
      catch: (cause) =>
        cause instanceof SilkError
          ? cause
          : new SilkError({ operation, message: 'Function-body mutation failed', cause }),
    }),
  )

/** @internal */
export const mutateModule = <A>(
  self: FunctionBodyActor.FunctionBody,
  operation: string,
  transition: (draft: Draft, module: BuilderState.MutableState) => A,
): Effect.Effect<A, SilkError> =>
  Effect.flatMap(Effect.fiberId, (fiber) =>
    Effect.flatMap(
      Effect.try({
        try: () => lookup(self, operation),
        catch: (cause) =>
          cause instanceof SilkError
            ? cause
            : new SilkError({ operation, message: 'Function-body lookup failed', cause }),
      }),
      (draft) =>
        BuilderState.mutate(draft.builder, operation, (module) => {
          assertActive(draft, fiber, operation)
          return transition(draft, module)
        }),
    ),
  )

/** @internal */
export const close = (
  self: FunctionBodyActor.FunctionBody,
  status: 'committed' | 'failed',
): void => {
  const draft = drafts.get(self)
  if (draft !== undefined) draft.status = status
}

/** @internal */
const localEntry = <A extends object>(
  entries: WeakMap<A, LocalEntry>,
  draft: Draft,
  handle: A,
  operation: string,
  kind: string,
): LocalEntry => {
  const entry = entries.get(handle)
  if (entry === undefined) return fail(operation, `Unknown ${kind} handle`, handle)
  if (entry.draft !== draft) {
    return fail(operation, `The ${kind} handle belongs to a different function body`, handle)
  }
  return entry
}

/** @internal */
export const resolveBlock = (draft: Draft, block: BlockActor.Block, operation: string): number =>
  localEntry(blockEntries, draft, block, operation, 'block').index

/** @internal */
export const resolveInstruction = (
  draft: Draft,
  instruction: FunctionBodyActor.Instruction,
  operation: string,
): number => localEntry(instructionEntries, draft, instruction, operation, 'instruction').index

/** @internal */
export const resolvePhi = (draft: Draft, phi: FunctionBodyActor.Phi, operation: string): number =>
  localEntry(phiEntries, draft, phi, operation, 'phi').index

/** @internal */
export const resolveSwitch = (
  draft: Draft,
  value: FunctionBodyActor.Switch,
  operation: string,
): number => localEntry(switchEntries, draft, value, operation, 'switch').index

/** @internal */
const resolveLocalValue = (
  draft: Draft,
  value: ValueActor.Value,
  operation: string,
): { readonly operand: FunctionBodyDescription.Operand; readonly type: number } => {
  const entry = localEntry(valueEntries, draft, value, operation, 'value')
  const description = draft.values[entry.index]
  if (description === undefined) return fail(operation, 'Local value table entry is missing', value)
  return { operand: { _tag: 'Local', value: entry.index }, type: description.type }
}

/** @internal */
export const resolveOperand = (
  draft: Draft,
  module: BuilderState.MutableState,
  value: OperandInput,
  operation: string,
): { readonly operand: FunctionBodyDescription.Operand; readonly type: number } => {
  if (valueEntries.has(value as ValueActor.Value)) {
    return resolveLocalValue(draft, value as ValueActor.Value, operation)
  }
  const index = Handle.resolve(draft.builder, draft.moduleOwner, value, 'Constant', operation)
  const constant = module.constants[index]
  if (constant === undefined) return fail(operation, 'Constant table entry is missing', value)
  return { operand: { _tag: 'Constant', constant: index }, type: constant.type }
}

/** @internal */
export const typeAt = (
  module: BuilderState.MutableState,
  index: number,
  operation: string,
): TypeDescription.Description => {
  const description = module.types[index]
  return description === undefined
    ? fail(operation, 'Type table entry is missing', index)
    : description
}

/** @internal */
const scalarType = (
  module: BuilderState.MutableState,
  index: number,
  operation: string,
): TypeDescription.Description => {
  const description = typeAt(module, index, operation)
  return description._tag === 'Vector' ? typeAt(module, description.child, operation) : description
}

/** @internal */
export const isIntegerType = (
  module: BuilderState.MutableState,
  index: number,
  operation: string,
): boolean => scalarType(module, index, operation)._tag === 'Integer'

/** @internal */
export const isFloatingType = (
  module: BuilderState.MutableState,
  index: number,
  operation: string,
): boolean => {
  const description = scalarType(module, index, operation)
  return (
    description._tag === 'Simple' &&
    (description.tag === 'Half' ||
      description.tag === 'BFloat' ||
      description.tag === 'Float' ||
      description.tag === 'Double' ||
      description.tag === 'X86Fp80' ||
      description.tag === 'Fp128' ||
      description.tag === 'PpcFp128')
  )
}

/** @internal */
export const isPointerType = (
  module: BuilderState.MutableState,
  index: number,
  operation: string,
): boolean => scalarType(module, index, operation)._tag === 'Pointer'

/** @internal */
const typeKey = (description: TypeDescription.Description): string => {
  if (description._tag === 'Integer') {
    return CanonicalKey.tagged('integer', [CanonicalKey.integer(description.bitWidth)])
  }
  if (description._tag === 'Vector') {
    return CanonicalKey.tagged('vector', [
      CanonicalKey.integer(description.child),
      CanonicalKey.integer(description.length),
      description.scalable ? '1' : '0',
    ])
  }
  return fail('FunctionBody.typeKey', 'Unsupported comparison result type', description)
}

/** @internal */
const internType = (
  draft: Draft,
  module: BuilderState.MutableState,
  description: TypeDescription.Description,
): number => {
  const key = typeKey(description)
  const found = module.typeKeys.get(key)
  if (found !== undefined) return found
  const index = module.types.length
  const handle = Handle.make('Type', draft.moduleOwner, index)
  module.types.push(description)
  module.typeHandles.push(handle)
  module.typeKeys.set(key, index)
  return index
}

/** @internal */
export const comparisonType = (
  draft: Draft,
  module: BuilderState.MutableState,
  operandType: number,
): number => {
  const i1 = internType(draft, module, Object.freeze({ _tag: 'Integer', bitWidth: 1 }))
  const description = typeAt(module, operandType, 'FunctionBody.compare')
  return description._tag === 'Vector'
    ? internType(
        draft,
        module,
        Object.freeze({
          _tag: 'Vector',
          child: i1,
          length: description.length,
          scalable: description.scalable,
        }),
      )
    : i1
}

/** @internal */
const valueHandle = (draft: Draft, index: number, operation: string): ValueActor.Value => {
  const handle = draft.valueHandles[index]
  return handle === undefined ? fail(operation, 'Value table handle is missing', index) : handle
}

/** @internal */
const instructionHandle = (
  draft: Draft,
  index: number,
  operation: string,
): FunctionBodyActor.Instruction => {
  const handle = draft.instructionHandles[index]
  return handle === undefined
    ? fail(operation, 'Instruction table handle is missing', index)
    : handle
}

/** @internal */
const currentBlock = (
  draft: Draft,
  operation: string,
): { readonly index: number; readonly block: MutableBlock } => {
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
  return { index, block }
}

/** @internal */
export const appendInstruction = (
  draft: Draft,
  instruction: FunctionBodyDescription.Instruction,
): FunctionBodyActor.Instruction => {
  const cursor = currentBlock(draft, 'FunctionBody.appendInstruction')
  const index = draft.instructions.length
  const handle = Handle.make('Instruction', draft.owner, index)
  draft.instructions.push(instruction)
  draft.metadata.push([])
  draft.debugLocations.push(undefined)
  draft.instructionHandles.push(handle)
  cursor.block.instructions.push(index)
  instructionEntries.set(handle, { draft, index })
  return handle
}

/** @internal */
export const appendResult = (
  draft: Draft,
  type: number,
  name: ByteString.ByteString | Uint8Array | string | undefined,
  makeInstruction: (
    result: number,
    name: ByteString.ByteString,
  ) => FunctionBodyDescription.Instruction,
): { readonly value: ValueActor.Value; readonly instruction: FunctionBodyActor.Instruction } => {
  currentBlock(draft, 'FunctionBody.appendResult')
  const result = draft.values.length
  const instructionIndex = draft.instructions.length
  const finalName = bytes(name)
  const value = Handle.make('Value', draft.owner, result)
  draft.values.push({
    type,
    name: finalName,
    source: { _tag: 'Instruction', instruction: instructionIndex },
  })
  draft.valueHandles.push(value)
  valueEntries.set(value, { draft, index: result })
  const instruction = appendInstruction(draft, makeInstruction(result, finalName))
  return { value, instruction }
}

/** @internal */
export const makeBlock = (
  draft: Draft,
  name: ByteString.ByteString | Uint8Array | string | undefined,
): BlockActor.Block => {
  const index = draft.blocks.length
  const handle = Handle.make('Block', draft.owner, index)
  draft.blocks.push({ name: bytes(name), instructions: [], predecessors: new Set() })
  draft.blockHandles.push(handle)
  blockEntries.set(handle, { draft, index })
  if (draft.cursor === undefined) draft.cursor = index
  return handle
}

/** @internal */
export const addPredecessor = (draft: Draft, block: number, predecessor: number): void => {
  const target = draft.blocks[block]
  if (target === undefined) {
    throw new SilkError({
      operation: 'FunctionBody.addPredecessor',
      message: 'Destination block is missing',
      cause: block,
    })
  }
  target.predecessors.add(predecessor)
}

/** @internal */
export const setCursor = (draft: Draft, block: BlockActor.Block): void => {
  draft.cursor = resolveBlock(draft, block, 'Block.setInsertionPoint')
}

/** @internal */
export const argument = (draft: Draft, index: number): ValueActor.Value => {
  if (!Number.isSafeInteger(index) || index < 0) {
    return fail('Value.argument', 'Argument index must be a non-negative integer', index)
  }
  const value = draft.arguments[index]
  return value === undefined
    ? fail('Value.argument', 'Argument index is outside the function signature', index)
    : valueHandle(draft, value, 'Value.argument')
}

/** @internal */
export const forward = (
  draft: Draft,
  type: number,
  name: ByteString.ByteString | Uint8Array | string | undefined,
): ValueActor.Value => {
  const index = draft.values.length
  const handle = Handle.make('Value', draft.owner, index)
  draft.values.push({ type, name: bytes(name), source: { _tag: 'Forward', resolved: undefined } })
  draft.valueHandles.push(handle)
  valueEntries.set(handle, { draft, index })
  return handle
}

/** @internal */
export const resolveForward = (
  draft: Draft,
  module: BuilderState.MutableState,
  forwardValue: ValueActor.Value,
  resolved: OperandInput,
): void => {
  const entry = localEntry(valueEntries, draft, forwardValue, 'Value.resolveForward', 'value')
  const value = draft.values[entry.index]
  if (value === undefined || value.source._tag !== 'Forward') {
    throw new SilkError({
      operation: 'Value.resolveForward',
      message: 'Only a reserved forward value can be resolved',
      cause: forwardValue,
    })
  }
  const source = value.source
  if (source.resolved !== undefined) {
    fail('Value.resolveForward', 'Forward value has already been resolved', forwardValue)
  }
  const target = resolveOperand(draft, module, resolved, 'Value.resolveForward')
  if (target.type !== value.type) {
    fail('Value.resolveForward', 'Forward value resolution has the wrong type', resolved)
  }
  source.resolved = target.operand
}

/** @internal */
export const valueType = (draft: Draft, value: ValueActor.Value): number => {
  const entry = localEntry(valueEntries, draft, value, 'Value.typeOf', 'value')
  const description = draft.values[entry.index]
  return description === undefined
    ? fail('Value.typeOf', 'Value table entry is missing', value)
    : description.type
}

/** @internal */
export const setValueName = (
  draft: Draft,
  value: ValueActor.Value,
  name: ByteString.ByteString | Uint8Array | string,
): void => {
  const entry = localEntry(valueEntries, draft, value, 'Value.setName', 'value')
  const description = draft.values[entry.index]
  if (description === undefined) {
    throw new SilkError({
      operation: 'Value.setName',
      message: 'Value table entry is missing',
      cause: value,
    })
  }
  description.name = bytes(name)
  if (description.source._tag === 'Instruction') {
    const instruction = draft.instructions[description.source.instruction]
    if (instruction !== undefined) {
      draft.instructions[description.source.instruction] = Object.freeze({
        ...instruction,
        name: description.name,
      })
    }
  }
}

/** @internal */
export const valueName = (draft: Draft, value: ValueActor.Value): ByteString.ByteString => {
  const entry = localEntry(valueEntries, draft, value, 'Value.name', 'value')
  const description = draft.values[entry.index]
  return description === undefined
    ? fail('Value.name', 'Value table entry is missing', value)
    : description.name
}

/** @internal */
export const valueInstruction = (
  draft: Draft,
  value: ValueActor.Value,
): FunctionBodyActor.Instruction | undefined => {
  const entry = localEntry(valueEntries, draft, value, 'Value.instruction', 'value')
  const source = draft.values[entry.index]?.source
  return source?._tag === 'Instruction'
    ? instructionHandle(draft, source.instruction, 'Value.instruction')
    : undefined
}

/** @internal */
export const instructionResult = (
  draft: Draft,
  instruction: FunctionBodyActor.Instruction,
): ValueActor.Value | undefined => {
  const index = resolveInstruction(draft, instruction, 'FunctionBody.instructionResult')
  const result = draft.instructions[index]?.result
  return result === undefined
    ? undefined
    : valueHandle(draft, result, 'FunctionBody.instructionResult')
}

/** @internal */
export const makePhiHandle = (
  draft: Draft,
  instruction: FunctionBodyActor.Instruction,
): FunctionBodyActor.Phi => {
  const index = resolveInstruction(draft, instruction, 'FunctionBody.phi')
  const handle = Handle.make('Phi', draft.owner, index)
  phiEntries.set(handle, { draft, index })
  return handle
}

/** @internal */
export const makeSwitchHandle = (
  draft: Draft,
  instruction: FunctionBodyActor.Instruction,
): FunctionBodyActor.Switch => {
  const index = resolveInstruction(draft, instruction, 'FunctionBody.switchTerminator')
  const handle = Handle.make('Switch', draft.owner, index)
  switchEntries.set(handle, { draft, index })
  return handle
}

/** @internal */
const freezeInstruction = (
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
  if (instruction._tag === 'Call') {
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
): FunctionBodyDescription.Operand => {
  if (operand._tag === 'Constant') return operand
  const value = draft.values[operand.value]
  if (value === undefined) return fail('FunctionBody.validate', 'Local operand is missing', operand)
  if (value.source._tag !== 'Forward') return operand
  if (seen.has(operand.value))
    return fail('FunctionBody.validate', 'Forward value cycle detected', operand)
  if (value.source.resolved === undefined) {
    return fail('FunctionBody.validate', 'Forward value was never resolved', operand)
  }
  seen.add(operand.value)
  return resolvedOperand(draft, value.source.resolved, seen)
}

/** @internal */
const validateInstructionOperands = (
  draft: Draft,
  instruction: FunctionBodyDescription.Instruction,
): void => {
  const operands: Array<FunctionBodyDescription.Operand> = []
  switch (instruction._tag) {
    case 'Unary':
    case 'Cast':
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
    case 'Call':
      operands.push(
        instruction.callee,
        ...instruction.arguments,
        ...instruction.operandBundles.flatMap((bundle) => bundle.operands),
      )
      break
    case 'Branch':
    case 'Fence':
    case 'ReturnVoid':
    case 'Unreachable':
      break
  }
  for (const operand of operands) resolvedOperand(draft, operand)
}

/** @internal */
export const validate = Effect.fn('FunctionBody.validate')(function* (
  self: FunctionBodyActor.FunctionBody,
): Effect.fn.Return<FunctionBodyDescription.Snapshot, SilkError> {
  return yield* mutate(self, 'FunctionBody.validate', (draft) => {
    if (draft.blocks.length === 0) {
      return fail('FunctionBody.validate', 'A function body requires at least one block', self)
    }
    for (let blockIndex = 0; blockIndex < draft.blocks.length; blockIndex += 1) {
      const block = draft.blocks[blockIndex]
      if (block === undefined || block.instructions.length === 0) {
        return fail('FunctionBody.validate', 'Every block must contain a terminator', blockIndex)
      }
      const terminatorIndex = block.instructions.at(-1)
      const terminator =
        terminatorIndex === undefined ? undefined : draft.instructions[terminatorIndex]
      if (terminator === undefined || !FunctionBodyDescription.isTerminator(terminator)) {
        return fail(
          'FunctionBody.validate',
          'Every block must end in exactly one terminator',
          blockIndex,
        )
      }
      for (const instructionIndex of block.instructions) {
        const instruction = draft.instructions[instructionIndex]
        if (instruction === undefined) {
          return fail(
            'FunctionBody.validate',
            'Block references a missing instruction',
            instructionIndex,
          )
        }
        validateInstructionOperands(draft, instruction)
        if (instruction._tag === 'Switch' && !instruction.sealed) {
          fail('FunctionBody.validate', 'Switch construction was not finalized', instructionIndex)
        }
        if (instruction._tag === 'Phi') {
          if (!instruction.sealed) {
            fail('FunctionBody.validate', 'Phi construction was not finalized', instructionIndex)
          }
          const incoming = new Set(instruction.incoming.map((entry) => entry.block))
          if (
            incoming.size !== block.predecessors.size ||
            [...block.predecessors].some((predecessor) => !incoming.has(predecessor))
          ) {
            fail('FunctionBody.validate', 'Phi incoming blocks must cover every predecessor once', {
              block: blockIndex,
              predecessors: [...block.predecessors],
              incoming: [...incoming],
            })
          }
        }
      }
    }
    for (let valueIndex = 0; valueIndex < draft.values.length; valueIndex += 1) {
      const value = draft.values[valueIndex]
      if (value?.source._tag === 'Forward') {
        resolvedOperand(draft, { _tag: 'Local', value: valueIndex })
      }
    }
    return Object.freeze({
      arguments: Object.freeze([...draft.arguments]),
      blocks: Object.freeze(
        draft.blocks.map((block) =>
          Object.freeze({
            name: block.name,
            instructions: Object.freeze([...block.instructions]),
            predecessors: Object.freeze(
              [...block.predecessors].sort((left, right) => left - right),
            ),
          }),
        ),
      ),
      instructions: Object.freeze(draft.instructions.map(freezeInstruction)),
      values: Object.freeze(
        draft.values.map((value) =>
          Object.freeze({
            type: value.type,
            name: value.name,
            source:
              value.source._tag === 'Forward'
                ? Object.freeze({
                    _tag: 'Forward' as const,
                    resolved: value.source.resolved,
                  })
                : value.source,
          }),
        ),
      ),
      metadata: Object.freeze(draft.metadata.map((attachments) => Object.freeze([...attachments]))),
      debugLocations: Object.freeze([...draft.debugLocations]),
    })
  })
})

/** @internal */
export const snapshotDraft = (self: FunctionBodyActor.FunctionBody): Draft | undefined =>
  drafts.get(self)
