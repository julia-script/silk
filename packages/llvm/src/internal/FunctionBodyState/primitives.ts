import * as Result from 'effect/Result'
import type * as BlockActor from '../../Block.js'
import type * as Builder from '../../Builder.js'
import type * as ByteString from '../../ByteString.js'
import type * as Constant from '../../Constant.js'
import type * as FunctionBodyActor from '../../FunctionBody.js'
import { invalidInput, type LlvmError } from '../../LlvmError.js'
import type * as ValueActor from '../../Value.js'
import type * as FunctionBodyDescription from '../FunctionBodyDescription.js'
import type * as MetadataDescription from '../MetadataDescription.js'
import type * as OwnedHandle from '../OwnedHandle.js'

export type OperandInput = ValueActor.Value | Constant.Constant

export interface MutableBlock {
  name: ByteString.ByteString
  instructions: Array<number>
  predecessors: Set<number>
}

export interface MutableValue {
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

export interface LocalEntry {
  readonly draft: Draft
  readonly index: number
}

export const drafts = new WeakMap<FunctionBodyActor.FunctionBody, Draft>()

export const blockEntries = new WeakMap<BlockActor.Block, LocalEntry>()

export const instructionEntries = new WeakMap<FunctionBodyActor.Instruction, LocalEntry>()

export const valueEntries = new WeakMap<ValueActor.Value, LocalEntry>()

export const phiEntries = new WeakMap<FunctionBodyActor.Phi, LocalEntry>()

export const switchEntries = new WeakMap<FunctionBodyActor.Switch, LocalEntry>()

/** @internal */
export const fail = (
  operation: string,
  message: string,
  cause: unknown,
): Result.Result<never, LlvmError> =>
  Result.fail(invalidInput({ operation, message, input: cause }))

/** @internal */
export const lookup = (
  self: FunctionBodyActor.FunctionBody,
  operation: string,
): Result.Result<Draft, LlvmError> => {
  const draft = drafts.get(self)
  if (draft === undefined) return fail(operation, 'Unknown function-body draft', self)
  return Result.succeed(draft)
}

/** @internal */
export const assertActive = (
  draft: Draft,
  fiber: number,
  operation: string,
): Result.Result<void, LlvmError> => {
  if (draft.status !== 'active') {
    return fail(operation, 'The function-body draft is no longer active', draft.status)
  }
  if (draft.creatorFiber !== fiber) {
    return fail(operation, 'The function-body draft cannot be used from another fiber', {
      expected: draft.creatorFiber,
      actual: fiber,
    })
  }
  return Result.succeed(undefined)
}

/** @internal */
export const localEntry = <A extends object>(
  entries: WeakMap<A, LocalEntry>,
  draft: Draft,
  handle: A,
  operation: string,
  kind: string,
): Result.Result<LocalEntry, LlvmError> => {
  const entry = entries.get(handle)
  if (entry === undefined) return fail(operation, `Unknown ${kind} handle`, handle)
  if (entry.draft !== draft) {
    return fail(operation, `The ${kind} handle belongs to a different function body`, handle)
  }
  return Result.succeed(entry)
}
