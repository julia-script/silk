import * as Result from 'effect/Result'
import type * as Builder from '../Builder.js'
import { invalidState, type LlvmError } from '../LlvmError.js'
import type * as OwnedHandle from './OwnedHandle.js'

const HandleTypeId: unique symbol = Symbol.for('@silk-lang/llvm/internal/Handle')

export interface Handle<Tag extends string> {
  readonly _tag: Tag
  readonly [HandleTypeId]: typeof HandleTypeId
}

interface Entry extends OwnedHandle.OwnedHandle {
  readonly kind: string
}

const entries = new WeakMap<object, Entry>()

/** @internal */
export const make = <Tag extends string>(
  tag: Tag,
  owner: OwnedHandle.Owner,
  index: number,
): Handle<Tag> => {
  const handle: Handle<Tag> = { _tag: tag, [HandleTypeId]: HandleTypeId }
  Object.freeze(handle)
  entries.set(handle, { kind: tag, owner, index })
  return handle
}

/** @internal */
export const resolve = <Tag extends string>(
  builder: Builder.Builder,
  owner: OwnedHandle.Owner,
  handle: Handle<Tag>,
  tag: Tag,
  operation: string,
): Result.Result<number, LlvmError> => {
  const entry = entries.get(handle)
  if (entry === undefined || entry.kind !== tag) {
    return Result.fail(invalidState({ operation, message: `Unknown ${tag} handle`, state: handle }))
  }
  if (entry.owner.token !== owner.token) {
    return Result.fail(
      invalidState({
        operation,
        message: `The ${tag} handle belongs to a different LLVM builder`,
        state: { builder, handle },
      }),
    )
  }
  return Result.succeed(entry.index)
}
