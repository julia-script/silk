import type * as Builder from '../Builder.js'
import { SilkError } from '../SilkError.js'
import type * as OwnedHandle from './OwnedHandle.js'

const HandleTypeId: unique symbol = Symbol.for('@silk-effect/llvm/internal/Handle')

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
): number => {
  const entry = entries.get(handle)
  if (entry === undefined || entry.kind !== tag) {
    throw new SilkError({ operation, message: `Unknown ${tag} handle`, cause: handle })
  }
  if (entry.owner.token !== owner.token) {
    throw new SilkError({
      operation,
      message: `The ${tag} handle belongs to a different LLVM builder`,
      cause: { builder, handle },
    })
  }
  return entry.index
}
