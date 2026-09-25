import * as Result from 'effect/Result'
import { invalidState, type LlvmError } from '../LlvmError.js'

/**
 * A description/handle/key triple interning a canonical key to one description and handle.
 *
 * @internal
 */
export interface Table<D, H> {
  descriptions: Array<D>
  handles: Array<H>
  keys: Map<string, number>
}

/** @internal */
export const make = <D, H>(): Table<D, H> => ({ descriptions: [], handles: [], keys: new Map() })

/** @internal */
export const handleAt = <D, H>(
  table: Table<D, H>,
  index: number,
  operation: string,
  kind: string,
): Result.Result<H, LlvmError> => {
  const handle = table.handles[index]
  if (handle === undefined) {
    return Result.fail(
      invalidState({ operation, message: `${kind} table handle is missing`, state: index }),
    )
  }
  return Result.succeed(handle)
}

/** @internal */
export const descriptionAt = <D, H>(
  table: Table<D, H>,
  index: number,
  operation: string,
  kind: string,
): Result.Result<D, LlvmError> => {
  const description = table.descriptions[index]
  if (description === undefined) {
    return Result.fail(
      invalidState({ operation, message: `${kind} table entry is missing`, state: index }),
    )
  }
  return Result.succeed(description)
}

/** @internal */
export const find = <D, H>(table: Table<D, H>, key: string): number | undefined =>
  table.keys.get(key)

/**
 * Interns a description under its canonical key and returns the table handle. Every constant,
 * type, and attribute request of native emission lands here, so it returns the handle directly.
 *
 * @internal
 */
export const intern = <D, H>(
  table: Table<D, H>,
  operation: string,
  kind: string,
  key: string,
  description: D,
  makeHandle: (index: number) => H,
): Result.Result<H, LlvmError> => {
  const found = table.keys.get(key)
  if (found !== undefined) return handleAt(table, found, operation, kind)
  const index = table.descriptions.length
  const handle = makeHandle(index)
  table.descriptions.push(description)
  table.handles.push(handle)
  table.keys.set(key, index)
  return Result.succeed(handle)
}

/** @internal */
export const freeze = <D, H>(
  table: Table<D, H>,
): { readonly descriptions: ReadonlyArray<D>; readonly handles: ReadonlyArray<H> } => ({
  descriptions: [...table.descriptions],
  handles: [...table.handles],
})
