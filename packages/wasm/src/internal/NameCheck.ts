import * as Result from 'effect/Result'
import { invalidInput, type WasmError } from '../WasmError.js'
import * as Utf8 from './Utf8.js'

/**
 * Rejects a declaration name whose canonical UTF-8 identity is already used inside its index
 * space. Text identifiers must be unique per space, so uniqueness is enforced when the name is
 * declared rather than surfacing as an emission failure later.
 *
 * @internal
 */
export const ensureFresh = (
  entries: ReadonlyArray<{ readonly name: string | undefined }>,
  name: string | undefined,
  operation: string,
): Result.Result<void, WasmError> => {
  const key = name === undefined ? undefined : Utf8.canonicalKey(name)
  if (
    key !== undefined &&
    entries.some((entry) => entry.name !== undefined && Utf8.canonicalKey(entry.name) === key)
  ) {
    return Result.fail(
      invalidInput({
        operation,
        message: `The name ${JSON.stringify(name)} is already used in this index space`,
        input: name,
      }),
    )
  }
  return Result.succeed(undefined)
}
