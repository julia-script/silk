import * as Result from 'effect/Result'
import type * as Limits from '../Limits.js'
import { invalidInput, type WasmError } from '../WasmError.js'
import type * as ModuleState from './ModuleState.js'

const isU32 = (value: number): boolean =>
  Number.isInteger(value) && value >= 0 && value <= 0xffffffff

/** @internal */
export const check = (
  limits: Limits.Limits,
  bound: number,
  operation: string,
): Result.Result<ModuleState.Limits, WasmError> => {
  if (!isU32(limits.min) || limits.min > bound) {
    return Result.fail(
      invalidInput({
        operation,
        message: `Limits minimum must be an integer between 0 and ${bound}`,
        input: limits,
      }),
    )
  }
  if (limits.max !== undefined) {
    if (!isU32(limits.max) || limits.max > bound) {
      return Result.fail(
        invalidInput({
          operation,
          message: `Limits maximum must be an integer between 0 and ${bound}`,
          input: limits,
        }),
      )
    }
    if (limits.max < limits.min) {
      return Result.fail(
        invalidInput({
          operation,
          message: 'Limits maximum must not be below the minimum',
          input: limits,
        }),
      )
    }
  }
  return Result.succeed({ min: limits.min, max: limits.max })
}
