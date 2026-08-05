import * as Result from 'effect/Result'
import type * as Limits from '../Limits.js'
import { invalidInput, type WasmError } from '../WasmError.js'
import type * as ModuleState from './ModuleState.js'

/** The page bound of a 32-bit memory (2^16) and a 64-bit memory (2^48). */
const memoryBounds = { i32: 65536n, i64: 2n ** 48n }
/** The element bound of a 32-bit table and a 64-bit table. */
const tableBounds = { i32: 0xffffffffn, i64: 2n ** 64n - 1n }

const normalize = (value: number | bigint): bigint | undefined => {
  if (typeof value === 'bigint') return value >= 0n ? value : undefined
  return Number.isInteger(value) && value >= 0 ? BigInt(value) : undefined
}

/** @internal */
export const check = (
  limits: Limits.Limits,
  entity: 'memory' | 'table',
  addressType: ModuleState.AddressType,
  operation: string,
): Result.Result<ModuleState.Limits, WasmError> => {
  const bound = entity === 'memory' ? memoryBounds[addressType] : tableBounds[addressType]
  const min = normalize(limits.min)
  if (min === undefined || min > bound) {
    return Result.fail(
      invalidInput({
        operation,
        message: `Limits minimum must be an integer between 0 and ${bound}`,
        input: limits,
      }),
    )
  }
  if (limits.max !== undefined) {
    const max = normalize(limits.max)
    if (max === undefined || max > bound) {
      return Result.fail(
        invalidInput({
          operation,
          message: `Limits maximum must be an integer between 0 and ${bound}`,
          input: limits,
        }),
      )
    }
    if (max < min) {
      return Result.fail(
        invalidInput({
          operation,
          message: 'Limits maximum must not be below the minimum',
          input: limits,
        }),
      )
    }
    return Result.succeed({ min, max })
  }
  return Result.succeed({ min, max: undefined })
}
