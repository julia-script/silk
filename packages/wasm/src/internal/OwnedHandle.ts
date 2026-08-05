import * as Result from 'effect/Result'
import { invalidState, type WasmError } from '../WasmError.js'

export interface Owner {
  readonly token: symbol
}

/** @internal */
export const makeOwner = (): Owner => Object.freeze({ token: Symbol('wasm-builder-owner') })

/** @internal */
export const ensureOwner = (
  expected: Owner,
  actual: Owner,
  operation: string,
): Result.Result<void, WasmError> =>
  expected.token === actual.token
    ? Result.succeed(undefined)
    : Result.fail(
        invalidState({
          operation,
          message: 'The handle belongs to a different WebAssembly builder',
          state: actual,
        }),
      )
