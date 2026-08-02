import * as Result from 'effect/Result'
import { invalidState, type LlvmError } from '../LlvmError.js'

export interface Owner {
  readonly token: symbol
}

export interface OwnedHandle {
  readonly owner: Owner
  readonly index: number
}

/** @internal */
export const makeOwner = (): Owner => Object.freeze({ token: Symbol('llvm-builder-owner') })

/** @internal */
export const make = (owner: Owner, index: number): OwnedHandle => Object.freeze({ owner, index })

/** @internal */
export const ensureOwner = (
  expected: Owner,
  handle: OwnedHandle,
  operation: string,
): Result.Result<void, LlvmError> =>
  expected.token === handle.owner.token
    ? Result.succeed(undefined)
    : Result.fail(
        invalidState({
          operation,
          message: 'The handle belongs to a different LLVM builder',
          state: handle,
        }),
      )
