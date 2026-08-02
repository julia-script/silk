import { SilkError } from '../SilkError.js'

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
): SilkError | undefined =>
  expected.token === handle.owner.token
    ? undefined
    : new SilkError({
        operation,
        message: 'The handle belongs to a different LLVM builder',
        cause: handle,
      })
