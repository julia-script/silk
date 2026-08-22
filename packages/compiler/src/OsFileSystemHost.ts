/** Stable compiler-owned low-level reasons written by native OS intrinsics. */
export type Reason =
  | 'NotFound'
  | 'AlreadyExists'
  | 'PermissionDenied'
  | 'InvalidPath'
  | 'WrongType'
  | 'NotEmpty'
  | 'NoSpace'
  | 'TooLarge'
  | 'BufferTooSmall'
  | 'Unsupported'
  | 'Other'

export const reasons: ReadonlyArray<Reason> = Object.freeze([
  'NotFound',
  'AlreadyExists',
  'PermissionDenied',
  'InvalidPath',
  'WrongType',
  'NotEmpty',
  'NoSpace',
  'TooLarge',
  'BufferTooSmall',
  'Unsupported',
  'Other',
])

export const reasonCode = (reason: Reason): number => reasons.indexOf(reason)

export interface Failure {
  readonly _tag: 'Failure'
  readonly reason: Reason
  readonly nativeCode?: number
  readonly cause?: unknown
}

export interface Handle {
  readonly identity: number
  readonly kind: 'File' | 'Directory'
}

export type OpenResult = { readonly _tag: 'Opened'; readonly handle: Handle } | Failure
export type TransferResult = { readonly _tag: 'Transferred'; readonly count: number } | Failure
export type CommandResult = { readonly _tag: 'Completed' } | Failure
export type InspectResult =
  | { readonly _tag: 'Inspected'; readonly kind: Handle['kind']; readonly byteLength: number }
  | Failure
/**
 * A unique-directory creation. The provider picks the suffix, so the created final component name
 * comes back rather than going in, and a caller with too small a buffer learns the exact capacity
 * it needs without anything having been created.
 */
export type DirectoryCreateUniqueResult =
  | { readonly _tag: 'Created'; readonly name: ReadonlyArray<number> }
  | { readonly _tag: 'BufferTooSmall'; readonly requiredCapacity: number }
  | Failure
export type DirectoryNextResult =
  | { readonly _tag: 'Entry'; readonly name: ReadonlyArray<number>; readonly kind: Handle['kind'] }
  | { readonly _tag: 'End' }
  | { readonly _tag: 'BufferTooSmall'; readonly requiredCapacity: number }
  | Failure

/** Explicit evaluator boundary for the unsafe native handle protocol. */
export interface Provider {
  readonly fileOpen: (
    root: ReadonlyArray<number>,
    path: ReadonlyArray<number>,
    mode: number,
  ) => OpenResult
  readonly fileRead: (
    handle: Handle,
    capacity: number,
  ) => { readonly _tag: 'Read'; readonly bytes: ReadonlyArray<number> } | Failure
  readonly fileWrite: (handle: Handle, input: ReadonlyArray<number>) => TransferResult
  readonly directoryOpen: (root: ReadonlyArray<number>, path: ReadonlyArray<number>) => OpenResult
  readonly directoryNext: (handle: Handle, capacity: number) => DirectoryNextResult
  readonly pathInspect: (root: ReadonlyArray<number>, path: ReadonlyArray<number>) => InspectResult
  readonly directoryCreate: (
    root: ReadonlyArray<number>,
    path: ReadonlyArray<number>,
  ) => CommandResult
  readonly directoryCreateUnique: (
    root: ReadonlyArray<number>,
    parent: ReadonlyArray<number>,
    prefix: ReadonlyArray<number>,
    capacity: number,
  ) => DirectoryCreateUniqueResult
  readonly fileRemove: (root: ReadonlyArray<number>, path: ReadonlyArray<number>) => CommandResult
  readonly directoryRemove: (
    root: ReadonlyArray<number>,
    path: ReadonlyArray<number>,
  ) => CommandResult
  readonly handleClose: (handle: Handle) => CommandResult
}
