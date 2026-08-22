import type * as OsFileSystemHost from './OsFileSystemHost.js'
import type * as StandardStreams from './StandardStreams.js'

/** Preserves an arbitrary stream-provider throw as observable evaluator data. */
export const writeFailure = (cause: unknown): StandardStreams.WriteResult =>
  Object.freeze({
    _tag: 'WriteFailure',
    message: 'standard stream provider threw',
    cause,
  })

/** Preserves an arbitrary filesystem-provider throw in the typed host failure channel. */
export const osFailure = (cause: unknown): OsFileSystemHost.Failure =>
  Object.freeze({ _tag: 'Failure', reason: 'Other', cause })
