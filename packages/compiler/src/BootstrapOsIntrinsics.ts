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

/** Executes one standard-stream boundary call and preserves an arbitrary thrown cause. */
export const writeAll = (
  provider: StandardStreams.Provider,
  destination: StandardStreams.Destination,
  bytes: ReadonlyArray<number>,
): StandardStreams.WriteResult => {
  try {
    return provider.writeAll(destination, bytes)
  } catch (cause) {
    return writeFailure(cause)
  }
}

/** Executes one filesystem host operation and translates a JavaScript throw exactly once. */
export const invoke = <A>(run: () => A): A | OsFileSystemHost.Failure => {
  try {
    return run()
  } catch (cause) {
    return osFailure(cause)
  }
}
