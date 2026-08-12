/** The explicit byte-input boundary, separate from the write-only standard-streams boundary. */

/** One completed read observation retained by deterministic and in-memory providers. */
export interface ReadEvent {
  readonly _tag: 'HostRead'
  readonly capacity: number
  readonly bytes: ReadonlyArray<number>
}

/**
 * The typed provider result. A read commits between zero and `capacity` bytes; an empty commit is
 * the end of input rather than a failure, and a host error is a distinct typed failure.
 */
export type ReadResult =
  | { readonly _tag: 'Read'; readonly bytes: ReadonlyArray<number> }
  | { readonly _tag: 'ReadFailure'; readonly message: string }

/** The explicit host boundary used by evaluation. */
export interface Provider {
  readonly read: (capacity: number) => ReadResult
}

/** A replaceable provider plus its immutable event snapshot. */
export interface Memory {
  readonly _tag: 'StandardInputMemory'
  readonly provider: Provider
  readonly events: () => ReadonlyArray<ReadEvent>
}

/**
 * Builds an in-memory provider that replays `bytes` in at most `chunk` byte commits.
 *
 * `chunk` models a host that hands back fewer bytes than the caller asked for; `failAt` is a
 * zero-based attempted-read ordinal that reports a host error instead of data.
 */
export const memory = (
  bytes: ReadonlyArray<number> = [],
  options: { readonly chunk?: number; readonly failAt?: number } = {},
): Memory => {
  const source = Array.from(bytes)
  const chunk = options.chunk === undefined ? source.length : options.chunk
  if (!Number.isSafeInteger(chunk) || chunk < 0) {
    throw new RangeError('standard-input chunk must be a non-negative safe integer')
  }
  const recorded: Array<ReadEvent> = []
  let offset = 0
  let attempted = 0
  const provider: Provider = Object.freeze({
    read: (capacity: number) => {
      const ordinal = attempted
      attempted += 1
      if (ordinal === options.failAt) {
        return Object.freeze({
          _tag: 'ReadFailure',
          message: `standard input read ${ordinal} failed`,
        })
      }
      const length = Math.min(capacity, chunk, source.length - offset)
      const committed = Object.freeze(source.slice(offset, offset + length))
      offset += length
      recorded.push(Object.freeze({ _tag: 'HostRead', capacity, bytes: committed }))
      return Object.freeze({ _tag: 'Read', bytes: committed })
    },
  })
  return Object.freeze({
    _tag: 'StandardInputMemory',
    provider,
    events: () => Object.freeze(recorded.map((event) => event)),
  })
}
