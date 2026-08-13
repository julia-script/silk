/** The explicit child-process boundary: one blocking execution from structured input to output. */
import type * as OsFileSystemHost from './OsFileSystemHost.js'

/**
 * One complete execution request. Every byte sequence is exact platform bytes rather than checked
 * text, so an argument or variable that is not UTF-8 reaches the child unchanged.
 */
export interface Request {
  /** The executable path bytes. The child's `argv[0]` is this same path. */
  readonly program: ReadonlyArray<number>
  /** The ordered arguments after `argv[0]`. */
  readonly arguments: ReadonlyArray<ReadonlyArray<number>>
  /** The exact environment. An empty list runs the child with no variables at all. */
  readonly environment: ReadonlyArray<ReadonlyArray<number>>
  /** The child's working directory, absent when the child inherits the caller's. */
  readonly workingDirectory?: ReadonlyArray<number>
}

/**
 * The typed provider result. A nonzero exit code is ordinary data; only a failure to start, to
 * wait, or to capture is a typed failure.
 */
export type ExecuteResult =
  | {
      readonly _tag: 'Exited'
      readonly code: number
      readonly output: ReadonlyArray<number>
      readonly errors: ReadonlyArray<number>
    }
  | {
      readonly _tag: 'Signaled'
      readonly signal: number
      readonly output: ReadonlyArray<number>
      readonly errors: ReadonlyArray<number>
    }
  | {
      readonly _tag: 'ExecuteFailure'
      readonly reason: OsFileSystemHost.Reason
      readonly nativeCode?: number
    }

/** The explicit host boundary used by evaluation. */
export interface Provider {
  readonly execute: (request: Request) => ExecuteResult
}

/** A replaceable provider plus the immutable requests it observed, in submission order. */
export interface Memory {
  readonly _tag: 'ChildProcessMemory'
  readonly provider: Provider
  readonly requests: () => ReadonlyArray<Request>
}

/**
 * Builds an in-memory provider that answers executions from `responses` in order and records every
 * request it received. An execution past the end of the script reports a typed failure rather than
 * inventing an outcome, so a test that runs one child too many fails as a missing script instead of
 * as a passing program.
 */
export const memory = (responses: ReadonlyArray<ExecuteResult> = []): Memory => {
  const script = Array.from(responses)
  const recorded: Array<Request> = []
  const provider: Provider = Object.freeze({
    execute: (request: Request) => {
      recorded.push(request)
      const selected = script.at(recorded.length - 1)
      return selected === undefined
        ? Object.freeze({ _tag: 'ExecuteFailure' as const, reason: 'Other' as const })
        : selected
    },
  })
  return Object.freeze({
    _tag: 'ChildProcessMemory',
    provider,
    requests: () => Object.freeze(recorded.map((request) => request)),
  })
}
