import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import type * as PlatformError from 'effect/PlatformError'
import { ChildProcess, ChildProcessSpawner } from 'effect/unstable/process'

/** Starting or waiting for a compiled program failed before it could report an exit status. */
export class ProgramError extends Data.TaggedError('ProgramError')<{
  readonly operation: 'Program.run'
  readonly executable: string
  readonly message: string
  readonly reason: { readonly _tag: 'WrappedFailure'; readonly cause: PlatformError.PlatformError }
}> {}

/** Runs a compiled program with literal arguments and inherited standard streams. */
export const run = Effect.fn('Program.run')(function* (
  executable: string,
  arguments_: ReadonlyArray<string> = [],
): Effect.fn.Return<number, ProgramError, ChildProcessSpawner.ChildProcessSpawner> {
  const spawner = yield* ChildProcessSpawner.ChildProcessSpawner
  const command = ChildProcess.make(executable, arguments_, {
    stdin: 'inherit',
    stdout: 'inherit',
    stderr: 'inherit',
  })
  return yield* spawner.exitCode(command).pipe(
    Effect.map(Number),
    Effect.mapError(
      (cause) =>
        new ProgramError({
          operation: 'Program.run',
          executable,
          message: `Cannot run compiled program ${executable}`,
          reason: { _tag: 'WrappedFailure', cause },
        }),
    ),
  )
})
