import * as Effect from 'effect/Effect'
import * as Stream from 'effect/Stream'
import { ChildProcess } from 'effect/unstable/process'

/** The captured result of one scoped test process. */
export interface Result {
  readonly exitCode: number
  readonly stdout: string
  readonly stderr: string
}

const collectText = Stream.runFold(
  () => '',
  (text: string, chunk: string) => text + chunk,
)

/** Runs one command while concurrently draining both output streams. */
export const run = Effect.fnUntraced(function* (command: string, args: ReadonlyArray<string>) {
  const process = yield* ChildProcess.make(command, args, { stdin: 'ignore' })
  const [exitCode, stdout, stderr] = yield* Effect.all(
    [
      process.exitCode,
      process.stdout.pipe(Stream.decodeText(), collectText),
      process.stderr.pipe(Stream.decodeText(), collectText),
    ],
    { concurrency: 'unbounded' },
  )
  return Object.freeze({ exitCode, stdout, stderr })
})
