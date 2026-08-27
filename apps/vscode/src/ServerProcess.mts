import { spawn } from 'node:child_process'
import * as Data from 'effect/Data'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'

export interface ExitStatus {
  readonly code: number | null
  readonly signal: NodeJS.Signals | null
}

export class ServerProcessError extends Data.TaggedError('ServerProcessError')<{
  readonly operation: 'spawn' | 'terminate'
  readonly message: string
  readonly cause?: unknown
}> {}

/** Detached protocol streams plus exclusive, acknowledged ownership of the child process. */
export interface ServerProcess {
  readonly _tag: 'ServerProcess'
  readonly reader: NodeJS.ReadableStream
  readonly writer: NodeJS.WritableStream
  readonly awaitExit: Effect.Effect<ExitStatus>
  readonly terminate: Effect.Effect<void, ServerProcessError>
}

/** Spawns the stdio language server without transferring process ownership to languageclient. */
export const make = Effect.fn('ServerProcess.make')(function* (options: {
  readonly module: string
  readonly cwd?: string
  readonly env?: NodeJS.ProcessEnv
  readonly onStderr?: (text: string) => void
}): Effect.fn.Return<ServerProcess, ServerProcessError> {
  const child = yield* Effect.try({
    try: () =>
      spawn(process.execPath, [options.module], {
        stdio: ['pipe', 'pipe', 'pipe'],
        ...(options.cwd === undefined ? {} : { cwd: options.cwd }),
        ...(options.env === undefined ? {} : { env: options.env }),
      }),
    catch: (cause) =>
      new ServerProcessError({
        operation: 'spawn',
        message: 'Could not spawn the Silk language server',
        cause,
      }),
  })
  const spawned = Deferred.makeUnsafe<void, ServerProcessError>()
  const exited = Deferred.makeUnsafe<ExitStatus>()
  child.once('spawn', () => {
    Deferred.doneUnsafe(spawned, Effect.succeed(undefined))
  })
  child.once('exit', (code, signal) => {
    Deferred.doneUnsafe(exited, Effect.succeed(Object.freeze({ code, signal })))
  })
  child.once('error', (cause) => {
    Deferred.doneUnsafe(
      spawned,
      Effect.fail(
        new ServerProcessError({
          operation: 'spawn',
          message: 'Could not spawn the Silk language server',
          cause,
        }),
      ),
    )
    Deferred.doneUnsafe(exited, Effect.succeed(Object.freeze({ code: null, signal: null })))
  })
  if (child.stdin === null || child.stdout === null) {
    child.kill('SIGKILL')
    return yield* new ServerProcessError({
      operation: 'spawn',
      message: 'The Silk language server did not expose stdio streams',
    })
  }
  yield* Deferred.await(spawned)
  if (options.onStderr !== undefined)
    child.stderr?.on('data', (chunk: Buffer | string) => {
      options.onStderr?.(String(chunk))
    })
  let terminationRequested = false
  const terminate = Effect.fn('ServerProcess.terminate')(function* () {
    if (yield* Deferred.isDone(exited)) return
    if (!terminationRequested) {
      terminationRequested = true
      yield* Effect.try({
        try: () => {
          child.kill('SIGKILL')
        },
        catch: (cause) =>
          new ServerProcessError({
            operation: 'terminate',
            message: 'Could not terminate the Silk language server',
            cause,
          }),
      })
    }
    yield* Deferred.await(exited)
  })
  return Object.freeze({
    _tag: 'ServerProcess',
    reader: child.stdout,
    writer: child.stdin,
    awaitExit: Deferred.await(exited),
    terminate: terminate(),
  })
})
