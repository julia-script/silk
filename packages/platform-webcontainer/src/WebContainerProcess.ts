import * as Effect from 'effect/Effect'
import type * as Scope from 'effect/Scope'
import * as Sink from 'effect/Sink'
import * as Stream from 'effect/Stream'
import * as WebContainerError from './WebContainerError.js'

/**
 * Positive terminal dimensions accepted by {@link resize}.
 *
 * @category models
 * @since 0.0.0
 */
export interface Dimensions {
  readonly cols: number
  readonly rows: number
}

/**
 * Effect-native view of a WebContainer process and its combined terminal I/O.
 *
 * **Gotchas**
 *
 * `output` is single-consumer because the browser `ReadableStream` has one reader lock. A nonzero
 * exit code remains a successful integer result.
 *
 * @category models
 * @since 0.0.0
 */
export interface Process {
  readonly exit: Effect.Effect<number, WebContainerError.WebContainerError>
  readonly output: Stream.Stream<string, WebContainerError.WebContainerError>
  readonly input: Sink.Sink<void, string, never, WebContainerError.WebContainerError>
  readonly kill: Effect.Effect<void, WebContainerError.WebContainerError>
  readonly resize: (
    dimensions: Dimensions,
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
}

/**
 * Effect-native fields accepted by {@link make}.
 *
 * @category models
 * @since 0.0.0
 */
export interface Options extends Omit<Process, 'resize'> {
  readonly resize: (
    dimensions: Dimensions,
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
}

/**
 * Browser stream fields accepted by {@link fromWebStreams}.
 *
 * @category models
 * @since 0.0.0
 */
export interface WebStreamOptions {
  readonly command: string
  readonly exit: Effect.Effect<number, WebContainerError.WebContainerError>
  readonly output: ReadableStream<string> | undefined
  readonly input: WritableStream<string>
  readonly kill: Effect.Effect<void, WebContainerError.WebContainerError>
  readonly resize: (
    dimensions: Dimensions,
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
}

/**
 * Active-state and cleanup capabilities acquired by {@link scoped}.
 *
 * @category resource management
 * @since 0.0.0
 */
export interface ScopedResource {
  readonly process: Process
  readonly running: Effect.Effect<boolean>
  readonly release: Effect.Effect<void>
}

const validDimension = (value: number): boolean => Number.isFinite(value) && value > 0
const inputForEach = Sink.forEach

/**
 * Creates a process value and adds terminal-dimension validation to its resize capability.
 *
 * @category constructors
 * @since 0.0.0
 */
export const make = (options: Options): Process => ({
  ...options,
  resize: Effect.fnUntraced(function* (dimensions) {
    if (!validDimension(dimensions.cols) || !validDimension(dimensions.rows)) {
      return yield* WebContainerError.invalidInput(
        'WebContainerProcess.resize',
        'Terminal dimensions must be positive finite numbers',
        `Received ${dimensions.cols} columns and ${dimensions.rows} rows`,
      )
    }
    return yield* options.resize(dimensions)
  }),
})

/**
 * Adapts browser terminal streams into Effect streams and sinks with deterministic lock cleanup.
 *
 * **Details**
 *
 * An absent output stream represents `SpawnOptions.output: false` and becomes an empty Effect
 * stream. The input writer is acquired for each sink run and released after every exit.
 *
 * @category converting
 * @since 0.0.0
 */
export const fromWebStreams = (options: WebStreamOptions): Process =>
  make({
    exit: options.exit,
    output:
      options.output === undefined
        ? Stream.empty
        : Stream.fromReadableStream({
            evaluate: () => options.output ?? new ReadableStream<string>(),
            releaseLockOnEnd: true,
            onError: (cause) =>
              WebContainerError.wrappedFailure(
                'WebContainerProcess.output',
                `Unable to read output from ${options.command}`,
                cause,
                { command: options.command },
              ),
          }),
    input: Sink.unwrap(
      Effect.acquireRelease(
        Effect.try({
          try: () => options.input.getWriter(),
          catch: (cause) =>
            WebContainerError.wrappedFailure(
              'WebContainerProcess.input.acquire',
              `Unable to acquire input for ${options.command}`,
              cause,
              { command: options.command },
            ),
        }),
        (writer) => Effect.sync(() => writer.releaseLock()),
      ).pipe(
        Effect.map((writer) =>
          inputForEach((chunk: string) =>
            Effect.tryPromise({
              try: () => writer.ready.then(() => writer.write(chunk)),
              catch: (cause) =>
                WebContainerError.wrappedFailure(
                  'WebContainerProcess.input',
                  `Unable to write input to ${options.command}`,
                  cause,
                  { command: options.command },
                ),
            }),
          ),
        ),
      ),
    ),
    kill: options.kill,
    resize: options.resize,
  })

/**
 * Acquires a process resource and releases it only while it remains active.
 *
 * **When to use**
 *
 * Use when implementing a scoped WebContainer process boundary. Application code normally uses
 * `WebContainer.spawn` instead.
 *
 * @category resource management
 * @since 0.0.0
 */
export const scoped = Effect.fn('WebContainerProcess.scoped')(function* (
  acquire: Effect.Effect<ScopedResource, WebContainerError.WebContainerError, Scope.Scope>,
): Effect.fn.Return<Process, WebContainerError.WebContainerError, Scope.Scope> {
  const resource = yield* Effect.acquireRelease(acquire, (current) =>
    current.running.pipe(Effect.flatMap((running) => (running ? current.release : Effect.void))),
  )
  return resource.process
})

/**
 * Awaits and returns a process exit code without reclassifying nonzero codes as failures.
 *
 * @category running
 * @since 0.0.0
 */
export const awaitExit = Effect.fn('WebContainerProcess.awaitExit')(function* (self: Process) {
  return yield* self.exit
})

/**
 * Requests process termination through the typed WebContainer boundary.
 *
 * @category destructors
 * @since 0.0.0
 */
export const kill = Effect.fn('WebContainerProcess.kill')(function* (self: Process) {
  return yield* self.kill
})

/**
 * Resizes a process terminal after validating positive finite dimensions.
 *
 * @category transforming
 * @since 0.0.0
 */
export const resize = Effect.fn('WebContainerProcess.resize')(function* (
  self: Process,
  dimensions: Dimensions,
) {
  return yield* self.resize(dimensions)
})
