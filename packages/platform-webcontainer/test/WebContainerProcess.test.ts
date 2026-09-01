import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as Sink from 'effect/Sink'
import * as Stream from 'effect/Stream'
import * as WebContainerProcess from '../src/WebContainerProcess.js'

it.effect('preserves combined output order and releases the reader lock', () =>
  Effect.gen(function* () {
    let cancelled = 0
    const output = new ReadableStream<string>({
      start(controller) {
        controller.enqueue('stdout\n')
        controller.enqueue('stderr\n')
        controller.close()
      },
      cancel() {
        cancelled += 1
      },
    })
    const process = WebContainerProcess.fromWebStreams({
      command: 'demo',
      exit: Effect.succeed(0),
      output,
      input: new WritableStream<string>(),
      kill: Effect.void,
      resize: () => Effect.void,
    })
    const chunks = yield* Stream.runCollect(process.output)
    assert.deepStrictEqual(chunks, ['stdout\n', 'stderr\n'])
    assert.isFalse(output.locked)
    assert.strictEqual(cancelled, 0)
  }),
)

it.effect('makes disabled output empty', () =>
  Effect.gen(function* () {
    const process = WebContainerProcess.fromWebStreams({
      command: 'quiet',
      exit: Effect.succeed(9),
      output: undefined,
      input: new WritableStream<string>(),
      kill: Effect.void,
      resize: () => Effect.void,
    })
    const chunks = yield* Stream.runCollect(process.output)
    const exit = yield* WebContainerProcess.awaitExit(process)
    assert.deepStrictEqual(chunks, [])
    assert.strictEqual(exit, 9)
  }),
)

it.effect('writes terminal input in order and releases the writer lock', () =>
  Effect.gen(function* () {
    const chunks: Array<string> = []
    const input = new WritableStream<string>({
      write(chunk) {
        chunks.push(chunk)
      },
    })
    const process = WebContainerProcess.fromWebStreams({
      command: 'repl',
      exit: Effect.succeed(0),
      output: undefined,
      input,
      kill: Effect.void,
      resize: () => Effect.void,
    })
    yield* Stream.make('first', 'second').pipe(Stream.run(process.input))
    assert.deepStrictEqual(chunks, ['first', 'second'])
    assert.isFalse(input.locked)
  }),
)

it.effect('releases reader and writer locks after interruption', () =>
  Effect.gen(function* () {
    const output = new ReadableStream<string>({
      pull() {
        return Promise.withResolvers<void>().promise
      },
    })
    const input = new WritableStream<string>({
      write() {
        return Promise.withResolvers<void>().promise
      },
    })
    const process = WebContainerProcess.fromWebStreams({
      command: 'blocked',
      exit: Effect.never,
      output,
      input,
      kill: Effect.void,
      resize: () => Effect.void,
    })
    const reader = yield* process.output.pipe(Stream.runDrain, Effect.forkChild)
    const writer = yield* Stream.make('blocked').pipe(Stream.run(process.input), Effect.forkChild)
    yield* Effect.yieldNow
    assert.isTrue(output.locked)
    assert.isTrue(input.locked)
    yield* Fiber.interrupt(reader)
    yield* Fiber.interrupt(writer)
    assert.isFalse(output.locked)
    assert.isFalse(input.locked)
  }),
)

it.effect('releases only processes that are still running', () =>
  Effect.gen(function* () {
    let releases = 0
    const process = WebContainerProcess.make({
      exit: Effect.never,
      output: Stream.empty,
      input: Sink.drain,
      kill: Effect.void,
      resize: () => Effect.void,
    })
    yield* Effect.scoped(
      WebContainerProcess.scoped(
        Effect.succeed({
          process,
          running: Effect.succeed(true),
          release: Effect.sync(() => {
            releases += 1
          }),
        }),
      ),
    )
    yield* Effect.scoped(
      WebContainerProcess.scoped(
        Effect.succeed({
          process,
          running: Effect.succeed(false),
          release: Effect.sync(() => {
            releases += 1
          }),
        }),
      ),
    )
    assert.strictEqual(releases, 1)
  }),
)

it.effect('validates dimensions before invoking resize and exposes kill', () =>
  Effect.gen(function* () {
    let kills = 0
    let resizes = 0
    const process = WebContainerProcess.make({
      exit: Effect.succeed(0),
      output: Stream.empty,
      input: Sink.drain,
      kill: Effect.sync(() => {
        kills += 1
      }),
      resize: () =>
        Effect.sync(() => {
          resizes += 1
        }),
    })
    const invalid = yield* Effect.result(WebContainerProcess.resize(process, { cols: 0, rows: 20 }))
    yield* WebContainerProcess.resize(process, { cols: 80, rows: 24 })
    yield* WebContainerProcess.kill(process)
    assert.strictEqual(invalid._tag, 'Failure')
    if (invalid._tag === 'Failure') {
      assert.strictEqual(invalid.failure.reason._tag, 'InvalidInput')
    }
    assert.strictEqual(resizes, 1)
    assert.strictEqual(kills, 1)
  }),
)
