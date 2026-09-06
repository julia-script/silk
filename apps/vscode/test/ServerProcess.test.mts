import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as ServerProcess from '../src/ServerProcess.mjs'

it.effect('observes normal child exit without transferring process ownership', () =>
  Effect.gen(function* () {
    const process = yield* ServerProcess.make({
      module: new URL('./fixtures/process-exits.mjs', import.meta.url).pathname,
    })
    const status = yield* process.awaitExit
    assert.strictEqual(status.code, 7)
    assert.strictEqual(status.signal, null)
    yield* process.terminate
  }),
)

it.effect('forcibly terminates a wedged child and acknowledges exit idempotently', () =>
  Effect.gen(function* () {
    const process = yield* ServerProcess.make({
      module: new URL('./fixtures/process-wedged.mjs', import.meta.url).pathname,
    })
    yield* process.terminate
    const status = yield* process.awaitExit
    assert.strictEqual(status.signal, 'SIGKILL')
    yield* process.terminate
  }),
)

it.effect('reports asynchronous spawn failure through the typed boundary', () =>
  Effect.gen(function* () {
    const result = yield* Effect.exit(
      ServerProcess.make({
        module: new URL('./fixtures/process-exits.mjs', import.meta.url).pathname,
        cwd: '/silklang-path-that-does-not-exist',
      }),
    )
    assert.isTrue(Exit.isFailure(result))
    if (Exit.isFailure(result)) assert.include(String(result.cause), 'ServerProcessError')
  }),
)
