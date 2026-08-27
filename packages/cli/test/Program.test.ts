import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Program from '../src/Program.js'

it.effect('forwards literal arguments and returns the exact child exit status', () =>
  Effect.gen(function* () {
    const status = yield* Program.run(process.execPath, [
      '-e',
      'process.exit(Number(process.argv[1]))',
      '42',
    ])
    assert.strictEqual(status, 42)
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('wraps process startup failures', () =>
  Effect.gen(function* () {
    const attempted = yield* Effect.result(Program.run('/silk-test/missing-executable'))
    assert.strictEqual(attempted._tag, 'Failure')
    if (attempted._tag === 'Failure') {
      assert.strictEqual(attempted.failure._tag, 'ProgramError')
      assert.strictEqual(attempted.failure.reason._tag, 'WrappedFailure')
    }
  }).pipe(Effect.provide(NodeServices.layer)),
)
