import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Cause from 'effect/Cause'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as FileSystem from 'effect/FileSystem'
import * as PlatformError from 'effect/PlatformError'
import * as Result from 'effect/Result'
import { ChildProcessSpawner as ChildProcessSpawnerModule } from 'effect/unstable/process'
import * as Program from '../src/Program.js'
import * as TestExchange from '../src/TestExchange.js'

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

const nonce = Uint8Array.from({ length: 32 }, (_, index) => index)
const catalogDigest = Uint8Array.from({ length: 32 }, (_, index) => 255 - index)
const compactPlan: TestExchange.UncachedPlan = {
  _tag: 'Uncached',
  nonce,
  catalogDigest,
  discovered: 0n,
}

it.effect('uses private files for a complete receipt and removes the scratch directory', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const marker = `${root}/scratch-path`
    const planBytes = yield* TestExchange.encodePlan(compactPlan)
    const receiptBytes = yield* TestExchange.encodeReceipt({
      _tag: 'Uncached',
      nonce,
      planDigest: yield* TestExchange.planDigest(planBytes),
      counts: {
        discovered: 0n,
        selected: 0n,
        cached: 0n,
        executed: 0n,
        passed: 0n,
        failed: 0n,
      },
      status: 0,
    })
    const script = `
const fs = require('node:fs')
const path = require('node:path')
const args = process.argv.slice(1)
const result = args.at(args.indexOf('--silk-test-result') + 1)
fs.writeFileSync(result, Buffer.from(args[0], 'base64'))
fs.writeFileSync(args[1], path.dirname(result))
process.stdout.write('SLKTRCP1 forged stdout is not authority\\n')
`
    const outcome = yield* Program.runTest(process.execPath, compactPlan, [
      '-e',
      script,
      Buffer.from(receiptBytes).toString('base64'),
      marker,
    ])
    assert.strictEqual(outcome._tag, 'Completed')
    const scratch = yield* fileSystem.readFileString(marker)
    const found = yield* Effect.result(fileSystem.stat(scratch))
    assert.isTrue(Result.isFailure(found))
    if (Result.isFailure(found)) assert.strictEqual(found.failure.reason._tag, 'NotFound')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('treats a normal exit without a receipt as operational status 2', () =>
  Effect.gen(function* () {
    const outcome = yield* Program.runTest(process.execPath, compactPlan, [
      '-e',
      "process.stdout.write('summary: forged pass\\n')",
      'sentinel',
    ])
    assert.strictEqual(outcome._tag, 'InvalidReceipt')
    assert.strictEqual(outcome.status, 2)
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('preserves abnormal child termination without admitting a receipt', () =>
  Effect.gen(function* () {
    const outcome = yield* Effect.result(
      Program.runTest(process.execPath, compactPlan, [
        '-e',
        "process.kill(process.pid, 'SIGKILL')",
        'sentinel',
      ]),
    )
    assert.isTrue(Result.isFailure(outcome))
    if (Result.isFailure(outcome)) assert.strictEqual(outcome.failure._tag, 'ProgramError')
  }).pipe(Effect.provide(NodeServices.layer)),
)

const writeFailure = PlatformError.systemError({
  _tag: 'PermissionDenied',
  module: 'FileSystem',
  method: 'writeFile',
  description: 'injected write failure',
  pathOrDescriptor: '/scratch/plan.bin',
})

const cleanupFileSystem = (
  cleanup: () => void,
  write: FileSystem.FileSystem['writeFile'] = () => Effect.void,
): FileSystem.FileSystem =>
  FileSystem.makeNoop({
    makeTempDirectory: () => Effect.succeed('/scratch'),
    writeFile: write,
    remove: () => Effect.sync(cleanup),
  })

it.effect('removes scratch state after a typed setup failure and a defect', () =>
  Effect.gen(function* () {
    let typedCleanups = 0
    const typed = yield* Effect.result(
      Program.runTest('/program', compactPlan).pipe(
        Effect.provideService(
          FileSystem.FileSystem,
          cleanupFileSystem(
            () => {
              typedCleanups += 1
            },
            () => Effect.fail(writeFailure),
          ),
        ),
      ),
    )
    assert.isTrue(Result.isFailure(typed))
    assert.strictEqual(typedCleanups, 1)

    let defectCleanups = 0
    const defect = Object.freeze({ injected: 'program-defect' })
    const spawner = ChildProcessSpawnerModule.make(() => Effect.die(defect))
    const exit = yield* Effect.exit(
      Program.runTest('/program', compactPlan).pipe(
        Effect.provideService(
          FileSystem.FileSystem,
          cleanupFileSystem(() => {
            defectCleanups += 1
          }),
        ),
        Effect.provideService(ChildProcessSpawnerModule.ChildProcessSpawner, spawner),
      ),
    )
    assert.strictEqual(defectCleanups, 1)
    assert.isTrue(Exit.isFailure(exit))
    if (Exit.isFailure(exit)) {
      assert.isTrue(Cause.hasDies(exit.cause))
      const found = Cause.findDefect(exit.cause)
      assert.isTrue(Result.isSuccess(found))
      if (Result.isSuccess(found)) assert.strictEqual(found.success, defect)
    }
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('removes scratch state when process execution is interrupted', () =>
  Effect.gen(function* () {
    const started = yield* Deferred.make<void>()
    let cleanups = 0
    const spawner = ChildProcessSpawnerModule.make(() =>
      Deferred.succeed(started, undefined).pipe(Effect.andThen(Effect.never)),
    )
    const fiber = yield* Program.runTest('/program', compactPlan).pipe(
      Effect.provideService(
        FileSystem.FileSystem,
        cleanupFileSystem(() => {
          cleanups += 1
        }),
      ),
      Effect.provideService(ChildProcessSpawnerModule.ChildProcessSpawner, spawner),
      Effect.forkScoped,
    )
    yield* Deferred.await(started)
    yield* Fiber.interrupt(fiber)
    const exit = yield* Fiber.await(fiber)
    assert.strictEqual(cleanups, 1)
    assert.isTrue(Exit.isFailure(exit))
    if (Exit.isFailure(exit)) assert.isTrue(Cause.hasInterruptsOnly(exit.cause))
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
