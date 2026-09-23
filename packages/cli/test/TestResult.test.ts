import { NodeServices } from '@effect/platform-node'
import * as Storage from '@silklang/compiler/Storage'
import { assert, it } from '@effect/vitest'
import * as Cause from 'effect/Cause'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import * as TestResult from '../src/TestResult.js'

const identity = '11'.repeat(32)
const otherIdentity = '22'.repeat(32)

it.effect('reuses one exact completed pass from memory and never publishes failure data', () =>
  Effect.gen(function* () {
    assert.strictEqual((yield* TestResult.lookup(identity))._tag, 'Miss')
    assert.strictEqual((yield* TestResult.publishPass(identity))._tag, 'Published')
    assert.deepEqual(yield* TestResult.lookup(identity), { _tag: 'Hit', identity })
    assert.strictEqual((yield* TestResult.lookup(otherIdentity))._tag, 'Miss')
    assert.strictEqual((yield* TestResult.publishPass('not-an-execution-identity'))._tag, 'Skipped')
  }).pipe(Effect.provide(Layer.merge(Storage.memory, NodeServices.layer))),
)

it.effect('persists admitted passes across reconstructed filesystem services', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const first = TestResult.fileSystem(root)
    assert.strictEqual(
      (yield* TestResult.publishPass(identity).pipe(Effect.provide(first)))._tag,
      'Published',
    )
    const second = TestResult.fileSystem(root)
    assert.deepEqual(yield* TestResult.lookup(identity).pipe(Effect.provide(second)), {
      _tag: 'Hit',
      identity,
    })
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('treats corrupt, mismatched, and oversized records as misses', () =>
  Effect.gen(function* () {
    yield* Storage.publish(TestResult.namespace, identity, Uint8Array.from([1, 2, 3]), 4096)
    const corrupt = yield* TestResult.lookup(identity)
    assert.strictEqual(corrupt._tag, 'Miss')
    if (corrupt._tag === 'Miss') assert.strictEqual(corrupt.reason._tag, 'InvalidRecord')

    const encoded = yield* TestResult.encodePass(otherIdentity)
    assert.isTrue(Option.isSome(encoded))
    if (Option.isSome(encoded))
      yield* Storage.publish(TestResult.namespace, identity, encoded.value, 4096)
    const mismatched = yield* TestResult.lookup(identity)
    assert.strictEqual(mismatched._tag, 'Miss')
    if (mismatched._tag === 'Miss') assert.strictEqual(mismatched.reason._tag, 'InvalidRecord')

    yield* Storage.publish(
      TestResult.namespace,
      identity,
      new Uint8Array(TestResult.maximumBytes + 1),
      TestResult.maximumBytes + 1,
    )
    const oversized = yield* TestResult.lookup(identity)
    assert.strictEqual(oversized._tag, 'Miss')
    if (oversized._tag === 'Miss') assert.strictEqual(oversized.reason._tag, 'StorageFailure')
  }).pipe(Effect.provide(Layer.merge(Storage.memory, NodeServices.layer))),
)

const storageFailure = new Storage.StorageError({
  operation: 'Storage.read',
  namespace: TestResult.namespace,
  key: identity,
  message: 'injected read failure',
  reason: { _tag: 'ReadFailure', cause: 'injected' },
})

it.effect('degrades typed storage failures without swallowing defects', () =>
  Effect.gen(function* () {
    const failing = Storage.Storage.of({
      read: () => Effect.fail(storageFailure),
      publish: () =>
        Effect.fail(
          new Storage.StorageError({
            operation: 'Storage.publish',
            namespace: TestResult.namespace,
            key: identity,
            message: 'injected publication failure',
            reason: { _tag: 'PublishFailure', cause: 'injected' },
          }),
        ),
    })
    const lookup = yield* TestResult.lookup(identity).pipe(
      Effect.provideService(Storage.Storage, failing),
    )
    assert.strictEqual(lookup._tag, 'Miss')
    if (lookup._tag === 'Miss') assert.strictEqual(lookup.reason._tag, 'StorageFailure')
    const publication = yield* TestResult.publishPass(identity).pipe(
      Effect.provideService(Storage.Storage, failing),
    )
    assert.strictEqual(publication._tag, 'Skipped')

    const defect = Object.freeze({ injected: 'result-cache-defect' })
    const defective = Storage.Storage.of({
      read: () => Effect.die(defect),
      publish: () => Effect.void,
    })
    const exit = yield* Effect.exit(
      TestResult.lookup(identity).pipe(Effect.provideService(Storage.Storage, defective)),
    )
    assert.isTrue(Exit.isFailure(exit))
    if (Exit.isFailure(exit)) {
      assert.isTrue(Cause.hasDies(exit.cause))
      const found = Cause.findDefect(exit.cause)
      assert.isTrue(Result.isSuccess(found))
      if (Result.isSuccess(found)) assert.strictEqual(found.success, defect)
    }
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('preserves interruption while a cache provider is blocked', () =>
  Effect.gen(function* () {
    const started = yield* Deferred.make<void>()
    const blocked = Storage.Storage.of({
      read: () => Deferred.succeed(started, undefined).pipe(Effect.andThen(Effect.never)),
      publish: () => Effect.void,
    })
    const fiber = yield* TestResult.lookup(identity).pipe(
      Effect.provideService(Storage.Storage, blocked),
      Effect.forkScoped,
    )
    yield* Deferred.await(started)
    yield* Fiber.interrupt(fiber)
    const exit = yield* Fiber.await(fiber)
    assert.isTrue(Exit.isFailure(exit))
    if (Exit.isFailure(exit)) assert.isTrue(Cause.hasInterruptsOnly(exit.cause))
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it('keeps the record bound explicit', () => {
  assert.strictEqual(TestResult.maximumBytes, 4096)
})
