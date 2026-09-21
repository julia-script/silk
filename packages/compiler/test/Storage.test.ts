import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Cause from 'effect/Cause'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Fiber from 'effect/Fiber'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import * as PlatformError from 'effect/PlatformError'
import * as Result from 'effect/Result'
import * as Storage from '../src/Storage.js'

const bytes = (...values: ReadonlyArray<number>): Uint8Array => Uint8Array.from(values)

const contract = Effect.fnUntraced(function* <R>(
  layer: Layer.Layer<Storage.Storage, never, R>,
): Effect.fn.Return<void, Storage.StorageError, R> {
  yield* Effect.gen(function* () {
    const read = (namespace: string, key: string, maximumBytes = 32) =>
      Storage.read(namespace, key, maximumBytes)
    const publish = (namespace: string, key: string, value: Uint8Array, maximumBytes = 32) =>
      Storage.publish(namespace, key, value, maximumBytes)

    assert.isTrue(Option.isNone(yield* read('first', 'missing')))
    const input = bytes(1, 2, 3)
    yield* publish('first', 'record', input)
    input[0] = 9
    const first = yield* read('first', 'record')
    assert.isTrue(Option.isSome(first))
    if (Option.isSome(first)) first.value[1] = 9
    assert.deepEqual(yield* read('first', 'record'), Option.some(bytes(1, 2, 3)))

    yield* publish('first', 'record', bytes(4, 5))
    yield* publish('second', 'record', bytes(6))
    yield* publish('first', 'other', bytes(7))
    assert.deepEqual(yield* read('first', 'record'), Option.some(bytes(4, 5)))
    assert.deepEqual(yield* read('second', 'record'), Option.some(bytes(6)))
    assert.deepEqual(yield* read('first', 'other'), Option.some(bytes(7)))

    const oversizeRead = yield* Effect.result(read('first', 'record', 1))
    assert.isTrue(Result.isFailure(oversizeRead))
    if (Result.isFailure(oversizeRead))
      assert.strictEqual(oversizeRead.failure.reason._tag, 'Oversize')
    const oversizePublish = yield* Effect.result(publish('first', 'large', bytes(1, 2), 1))
    assert.isTrue(Result.isFailure(oversizePublish))
    if (Result.isFailure(oversizePublish))
      assert.strictEqual(oversizePublish.failure.reason._tag, 'Oversize')
  }).pipe(Effect.provide(layer))
})

it.effect('satisfies the shared memory provider contract', () => contract(Storage.memory))

it.effect('satisfies the shared atomic-filesystem provider contract', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* contract(Storage.fileSystem(root))
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects invalid addresses and limits before provider work', () =>
  Effect.gen(function* () {
    for (const effect of [
      Storage.read('', 'key', 1),
      Storage.read('safe', '../key', 1),
      Storage.read('safe', 'key', 0),
      Storage.read('safe', 'key', 1.5),
    ]) {
      const result = yield* Effect.result(effect)
      assert.isTrue(Result.isFailure(result))
      if (Result.isFailure(result))
        assert.include(['InvalidAddress', 'InvalidLimit'], result.failure.reason._tag)
    }
  }).pipe(Effect.provide(Storage.memory)),
)

it.effect('preserves unexpected provider defects', () =>
  Effect.gen(function* () {
    const defect = Object.freeze({ injected: 'storage-defect' })
    const service = Storage.Storage.of({
      read: () => Effect.die(defect),
      publish: () => Effect.void,
    })
    const exit = yield* Effect.exit(
      Storage.read('safe', 'record', 4).pipe(Effect.provideService(Storage.Storage, service)),
    )
    assert.isTrue(Exit.isFailure(exit))
    if (Exit.isFailure(exit)) {
      assert.isTrue(Cause.hasDies(exit.cause))
      const found = Cause.findDefect(exit.cause)
      assert.isTrue(Result.isSuccess(found))
      if (Result.isSuccess(found)) assert.strictEqual(found.success, defect)
    }
  }),
)

it.effect('rejects filesystem records from metadata before reading their contents', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* fileSystem.makeDirectory(`${root}/bounded`)
    yield* fileSystem.writeFile(`${root}/bounded/record`, bytes(1, 2, 3, 4))
    const result = yield* Effect.result(
      Storage.read('bounded', 'record', 3).pipe(Effect.provide(Storage.fileSystem(root))),
    )
    assert.isTrue(Result.isFailure(result))
    if (Result.isFailure(result)) {
      assert.strictEqual(result.failure.reason._tag, 'Oversize')
      if (result.failure.reason._tag === 'Oversize')
        assert.strictEqual(result.failure.reason.actualBytes, 4n)
    }
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('reports external filesystem reads with logical resource context', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* fileSystem.makeDirectory(`${root}/failure/record`, { recursive: true })
    const result = yield* Effect.result(
      Storage.read('failure', 'record', 1024 * 1024).pipe(Effect.provide(Storage.fileSystem(root))),
    )
    assert.isTrue(Result.isFailure(result))
    if (Result.isFailure(result)) {
      assert.strictEqual(result.failure.reason._tag, 'ReadFailure')
      assert.strictEqual(result.failure.namespace, 'failure')
      assert.strictEqual(result.failure.key, 'record')
    }
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('leaves one complete winner after concurrent filesystem publication', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    const layer = Storage.fileSystem(root)
    const left = bytes(...Array.from({ length: 512 }, () => 0x11))
    const right = bytes(...Array.from({ length: 512 }, () => 0x22))
    yield* Effect.all(
      [
        Storage.publish('concurrent', 'record', left, 1024),
        Storage.publish('concurrent', 'record', right, 1024),
      ],
      { concurrency: 'unbounded' },
    ).pipe(Effect.provide(layer))
    const found = yield* Storage.read('concurrent', 'record', 1024).pipe(Effect.provide(layer))
    assert.isTrue(Option.isSome(found))
    if (Option.isSome(found))
      assert.isTrue(
        found.value.every((value) => value === 0x11) ||
          found.value.every((value) => value === 0x22),
      )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

const platformFailure = PlatformError.systemError({
  _tag: 'PermissionDenied',
  module: 'FileSystem',
  method: 'writeFile',
  description: 'injected write failure',
  pathOrDescriptor: '/storage/namespace/temp/record',
})

it.effect('cleans temporary state after publication failure without masking it', () =>
  Effect.gen(function* () {
    const path = yield* Path.Path
    let cleanups = 0
    const fileSystem = FileSystem.makeNoop({
      makeDirectory: () => Effect.void,
      makeTempFile: () => Effect.succeed('/storage/namespace/temp/record'),
      writeFile: () => Effect.fail(platformFailure),
      remove: () =>
        Effect.sync(() => {
          cleanups += 1
        }),
    })
    const service = yield* Storage.fileSystemService('/storage').pipe(
      Effect.provideService(FileSystem.FileSystem, fileSystem),
      Effect.provideService(Path.Path, path),
    )
    const result = yield* Effect.result(
      Storage.publish('namespace', 'record', bytes(1), 4).pipe(
        Effect.provideService(Storage.Storage, service),
      ),
    )
    assert.isTrue(Result.isFailure(result))
    if (Result.isFailure(result)) assert.strictEqual(result.failure.reason._tag, 'PublishFailure')
    assert.strictEqual(cleanups, 1)
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('cleans temporary state when publication is interrupted', () =>
  Effect.gen(function* () {
    const path = yield* Path.Path
    const writing = yield* Deferred.make<void>()
    let cleanups = 0
    const fileSystem = FileSystem.makeNoop({
      makeDirectory: () => Effect.void,
      makeTempFile: () => Effect.succeed('/storage/namespace/temp/record'),
      writeFile: () => Deferred.succeed(writing, undefined).pipe(Effect.andThen(Effect.never)),
      remove: () =>
        Effect.sync(() => {
          cleanups += 1
        }),
    })
    const service = yield* Storage.fileSystemService('/storage').pipe(
      Effect.provideService(FileSystem.FileSystem, fileSystem),
      Effect.provideService(Path.Path, path),
    )
    const fiber = yield* Storage.publish('namespace', 'record', bytes(1), 4).pipe(
      Effect.provideService(Storage.Storage, service),
      Effect.forkScoped,
    )
    yield* Deferred.await(writing)
    yield* Fiber.interrupt(fiber)
    assert.strictEqual(cleanups, 1)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
