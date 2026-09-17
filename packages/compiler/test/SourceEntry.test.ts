import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as SourceEntry from '../src/SourceEntry.js'

it('drops the extension to derive a canonical module identity', () => {
  assert.strictEqual(SourceEntry.identify('main.silk'), 'main')
})

it('requires the exact Silk source suffix', () => {
  assert.strictEqual(SourceEntry.identify('main'), undefined)
})

it('strips exactly one suffix and rejects remaining dots', () => {
  assert.strictEqual(SourceEntry.identify('main.test.silk'), undefined)
  assert.strictEqual(SourceEntry.identify('app/Main.silk'), 'app/Main')
})

it('rejects a name whose stem is not a canonical identity', () => {
  // Entry selection and closure loading reject the same malformed identities.
  assert.strictEqual(SourceEntry.identify('my module.silk'), undefined)
  assert.strictEqual(SourceEntry.identify('.silk'), undefined)
})

it.effect('selects a path and derives its module without a file read', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const directory = yield* fileSystem.makeTempDirectoryScoped()
    const file = `${directory}/main.silk`

    const entry = yield* SourceEntry.select(file)

    assert.strictEqual(entry.module, 'main')
    assert.strictEqual(entry.path, file)
    assert.strictEqual(entry.sourceRoot, directory)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('derives a nested entry identity from an explicit source root', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const directory = yield* fileSystem.makeTempDirectoryScoped()
    const nested = `${directory}/app`
    yield* fileSystem.makeDirectory(nested)
    const file = `${nested}/Main.silk`

    const entry = yield* SourceEntry.select(file, directory)
    assert.strictEqual(entry.module, 'app/Main')
    assert.strictEqual(entry.sourceRoot, directory)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('rejects an entry outside the selected source root', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const directory = yield* fileSystem.makeTempDirectoryScoped()
    const sourceRoot = `${directory}/src`
    yield* fileSystem.makeDirectory(sourceRoot)
    const file = `${directory}/Main.silk`

    const error = yield* Effect.flip(SourceEntry.select(file, sourceRoot))
    assert.strictEqual(error.reason._tag, 'OutsideSourceRoot')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('fails with a typed error when the file name yields no identity', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const directory = yield* fileSystem.makeTempDirectoryScoped()
    const file = `${directory}/not valid.silk`

    const error = yield* Effect.flip(SourceEntry.select(file))

    assert.strictEqual(error.reason._tag, 'InvalidIdentity')
    assert.strictEqual(error.operation, 'SourceEntry.select')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('selects a nonexistent path without requiring source access', () =>
  Effect.gen(function* () {
    const entry = yield* SourceEntry.select('/nonexistent/main.silk')
    assert.strictEqual(entry.module, 'main')
  }).pipe(Effect.provide(NodeServices.layer)),
)
