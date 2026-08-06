import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as SourceEntry from '../src/SourceEntry.js'

it('drops the extension to derive a canonical module identity', () => {
  assert.strictEqual(SourceEntry.identify('main.silk'), 'main')
})

it('accepts a bare name with no extension', () => {
  assert.strictEqual(SourceEntry.identify('main'), 'main')
})

it('drops every extension so a compound suffix still yields one identity', () => {
  assert.strictEqual(SourceEntry.identify('main.test.silk'), 'main')
})

it('rejects a name whose stem is not a canonical identity', () => {
  // ModuleClosure throws a RangeError on such identities; rejecting here keeps it a typed error.
  assert.strictEqual(SourceEntry.identify('my module.silk'), undefined)
  assert.strictEqual(SourceEntry.identify('.silk'), undefined)
})

it.effect('reads a file and pairs its bytes with the derived module identity', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const directory = yield* fileSystem.makeTempDirectoryScoped()
    const file = `${directory}/main.silk`
    yield* fileSystem.writeFileString(file, 'pub fn main() -> I32 { return 42 }')

    const entry = yield* SourceEntry.read(file)

    assert.strictEqual(entry.module, 'main')
    assert.strictEqual(entry.path, file)
    assert.strictEqual(new TextDecoder().decode(entry.bytes), 'pub fn main() -> I32 { return 42 }')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('fails with a typed error when the file name yields no identity', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const directory = yield* fileSystem.makeTempDirectoryScoped()
    const file = `${directory}/not valid.silk`
    yield* fileSystem.writeFileString(file, 'pub fn main() -> I32 { return 42 }')

    const error = yield* Effect.flip(SourceEntry.read(file))

    assert.strictEqual(error.reason._tag, 'InvalidIdentity')
    assert.strictEqual(error.operation, 'SourceEntry.identify')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('fails with a wrapped error when the file cannot be read', () =>
  Effect.gen(function* () {
    const error = yield* Effect.flip(SourceEntry.read('/nonexistent/main.silk'))

    assert.strictEqual(error.reason._tag, 'WrappedFailure')
    assert.strictEqual(error.operation, 'SourceEntry.read')
  }).pipe(Effect.provide(NodeServices.layer)),
)
