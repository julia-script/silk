import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import * as FileSourceResolver from '../src/FileSourceResolver.js'

it.effect('maps canonical modules exactly below the source root', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* fileSystem.makeDirectory(`${root}/compiler`)
    yield* fileSystem.writeFileString(`${root}/compiler/Syntax.silk`, 'source')
    const resolver = FileSourceResolver.layer(FileSourceResolver.make(root))

    const found = yield* SourceResolver.resolve('compiler/Syntax').pipe(Effect.provide(resolver))
    assert.strictEqual(Option.isSome(found), true)
    if (Option.isSome(found)) assert.strictEqual(new TextDecoder().decode(found.value), 'source')
    const absent = yield* SourceResolver.resolve('missing/Module').pipe(Effect.provide(resolver))
    assert.strictEqual(Option.isNone(absent), true)
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect('distinguishes not found from an operational read failure', () =>
  Effect.gen(function* () {
    const fileSystem = yield* FileSystem.FileSystem
    const root = yield* fileSystem.makeTempDirectoryScoped()
    yield* fileSystem.makeDirectory(`${root}/unreadable.silk`)
    const resolver = FileSourceResolver.layer(FileSourceResolver.make(root))
    const result = yield* Effect.result(
      SourceResolver.resolve('unreadable').pipe(Effect.provide(resolver)),
    )
    assert.strictEqual(Result.isFailure(result), true)
    if (Result.isFailure(result)) assert.strictEqual(result.failure.reason._tag, 'WrappedFailure')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
