import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('resolves exact immutable bytes from replaceable memory storage', () =>
  Effect.gen(function* () {
    const original = ascii('source')
    const layer = SourceResolver.memory(new Map([['compiler/Syntax', original]]))
    original[0] = 0
    const found = yield* SourceResolver.resolve('compiler/Syntax').pipe(Effect.provide(layer))
    assert.strictEqual(Option.isSome(found), true)
    if (Option.isSome(found)) assert.deepEqual(found.value, ascii('source'))
    const absent = yield* SourceResolver.resolve('compiler/syntax').pipe(Effect.provide(layer))
    assert.strictEqual(Option.isNone(absent), true)
  }),
)

it.effect('empty storage reports absence and invalid identities fail precisely', () =>
  Effect.gen(function* () {
    const absent = yield* SourceResolver.resolve('missing/Module').pipe(
      Effect.provide(SourceResolver.empty),
    )
    assert.strictEqual(Option.isNone(absent), true)
    const invalid = yield* Effect.result(
      SourceResolver.resolve('../escape').pipe(Effect.provide(SourceResolver.empty)),
    )
    assert.strictEqual(Result.isFailure(invalid), true)
    if (Result.isFailure(invalid)) {
      assert.strictEqual(invalid.failure.reason._tag, 'InvalidModuleIdentity')
    }
  }),
)
