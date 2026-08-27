import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (text: string) =>
  Analysis.makeRealized({ root: SourceFile.make('root', ascii(text)) }).pipe(
    Effect.provide(SourceResolver.memory(new Map())),
  )

const codes = (self: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)

it.effect('surfaces disagreeing effect-block return types instead of last-return-wins', () =>
  Effect.gen(function* () {
    // Joinable-but-different return types form the canonical union join, so the block types as
    // Effect<bool | i32> and the surrounding i32 context rejects it — never a silent adoption of
    // the lexically last return's type. (A join with no union form reports SEM0163 directly.)
    const self = yield* analyze(`pub fn main() -> i32 {
  let flag = true
  let deferred = effect {
    if flag { return true }
    return 1
  }
  return run deferred
}`)
    assert.include(codes(self), 'SEM0040')
  }),
)

it.effect('joins compatible effect-block returns across branches without diagnostics', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`pub fn main() -> i32 {
  let flag = false
  let deferred = effect {
    if flag { return 1 }
    return 42
  }
  return run deferred
}`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('keeps a failure raised inside an unsafe block in the effect failure row', () =>
  Effect.gen(function* () {
    // Pre-fix, terminals under `unsafe { }` were invisible to effect-block typing, so this run
    // site was accepted with an empty failure row and the failure escaped unchecked.
    const self = yield* analyze(`struct Boom { code: i32 }
pub fn main() -> i32 {
  let flag = true
  let deferred = effect {
    if flag { unsafe { fail Boom { code: 1 } } }
    return 42
  }
  return run deferred
}`)
    assert.include(codes(self), 'SEM0066')
  }),
)

it.effect('collects the success type from a return nested inside an unsafe block', () =>
  Effect.gen(function* () {
    const self = yield* analyze(`pub fn main() -> i32 {
  let deferred = effect { unsafe { return 42 } }
  return run deferred
}`)
    assert.deepEqual(codes(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('keeps a generic value-parameter failure in the effect failure row', () =>
  Effect.gen(function* () {
    // Pre-fix the nominal-only filter dropped the type-parameter failure, so the specialized
    // failure escaped the run site's unhandled-failure check.
    const self = yield* analyze(`struct Boom { code: i32 }
fn wrap<E>(flag: bool, problem: E) -> i32 {
  let deferred = effect {
    if flag { fail move problem }
    return 1
  }
  return run deferred
}
pub fn main() -> i32 { return wrap<Boom>(true, Boom { code: 7 }) }`)
    assert.include(codes(self), 'SEM0066')
  }),
)
