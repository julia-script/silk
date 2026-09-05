import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

const snapshot = (name: string, source: string) =>
  Analysis.ofSource(`stabilization/${name}`, encoder.encode(source))

const diagnostics = Effect.fnUntraced(function* (name: string, source: string) {
  const self = yield* snapshot(name, source)
  return Analysis.diagnostics(self).map((diagnostic) => [
    diagnostic.code,
    diagnostic.span.start,
    diagnostic.span.end,
  ])
})

// ISSUE-50 — EFFECT-OWN-001: an Effect may not escape with a borrow of function-owned storage.
it.effect('rejects an Effect escaping with a borrow of a local owner', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* diagnostics(
        'effect-escape-local',
        `effect fn inspect(values: &[i32]) -> i32 { return values[0] }
fn make() -> Effect<'static; i32> {
  let v = [21]
  return inspect(&v)
}
pub fn main() -> i32 { return run make() }`,
      ),
      [
        ['OWN0019', 121, 133],
        ['OWN0018', 130, 132],
        ['SEM0212', 130, 132],
      ],
    )
    assert.deepEqual(
      yield* diagnostics(
        'effect-escape-temp-view',
        `fn identity(values: &[i32]) -> &[i32] { return values }
fn make() -> Effect<'static; i32> {
  let view = identity(&[1, 2])
  return effect { return view[0] }
}
pub fn main() -> i32 { let e = make() return run e }`,
      ),
      [
        ['SEM0212', 114, 121],
        ['OWN0019', 131, 157],
        ['OWN0018', 147, 152],
        ['OWN0019', 147, 152],
        ['OWN0019', 147, 155],
      ],
    )
    assert.deepEqual(
      yield* diagnostics(
        'effect-escape-bound',
        `effect fn inspect(values: &[i32]) -> i32 { return values[0] }
fn make() -> Effect<'static; i32> {
  let v = [21]
  let e = inspect(&v)
  return e
}
pub fn main() -> i32 { return run make() }`,
      ),
      [
        ['OWN0018', 131, 133],
        ['SEM0212', 131, 133],
        ['OWN0019', 143, 145],
      ],
    )
  }),
)

// ISSUE-103 — CONST-001: a constant has no address and cannot be borrowed.
it.effect('rejects borrowing a constant', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* diagnostics(
        'const-borrow',
        'const x: i32 = 1\nfn peek(v: &i32) -> i32 { return 5 }\npub fn main() -> i32 { return peek(&x) }',
      ),
      [['SEM0086', 89, 91]],
    )
    assert.deepEqual(
      yield* diagnostics(
        'const-mut-borrow',
        'const x: i32 = 1\nfn poke(v: &mut i32) -> i32 { return 5 }\npub fn main() -> i32 { return poke(&mut x) }',
      ),
      [['SEM0086', 93, 99]],
    )
  }),
)
