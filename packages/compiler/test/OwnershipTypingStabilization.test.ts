import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

const snapshot = (name: string, source: string, target = 'wasm32-unknown-unknown') =>
  Analysis.ofSourceRealized(`stabilization/${name}`, encoder.encode(source), target)

const codes = Effect.fnUntraced(function* (name: string, source: string, target?: string) {
  const self = yield* snapshot(name, source, target)
  return Analysis.diagnostics(self).map((diagnostic) => diagnostic.code)
})

// ISSUE-50 — EFFECT-OWN-001: an Effect may not escape with a borrow of function-owned storage.
it.effect('rejects an Effect escaping with a borrow of a local owner', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codes(
        'effect-escape-local',
        `import silk.effect { Effect }
effect fn inspect(values: &[i32]) -> i32 { return values[0] }
fn make() -> Effect<i32> {
  let v = [21]
  return inspect(&v)
}
pub fn main() -> i32 { return run make() }`,
      ),
      ['OWN0018'],
    )
    assert.deepEqual(
      yield* codes(
        'effect-escape-temp-view',
        `import silk.effect { Effect }
fn identity(values: &[i32]) -> &[i32] { return values }
fn make() -> Effect<i32> {
  let view = identity(&[1, 2])
  return effect { return view[0] }
}
pub fn main() -> i32 { let e = make() return run e }`,
      ),
      ['OWN0018'],
    )
    assert.deepEqual(
      yield* codes(
        'effect-escape-bound',
        `import silk.effect { Effect }
effect fn inspect(values: &[i32]) -> i32 { return values[0] }
fn make() -> Effect<i32> {
  let v = [21]
  let e = inspect(&v)
  return e
}
pub fn main() -> i32 { return run make() }`,
      ),
      ['OWN0018'],
    )
  }),
)

// ISSUE-103 — CONST-001: a constant has no address and cannot be borrowed.
it.effect('rejects borrowing a constant', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codes(
        'const-borrow',
        'const x: i32 = 1\nfn peek(v: &i32) -> i32 { return 5 }\npub fn main() -> i32 { return peek(&x) }',
      ),
      ['SEM0086'],
    )
    assert.deepEqual(
      yield* codes(
        'const-mut-borrow',
        'const x: i32 = 1\nfn poke(v: &mut i32) -> i32 { return 5 }\npub fn main() -> i32 { return poke(&mut x) }',
      ),
      ['SEM0086'],
    )
  }),
)
