import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const codesOf = (name: string, source: string) =>
  Effect.map(Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown'), (snapshot) =>
    Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
  )

// ISSUE-2: joining two named function items reports SEM0080 instead of invalid MIR.
it.effect('rejects a match that joins two named function items', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codesOf(
        'callable-stabilization/match-join-named',
        `fn inc(v: i32) -> i32 { return v + 1 }
fn dec(v: i32) -> i32 { return v - 1 }
struct A {}
struct B {}
fn pick(x: A | B) -> i32 {
  let f = match &x {
    A {} => inc
    B {} => dec
  }
  return f(41)
}
pub fn main() -> i32 { return pick(A {}) }`,
      ),
      ['SEM0080'],
    )
  }),
)

it.effect('keeps callable reassignment tied to one construction identity', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codesOf(
        'callable-stabilization/reassign-named',
        `fn inc(v: i32) -> i32 { return v + 1 }
fn dec(v: i32) -> i32 { return v - 1 }
pub fn main() -> i32 {
  let mut f = inc
  f = dec
  return f(43)
}`,
      ),
      ['SEM0080'],
    )
    assert.deepEqual(
      yield* codesOf(
        'callable-stabilization/reassign-anonymous',
        `pub fn main() -> i32 {
  let a = fn(v: i32) -> i32 { return v + 1 }
  let b = fn(v: i32) -> i32 { return v + 2 }
  let mut choice = a
  choice = b
  return choice(40)
}`,
      ),
      ['SEM0080'],
    )
  }),
)

it.effect('rejects a returned section whose borrow is rooted in a local', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codesOf(
        'callable-stabilization/escape-local-root',
        `fn read(value: i32, values: &mut [i32]) -> i32 { return value + values[0] }
fn make() -> mut fn(i32) -> i32 {
  let mut values = [0]
  return read(&mut values)
}
pub fn main() -> i32 { let mut f = make() return f(1) }`,
      ),
      ['OWN0018'],
    )
  }),
)
