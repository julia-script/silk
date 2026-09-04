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
