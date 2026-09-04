import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** A parametric Drop over a struct that is all-Copy at some instantiation. */
const copyInstantiation = `struct Holder<T> {
  value: T
}

impl<T: Copy> Copy for Holder<T> {}

impl<T> Drop for Holder<T> {
  fn drop(self: &mut Holder<T>) -> () { return () }
}

fn keep<T>(value: T) -> i32 {
  let held = Holder<T> { value: move value }
  return 1
}

pub fn main() -> i32 { return keep<i32>(41) + 1 }`

it.effect('rejects conflicting parametric Copy and Drop declarations', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'drop-hook/copy-instantiation',
      ascii(copyInstantiation),
      'wasm32-unknown-unknown',
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0083',
    )
  }),
)
