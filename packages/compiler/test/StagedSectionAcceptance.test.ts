import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('still rejects supplying more arguments than a section awaits', () =>
  Effect.gen(function* () {
    const source = `fn combine(a: i32, b: i32, c: i32) -> i32 { return a + b + c }
pub fn main() -> i32 {
  let withThree = combine(3)
  return withThree(1, 2, 4)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'staged-section/too-many',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0007'],
    )
  }),
)
