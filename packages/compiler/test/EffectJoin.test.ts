import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nonFiniteSource = `struct First {}
struct Second {}
fn choose<F: Effect<i32>>(input: First | Second, operation: F) -> Effect<i32> {
  return match move input {
    First {} => operation
    Second {} => effect { return 42 }
  }
}`

const divergingArmSource = `struct First {}
struct Second {}
fn diverge() -> never { return diverge() }
fn choose(input: First | Second) -> Effect<i32> {
  return match move input {
    First {} => effect { return 42 }
    Second {} => diverge()
  }
}
pub fn main() -> i32 {
  return run choose(First {})
}`

const snapshotOf = (name: string, text: string) =>
  Analysis.ofSourceRealized(name, ascii(text), 'wasm32-unknown-unknown')

it.effect('diagnoses a join whose representation is not a closed finite set', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/non-finite', nonFiniteSource)
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0132'],
    )
  }),
)

it.effect('accepts a join between an Effect arm and a diverging never arm', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf('effect-join/diverging-arm', divergingArmSource)
    assert.notInclude(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0132',
    )
  }),
)
