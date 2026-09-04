import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('rejects invalid replace places with assignment diagnostics', () =>
  Effect.gen(function* () {
    const immutable = yield* Analysis.ofSourceRealized(
      'place-replace/immutable',
      ascii(`pub fn main() -> i32 {
  let value = 1
  let old = Intrinsic.replace(value, 2)
  return old
}`),
    )
    assert.include(
      Analysis.diagnostics(immutable).map((diagnostic) => diagnostic.code),
      'SEM0035',
    )

    const sharedRoot = yield* Analysis.ofSourceRealized(
      'place-replace/shared',
      ascii(`struct Counter {
  value: i32
}
fn peek(self: &Counter) -> i32 {
  let old = Intrinsic.replace(self.value, 2)
  return old
}
pub fn main() -> i32 { return 0 }`),
    )
    assert.include(
      Analysis.diagnostics(sharedRoot).map((diagnostic) => diagnostic.code),
      'SEM0036',
    )

    const missingRoot = yield* Analysis.ofSourceRealized(
      'place-replace/missing',
      ascii(`pub fn main() -> i32 {
  let old = Intrinsic.replace(missing, 2)
  return old
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(missingRoot).map((diagnostic) => diagnostic.code),
      ['SEM0006'],
    )

    const arity = yield* Analysis.ofSourceRealized(
      'place-replace/arity',
      ascii(`pub fn main() -> i32 {
  let mut value = 1
  let old = Intrinsic.replace(value)
  return old
}`),
    )
    assert.include(
      Analysis.diagnostics(arity).map((diagnostic) => diagnostic.code),
      'SEM0007',
    )
  }),
)
