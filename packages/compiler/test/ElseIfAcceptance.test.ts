import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('reports a chained arm whose condition is not a boolean', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'else-if/condition-type',
      ascii(`pub fn main() -> i32 {
  if false {
    return 1
  } else if 2 {
    return 2
  }
  return 0
}`),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0011',
    )
  }),
)
