import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('rejects a misplaced separator before parsing', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'digit-separator/trailing',
      ascii('pub fn main() -> i32 { return 1_ }'),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'LEX0005',
    )
    assert.strictEqual(
      Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.code === 'LEX0005').length,
      1,
    )
  }),
)
