import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

it.effect('rejects a raw literal whose body is not valid UTF-8', () =>
  Effect.gen(function* () {
    const bytes = Uint8Array.from([
      ...encoder.encode('pub fn main() -> i32 { let value = r"'),
      0xff,
      ...encoder.encode('" return 0 }'),
    ])
    const snapshot = yield* Analysis.ofSourceRealized('raw-string/invalid-utf8', bytes)
    assert.isAbove(Analysis.diagnostics(snapshot).length, 0)
  }),
)
