import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('reports the existing out-of-range diagnostic for a prefixed literal', () =>
  Effect.gen(function* () {
    const source = `fn accept(value: u8) -> u8 { return value }
pub fn main() -> i32 { let value = accept(0x1ff) return 42 }`
    const snapshot = yield* Analysis.ofSourceRealized('integer-base/out-of-range', ascii(source))
    const outOfRange = Analysis.expressionsOf(snapshot, 'integer-base/out-of-range').filter(
      (expression) => expression._tag === 'Integer' && expression.integer._tag === 'OutOfRange',
    )
    assert.strictEqual(outOfRange.length, 1)
    const reported = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.reason._tag === 'IntegerOutOfRange',
    )
    assert.strictEqual(reported.length, 1)
    assert.deepEqual(
      reported.map((diagnostic) =>
        diagnostic.reason._tag === 'IntegerOutOfRange' ? diagnostic.reason.spelling : undefined,
      ),
      ['0x1ff'],
    )
  }),
)

it.effect('rejects a base prefix without digits before parsing', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'integer-base/missing-digits',
      ascii('pub fn main() -> i32 { return 0x }'),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'LEX0004',
    )
  }),
)
