import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * The #98 cleanup criterion, as reworded by the 2026-08-13 decision: it is about "an arm that
 * captures a resource and is never invoked". Under zero-arity arms no such arm exists to be
 * written, and this test is what discharges that claim rather than a leak assertion that cannot be
 * expressed.
 *
 * `bootstrap-callable-values` specifies that a section supplying "exactly parameters one through
 * the last" constructs "a unary callable awaiting parameter zero", so section construction always
 * leaves arity 1 and never 0; a named function has no environment at all. `hold(move clock)` is
 * therefore a full application producing an `Effect`, not a zero-arity callable holding the
 * `Clock`, and it is rejected against the arm's declared contract.
 */
it.effect('rejects a capturing value where a zero-arity arm is required', () =>
  Effect.gen(function* () {
    const source = `import silk.effect { Effect }
struct Clock { value: i32 }

effect fn hold(held: Clock) -> i32 { return held.value }

effect fn plain() -> i32 { return 5 }

pub fn main() -> i32 {
  let clock = Clock { value: 9 }
  return run Effect.ifThenElse(false, hold(move clock), plain)
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'if-then-else/capturing-arm',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0052'],
    )
  }),
)
