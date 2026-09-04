import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const requirements = `import silk.effect { Effect }
service Alpha { effect fn alpha() -> i32 ? &Alpha }
service Beta { effect fn beta() -> i32 ? &Beta }
struct Fixed { value: i32 }
effect fn alpha(self: &Fixed) -> i32 { return self.value }
effect fn beta(self: &Fixed) -> i32 { return self.value }
impl Alpha for Fixed { alpha: Fixed.alpha }
impl Beta for Fixed { beta: Fixed.beta }
effect fn fromAlpha() -> i32 ? &Alpha { return run Alpha.alpha() }
effect fn fromBeta() -> i32 ? &Beta { return run Beta.beta() }
pub fn main() -> i32 {
  let alphaProvider = Fixed { value: 3 }
  let betaProvider = Fixed { value: 4 }
  let chosen = Effect.ifThenElse(true, fromAlpha, fromBeta)
  let provided = Effect.provide<Beta>(
    Effect.provide<Alpha>(move chosen, &alphaProvider),
    &betaProvider,
  )
  return run provided
}`

it.effect('unions the requirement rows of both arms before provisioning', () =>
  Effect.gen(function* () {
    const module = 'if-then-else/requirements'
    const snapshot = yield* Analysis.ofSourceRealized(
      module,
      ascii(requirements),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const available = Analysis.expressionsOf(snapshot, module).flatMap((expression) =>
      expression.type._tag === 'Available' ? [Type.encode(expression.type.type)] : [],
    )
    assert.include(available, `Effect<i32 ? &${module}.Alpha | &${module}.Beta>`)
  }),
)

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
