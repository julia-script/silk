import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshotOf = (name: string, source: string) => Analysis.ofSource(name, ascii(source))

const codesOf = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

// EFF-007: an `effect {}` block whose only terminal is `fail` has success type `never`, and that
// success satisfies any declared success while the failure channel is still checked.
it.effect('rejects a fail-only effect block whose failure exceeds the declared channel', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf(
      'effect-typing/fail-only-undeclared',
      `struct ProblemError {}
fn f() -> Effect<'static; i32> {
  return effect { fail ProblemError {} }
}
pub fn main() -> i32 { return run f() }`,
    )
    assert.deepEqual(codesOf(snapshot), ['SEM0129'])
  }),
)

// SUSP-005: a suspend wrapper keeps its failure and requirement channels and composes with
// Effect.provide and Effect.catchAll in either order.
const suspendComposition = (body: string) => `import silk.effect { Effect }
struct ProblemError { code: i32 }
service Clock {
  effect fn value() -> i32 ? &Clock
}
struct FixedClock { value: i32 }
impl Clock for FixedClock {
  effect fn value(self: &Self) -> i32 { return self.value }
}
effect fn work(n: i32) -> i32 ! ProblemError ? &Clock {
  let base = run Clock.value()
  if n < 0 { fail ProblemError { code: base + n } }
  return base + n
}
effect fn protected(n: i32) -> i32 ! ProblemError ? &Clock {
  return run Effect.suspend(work(n))
}
effect fn recover(error: ProblemError) -> i32 { return error.code * 100 }
pub fn main() -> i32 {
  let clock = FixedClock { value: 40 }
${body}
  if ok != 42 { return 1 }
  if bad != 3700 { return 2 }
  return 0
}`

it.effect('still rejects running a suspended fallible Effect inside an ordinary function', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf(
      'effect-typing/suspend-unhandled',
      suspendComposition(`  let ok = run Effect.provide(protected(2), &clock)
  let bad = run Effect.provide(Effect.catchAll(protected(-3), recover), &clock)`),
    )
    assert.deepEqual(codesOf(snapshot), ['SEM0066'])
  }),
)

// STORAGE-001: a bare Effect field has no hidden concrete identity to lay out; like a bare
// callable field it is fenced at construction instead of reaching MIR.
it.effect('fences a struct that stores a bare Effect field at its construction', () =>
  Effect.gen(function* () {
    for (const [name, source] of [
      [
        'effect-typing/bare-effect-field-once',
        `import silk.effect { Effect }
struct Payload { value: i32 }
struct Holder { e: once Effect<'static; Payload> }
fn prepare(payload: Payload) -> once Effect<'static; Payload> { return effect { return move payload } }
pub fn main() -> i32 {
  let h = Holder { e: prepare(Payload { value: 30 }) }
  return 1
}`,
      ],
      [
        'effect-typing/bare-effect-field-shared',
        `import silk.effect { Effect }
struct Holder { e: Effect<'static; i32> }
effect fn base() -> i32 { return 42 }
fn run_it(h: &Holder) -> i32 { return run h.e }
pub fn main() -> i32 {
  let h = Holder { e: base() }
  return run_it(&h)
}`,
      ],
    ] as const) {
      const snapshot = Analysis.realize(yield* snapshotOf(name, source), 'wasm32-unknown-unknown')
      assert.deepEqual(codesOf(snapshot), ['SEM0103'], name)
      assert.strictEqual(snapshot.mir._tag, 'Unavailable', name)
    }
  }),
)
