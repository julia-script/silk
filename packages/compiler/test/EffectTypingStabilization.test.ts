import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshotOf = (name: string, source: string) =>
  Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')

/** Asserts the program is clean and that the evaluator and direct WebAssembly agree on `expected`. */
const assertRuns = (name: string, source: string, expected: number) =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf(name, source)
    assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
    assert.strictEqual(snapshot.mir._tag, 'Available', name)
    if (snapshot.mir._tag === 'Available')
      assert.deepEqual(MirVerification.verify(snapshot.mir.value), [], name)
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      Json.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, BigInt(expected))
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected, name)
  })

const codesOf = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

// FAIL-004: the recovered success type is the union `A | B`; both the protected value and the
// handler value must be injected into it so a later `match` sees an active member.
it.effect('matches the A | B success union produced by Effect.catch on every engine', () =>
  assertRuns(
    'effect-typing/catch-union-match',
    `import silk.effect { Effect }
struct NotFoundError {}
effect fn load(flag: bool) -> i32 ! NotFoundError {
  if flag { fail NotFoundError {} }
  return 5
}
effect fn recover(error: NotFoundError) -> string { return "missing" }
fn handled(flag: bool) -> Effect<i32 | string> {
  return Effect.catch<NotFoundError>(load(flag), recover)
}
pub fn main() -> i32 {
  let a = run handled(true)
  let b = run handled(false)
  let x = match move a { i32 n => n
    string _ => 100 }
  let y = match move b { i32 n => n
    string _ => 100 }
  return x + y
}`,
    105,
  ),
)

it.effect('injects the protected success into a union recovered from a multi-member row', () =>
  assertRuns(
    'effect-typing/catch-subset-union-match',
    `import silk.effect { Effect }
struct NotFoundError {}
struct OfflineError {}
effect fn work(flag: bool) -> i32 ! NotFoundError | OfflineError {
  if flag { fail NotFoundError {} }
  return 4
}
effect fn recover(error: NotFoundError) -> string { return "fallback" }
effect fn handled(flag: bool) -> i32 | string ! OfflineError {
  return run Effect.catch<NotFoundError>(work(flag), recover)
}
effect fn last(error: OfflineError) -> i32 | string { return 9 }
pub fn main() -> i32 {
  let a = run Effect.catchAll(handled(true), last)
  let b = run Effect.catchAll(handled(false), last)
  let x = match move a { i32 n => n
    string _ => 100 }
  let y = match move b { i32 n => n
    string _ => 100 }
  return x * 10 + y
}`,
    1004,
  ),
)

// FAIL-004: a handler with success type `never` contributes no success member; its run diverges
// instead of copying a value into the recovered success.
it.effect('runs a catch whose handler succeeds with never on every engine', () =>
  assertRuns(
    'effect-typing/catch-never-handler',
    `import silk.effect { Effect }
struct NotFoundError {}
struct OtherError {}
effect fn load(flag: bool) -> i32 ! NotFoundError {
  if flag { fail NotFoundError {} }
  return 5
}
effect fn rethrow(error: NotFoundError) -> never ! OtherError { fail OtherError {} }
fn handled(flag: bool) -> Effect<i32 ! OtherError> {
  return Effect.catch<NotFoundError>(load(flag), rethrow)
}
effect fn rec(e: OtherError) -> i32 { return 1 }
pub fn main() -> i32 {
  let a = run Effect.catch<OtherError>(handled(true), rec)
  let b = run Effect.catch<OtherError>(handled(false), rec)
  return a * 10 + b
}`,
    15,
  ),
)

// EFF-007: an `effect {}` block whose only terminal is `fail` has success type `never`, and that
// success satisfies any declared success while the failure channel is still checked.
it.effect('rejects a fail-only effect block whose failure exceeds the declared channel', () =>
  Effect.gen(function* () {
    const snapshot = yield* snapshotOf(
      'effect-typing/fail-only-undeclared',
      `struct ProblemError {}
fn f() -> Effect<i32> {
  return effect { fail ProblemError {} }
}
pub fn main() -> i32 { return run f() }`,
    )
    assert.deepEqual(codesOf(snapshot), ['SEM0129'])
  }),
)

it.effect('runs a fail-only effect block through Effect.catch on every engine', () =>
  assertRuns(
    'effect-typing/fail-only-declared',
    `import silk.effect { Effect }
struct ProblemError {}
fn fallible() -> Effect<i32 ! ProblemError> {
  return effect { fail ProblemError {} }
}
effect fn recover(e: ProblemError) -> i32 { return 7 }
pub fn main() -> i32 {
  return run Effect.catch<ProblemError>(fallible(), recover)
}`,
    7,
  ),
)

it.effect('runs a generic fail-only effect block on every engine', () =>
  assertRuns(
    'effect-typing/fail-only-generic',
    `import silk.effect { Effect }
pub struct ProblemError { code: i32 }
fn failWith<E>(problem: E) -> once Effect<i32 ! E> {
  return effect {
    fail move problem
  }
}
effect fn recover(e: ProblemError) -> i32 { return e.code }
pub fn main() -> i32 {
  return run Effect.catchAll(failWith<ProblemError>(ProblemError { code: 9 }), recover)
}`,
    9,
  ),
)

it.effect('propagates a fail-only effect block run as a statement', () =>
  assertRuns(
    'effect-typing/fail-only-statement',
    `import silk.effect { Effect }
struct UseError {}
effect fn failing() -> () ! UseError {
  run effect { fail UseError {} }
  return ()
}
effect fn recover(error: UseError) -> () { return () }
pub fn main() -> i32 {
  run Effect.catchAll(failing(), recover)
  return 0
}`,
    0,
  ),
)

// EFF-013 / EFFECT-OWN-002: distinct return sites join under the declared contract, so each call
// runs the Effect its own branch constructed rather than the last construction site.
const onceJoinProgram = (branches: string) => `struct Token { value: i32 }
impl Drop for Token {
  fn drop(self: &mut Token) -> () { return () }
}
effect fn withToken(t: Token) -> i32 { return t.value }
effect fn withToken2(t: Token) -> i32 { return t.value * 2 }
fn choose(flag: bool) -> once Effect<i32> {
${branches}
}
pub fn main() -> i32 {
  let a = run choose(true)
  let b = run choose(false)
  return a * 10 + b
}`

it.effect('joins a once effect-fn call with a reusable effect block under a once result', () =>
  assertRuns(
    'effect-typing/once-join-call',
    onceJoinProgram(`  if flag {
    let t = Token { value: 1 }
    return withToken(move t)
  }
  return effect { return 2 }`),
    12,
  ),
)

it.effect('joins a reusable effect block with a once effect-fn call in the other order', () =>
  assertRuns(
    'effect-typing/once-join-call-swapped',
    onceJoinProgram(`  if flag {
    return effect { return 2 }
  }
  let t = Token { value: 1 }
  return withToken(move t)`),
    21,
  ),
)

it.effect('runs the branch-selected alternative when both return sites are once calls', () =>
  assertRuns(
    'effect-typing/once-join-two-calls',
    onceJoinProgram(`  if flag {
    let t = Token { value: 1 }
    return withToken(move t)
  }
  let u = Token { value: 3 }
  return withToken2(move u)`),
    16,
  ),
)

it.effect('runs the branch-selected alternative when both return sites are effect blocks', () =>
  assertRuns(
    'effect-typing/block-join-branches',
    `fn choose(flag: bool) -> Effect<i32> {
  if flag { return effect { return 1 } }
  return effect { return 2 }
}
pub fn main() -> i32 {
  let a = run choose(true)
  let b = run choose(false)
  return a * 10 + b
}`,
    12,
  ),
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

it.effect('composes a suspended failing and requiring Effect with provide around catchAll', () =>
  assertRuns(
    'effect-typing/suspend-provide-catch',
    suspendComposition(`  let ok = run Effect.provide(Effect.catchAll(protected(2), recover), &clock)
  let bad = run Effect.provide(Effect.catchAll(protected(-3), recover), &clock)`),
    0,
  ),
)

it.effect('composes a suspended failing and requiring Effect with catchAll around provide', () =>
  assertRuns(
    'effect-typing/suspend-catch-provide',
    suspendComposition(`  let ok = run Effect.catchAll(Effect.provide(protected(2), &clock), recover)
  let bad = run Effect.catchAll(Effect.provide(protected(-3), &clock), recover)`),
    0,
  ),
)

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
struct Holder { e: once Effect<Payload> }
fn prepare(payload: Payload) -> once Effect<Payload> { return effect { return move payload } }
pub fn main() -> i32 {
  let h = Holder { e: prepare(Payload { value: 30 }) }
  return 1
}`,
      ],
      [
        'effect-typing/bare-effect-field-shared',
        `import silk.effect { Effect }
struct Holder { e: Effect<i32> }
effect fn base() -> i32 { return 42 }
fn run_it(h: &Holder) -> i32 { return run h.e }
pub fn main() -> i32 {
  let h = Holder { e: base() }
  return run_it(&h)
}`,
      ],
    ] as const) {
      const snapshot = yield* snapshotOf(name, source)
      assert.deepEqual(codesOf(snapshot), ['SEM0103'], name)
      assert.strictEqual(snapshot.mir._tag, 'Unavailable', name)
    }
  }),
)
