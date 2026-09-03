import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Diagnostic from '../src/Diagnostic.js'

/**
 * INV-1: `Analysis.*` never throws. Every program here used to die inside the compiler (a stack
 * overflow, a lost invariant, or a parser gap turning into a semantic defect); each must now be
 * either a valid program or a typed diagnostic.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const target = 'wasm32-unknown-unknown'

const realized = (id: string, source: string) =>
  Analysis.ofSourceRealized(id, ascii(source), target)

const codes = (snapshot: Analysis.FrontendSnapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

const spans = (
  snapshot: Analysis.FrontendSnapshot,
  code: Diagnostic.Code,
): ReadonlyArray<readonly [number, number]> =>
  Analysis.diagnostics(snapshot)
    .filter((diagnostic) => diagnostic.code === code)
    .map((diagnostic) => [diagnostic.span.start, diagnostic.span.end] as const)

const evaluated = (snapshot: Analysis.Snapshot): bigint => {
  const outcome = Analysis.evaluate(snapshot)
  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return -1n
  assert.strictEqual(outcome.result._tag, 'IntegerValue')
  return outcome.result._tag === 'IntegerValue' ? outcome.result.value : -1n
}

const wasm = Effect.fnUntraced(function* (snapshot: Analysis.Snapshot) {
  const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
  const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
  return (instance.exports.silk_main as () => number)()
})

const chain = (count: number, operator: string, term: string): string =>
  Array.from({ length: count }, () => term).join(` ${operator} `)

// ISSUE-96 — long chains and long statement lists must not become JavaScript recursion.

it.effect('lowers a 257-term left-nested arithmetic chain on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stabilization/chain-257',
      `pub fn main() -> i32 {\n  return ${chain(257, '+', '1')}\n}\n`,
    )
    assert.deepEqual(codes(snapshot), [])
    assert.strictEqual(evaluated(snapshot), 257n)
    assert.strictEqual(yield* wasm(snapshot), 257)
  }),
)

it.effect('rejects a chain nesting past EXPR-006 with PAR0005 instead of overflowing', () =>
  Effect.gen(function* () {
    const plus = yield* realized(
      'stabilization/chain-300',
      `pub fn main() -> i32 {\n  return ${chain(300, '+', '1')}\n}\n`,
    )
    assert.deepEqual(codes(plus), [Diagnostic.expressionNestingLimitExceededCode])
    const and = yield* realized(
      'stabilization/and-2000',
      `pub fn main() -> i32 {\n  let t = true\n  if ${chain(2000, '&&', 't')} { return 1 }\n  return 0\n}\n`,
    )
    assert.deepEqual(codes(and), [Diagnostic.expressionNestingLimitExceededCode])
    const pipe = yield* realized(
      'stabilization/pipe-2000',
      `fn id(x: i32) -> i32 { return x }\npub fn main() -> i32 {\n  return 0${' |> id'.repeat(2000)}\n}\n`,
    )
    assert.deepEqual(codes(pipe), [Diagnostic.expressionNestingLimitExceededCode])
  }),
)

it.effect('lowers 256 nested prefix operators and a 257-stage pipeline', () =>
  Effect.gen(function* () {
    const negated = yield* realized(
      'stabilization/prefix-256',
      `pub fn main() -> i32 {\n  return ${'- '.repeat(256)}42\n}\n`,
    )
    assert.deepEqual(codes(negated), [])
    assert.strictEqual(evaluated(negated), 42n)
    const piped = yield* realized(
      'stabilization/pipe-256',
      `fn id(x: i32) -> i32 { return x }\npub fn main() -> i32 {\n  return 42${' |> id'.repeat(256)}\n}\n`,
    )
    assert.deepEqual(codes(piped), [])
    assert.strictEqual(evaluated(piped), 42n)
  }),
)

it.effect(
  'lowers a 5000-statement function on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* realized(
        'stabilization/statements-5000',
        `pub fn main() -> i32 {\n  let mut x = 0\n${'  x = x + 1\n'.repeat(5000)}  return x - 4958\n}\n`,
      )
      assert.deepEqual(codes(snapshot), [])
      assert.strictEqual(evaluated(snapshot), 42n)
      assert.strictEqual(yield* wasm(snapshot), 42)
    }),
  240_000,
)

// ISSUE-43 — a callable contract as an explicit generic argument in expression position.

it.effect('parses a callable contract type argument on a generic call or literal', () =>
  Effect.gen(function* () {
    const choose = `fn choose<A, B>(left: A, right: B) -> A {\n  drop right\n  return move left\n}\n`
    const call = yield* realized(
      'stabilization/generic-callable-call',
      `${choose}pub fn main() -> i32 { return choose<fn(i32) -> i32>(1, true) }\n`,
    )
    assert.deepEqual(codes(call), [Diagnostic.typeArgumentConflictCode])
    const once = yield* realized(
      'stabilization/generic-callable-once',
      `${choose}pub fn main() -> i32 {\n  choose<once fn(i32) -> i32>(1, true)\n  return 1\n}\n`,
    )
    assert.deepEqual(codes(once), [Diagnostic.typeArgumentConflictCode])
    const literal = yield* realized(
      'stabilization/generic-callable-literal',
      `struct Pair<A, B> {\n  first: A\n  second: B\n}\npub fn main() -> i32 {\n  let p = Pair<fn(i32) -> i32> { first: 1, second: true }\n  return 1\n}\n`,
    )
    assert.deepEqual(codes(literal), [Diagnostic.typeArgumentConflictCode])
    const valid = yield* realized(
      'stabilization/generic-callable-valid',
      `fn add(a: i32, b: i32) -> i32 { return a + b }\nfn makeAdder(base: i32) -> fn(i32) -> i32 { return add(base) }\nfn apply<A>(x: i32, f: once fn(i32) -> A) -> A { return f(x) }\npub fn main() -> i32 {\n  let f = apply<fn(i32) -> i32>(2, makeAdder)\n  return f(40)\n}\n`,
    )
    assert.deepEqual(codes(valid), [])
  }),
)

// ISSUE-89 — a bodiless `export "C" fn` is a parser diagnostic.

it.effect('reports a missing exported function body as a parser diagnostic', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stabilization/export-bodiless',
      `export "C" fn bad(value: i32) -> i32\npub fn main() -> i32 { return 0 }\n`,
    )
    assert.include(codes(snapshot), Diagnostic.missingTokenCode)
  }),
)

// ISSUE-97 — an effect entry whose body is rejected has no runnable entry, not a lost runner.

it.effect('keeps the ownership diagnostic when an effect entry body cannot be lowered', () =>
  Effect.gen(function* () {
    const snapshot = yield* realized(
      'stabilization/effect-entry-rejected',
      `effect fn build(trace: &mut [i32]) -> i32 { return trace[0] }\npub effect fn main() {\n  let mut t = [0]\n  let e = build(&mut t)\n  if t[0] != 0 { return () }\n  let r = run e\n  return ()\n}\n`,
    )
    assert.deepEqual(codes(snapshot), [Diagnostic.ownerAccessDuringLoanCode])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Blocked')
  }),
)

// ISSUE-3 — a static function has no runtime function item (STATIC-001).

it.effect('rejects a static function used as a runtime callable value', () =>
  Effect.gen(function* () {
    const source = `static fn c(v: i32) -> i32 { return v }\nfn apply(f: fn(i32) -> i32) -> i32 { return f(1) }\npub fn main() -> i32 {\n  let g = c\n  return apply(c)\n}\n`
    const snapshot = yield* realized('stabilization/static-as-value', source)
    assert.deepEqual(
      spans(snapshot, Diagnostic.staticPhaseViolationCode).map(([start, end]) =>
        source.slice(start, end).trim(),
      ),
      ['c', 'c'],
    )
    const runtimeCall = yield* realized(
      'stabilization/static-runtime-call',
      `static fn c(v: i32) -> i32 { return v + 41 }\npub fn main() -> i32 {\n  let n = 1\n  return c(n)\n}\n`,
    )
    assert.deepEqual(codes(runtimeCall), [Diagnostic.staticPhaseViolationCode])
  }),
)

// ISSUE-39 — a struct literal in argument position beside or containing a call.

it.effect('parses struct literals in call arguments beside and containing calls', () =>
  Effect.gen(function* () {
    const prelude = `struct P { code: i32 }\nfn two(a: i32, p: P) -> i32 { return a + p.code }\nfn one() -> i32 { return 1 }\n`
    const after = yield* realized(
      'stabilization/struct-arg-after-call',
      `${prelude}pub fn main() -> i32 { return two(one(), P { code: 41 }) }\n`,
    )
    assert.deepEqual(codes(after), [])
    assert.strictEqual(evaluated(after), 42n)
    const grouped = yield* realized(
      'stabilization/struct-arg-grouped',
      `${prelude}pub fn main() -> i32 { return two(one(), (P { code: 41 })) }\n`,
    )
    assert.deepEqual(codes(grouped), [])
    assert.strictEqual(evaluated(grouped), 42n)
    const nested = yield* realized(
      'stabilization/struct-arg-nested-call',
      `struct Log { v: i32 }\nstruct Node { marker: i32 }\nfn peek(log: &Log) -> i32 { return log.v }\nfn take(n: Node) -> i32 { return n.marker }\npub fn main() -> i32 {\n  let log = Log { v: 42 }\n  return take(Node { marker: peek(&log) })\n}\n`,
    )
    assert.deepEqual(codes(nested), [])
    assert.strictEqual(evaluated(nested), 42n)
  }),
)

// ISSUE-8 — fixed-array whole-value type patterns (PATT-016, PATT-018).

it.effect('matches fixed-array whole-value type patterns in match and if-let', () =>
  Effect.gen(function* () {
    const matched = yield* realized(
      'stabilization/array-type-pattern-match',
      `fn choose(value: i32 | [i32; 2]) -> i32 {\n  return match move value {\n    i32 number => number\n    [i32; 2] numbers => numbers[0]\n  }\n}\npub fn main() -> i32 { return choose([40, 1]) + choose(2) }\n`,
    )
    assert.deepEqual(codes(matched), [])
    assert.strictEqual(evaluated(matched), 42n)
    assert.strictEqual(yield* wasm(matched), 42)
    const conditional = yield* realized(
      'stabilization/array-type-pattern-if-let',
      `fn first(value: [i32; 2] | bool) -> i32 {\n  if let [i32; 2] numbers = &value {\n    return numbers[0]\n  }\n  return 0\n}\npub fn main() -> i32 { return first([42, 1]) + first(false) }\n`,
    )
    assert.deepEqual(codes(conditional), [])
    assert.strictEqual(evaluated(conditional), 42n)
  }),
)

// ISSUE-37 — invalid entry shapes are reported at the `main` declaration.

it.effect('reports invalid entry shapes at the declaration', () =>
  Effect.gen(function* () {
    const cases: ReadonlyArray<readonly [string, string, readonly [number, number]]> = [
      ['private', 'effect fn main() {\n}\n', [0, 20]],
      ['bool', 'pub fn main() -> bool {\n  return true\n}\n', [0, 39]],
      ['effect-i32', 'pub effect fn main() -> i32 { return 42 }\n', [0, 41]],
      ['params', 'pub fn main(argc: i32) -> i32 {\n  return argc\n}\n', [0, 47]],
      ['generic', 'pub fn main<T>() -> i32 {\n  return 0\n}\n', [0, 38]],
      [
        'open',
        'service Clock {}\n\neffect fn work() -> () ? &Clock {\n  return ()\n}\n\npub effect fn main() ? &Clock {\n  return run work()\n}\n',
        [65, 120],
      ],
    ]
    for (const [name, source, span] of cases) {
      const snapshot = yield* realized(`stabilization/entry-${name}`, source)
      assert.deepEqual(codes(snapshot), [Diagnostic.invalidEntryShapeCode], name)
      assert.deepEqual(spans(snapshot, Diagnostic.invalidEntryShapeCode), [span], name)
    }
    const open = yield* realized(
      'stabilization/entry-open-two',
      'service Clock {}\nservice Logger {}\nrole Audit\n\neffect fn work() -> () ? &Clock | &mut Logger at Audit {\n  return ()\n}\n\npub effect fn main() ? &Clock | &mut Logger at Audit {\n  return run work()\n}\n',
    )
    const [diagnostic] = Analysis.diagnostics(open)
    assert.strictEqual(diagnostic?.code, Diagnostic.invalidEntryShapeCode)
    assert.deepEqual(diagnostic?.reason, {
      _tag: 'InvalidEntryShape',
      detail:
        'has unresolved requirements: &stabilization/entry-open-two.Clock, &mut stabilization/entry-open-two.Logger at Audit',
    })
  }),
)
