import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Json from './support/Json.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const codesOf = (name: string, source: string) =>
  Effect.map(Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown'), (snapshot) =>
    Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
  )

/** Asserts a clean program computes `expected` on the evaluator and through the Wasm backend. */
const completes = (name: string, source: string, expected: number) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      `${name}: ${Json.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? value.toString() : value))}`,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, BigInt(expected), name)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected, name)
  })

// ISSUE-4: a callable returned through a structural `fn(..)` result keeps its exact target.
it.effect(
  'invokes callables returned through a structural callable result type',
  () =>
    Effect.gen(function* () {
      yield* completes(
        'callable-stabilization/returned-named',
        `fn inc2(value: i32) -> i32 { return value + 2 }
fn make() -> fn(i32) -> i32 { return inc2 }
pub fn main() -> i32 {
  let add2 = make()
  return add2(40)
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/returned-anonymous',
        `fn adder(offset: i32) -> fn(i32) -> i32 {
  return fn(value: i32) -> i32 { return value + offset }
}
pub fn main() -> i32 {
  let add2 = adder(2)
  return add2(40)
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/returned-section',
        `fn combine(a: i32, b: i32) -> i32 { return a * 10 + b }
fn make() -> fn(i32) -> i32 { return combine(2) }
pub fn main() -> i32 {
  let f = make()
  return f(4)
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/returned-once',
        `struct Token { value: i32 }
fn prepare(token: Token) -> once fn() -> Token {
  return fn() -> Token { return move token }
}
pub fn main() -> i32 {
  let take = prepare(Token { value: 42 })
  let t = take()
  return t.value
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/returned-through-parameter',
        `fn pass(f: fn(i32) -> i32) -> fn(i32) -> i32 { return f }
fn inc2(value: i32) -> i32 { return value + 2 }
pub fn main() -> i32 {
  let add2 = pass(inc2)
  return add2(40)
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/returned-copy-binding',
        `fn make() -> fn() -> i32 {
  let x = 42
  let get = fn() -> i32 { return x }
  return get
}
pub fn main() -> i32 { let f = make() return f() }`,
        42,
      )
    }),
  120_000,
)

// ISSUE-4: reassigning a callable binding keeps its exact identity or reports SEM0080.
it.effect('rejects reassigning a callable binding to another construction site', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codesOf(
        'callable-stabilization/reassign-named',
        `fn inc(v: i32) -> i32 { return v + 1 }
fn dec(v: i32) -> i32 { return v - 1 }
pub fn main() -> i32 {
  let mut f = inc
  f = dec
  return f(43)
}`,
      ),
      ['SEM0080'],
    )
    assert.deepEqual(
      yield* codesOf(
        'callable-stabilization/reassign-anonymous',
        `pub fn main() -> i32 {
  let a = fn(v: i32) -> i32 { return v + 1 }
  let b = fn(v: i32) -> i32 { return v + 2 }
  let mut choice = a
  choice = b
  return choice(40)
}`,
      ),
      ['SEM0080'],
    )
    yield* completes(
      'callable-stabilization/reassign-same',
      `fn inc(v: i32) -> i32 { return v + 1 }
pub fn main() -> i32 {
  let mut f = inc
  f = inc
  return f(41)
}`,
      42,
    )
  }),
)

// ISSUE-2: joining two named function items reports SEM0080 instead of invalid MIR.
it.effect('rejects a match that joins two named function items', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      yield* codesOf(
        'callable-stabilization/match-join-named',
        `fn inc(v: i32) -> i32 { return v + 1 }
fn dec(v: i32) -> i32 { return v - 1 }
struct A {}
struct B {}
fn pick(x: A | B) -> i32 {
  let f = match &x {
    A {} => inc
    B {} => dec
  }
  return f(41)
}
pub fn main() -> i32 { return pick(A {}) }`,
      ),
      ['SEM0080'],
    )
  }),
)

// ISSUE-46 / ISSUE-51: a section may leave with a loan rooted in a caller-owned borrowed parameter.
it.effect(
  'returns sections that capture borrowed parameters and names an escaping local root',
  () =>
    Effect.gen(function* () {
      yield* completes(
        'callable-stabilization/escape-direct-slice-parameter',
        `fn select(value: i32, values: &[i32]) -> i32 { return value + values[0] }
fn prepare(values: &[i32]) -> fn(i32) -> i32 { return select(values) }
pub fn main() -> i32 {
  let v = [40]
  let cb = prepare(&v)
  return cb(2)
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/escape-reborrowed-slice-parameter',
        `fn select(value: i32, values: &[i32]) -> i32 { return value + values[0] }
fn prepare(values: &[i32]) -> fn(i32) -> i32 { return select(&values) }
pub fn main() -> i32 {
  let v = [40]
  let cb = prepare(&v)
  return cb(2)
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/escape-exclusive-parameter',
        `fn select(value: i32, values: &mut [i32]) -> i32 {
  values[0] = values[0] + 1
  return value + values[0]
}
fn prepare(values: &mut [i32]) -> mut fn(i32) -> i32 { return select(&mut values) }
pub fn main() -> i32 {
  let mut v = [40]
  let mut cb = prepare(&mut v)
  let r = cb(1)
  drop cb
  return r + v[0] - 41
}`,
        42,
      )
      const snapshot = yield* Analysis.ofSourceRealized(
        'callable-stabilization/escape-local-root',
        ascii(`fn read(value: i32, values: &mut [i32]) -> i32 { return value + values[0] }
fn make() -> mut fn(i32) -> i32 {
  let mut v = [0]
  return read(&mut v)
}
pub fn main() -> i32 { let mut f = make() return f(1) }`),
        'wasm32-unknown-unknown',
      )
      const diagnostics = Analysis.diagnostics(snapshot)
      assert.deepEqual(
        diagnostics.map((diagnostic) => diagnostic.code),
        ['OWN0018'],
      )
      assert.include(diagnostics.at(0)?.message, 'exclusive borrow of local v')
    }),
  120_000,
)

// ISSUE-57: one named function admitted under `fn` and `once fn` representation parameters.
it.effect(
  'admits one named function under both fn and once fn representation bounds',
  () =>
    Effect.gen(function* () {
      const declarations = `struct Parser<F: fn(i32) -> i32> { parse: F }
struct Once<F: once fn(i32) -> i32> { parse: F }
fn parseDecimal(v: i32) -> i32 { return v * 10 }`
      yield* completes(
        'callable-stabilization/representation-once-then-fn',
        `${declarations}
pub fn main() -> i32 {
  let o = Once { parse: parseDecimal }
  let p = Parser { parse: parseDecimal }
  return p.parse(4) + 2
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/representation-fn-then-once',
        `${declarations}
pub fn main() -> i32 {
  let p = Parser { parse: parseDecimal }
  let o = Once { parse: parseDecimal }
  return p.parse(4) + 2
}`,
        42,
      )
      yield* completes(
        'callable-stabilization/representation-mode-mismatch',
        `${declarations}
fn takeOnce<F: once fn(i32) -> i32>(o: Once<F>) -> i32 { return o.parse(1) }
fn takeParser<F: fn(i32) -> i32>(p: Parser<F>) -> i32 { return p.parse(1) + p.parse(2) }
pub fn main() -> i32 {
  let o = Once { parse: parseDecimal }
  let p = Parser { parse: parseDecimal }
  return takeOnce(move o) + takeParser(move p) + 2
}`,
        42,
      )
    }),
  120_000,
)
