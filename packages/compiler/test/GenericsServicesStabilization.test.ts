import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const describeValue = (value: unknown): string =>
  Json.stringify(value, (_, inner) => (typeof inner === 'bigint' ? inner.toString() : inner))

/** Asserts a program is accepted and returns `expected` from both the evaluator and Wasm. */
const runs = (id: string, source: string, expected: number) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(id, ascii(source), 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describeValue(evaluated))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, BigInt(expected))
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected)
  })

/** Asserts a program reports exactly `codes`, in source order. */
const rejects = (id: string, source: string, codes: ReadonlyArray<string>) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(id, ascii(source), 'wasm32-unknown-unknown')
    const diagnostics = Analysis.diagnostics(snapshot)
    assert.deepEqual(
      diagnostics.map((diagnostic) => diagnostic.code),
      codes,
      describeValue(diagnostics.map((diagnostic) => [diagnostic.code, diagnostic.message])),
    )
  })

const hashable = `interface Hashable { fn hash(value: &Self) -> i32 }
interface Display { fn display(value: &Self) -> i32 }
struct Key { k: i32 }
impl Hashable for Key { fn hash(value: &Self) -> i32 { return value.k } }
impl Display for Key { fn display(value: &Self) -> i32 { return value.k * 2 } }
fn onlyHash<T: Hashable>(value: &T) -> i32 { return Hashable.hash(value) }
fn onlyDisplay<T: Display>(value: &T) -> i32 { return Display.display(value) }
`

// ISSUE-59 — INTF-004/GEN-004: a bound is evidence for a callee bound.
it.effect('forwards a bounded parameter to a callee with the same bound', () =>
  runs(
    'stabilization/forward-same-bound',
    `${hashable}fn forward<T: Hashable>(value: &T) -> i32 { return onlyHash(value) }
pub fn main() -> i32 {
  let k = Key { k: 42 }
  return forward(&k)
}`,
    42,
  ),
)

it.effect('forwards an explicitly specialized bounded parameter by value', () =>
  runs(
    'stabilization/forward-explicit-value',
    `interface Hashable { fn hash(value: &Self) -> i32 }
struct Key { k: i32 }
impl Hashable for Key { fn hash(value: &Self) -> i32 { return value.k } }
fn onlyHash<T: Hashable>(value: T) -> i32 { return Hashable.hash(&value) }
fn forward<T: Hashable>(value: T) -> i32 { return onlyHash<T>(move value) }
pub fn main() -> i32 {
  let k = Key { k: 42 }
  return forward(move k)
}`,
    42,
  ),
)

it.effect('forwards a subset of a conjunction bound and keeps the other bound usable', () =>
  runs(
    'stabilization/forward-subset-bound',
    `${hashable}fn inspect<T: Hashable + Display>(value: &T) -> i32 { return onlyDisplay(value) + Display.display(value) }
pub fn main() -> i32 {
  let k = Key { k: 21 }
  return inspect(&k) - 42
}`,
    42,
  ),
)

it.effect('still rejects forwarding to a bound the caller never promised', () =>
  rejects(
    'stabilization/forward-missing-bound',
    `${hashable}fn bad<T: Display>(value: &T) -> i32 { return onlyHash(value) }
pub fn main() -> i32 { return 1 }`,
    ['SEM0083'],
  ),
)

// ISSUE-60 — IMPL-006/IMPL-001: inline bodies of a generic conformance.
const box = `struct Box<T> { value: T }
`

it.effect('reaches an inline operation of a generic conformance through a bound', () =>
  runs(
    'stabilization/inline-generic-bound',
    `interface Marker { fn mark(value: &Self) -> i32 }
${box}impl<T> Marker for Box<T> { fn mark(value: &Self) -> i32 { return 42 } }
fn go<T: Marker>(v: &T) -> i32 { return Marker.mark(v) }
pub fn main() -> i32 {
  let b = Box { value: true }
  return go(&b)
}`,
    42,
  ),
)

it.effect('reaches an inline operation of a generic conformance as a receiver method', () =>
  runs(
    'stabilization/inline-generic-receiver',
    `interface Marker { fn mark(value: &Self) -> i32 }
${box}impl<T> Marker for Box<T> { fn mark(value: &Self) -> i32 { return 42 } }
pub fn main() -> i32 {
  let b = Box { value: true }
  return b.mark()
}`,
    42,
  ),
)

it.effect('scopes the header binder inside an inline generic conformance body', () =>
  runs(
    'stabilization/inline-generic-binder-scope',
    `interface Wrap { fn wrap(value: &Self) -> i32 }
${box}struct Leaf { n: i32 }
fn size<U>(b: &Box<U>) -> i32 { return 40 }
impl<T> Wrap for Box<T> {
  fn wrap(value: &Self) -> i32 {
    let inner: &T = &value.value
    return size<T>(value) + 2
  }
}
fn go<T: Wrap>(v: &T) -> i32 { return Wrap.wrap(v) }
pub fn main() -> i32 {
  let b = Box { value: Leaf { n: 1 } }
  return go(&b)
}`,
    42,
  ),
)

it.effect('honours the header bound inside an inline generic conformance body', () =>
  runs(
    'stabilization/inline-generic-header-bound',
    `interface Printable { fn print(value: &Self) -> i32 }
struct Document { size: i32 }
${box}impl Printable for Document { fn print(value: &Self) -> i32 { return value.size } }
impl<T: Printable> Printable for Box<T> { fn print(value: &Self) -> i32 { return value.value.print() + 1 } }
fn show<T: Printable>(value: &T) -> i32 { return Printable.print(value) }
pub fn main() -> i32 {
  let b = Box { value: Box { value: Document { size: 40 } } }
  return show(&b)
}`,
    42,
  ),
)

it.effect('runs an inline effect operation of a generic conformance', () =>
  runs(
    'stabilization/inline-generic-effect',
    `interface Marker { effect fn mark(value: &Self) -> i32 }
${box}impl<T> Marker for Box<T> { effect fn mark(value: &Self) -> i32 { return 42 } }
effect fn go<T: Marker>(v: &T) -> i32 { return run Marker.mark(v) }
pub fn main() -> i32 {
  let b = Box { value: true }
  return run go(&b)
}`,
    42,
  ),
)

// ISSUE-88 — UNSAFE-005: generic unsafe callable bounds.
const unsafeInvoke = `unsafe fn raw(value: i32) -> i32 { return value * 2 }
fn safe(value: i32) -> i32 { return value * 3 }
`

it.effect('acknowledges a generic unsafe callable bound with the prefix form', () =>
  runs(
    'stabilization/unsafe-generic-prefix',
    `${unsafeInvoke}fn invoke<F: unsafe fn(i32) -> i32>(operation: F, value: i32) -> i32 {
  return unsafe operation(value)
}
pub fn main() -> i32 {
  let a = invoke(raw, 1)
  let b = invoke(safe, 1)
  return a * 10 + b
}`,
    23,
  ),
)

it.effect('instantiates a generic unsafe callable bound with a safe witness in a block', () =>
  runs(
    'stabilization/unsafe-generic-block-safe',
    `${unsafeInvoke}fn invoke<F: unsafe fn(i32) -> i32>(operation: F, value: i32) -> i32 {
  unsafe {
    return operation(value)
  }
  return 0
}
pub fn main() -> i32 {
  return invoke(safe, 1)
}`,
    3,
  ),
)

it.effect('still requires acknowledgement for a generic unsafe callable bound', () =>
  rejects(
    'stabilization/unsafe-generic-unacknowledged',
    `${unsafeInvoke}fn invoke<F: unsafe fn(i32) -> i32>(operation: F, value: i32) -> i32 {
  return operation(value)
}
pub fn main() -> i32 { return invoke(safe, 1) }`,
    ['SEM0082'],
  ),
)

// ISSUE-29 — SERV-003: a service used as an interface.
const schemaInfo = (keyword: string) => `${keyword} SchemaInfo {
  fn decode(value: &Self) -> i32
  fn width(value: &Self) -> i32
}
struct Schema {}
fn schemaWidth(value: &Schema) -> i32 { return 32 }
impl SchemaInfo for Schema {
  fn decode(value: &Self) -> i32 { return 42 }
  width: Schema.schemaWidth
}
fn use<T: SchemaInfo>(v: &T) -> i32 { return SchemaInfo.decode(v) + SchemaInfo.width(v) }
pub fn main() -> i32 {
  let s = Schema {}
  return use(&s)
}`

it.effect('gives a service exactly the semantics of the equivalent interface', () =>
  Effect.gen(function* () {
    yield* runs('stabilization/service-as-interface', schemaInfo('interface'), 74)
    yield* runs('stabilization/service-as-interface-service', schemaInfo('service'), 74)
  }),
)

it.effect('resolves a static call through a service bound with a self-named receiver', () =>
  runs(
    'stabilization/service-bound-self-name',
    `service Clock {
  fn now(self: &Self) -> i64
}
struct SystemClock {}
impl Clock for SystemClock {
  fn now(self: &Self) -> i64 { return 7 }
}
fn acceptsClock<T: Clock>(provider: &T) -> i64 { return Clock.now(provider) }
pub fn main() -> i32 {
  let c = SystemClock {}
  if acceptsClock(&c) == 7 { return 1 }
  return 0
}`,
    1,
  ),
)

// ISSUE-31/ISSUE-32 — SERV-008: `Without` written in a type position.
it.effect('subtracts a service key from a requirement row written in a type position', () =>
  runs(
    'stabilization/without-type-position',
    `import silk.effect { Effect }
service Clock {}
service Logger {}
struct SystemClock {}
impl Clock for SystemClock {}
struct Log {}
impl Logger for Log {}
effect fn work() -> i32 ? &Clock | &Logger { return 1 }
fn narrowed(c: &SystemClock) -> Effect<i32 ? Without<&Clock | &Logger, Clock>> {
  return Effect.provide<Clock>(work(), c)
}
pub fn main() -> i32 {
  let c = SystemClock {}
  let l = Log {}
  return run Effect.provide<Logger>(narrowed(&c), &l)
}`,
    1,
  ),
)

// ISSUE-58 / ISSUE-63 — INTF-006 / GEN-004: bare qualified calls at concrete and unbounded sites.
const encodable = `interface Encodable<A> { fn encode(value: &Self) -> A }
struct Age { years: i32 }
impl Encodable<i32> for Age { fn encode(value: &Self) -> i32 { return value.years } }
`

it.effect('resolves a bare qualified interface call from its concrete receiver operand', () =>
  runs(
    'stabilization/bare-qualified-concrete',
    `${encodable}pub fn main() -> i32 {
  let age = Age { years: 42 }
  return Encodable.encode(&age)
}`,
    42,
  ),
)

it.effect('reports two applications supplying a bare qualified call as ambiguous', () =>
  rejects(
    'stabilization/bare-qualified-ambiguous',
    `${encodable}impl Encodable<bool> for Age { fn encode(value: &Self) -> bool { return value.years > 18 } }
pub fn main() -> i32 {
  let age = Age { years: 40 }
  let numeric: i32 = Encodable.encode(&age)
  return numeric
}`,
    ['SEM0202'],
  ),
)

it.effect(
  'names the unbounded parameter when a generic body calls an interface it never bounds',
  () =>
    rejects(
      'stabilization/bare-qualified-missing-bound',
      `${encodable}fn decode<T>(value: &T) -> i32 { return Encodable.encode(value) }
pub fn main() -> i32 { return 1 }`,
      ['SEM0083'],
    ),
)

// ISSUE-62 — GEN-004: an unknown bound is reported at its declaration.
it.effect('reports an unknown bound on an unreachable declaration', () =>
  rejects(
    'stabilization/unknown-bound-unreachable',
    `fn decode<T: Nope>(value: T) -> T { return move value }
pub fn main() -> i32 { return 1 }`,
    ['SEM0083'],
  ),
)

// ISSUE-27 — SERV-002: only a service keys a requirement row in a type position.
it.effect('rejects a struct as a requirement key in an Effect type position', () =>
  rejects(
    'stabilization/struct-requirement-key',
    `struct Config {}
fn take(e: once Effect<i32 ? &Config>) -> i32 { return 0 }
pub fn main() -> i32 { return 0 }`,
    ['SEM0070'],
  ),
)

it.effect('parses a role-qualified key inside a Without operand', () =>
  runs(
    'stabilization/without-at-role',
    `import silk.effect { Effect }
service Clock {}
struct SystemClock {}
impl Clock for SystemClock {}
role Primary
effect fn work() -> i32 ? &Clock at Primary { return 1 }
fn narrowed(c: &SystemClock) -> Effect<i32 ? Without<&Clock at Primary, Clock at Primary>> {
  return Effect.provide<Clock at Primary>(work(), c)
}
pub fn main() -> i32 {
  let c = SystemClock {}
  return run narrowed(&c)
}`,
    1,
  ),
)
