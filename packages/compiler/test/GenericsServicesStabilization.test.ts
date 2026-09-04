import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const describeValue = (value: unknown): string =>
  Json.stringify(value, (_, inner) => (typeof inner === 'bigint' ? inner.toString() : inner))

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

it.effect('still rejects forwarding to a bound the caller never promised', () =>
  rejects(
    'stabilization/forward-missing-bound',
    `${hashable}fn bad<T: Display>(value: &T) -> i32 { return onlyHash(value) }
pub fn main() -> i32 { return 1 }`,
    ['SEM0083'],
  ),
)

// ISSUE-88 — UNSAFE-005: generic unsafe callable bounds.
const unsafeInvoke = `unsafe fn raw(value: i32) -> i32 { return value * 2 }
fn safe(value: i32) -> i32 { return value * 3 }
`

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

// ISSUE-58 / ISSUE-63 — INTF-006 / GEN-004: bare qualified calls at concrete and unbounded sites.
const encodable = `interface Encodable<A> { fn encode(value: &Self) -> A }
struct Age { years: i32 }
impl Encodable<i32> for Age { fn encode(value: &Self) -> i32 { return value.years } }
`

it.effect('resolves a service-bound call with a self-named receiver', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'stabilization/service-bound-self-name',
      ascii(`service Clock { fn now(self: &Self) -> i64 }
struct SystemClock {}
impl Clock for SystemClock { fn now(self: &Self) -> i64 { return 7 } }
fn acceptsClock<T: Clock>(provider: &T) -> i64 { return Clock.now(provider) }
pub fn main() -> i32 { let clock = SystemClock {} drop acceptsClock(&clock) return 42 }`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.include(
      Analysis.instancesOf(snapshot).instances.map((instance) => instance.key.declaration.name),
      'impl@0.now',
    )
  }),
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
