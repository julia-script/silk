import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

/**
 * The bound-form confirmation #34's implementation note asks for, before any collection code.
 *
 * `HashKey` needs two operations over one bound: an equivalence, whose name `==` spells, and a
 * hash, whose name no operator spells. The first is reached through its operator; the second is
 * reached only through `Bound.operation(args)`. Each half is confirmed here against both witness
 * kinds a provider may have — a sealed intrinsic and a function of the provider's own actor —
 * because `HashMap` is generic over the key and therefore meets both.
 *
 * Three of the four combinations carry the collection. The fourth does not, and the test that
 * records it asserts the failure rather than skipping it, so it fails the moment the gap closes.
 */

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string) => Analysis.ofSourceRealized(name, ascii(source))

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

/** Evaluated scalars can be BigInt, which plain JSON serialization refuses. */
const describe = (outcome: unknown): string =>
  JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value))

const evaluatedValue = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source)
    assert.deepEqual(messages(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    return outcome._tag === 'Completed' ? outcome.result.value : undefined
  })

/**
 * The shape of `HashKey` itself: an equivalence an operator spells and a digest none does, declared
 * over one bound and mapped by one witness. `i32` answers both with sealed intrinsics.
 */
const scalarKey = `pub interface HashKey<T> {
  fn equals(left: T, right: T) -> bool
  fn digest(left: T, right: T) -> T
}
impl HashKey<i32> for i32 { equals: Intrinsic.i32Equals digest: Intrinsic.i32WrappingAdd }
pub fn probe<T: HashKey>(left: T, right: T) -> T {
  if left == right { return HashKey.digest(left, right) }
  return HashKey.digest(right, left)
}`

it.effect('accepts a two-operation bound whose second operation no operator spells', () =>
  Effect.gen(function* () {
    // Requirement 3. Both halves are reached from one body: `==` selects `equals`, and the bound's
    // own name reaches `digest`, which no operator could have spelled.
    const value = yield* evaluatedValue(
      'hash-key-bound/scalar',
      `${scalarKey}
pub fn main() -> i32 { return probe<i32>(21, 21) }`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('reaches a different witness per specialization for the non-operator operation', () =>
  Effect.gen(function* () {
    // A key type's hash is its own; no two providers share one. The two witnesses here answer
    // `digest` with instructions that are not each other's width-neutral form.
    const value = yield* evaluatedValue(
      'hash-key-bound/per-specialization',
      `pub interface HashKey<T> {
  fn equals(left: T, right: T) -> bool
  fn digest(left: T, right: T) -> T
}
impl HashKey<i32> for i32 { equals: Intrinsic.i32Equals digest: Intrinsic.i32WrappingAdd }
impl HashKey<u8> for u8 { equals: Intrinsic.u8Equals digest: Intrinsic.u8SaturatingAdd }
pub fn digestOf<T: HashKey>(left: T, right: T) -> T { return HashKey.digest(left, right) }
pub fn main() -> i32 {
  let saturated = digestOf<u8>(200, 100)
  let wrapped = digestOf<i32>(2147483647, 1)
  if saturated != 255 { return 1 }
  if wrapped != -2147483648 { return 2 }
  return 42
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

/**
 * The same two-operation bound at a user-defined key. Only the equivalence half is exercised, which
 * is the half an operator spells; the digest is declared and mapped so the conformance is complete.
 */
const userKey = `interface HashKey<T> {
  fn equals(left: &T, right: &T) -> bool
  fn digest(left: &T, right: &T) -> u64
}

struct Cell { weight: i32 }

fn cellEquals(left: &Cell, right: &Cell) -> bool { return left.weight == right.weight }
fn cellDigest(left: &Cell, right: &Cell) -> u64 { return 7 }

impl HashKey<Cell> for Cell { equals: Cell.cellEquals digest: Cell.cellDigest }`

it.effect('reaches the operator-spelled half of a two-operation bound at a user key', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'hash-key-bound/user-equivalence',
      `${userKey}
fn same<T: HashKey>(left: T, right: T) -> bool { return left == right }
pub fn main() -> i32 {
  if same<Cell>(Cell { weight: 1 }, Cell { weight: 1 }) { return 42 }
  return 1
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('refuses a source witness for a scalar provider', () =>
  Effect.gen(function* () {
    // A scalar's conformance admits a sealed intrinsic and nothing else, so a scalar key's hash
    // cannot be ordinary Silk. Since no intrinsic computes a hash, and requirement 9 forbids one
    // being added, a scalar key has no honest `hash` witness to name.
    const snapshot = yield* analyzed(
      'hash-key-bound/scalar-source-witness',
      `interface HashKey<T> {
  fn digest(left: &T, right: &T) -> u64
}
fn u64Digest(left: &u64, right: &u64) -> u64 { return 7 }
impl HashKey<u64> for u64 { digest: u64.u64Digest }
pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), [
      'Invalid conformance: u64.u64Digest is incompatible with HashKey.digest',
    ])
  }),
)

/**
 * The contract shape `HashKey` actually wants, checked at the conformance layer: an operand whose
 * type is not the interface's parameter at all. The seed is one value for the whole map, so it is
 * a parameter of the hash rather than part of the key.
 */
const seededKey = `struct HashSeed { value: u64 }

interface HashKey<T> {
  fn equals(left: &T, right: &T) -> bool
  fn hash(value: &T, seed: &HashSeed) -> u64
}

struct Cell { weight: i32 }

fn cellEquals(left: &Cell, right: &Cell) -> bool { return left.weight == right.weight }
fn cellHash(value: &Cell, seed: &HashSeed) -> u64 { return seed.value }`

it.effect('admits a witness for a contract operand of a fixed non-parameter type', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(
      'hash-key-bound/seeded-contract',
      `${seededKey}
impl HashKey<Cell> for Cell { equals: Cell.cellEquals hash: Cell.cellHash }
pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), [])
  }),
)

it.effect('rejects a value witness when the interface explicitly promises a shared borrow', () =>
  Effect.gen(function* () {
    // The seed is `Copy`, but its authored borrow remains literal rather than type-dependent.
    const snapshot = yield* analyzed(
      'hash-key-bound/by-value-operand',
      `${seededKey}
fn cellHashByValue(value: &Cell, seed: HashSeed) -> u64 { return seed.value }
impl HashKey<Cell> for Cell { equals: Cell.cellEquals hash: Cell.cellHashByValue }
pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), [
      'Invalid conformance: Cell.cellHashByValue is incompatible with HashKey.hash: parameter seed requires take access but the interface promises shared access',
    ])
  }),
)

/**
 * The combination `HashKey.hash` actually needs: the non-operator operation over a user-defined
 * provider, whose witness is a function of the provider's own actor rather than a sealed intrinsic.
 *
 * This case was written asserting the failure it found — `Bound.operation(args)` (#118, PR #141)
 * lowered by reading only the intrinsic a witness names, while a source witness (#129, PR #142) was
 * read only from the operator path, so the call passed analysis and then lowered to nothing. #155
 * (PR #157) taught lowering the fallback the operator path already had, and the assertion is
 * inverted here to the working outcome. The case is kept rather than deleted: this is the exact
 * path every `HashKey.hash` call over a user-defined key takes.
 */
it.effect('reaches a source witness through the bound-operation call', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'hash-key-bound/user-digest',
      `${userKey}
fn digestOf<T: HashKey>(left: T, right: T) -> u64 { return HashKey.digest(&left, &right) }
pub fn main() -> i32 {
  let out = digestOf<Cell>(Cell { weight: 1 }, Cell { weight: 2 })
  if out == 7 { return 42 }
  return 1
}`,
    )
    assert.strictEqual(value, 42)
  }),
)
