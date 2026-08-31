import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

/**
 * The bound-form confirmation #34's implementation note asks for, before any collection code.
 *
 * `HashKey` needs two operations over one bound: an equivalence, whose name `==` spells, and a
 * hash, whose name no operator spells. The first is reached through its operator; the second is
 * reached only through `Bound.operation(args)`. The operator-spelled half is confirmed with both
 * sealed and source witnesses. The named half is confirmed with source witnesses, because no
 * sealed intrinsic is a hash primitive.
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
    return outcome._tag === 'Completed' ? Number(outcome.result.value) : undefined
  })

it.effect('reaches a different witness per specialization for the non-operator operation', () =>
  Effect.gen(function* () {
    // A key type's hash is its own; no two providers share one. The two witnesses here answer
    // `digest` with instructions that are not each other's width-neutral form.
    const value = yield* evaluatedValue(
      'hash-key-bound/per-specialization',
      `pub interface HashKey {
  fn equals(left: Self, right: Self) -> bool
  fn digest(left: Self, right: Self) -> Self
}
impl HashKey for i32 { equals: Intrinsic.i32Equals digest: Intrinsic.i32WrappingAdd }
impl HashKey for u8 { equals: Intrinsic.u8Equals digest: Intrinsic.u8SaturatingAdd }
pub fn digestOf<T: HashKey>(left: T, right: T) -> T {
  return HashKey.digest(move left, move right)
}
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
const userKey = `interface HashKey {
  operator == fn equals(left: &Self, right: &Self) -> bool
  fn digest(left: &Self, right: &Self) -> u64
}

struct Cell { weight: i32 }

fn cellEquals(left: &Cell, right: &Cell) -> bool { return left.weight == right.weight }
fn cellDigest(left: &Cell, right: &Cell) -> u64 { return 7 }

impl HashKey for Cell { equals: Cell.cellEquals digest: Cell.cellDigest }`

it.effect('reaches the operator-spelled half of a two-operation bound at a user key', () =>
  Effect.gen(function* () {
    const value = yield* evaluatedValue(
      'hash-key-bound/user-equivalence',
      `${userKey}
fn same<T: HashKey>(left: T, right: T) -> bool { return (&left) == (&right) }
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
    // A scalar has no provider-owned module from which an ordinary mapped function can be selected.
    // Source witnesses for scalars therefore have one unambiguous declaration form: inline.
    const snapshot = yield* analyzed(
      'hash-key-bound/scalar-source-witness',
      `import silk.u64 as u64
interface HashKey {
  fn digest(left: &Self, right: &Self) -> u64
}
fn u64Digest(left: &u64, right: &u64) -> u64 { return 7 }
impl HashKey for u64 { digest: u64.u64Digest }
pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), [
      'Invalid conformance: u64.u64Digest is incompatible with HashKey.digest: scalar source witnesses must be declared inline',
    ])
  }),
)

/**
 * The contract shape `HashKey` actually wants, checked at the conformance layer: an operand whose
 * type is not the interface's parameter at all. The seed is one value for the whole map, so it is
 * a parameter of the hash rather than part of the key.
 */
const seededKey = `struct HashSeed { value: u64 }

interface HashKey {
  fn equals(left: &Self, right: &Self) -> bool
  fn hash(value: &Self, seed: &HashSeed) -> u64
}

struct Cell { weight: i32 }

fn cellEquals(left: &Cell, right: &Cell) -> bool { return left.weight == right.weight }
fn cellHash(value: &Cell, seed: &HashSeed) -> u64 { return seed.value }`

it.effect('admits a witness for a contract operand of a fixed non-parameter type', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(
      'hash-key-bound/seeded-contract',
      `${seededKey}
impl HashKey for Cell { equals: Cell.cellEquals hash: Cell.cellHash }
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
impl HashKey for Cell { equals: Cell.cellEquals hash: Cell.cellHashByValue }
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
