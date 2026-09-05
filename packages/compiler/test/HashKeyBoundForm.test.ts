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
      'Invalid conformance: u64.u64Digest is incompatible with HashKey.digest: scalar and string source witnesses must be declared inline',
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
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => [
        diagnostic.code,
        diagnostic.span.start,
        diagnostic.span.end,
      ]),
      [['SEM0083', 464, 491]],
    )
  }),
)
