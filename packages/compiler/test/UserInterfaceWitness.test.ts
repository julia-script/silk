import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string) => Analysis.ofSourceRealized(name, ascii(source))

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

const evaluatedValue = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(name, source)
    assert.deepEqual(messages(snapshot), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      JSON.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
    )
    return outcome._tag === 'Completed' ? outcome.result.value : undefined
  })

/**
 * One user-declared interface with two operator-spelled operations, one user struct witnessing
 * both, and one generic body bounded by the interface. A witness observes each operand through a
 * shared borrow, because the interface declares that ownership explicitly.
 */
const twoOperations = `interface Blend<T> {
  fn add(left: &T, right: &T) -> T
  fn lessThan(left: &T, right: &T) -> bool
}

struct Cell {
  weight: i32
}

fn cellAdd(left: &Cell, right: &Cell) -> Cell {
  return Cell { weight: left.weight + right.weight }
}

fn cellLess(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Blend<Cell> for Cell {
  add: Cell.cellAdd
  lessThan: Cell.cellLess
}

/// Reaches both mapped operations: the comparison decides the branch and the sum is the result.
fn merged<T: Blend>(left: T, right: T) -> T {
  if left < right {
    return left + right
  }
  return right + left
}
`

it.effect('specializes a two-operation bound at a user struct and calls both operations', () =>
  Effect.gen(function* () {
    // 3 < 8 takes the first branch, and the mapped `add` folds the two weights into 11.
    const value = yield* evaluatedValue(
      'user-witness/two-operations',
      `${twoOperations}
pub fn main() -> i32 {
  let blended = merged<Cell>(Cell { weight: 3 }, Cell { weight: 8 })
  let reversed = merged<Cell>(Cell { weight: 8 }, Cell { weight: 3 })
  return blended.weight * 100 + reversed.weight
}`,
    )
    assert.strictEqual(value, 1111)
  }),
)

it.effect('rejects a conformance that leaves one operation unmapped, naming it', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(
      'user-witness/missing-operation',
      `interface Blend<T> {
  fn add(left: &T, right: &T) -> T
  fn lessThan(left: &T, right: &T) -> bool
}

struct Cell {
  weight: i32
}

fn cellLess(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Blend<Cell> for Cell {
  lessThan: Cell.cellLess
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), ['Invalid conformance: missing add'])
  }),
)

it.effect('rejects a bounded specialization at a type whose conformance is incomplete', () =>
  Effect.gen(function* () {
    // The coverage check reaches the call site too: a half-mapped witness cannot satisfy a bound.
    const snapshot = yield* analyzed(
      'user-witness/incomplete-specialization',
      `interface Blend<T> {
  fn add(left: &T, right: &T) -> T
  fn lessThan(left: &T, right: &T) -> bool
}

struct Cell {
  weight: i32
}

fn cellLess(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Blend<Cell> for Cell {
  lessThan: Cell.cellLess
}

fn ordered<T: Blend>(left: T, right: T) -> bool {
  return left < right
}

pub fn main() -> i32 {
  if ordered<Cell>(Cell { weight: 1 }, Cell { weight: 2 }) { return 1 }
  return 0
}`,
    )
    assert.include(
      messages(snapshot),
      'Invalid conformance: user-witness/incomplete-specialization.Cell does not implement Blend.add',
    )
  }),
)

it.effect('admits a value witness only when the interface literally transfers ownership', () =>
  Effect.gen(function* () {
    // There is no blanket adaptation: authored value ownership matches an authored value witness.
    const snapshot = yield* analyzed(
      'user-witness/by-value-operand',
      `interface Ordered<T> {
  fn lessThan(left: T, right: T) -> bool
}

struct Cell {
  weight: i32
}

fn cellLess(left: Cell, right: Cell) -> bool {
  return left.weight < right.weight
}

impl Ordered<Cell> for Cell {
  lessThan: Cell.cellLess
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), [])
  }),
)

it.effect('rejects a witness whose result disagrees with the contract', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(
      'user-witness/wrong-result',
      `interface Ordered<T> {
  fn lessThan(left: &T, right: &T) -> bool
}

struct Cell {
  weight: i32
}

fn cellLess(left: &Cell, right: &Cell) -> i32 {
  return left.weight - right.weight
}

impl Ordered<Cell> for Cell {
  lessThan: Cell.cellLess
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), [
      'Invalid conformance: Cell.cellLess is incompatible with Ordered.lessThan: witness returns i32 but the interface promises bool',
    ])
  }),
)

it.effect('rejects a mapping that names a function the provider actor does not declare', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyzed(
      'user-witness/absent-function',
      `interface Ordered<T> {
  fn lessThan(left: &T, right: &T) -> bool
}

struct Cell {
  weight: i32
}

impl Ordered<Cell> for Cell {
  lessThan: Cell.absent
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(messages(snapshot), [
      'Invalid conformance: mapped operation Cell.absent does not exist',
    ])
  }),
)

it.effect('keeps the standard library Order and Integer witnesses selecting their intrinsics', () =>
  Effect.gen(function* () {
    // The user witness path must not disturb the scalar one: both interfaces still specialize
    // through `Intrinsic.*` for every scalar the standard library maps.
    const value = yield* evaluatedValue(
      'user-witness/intrinsic-unchanged',
      `import silk.order { less }
import silk.numeric { add }
pub fn main() -> i32 {
  let mut score = 0
  if less<i32>(1, 2) { score = score + 1 }
  if less<u8>(2, 1) { score = score + 10 }
  return score + add<i32>(20, 21)
}`,
    )
    assert.strictEqual(value, 42)
  }),
)

it.effect('lets a user witness and an intrinsic witness serve the same interface', () =>
  Effect.gen(function* () {
    // One generic body, two specializations: `i32` keeps the compiler-known comparison and `Cell`
    // reaches ordinary Silk, so the two witness kinds coexist under one bound.
    const value = yield* evaluatedValue(
      'user-witness/mixed-witnesses',
      `import silk.order { Order, less }

struct Cell {
  weight: i32
}

fn cellLess(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Order<Cell> for Cell {
  lessThan: Cell.cellLess
}

pub fn main() -> i32 {
  let mut score = 0
  if less<i32>(1, 2) { score = score + 1 }
  if less<Cell>(Cell { weight: 5 }, Cell { weight: 4 }) { score = score + 10 }
  if less<Cell>(Cell { weight: 4 }, Cell { weight: 5 }) { score = score + 100 }
  return score
}`,
    )
    assert.strictEqual(value, 101)
  }),
)
