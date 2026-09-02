import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
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
      Json.stringify(outcome, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
    )
    return outcome._tag === 'Completed' ? Number(outcome.result.value) : undefined
  })

it.effect('allows a mutable owned witness parameter without changing interface conformance', () =>
  evaluatedValue(
    'user-witness/mutable-owned-parameter',
    `interface Transform {
  fn transform(self: &Self, value: Counter) -> Counter
}
struct Counter { value: i32 }
struct Increment {}
fn transform(self: &Increment, mut value: Counter) -> Counter {
  value.value = value.value + 1
  return move value
}
impl Transform for Increment { transform: Increment.transform }
fn apply<T: Transform>(transform: &T, value: Counter) -> Counter {
  return Transform.transform(transform, move value)
}
pub fn main() -> i32 {
  let transform = Increment {}
  let counter = Counter { value: 41 }
  let result = apply<Increment>(&transform, move counter)
  return result.value
}`,
  ),
)

it.effect('enforces unsafe operation variance and bound-call acknowledgement', () =>
  Effect.gen(function* () {
    const accepted = yield* analyzed(
      'user-witness/unsafe-operation',
      `interface Read {
  unsafe fn read(value: &Self) -> i32
}
struct Cell { value: i32 }
fn readCell(value: &Cell) -> i32 { return value.value }
impl Read for Cell { read: Cell.readCell }
fn readGeneric<T: Read>(value: T) -> i32 { return unsafe Read.read(&value) }
pub fn main() -> i32 { return readGeneric<Cell>(Cell { value: 7 }) }`,
    )
    assert.deepEqual(messages(accepted), [])

    const missingAcknowledgement = yield* analyzed(
      'user-witness/unsafe-bound-call',
      `interface Read { unsafe fn read(value: &Self) -> i32 }
fn readGeneric<T: Read>(value: T) -> i32 { return Read.read(&value) }`,
    )
    assert.include(
      Analysis.diagnostics(missingAcknowledgement).map((diagnostic) => diagnostic.code),
      'SEM0082',
    )

    const unsafeWitness = yield* analyzed(
      'user-witness/unsafe-witness-for-safe-operation',
      `interface Read { fn read(value: &Self) -> i32 }
struct Cell { value: i32 }
unsafe fn readCell(value: &Cell) -> i32 { return value.value }
impl Read for Cell { read: Cell.readCell }`,
    )
    assert.isTrue(
      messages(unsafeWitness).some((message) =>
        message.includes('unsafe witness cannot satisfy a safe operation contract'),
      ),
    )
  }),
)

/**
 * One user-declared interface with two operator-spelled operations, one user struct witnessing
 * both, and one generic body bounded by the interface. A witness observes each operand through a
 * shared borrow, because the interface declares that ownership explicitly.
 */
const twoOperations = `interface Blend {
  operator + fn add(left: &Self, right: &Self) -> Self
  operator < fn lessThan(left: &Self, right: &Self) -> bool
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

impl Blend for Cell {
  add: Cell.cellAdd
  lessThan: Cell.cellLess
}

/// Reaches both mapped operations: the comparison decides the branch and the sum is the result.
fn merged<T: Blend>(left: T, right: T) -> T {
  if (&left) < (&right) {
    return (&left) + (&right)
  }
  return (&right) + (&left)
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
      `interface Blend {
  operator + fn add(left: &Self, right: &Self) -> Self
  operator < fn lessThan(left: &Self, right: &Self) -> bool
}

struct Cell {
  weight: i32
}

fn cellLess(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Blend for Cell {
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
      `interface Blend {
  operator + fn add(left: &Self, right: &Self) -> Self
  operator < fn lessThan(left: &Self, right: &Self) -> bool
}

struct Cell {
  weight: i32
}

fn cellLess(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Blend for Cell {
  lessThan: Cell.cellLess
}

fn ordered<T: Blend>(left: T, right: T) -> bool {
  return (&left) < (&right)
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
      `interface Ordered {
  fn lessThan(left: Self, right: Self) -> bool
}

struct Cell {
  weight: i32
}

fn cellLess(left: Cell, right: Cell) -> bool {
  return left.weight < right.weight
}

impl Ordered for Cell {
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
      `interface Ordered {
  fn lessThan(left: &Self, right: &Self) -> bool
}

struct Cell {
  weight: i32
}

fn cellLess(left: &Cell, right: &Cell) -> i32 {
  return left.weight - right.weight
}

impl Ordered for Cell {
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
      `interface Ordered {
  fn lessThan(left: &Self, right: &Self) -> bool
}

struct Cell {
  weight: i32
}

impl Ordered for Cell {
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
      `import silk.order { Order }
import silk.numeric { Numeric }
pub fn main() -> i32 {
  let mut score = 0
  if Order.less<i32>(1, 2) { score = score + 1 }
  if Order.less<u8>(2, 1) { score = score + 10 }
  return score + Numeric.add<i32>(20, 21)
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
      `import silk.order { Order }

struct Cell {
  weight: i32
}

fn cellLess(left: &Cell, right: &Cell) -> bool {
  return left.weight < right.weight
}

impl Order for Cell {
  lessThan: Cell.cellLess
}

pub fn main() -> i32 {
  let mut score = 0
  if Order.less<i32>(1, 2) { score = score + 1 }
  if Order.less<Cell>(Cell { weight: 5 }, Cell { weight: 4 }) { score = score + 10 }
  if Order.less<Cell>(Cell { weight: 4 }, Cell { weight: 5 }) { score = score + 100 }
  return score
}`,
    )
    assert.strictEqual(value, 101)
  }),
)
