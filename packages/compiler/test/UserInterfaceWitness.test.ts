import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyzed = (name: string, source: string) => Analysis.ofSourceRealized(name, ascii(source))

const messages = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.message)

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
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => [
        diagnostic.code,
        diagnostic.span.start,
        diagnostic.span.end,
      ]),
      [['SEM0083', 213, 239]],
    )
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
