import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const codes = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code)

/**
 * The qualified and imported spellings name one function through two syntaxes, so they are held to
 * one standard: the same diagnostics and, since the module identity is the same, the same MIR.
 * Only the reference form differs between the two sources.
 */
const shared = `import silk.vector { Vector, make }

pub fn main() -> usize {
  let values = Vector.make<i32>()
  let count = Vector.length<i32>(&values)
  drop values
  return count
}`

const importedShared = `import silk.vector { Vector, make, length }

pub fn main() -> usize {
  let values = make<i32>()
  let count = length<i32>(&values)
  drop values
  return count
}`

const exclusive = `import silk.vector { Vector, make }

pub fn main() -> usize {
  let mut values = Vector.make<i32>()
  let slice = Vector.asMutSlice<i32>(&mut values)
  drop slice
  let count = Vector.length<i32>(&values)
  drop values
  return count
}`

const importedExclusive = `import silk.vector { Vector, make, length, asMutSlice }

pub fn main() -> usize {
  let mut values = make<i32>()
  let slice = asMutSlice<i32>(&mut values)
  drop slice
  let count = length<i32>(&values)
  drop values
  return count
}`

/** A borrow in a position that wants an owned value is still an invalid borrow position. */
const ownedPosition = `import silk.vector { Vector, make }

pub fn main() -> usize {
  let values = Vector.make<i32>()
  let taken = Vector.get<i32>(&values, &values)
  drop taken
  drop values
  return 0
}`

it.effect('accepts a shared borrow through the qualified spelling', () =>
  Effect.gen(function* () {
    const qualified = yield* Analysis.ofSourceRealized('borrow/program', ascii(shared))
    const imported = yield* Analysis.ofSourceRealized('borrow/program', ascii(importedShared))
    assert.deepEqual(codes(qualified), [])
    assert.deepEqual(codes(imported), [])
    // One call, one lowering: the qualifier is a spelling, not a different operation.
    assert.strictEqual(
      Mir.encode(Analysis.loweredMir(qualified)),
      Mir.encode(Analysis.loweredMir(imported)),
    )
  }),
)

it.effect('accepts an exclusive borrow through the qualified spelling', () =>
  Effect.gen(function* () {
    const qualified = yield* Analysis.ofSourceRealized('borrow/exclusive', ascii(exclusive))
    const imported = yield* Analysis.ofSourceRealized('borrow/exclusive', ascii(importedExclusive))
    assert.deepEqual(codes(qualified), [])
    assert.deepEqual(codes(imported), [])
    assert.strictEqual(
      Mir.encode(Analysis.loweredMir(qualified)),
      Mir.encode(Analysis.loweredMir(imported)),
    )
  }),
)

it.effect('still rejects a borrow where the parameter wants an owned value', () =>
  Effect.gen(function* () {
    // `get`'s second parameter is a `usize`, so the borrow has no reference type to take.
    const snapshot = yield* Analysis.ofSourceRealized('borrow/owned', ascii(ownedPosition))
    assert.include(codes(snapshot), 'SEM0055')
  }),
)

/**
 * The shape issue #70 was filed on: no import at all. The manifest namespace seeds `Vector`, so
 * this is the spelling the namespace feature exists to enable, and a borrow argument is what it
 * could not reach.
 */
const seeded = `import silk.vector { Vector }
pub fn main() -> usize {
  let values = Vector.make<i32>()
  let count = Vector.length<i32>(&values)
  drop values
  return count
}`

it.effect('accepts a borrow through a seeded namespace with no import', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('borrow/seeded', ascii(seeded))
    assert.deepEqual(codes(snapshot), [])
  }),
)
