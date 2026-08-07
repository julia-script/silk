import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (source: string) => Analysis.ofSource('slices/Ownership', ascii(source))

it.effect('records deterministic call-scoped loans and accepts shared aliases', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn compare(left: &[I32], right: &[I32]) -> I32 { return 1 }
fn valid() -> I32 { let values = [1, 2, 3] return compare(&values, &values) }
pub fn main() -> I32 { return valid() }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const ownership = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(1)
    assert.deepEqual(
      ownership?.loans.map((loan) => ({
        ordinal: loan.id.ordinal,
        access: loan.access,
        root: loan.root._tag,
        start: loan.startRegion.ordinal,
        end: loan.endRegion.ordinal,
      })),
      [
        { ordinal: 0, access: 'Shared', root: 'Let', start: 1, end: 1 },
        { ordinal: 1, access: 'Shared', root: 'Let', start: 1, end: 1 },
      ],
    )
    assert.deepEqual(
      ownership?.exits.at(0)?.loanEnds.map((loan) => loan.ordinal),
      [0, 1],
    )
  }),
)

it.effect('rejects conflicting aliases and later owner access during a call', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn mixed(left: &[I32], right: &mut [I32]) -> I32 { return 1 }
fn both(left: &mut [I32], right: &mut [I32]) -> I32 { return 1 }
fn observe(view: &mut [I32], value: I32) -> I32 { return value }
fn aliases() -> I32 { let mut values = [1, 2] return mixed(&values, &mut values) }
fn exclusives() -> I32 { let mut values = [1, 2] return both(&mut values, &mut values) }
fn later() -> I32 { let mut values = [1, 2] return observe(&mut values, values[0]) }
pub fn main() -> I32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0010', 'OWN0010', 'OWN0011'],
    )
  }),
)

it.effect('retains reborrow parent suspension and restores access after the call', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn edit(values: &mut [I32]) -> I32 { return 1 }
fn forward(values: &mut [I32]) -> I32 {
  let result = edit(&mut values)
  return values.length
}
pub fn main() -> I32 { return 0 }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const ownership = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(1)
    const loan = ownership?.loans.at(0)
    assert.strictEqual(loan?.origin, 'SliceReborrow')
    assert.strictEqual(loan?.parent?._tag, 'Parameter')
    assert.strictEqual(loan?.suspendsParent, true)
    assert.strictEqual(loan?.startRegion.ordinal, loan?.endRegion.ordinal)
  }),
)

it.effect('rejects moving a non-Copy value through a borrowed element place', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { value: I32 }
fn steal(values: &[Token], index: I32) -> Token { return move values[index] }
pub fn main() -> I32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0012'],
    )
  }),
)

it.effect('plans exactly one displaced cleanup for exclusive borrowed replacement', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { value: I32 }
struct Empty {}
fn replace(values: &mut [Token], index: I32) -> I32 {
  values[index] = Token { value: 42 }
  return values[index].value
}
fn clear(values: &mut [Empty], index: I32) -> I32 {
  values[index] = Empty {}
  return values.length
}
pub fn main() -> I32 { return 0 }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const replacements =
      Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(0)?.borrowedReplacements ?? []
    assert.strictEqual(replacements.length, 1)
    assert.strictEqual(replacements.at(0)?.displacedCleanup._tag, 'StructCleanup')
    const empty =
      Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(1)?.borrowedReplacements ?? []
    const cleanup = empty.at(0)?.displacedCleanup
    assert.strictEqual(cleanup?._tag, 'StructCleanup')
    if (cleanup?._tag === 'StructCleanup') {
      assert.deepEqual(cleanup.fields, [])
    }
  }),
)

it.effect('ends loop-body loans before continue and return cleanup boundaries', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn read(values: &[I32]) -> I32 { return 1 }
fn flowTest() -> I32 {
  let values = [1, 2]
  while false {
    let seen = read(&values)
    continue
  }
  return read(&values)
}
pub fn main() -> I32 { return 0 }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const ownership = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(1)
    const continuing = ownership?.exits.find((exit) => exit.kind === 'Continue')
    const returned = ownership?.exits.find(
      (exit) => exit.kind === 'Return' && exit.loanEnds.length > 0,
    )
    assert.deepEqual(continuing?.loanEnds, [])
    assert.deepEqual(
      returned?.loanEnds.map((loan) => loan.ordinal),
      [0],
    )
    assert.notStrictEqual(ownership?.loans.at(0)?.endRegion.ordinal, continuing?.region?.ordinal)
  }),
)
