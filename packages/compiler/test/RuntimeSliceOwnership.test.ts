import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Ownership from '../src/Ownership.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (source: string) => Analysis.ofSourceRealized('slices/Ownership', ascii(source))

it.effect('records deterministic call-scoped loans and accepts shared aliases', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn compare(left: &[i32], right: &[i32]) -> i32 { return 1 }
fn valid() -> i32 { let values = [1, 2, 3] return compare(&values, &values) }
pub fn main() -> i32 { return valid() }`)

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
    const self = yield* snapshot(`fn mixed(left: &[i32], right: &mut [i32]) -> i32 { return 1 }
fn both(left: &mut [i32], right: &mut [i32]) -> i32 { return 1 }
fn observe(view: &mut [i32], value: i32) -> i32 { return value }
fn aliases() -> i32 { let mut values = [1, 2] return mixed(&values, &mut values) }
fn exclusives() -> i32 { let mut values = [1, 2] return both(&mut values, &mut values) }
fn later() -> i32 { let mut values = [1, 2] return observe(&mut values, values[0]) }
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0010', 'OWN0010', 'OWN0011'],
    )
  }),
)

it.effect('retains reborrow parent suspension and restores access after the call', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn edit(values: &mut [i32]) -> i32 { return 1 }
fn forward(values: &mut [i32]) -> i32 {
  let result = edit(&mut values)
  return usize.toI32(values.length)
}
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const ownership = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(1)
    const loan = ownership?.loans.at(0)
    assert.strictEqual(loan?.origin, 'SliceReborrow')
    assert.strictEqual(loan?.parent?._tag, 'Parameter')
    assert.strictEqual(loan?.suspendsParent, true)
    assert.strictEqual(loan?.startRegion.ordinal, loan?.endRegion.ordinal)
  }),
)

it.effect('ends lexical borrow bindings at last use and restores their owner', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn conflict() -> i32 {
  let mut values = [1, 2]
  let view = &values
  values[0] = 40
  return view[1]
}
pub fn main() -> i32 {
  let mut values = [1, 2]
  let mut view = &mut values
  view[0] = 40
  let first = view[0]
  values[1] = 2
  return first + values[1]
}`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011'],
    )
    const main = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(1)
    const loan = main?.loans.find((candidate) => candidate.origin === 'FixedArrayBorrow')
    assert.strictEqual(loan?.access, 'Exclusive')
    assert.strictEqual(loan === undefined ? false : loan.endSpan.end > loan.startSpan.end, true)

    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('ends a reusable callable capture loan after its last invocation', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn inspect(value: i32, values: &[i32]) -> i32 {
  return value + values[0]
}
pub fn main() -> i32 {
  let mut values = [1]
  let callback = inspect(&values)
  let observed = callback(1)
  values[0] = 40
  return observed + values[0]
}`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
  }),
)

it.effect('retains a callable capture loan when the callable is stored after invocation', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn inspect(value: i32, values: &[i32]) -> i32 {
  return value + values[0]
}
pub fn main() -> i32 {
  let mut values = [1]
  let callback = inspect(&values)
  let observed = callback(1)
  let stored = callback
  values[0] = 40
  drop stored
  return observed + values[0]
}`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011'],
    )
  }),
)

it.effect('cleans a hidden temporary owner after its loan ends', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Guard { value: i32 }
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () { return () }
}
fn read(values: &[Guard]) -> i32 { return values[0].value }
pub fn main() -> i32 { return read(&[Guard { value: 42 }]) }`)

    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.mirOf(self)
    assert.strictEqual(mir._tag, 'Available')
    if (mir._tag !== 'Available') return
    const main = mir.value.functions.find((fn) => fn.id.name === 'main')
    const operations =
      main?.regions.flatMap((region) =>
        region._tag === 'OperationRegion'
          ? region.operations
          : region._tag === 'CleanupRegion'
            ? region.releases
            : [],
      ) ?? []
    const ending = operations.findIndex((operation) => operation._tag === 'EndLoan')
    const cleanup = operations.findIndex(
      (operation, ordinal) => operation._tag === 'Drop' && ordinal > ending,
    )
    assert.isAtLeast(ending, 0)
    assert.isAbove(cleanup, ending)
  }),
)

it.effect('keeps a returned shared view live through its last use and then restores mutation', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn identity(values: &[i32]) -> &[i32] { return values }
fn conflict() -> i32 {
  let mut values = [1, 2]
  let view = identity(&values)
  values[0] = 3
  return usize.toI32(view.length)
}
fn restored() -> i32 {
  let mut values = [1, 2]
  let view = identity(&values)
  let length = usize.toI32(view.length)
  values[0] = 3
  return length
}
pub fn main() -> i32 { return restored() }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011'],
    )
    const conflict = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(1)
    const returned = conflict?.loans.find((loan) => loan.origin === 'ReturnedView')
    assert.strictEqual(returned?.root._tag, 'Let')
    assert.strictEqual(returned?.access, 'Shared')
    assert.strictEqual(
      returned === undefined ? false : returned.endSpan.end > returned.startSpan.end,
      true,
    )
    const restored = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(2)
    assert.strictEqual(
      restored?.loans.some((loan) => loan.origin === 'ReturnedView'),
      true,
    )
  }),
)

it.effect('suspends all owner access for an exclusive returned view until its last use', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`fn identity(values: &mut [i32]) -> &mut [i32] { return values }
fn conflict() -> i32 {
  let mut values = [1, 2]
  let mut view = identity(&mut values)
  let ownerRead = values[0]
  view[1] = 3
  return ownerRead
}
fn restored() -> i32 {
  let mut values = [1, 2]
  let mut view = identity(&mut values)
  view[1] = 3
  return values[1]
}
pub fn main() -> i32 { return restored() }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011'],
    )
    const conflict = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(1)
    assert.strictEqual(conflict?.loans.at(0)?.access, 'Exclusive')
    assert.strictEqual(conflict?.loans.at(0)?.origin, 'ReturnedView')
    const ownership = Analysis.ownershipOf(self, 'slices/Ownership')
    assert.include(ownership === undefined ? '' : Ownership.encode(ownership), 'returned-view')
  }),
)

it.effect('rejects moving or dropping an owner while a returned view is live', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { value: i32 }
fn identity(values: &[Token]) -> &[Token] { return values }
fn moved() -> i32 {
  let values = [Token { value: 1 }]
  let view = identity(&values)
  let consumed = move values
  return view[0].value
}
fn dropped() -> i32 {
  let values = [Token { value: 1 }]
  let view = identity(&values)
  drop values
  return view[0].value
}
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011', 'OWN0011'],
    )
  }),
)

it.effect('rejects moving a non-Copy value through a borrowed element place', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { value: i32 }
fn steal(values: &[Token], index: usize) -> Token { return move values[index] }
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0012'],
    )
  }),
)

it.effect('plans exactly one displaced cleanup for exclusive borrowed replacement', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { value: i32 }
struct Empty {}
fn replace(values: &mut [Token], index: usize) -> i32 {
  values[index] = Token { value: 42 }
  return values[index].value
}
fn clear(values: &mut [Empty], index: usize) -> i32 {
  values[index] = Empty {}
  return usize.toI32(values.length)
}
pub fn main() -> i32 { return 0 }`)

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
    const self = yield* snapshot(`fn read(values: &[i32]) -> i32 { return 1 }
fn flowTest() -> i32 {
  let values = [1, 2]
  while false {
    let seen = read(&values)
    continue
  }
  return read(&values)
}
pub fn main() -> i32 { return 0 }`)

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
