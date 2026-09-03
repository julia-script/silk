import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as MirVerification from '../src/MirVerification.js'
import * as OwnershipEncoding from '../src/OwnershipEncoding.js'

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
    const self = yield* snapshot(`import silk.usize as usize
fn edit(values: &mut [i32]) -> i32 { return 1 }
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
      main?.regions.flatMap((region) => {
        if (region._tag === 'OperationRegion') return region.operations
        if (region._tag === 'CleanupRegion') return region.releases
        return []
      }) ?? []
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
    const self = yield* snapshot(`import silk.usize as usize
fn identity(values: &[i32]) -> &[i32] { return values }
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
    assert.include(
      ownership === undefined ? '' : OwnershipEncoding.encode(ownership),
      'returned-view',
    )
  }),
)

it.effect('retains equivalent returned-reference loans for direct and pipeline calls', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Counter { value: i32 }
fn identity(counter: &mut Counter) -> &mut Counter { return move counter }
fn directConflict() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut view = identity(&mut counter)
  let ownerRead = counter.value
  view.value = 2
  return ownerRead
}
fn pipelineConflict() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut view = &mut counter |> identity
  let ownerRead = counter.value
  view.value = 2
  return ownerRead
}
fn directRestored() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut view = identity(&mut counter)
  view.value = 2
  return counter.value
}
fn pipelineRestored() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut view = &mut counter |> identity
  view.value = 2
  return counter.value
}
pub fn main() -> i32 { return directRestored() + pipelineRestored() }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011', 'OWN0011'],
    )
    const ownership = Analysis.ownershipOf(self, 'slices/Ownership')
    for (const ordinal of [1, 2, 3, 4]) {
      const loan = ownership?.functions
        .at(ordinal)
        ?.loans.find((candidate) => candidate.origin === 'ReturnedView')
      assert.strictEqual(loan?.root._tag, 'Let')
      assert.strictEqual(loan?.access, 'Exclusive')
    }
  }),
)

it.effect('retains a returned reference sourced from an exact callable-section capture', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Counter { value: i32 }
fn select(delta: i32, counter: &mut Counter) -> &mut Counter {
  counter.value = counter.value + delta
  return move counter
}
fn conflict() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = select(&mut counter)
  let mut view = callback(1)
  counter.value = 20
  view.value = 42
  return counter.value
}
pub fn main() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = select(&mut counter)
  let mut view = callback(1)
  view.value = 42
  return counter.value
}`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011'],
    )
    assert.deepEqual(Hir.verify(Analysis.rootAnalysis(self).hir), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const main = Analysis.ownershipOf(self, 'slices/Ownership')?.functions.at(2)
    const capture = main?.loans.find((loan) => loan.origin === 'ReturnedCallableCapture')
    assert.strictEqual(capture?.access, 'Exclusive')
    assert.strictEqual(capture?.root._tag, 'Let')
    assert.strictEqual(
      capture === undefined ? false : capture.endSpan.end > capture.startSpan.end,
      true,
    )

    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag === 'Completed') assert.strictEqual(evaluated.result.value, 42n)

    const mir = Analysis.loweredMir(self)
    const mainMir = mir.functions.find((fn) => fn.id.name === 'main')
    const operations =
      mainMir?.regions.flatMap((region) => {
        if (region._tag === 'OperationRegion') return region.operations
        if (region._tag === 'CleanupRegion') return region.releases
        return []
      }) ?? []
    const ending = operations.findIndex((operation) => operation._tag === 'EndLoan')
    const restoredRead = operations.findLastIndex((operation) => operation._tag === 'ReadPlace')
    assert.isAtLeast(ending, 0)
    assert.isAbove(restoredRead, ending)
  }),
)

it.effect('tracks immutable callable aliases and rejects provenance after reassignment', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Counter { value: i32 }
fn identity(counter: &mut Counter) -> &mut Counter { return move counter }
fn select(delta: i32, counter: &mut Counter) -> &mut Counter {
  counter.value = counter.value + delta
  return move counter
}
fn aliased() -> i32 {
  let mut counter = Counter { value: 1 }
  let callback = identity
  let mut view = callback(&mut counter)
  counter.value = 20
  view.value = 42
  return counter.value
}
fn reassigned() -> i32 {
  let mut left = Counter { value: 1 }
  let mut right = Counter { value: 2 }
  let mut callback = select(&mut left)
  callback = select(&mut right)
  let mut view = callback(0)
  right.value = 20
  view.value = 42
  return right.value
}
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011', 'SEM0080', 'SEM0144'],
    )
  }),
)

it.effect('rejects every opaque callable route that returns a view', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Counter { value: i32 }
struct Left {}
struct Right {}
fn identity(counter: &mut Counter) -> &mut Counter { return move counter }
fn alternate(counter: &mut Counter) -> &mut Counter { return move counter }
fn ignore<T>(value: T) -> bool { drop value return false }
fn diverge() -> never { return diverge() }
fn select(delta: i32, counter: &mut Counter) -> &mut Counter {
  counter.value = counter.value + delta
  return move counter
}
fn beforeWrite() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  let view = callback(&mut counter)
  callback = alternate
  return view.value
}
fn snapshotAlias() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  let alias = callback
  callback = alternate
  let view = alias(&mut counter)
  return view.value
}
fn terminatingPaths(flag: bool) -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  if flag {
    let view = callback(&mut counter)
    return view.value
  } else {
    callback = alternate
    return 0
  }
}
fn matched(choice: Left | Right) -> i32 {
  let mut counter = Counter { value: 1 }
  let callback = match move choice {
    Left {} => identity
    Right {} => alternate
  }
  let view = callback(&mut counter)
  return view.value
}
fn invalidatedAlias() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  callback = alternate
  let alias = callback
  let view = alias(&mut counter)
  return view.value
}
fn grouped() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  callback = alternate
  let view = (callback)(&mut counter)
  return view.value
}
fn loopBackedge(flag: bool) -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  while flag {
    let value = callback(&mut counter).value
    callback = alternate
    drop value
    continue
  }
  return 0
}
fn terminatingLoop(flag: bool) -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  while flag {
    let view = callback(&mut counter)
    callback = alternate
    return view.value
  }
  return 0
}
fn conditionWrite() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  while ignore(Intrinsic.replace(callback, alternate)) { return 0 }
  let view = callback(&mut counter)
  return view.value
}
fn unreachableLoopWrite(flag: bool) -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  while flag {
    let impossible = diverge()
    callback = alternate
    drop impossible
  }
  let view = callback(&mut counter)
  return view.value
}
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      [
        'SEM0080',
        'SEM0080',
        'SEM0080',
        'SEM0080',
        'SEM0080',
        'SEM0144',
        'SEM0080',
        'SEM0144',
        'SEM0144',
        'SEM0080',
        'SEM0080',
        'SEM0144',
        'SEM0080',
      ],
    )
  }),
)

it.effect('isolates and rejects deferred mutation of a captured callable', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Counter { value: i32 }
fn identity(counter: &mut Counter) -> &mut Counter { return move counter }
fn alternate(counter: &mut Counter) -> &mut Counter { return move counter }
fn diverge() -> never { return diverge() }
fn simple(value: i32) -> i32 { return value }
fn alternateSimple(value: i32) -> i32 { return value + 1 }
fn make(value: i32) -> fn(i32) -> i32 { drop value return alternateSimple }
fn consume(value: i32, callback: fn(i32) -> i32) -> i32 {
  drop callback
  return value
}
fn assigned(flag: bool) -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  let pending = effect {
    if flag {
      callback = alternate
      return ()
    } else {
      return ()
    }
  }
  drop move pending
  let view = callback(&mut counter)
  return view.value
}
fn replaced() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  let pending = effect {
    let previous = Intrinsic.replace(callback, alternate)
    drop previous
    return ()
  }
  drop move pending
  let view = callback(&mut counter)
  return view.value
}
fn unreachable() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  let pending = effect {
    let impossible = diverge()
    callback = alternate
    drop impossible
    return ()
  }
  drop move pending
  let view = callback(&mut counter)
  return view.value
}
fn neverAssignment() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  let pending = effect {
    callback = diverge()
    return ()
  }
  drop move pending
  let view = callback(&mut counter)
  return view.value
}
fn neverReplace() -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  let pending = effect {
    let previous = Intrinsic.replace(callback, diverge())
    drop previous
    return ()
  }
  drop move pending
  let view = callback(&mut counter)
  return view.value
}
fn shortCircuit(flag: bool) -> i32 {
  let mut counter = Counter { value: 1 }
  let mut callback = identity
  let pending = effect {
    let skipped = flag && diverge()
    drop skipped
    callback = alternate
    return ()
  }
  drop move pending
  let view = callback(&mut counter)
  return view.value
}
fn eagerAssignment() -> i32 {
  let mut callback = simple
  let pending = effect {
    callback = make(diverge())
    return ()
  }
  drop move pending
  return callback(1)
}
fn eagerReplace() -> i32 {
  let mut callback = simple
  let pending = effect {
    let ignored = consume(diverge(), Intrinsic.replace(callback, alternateSimple))
    drop ignored
    return ()
  }
  drop move pending
  return callback(1)
}
pub fn main() -> i32 { return 0 }`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0145', 'SEM0145', 'SEM0012', 'SEM0145'],
    )
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
    const self = yield* snapshot(`import silk.usize as usize
struct Token { value: i32 }
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

it.effect('extends a view loan through a use nested in a place replace', () =>
  Effect.gen(function* () {
    // The view's last use sits inside Place.replace's value operand; the owner write between the
    // direct uses and that nested use must still count as access during the loan.
    const self = yield* snapshot(`pub fn main() -> i32 {
  let mut values = [1, 2]
  let view = &values
  let first = view[1]
  values[0] = 40
  let mut sink = 0
  let old = Intrinsic.replace(sink, view[0])
  return first + old + values[0]
}`)

    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['OWN0011'],
    )
  }),
)
