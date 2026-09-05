import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Instances from '../src/Instances.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SuspensionOwnership from '../src/SuspensionOwnership.js'
import * as Projections from './support/projections.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized(
    'suspension-ownership/main',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )

const available = (self: Analysis.Snapshot): SuspensionOwnership.Module => {
  const ownership = Projections.suspensionOwnershipOf(self)
  assert.strictEqual(ownership._tag, 'Available')
  if (ownership._tag === 'Available') return ownership.value
  throw new RangeError('expected suspension ownership')
}

const plansFor = (
  self: SuspensionOwnership.Module,
  declaration: string,
): ReadonlyArray<SuspensionOwnership.Plan> =>
  self.plans.filter((plan) => plan.function.declaration.name.startsWith(`${declaration}$effect$`))

const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
struct Owner { value: i32 }
effect fn delayed(value: i32) -> i32 {
  return run Effect.suspend(effect { return value })
}
effect fn scalar() -> i32 {
  let dead = 100
  return 40 + run delayed(2)
}
effect fn owned() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let firstLayout = Layout.of<i32>()
  let first = run Allocator.allocate(move firstLayout)
  let secondLayout = Layout.of<i32>()
  let second = run Allocator.allocate(move secondLayout)
  let right = run delayed(2)
  return 8 + right
}
effect fn borrowed(owner: &mut Owner) -> i32 {
  let right = run delayed(2)
  return owner.value + right
}
effect fn shared(owner: &Owner) -> i32 {
  let right = run delayed(2)
  return owner.value + right
}
effect fn branched(flag: bool, left: i32, right: i32) -> i32 {
  let value = run delayed(1)
  if flag { return left + value }
  return right + value
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 {
  let scalarValue = run scalar()
  let mut ownedAllocator = Allocator.systemAllocatorProvider()
  let ownedPending = owned() |> Effect.provideMut(&mut ownedAllocator)
  let ownedValue = run Effect.catchAll(move ownedPending, recover)
  let mut owner = Owner { value: 20 }
  let borrowedValue = run borrowed(&mut owner)
  let sharedValue = run shared(&owner)
  let branchedValue = run branched(true, 30, 12)
  return scalarValue + ownedValue + borrowedValue + sharedValue + branchedValue
}`

it.effect('classifies exact post-normalization MIR locals across relay', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const ownership = available(self)
    const sharedStart = source.indexOf(' run shared(&owner)')
    const caller = Analysis.loweredMir(self).functions.find((fn) => fn.id.name === 'main')
    const loan =
      caller === undefined
        ? undefined
        : MirVerification.operations(caller).find(
            (operation) =>
              operation._tag === 'BeginLoan' &&
              operation.access === 'Shared' &&
              operation.sourceType._tag === 'Nominal' &&
              operation.sourceType.type.name === 'Owner',
          )
    const callerPlan = ownership.plans.find((plan) => plan.span.start === sharedStart)
    assert.isTrue(
      loan?._tag === 'BeginLoan' &&
        callerPlan?.slots.some(
          (slot) => slot.local.ordinal === loan.root.ordinal && slot.type._tag === 'Nominal',
        ),
      'the last shared loan retains its actual owner storage independently of cleanup',
    )
    const scalar = plansFor(ownership, 'scalar').find((plan) => plan.frame === 'StatefulRelay')
    const owned = plansFor(ownership, 'owned').find(
      (plan) =>
        plan.frame === 'StatefulRelay' &&
        plan.slots.filter((slot) => slot.access._tag === 'AffineTransfer').length === 2,
    )
    const borrowed = plansFor(ownership, 'borrowed').find((plan) => plan.frame === 'StatefulRelay')
    const shared = plansFor(ownership, 'shared').find((plan) => plan.frame === 'StatefulRelay')
    const branched = plansFor(ownership, 'branched').find((plan) => plan.frame === 'StatefulRelay')
    assert.isDefined(scalar, SuspensionOwnership.encode(ownership))
    assert.isDefined(owned, SuspensionOwnership.encode(ownership))
    assert.isDefined(borrowed, SuspensionOwnership.encode(ownership))
    assert.isDefined(shared, SuspensionOwnership.encode(ownership))
    assert.isDefined(branched, SuspensionOwnership.encode(ownership))
    if (
      scalar === undefined ||
      owned === undefined ||
      borrowed === undefined ||
      shared === undefined ||
      branched === undefined
    )
      return
    const copiedScalars = scalar.slots.filter(
      (slot) => slot.access._tag === 'Copy' && slot.type._tag === 'i32',
    )
    assert.lengthOf(copiedScalars, 1, SuspensionOwnership.encode(ownership))
    assert.isTrue(
      owned.slots.some(
        (slot) => slot.access._tag === 'AffineTransfer' && slot.type._tag === 'Nominal',
      ),
      SuspensionOwnership.encode(ownership),
    )
    assert.isTrue(
      borrowed.slots.some(
        (slot) =>
          slot.access._tag === 'BorrowedDependency' &&
          slot.access.access === 'Exclusive' &&
          slot.access.loan._tag === 'BorrowedParameter',
      ),
      SuspensionOwnership.encode(ownership),
    )
    assert.isTrue(
      shared.slots.some(
        (slot) =>
          slot.access._tag === 'BorrowedDependency' &&
          slot.access.access === 'Shared' &&
          slot.access.loan._tag === 'BorrowedParameter',
      ),
      SuspensionOwnership.encode(ownership),
    )
    const affine = owned.slots.filter((slot) => slot.access._tag === 'AffineTransfer')
    assert.lengthOf(affine, 2)
    const affineLocals = affine.map((slot) => slot.local.ordinal)
    assert.deepEqual(
      owned.failure.releases
        .map((release) => release.local.ordinal)
        .filter((local) => affineLocals.includes(local)),
      [...affineLocals].reverse(),
    )
    // Only the user borrow is retained; private frame storage is not a source dependency.
    assert.lengthOf(
      borrowed.slots.filter((slot) => slot.access._tag === 'BorrowedDependency'),
      1,
    )
    assert.lengthOf(borrowed.failure.loanEnds, 1)
    assert.lengthOf(borrowed.success.loanEnds, 0)
    assert.deepEqual(
      branched.slots
        .filter((slot) => slot.local.ordinal < 3 && slot.access._tag === 'Copy')
        .map((slot) => slot.local.ordinal),
      [0, 1, 2],
    )
    assert.deepEqual(ownership.violations, [])
  }),
)

it.effect('publishes deterministic state ownership and restoration', () =>
  Effect.gen(function* () {
    const first = yield* snapshot(source)
    const second = yield* snapshot(source)
    const left = available(first)
    const right = available(second)
    assert.strictEqual(SuspensionOwnership.encode(left), SuspensionOwnership.encode(right))
    for (const plan of left.plans) {
      assert.deepEqual(
        plan.success.restores,
        plan.slots.map((slot) => slot.ordinal),
        Instances.keyText(plan.function),
      )
    }
  }),
)

it.effect('reports partial owner suspension during ordinary realized checking', () =>
  Effect.gen(function* () {
    const source = `struct Token { value: i32 }
impl Drop for Token { fn drop(self: &mut Token) -> () { return () } }
struct Pair { left: Token right: Token }
effect fn delayed() -> i32 { return run Intrinsic.suspendEffect(effect { return 2 }) }
effect fn partial() -> i32 {
  let owner = Pair { left: Token { value: 1 }, right: Token { value: 2 } }
  let extracted = move owner.left
  drop extracted
  let result = run delayed()
  return owner.right.value + result
}
effect fn complete() -> i32 {
  let owner = Pair { left: Token { value: 1 }, right: Token { value: 2 } }
  let result = run delayed()
  return owner.left.value + owner.right.value + result
}
pub fn main() -> i32 { let first = run partial() return first + run complete() }`
    const self = yield* snapshot(source)
    const start = source.indexOf(' run delayed()')
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => ({
        code: diagnostic.code,
        start: diagnostic.span.start,
        end: diagnostic.span.end,
      })),
      [{ code: 'OWN0020', start, end: start + ' run delayed()'.length }],
    )
    assert.strictEqual(self.mir._tag, 'Unavailable')
  }),
)
