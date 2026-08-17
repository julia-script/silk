import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Instances from '../src/Instances.js'
import * as SuspensionOwnership from '../src/SuspensionOwnership.js'

const encoder = new TextEncoder()

const snapshot = (source: string) =>
  Analysis.ofSourceRealized(
    'suspension-ownership/main',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )

const available = (self: Analysis.Snapshot): SuspensionOwnership.Module => {
  const ownership = Analysis.suspensionOwnershipOf(self)
  assert.strictEqual(ownership._tag, 'Available')
  if (ownership._tag === 'Available') return ownership.value
  throw new RangeError('expected suspension ownership')
}

const plansFor = (
  self: SuspensionOwnership.Module,
  declaration: string,
): ReadonlyArray<SuspensionOwnership.Plan> =>
  self.plans.filter((plan) => plan.function.declaration.name.startsWith(`${declaration}$effect$`))

const source = `struct Owner { value: i32 }
effect fn delayed(value: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  return run Effect.suspend(effect { return value })
}
effect fn scalar() -> i32 ! OutOfMemory ? &mut Allocator {
  let dead = 100
  return 40 + run delayed(2)
}
effect fn owned() -> i32 ! OutOfMemory ? &mut Allocator {
  let firstLayout = Layout.of<i32>()
  let first = run Allocator.allocate(move firstLayout)
  let secondLayout = Layout.of<i32>()
  let second = run Allocator.allocate(move secondLayout)
  let right = run delayed(2)
  return 8 + right
}
effect fn borrowed(owner: &mut Owner) -> i32 ! OutOfMemory ? &mut Allocator {
  let right = run delayed(2)
  return owner.value + right
}
effect fn branched(flag: bool, left: i32, right: i32) -> i32 ! OutOfMemory ? &mut Allocator {
  let value = run delayed(1)
  if flag { return left + value }
  return right + value
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let mut scalarAllocator = SystemAllocator.make()
  let scalarPending = scalar() |> Effect.provideMut(&mut scalarAllocator)
  let scalarValue = run Effect.catchAll(move scalarPending, recover)
  let mut ownedAllocator = SystemAllocator.make()
  let ownedPending = owned() |> Effect.provideMut(&mut ownedAllocator)
  let ownedValue = run Effect.catchAll(move ownedPending, recover)
  let mut owner = Owner { value: 20 }
  let mut borrowedAllocator = SystemAllocator.make()
  let borrowedPending = borrowed(&mut owner) |> Effect.provideMut(&mut borrowedAllocator)
  let borrowedValue = run Effect.catchAll(move borrowedPending, recover)
  let mut branchedAllocator = SystemAllocator.make()
  let branchedPending = branched(true, 30, 12) |> Effect.provideMut(&mut branchedAllocator)
  let branchedValue = run Effect.catchAll(move branchedPending, recover)
  return scalarValue + ownedValue + borrowedValue + branchedValue
}`

it.effect('classifies exact post-normalization MIR locals across relay', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const ownership = available(self)
    const scalar = plansFor(ownership, 'scalar').find((plan) => plan.frame === 'StatefulRelay')
    const owned = plansFor(ownership, 'owned').find(
      (plan) =>
        plan.frame === 'StatefulRelay' &&
        plan.slots.filter((slot) => slot.access._tag === 'AffineTransfer').length === 2,
    )
    const borrowed = plansFor(ownership, 'borrowed').find((plan) => plan.frame === 'StatefulRelay')
    const branched = plansFor(ownership, 'branched').find((plan) => plan.frame === 'StatefulRelay')
    assert.isDefined(scalar, SuspensionOwnership.encode(ownership))
    assert.isDefined(owned, SuspensionOwnership.encode(ownership))
    assert.isDefined(borrowed, SuspensionOwnership.encode(ownership))
    assert.isDefined(branched, SuspensionOwnership.encode(ownership))
    if (
      scalar === undefined ||
      owned === undefined ||
      borrowed === undefined ||
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
    const affine = owned.slots.filter((slot) => slot.access._tag === 'AffineTransfer')
    assert.lengthOf(affine, 2)
    const affineLocals = affine.map((slot) => slot.local.ordinal)
    assert.deepEqual(
      owned.allocationRefusal.releases.map((release) => release.local.ordinal),
      [...affineLocals].reverse(),
    )
    for (const rollback of owned.prefixRollbacks) {
      assert.deepEqual(
        [...rollback.frameDrops, ...rollback.sourceReleases]
          .map((release) => release.local.ordinal)
          .filter((local) => affineLocals.includes(local))
          .sort((left, right) => left - right),
        [...affineLocals].sort((left, right) => left - right),
      )
    }
    assert.deepEqual(
      owned.prefixRollbacks.at(-1)?.frameDrops.map((release) => release.local.ordinal),
      [...affineLocals].reverse(),
    )
    assert.lengthOf(borrowed.allocationRefusal.loanEnds, 1)
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

it.effect('publishes deterministic initialization and every prefix rollback', () =>
  Effect.gen(function* () {
    const first = yield* snapshot(source)
    const second = yield* snapshot(source)
    const left = available(first)
    const right = available(second)
    assert.strictEqual(SuspensionOwnership.encode(left), SuspensionOwnership.encode(right))
    for (const plan of left.plans) {
      assert.deepEqual(
        plan.initializationOrder,
        plan.slots.map((slot) => slot.ordinal),
        Instances.keyText(plan.function),
      )
      assert.deepEqual(
        plan.prefixRollbacks.map((rollback) => rollback.initialized),
        Array.from({ length: plan.slots.length + 1 }, (_value, ordinal) => ordinal),
      )
      assert.deepEqual(plan.success.restores, plan.initializationOrder)
    }
  }),
)
