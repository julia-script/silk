import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'

const encoder = new TextEncoder()

const key = (actor: string, operation: string): string => `${actor}.${operation}`

const operationKeys = (snapshot: Analysis.Snapshot): ReadonlyArray<string> =>
  [...snapshot.semanticOccurrences.modules.values()].flatMap((module) =>
    module.occurrences.flatMap((occurrence) =>
      occurrence.resolution._tag === 'Available' &&
      occurrence.resolution.identity._tag === 'IntrinsicOperationIdentity'
        ? [key(occurrence.resolution.identity.id.actor, occurrence.resolution.identity.id.name)]
        : [],
    ),
  )

const acceptedSources = Object.freeze([
  `pub fn main() -> I32 {
  let i00 = I32.negate(1)
  let i01 = I32.add(1, 2)
  let i02 = I32.subtract(2, 1)
  let i03 = I32.multiply(2, 3)
  let i04 = I32.divide(4, 2)
  let i05 = I32.remainder(5, 2)
  let i06 = I32.equals(1, 1)
  let i07 = I32.notEquals(1, 2)
  let i08 = I32.lessThan(1, 2)
  let i09 = I32.lessOrEqual(1, 2)
  let i10 = I32.greaterThan(2, 1)
  let i11 = I32.greaterOrEqual(2, 1)
  let u00 = Usize.add(1, 2)
  let u01 = Usize.subtract(2, 1)
  let u02 = Usize.multiply(2, 3)
  let u03 = Usize.divide(4, 2)
  let u04 = Usize.remainder(5, 2)
  let u05 = Usize.equals(1, 1)
  let u06 = Usize.notEquals(1, 2)
  let u07 = Usize.lessThan(1, 2)
  let u08 = Usize.lessOrEqual(1, 2)
  let u09 = Usize.greaterThan(2, 1)
  let u10 = Usize.greaterOrEqual(2, 1)
  let b00 = Bool.equals(true, false)
  let b01 = Bool.notEquals(true, false)
  let b02 = Bool.not(false)
  let layout = Layout.of<I32>()
  let repeated = Layout.repeat(move layout, 2)
  let made = Layout.make(4, 4)
  let allocator = SystemAllocator.make()
  let unit = Unit.make()
  return i00
}`,
  `effect fn storage() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[I32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<I32>(move allocation, 2)
    let count = RawBuffer.count(&buffer)
    let firstWrite = Slot.write(RawBuffer.slot(&mut buffer, 0), 21)
    let secondWrite = Slot.write(RawBuffer.slot(&mut buffer, 1), 21)
    let copied = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let dropped = Slot.drop(RawBuffer.slot(&mut buffer, 1))
    drop buffer
    return copied + taken
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> I32 { return 0 }
pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(storage(), recover) }`,
  `struct Problem {}
struct Clock {}
effect fn succeed(value: I32) -> I32 { return value }
effect fn double(value: I32) -> I32 { return value * 2 }
effect fn observe(value: I32) -> Unit { return Unit.make() }
effect fn risky() -> I32 ! Problem { fail Problem {} }
effect fn recover(error: Problem) -> I32 { return 1 }
effect fn read() -> I32 ? &Clock { return 20 }
effect fn acquire() -> Clock { return Clock {} }
pub fn main() -> I32 {
  let mapped = succeed(1) |> Effect.map(I32.add(1))
  let chained = mapped |> Effect.flatMap(double)
  let tapped = chained |> Effect.tap(observe)
  let retried = tapped |> Effect.retry(1)
  let handled = risky() |> Effect.catch<Problem>(recover)
  let clock = Clock {}
  let provided = read() |> Clock.provide(&clock)
  let acquired = read() |> Clock.provideWith(acquire())
  let retriedValue = run retried
  let handledValue = run handled
  let providedValue = run provided
  let acquiredValue = run acquired
  return retriedValue + handledValue + providedValue + acquiredValue
}`,
  `struct Counter { value: I32 }
fn replace(self: &mut Counter) -> I32 { return Place.replace(self.value, 42) }
pub fn main() -> I32 {
  let mut counter = Counter { value: 1 }
  return replace(&mut counter)
}`,
])

it.effect('pairs every intrinsic presentation with accepted semantic analysis', () =>
  Effect.gen(function* () {
    const observed = new Set<string>()
    for (const [ordinal, source] of acceptedSources.entries()) {
      const snapshot = yield* Analysis.ofSource(
        `intrinsic/accepted-${ordinal}`,
        encoder.encode(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], `accepted intrinsic fixture ${ordinal}`)
      for (const operation of operationKeys(snapshot)) observed.add(operation)
    }
    const catalog = Intrinsic.all().flatMap((actor) =>
      actor.operations.map((operation) => key(actor.spelling, operation.spelling)),
    )
    assert.deepEqual([...observed].sort(), [...catalog].sort())
  }),
)

it.effect('keeps every intrinsic identifiable and presentable in rejected calls', () =>
  Effect.gen(function* () {
    for (const actor of Intrinsic.all())
      for (const operation of actor.operations) {
        const arguments_ = operation.parameters.length === 0 ? '0' : ''
        const source = `pub fn main() -> I32 { let rejected = ${actor.spelling}.${operation.spelling}(${arguments_}) return 0 }`
        const snapshot = yield* Analysis.ofSource(
          `intrinsic/rejected-${actor.spelling}-${operation.spelling}`,
          encoder.encode(source),
        )
        assert.isAbove(Analysis.diagnostics(snapshot).length, 0)
        assert.include(operationKeys(snapshot), key(actor.spelling, operation.spelling))
        assert.include(Intrinsic.signature(operation), operation.spelling)
      }
  }),
)

it('keeps catalog ordering stable across fresh reads', () => {
  const first = Intrinsic.all().map((actor) => ({
    actor: actor.spelling,
    operations: actor.operations.map((operation) => operation.spelling),
  }))
  const second = Intrinsic.all().map((actor) => ({
    actor: actor.spelling,
    operations: actor.operations.map((operation) => operation.spelling),
  }))
  assert.deepEqual(second, first)
})
