import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** Copy reads the same initialized slot twice without consuming it; take still works after. */
const copyRead = `effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), 21)
    let first = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let second = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop buffer
    return first + second - taken + 21
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

/** Slot and shared-buffer copies preserve the active member across distinct union payload shapes. */
const unionCopyRead = `struct Left { value: i32 }
struct Right {
  marker: u8
  value: i32
}
struct EmptyEvent {}

fn left(value: i32) -> EmptyEvent | Left | Right { return Left { value: value } }
fn right(marker: u8, value: i32) -> EmptyEvent | Left | Right {
  return Right { marker: marker, value: value }
}
fn empty() -> EmptyEvent | Left | Right { return EmptyEvent {} }

fn observed(input: EmptyEvent | Left | Right) -> i32 {
  return match move input {
    EmptyEvent {} => 5
    Left { value: leftValue } => leftValue
    Right { marker, value: rightValue } => u8.toI32(marker) + rightValue
  }
}

effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[EmptyEvent | Left | Right; 3]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<EmptyEvent | Left | Right>(move allocation, 3)
    let leftEvent = left(7)
    let rightEvent = right(3, 11)
    let emptyEvent = empty()
    let leftWritten = Slot.write<EmptyEvent | Left | Right>(RawBuffer.slot(&mut buffer, 0), move leftEvent)
    let rightWritten = Slot.write<EmptyEvent | Left | Right>(RawBuffer.slot(&mut buffer, 1), move rightEvent)
    let emptyWritten = Slot.write<EmptyEvent | Left | Right>(RawBuffer.slot(&mut buffer, 2), move emptyEvent)
    let slotCopy = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let sharedLeft = RawBuffer.read<EmptyEvent | Left | Right>(&buffer, 0)
    let sharedRight = RawBuffer.read<EmptyEvent | Left | Right>(&buffer, 1)
    let sharedEmpty = RawBuffer.read<EmptyEvent | Left | Right>(&buffer, 2)
    let takenLeft = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let takenRight = Slot.take(RawBuffer.slot(&mut buffer, 1))
    let takenEmpty = Slot.take(RawBuffer.slot(&mut buffer, 2))
    drop buffer
    return observed(move slotCopy) + observed(move sharedLeft) + observed(move sharedRight) +
      observed(move sharedEmpty) + observed(move takenLeft) + observed(move takenRight) +
      observed(move takenEmpty)
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('copies initialized Copy slots without consuming them on the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'slot-copy/read',
      ascii(copyRead),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'SlotCopy').length, 2)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'copies all-Copy structural unions through Slot and shared aliases on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'slot-copy/structural-union',
        ascii(unionCopyRead),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? `${value}n` : value)),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 59n)
      assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'SlotCopy').length, 1)
      assert.strictEqual(
        evaluated.trace.filter((event) => event._tag === 'RawBufferRead').length,
        3,
      )
      const firstCopy = evaluated.trace.findIndex((event) => event._tag === 'SlotCopy')
      const firstTake = evaluated.trace.findIndex((event) => event._tag === 'SlotTake')
      assert.isAtLeast(firstCopy, 0)
      assert.isAbove(firstTake, firstCopy)
      assert.deepEqual(
        evaluated.trace
          .slice(firstCopy, firstTake)
          .filter((event) =>
            [
              'AllocationAcquire',
              'AllocationRelease',
              'SlotCopy',
              'RawBufferRead',
              'SlotDrop',
            ].includes(event._tag),
          )
          .map((event) => event._tag),
        ['SlotCopy', 'RawBufferRead', 'RawBufferRead', 'RawBufferRead'],
      )
      const encoded = Mir.encode(Analysis.loweredMir(snapshot))
      assert.include(encoded, 'slot-copy')
      assert.include(encoded, 'raw-buffer-read')
      const repeated = yield* Analysis.ofSourceRealized(
        'slot-copy/structural-union',
        ascii(unionCopyRead),
        'wasm32-unknown-unknown',
      )
      assert.strictEqual(encoded, Mir.encode(Analysis.loweredMir(repeated)))

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 59)
    }),
)

/** Copying a non-Copy element is rejected when the concrete instantiation is verified. */
const nonCopy = `struct Guard { storage: Allocation }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[Guard; 1]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let inner = Layout.of<[i32; 1]>()
  let innerRecipe = Allocator.allocate(move inner) |> Effect.provideMut(&mut allocator)
  let payload = run innerRecipe
  unsafe {
    let mut buffer = RawBuffer.from<Guard>(move allocation, 1)
    let element = Guard { storage: move payload }
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), move element)
    let copied = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop taken
    drop copied
    drop buffer
    return 42
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('rejects copying a non-Copy element at MIR verification', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized('slot-copy/non-copy', ascii(nonCopy))
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Blocked')
    if (evaluated._tag !== 'Blocked') return
    assert.strictEqual(evaluated.reason._tag, 'InvalidMir')
  }),
)
