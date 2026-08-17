import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * A slot lane narrower than four bytes shares its four-byte window with its neighbours, so a
 * fixed-width `i32.load` for the lane at index 0 swallows whatever the neighbour wrote. Each case
 * writes two elements inside one window and reads both back through `Slot.copy` and `Slot.take`:
 * the sum only survives when every lane load is exactly as wide as its element.
 */
interface Case {
  /** The element type the buffer is instantiated at. */
  readonly element: string
  /** The index sharing a four-byte window with index 0 at this element's stride. */
  readonly neighbour: number
  /** The value written at index 0. Negative for signed elements, to pin sign extension too. */
  readonly first: string
  /** The value written at `neighbour`. */
  readonly second: string
  /** `100 + 2 * first + 2 * second`, as the program computes it. */
  readonly expected: number
}

const cases: ReadonlyArray<Case> = [
  { element: 'u8', neighbour: 3, first: '7', second: '11', expected: 136 },
  { element: 'i8', neighbour: 3, first: '-7', second: '11', expected: 108 },
  { element: 'u16', neighbour: 1, first: '7', second: '11', expected: 136 },
  { element: 'i16', neighbour: 1, first: '-7', second: '11', expected: 108 },
  { element: 'u32', neighbour: 1, first: '7', second: '11', expected: 136 },
  { element: 'i32', neighbour: 1, first: '-7', second: '11', expected: 108 },
  { element: 'u64', neighbour: 1, first: '7', second: '11', expected: 136 },
  { element: 'i64', neighbour: 1, first: '-7', second: '11', expected: 108 },
  { element: 'usize', neighbour: 1, first: '7', second: '11', expected: 136 },
  { element: 'isize', neighbour: 1, first: '-7', second: '11', expected: 108 },
  { element: 'f32', neighbour: 1, first: '-7.0', second: '11.0', expected: 108 },
  { element: 'f64', neighbour: 1, first: '-7.0', second: '11.0', expected: 108 },
]

const program = (entry: Case): string => `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[${entry.element}; 4]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<${entry.element}>(move allocation, 4)
    let firstWritten = Slot.write<${entry.element}>(RawBuffer.slot(&mut buffer, 0), ${entry.first})
    let secondWritten = Slot.write<${entry.element}>(RawBuffer.slot(&mut buffer, ${entry.neighbour}), ${entry.second})
    let firstCopy = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let secondCopy = Slot.copy(RawBuffer.slot(&mut buffer, ${entry.neighbour}))
    let firstTake = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let secondTake = Slot.take(RawBuffer.slot(&mut buffer, ${entry.neighbour}))
    drop buffer
    return 100 + ${entry.element}.toI32(firstCopy) + ${entry.element}.toI32(secondCopy) + ${entry.element}.toI32(firstTake) + ${entry.element}.toI32(secondTake)
  }
  return 0
}

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

for (const entry of cases) {
  it.effect(`reads a ${entry.element} slot lane at its own width on every engine`, () =>
    Effect.gen(function* () {
      const source = program(entry)
      const name = `slot-lane-width/${entry.element}`
      const snapshot = yield* Analysis.ofSourceRealized(
        name,
        ascii(source),
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
      assert.strictEqual(evaluated.result.value, entry.expected)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), entry.expected)
    }),
  )
}

/** The reproduction as issue #114 states it: two `7u8` in one word, taken back and summed. */
const reported = `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[u8; 4]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<u8>(move allocation, 4)
    let firstWritten = Slot.write<u8>(RawBuffer.slot(&mut buffer, 0), 7)
    let secondWritten = Slot.write<u8>(RawBuffer.slot(&mut buffer, 3), 7)
    let first = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let second = Slot.take(RawBuffer.slot(&mut buffer, 3))
    drop buffer
    return u8.toI32(first) + u8.toI32(second)
  }
  return 0
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

it.effect('sums two u8 slots in one word to 14 on every engine', () =>
  Effect.gen(function* () {
    const name = 'slot-lane-width/reported'
    const snapshot = yield* Analysis.ofSourceRealized(
      name,
      ascii(reported),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 14)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 14)
  }),
)

/**
 * Reading a field through a reference lands in the same lane-load helper the slot reads use, so
 * a struct packed with sub-word fields catches a fixed-width load there the same way: every
 * field but the last would otherwise drag its neighbours in, and the signed ones pin the
 * sign-extending mnemonic rather than the zero-extending one.
 */
const packedReference = `struct Packed {
  first: u8
  second: i8
  third: u16
  fourth: i16
}

fn peek(self: &Packed) -> i32 {
  return u8.toI32(self.first) + i8.toI32(self.second) + u16.toI32(self.third) + i16.toI32(self.fourth)
}

pub fn main() -> i32 {
  let packed = Packed { first: 7, second: -5, third: 200, fourth: -9 }
  return 50 + peek(&packed)
}`

it.effect('reads a sub-word field through a reference at its own width on every engine', () =>
  Effect.gen(function* () {
    const name = 'slot-lane-width/packed-reference'
    const expected = 243
    const snapshot = yield* Analysis.ofSourceRealized(
      name,
      ascii(packedReference),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, expected)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), expected)
  }),
)
