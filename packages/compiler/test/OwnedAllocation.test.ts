import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), 41)
    let result = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop buffer
    return result
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  let recipe = Effect.catch<OutOfMemory>(store(), recover)
  return run recipe
}`

const unsafeProgram = (
  body: string,
  layout = '[i32; 2]',
): string => `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<${layout}>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
${body}
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  return run Effect.catch<OutOfMemory>(store(), recover)
}`

const expectTrap = Effect.fnUntraced(function* (name: string, source: string, reason: string) {
  const snapshot = yield* Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')
  assert.deepEqual(Analysis.diagnostics(snapshot), [])
  const evaluated = Analysis.evaluate(snapshot)
  assert.strictEqual(evaluated._tag, 'Blocked')
  if (evaluated._tag !== 'Blocked') return
  assert.strictEqual(evaluated.reason._tag, 'Trap', JSON.stringify(evaluated.reason))
  if (evaluated.reason._tag === 'Trap') assert.strictEqual(evaluated.reason.reason, reason)
})

it.effect('moves one allocation through RawBuffer and lexical Slot operations', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'owned-allocation/raw-buffer',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const mir = Analysis.loweredMir(snapshot)
    assert.deepEqual(Mir.verify(mir), [])
    const operations = mir.functions.flatMap(Mir.operations)
    assert.include(
      operations.map((operation) => operation._tag),
      'RawBufferFrom',
    )
    assert.include(
      operations.map((operation) => operation._tag),
      'RawBufferSlot',
    )
    assert.include(
      operations.map((operation) => operation._tag),
      'SlotWrite',
    )
    assert.include(
      operations.map((operation) => operation._tag),
      'SlotTake',
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 41)
    assert.deepEqual(
      Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      [
        'AllocationAcquire',
        'RawBufferForm',
        'SlotProject',
        'SlotWrite',
        'SlotProject',
        'SlotTake',
        'AllocationRelease',
      ],
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main as () => number
    assert.strictEqual(main(), 41)
    const nativeSnapshot = yield* Analysis.ofSourceRealized(
      'owned-allocation/raw-buffer-native',
      ascii(source),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(nativeSnapshot), [])
    const native = yield* Analysis.codegen(nativeSnapshot, { mode: 'release' })
    assert.include(native.ir, '@malloc')
    assert.include(native.ir, '@free')
  }),
)

it.effect('traps when RawBuffer provenance does not match its allocation layout', () =>
  expectTrap(
    'owned-allocation/layout-mismatch',
    unsafeProgram('    return 0', '[i32; 1]'),
    'RawBuffer allocation layout does not match its element type and count',
  ),
)

it.effect('traps on an out-of-bounds Slot projection', () =>
  expectTrap(
    'owned-allocation/out-of-bounds',
    unsafeProgram('    return Slot.take(RawBuffer.slot(&mut buffer, 2))'),
    'RawBuffer slot index is out of bounds',
  ),
)

it.effect('traps when Slot.take observes storage that was never initialized', () =>
  expectTrap(
    'owned-allocation/take-uninitialized',
    unsafeProgram('    return Slot.take(RawBuffer.slot(&mut buffer, 0))'),
    'Slot.take requires live initialized storage',
  ),
)

it.effect('traps when Slot.write overwrites initialized storage', () =>
  expectTrap(
    'owned-allocation/duplicate-write',
    unsafeProgram(
      '    let first = Slot.write(RawBuffer.slot(&mut buffer, 0), 41)\n    let second = Slot.write(RawBuffer.slot(&mut buffer, 0), 42)\n    return 0',
    ),
    'Slot.write requires live uninitialized storage',
  ),
)
