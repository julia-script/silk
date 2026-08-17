import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
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
  let recipe = Effect.catchAll(store(), recover)
  return run recipe
}`

const sharedReadSource = `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 1]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 1)
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), 21)
    let first = RawBuffer.read<i32>(&buffer, 0)
    let second = RawBuffer.read<i32>(&buffer, 0)
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop buffer
    return first + second + taken
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

const nonCopyReadSource = `struct Guard { storage: Allocation }
effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[Guard; 1]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let innerLayout = Layout.of<[i32; 1]>()
  let innerRecipe = Allocator.allocate(move innerLayout) |> Effect.provideMut(&mut allocator)
  let payload = run innerRecipe
  unsafe {
    let mut buffer = RawBuffer.from<Guard>(move allocation, 1)
    let element = Guard { storage: move payload }
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), move element)
    let copied = RawBuffer.read<Guard>(&buffer, 0)
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop copied
    drop taken
    drop buffer
    return 42
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

const unionReadSource = `struct Left { value: i32 }
struct Right { value: i32 }
fn left(value: i32) -> Left | Right { return Left { value: value } }
effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[Left | Right; 1]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<Left | Right>(move allocation, 1)
    let element = left(42)
    let written = Slot.write<Left | Right>(RawBuffer.slot(&mut buffer, 0), move element)
    let copied = RawBuffer.read<Left | Right>(&buffer, 0)
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop taken
    drop buffer
    return match move copied { Left { value } => value Right { value } => value }
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

const moveOnlyUnionReadSource = `struct Guard { storage: Allocation }
struct Marker { value: i32 }

fn guarded(storage: Allocation) -> Guard | Marker {
  return Guard { storage: move storage }
}

effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[Guard | Marker; 1]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let innerLayout = Layout.of<[i32; 1]>()
  let innerRecipe = Allocator.allocate(move innerLayout) |> Effect.provideMut(&mut allocator)
  let payload = run innerRecipe
  unsafe {
    let mut buffer = RawBuffer.from<Guard | Marker>(move allocation, 1)
    let event = guarded(move payload)
    let written = Slot.write<Guard | Marker>(RawBuffer.slot(&mut buffer, 0), move event)
    let copied = RawBuffer.read<Guard | Marker>(&buffer, 0)
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop copied
    drop taken
    drop buffer
    return 42
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

const unsafeProgram = (
  body: string,
  layout = '[i32; 2]',
): string => `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<${layout}>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
${body}
  }
  return 0
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  return run Effect.catchAll(store(), recover)
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

it.effect('reads initialized Copy storage repeatedly through shared RawBuffer borrows', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'owned-allocation/shared-read',
      ascii(sharedReadSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const mir = Analysis.loweredMir(snapshot)
    assert.deepEqual(Mir.verify(mir), [])
    const operations = mir.functions.flatMap(Mir.operations)
    assert.strictEqual(
      operations.filter((operation) => operation._tag === 'RawBufferRead').length,
      1,
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 63)
    assert.deepEqual(
      Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      [
        'AllocationAcquire',
        'RawBufferForm',
        'SlotProject',
        'SlotWrite',
        'RawBufferRead',
        'RawBufferRead',
        'SlotProject',
        'SlotTake',
        'AllocationRelease',
      ],
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 63)
    const nativeSnapshot = yield* Analysis.ofSourceRealized(
      'owned-allocation/shared-read-native',
      ascii(sharedReadSource),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(nativeSnapshot), [])
    const native = yield* Analysis.codegen(nativeSnapshot, { mode: 'release' })
    assert.include(native.ir, 'raw_read')
  }),
)

it.effect('reads an all-Copy structural union through a shared RawBuffer borrow', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'owned-allocation/read-structural-union',
      ascii(unionReadSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
    assert.strictEqual(evaluated.trace.filter((event) => event._tag === 'RawBufferRead').length, 1)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('rejects shared RawBuffer reads of move-only nominal and union elements', () =>
  Effect.gen(function* () {
    for (const [name, source] of [
      ['move-only', nonCopyReadSource],
      ['move-only-union', moveOnlyUnionReadSource],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `owned-allocation/read-${name}`,
        ascii(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Blocked')
      if (evaluated._tag !== 'Blocked') continue
      assert.strictEqual(evaluated.reason._tag, 'InvalidMir')
      if (evaluated.reason._tag !== 'InvalidMir') continue
      assert.isTrue(
        evaluated.reason.violations.some(
          (violation) =>
            violation.rule === 'InvalidRawStorageOperation' &&
            violation.detail.includes('RawBuffer.read'),
        ),
      )
    }
  }),
)

it.effect('requires shared rather than exclusive access for RawBuffer.read', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'owned-allocation/read-exclusive',
      ascii(unsafeProgram('    return RawBuffer.read<i32>(&mut buffer, 0)')),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0056',
    )
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

it.effect('traps on an out-of-bounds shared RawBuffer read', () =>
  expectTrap(
    'owned-allocation/read-out-of-bounds',
    unsafeProgram('    return RawBuffer.read<i32>(&buffer, 2)'),
    'RawBuffer read index is out of bounds',
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
