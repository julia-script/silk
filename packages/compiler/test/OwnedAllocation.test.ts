import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nonCopyReadSource = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
struct Guard { storage: Allocation }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
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
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

const moveOnlyUnionReadSource = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
struct Guard { storage: Allocation }
struct Marker { value: i32 }

fn guarded(storage: Allocation) -> Guard | Marker {
  return Guard { storage: move storage }
}

effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
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
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

const unsafeProgram = (
  body: string,
  layout = '[i32; 2]',
): string => `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<${layout}>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
${body}
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 {
  return run Effect.catchAll(store(), recover)
}`

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
      assert.include(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        'SEM0083',
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
