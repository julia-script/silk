import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('rejects a shared vector read when one union member is move-only', () =>
  Effect.gen(function* () {
    const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.vector { Vector }
struct Guard { storage: Allocation }
struct Marker { value: i32 }
fn guarded(storage: Allocation) -> Guard | Marker { return Guard { storage: move storage } }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 1]>()
  let allocation = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let mut events = Vector.make<Guard | Marker>()
  let event = guarded(move allocation)
  let appended = run Vector.append<Guard | Marker>(&mut events, move event) |>
    Effect.provideMut(&mut allocator)
  let observed = Vector.get<Guard | Marker>(&events, 0)
  drop observed
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'vector-acceptance/move-only-union-read',
      ascii(source),
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'SEM0083',
    )
  }),
)
