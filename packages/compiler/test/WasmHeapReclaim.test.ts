import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as StandardStreams from '../src/StandardStreams.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const pagesOf = (instance: WebAssembly.Instance): number => {
  const memory = instance.exports[StandardStreams.wasmMemoryExport]
  assert.instanceOf(memory, WebAssembly.Memory)
  if (!(memory instanceof WebAssembly.Memory)) return Number.POSITIVE_INFINITY
  return memory.buffer.byteLength / 65536
}

/**
 * An allocate-and-drop loop whose drops are deliberately not the reverse of its allocations: `a`
 * is released while the younger `b` is still live, and `b` while the younger `c` is. A bump
 * pointer that only unwinds its most recent block reclaims one of the three per iteration, so this
 * program separates a real free list from a LIFO unwind — both of which pass a nested loop.
 *
 * Each block is 512 payload bytes, so a thousand iterations acquire roughly 1.5 MiB. The heap that
 * serves it stays inside a couple of pages only if released blocks come back.
 */
const interleaved = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut index = 0
  while index < 1000 {
    let firstLayout = Layout.of<[i64; 64]>()
    let firstPending = Allocator.allocate(move firstLayout) |> Effect.provideMut(&mut allocator)
    let first = run firstPending
    let secondLayout = Layout.of<[i64; 64]>()
    let secondPending = Allocator.allocate(move secondLayout) |> Effect.provideMut(&mut allocator)
    let second = run secondPending
    drop first
    let thirdLayout = Layout.of<[i64; 64]>()
    let thirdPending = Allocator.allocate(move thirdLayout) |> Effect.provideMut(&mut allocator)
    let third = run thirdPending
    drop second
    drop third
    index = index + 1
  }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'keeps the Wasm heap bounded across interleaved non-LIFO allocate and drop cycles',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'wasm-heap-reclaim/interleaved',
        ascii(interleaved),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

      // Three thousand blocks of 512 bytes passed through this heap. A module that never reclaims
      // needs upwards of twenty pages to hold them; one that does needs the two it started with.
      assert.isAtMost(pagesOf(instance), 4)
    }),
  60_000,
)

/**
 * The same interleaving, run long enough that the bound has to come from reuse rather than from
 * slack: ten times the cycles must not cost a single extra page over the shorter run.
 */
const sustained = interleaved.replace('index < 1000', 'index < 10000')

it.effect(
  'holds the same Wasm heap bound when the interleaved cycle count grows tenfold',
  () =>
    Effect.gen(function* () {
      const shorter = yield* Analysis.ofSourceRealized(
        'wasm-heap-reclaim/interleaved',
        ascii(interleaved),
        'wasm32-unknown-unknown',
      )
      const longer = yield* Analysis.ofSourceRealized(
        'wasm-heap-reclaim/sustained',
        ascii(sustained),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(longer), [])

      const shorterWasm = yield* Analysis.codegenWasm(shorter, { mode: 'release' })
      const longerWasm = yield* Analysis.codegenWasm(longer, { mode: 'release' })
      const shorterInstance = new WebAssembly.Instance(
        new WebAssembly.Module(shorterWasm.bytes.slice()),
        {},
      )
      const longerInstance = new WebAssembly.Instance(
        new WebAssembly.Module(longerWasm.bytes.slice()),
        {},
      )
      assert.strictEqual((shorterInstance.exports.silk_main as () => number)(), 42)
      assert.strictEqual((longerInstance.exports.silk_main as () => number)(), 42)
      assert.strictEqual(pagesOf(longerInstance), pagesOf(shorterInstance))
    }),
  120_000,
)

/**
 * Count parity is a separate property from memory parity: the acquire and release counts a
 * provider folds into an ordinary Silk value are backend-independent, and were already equal
 * before either backend reclaimed anything. This test pins the count Wasm reports, and says
 * nothing about how much memory the engine holds while doing it.
 */
const counted = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.usize as usize
import silk.metrics {
  AllocationMetrics,
  copy as copyMetrics,
  make as zeroedMetrics,
  recordAcquire,
  recordRelease
}

struct CountingAllocator {
  inner: SystemAllocator
  metrics: AllocationMetrics
}

effect fn allocate(self: &mut CountingAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut self.inner)
  let block = run pending
  recordAcquire(&mut self.metrics)
  return move block
}

impl Allocator for CountingAllocator { allocate: CountingAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = CountingAllocator {
    inner: Allocator.systemAllocatorProvider(),
    metrics: zeroedMetrics()
  }
  let firstLayout = Layout.of<[i32; 8]>()
  let firstPending = Allocator.allocate(move firstLayout) |> Effect.provideMut(&mut allocator)
  let first = run firstPending
  let secondLayout = Layout.of<[i32; 8]>()
  let secondPending = Allocator.allocate(move secondLayout) |> Effect.provideMut(&mut allocator)
  let second = run secondPending

  // Released oldest first, so the release order is not the reverse of the acquire order.
  drop first
  recordRelease(&mut allocator.metrics)

  let thirdLayout = Layout.of<[i32; 8]>()
  let thirdPending = Allocator.allocate(move thirdLayout) |> Effect.provideMut(&mut allocator)
  let third = run thirdPending
  drop second
  recordRelease(&mut allocator.metrics)
  drop third
  recordRelease(&mut allocator.metrics)

  let observed = copyMetrics(&allocator.metrics)
  if observed.acquired == 3 {} else { return 1 }
  return usize.toI32(observed.released)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('reports the folded release count on Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'wasm-heap-reclaim/counted',
      ascii(counted),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const wasmReleases = (instance.exports.silk_main as () => number)()

    assert.strictEqual(wasmReleases, 3)
  }),
)

const interleavedShared = `import silk.allocator { Allocator }
import silk.allocator { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.shared as Shared
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut index = 0
  while index < 10000 {
    let first = run (Shared.make<i32>(1) |> Effect.provideMut<Allocator>(&mut allocator))
    let second = run (Shared.make<i32>(2) |> Effect.provideMut<Allocator>(&mut allocator))
    drop first
    let third = run (Shared.make<i32>(3) |> Effect.provideMut<Allocator>(&mut allocator))
    drop second
    drop third
    index = index + 1
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'returns non-LIFO local-shared control blocks through the Wasm reclaim path',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'wasm-heap-reclaim/local-shared-interleaved',
        ascii(interleavedShared),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
      assert.isAtMost(pagesOf(instance), 4)
    }),
  120_000,
)

const repeatedSharedAccess = `import silk.allocator { Allocator }
import silk.allocator { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.shared as Shared
struct Counter { value: i32 }
fn increment(value: &mut Counter) -> i32 {
  value.value = value.value + 1
  return value.value
}
fn read(value: &Counter) -> i32 { return value.value }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let shared = run (Shared.make<Counter>(Counter { value: 0 })
    |> Effect.provideMut<Allocator>(&mut allocator))
  let mut index = 0
  while index < 10000 {
    let alias = Shared.clone<Counter>(&shared)
    let updated = Shared.withMut<Counter, i32>(&alias, increment)
    drop alias
    index = index + 1
  }
  let result = Shared.with<Counter, i32>(&shared, read)
  drop shared
  if result == 10000 { return 42 }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'keeps repeated local-shared clone and access allocation-free on Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'wasm-heap-reclaim/local-shared-clone-access',
        ascii(repeatedSharedAccess),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
      assert.isAtMost(pagesOf(instance), 4)
    }),
  120_000,
)
