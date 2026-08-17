import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * A provider written in ordinary Silk. It owns one `AllocationMetrics` field, folds its own
 * acquires into it, and publishes a snapshot through a shared borrow. No compiler phase knows the
 * metrics type, and the provider uses no privilege unavailable to any other Silk program.
 */
const provider = `import silk.metrics {
  AllocationMetrics,
  copy as copyMetrics,
  live,
  make as zeroedMetrics,
  recordAcquire,
  recordRelease
}

struct CountingAllocator {
  inner: SystemAllocator
  metrics: AllocationMetrics
}

effect fn allocate(self: &mut CountingAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut self.inner)
  let block = run pending
  recordAcquire(&mut self.metrics)
  return move block
}

impl Allocator for CountingAllocator { allocate: CountingAllocator.allocate }

fn published(self: &CountingAllocator) -> AllocationMetrics {
  return copyMetrics(&self.metrics)
}
`

/**
 * Three acquires and one recorded release. The live count is the difference the standard library
 * computes, and reading it twice leaves the provider's own value where it is.
 */
const counted = `${provider}
effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = CountingAllocator {
    inner: SystemAllocator.make(),
    metrics: zeroedMetrics()
  }
  let firstLayout = Layout.of<[i32; 2]>()
  let firstPending = Allocator.allocate(move firstLayout) |> Effect.provideMut(&mut allocator)
  let first = run firstPending
  let secondLayout = Layout.of<[i32; 2]>()
  let secondPending = Allocator.allocate(move secondLayout) |> Effect.provideMut(&mut allocator)
  let second = run secondPending
  let thirdLayout = Layout.of<[i32; 2]>()
  let thirdPending = Allocator.allocate(move thirdLayout) |> Effect.provideMut(&mut allocator)
  let third = run thirdPending

  let atPeak = published(&allocator)
  if live(&atPeak) == 3 {} else { return 1 }

  drop third
  recordRelease(&mut allocator.metrics)

  let observed = published(&allocator)
  if observed.acquired == 3 {} else { return 2 }
  if observed.released == 1 {} else { return 3 }
  if live(&observed) == 2 {} else { return 4 }

  // Reading the counts is a read, not a move: the provider still owns its own value, and the
  // snapshot the caller already holds is independent of the next one.
  let again = published(&allocator)
  if live(&again) == 2 {} else { return 5 }
  if live(&observed) == 2 {} else { return 6 }

  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'counts three acquires and one release as an ordinary Silk value on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'allocation-metrics-acceptance/counted',
        ascii(counted),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify(evaluated, (_, value) =>
          typeof value === 'bigint' ? value.toString() : value,
        ),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)

      // The recorded counts describe the substrate honestly: three logical blocks were acquired,
      // and all three are released by the time the program returns.
      const events = Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag)
      assert.strictEqual(events.filter((event) => event === 'AllocationAcquire').length, 3)
      assert.strictEqual(events.filter((event) => event === 'AllocationRelease').length, 3)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  60_000,
)

/**
 * The peak is not the live count. Every release lowers the live count and leaves the peak where
 * the busiest moment put it, including once the program is back to zero live allocations and
 * once a later acquire comes in below the peak.
 */
const peakSurvivesDrops = `${provider}
effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = CountingAllocator {
    inner: SystemAllocator.make(),
    metrics: zeroedMetrics()
  }
  let firstLayout = Layout.of<[i32; 2]>()
  let firstPending = Allocator.allocate(move firstLayout) |> Effect.provideMut(&mut allocator)
  let first = run firstPending
  let secondLayout = Layout.of<[i32; 2]>()
  let secondPending = Allocator.allocate(move secondLayout) |> Effect.provideMut(&mut allocator)
  let second = run secondPending

  let atPeak = published(&allocator)
  if atPeak.peakLive == 2 {} else { return 1 }

  drop first
  recordRelease(&mut allocator.metrics)
  let afterFirst = published(&allocator)
  if live(&afterFirst) == 1 {} else { return 2 }
  if afterFirst.peakLive == 2 {} else { return 3 }

  drop second
  recordRelease(&mut allocator.metrics)
  let afterSecond = published(&allocator)
  if live(&afterSecond) == 0 {} else { return 4 }
  if afterSecond.peakLive == 2 {} else { return 5 }

  let thirdLayout = Layout.of<[i32; 2]>()
  let thirdPending = Allocator.allocate(move thirdLayout) |> Effect.provideMut(&mut allocator)
  let third = run thirdPending
  let afterThird = published(&allocator)
  if live(&afterThird) == 1 {} else { return 6 }
  if afterThird.peakLive == 2 {} else { return 7 }
  drop third

  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 8 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect(
  'keeps the peak live count across drops on the evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'allocation-metrics-acceptance/peak',
        ascii(peakSurvivesDrops),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  60_000,
)

/**
 * The counts live in their own standard-library module, so a program that never reads them never
 * loads it: nothing from `silk/metrics` reaches the module closure or MIR, and the program
 * allocates exactly what it asked for and nothing more.
 */
const unmetered = `effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let block = run pending
  drop block
  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('costs a program that never reads the metrics nothing at all', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'allocation-metrics-acceptance/unmetered',
      ascii(unmetered),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    // The metrics module is outside the program's module closure entirely.
    assert.notInclude(
      Analysis.modules(snapshot).map((module) => module.name),
      'silk/metrics',
    )

    // Nothing from the metrics module is lowered, so no metrics value is ever constructed.
    const mir = Analysis.loweredMir(snapshot)
    assert.deepEqual(
      mir.functions.filter((fn) => fn.id.module === 'silk/metrics').map((fn) => fn.id.name),
      [],
    )

    // One acquire, one release: the unmetered program allocates exactly what it asked for.
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)
    assert.deepEqual(
      Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      ['AllocationAcquire', 'AllocationRelease'],
    )

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

/**
 * Copy all the way down is what keeps a metrics value out of the cleanup machinery: it can never
 * implement Drop, and a program that holds one allocates nothing and schedules no release.
 */
it.effect('declares metrics as a cleanup-free Copy value', () =>
  Effect.gen(function* () {
    const droppable = `import silk.metrics { AllocationMetrics, make as zeroedMetrics }

struct Ledger { metrics: AllocationMetrics }

impl Drop for Ledger {
  fn drop(self: &mut Ledger) -> () { return () }
}

pub fn main() -> i32 {
  let ledger = Ledger { metrics: zeroedMetrics() }
  return 0
}`
    const rejected = yield* Analysis.ofSourceRealized(
      'allocation-metrics-acceptance/droppable',
      ascii(droppable),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(rejected).map((diagnostic) => diagnostic.code),
      ['SEM0084'],
    )

    const plain = `import silk.metrics {
  copy as copyMetrics,
  live,
  make as zeroedMetrics,
  recordAcquire,
  recordRelease
}

pub fn main() -> i32 {
  let mut counts = zeroedMetrics()
  recordAcquire(&mut counts)
  recordAcquire(&mut counts)
  recordRelease(&mut counts)
  let published = copyMetrics(&counts)
  if live(&published) == 1 {} else { return 1 }
  if published.acquired == 2 {} else { return 2 }
  if published.released == 1 {} else { return 3 }
  if published.peakLive == 2 {} else { return 4 }
  return 42
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'allocation-metrics-acceptance/plain',
      ascii(plain),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42)

    // Holding and reading metrics allocates nothing.
    assert.deepEqual(Analysis.allocationTraceEventsOf(evaluated), [])

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)
