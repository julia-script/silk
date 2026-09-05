import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Lifetime from '../src/Lifetime.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Ownership from '../src/Ownership.js'
import * as Type from '../src/Type.js'
import { ordinaryStorageSource } from './support/ordinaryStorageSource.js'

it.effect('rejects a callback-dependent Effect environment at a local-shared boundary', () =>
  Effect.gen(function* () {
    const source = ascii(`struct Pair { first: i32 second: i32 }
fn deferred<'a>(value: &'a mut Pair) -> Effect<'a; i32> {
  return effect { return value.first }
}
fn fallback() -> Effect<'static; i32> { return effect { return 0 } }
unsafe fn construct<'scope>(core: &'scope Intrinsic.SharedCore<Pair>) -> () {
  let escaped = Intrinsic.sharedWithMut<Pair, Effect<'scope; i32>>(core, deferred, fallback)
  drop escaped
  return ()
}
pub fn main() -> i32 { return 0 }`)
    const snapshot = yield* Analysis.ofSource('local-shared-lifecycle/escape', source)
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0076'],
    )
  }),
)

it('classifies inexpressible local-shared escape containers at the ownership-fact tier', () => {
  const narrowed = Type.slice('Shared', 'i32', Lifetime.staticLifetime)
  const genericAggregate = Type.nominal('test', 'Box', [narrowed])
  const failureValue = Type.effect('i32', [Type.nominal('test', 'Problem', [narrowed])], {
    environment: Lifetime.staticLifetime,
    lifetimeBinders: [],
  })
  const storedCallable = Type.callable(
    [],
    narrowed,
    { environment: Lifetime.staticLifetime, lifetimeBinders: [] },
    'Take',
  )
  for (const resultType of [narrowed, genericAggregate, failureValue, storedCallable]) {
    assert.isTrue(
      Ownership.localSharedResultEscapes({
        resultType,
        capturesRestrictedParameter: false,
        referencesRestrictedParameter: true,
      }),
    )
  }
  assert.isFalse(
    Ownership.localSharedResultEscapes({
      resultType: 'i32',
      capturesRestrictedParameter: false,
      referencesRestrictedParameter: true,
    }),
  )
})

it.effect('rejects a direct callback-dependent reference at a local-shared boundary', () =>
  Effect.gen(function* () {
    const source = ascii(`struct Pair { first: i32 second: i32 }
fn direct<'a>(value: &'a mut Pair) -> &'a mut Pair { return move value }
fn directConflict<'a>() -> &'a mut Pair { return directConflict() }
unsafe fn directProbe<'scope>(core: &'scope Intrinsic.SharedCore<Pair>) -> &'scope mut Pair {
  return Intrinsic.sharedWithMut<Pair, &'scope mut Pair>(core, direct, directConflict)
}
pub fn main() -> i32 { return 0 }`)
    const snapshot = yield* Analysis.ofSource('local-shared-lifecycle/direct-escape', source)
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0076'],
    )
  }),
)

it.effect('rejects generic aggregate capture and suspension across local-shared access', () =>
  Effect.gen(function* () {
    const source = ascii(`struct Pair { first: i32 second: i32 }
struct Box<A> { value: A }
fn wrap<A>(value: A) -> Box<A> { return Box<A> { value: move value } }
fn deferred<'a>(value: &'a mut Pair) -> Box<Effect<'a; i32>> {
  let escaped = effect { return value.first }
  return wrap<Effect<'a; i32>>(move escaped)
}
fn deferredConflict() -> Box<Effect<'static; i32>> {
  return wrap<Effect<'static; i32>>(effect { return 0 })
}
effect fn read(value: &mut Pair) -> i32 { return value.second }
fn suspended(value: &mut Pair) -> i32 {
  let result = run read(move value)
  return result
}
fn numberConflict() -> i32 { return 0 }
unsafe fn aggregateProbe<'scope>(core: &'scope Intrinsic.SharedCore<Pair>) -> Box<Effect<'scope; i32>> {
  return Intrinsic.sharedWithMut<Pair, Box<Effect<'scope; i32>>>(
    core,
    deferred,
    deferredConflict,
  )
}
unsafe fn suspensionProbe(core: &Intrinsic.SharedCore<Pair>) -> i32 {
  return Intrinsic.sharedWithMut<Pair, i32>(core, suspended, numberConflict)
}
pub fn main() -> i32 { return 0 }`)
    const snapshot = yield* Analysis.ofSource(
      'local-shared-lifecycle/aggregate-suspension-escape',
      source,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot)
        .map((diagnostic) => diagnostic.code)
        .sort(),
      ['OWN0016', 'SEM0076'],
    )
    const diagnostics = Analysis.diagnostics(snapshot).filter(
      (candidate) => candidate.code === 'OWN0016',
    )
    assert.deepEqual(
      diagnostics.map((diagnostic) =>
        diagnostic.reason._tag === 'LocalSharedAccessEscape'
          ? diagnostic.reason.kind
          : diagnostic.reason._tag,
      ),
      ['Suspension'],
    )
    for (const diagnostic of diagnostics) {
      assert.strictEqual(diagnostic.relatedSpans?.length, 1)
      assert.strictEqual(
        diagnostic.relatedSpans?.at(0)?.span.sourceId,
        'local-shared-lifecycle/aggregate-suspension-escape',
      )
    }
  }),
)

it.effect('rejects consuming the borrowed local-shared receiver in its selected callback', () =>
  Effect.gen(function* () {
    const source = ascii(`fn consume(value: &mut i32, core: Intrinsic.SharedCore<i32>) -> i32 {
  drop core
  return 0
}
fn conflict() -> i32 { return 0 }
unsafe fn probe(core: Intrinsic.SharedCore<i32>) -> i32 {
  return Intrinsic.sharedWithMut<i32, i32>(&core, consume(move core), conflict)
}
pub fn main() -> i32 { return 0 }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-lifecycle/consume-borrowed-receiver',
      source,
      'wasm32-unknown-unknown',
    )
    assert.include(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      'OWN0011',
    )
  }),
)

it.effect('transports exact local-shared allocation provenance through an ordinary helper', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
fn forward(allocation: Allocation) -> Allocation { return move allocation }
effect fn construct() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let acquired = run Intrinsic.systemAllocationAcquire(move layout)
  let allocation = forward(move acquired)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/helper',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

it.effect('invalidates inherited allocation provenance after a mutable parameter write', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
fn replace(mut allocation: Allocation, replacement: Allocation) -> Allocation {
  allocation = move replacement
  return move allocation
}
effect fn construct() -> i32 ! OutOfMemoryError {
  let firstLayout = Intrinsic.sharedLayout<i32>()
  let first = run Intrinsic.systemAllocationAcquire(move firstLayout)
  let secondLayout = Intrinsic.sharedLayout<i32>()
  let second = run Intrinsic.systemAllocationAcquire(move secondLayout)
  let allocation = replace(move first, move second)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/mutable-parameter',
      source,
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0138'],
    )
    const diagnostic = Analysis.diagnostics(snapshot).at(0)
    assert.strictEqual(
      diagnostic?.reason._tag === 'LocalSharedLayoutMismatch'
        ? diagnostic.reason.actual
        : undefined,
      'mutable parameter allocation provenance',
    )
  }),
)

it.effect('invalidates inherited allocation provenance after replacing a mutable parameter', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
fn replace(mut allocation: Allocation, replacement: Allocation) -> Allocation {
  let old = Intrinsic.replace(allocation, move replacement)
  drop old
  return move allocation
}
effect fn construct() -> i32 ! OutOfMemoryError {
  let firstLayout = Intrinsic.sharedLayout<i32>()
  let first = run Intrinsic.systemAllocationAcquire(move firstLayout)
  let secondLayout = Intrinsic.sharedLayout<i64>()
  let second = run Intrinsic.systemAllocationAcquire(move secondLayout)
  let allocation = replace(move first, move second)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/mutable-parameter-replace',
      source,
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      ['SEM0138'],
    )
    const diagnostic = Analysis.diagnostics(snapshot).at(0)
    assert.strictEqual(
      diagnostic?.reason._tag === 'LocalSharedLayoutMismatch'
        ? diagnostic.reason.actual
        : undefined,
      'mutable parameter allocation provenance',
    )
  }),
)

it.effect('proves exact local-shared provenance through the selected allocator provider', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
effect fn construct() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let recipe = Allocator.allocate(Intrinsic.sharedLayout<i32>())
    |> Effect.provideMut<Allocator>(&mut allocator)
  let allocation = run recipe
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/provider',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(ordinaryStorageSource(value), (character) => character.charCodeAt(0))

/** The accepted shape every negative below deviates from in exactly one way. */
const guarded = (body: string): string => `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
${body}
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

/**
 * The substrate is only sound if the frontend keeps rejecting the programs that would violate
 * it, so each prohibited shape is pinned to the code that rejects it. A regression here would
 * otherwise surface as a trap — or as undefined behaviour in a released backend — rather than
 * as a compile error.
 */
it.effect(
  'rejects every prohibited allocation shape before lowering',
  () =>
    Effect.gen(function* () {
      const cases: ReadonlyArray<readonly [string, string, string]> = [
        [
          'local-shared-layout-provenance-mismatch',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i64>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'SEM0138',
        ],
        [
          'local-shared-ordinary-layout',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
import silk.layout { Layout }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Layout.of<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'SEM0138',
        ],
        [
          'local-shared-helper-provenance-mismatch',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
fn forward(allocation: Allocation) -> Allocation { return move allocation }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<u32>()
  let acquired = run Intrinsic.systemAllocationAcquire(move layout)
  let allocation = forward(move acquired)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'SEM0138',
        ],
        [
          'local-shared-conditional-helper-provenance-mismatch',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
fn choose(flag: bool, wrong: Allocation, right: Allocation) -> Allocation {
  if flag {
    drop right
    return move wrong
  }
  drop wrong
  return move right
}
effect fn store() -> i32 ! OutOfMemoryError {
  let wrongLayout = Intrinsic.sharedLayout<u32>()
  let wrong = run Intrinsic.systemAllocationAcquire(move wrongLayout)
  let rightLayout = Intrinsic.sharedLayout<i32>()
  let right = run Intrinsic.systemAllocationAcquire(move rightLayout)
  let allocation = choose(true, move wrong, move right)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'SEM0138',
        ],
        [
          'local-shared-provider-forges-provenance',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
import silk.layout { Layout }
service Forge {
  effect fn allocate(layout: Layout) -> Allocation ! OutOfMemoryError ? &mut Forge
}
struct BadForge {}
effect fn allocate(self: &mut BadForge, layout: Layout) -> Allocation ! OutOfMemoryError {
  drop layout
  return run Intrinsic.systemAllocationAcquire(Layout.of<i32>())
}
impl Forge for BadForge { allocate: BadForge.allocate }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut forge = BadForge {}
  let recipe = Forge.allocate(Intrinsic.sharedLayout<i32>())
    |> Effect.provideMut<Forge>(&mut forge)
  let allocation = run recipe
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'SEM0138',
        ],
        [
          'local-shared-same-spelling-layout',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
import silk.layout { Layout }
fn sharedLayout() -> Layout { return Layout.of<i32>() }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = sharedLayout()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'SEM0138',
        ],
        [
          'local-shared-outside-unsafe',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
  drop core
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'SEM0082',
        ],
        [
          'local-shared-reuses-consumed-allocation',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
effect fn store() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop allocation
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'OWN0001',
        ],
        [
          'local-shared-reuses-consumed-payload',
          `import silk.allocator { OutOfMemoryError }
import silk.effect { Effect }
effect fn store() -> i32 ! OutOfMemoryError {
  let blockLayout = Intrinsic.sharedLayout<Allocation>()
  let block = run Intrinsic.systemAllocationAcquire(move blockLayout)
  let payloadLayout = Intrinsic.sharedLayout<i32>()
  let payload = run Intrinsic.systemAllocationAcquire(move payloadLayout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<Allocation>(move block, move payload)
    drop payload
    drop core
  }
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'OWN0001',
        ],
        [
          'raw-storage-outside-unsafe',
          `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let mut buffer = Intrinsic.rawBufferFrom<i32>(move allocation, 2)
  drop buffer
  return 1
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
          'SEM0082',
        ],
        [
          'slot-escapes-its-buffer',
          guarded(`    let slot = RawBuffer.slot(&mut buffer, 0)
    drop buffer
    let value = Slot.take(move slot)
    return value`),
          'OWN0011',
        ],
        [
          'buffer-moves-under-a-live-slot',
          guarded(`    let slot = RawBuffer.slot(&mut buffer, 0)
    let moved = move buffer
    let value = Slot.take(move slot)
    drop moved
    return value`),
          'OWN0011',
        ],
        [
          'foreign-allocator-conformance',
          `import silk.allocator { Allocator }
struct TestAllocator { remaining: i32 }
impl Allocator for TestAllocator { allocate: Foreign.allocate }
pub fn main() -> i32 { return 0 }`,
          'SEM0083',
        ],
        [
          'drop-hook-on-a-copy-type',
          `struct CopyValue { value: i32 }
impl Copy for CopyValue {}
impl Drop for CopyValue { fn drop(self: &mut CopyValue) -> () { return () } }
pub fn main() -> i32 { return 0 }`,
          'SEM0083',
        ],
      ]

      for (const [name, source, code] of cases) {
        const realized =
          code === 'SEM0138'
            ? yield* Analysis.ofSourceRealized(
                `owned-allocation-negative/${name}`,
                ascii(source),
                'wasm32-unknown-unknown',
              )
            : undefined
        const snapshot =
          realized ?? (yield* Analysis.ofSource(`owned-allocation-negative/${name}`, ascii(source)))
        assert.include(
          Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
          code,
          `${name}\n${Hir.encode(Analysis.rootAnalysis(snapshot).hir)}`,
        )
        if (realized !== undefined)
          assert.throws(() => Analysis.loweredMir(realized), /MIR is unavailable/)
      }
    }),
  // Measured near the 60s floor while the full parallel gate saturates the host; the timeout
  // is headroom for contention, not a performance assertion.
  180_000,
)
