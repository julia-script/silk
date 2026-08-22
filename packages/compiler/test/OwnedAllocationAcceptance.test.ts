import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import type * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Projections from './support/projections.js'

const bytes = new Uint8Array(
  readFileSync(new URL('./fixtures/owned-allocation-guard.silk', import.meta.url)),
)
const moduleName = 'owned-allocation-acceptance/main'

/**
 * The engines only agree by construction if they agree on the substrate, so the guard program
 * runs on the evaluator and Wasm rather than on the evaluator alone. The logical trace is
 * asserted on the evaluator because it is the only engine that publishes one; Wasm is held to
 * the observable result the trace predicts. Native agreement on this program is proven by the
 * differential corpus (`support/corpus.ts`).
 */
it.effect('keeps one owned allocation in parity across the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSourceRealized(moduleName, bytes, 'aarch64-apple-darwin')
    const wasm = yield* Analysis.ofSourceRealized(moduleName, bytes, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(native), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])

    const evaluated = Analysis.evaluate(native)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    // Exactly one logical block is acquired and released, and every typed storage step in
    // between is ordered: no take precedes its write, and no release precedes the last take.
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      [
        'AllocationAcquire',
        'RawBufferForm',
        'SlotProject',
        'SlotWrite',
        'SlotProject',
        'SlotWrite',
        'SlotProject',
        'SlotTake',
        'SlotProject',
        'SlotTake',
        'AllocationRelease',
      ],
    )

    const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const wasmInstance = new WebAssembly.Instance(
      new WebAssembly.Module(wasmArtifact.bytes.slice()),
      {},
    )
    assert.strictEqual((wasmInstance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('computes a local-shared layout without allocator access or cleanup authority', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.layout { Layout }
pub fn main() -> i32 {
  let layout = Intrinsic.sharedLayout<i32>()
  if layout.bytes == 72 { return 42 }
  return 0
}`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-layout/native',
      source,
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(Projections.allocationTraceEventsOf(evaluated), [])
  }),
)

it.effect('initializes one caller-funded local-shared core in evaluator and Wasm parity', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.core { OutOfMemoryError }
import silk.effect as Effect
effect fn construct() -> i32 ! OutOfMemoryError {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const native = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/native',
      source,
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/wasm',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    const mir = Analysis.loweredMir(native)
    assert.deepEqual(MirVerification.verify(mir), [])
    assert.strictEqual(
      mir.functions
        .flatMap(MirVerification.operations)
        .filter((operation) => operation._tag === 'SharedFromAllocation').length,
      1,
    )
    const malformed: Mir.Module = Object.freeze({
      ...mir,
      functions: Object.freeze(
        mir.functions.map((fn) =>
          Object.freeze({
            ...fn,
            regions: Object.freeze(
              fn.regions.map((region) =>
                region._tag !== 'OperationRegion'
                  ? region
                  : Object.freeze({
                      ...region,
                      operations: Object.freeze(
                        region.operations.map((operation) =>
                          operation._tag !== 'SharedFromAllocation'
                            ? operation
                            : Object.freeze({
                                ...operation,
                                block: Object.freeze({
                                  ...operation.block,
                                  size: operation.block.size + 1,
                                }),
                              }),
                        ),
                      ),
                    }),
              ),
            ),
          }),
        ),
      ),
    })
    assert.include(
      MirVerification.verify(malformed).map((violation) => violation.rule),
      'InvalidLocalSharedOperation',
    )
    const evaluated = Analysis.evaluate(native)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      evaluated._tag === 'Blocked'
        ? evaluated.reason._tag === 'InvalidMir'
          ? evaluated.reason.violations.map((violation) => violation.detail).join('\n')
          : evaluated.reason._tag
        : evaluated._tag,
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => ({
        tag: event._tag,
        strong: event.strong,
        access: event.access,
      })),
      [
        { tag: 'AllocationAcquire', strong: undefined, access: undefined },
        { tag: 'SharedInitialize', strong: 1n, access: 'Available' },
      ],
    )
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('leaves the payload cleanup obligation with source when allocation is exhausted', () =>
  Effect.gen(function* () {
    const source = ascii(`import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.effect as Effect
import silk.layout { Layout }
struct Token { storage: Allocation }
struct Exhausted {}
effect fn reject(self: &mut Exhausted, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}
impl Allocator for Exhausted { allocate: Exhausted.reject }
effect fn construct() -> i32 ! OutOfMemoryError {
  let payloadLayout = Layout.of<i32>()
  let storage = run Intrinsic.systemAllocationAcquire(move payloadLayout)
  let token = Token { storage: move storage }
  let mut allocator = Exhausted {}
  let recipe = Allocator.allocate(Intrinsic.sharedLayout<Token>())
    |> Effect.provideMut<Allocator>(&mut allocator)
  let allocation = run recipe
  unsafe {
    let core = Intrinsic.sharedFromAllocation<Token>(move allocation, move token)
    drop core
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`)
    const snapshot = yield* Analysis.ofSourceRealized(
      'local-shared-allocation/exhausted',
      source,
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag),
      ['AllocationAcquire', 'AllocationRelease'],
    )
  }),
)

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** The accepted shape every negative below deviates from in exactly one way. */
const guarded = (body: string): string => `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
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
it.effect('rejects every prohibited allocation shape before lowering', () =>
  Effect.gen(function* () {
    const cases: ReadonlyArray<readonly [string, string, string]> = [
      [
        'local-shared-layout-provenance-mismatch',
        `import silk.core { OutOfMemoryError }
import silk.effect as Effect
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
        'local-shared-outside-unsafe',
        `import silk.core { OutOfMemoryError }
import silk.effect as Effect
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
        `import silk.core { OutOfMemoryError }
import silk.effect as Effect
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
        `import silk.core { OutOfMemoryError }
import silk.effect as Effect
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
        `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
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
    drop moved
    return 1`),
        'OWN0011',
      ],
      [
        'foreign-allocator-conformance',
        `import silk.core { Allocator }
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
      const snapshot = yield* Analysis.ofSourceRealized(
        `owned-allocation-negative/${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.include(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        code,
        `${name}\n${Hir.encode(Analysis.rootAnalysis(snapshot).hir)}`,
      )
    }
  }),
)

/**
 * A fresh process must reach the same artifacts: the substrate introduces ticket ordinals and
 * logical addresses, and either would reintroduce run-to-run variation if it leaked into a key.
 */
it.effect('produces byte-identical artifacts across repeated analyses', () =>
  Effect.gen(function* () {
    const first = yield* Analysis.ofSourceRealized(moduleName, bytes, 'wasm32-unknown-unknown')
    const second = yield* Analysis.ofSourceRealized(moduleName, bytes, 'wasm32-unknown-unknown')

    assert.strictEqual(
      MirEncoding.encode(Analysis.loweredMir(first)),
      MirEncoding.encode(Analysis.loweredMir(second)),
    )
    const firstWasm = yield* Analysis.codegenWasm(first, { mode: 'release' })
    const secondWasm = yield* Analysis.codegenWasm(second, { mode: 'release' })
    assert.deepEqual(Array.from(firstWasm.bytes), Array.from(secondWasm.bytes))
  }),
)
