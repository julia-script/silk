import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'

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
    assert.strictEqual(evaluated.result.value, 42)

    // Exactly one logical block is acquired and released, and every typed storage step in
    // between is ordered: no take precedes its write, and no release precedes the last take.
    assert.deepEqual(
      Analysis.allocationTraceEventsOf(evaluated).map((event) => event._tag),
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

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/** The accepted shape every negative below deviates from in exactly one way. */
const guarded = (body: string): string => `effect fn store() -> i32 ! OutOfMemory {
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
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
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
        'raw-storage-outside-unsafe',
        `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let mut buffer = Intrinsic.rawBufferFrom<i32>(move allocation, 2)
  drop buffer
  return 1
}
effect fn recover(error: OutOfMemory) -> i32 { return 0 }
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
        `struct TestAllocator { remaining: i32 }
impl Allocator for TestAllocator { allocate: Foreign.allocate }
pub fn main() -> i32 { return 0 }`,
        'SEM0083',
      ],
      [
        'drop-hook-on-a-copy-type',
        `struct CopyValue { value: i32 }
impl Drop for CopyValue { fn drop(self: &mut CopyValue) -> () { return () } }
pub fn main() -> i32 { return 0 }`,
        'SEM0084',
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
        name,
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
      Mir.encode(Analysis.loweredMir(first)),
      Mir.encode(Analysis.loweredMir(second)),
    )
    const firstWasm = yield* Analysis.codegenWasm(first, { mode: 'release' })
    const secondWasm = yield* Analysis.codegenWasm(second, { mode: 'release' })
    assert.deepEqual(Array.from(firstWasm.bytes), Array.from(secondWasm.bytes))
  }),
)
