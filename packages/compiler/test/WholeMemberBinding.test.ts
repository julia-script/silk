import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const run = (label: string, text: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `probe/${label}`,
      ascii(text),
      'wasm32-unknown-unknown',
    )
    const diagnostics = snapshot.diagnostics.map((d) => `${d.code}: ${d.message}`)
    const evaluation = Analysis.evaluate(snapshot)
    const wasm = yield* Effect.exit(Analysis.codegenWasm(snapshot, { mode: 'release' }))
    const wasmResult =
      wasm._tag === 'Success'
        ? (
            new WebAssembly.Instance(new WebAssembly.Module(wasm.value.bytes.slice()), {}).exports
              .silk_main as () => number
          )()
        : String(wasm.cause).slice(0, 260)
    const native = yield* Analysis.ofSourceRealized(
      `probe/${label}`,
      ascii(text),
      'aarch64-apple-darwin',
    )
    const llvm = yield* Effect.exit(Analysis.codegen(native, { mode: 'release' }))
    return {
      diagnostics,
      result: evaluation._tag === 'Completed' ? Number(evaluation.result.value) : evaluation._tag,
      wasmResult,
      llvm: llvm._tag === 'Success' ? 'ok' : String(llvm.cause).slice(0, 240),
    }
  })

// Whole-member binding on a source-defined Layout value from Layout.repeat's union.
const layoutExtract = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.layout { LayoutOverflow }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let element = Layout.of<i32>()
  let plan = Layout.repeat(move element, 3)
  let layout = match move plan {
    Layout value => move value
    LayoutOverflow overflow => trapLayout()
  }
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 3)
    let written = Slot.write(RawBuffer.slot(&mut buffer, 2), 42)
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 2))
    drop buffer
    return taken
  }
  return 0
}
fn trapLayout() -> Layout {
  let boom = 1 / 0
  return trapLayout()
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`

// Whole-member binding on an affine member (move it out of the union).
const affineExtract = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct Empty {}
struct Full { storage: Allocation }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let cell = Full { storage: move allocation }
  let widened = wrap(move cell)
  let restored = match move widened {
    Empty empty => run fallback()
    Full full => Full { storage: takeStorage(move full) }
  }
  drop restored
  return 42
}
fn wrap(cell: Full) -> Empty | Full { return move cell }
fn takeStorage(full: Full) -> Allocation {
  return match move full {
    Full { storage } => move storage
  }
}
effect fn fallback() -> Full ! OutOfMemoryError { fail OutOfMemoryError {} }
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

it.effect('binds whole members and lowers runtime layouts on all three engines', () =>
  Effect.gen(function* () {
    assert.deepEqual(
      {
        layoutExtract: yield* run('layout', layoutExtract),
        affineExtract: yield* run('affine', affineExtract),
      },
      {
        layoutExtract: { diagnostics: [], result: 42, wasmResult: 42, llvm: 'ok' },
        affineExtract: { diagnostics: [], result: 42, wasmResult: 42, llvm: 'ok' },
      },
    )
  }),
)
