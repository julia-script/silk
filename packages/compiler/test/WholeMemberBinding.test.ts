import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const run = (label: string, text: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
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
    const native = yield* Analysis.ofSource(`probe/${label}`, ascii(text), 'aarch64-apple-darwin')
    const llvm = yield* Effect.exit(Analysis.codegen(native, { mode: 'release' }))
    return {
      diagnostics,
      result: evaluation._tag === 'Completed' ? evaluation.result.value : evaluation._tag,
      wasmResult,
      llvm: llvm._tag === 'Success' ? 'ok' : String(llvm.cause).slice(0, 240),
    }
  })

// Whole-member binding on a Copy member (Layout out of Layout.repeat's union).
const layoutExtract = `effect fn store() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let element = Layout.of<I32>()
  let plan = Layout.repeat(element, 3)
  let layout = match move plan {
    Layout value => value
    LayoutOverflow overflow => trapLayout()
  }
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<I32>(move allocation, 3)
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
effect fn recover(error: OutOfMemory) -> I32 { return 7 }
pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(store(), recover) }`

// Whole-member binding on an affine member (move it out of the union).
const affineExtract = `struct Empty {}
struct Full { storage: Allocation }
effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[I32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
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
effect fn fallback() -> Full ! OutOfMemory { fail OutOfMemory {} }
effect fn recover(error: OutOfMemory) -> I32 { return 7 }
pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

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
