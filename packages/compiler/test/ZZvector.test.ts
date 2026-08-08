import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const program = `import silk.vector { Vector, make, append, get, length, capacity }

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<I32>()
  let pending0 = append<I32>(&mut values, 10) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<I32>(&mut values, 11) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<I32>(&mut values, 12) |> Allocator.provide(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<I32>(&mut values, 13) |> Allocator.provide(&mut allocator)
  let appended3 = run pending3
  let pending4 = append<I32>(&mut values, 14) |> Allocator.provide(&mut allocator)
  let appended4 = run pending4
  let pending5 = append<I32>(&mut values, 15) |> Allocator.provide(&mut allocator)
  let appended5 = run pending5
  if length<I32>(&values) == 6 {} else { return 0 }
  if capacity<I32>(&values) == 8 {} else { return 1 }
  let first = get<I32>(&mut values, 0)
  let last = get<I32>(&mut values, 5)
  return first + last + 17
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

it.effect('probe', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource('probe/vector', ascii(program), 'wasm32-unknown-unknown')
    assert.deepEqual(snapshot.diagnostics, [])
    const evaluated = Analysis.evaluate(snapshot)
    const reason =
      evaluated._tag === 'Blocked'
        ? JSON.stringify(evaluated.reason, (k, v) =>
            k === 'span' ? undefined : typeof v === 'bigint' ? String(v) : v,
          ).slice(0, 2400)
        : ''
    const wasm = yield* Effect.exit(Analysis.codegenWasm(snapshot, { mode: 'release' }))
    const wasmResult =
      wasm._tag === 'Success'
        ? (
            new WebAssembly.Instance(new WebAssembly.Module(wasm.value.bitcode.slice()), {})
              .exports.silk_main as () => number
          )()
        : String(wasm.cause).slice(0, 240)
    const native = yield* Analysis.ofSource('probe/vector', ascii(program), 'aarch64-apple-darwin')
    const llvm = yield* Effect.exit(Analysis.codegen(native, { mode: 'release' }))
    assert.deepEqual(
      {
        result: evaluated._tag === 'Completed' ? evaluated.result.value : `${evaluated._tag} ${reason}`,
        wasmResult,
        llvm:
          llvm._tag === 'Success'
            ? 'ok'
            : JSON.stringify(llvm.cause, (k, v) => (typeof v === 'bigint' ? String(v) : v)).slice(
                0,
                700,
              ),
      },
      { sentinel: true },
    )
  }),
)
