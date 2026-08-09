import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'

const source = `fn recurse(value: i32) -> i32 {
  if value == 0 { return 42 }
  return recurse(value - 1)
}
pub fn main() -> i32 { return recurse(4) }`
const bytes = new TextEncoder().encode(source)
const snapshot = (name, target) => Effect.runPromise(Analysis.ofSourceRealized(name, bytes, target))
const native = await snapshot('fixture/runtime-recursion-native', 'aarch64-apple-darwin')
const wasm = await snapshot('fixture/runtime-recursion-wasm', 'wasm32-unknown-unknown')
const evaluation = Analysis.evaluate(native)
if (evaluation._tag !== 'Completed') throw new Error(`evaluation ${evaluation._tag}`)
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))

process.stdout.write(
  JSON.stringify({
    diagnostics: Analysis.diagnostics(native),
    result: evaluation.result.value,
    frames: evaluation.trace.filter((event) => event._tag === 'Entry').length,
    trace: evaluation.trace,
    nativeIr: nativeArtifact.ir,
    wasmIr: wasmArtifact.wat,
    native: createHash('sha256').update(nativeArtifact.bitcode).digest('hex'),
    wasm: createHash('sha256').update(wasmArtifact.bytes).digest('hex'),
  }),
)
