import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'

const source = `pub fn main() -> i32 {
  if f32.toBits(f32.sin(f32.fromBits(1148846080))) != 1062448736 { return 1 }
  if f32.toBits(f32.cos(f32.fromBits(1232348160))) != 1064292093 { return 2 }
  if f64.toBits(f64.sin(f64.fromBits(4786511204640096256))) != 13827052805184570195 { return 3 }
  if f64.toBits(f64.cos(f64.fromBits(4786511204640096256))) != 4605303934085493283 { return 4 }
  return 42
}`
const bytes = new TextEncoder().encode(source)
const snapshot = (name, target) => Effect.runPromise(Analysis.ofSource(name, bytes, target))
const native = await snapshot('fixture/transcendental-native', 'aarch64-apple-darwin')
const wasm = await snapshot('fixture/transcendental-wasm', 'wasm32-unknown-unknown')
const evaluation = Analysis.evaluate(native)
if (evaluation._tag !== 'Completed') throw new Error(`evaluation ${evaluation._tag}`)
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))

process.stdout.write(
  JSON.stringify({
    diagnostics: Analysis.diagnostics(native),
    result: evaluation.result.value,
    trace: evaluation.trace,
    nativeIr: nativeArtifact.ir,
    wasmIr: wasmArtifact.wat,
    native: createHash('sha256').update(nativeArtifact.bitcode).digest('hex'),
    wasm: createHash('sha256').update(wasmArtifact.bytes).digest('hex'),
  }),
)
