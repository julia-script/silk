import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'

const source = new TextEncoder().encode(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`)
const snapshot = await Effect.runPromise(
  Analysis.ofSource('fixture/llvm-wasm-determinism', source, 'wasm32-unknown-unknown'),
)
const artifact = await Effect.runPromise(Analysis.codegen(snapshot, { mode: 'release' }))
process.stdout.write(
  JSON.stringify({
    tag: artifact._tag,
    ir: createHash('sha256').update(artifact.ir).digest('hex'),
    bitcode: createHash('sha256').update(artifact.bitcode).digest('hex'),
    symbols: artifact.symbols.map((entry) => entry.symbol),
  }),
)
