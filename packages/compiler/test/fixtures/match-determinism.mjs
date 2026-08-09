import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as Ownership from '../../dist/Ownership.js'
import * as SyntaxFile from '../../dist/SyntaxFile.js'
import * as Type from '../../dist/Type.js'

const source = `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer + 1
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 41 }) }`
const bytes = new TextEncoder().encode(source)
const native = await Effect.runPromise(
  Analysis.ofSourceRealized('fixture/match-determinism', bytes, 'aarch64-apple-darwin'),
)
const wasm = await Effect.runPromise(
  Analysis.ofSourceRealized('fixture/match-determinism', bytes, 'wasm32-unknown-unknown'),
)
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const semantic = Analysis.matchesOf(native, 'fixture/match-determinism').map((match) => ({
  access: match.access,
  members: match.members.map(Type.encode),
  arms: match.arms.map((arm) => ({
    before: arm.before.map(Type.encode),
    after: arm.after.map(Type.encode),
    guarded: arm.guard !== undefined,
    reachable: arm.reachable,
  })),
}))
const ownership = Analysis.ownershipOf(native, 'fixture/match-determinism')
const layout = Analysis.layoutOf(native)
const syntax = Analysis.syntaxOf(native, 'fixture/match-determinism')

process.stdout.write(
  JSON.stringify({
    syntax: syntax === undefined ? '' : SyntaxFile.encode(syntax),
    semantic,
    hir: Hir.encode(Analysis.hirOf(native, 'fixture/match-determinism')),
    ownership: ownership === undefined ? '' : Ownership.encode(ownership),
    instances: Analysis.instancesOf(native).instances.map((instance) => instance.key.declaration),
    layout: layout._tag === 'Available' ? Layout.encode(layout.value) : layout.error.message,
    mir: Mir.encode(Analysis.loweredMir(native)),
    trace: Analysis.traceOf(Analysis.evaluate(native)),
    native: createHash('sha256').update(nativeArtifact.bitcode).digest('hex'),
    wasm: createHash('sha256').update(wasmArtifact.bytes).digest('hex'),
  }),
)
