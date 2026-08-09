import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as Ownership from '../../dist/Ownership.js'

const source = `struct Problem { code: I32 }
effect fn risky<T>(value: T, selector: I32) -> T ! Problem {
  if selector == 0 { fail move Problem { code: 41 } }
  return move value
}
effect fn relay(value: I32) -> I32 ! Problem {
  let pending = risky<I32>(value, value)
  return run pending
}
effect fn recover(problem: Problem) -> I32 { return problem.code |> I32.add(1) }
pub fn main() -> I32 {
  let recipe = relay(0) |> Effect.catch<Problem>(recover)
  return run recipe
}`
const bytes = new TextEncoder().encode(source)
const snapshot = (name, target) => Effect.runPromise(Analysis.ofSource(name, bytes, target))
const native = await snapshot('fixture/effect-native-determinism', 'aarch64-apple-darwin')
const wasm = await snapshot('fixture/effect-wasm-determinism', 'wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const nativeLayout = Analysis.layoutOf(native)
const wasmLayout = Analysis.layoutOf(wasm)
const nativeOwnership = Analysis.ownershipOf(native, 'fixture/effect-native-determinism')

process.stdout.write(
  JSON.stringify({
    diagnostics: Analysis.diagnostics(native),
    hir: Hir.encode(Analysis.hirOf(native, 'fixture/effect-native-determinism')),
    ownership:
      nativeOwnership === undefined ? 'ownership-unavailable' : Ownership.encode(nativeOwnership),
    instances: Analysis.instancesOf(native).instances.map((instance) => instance.symbol),
    nativeLayout:
      nativeLayout._tag === 'Available'
        ? Layout.encode(nativeLayout.value)
        : nativeLayout.error.message,
    wasmLayout:
      wasmLayout._tag === 'Available' ? Layout.encode(wasmLayout.value) : wasmLayout.error.message,
    nativeMir: Mir.encode(Analysis.loweredMir(native)),
    wasmMir: Mir.encode(Analysis.loweredMir(wasm)),
    trace: Analysis.traceOf(Analysis.evaluate(native)),
    nativeIr: nativeArtifact.ir,
    wasmIr: wasmArtifact.wat,
    native: createHash('sha256').update(nativeArtifact.bitcode).digest('hex'),
    wasm: createHash('sha256').update(wasmArtifact.bytes).digest('hex'),
  }),
)
