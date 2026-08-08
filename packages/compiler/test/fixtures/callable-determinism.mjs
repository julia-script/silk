import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as Ownership from '../../dist/Ownership.js'

const source = `fn write(value: I32, values: &mut [I32]) -> I32 {
  values[0] = value
  return values[0]
}
pub fn main() -> I32 {
  let mut values = [0]
  let mut callback = write(&mut values)
  let first = callback(40)
  let second = callback(first + 2)
  drop callback
  return second
}`
const bytes = new TextEncoder().encode(source)
const snapshot = (name, target) => Effect.runPromise(Analysis.ofSource(name, bytes, target))
const native = await snapshot('fixture/callable-native-determinism', 'aarch64-apple-darwin')
const wasm = await snapshot('fixture/callable-wasm-determinism', 'wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const nativeLayout = Analysis.layoutOf(native)
const wasmLayout = Analysis.layoutOf(wasm)
const ownership = Analysis.ownershipOf(native, 'fixture/callable-native-determinism')

process.stdout.write(
  JSON.stringify({
    diagnostics: Analysis.diagnostics(native),
    hir: Hir.encode(Analysis.hirOf(native, 'fixture/callable-native-determinism')),
    ownership: ownership === undefined ? 'ownership-unavailable' : Ownership.encode(ownership),
    callables: Analysis.instancesOf(native).callables,
    nativeLayout:
      nativeLayout._tag === 'Available'
        ? Layout.encode(nativeLayout.value)
        : nativeLayout.error.message,
    wasmLayout:
      wasmLayout._tag === 'Available' ? Layout.encode(wasmLayout.value) : wasmLayout.error.message,
    nativeMir: Mir.encode(Analysis.loweredMir(native)),
    wasmMir: Mir.encode(Analysis.loweredMir(wasm)),
    nativeIr: nativeArtifact.ir,
    wasmIr: wasmArtifact.ir,
    native: createHash('sha256').update(nativeArtifact.bitcode).digest('hex'),
    wasm: createHash('sha256').update(wasmArtifact.bitcode).digest('hex'),
  }),
)
