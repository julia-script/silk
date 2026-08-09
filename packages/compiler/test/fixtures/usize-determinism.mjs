import { createHash } from 'node:crypto'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Hir from '../../dist/Hir.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as SourceFile from '../../dist/SourceFile.js'
import * as SourceResolver from '../../dist/SourceResolver.js'

const constantsSource = `pub const exactValue: usize = 9007199254740993`
const nativeSource = `import constants { exactValue }
fn exact() -> usize { return exactValue }
pub fn main() -> i32 {
  if exact() == exactValue { return 42 }
  return 0
}`
const wasmSource = `const maximumValue: usize = 4294967295
fn maximum() -> usize { return maximumValue }
pub fn main() -> i32 {
  if maximum() > 2147483647 { return 42 }
  return 0
}`
const snapshot = (name, source, target) =>
  Effect.runPromise(
    Analysis.makeRealized({
      root: SourceFile.make(name, new TextEncoder().encode(source)),
      target,
    }).pipe(
      Effect.provide(
        SourceResolver.memory(new Map([['constants', new TextEncoder().encode(constantsSource)]])),
      ),
    ),
  )

const native = await snapshot(
  'fixture/usize-native-determinism',
  nativeSource,
  'aarch64-apple-darwin',
)
const wasm = await snapshot('fixture/usize-wasm-determinism', wasmSource, 'wasm32-unknown-unknown')
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const nativeLayout = Analysis.layoutOf(native)
const wasmLayout = Analysis.layoutOf(wasm)

process.stdout.write(
  JSON.stringify(
    {
      exact: Hir.encode(Analysis.hirOf(native, 'fixture/usize-native-determinism')),
      nativeLayout:
        nativeLayout._tag === 'Available'
          ? Layout.encode(nativeLayout.value)
          : nativeLayout.error.message,
      wasmLayout:
        wasmLayout._tag === 'Available'
          ? Layout.encode(wasmLayout.value)
          : wasmLayout.error.message,
      nativeMir: Mir.encode(Analysis.loweredMir(native)),
      wasmMir: Mir.encode(Analysis.loweredMir(wasm)),
      trace: Analysis.traceOf(Analysis.evaluate(native)),
      native: createHash('sha256').update(nativeArtifact.bitcode).digest('hex'),
      wasm: createHash('sha256').update(wasmArtifact.bytes).digest('hex'),
    },
    (_key, value) => (typeof value === 'bigint' ? value.toString() : value),
  ),
)
