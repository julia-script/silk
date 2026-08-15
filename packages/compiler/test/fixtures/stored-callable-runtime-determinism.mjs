import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import { existsSync } from 'node:fs'
import { join } from 'node:path'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Instances from '../../dist/Instances.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as NativeToolchain from '../../dist/NativeToolchain.js'
import * as Target from '../../dist/Target.js'

const source = `struct Parser<F: fn(i32) -> i32> { parse: F }
struct Boxed<F: fn(i32) -> i32> { inner: Parser<F> }
fn box<F: fn(i32) -> i32>(inner: Parser<F>) -> Boxed<F> {
  return Boxed<F> { inner: move inner }
}
pub fn main() -> i32 {
  let parser = Parser { parse: i32.add(2) }
  let boxed = box(move parser)
  return boxed.inner.parse(40)
}`
const bytes = new TextEncoder().encode(source)
const moduleName = 'fixture/stored-callable-runtime-determinism'
const host = await Effect.runPromise(Target.host())
const native = await Effect.runPromise(Analysis.ofSourceRealized(moduleName, bytes, host.id))
const wasm = await Effect.runPromise(
  Analysis.ofSourceRealized(moduleName, bytes, Target.wasm32UnknownUnknown.id),
)
const nativeArtifact = await Effect.runPromise(Analysis.codegen(native, { mode: 'release' }))
const wasmArtifact = await Effect.runPromise(Analysis.codegenWasm(wasm, { mode: 'release' }))
const evaluated = Analysis.evaluate(native)
const wasmInstance = new WebAssembly.Instance(
  new WebAssembly.Module(wasmArtifact.bytes.slice()),
  {},
)
const wasmMain = wasmInstance.exports.silk_main

const clang =
  process.env.SILK_TEST_CLANG ??
  (existsSync('/opt/homebrew/opt/llvm/bin/clang')
    ? '/opt/homebrew/opt/llvm/bin/clang'
    : existsSync('/usr/local/opt/llvm/bin/clang')
      ? '/usr/local/opt/llvm/bin/clang'
      : 'clang')
const toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  shimCache: NativeToolchain.makeShimCache(),
})
const nativeResult = NativeToolchain.withBuildScope('stored-callable-fresh-process', (scope) => {
  const object = NativeToolchain.emitObject(toolchain, scope, nativeArtifact, host, 'release')
  const shim = NativeToolchain.compileShim(toolchain, scope, host, nativeArtifact.termination)
  if (object._tag !== 'ObjectArtifact' || shim._tag !== 'ObjectArtifact') return -1
  const executable = NativeToolchain.ClangLinker.link(
    toolchain,
    host,
    [object.artifact, shim.artifact],
    [],
    join(scope.root, 'program'),
  )
  if (executable._tag !== 'Executable') return -2
  return spawnSync(executable.path, [], { encoding: 'utf8' }).status ?? -3
})

const hash = (value) => createHash('sha256').update(value).digest('hex')
const layoutHash = (snapshot) => {
  const layout = Analysis.layoutOf(snapshot)
  return layout._tag === 'Available' ? hash(Layout.encode(layout.value)) : layout.error.message
}

process.stdout.write(
  JSON.stringify({
    diagnostics: Analysis.diagnostics(native).map((diagnostic) => diagnostic.code),
    evaluator: evaluated._tag === 'Completed' ? evaluated.result.value : evaluated._tag,
    wasm: typeof wasmMain === 'function' ? wasmMain() : 'missing',
    native: nativeResult,
    instanceKeys: Analysis.instancesOf(native).instances.map((instance) =>
      Instances.keyText(instance.key),
    ),
    nativeLayout: layoutHash(native),
    wasmLayout: layoutHash(wasm),
    nativeMir: hash(Mir.encode(Analysis.loweredMir(native))),
    wasmMir: hash(Mir.encode(Analysis.loweredMir(wasm))),
    nativeSymbols: nativeArtifact.symbols.map((entry) => entry.symbol),
    wasmSymbols: wasmArtifact.symbols.map((entry) => entry.symbol),
    nativeArtifact: hash(nativeArtifact.bitcode),
    wasmArtifact: hash(wasmArtifact.bytes),
    directWasm:
      !wasmArtifact.wat.includes('call_indirect') && !wasmArtifact.wat.includes('(table '),
  }),
)
