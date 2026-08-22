import { createHash } from 'node:crypto'
import { NodeServices } from '@effect/platform-node'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Stream from 'effect/Stream'
import { ChildProcess } from 'effect/unstable/process'
import * as Analysis from '../../dist/Analysis.js'
import * as Driver from '../../dist/Driver.js'
import * as Instances from '../../dist/Instances.js'
import * as Layout from '../../dist/Layout.js'
import * as Mir from '../../dist/Mir.js'
import * as NativeToolchain from '../../dist/NativeToolchain.js'
import * as SourceFile from '../../dist/SourceFile.js'
import * as SourceResolver from '../../dist/SourceResolver.js'
import * as Target from '../../dist/Target.js'

const source = `import silk.i32 as i32

struct Parser<F: fn(i32) -> i32> { parse: F }
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

const collectText = Stream.runFold(
  () => '',
  (text, chunk) => text + chunk,
)

const runProcess = Effect.fnUntraced(function* (command, args) {
  const process = yield* ChildProcess.make(command, args, { stdin: 'ignore' })
  const [exitCode, stdout, stderr] = yield* Effect.all(
    [
      process.exitCode,
      process.stdout.pipe(Stream.decodeText(), collectText),
      process.stderr.pipe(Stream.decodeText(), collectText),
    ],
    { concurrency: 'unbounded' },
  )
  return Object.freeze({ exitCode, stdout, stderr })
})

const clang = Effect.fnUntraced(function* () {
  const configured = yield* Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(''))
  if (configured.length > 0) return configured
  const fileSystem = yield* FileSystem.FileSystem
  for (const candidate of ['/opt/homebrew/opt/llvm/bin/clang', '/usr/local/opt/llvm/bin/clang'])
    if (yield* fileSystem.exists(candidate)) return candidate
  return 'clang'
})

const hash = Effect.fnUntraced(function* (value) {
  return yield* Effect.try(() => createHash('sha256').update(value).digest('hex'))
})

const layoutHash = Effect.fnUntraced(function* (snapshot) {
  const layout = Analysis.layoutOf(snapshot)
  return layout._tag === 'Available'
    ? yield* hash(Layout.encode(layout.value))
    : layout.error.message
})

const runWasm = Effect.fnUntraced(function* (artifact) {
  return yield* Effect.try(() => {
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') return 'missing'
    const result = main()
    return typeof result === 'number' ? result : 'invalid'
  })
})

const program = Effect.gen(function* () {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const destinationRoot = yield* fileSystem.makeTempDirectoryScoped()
  const host = yield* NativeToolchain.hostTarget()
  const native = yield* Analysis.ofSourceRealized(moduleName, bytes, host.id)
  const wasm = yield* Analysis.ofSourceRealized(moduleName, bytes, Target.wasm32UnknownUnknown.id)
  const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
  const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
  const evaluated = Analysis.evaluate(native)
  const wasmResult = yield* runWasm(wasmArtifact)
  const compiler = yield* clang()
  const compiled = yield* Driver.compile({
    compilation: { root: SourceFile.make(moduleName, bytes) },
    toolchain: Object.freeze({ _tag: 'Toolchain', clang: compiler }),
    profile: 'release',
    destination: path.join(destinationRoot, 'program'),
  }).pipe(Effect.provide(SourceResolver.empty))
  const nativeRun =
    compiled._tag === 'Compiled'
      ? yield* runProcess(compiled.path, [])
      : { exitCode: -1, stdout: '', stderr: compiled._tag }

  const report = {
    diagnostics: Analysis.diagnostics(native).map((diagnostic) => diagnostic.code),
    evaluator: evaluated._tag === 'Completed' ? Number(evaluated.result.value) : evaluated._tag,
    wasm: wasmResult,
    native: nativeRun.exitCode,
    instanceKeys: Analysis.instancesOf(native).instances.map((instance) =>
      Instances.keyText(instance.key),
    ),
    nativeLayout: yield* layoutHash(native),
    wasmLayout: yield* layoutHash(wasm),
    nativeMir: yield* hash(Mir.encode(Analysis.loweredMir(native))),
    wasmMir: yield* hash(Mir.encode(Analysis.loweredMir(wasm))),
    nativeSymbols: nativeArtifact.symbols.map((entry) => entry.symbol),
    wasmSymbols: wasmArtifact.symbols.map((entry) => entry.symbol),
    nativeArtifact: yield* hash(nativeArtifact.bitcode),
    wasmArtifact: yield* hash(wasmArtifact.bytes),
    directWasm:
      !wasmArtifact.wat.includes('call_indirect') && !wasmArtifact.wat.includes('(table '),
  }
  yield* Effect.try(() =>
    process.stdout.write(
      JSON.stringify(report, (_, value) => (typeof value === 'bigint' ? value.toString() : value)),
    ),
  )
}).pipe(Effect.scoped, Effect.provide(NodeServices.layer))

await Effect.runPromise(program)
