import * as NodeRuntime from '@effect/platform-node/NodeRuntime'
import * as NodeServices from '@effect/platform-node/NodeServices'
import * as Config from 'effect/Config'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Path from 'effect/Path'
import * as FetchHttpClient from 'effect/unstable/http/FetchHttpClient'
import * as OtlpSerialization from 'effect/unstable/observability/OtlpSerialization'
import * as OtlpTracer from 'effect/unstable/observability/OtlpTracer'
import * as Driver from '../src/Driver.js'
import * as FileSourceResolver from '../src/FileSourceResolver.js'
import * as NodeHeapObservation from '../src/NodeHeapObservation.js'
import * as ChromeTrace from './ChromeTrace.js'

const program = Effect.gen(function* () {
  const fs = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const directory = yield* path.fromFileUrl(new URL('.', import.meta.url))
  // const sourceRoot = path.join(directory, 'hello-world')
  const sourceRoot = path.join(directory, '../../../compiler/src')

  const destination = path.join(directory, 'dist', 'hello-world')

  const homebrewClang = '/opt/homebrew/opt/llvm/bin/clang'
  const defaultClang = (yield* fs.exists(homebrewClang)) ? homebrewClang : 'clang'
  const clang = yield* Config.string('SILK_CLANG').pipe(Config.withDefault(defaultClang))
  const siblingArchiver = path.join(path.dirname(clang), 'llvm-ar')
  const defaultArchiver = (yield* fs.exists(siblingArchiver)) ? siblingArchiver : 'llvm-ar'
  const llvmAr = yield* Config.string('SILK_LLVM_AR').pipe(Config.withDefault(defaultArchiver))
  yield* fs.makeDirectory(path.dirname(destination), { recursive: true })

  // Set a breakpoint here, then step into Driver.compile or any compiler phase in src/.
  const outcome = yield* Driver.compile({
    compilation: { root: 'main' },
    packageName: 'scratchpad-hello-world',
    artifactKind: 'NativeExecutable',
    toolchain: { _tag: 'Toolchain', clang, llvmAr },
    optimization: 'debug',
    destination,
    cache: false,
    saveTemps: true,
  }).pipe(Effect.provide(FileSourceResolver.layer(FileSourceResolver.make(sourceRoot))))

  // Inspect outcome.report here for phase timings; rejected builds retain diagnostics/sources.
  if (outcome._tag !== 'Compiled') return yield* Effect.fail(outcome)
  yield* Console.log(`Compiled: ${outcome.path}`)
  yield* Console.log(`Run: "${outcome.path}"`)
})

const TracingLive = Layer.unwrap(
  Config.string('OTEL_EXPORTER_OTLP_TRACES_ENDPOINT').pipe(
    Config.withDefault('http://127.0.0.1:4318/v1/traces'),
    Effect.map((url) =>
      OtlpTracer.layer({
        url,
        resource: { serviceName: 'comp' },
        exportInterval: '1 second',
        shutdownTimeout: '5 seconds',
      }),
    ),
  ),
).pipe(Layer.provide([FetchHttpClient.layer, OtlpSerialization.layerJson]))

NodeRuntime.runMain(
  Effect.gen(function* () {
    const path = yield* Path.Path
    const directory = yield* path.fromFileUrl(new URL('.', import.meta.url))
    return yield* ChromeTrace.record(
      program.pipe(Effect.withSpan('Scratchpad.compile')),
      path.join(directory, 'dist', 'comp.chrome-trace.json'),
    )
  }).pipe(
    Effect.provide(Layer.mergeAll(NodeServices.layer, NodeHeapObservation.layer, TracingLive)),
  ),
)
