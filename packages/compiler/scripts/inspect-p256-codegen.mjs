// Opt-in target/mode inspection after `pnpm --filter @silklang/compiler build`.
// Usage: node packages/compiler/scripts/inspect-p256-codegen.mjs /tmp/p256-codegen
import { NodeServices } from '@effect/platform-node'
import * as Config from 'effect/Config'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Path from 'effect/Path'
import * as Stream from 'effect/Stream'
import * as ChildProcess from 'effect/unstable/process/ChildProcess'
import * as ChildProcessSpawner from 'effect/unstable/process/ChildProcessSpawner'
import * as NodeHeapObservation from '../dist/NodeHeapObservation.js'
import * as Driver from '../dist/Driver.js'
import * as SourceFile from '../dist/SourceFile.js'
import * as SourceResolver from '../dist/SourceResolver.js'
import { p256WasmAcceptanceSource } from '../test/support/p256Acceptance.ts'

class InspectionFailure extends Data.TaggedError('InspectionFailure') {}
const commandOutput = Effect.fnUntraced(
  function* (/** @type {string} */ executable, /** @type {ReadonlyArray<string>} */ arguments_) {
    const spawner = yield* ChildProcessSpawner.ChildProcessSpawner
    return yield* Effect.scoped(
      Effect.gen(function* () {
        const handle = yield* spawner.spawn(
          ChildProcess.make(executable, arguments_, { stderr: 'inherit' }),
        )
        const [output, status] = yield* Effect.all(
          [Stream.runCollect(Stream.decodeText(handle.stdout)), handle.exitCode],
          { concurrency: 'unbounded' },
        )
        if (status !== 0) return yield* new InspectionFailure({ executable, arguments_, status })
        return output.join('')
      }),
    )
  },
)
const inspect = Effect.fnUntraced(function* () {
  const directory = process.argv[2]
  if (directory === undefined)
    return yield* new InspectionFailure({ message: 'Provide an inspection output directory' })
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const llvm = yield* Config.string('LLVM_BIN').pipe(
    Config.withDefault('/opt/homebrew/opt/llvm/bin'),
  )
  const toolchain = {
    _tag: 'Toolchain',
    clang: path.join(llvm, 'clang'),
    llvmAr: path.join(llvm, 'llvm-ar'),
  }
  yield* fileSystem.makeDirectory(directory, { recursive: true })
  for (const target of [
    'aarch64-apple-darwin',
    'x86_64-unknown-linux-gnu',
    'aarch64-unknown-linux-gnu',
    'wasm32-unknown-unknown',
  ]) {
    for (const optimization of ['debug', 'release', 'release-with-debug']) {
      const wasm = target.startsWith('wasm')
      const stem = path.join(directory, `${target}-${optimization}`)
      const outcome = yield* Driver.compile({
        packageName: 'p256-inspection',
        ...(wasm ? {} : { stage: 'llvm-bitcode' }),
        compilation: {
          root: SourceFile.make(
            'memory/p256-inspection',
            new TextEncoder().encode(p256WasmAcceptanceSource),
          ),
          target,
        },
        toolchain,
        optimization,
        destination: `${stem}.${wasm ? 'wasm' : 'bc'}`,
        // Executable roots retain main and its arithmetic. A rootless NativeObject is empty.
        artifactKind: wasm ? 'WebAssemblyModule' : 'NativeExecutable',
        cache: false,
      })
      if (outcome._tag !== 'Compiled') return yield* new InspectionFailure({ outcome })
      const artifact = wasm ? outcome.path : `${stem}.o`
      if (!wasm) {
        const arguments_ = [
          '--no-default-config',
          `--target=${target}`,
          '-c',
          '-x',
          'ir',
          outcome.path,
          optimization === 'debug' ? '-O0' : '-O2',
          '-o',
          artifact,
        ]
        if (target === 'aarch64-apple-darwin') arguments_.push('-mmacosx-version-min=11.0.0')
        yield* commandOutput(toolchain.clang, arguments_)
      }
      const dump = wasm
        ? yield* commandOutput('wasm2wat', [artifact])
        : yield* commandOutput(path.join(llvm, 'llvm-objdump'), [
            '-d',
            '--no-show-raw-insn',
            artifact,
          ])
      const text = dump.replace(/(silk_[A-Za-z0-9_]+?)__[0-9a-f_]+/g, '$1')
      if (!wasm && !text.includes('p256_multiply'))
        return yield* new InspectionFailure({
          artifact,
          message: 'Missing retained P256 multiplication',
        })
      // Release Wasm strips internal names. Inspect its exported main and arithmetic call graph.
      if (wasm && !text.includes('(export "main"'))
        return yield* new InspectionFailure({ artifact, message: 'Missing main export' })
      yield* fileSystem.writeFileString(`${stem}.${wasm ? 'wat' : 'asm'}`, text)
    }
  }
})
await Effect.runPromise(
  inspect().pipe(
    Effect.provide(
      Layer.mergeAll(SourceResolver.empty, NodeHeapObservation.layer, NodeServices.layer),
    ),
  ),
)
