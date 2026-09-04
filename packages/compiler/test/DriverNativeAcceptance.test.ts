import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs'
import { platform, tmpdir } from 'node:os'
import { dirname, join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import type * as ArtifactKind from '../src/ArtifactKind.js'
import * as NativeLinkInput from '../src/NativeLinkInput.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import { nativeCorpus } from './support/corpus.js'
import * as Driver from './support/TestDriver.js'

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}

const configured = (name: string, fallback = ''): string =>
  Effect.runSync(Config.string(name).pipe(Config.withDefault(fallback)))
const clang = configured('SILK_TEST_CLANG', defaultClang())
const defaultLlvmAr = join(dirname(clang), 'llvm-ar')
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  llvmAr: configured('SILK_TEST_LLVM_AR', existsSync(defaultLlvmAr) ? defaultLlvmAr : 'llvm-ar'),
  runtimeObjectCache: NativeToolchain.makeRuntimeObjectCache(),
})

// UTF-8, not charCodeAt: corpus programs may carry non-ASCII literals, and for ASCII sources the
// bytes are identical.
const encoder = new TextEncoder()
const ascii = (value: string): Uint8Array => encoder.encode(value)

const runCompiled = Effect.fnUntraced(function* (
  path: string,
  nativeEnvironment: Readonly<Record<string, string>> | undefined,
) {
  return yield* Effect.sync(() =>
    spawnSync(path, [], {
      encoding: 'utf8',
      ...(nativeEnvironment === undefined ? {} : { env: { ...process.env, ...nativeEnvironment } }),
    }),
  )
})

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-driver-native-acceptance-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const compileSource = (
  name: string,
  text: string,
  imports?: Readonly<Record<string, string>>,
  options: {
    readonly artifactKind?: ArtifactKind.ArtifactKind
    readonly packageName?: string
    readonly nativeLinkInputs?: ReadonlyArray<NativeLinkInput.NativeLinkInput>
    readonly cache?: boolean
  } = {},
): Effect.Effect<Driver.Outcome, Driver.SourceResolutionFailed | NativeToolchain.ToolchainError> =>
  Driver.compile({
    compilation: {
      root: SourceFile.make('memory/driver', ascii(text)),
    },
    toolchain,
    profile: 'release',
    artifactKind: options.artifactKind ?? 'NativeExecutable',
    packageName: options.packageName ?? 'compiler-test',
    destination: join(destinationRoot, name),
    ...(options.cache === undefined ? {} : { cache: options.cache }),
    ...(options.nativeLinkInputs === undefined
      ? {}
      : { nativeLinkInputs: options.nativeLinkInputs }),
  }).pipe(
    Effect.provide(
      imports === undefined
        ? SourceResolver.empty
        : SourceResolver.memory(
            new Map(
              Object.entries(imports).map(([module, source]) => [module, ascii(source)] as const),
            ),
          ),
    ),
  )

/** Compiles corpus C sources through the pinned Clang into durable objects under the test root. */
const compileCSources = Effect.fnUntraced(function* (
  name: string,
  sources: Readonly<Record<string, string>>,
) {
  const target = yield* NativeToolchain.hostTarget()
  return yield* NativeToolchain.withBuildScope(`${name}-c`, (scope) =>
    Effect.forEach(Object.entries(sources), ([unit, text]) =>
      Effect.gen(function* () {
        const object = yield* NativeToolchain.compileCObject(toolchain, scope, target, unit, text)
        const path = join(destinationRoot, `${name}-${unit}.o`)
        writeFileSync(path, readFileSync(object.artifact.path))
        return path
      }),
    ),
  )
})

/**
 * `SILK_NATIVE_SHARD=k/n` selects every n-th corpus case starting at k (1-based), letting CI run
 * the corpus as a job matrix instead of one serial sweep. Unset runs everything.
 */
const shard = /^([1-9]\d*)\/([1-9]\d*)$/.exec(configured('SILK_NATIVE_SHARD'))
const selectedNativeCase = configured('SILK_NATIVE_CORPUS_CASE')
const shardedCorpus =
  shard === null
    ? nativeCorpus
    : nativeCorpus.filter((_, index) => index % Number(shard[2]) === Number(shard[1]) - 1)

it('assigns every native corpus case to exactly one CI shard', () => {
  const assignments = Array.from({ length: 3 }, (_, shardIndex) =>
    nativeCorpus.filter((_, index) => index % 3 === shardIndex),
  ).flat()
  assert.strictEqual(new Set(assignments.map((program) => program.name)).size, nativeCorpus.length)
  assert.deepEqual(
    assignments.map((program) => program.name).sort(),
    nativeCorpus.map((program) => program.name).sort(),
  )
})

const librarySource = `unsafe extern "C" fn abs(value: i32) -> i32
fn helper(value: i32) -> i32 { return value + 1 }
export "C" fn increment(value: i32) -> i32 { return unsafe abs(helper(value)) }
export "C" static silk_abi_version: u32 = 1`

const consumerSource = `#include "answer.h"
int main(void) { return increment(40) + (int32_t)silk_abi_version; }
`

it.effect(
  'builds loadable shared/static libraries with only C exports visible',
  () =>
    Effect.gen(function* () {
      const sharedName = platform() === 'darwin' ? 'libsilk_answer.dylib' : 'libsilk_answer.so'
      const shared = yield* compileSource(sharedName, librarySource, undefined, {
        artifactKind: 'NativeSharedLibrary',
        packageName: 'answer',
        cache: true,
      })
      assert.strictEqual(shared._tag, 'Compiled')
      if (shared._tag !== 'Compiled') return
      assert.deepStrictEqual(
        shared.foreignExports.map((export_) => export_.symbol),
        ['increment'],
      )
      assert.deepStrictEqual(shared.foreignStatics, [
        { symbol: 'silk_abi_version', type: 'u32', direction: 'Export' },
      ])
      assert.deepStrictEqual(
        shared.foreignImports.map((import_) => import_.symbol),
        ['abs'],
      )
      assert.deepStrictEqual(shared.libraryInterface, {
        _tag: 'LibraryInterfaceArtifacts',
        cHeader: join(destinationRoot, 'answer.h'),
        abiManifest: join(destinationRoot, 'answer.abi.json'),
      })
      assert.strictEqual('termination' in shared, false)
      const interfacePhase = shared.report.find((entry) => entry.phase === 'library-interface')
      assert.deepStrictEqual(
        interfacePhase === undefined
          ? undefined
          : { inputs: interfacePhase.inputs, outputs: interfacePhase.outputs },
        { inputs: 3, outputs: 2 },
      )
      const firstHeader = readFileSync(join(destinationRoot, 'answer.h'))
      const firstManifest = readFileSync(join(destinationRoot, 'answer.abi.json'))

      const symbolDump =
        platform() === 'darwin'
          ? spawnSync('nm', ['-gU', shared.path], { encoding: 'utf8' })
          : spawnSync('nm', ['-D', '--defined-only', shared.path], { encoding: 'utf8' })
      assert.strictEqual(symbolDump.status, 0, symbolDump.stderr)
      const visible = symbolDump.stdout
        .split('\n')
        .map((line) => line.trim().split(/\s+/).at(-1) ?? '')
        .filter((symbol) => symbol.length > 0)
        .map((symbol) => (platform() === 'darwin' ? symbol.replace(/^_/, '') : symbol))
      assert.include(visible, 'increment')
      assert.include(visible, 'silk_abi_version')
      assert.deepStrictEqual(
        visible.filter((symbol) => symbol.startsWith('silk_') && symbol !== 'silk_abi_version'),
        [],
      )

      const consumerPath = join(destinationRoot, 'library-consumer.c')
      const sharedExecutable = join(destinationRoot, 'shared-consumer')
      writeFileSync(consumerPath, consumerSource)
      const sharedCompile = spawnSync(
        clang,
        [
          '-I',
          destinationRoot,
          consumerPath,
          shared.path,
          `-Wl,-rpath,${destinationRoot}`,
          '-o',
          sharedExecutable,
        ],
        { encoding: 'utf8' },
      )
      assert.strictEqual(sharedCompile.status, 0, sharedCompile.stderr)
      assert.strictEqual(spawnSync(sharedExecutable).status, 42)

      rmSync(join(destinationRoot, 'answer.h'))
      rmSync(join(destinationRoot, 'answer.abi.json'))
      const cachedShared = yield* compileSource(sharedName, librarySource, undefined, {
        artifactKind: 'NativeSharedLibrary',
        packageName: 'answer',
        cache: true,
      })
      assert.strictEqual(cachedShared._tag, 'Compiled')
      if (cachedShared._tag !== 'Compiled') return
      assert.include(
        cachedShared.report.map((entry) => entry.phase),
        'backend-cache',
      )
      assert.include(
        cachedShared.report.map((entry) => entry.phase),
        'artifact-cache',
      )
      assert.deepStrictEqual(readFileSync(join(destinationRoot, 'answer.h')), firstHeader)
      assert.deepStrictEqual(readFileSync(join(destinationRoot, 'answer.abi.json')), firstManifest)

      const firstArchive = yield* compileSource(
        'libsilk_answer-first.a',
        librarySource,
        undefined,
        {
          artifactKind: 'NativeStaticLibrary',
          packageName: 'answer',
          cache: false,
        },
      )
      assert.strictEqual(firstArchive._tag, 'Compiled')
      if (firstArchive._tag !== 'Compiled') return
      assert.deepStrictEqual(firstArchive.libraryInterface, shared.libraryInterface)
      assert.deepStrictEqual(readFileSync(join(destinationRoot, 'answer.h')), firstHeader)
      assert.deepStrictEqual(readFileSync(join(destinationRoot, 'answer.abi.json')), firstManifest)

      const staticExecutable = join(destinationRoot, 'static-consumer')
      const staticCompile = spawnSync(
        clang,
        ['-I', destinationRoot, consumerPath, firstArchive.path, '-lm', '-o', staticExecutable],
        { encoding: 'utf8' },
      )
      assert.strictEqual(staticCompile.status, 0, staticCompile.stderr)
      assert.strictEqual(spawnSync(staticExecutable).status, 42)
    }),
  20_000,
)

it.each(shardedCorpus)(
  'runs the native corpus case $name',
  async (program) => {
    await Effect.gen(function* () {
      if (selectedNativeCase.length > 0 && program.name !== selectedNativeCase) return
      const snapshot = yield* Analysis.ofSourceRealized('memory/driver', ascii(program.source))
      assert.strictEqual(
        snapshot.mir._tag,
        'Available',
        `${program.name}: ${Analysis.diagnostics(snapshot)
          .map((diagnostic) => diagnostic.code)
          .join(',')}`,
      )
      if (snapshot.mir._tag !== 'Available') return
      const compiledObjects =
        program.nativeCSources === undefined
          ? []
          : yield* compileCSources(`corpus-${program.name}`, program.nativeCSources)
      const outcome = yield* compileSource(
        `corpus-${program.name}`,
        program.nativeSource ?? program.source,
        program.nativeImports,
        {
          nativeLinkInputs: [
            ...compiledObjects.map(NativeLinkInput.object),
            ...(program.nativeDynamicLibraries ?? []).map((name) =>
              NativeLinkInput.library(name, 'Dynamic'),
            ),
          ],
        },
      )

      if (program.expected._tag === 'UnavailableEntry') {
        if (program.expected.reason === 'MissingEntry') {
          assert.strictEqual(outcome._tag, 'NoEntry', program.name)
        } else {
          assert.strictEqual(outcome._tag, 'Rejected', program.name)
          assert.deepEqual(
            outcome._tag === 'Rejected'
              ? outcome.diagnostics.map((diagnostic) => diagnostic.code)
              : [],
            ['SEM0204'],
            program.name,
          )
        }
        return
      }

      let compilationMessage = program.name
      if (outcome._tag === 'BackendFailed') {
        compilationMessage = `${program.name}: ${outcome.error.message}\n${Json.stringify(outcome.error.reason)}`
      } else if (outcome._tag === 'Rejected') {
        compilationMessage = `${program.name}: ${outcome.diagnostics.map((diagnostic) => diagnostic.code).join(',')}`
      }
      assert.strictEqual(outcome._tag, 'Compiled', compilationMessage)
      if (outcome._tag !== 'Compiled') return

      if (program.expected._tag === 'Completes') {
        const run = yield* runCompiled(outcome.path, program.nativeEnvironment)
        if (program.nativeStdout !== undefined)
          assert.strictEqual(run.stdout, program.nativeStdout, program.name)
        const nativeStatus = run.status === null ? null : BigInt(run.status)
        // POSIX exposes only the low unsigned byte of a process exit value.
        const expectedStatus = BigInt(program.expected.result) & 0xffn
        assert.strictEqual(
          nativeStatus,
          expectedStatus,
          `unexpected native result for ${program.name}: expected ${program.expected.result}, native ${run.status}`,
        )
        return
      }

      if (program.expected._tag === 'Trap') {
        const run = yield* runCompiled(outcome.path, program.nativeEnvironment)
        assert.strictEqual(
          run.signal !== null || (run.status !== null && run.status !== 0),
          true,
          `expected ${program.name} to trap, native exited ${run.status}`,
        )
      }
    }).pipe(Effect.runPromise)
  },
  1_500_000,
)
it.effect(
  'fails to link a foreign symbol nothing defines and keeps the linker output',
  () =>
    Effect.gen(function* () {
      const destination = join(destinationRoot, 'foreign-undefined')
      const result = yield* Effect.result(
        compileSource(
          'foreign-undefined',
          `unsafe extern "C" fn silk_test_missing_symbol(value: i32) -> i32
pub fn main() -> i32 { return unsafe silk_test_missing_symbol(1) }`,
        ),
      )
      assert.strictEqual(result._tag, 'Failure')
      if (result._tag !== 'Failure') return
      assert.strictEqual(result.failure._tag, 'ToolchainError')
      if (result.failure._tag !== 'ToolchainError') return
      assert.strictEqual(result.failure.reason._tag, 'LinkFailed')
      if (result.failure.reason._tag !== 'LinkFailed') return
      assert.match(result.failure.reason.output, /silk_test_missing_symbol/)
      assert.strictEqual(existsSync(destination), false)
    }),
  120_000,
)
it.effect(
  'links and runs the native system and monotonic clock ABI',
  () =>
    Effect.gen(function* () {
      const outcome = yield* compileSource(
        'native-clocks',
        `import silk.effect { Effect }
import silk.monotonic_clock { MonotonicClock }
import silk.os_monotonic_clock { OsMonotonicClock }
import silk.os_system_clock { OsSystemClock }
import silk.system_clock { SystemClock }
pub fn main() -> i32 {
  let mut system = OsSystemClock.make()
  let systemNow = run Effect.provideMut(SystemClock.now(), &mut system)
  let systemResolution = run Effect.provideMut(SystemClock.getResolution(), &mut system)
  let systemFraction = SystemClock.nanoseconds(&systemNow)
  if systemFraction < 0 { return 1 }
  if systemFraction >= 1000000000 { return 2 }
  if systemResolution == 0 { return 3 }

  let mut monotonic = OsMonotonicClock.make()
  let mark = run Effect.provideMut(MonotonicClock.now(), &mut monotonic)
  let monotonicResolution = run Effect.provideMut(
    MonotonicClock.getResolution(),
    &mut monotonic
  )
  let monotonicFraction = SystemClock.nanoseconds(&mark)
  if monotonicFraction < 0 { return 4 }
  if monotonicFraction >= 1000000000 { return 5 }
  if monotonicResolution == 0 { return 6 }
  run Effect.provideMut(MonotonicClock.waitUntil(move mark), &mut monotonic)
  run Effect.provideMut(MonotonicClock.waitFor(1), &mut monotonic)
  return 42
}`,
      )
      assert.strictEqual(
        outcome._tag,
        'Compiled',
        outcome._tag === 'BackendFailed'
          ? `${outcome.error.message}\n${Json.stringify(outcome.error.reason)}`
          : outcome._tag,
      )
      if (outcome._tag !== 'Compiled') return
      const run = yield* runCompiled(outcome.path, undefined)
      assert.strictEqual(run.signal, null)
      assert.strictEqual(run.status, 42)
    }),
  120_000,
)
