import * as AbiManifest from '../src/AbiManifest.js'
import * as ForeignContract from '../src/ForeignContract.js'
import * as Target from '../src/Target.js'
import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Config from 'effect/Config'
import * as ConfigProvider from 'effect/ConfigProvider'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as TestClock from 'effect/testing/TestClock'
import * as Analysis from '../src/Analysis.js'
import * as NativeLinkInput from '../src/NativeLinkInput.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as PhaseReport from '../src/PhaseReport.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as ToolchainIntegrity from '../src/ToolchainIntegrity.js'
import { invalidGenericCorpus } from './support/corpus.js'
import * as Driver from './support/TestDriver.js'

const defaultClang = (): string => {
  if (existsSync('/opt/homebrew/opt/llvm/bin/clang')) return '/opt/homebrew/opt/llvm/bin/clang'
  if (existsSync('/usr/local/opt/llvm/bin/clang')) return '/usr/local/opt/llvm/bin/clang'
  return 'clang'
}

const clang = Effect.runSync(
  Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(defaultClang())),
)
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  llvmAr: 'llvm-ar',
  runtimeObjectCache: NativeToolchain.makeRuntimeObjectCache(),
})

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-driver-test-'))
afterAll(() => {
  rmSync(destinationRoot, { recursive: true, force: true })
})

const compileSource = (
  name: string,
  text: string,
  overrides: Partial<Driver.CompileRequest> = {},
): Effect.Effect<Driver.Outcome, Driver.SourceResolutionFailed | NativeToolchain.ToolchainError> =>
  Driver.compile({
    compilation: {
      root: SourceFile.make('memory/driver', ascii(text)),
    },
    toolchain,
    optimization: 'release',
    destination: join(destinationRoot, name),
    // This file asserts exact phase reports, so it builds uncached unless a test opts back in.
    cache: false,
    ...overrides,
    artifactKind: overrides.artifactKind ?? 'NativeExecutable',
  }).pipe(Effect.provide(SourceResolver.empty))

const expectedPhases = [
  'toolchain-integrity',
  'closure',
  'declaration-collection',
  'declaration-index',
  'name-resolution',
  'module-surface',
  'elaboration',
  'ownership',
  'opaque-realization',
  'instance-discovery',
  'target-layout',
  'mir-lowering',
  'toolchain-target',
  'backend',
  'object',
  'runtime',
  'link',
]

it.effect('measures Effect phases with the fiber clock', () =>
  Effect.gen(function* () {
    const reports: Array<PhaseReport.PhaseReport> = []
    const value = yield* PhaseReport.measureEffectInto(
      reports,
      'controlled-clock',
      1,
      Effect.gen(function* () {
        yield* TestClock.adjust(1250)
        return 42
      }),
      () => 1,
    )
    assert.strictEqual(value, 42)
    assert.strictEqual(reports.at(0)?.elapsedMs, 1250)
  }),
)

it.effect('reports every phase in order with counts and totals', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> i32 { return 42 }'
    const outcome = yield* compileSource('report', source)
    const analysis = yield* Analysis.ofSourceRealized('memory/driver', ascii(source))

    assert.strictEqual(outcome._tag, 'Compiled')
    if (outcome._tag !== 'Compiled') return
    assert.deepEqual(
      outcome.report.map((entry) => entry.phase),
      expectedPhases,
    )
    for (const entry of outcome.report) {
      assert.isAtLeast(entry.elapsedMs, 0, entry.phase)
      assert.isAtLeast(entry.outputs, 0, entry.phase)
      assert.isAtLeast(entry.heapBytes, 0, entry.phase)
    }
    const closure = outcome.report.find((entry) => entry.phase === 'closure')
    assert.strictEqual(closure?.inputs, 1)
    assert.strictEqual(closure?.outputs, 1)
    const compilerPhases = expectedPhases.slice(1, 12)
    assert.deepEqual(
      Analysis.phases(analysis)
        .map((entry) => entry.phase)
        .filter((phase) => phase !== 'semantic-occurrences' && phase !== 'anonymous-expressions'),
      compilerPhases,
    )
  }),
)

it.effect('reports array layouts and keeps array failures in their owning phase', () =>
  Effect.gen(function* () {
    const compiled = yield* compileSource(
      'array-report',
      'pub fn main() -> i32 { let values = [10, 42] return values[1] }',
    )
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const layout = compiled.report.find((entry) => entry.phase === 'target-layout')
    assert.isAtLeast(layout?.outputs ?? 0, 2)

    const mismatch = yield* compileSource(
      'array-mismatch',
      'pub fn main() -> [i32; 2] { return [1] }',
    )
    assert.strictEqual(mismatch._tag, 'Rejected')
    assert.strictEqual(
      mismatch.report.some((entry) => entry.phase === 'target-layout'),
      false,
    )

    const unavailable = yield* compileSource(
      'array-unavailable-layout',
      `fn consume(values: [[[i32; 2147483647]; 2147483647]; 0]) -> i32 { return 42 }
pub fn main() -> i32 { return consume([]) }`,
    )
    assert.strictEqual(unavailable._tag, 'BackendFailed')
    if (unavailable._tag !== 'BackendFailed') return
    assert.strictEqual(unavailable.error.reason._tag, 'InvalidMir')
    assert.strictEqual(
      unavailable.report.some((entry) => entry.phase === 'object'),
      false,
    )
  }),
)

it.effect('gates source rejection and operational resolution failure before backend work', () =>
  Effect.gen(function* () {
    const rejected = yield* compileSource('rejected', 'pub fn main() -> Mystery { return 42 }')
    assert.strictEqual(rejected._tag, 'Rejected')
    assert.strictEqual(
      rejected.report.some((entry) => entry.phase === 'target-layout'),
      false,
    )

    const resolver = Layer.succeed(SourceResolver.SourceResolver, {
      resolveStandardLibrary: SourceResolver.resolveEmbeddedStandardLibrary,
      toolchainSources: SourceResolver.embeddedToolchainSources,
      resolve: (module: string) =>
        Effect.fail(
          new SourceResolver.SourceResolverError({
            operation: 'test.resolve',
            module,
            message: `cannot read ${module}`,
            reason: { _tag: 'WrappedFailure', cause: new Error(module) },
          }),
        ),
    })
    const failed = yield* Effect.result(
      Driver.compile({
        compilation: {
          root: SourceFile.make(
            'memory/driver',
            ascii('import unreadable\npub fn main() -> i32 { return 42 }'),
          ),
        },
        toolchain,
        optimization: 'release',
        artifactKind: 'NativeExecutable',
        destination: join(destinationRoot, 'resolution-failed'),
      }).pipe(Effect.provide(resolver)),
    )
    assert.strictEqual(failed._tag, 'Failure')
    if (failed._tag === 'Failure') {
      assert.strictEqual(failed.failure._tag, 'SourceResolutionFailed')
      if (failed.failure._tag !== 'SourceResolutionFailed') return
      assert.deepEqual(
        failed.failure.failures.map((failure) => failure.module),
        ['unreadable'],
      )
      assert.strictEqual(
        failed.failure.report.some((entry) => entry.phase === 'target-layout'),
        false,
      )
    }
  }),
)

it.effect('rejects a mismatched distribution before resolving user imports', () =>
  Effect.gen(function* () {
    const installed = ToolchainIntegrity.installed()
    const mismatched = ToolchainIntegrity.make(
      installed.components.map((component) =>
        component.kind === 'Catalog' ? { ...component, digest: 'f'.repeat(64) } : component,
      ),
    )
    let projectResolutions = 0
    const resolver = Layer.succeed(SourceResolver.SourceResolver, {
      toolchainSources: SourceResolver.embeddedToolchainSources,
      resolveStandardLibrary: SourceResolver.resolveEmbeddedStandardLibrary,
      resolve: () => {
        projectResolutions += 1
        return Effect.succeedNone
      },
    })
    const outcome = yield* Driver.compile({
      compilation: {
        root: SourceFile.make(
          'memory/driver',
          ascii('import missing/project\npub fn main() -> i32 { return 42 }'),
        ),
      },
      toolchain,
      optimization: 'release',
      artifactKind: 'NativeExecutable',
      destination: join(destinationRoot, 'mismatched-distribution'),
      distribution: mismatched,
    }).pipe(Effect.provide(resolver))

    assert.strictEqual(outcome._tag, 'ToolchainFailed')
    assert.strictEqual(projectResolutions, 0)
    assert.deepEqual(
      outcome.report.map((entry) => entry.phase),
      ['toolchain-integrity'],
    )
  }),
)

it.effect(
  'rejects missing promised runtime support after reachable planning and before emission',
  () =>
    Effect.gen(function* () {
      const distribution = ToolchainIntegrity.make(
        ToolchainIntegrity.installed().components.filter(
          (component) =>
            component.kind !== 'RuntimeSupport' || !component.id.endsWith('/Intrinsic.i32Add'),
        ),
      )
      const outcome = yield* compileSource(
        'missing-runtime',
        'pub fn main() -> i32 { return Intrinsic.i32Add(20, 22) }',
        { distribution },
      )

      assert.strictEqual(outcome._tag, 'ToolchainFailed')
      assert.strictEqual(outcome.report.at(-1)?.phase, 'toolchain-target')
      assert.isFalse(outcome.report.some((entry) => entry.phase === 'backend'))
    }),
)

it.effect('surfaces a missing entry as a closed outcome without invoking the toolchain', () =>
  Effect.gen(function* () {
    const outcome = yield* compileSource('no-entry', 'pub fn answer() -> i32 { return 42 }')

    assert.strictEqual(outcome._tag, 'NoEntry')
    if (outcome._tag !== 'NoEntry') return
    assert.strictEqual(outcome.reason, 'MissingEntry')
    assert.strictEqual(
      outcome.report.some((entry) => entry.phase === 'object'),
      false,
    )
  }),
)

it.effect('names the failing native stage with command provenance', () =>
  Effect.gen(function* () {
    const outcome = yield* Effect.result(
      compileSource('bad-toolchain', 'pub fn main() -> i32 { return 42 }', {
        toolchain: Object.freeze({
          _tag: 'Toolchain',
          clang: '/nonexistent/clang',
          llvmAr: 'llvm-ar',
        }),
      }),
    )

    assert.strictEqual(outcome._tag, 'Failure')
    if (outcome._tag !== 'Failure') return
    assert.strictEqual(outcome.failure._tag, 'ToolchainError')
    if (outcome.failure._tag !== 'ToolchainError') return
    assert.strictEqual(outcome.failure.stage, 'object')
    assert.strictEqual(outcome.failure.reason._tag, 'SpawnFailed')
    if (outcome.failure.reason._tag !== 'SpawnFailed') return
    assert.strictEqual(outcome.failure.reason.planned.command, '/nonexistent/clang')
    assert.instanceOf(outcome.failure.reason.cause, Error)
  }),
)

it.effect('rejects invalid generic specialization before layout and MIR', () =>
  Effect.gen(function* () {
    for (const program of invalidGenericCorpus) {
      const outcome = yield* compileSource(program.name, program.source)
      assert.strictEqual(outcome._tag, 'Rejected', program.name)
      if (outcome._tag !== 'Rejected') continue
      const codes = outcome.diagnostics.map((diagnostic) => diagnostic.code)
      for (const code of program.codes) assert.include(codes, code, program.name)
      const phases = outcome.report.map((entry) => entry.phase)
      assert.notInclude(phases, 'target-layout', program.name)
      assert.notInclude(phases, 'mir-lowering', program.name)
      assert.notInclude(phases, 'backend', program.name)
      if (!program.codes.includes('SEM0053')) {
        assert.notInclude(phases, 'instance-discovery', program.name)
      }
    }
  }),
)

it.effect('stops unsupported targets before MIR or native tools', () =>
  Effect.gen(function* () {
    for (const target of ['mips-unknown-none']) {
      const outcome = yield* compileSource(
        `target-${target}`,
        'pub fn main() -> i32 { return 42 }',
        {
          compilation: {
            root: SourceFile.make('memory/driver', ascii('pub fn main() -> i32 { return 42 }')),
            target,
          },
        },
      )

      assert.strictEqual(outcome._tag, 'TargetFailed')
      assert.strictEqual(
        outcome.report.some((entry) => entry.phase === 'mir-lowering'),
        false,
      )
      assert.strictEqual(
        outcome.report.some((entry) => entry.phase === 'object'),
        false,
      )
    }
  }),
)

it.effect('reuses backend emission while denying native final-cache reads and writes', () =>
  Effect.gen(function* () {
    const initial = yield* compileSource(
      'old-native-cache-entry',
      'pub fn main() -> i32 { return 42 }',
    )
    assert.strictEqual(initial._tag, 'Compiled')
    if (initial._tag !== 'Compiled') return
    const oldBytes = readFileSync(initial.path)
    const entries = new Map<string, Uint8Array>()
    const reads: Array<string> = []
    const writes: Array<string> = []
    const artifactCache: NativeToolchain.ArtifactCache = Object.freeze({
      _tag: 'ArtifactCache',
      get: Effect.fnUntraced(function* (key: string) {
        reads.push(key)
        if (key.startsWith('backend-')) return entries.get(key)
        // Supply a fully authenticated old entry for the requested incomplete key. Encoding
        // through the real cache writer avoids mistaking envelope rejection for admission.
        yield* NativeToolchain.writeArtifactCache(
          {
            _tag: 'ArtifactCache',
            get: (seedKey: string) => Effect.sync(() => entries.get(seedKey)),
            set: (seedKey: string, bytes: Uint8Array) =>
              Effect.sync(() => {
                entries.set(seedKey, bytes)
              }),
          },
          key,
          oldBytes,
        )
        return entries.get(key)
      }),
      set: (key: string, bytes: Uint8Array) =>
        Effect.sync(() => {
          writes.push(key)
          entries.set(key, bytes)
        }),
    })
    const cachingToolchain = Object.freeze({ ...toolchain, artifactCache })
    const source = 'pub fn main() -> i32 { return 42 }'
    for (const name of ['admission-first', 'admission-second']) {
      const outcome = yield* compileSource(name, source, {
        toolchain: cachingToolchain,
        cache: true,
      })
      assert.strictEqual(outcome._tag, 'Compiled')
      if (outcome._tag !== 'Compiled') return
      const phases = outcome.report.map((entry) => entry.phase)
      assert.include(phases, 'link')
      assert.notInclude(phases, 'artifact-cache')
      if (name === 'admission-second') assert.include(phases, 'backend-cache')
    }
    const failed = yield* Effect.result(
      compileSource('admission-missing-library', source, {
        toolchain: cachingToolchain,
        cache: true,
        nativeLinkInputs: [NativeLinkInput.library('silk_missing_admission_fixture', 'Dynamic')],
      }),
    )
    assert.strictEqual(failed._tag, 'Failure')
    if (failed._tag !== 'Failure') return
    assert.strictEqual(failed.failure._tag, 'ToolchainError')
    if (failed.failure._tag !== 'ToolchainError') return
    assert.strictEqual(failed.failure.stage, 'link')
    assert.strictEqual(failed.failure.reason._tag, 'LinkFailed')
    assert.isNotEmpty(reads)
    assert.isNotEmpty(writes)
    assert.isTrue(reads.every((key) => key.startsWith('backend-')))
    assert.isTrue(writes.every((key) => key.startsWith('backend-')))
  }),
)

it.effect('reports a missing request-supplied object as linker input even when cached', () =>
  Effect.gen(function* () {
    const missing = join(destinationRoot, 'missing-extra.o')
    const destination = join(destinationRoot, 'native-link-inputs')
    const result = yield* Effect.result(
      compileSource('native-link-inputs', 'pub fn main() -> i32 { return 42 }', {
        cache: true,
        nativeLinkInputs: [
          NativeLinkInput.object(missing),
          NativeLinkInput.library('c', 'Dynamic'),
        ],
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure._tag, 'ToolchainError')
    if (result.failure._tag !== 'ToolchainError') return
    assert.strictEqual(result.failure.stage, 'link')
    assert.strictEqual(result.failure.reason._tag, 'LinkFailed')
    if (result.failure.reason._tag !== 'LinkFailed') return
    assert.strictEqual(result.failure.reason.status, null)
    assert.strictEqual(result.failure.reason.output, `missing linker input: ${missing}`)
    const args = result.failure.reason.planned.arguments
    assert.deepEqual(args.slice(args.indexOf(missing)), [missing, '-lc', '-lm', '-o', destination])
  }),
)

it.effect('translates a synchronously throwing artifact-cache read at the Driver boundary', () =>
  Effect.gen(function* () {
    const cause = Object.freeze({ injected: 'artifact-cache-read' })
    const artifactCache: NativeToolchain.ArtifactCache = Object.freeze({
      _tag: 'ArtifactCache',
      get: () => {
        throw cause
      },
      set: () => Effect.void,
    })
    const result = yield* Effect.result(
      compileSource('throwing-artifact-cache-read', 'pub fn main() -> i32 { return 42 }', {
        toolchain: Object.freeze({ ...toolchain, artifactCache }),
        cache: true,
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure._tag, 'ToolchainError')
    if (result.failure._tag !== 'ToolchainError') return
    assert.strictEqual(result.failure.operation, 'NativeToolchain.ArtifactCache.get')
    assert.strictEqual(result.failure.stage, 'cache-read')
    assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
    if (result.failure.reason._tag !== 'StorageFailed') return
    assert.strictEqual(result.failure.reason.cause, cause)
  }),
)

it.effect('translates a synchronously throwing artifact-cache write at the Driver boundary', () =>
  Effect.gen(function* () {
    const cause = Object.freeze({ injected: 'artifact-cache-write' })
    const artifactCache: NativeToolchain.ArtifactCache = Object.freeze({
      _tag: 'ArtifactCache',
      get: () => Effect.as(Effect.void, undefined),
      set: () => {
        throw cause
      },
    })
    const result = yield* Effect.result(
      compileSource('throwing-artifact-cache-write', 'pub fn main() -> i32 { return 42 }', {
        toolchain: Object.freeze({ ...toolchain, artifactCache }),
        cache: true,
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure._tag, 'ToolchainError')
    if (result.failure._tag !== 'ToolchainError') return
    assert.strictEqual(result.failure.operation, 'NativeToolchain.ArtifactCache.set')
    assert.strictEqual(result.failure.stage, 'cache-write')
    assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
    if (result.failure.reason._tag !== 'StorageFailed') return
    assert.strictEqual(result.failure.reason.cause, cause)
  }),
)

it.effect('selects the durable disk cache from SILK_NATIVE_CACHE_DIR by default', () =>
  Effect.acquireUseRelease(
    Effect.sync(() => mkdtempSync(join(tmpdir(), 'silk-default-cache-'))),
    (cacheDirectory) =>
      Effect.gen(function* () {
        // No artifactCache is pinned on either toolchain: the durable reuse below can only come
        // from the environment-selected default, and each compile builds its own toolchain value
        // so nothing is shared between them but the directory.
        const source = 'pub fn main() -> i32 { return 40 + 2 }'
        const first = yield* compileSource('default-cache-first', source, {
          toolchain: Object.freeze({ _tag: 'Toolchain', clang, llvmAr: 'llvm-ar' }),
          cache: true,
        })
        const second = yield* compileSource('default-cache-second', source, {
          toolchain: Object.freeze({ _tag: 'Toolchain', clang, llvmAr: 'llvm-ar' }),
          cache: true,
        })
        assert.strictEqual(first._tag, 'Compiled')
        assert.strictEqual(second._tag, 'Compiled')
        if (first._tag !== 'Compiled' || second._tag !== 'Compiled') return
        assert.strictEqual(
          first.report.some((entry) => entry.phase === 'link'),
          true,
        )
        assert.strictEqual(
          second.report.some((entry) => entry.phase === 'backend-cache'),
          true,
        )
        assert.strictEqual(
          second.report.some((entry) => entry.phase === 'link'),
          true,
        )
        assert.deepEqual(readFileSync(second.path), readFileSync(first.path))
        const run = spawnSync(second.path, [], { encoding: 'utf8' })
        assert.strictEqual(run.status, 42)
      }).pipe(
        Effect.provideService(
          ConfigProvider.ConfigProvider,
          ConfigProvider.fromUnknown({ SILK_NATIVE_CACHE_DIR: cacheDirectory }),
        ),
      ),
    (cacheDirectory) => Effect.sync(() => rmSync(cacheDirectory, { recursive: true, force: true })),
  ),
)

it.effect('rejects a supplied foreign contract before backend-cache or native-tool access', () =>
  Effect.gen(function* () {
    const text =
      'unsafe extern "C" fn foreign_operation() -> i32\npub fn main() -> i32 { return unsafe foreign_operation() }'
    const root = SourceFile.make('memory/driver', ascii(text))
    const supplied = SourceFile.make(
      'interfaces/vendor.abi.json',
      AbiManifest.encode(
        AbiManifest.make(
          Target.aarch64AppleDarwin,
          [
            {
              symbol: 'foreign_operation',
              parameters: [],
              result: 'i32',
              contract: { ...ForeignContract.conservative, memory: 'none' },
            },
          ],
          [],
          [],
        ),
      ),
    )
    let cacheReads = 0
    const outcome = yield* compileSource('rejected-interface', text, {
      compilation: { root, target: 'aarch64-apple-darwin' },
      foreignInterfaces: [supplied],
      cache: true,
      toolchain: {
        ...toolchain,
        clang: 'must-not-invoke-clang',
        artifactCache: {
          _tag: 'ArtifactCache',
          get: () =>
            Effect.sync(() => {
              cacheReads += 1
              return undefined
            }),
          set: () => Effect.void,
        },
      },
    })
    assert.strictEqual(outcome._tag, 'Rejected')
    if (outcome._tag !== 'Rejected') return
    const mismatch = outcome.diagnostics.find((diagnostic) => diagnostic.code === 'SEM0192')
    assert.strictEqual(mismatch?.span.sourceId, supplied.id)
    assert.strictEqual(mismatch?.relatedSpans?.at(0)?.span.sourceId, root.id)
    assert.isTrue(outcome.sources.has(supplied.id))
    assert.strictEqual(cacheReads, 0)
    assert.isFalse(existsSync(join(destinationRoot, 'rejected-interface')))
  }),
)
