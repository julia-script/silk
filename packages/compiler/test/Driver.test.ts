import { spawnSync } from 'node:child_process'
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as Driver from '../src/Driver.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as WasmBackend from '../src/WasmBackend.js'
import { corpus, invalidGenericCorpus } from './support/corpus.js'

const clang =
  process.env.SILK_TEST_CLANG ??
  (existsSync('/opt/homebrew/opt/llvm/bin/clang')
    ? '/opt/homebrew/opt/llvm/bin/clang'
    : existsSync('/usr/local/opt/llvm/bin/clang')
      ? '/usr/local/opt/llvm/bin/clang'
      : 'clang')
const toolchain: NativeToolchain.Toolchain = Object.freeze({
  _tag: 'Toolchain',
  clang,
  shimCache: NativeToolchain.makeShimCache(),
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
): Effect.Effect<Driver.Outcome, Driver.SourceResolutionFailed> =>
  Driver.compile({
    compilation: {
      root: SourceFile.make('memory/driver', ascii(text)),
    },
    toolchain,
    profile: 'release',
    destination: join(destinationRoot, name),
    // This file asserts exact phase reports, so it builds uncached unless a test opts back in.
    cache: false,
    ...overrides,
  }).pipe(Effect.provide(SourceResolver.empty))

const expectedPhases = [
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
  'backend',
  'object',
  'shim',
  'link',
]

it.effect('compiles a three-module call chain to native execution matching the interpreter', () =>
  Effect.gen(function* () {
    const sources = new Map([
      [
        'app/Main',
        ascii('import library.Answer { answer }\npub fn main() -> i32 { return answer() }'),
      ],
      [
        'library/Answer',
        ascii('import silk.i32 as i32\nimport values.Number { two }\npub fn answer() -> i32 { return i32.add(40, two()) }'),
      ],
      ['values/Number', ascii('pub fn two() -> i32 { return 2 }')],
    ])
    const root = sources.get('app/Main')
    if (root === undefined) return assert.fail('expected app/Main fixture')
    const imports = new Map([...sources].filter(([name]) => name !== 'app/Main'))
    const layer = SourceResolver.memory(imports)
    const outcome = yield* Driver.compile({
      compilation: { root: SourceFile.make('app/Main', root) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'cross-module'),
      cache: false,
    }).pipe(Effect.provide(layer))
    const interpreted = Analysis.evaluate(
      yield* Analysis.makeRealized({ root: SourceFile.make('app/Main', root) }).pipe(
        Effect.provide(layer),
      ),
    )
    assert.strictEqual(outcome._tag, 'Compiled')
    assert.strictEqual(interpreted._tag, 'Completed')
    if (outcome._tag !== 'Compiled' || interpreted._tag !== 'Completed') return
    const run = spawnSync(outcome.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, Number(interpreted.result.value))
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
      assert.isAbove(entry.heapBytes, 0, entry.phase)
    }
    const closure = outcome.report.at(0)
    assert.strictEqual(closure?.inputs, 1)
    assert.strictEqual(closure?.outputs, 1)
    const compilerPhases = expectedPhases.slice(0, 11)
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

it.effect(
  'routes emission through an injected backend service',
  () =>
    Effect.gen(function* () {
      let emissions = 0
      const spy: Backend.Backend = {
        _tag: 'Backend',
        id: 'llvm',
        name: 'Spy LLVM',
        targets: Backend.LlvmBackend.targets,
        emit: (program, request) => {
          emissions += 1
          return Backend.LlvmBackend.emit(program, request)
        },
      }
      const outcome = yield* compileSource(
        'injected-backend',
        'pub fn main() -> i32 { return 42 }',
        {
          backend: spy,
        },
      )

      assert.strictEqual(emissions, 1)
      assert.strictEqual(outcome._tag, 'Compiled')
    }),
  15_000,
)

it.effect('gates source rejection and operational resolution failure before backend work', () =>
  Effect.gen(function* () {
    let emissions = 0
    const spy: Backend.Backend = {
      _tag: 'Backend',
      id: 'llvm',
      name: 'Gate Spy',
      targets: Backend.LlvmBackend.targets,
      emit: (program, request) => {
        emissions += 1
        return Backend.LlvmBackend.emit(program, request)
      },
    }
    const rejected = yield* compileSource('rejected', 'pub fn main() -> Mystery { return 42 }', {
      backend: spy,
    })
    assert.strictEqual(rejected._tag, 'Rejected')
    assert.strictEqual(emissions, 0)
    assert.strictEqual(
      rejected.report.some((entry) => entry.phase === 'target-layout'),
      false,
    )

    const resolver = Layer.succeed(SourceResolver.SourceResolver, {
      resolveStandardLibrary: SourceResolver.resolveEmbeddedStandardLibrary,
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
        profile: 'release',
        destination: join(destinationRoot, 'resolution-failed'),
        backend: spy,
      }).pipe(Effect.provide(resolver)),
    )
    assert.strictEqual(failed._tag, 'Failure')
    if (failed._tag === 'Failure') {
      assert.strictEqual(failed.failure._tag, 'SourceResolutionFailed')
      assert.deepEqual(
        failed.failure.failures.map((failure) => failure.module),
        ['unreadable'],
      )
      assert.strictEqual(
        failed.failure.report.some((entry) => entry.phase === 'target-layout'),
        false,
      )
    }
    assert.strictEqual(emissions, 0)
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
    const outcome = yield* compileSource('bad-toolchain', 'pub fn main() -> i32 { return 42 }', {
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/nonexistent/clang' }),
    })

    assert.strictEqual(outcome._tag, 'Failed')
    if (outcome._tag !== 'Failed') return
    assert.strictEqual(outcome.stage, 'object')
    if (outcome.failure._tag !== 'ToolchainFailure')
      return assert.fail('expected toolchain failure')
    assert.strictEqual(outcome.failure.planned.command, '/nonexistent/clang')
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

it.effect('keeps direct WebAssembly in agreement across recursive corpus fixtures', () =>
  Effect.gen(function* () {
    const recursiveNames = new Set([
      'direct-recursion',
      'mutual-recursion',
      'same-specialization-recursion',
      'recursive-aggregate-return',
      'recursive-mutable-slice',
    ])
    for (const program of corpus.filter((candidate) => recursiveNames.has(candidate.name))) {
      if (program.expected._tag !== 'Completes') continue
      const snapshot = yield* Analysis.ofSourceRealized(
        `wasm-recursion/${program.name}`,
        ascii(program.source),
        'wasm32-unknown-unknown',
      )
      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      assert.strictEqual(
        (instance.exports.silk_main as () => number)(),
        program.expected.result,
        program.name,
      )
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

it.effect('rejects an incompatible backend-target pair before MIR and finalization', () =>
  Effect.gen(function* () {
    const outcome = yield* compileSource(
      'incompatible-backend',
      'pub fn main() -> i32 { return 42 }',
      {
        backend: WasmBackend.WasmBackend,
      },
    )
    assert.strictEqual(outcome._tag, 'BackendFailed')
    assert.strictEqual(
      outcome.report.some((entry) => entry.phase === 'mir-lowering'),
      false,
    )
    assert.strictEqual(
      outcome.report.some((entry) => entry.phase === 'backend'),
      false,
    )
    assert.strictEqual(
      outcome.report.some((entry) => entry.phase === 'artifact-commit'),
      false,
    )
  }),
)

it.effect(
  'commits instantiable LLVM and direct WebAssembly modules with parity',
  () =>
    Effect.gen(function* () {
      const source = 'pub fn main() -> i32 { return 42 }'
      const backends = [Backend.LlvmBackend, WasmBackend.WasmBackend] as const
      for (const backend of backends) {
        const outcome = yield* compileSource(`${backend.id}.wasm`, source, {
          compilation: {
            root: SourceFile.make('memory/driver', ascii(source)),
            target: 'wasm32-unknown-unknown',
          },
          backend,
          toolchain: Object.freeze({
            _tag: 'Toolchain',
            clang: backend.id === 'llvm' ? clang : '/nonexistent/direct-wasm-must-not-run-clang',
          }),
        })
        assert.strictEqual(outcome._tag, 'Compiled', backend.id)
        if (outcome._tag !== 'Compiled') continue
        assert.strictEqual(outcome.backend, backend.id)
        assert.strictEqual(outcome.artifactKind, 'WebAssemblyModule')
        const instance = new WebAssembly.Instance(
          new WebAssembly.Module(Uint8Array.from(readFileSync(outcome.path))),
          {},
        )
        const main = instance.exports.silk_main
        assert.isFunction(main)
        if (typeof main === 'function') assert.strictEqual(main(), 42)
        assert.strictEqual(
          outcome.report.some((entry) => entry.phase === 'wasm-finalize'),
          backend.id === 'llvm',
        )
        assert.strictEqual(
          outcome.report.some((entry) => entry.phase === 'artifact-commit'),
          backend.id === 'wasm',
        )
      }
    }),
  30_000,
)

it.effect(
  'keeps evaluator, LLVM Wasm, and direct Wasm trap parity',
  () =>
    Effect.gen(function* () {
      const source = 'import silk.i32 as i32\npub fn main() -> i32 { return i32.divide(1, 0) }'
      const snapshot = yield* Analysis.ofSourceRealized(
        'memory/driver',
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.strictEqual(Analysis.evaluate(snapshot)._tag, 'Trap')
      for (const backend of [Backend.LlvmBackend, WasmBackend.WasmBackend] as const) {
        const outcome = yield* compileSource(`trap-${backend.id}.wasm`, source, {
          compilation: {
            root: SourceFile.make('memory/driver', ascii(source)),
            target: 'wasm32-unknown-unknown',
          },
          backend,
          toolchain: Object.freeze({ _tag: 'Toolchain', clang }),
        })
        assert.strictEqual(outcome._tag, 'Compiled', backend.id)
        if (outcome._tag !== 'Compiled') continue
        const instance = new WebAssembly.Instance(
          new WebAssembly.Module(Uint8Array.from(readFileSync(outcome.path))),
          {},
        )
        const main = instance.exports.silk_main
        assert.isFunction(main)
        if (typeof main === 'function') assert.throws(() => main(), WebAssembly.RuntimeError)
      }
    }),
  30_000,
)

it.effect('serves identical bitcode from the artifact cache without invoking the toolchain', () =>
  Effect.gen(function* () {
    const cacheDirectory = mkdtempSync(join(tmpdir(), 'silk-artifact-cache-'))
    const cachingToolchain: NativeToolchain.Toolchain = Object.freeze({
      _tag: 'Toolchain',
      clang,
      shimCache: NativeToolchain.makeShimCache(),
      artifactCache: NativeToolchain.makeDiskArtifactCache(cacheDirectory),
    })
    const source = 'pub fn main() -> i32 { return 42 }'
    const first = yield* compileSource('cache-first', source, {
      toolchain: cachingToolchain,
      cache: true,
    })
    const second = yield* compileSource('cache-second', source, {
      toolchain: cachingToolchain,
      cache: true,
    })
    rmSync(cacheDirectory, { recursive: true, force: true })
    assert.strictEqual(first._tag, 'Compiled')
    assert.strictEqual(second._tag, 'Compiled')
    if (first._tag !== 'Compiled' || second._tag !== 'Compiled') return
    assert.strictEqual(
      first.report.some((entry) => entry.phase === 'link'),
      true,
    )
    assert.strictEqual(
      second.report.some((entry) => entry.phase === 'artifact-cache'),
      true,
    )
    assert.strictEqual(
      second.report.some((entry) => entry.phase === 'object'),
      false,
    )
    const run = spawnSync(second.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42)
  }),
)

it.effect('selects the durable disk cache from SILK_NATIVE_CACHE_DIR by default', () =>
  Effect.gen(function* () {
    const cacheDirectory = mkdtempSync(join(tmpdir(), 'silk-default-cache-'))
    const previous = process.env.SILK_NATIVE_CACHE_DIR
    process.env.SILK_NATIVE_CACHE_DIR = cacheDirectory
    try {
      // No artifactCache is pinned on either toolchain: the durable reuse below can only come
      // from the environment-selected default, and each compile builds its own toolchain value
      // so nothing is shared between them but the directory.
      const source = 'pub fn main() -> i32 { return 40 + 2 }'
      const first = yield* compileSource('default-cache-first', source, {
        toolchain: Object.freeze({ _tag: 'Toolchain', clang }),
        cache: true,
      })
      const second = yield* compileSource('default-cache-second', source, {
        toolchain: Object.freeze({ _tag: 'Toolchain', clang }),
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
        second.report.some((entry) => entry.phase === 'artifact-cache'),
        true,
      )
      assert.strictEqual(
        second.report.some((entry) => entry.phase === 'object'),
        false,
      )
      assert.deepEqual(readFileSync(second.path), readFileSync(first.path))
      const run = spawnSync(second.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42)
    } finally {
      if (previous === undefined) delete process.env.SILK_NATIVE_CACHE_DIR
      else process.env.SILK_NATIVE_CACHE_DIR = previous
      rmSync(cacheDirectory, { recursive: true, force: true })
    }
  }),
)
