import { spawnSync } from 'node:child_process'
import { existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as Analysis from '../src/Analysis.js'
import * as CoroutineRuntime from '../src/CoroutineRuntime.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as Target from '../src/Target.js'
import type * as Termination from '../src/Termination.js'
import * as ToolchainPlan from '../src/ToolchainPlan.js'

const clang =
  process.env.SILK_TEST_CLANG ??
  (existsSync('/opt/homebrew/opt/llvm/bin/clang')
    ? '/opt/homebrew/opt/llvm/bin/clang'
    : existsSync('/usr/local/opt/llvm/bin/clang')
      ? '/usr/local/opt/llvm/bin/clang'
      : 'clang')
const toolchain: NativeToolchain.Toolchain = Object.freeze({ _tag: 'Toolchain', clang })

const testRoot = mkdtempSync(join(tmpdir(), 'silk-native-boundary-test-'))
afterAll(() => {
  rmSync(testRoot, { recursive: true, force: true })
})

const termination = (...identities: ReadonlyArray<string>): Termination.Contract =>
  Object.freeze({
    _tag: 'EntryTermination',
    success: identities.length === 0 ? 'ReturnedStatus' : 'Zero',
    failures: Object.freeze(
      identities.map((identity, ordinal) => Object.freeze({ tag: ordinal + 1, identity })),
    ),
    logicalFrames: Object.freeze([]),
  })

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const artifactFor = Effect.fnUntraced(function* (
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
) {
  const snapshot = yield* Analysis.ofSourceRealized(
    'memory/native',
    ascii(
      'pub fn identity(value: i32) -> i32 { return value }\npub fn main() -> i32 { return identity(identity(42)) }',
    ),
    target.id,
  )
  return yield* Analysis.codegen(snapshot, { mode: ToolchainPlan.codegenModeFor(profile) })
})

it('plans fixed profile arguments against the canonical target id', () => {
  const target = Target.aarch64AppleDarwin
  const debug = ToolchainPlan.objectCommand(clang, target, 'debug', 'in.bc', 'out.o')
  const release = ToolchainPlan.objectCommand(clang, target, 'release', 'in.bc', 'out.o')
  assert.deepEqual(debug.arguments, [
    '--target=aarch64-apple-darwin',
    '-c',
    '-x',
    'ir',
    'in.bc',
    '-O0',
    '-g',
    '-o',
    'out.o',
  ])
  assert.deepEqual(release.arguments, [
    '--target=aarch64-apple-darwin',
    '-c',
    '-x',
    'ir',
    'in.bc',
    '-O2',
    '-o',
    'out.o',
  ])
})

it('generates effect-reporting shims from byte arrays with closed status handling', () => {
  const source = ToolchainPlan.shimSource(termination('module.Error"\\name'))
  const expected = Array.from(new TextEncoder().encode('Error: module.Error"\\name\n')).join(', ')
  assert.include(source, `{ ${expected} }`)
  assert.notInclude(source, 'Error: module.Error"\\name')
  assert.include(source, 'default:\n      return 2;')
})

it('includes coroutine storage only when suspension requests it', () => {
  const direct = ToolchainPlan.shimSource(termination())
  const suspended = ToolchainPlan.shimSource(termination(), CoroutineRuntime.symbols)
  assert.notInclude(direct, CoroutineRuntime.pushSymbol)
  assert.include(suspended, CoroutineRuntime.pushSymbol)
  assert.include(suspended, CoroutineRuntime.popSymbol)
})

it.effect('yields a typed spawn failure with command, stage, and arbitrary cause', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const artifact = yield* artifactFor(target, 'release')
    let scopeRoot = ''
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope('spawn-failure', (scope) => {
        scopeRoot = scope.root
        return NativeToolchain.emitObject(
          { _tag: 'Toolchain', clang: '/nonexistent/clang' },
          scope,
          artifact,
          target,
          'release',
        )
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure._tag, 'ToolchainError')
    assert.strictEqual(result.failure.stage, 'object')
    assert.strictEqual(result.failure.reason._tag, 'SpawnFailed')
    if (result.failure.reason._tag !== 'SpawnFailed') return
    assert.strictEqual(result.failure.reason.planned.command, '/nonexistent/clang')
    assert.instanceOf(result.failure.reason.cause, Error)
    assert.strictEqual(existsSync(scopeRoot), false)
  }),
)

it.effect('reuses explicitly shared shim bytes across cleaned build scopes', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const cache = NativeToolchain.makeShimCache()
    const cachedToolchain = Object.freeze({ ...toolchain, shimCache: cache })
    yield* NativeToolchain.withBuildScope('shim-miss', (scope) =>
      NativeToolchain.compileShim(cachedToolchain, scope, target, termination()),
    )
    yield* NativeToolchain.withBuildScope('shim-hit', (scope) =>
      NativeToolchain.compileShim(cachedToolchain, scope, target, termination()),
    )
    assert.deepEqual(NativeToolchain.shimCacheStats(cache), {
      entries: 1,
      hits: 1,
      misses: 1,
    })
  }),
)

it.effect('removes a build scope after interruption', () =>
  Effect.gen(function* () {
    let scopeRoot = ''
    const fiber = yield* Effect.forkChild(
      NativeToolchain.withBuildScope('interrupted', (scope) => {
        scopeRoot = scope.root
        return Effect.never
      }),
    )
    yield* Effect.yieldNow
    yield* Fiber.interrupt(fiber)
    assert.isNotEmpty(scopeRoot)
    assert.strictEqual(existsSync(scopeRoot), false)
  }),
)

it.effect('retries a throwing scope cleanup without replacing the protected failure', () =>
  Effect.gen(function* () {
    const protectedFailure = Object.freeze({ _tag: 'ProtectedFailure' as const })
    let scopeRoot = ''
    let cleanupAttempts = 0
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope(
        'cleanup-retry',
        (scope) => {
          scopeRoot = scope.root
          return Effect.fail(protectedFailure)
        },
        {
          cleanup: {
            remove: (path, options) => {
              cleanupAttempts += 1
              if (cleanupAttempts === 1) throw new Error('injected cleanup failure')
              rmSync(path, options)
            },
          },
        },
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure, protectedFailure)
    assert.strictEqual(cleanupAttempts, 2)
    assert.strictEqual(existsSync(scopeRoot), false)
  }),
)

it.effect('failed rename removes its temporary sibling and preserves the destination', () =>
  Effect.gen(function* () {
    const destination = join(testRoot, 'occupied-destination')
    mkdirSync(destination)
    const result = yield* Effect.result(
      NativeToolchain.atomicCommit(destination, Uint8Array.of(1, 2, 3)),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
    assert.strictEqual(existsSync(destination), true)
    assert.deepEqual(
      readdirSync(testRoot).filter((name) => name.startsWith('occupied-destination.silk-tmp-')),
      [],
    )
  }),
)

it.effect('retries throwing atomic cleanup and leaves no staged sibling', () =>
  Effect.gen(function* () {
    const destination = join(testRoot, 'occupied-cleanup-retry')
    mkdirSync(destination)
    let cleanupAttempts = 0
    const result = yield* Effect.result(
      NativeToolchain.atomicCommit(destination, Uint8Array.of(4, 5, 6), {
        cleanup: {
          remove: (path, options) => {
            cleanupAttempts += 1
            if (cleanupAttempts === 1) throw new Error('injected cleanup failure')
            rmSync(path, options)
          },
        },
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    assert.strictEqual(cleanupAttempts, 2)
    assert.deepEqual(
      readdirSync(testRoot).filter((name) => name.startsWith('occupied-cleanup-retry.silk-tmp-')),
      [],
    )
  }),
)

it.effect('falls back to Node cleanup when injected atomic cleanup keeps failing', () =>
  Effect.gen(function* () {
    const destination = join(testRoot, 'occupied-cleanup-fallback')
    mkdirSync(destination)
    let cleanupAttempts = 0
    const result = yield* Effect.result(
      NativeToolchain.atomicCommit(destination, Uint8Array.of(7, 8, 9), {
        cleanup: {
          remove: () => {
            cleanupAttempts += 1
            throw new Error('injected persistent cleanup failure')
          },
        },
      }),
    )
    assert.strictEqual(result._tag, 'Failure')
    assert.strictEqual(cleanupAttempts, 2)
    assert.deepEqual(
      readdirSync(testRoot).filter((name) =>
        name.startsWith('occupied-cleanup-fallback.silk-tmp-'),
      ),
      [],
    )
  }),
)

it.effect('translates synchronously throwing shim-cache reads with cache-stage provenance', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const cause = Object.freeze({ injected: 'cache-read' })
    const cache: NativeToolchain.ShimCache = Object.freeze({
      _tag: 'ShimCache',
      get: () => {
        throw cause
      },
      set: () => Effect.succeed(undefined),
      stats: () => Object.freeze({ entries: 0, hits: 0, misses: 0 }),
    })
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope('cache-read-failure', (scope) =>
        NativeToolchain.compileShim(
          Object.freeze({ ...toolchain, shimCache: cache }),
          scope,
          target,
          termination(),
        ),
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure.stage, 'cache-read')
    assert.strictEqual(result.failure.operation, 'NativeToolchain.ShimCache.get')
    assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
    if (result.failure.reason._tag !== 'StorageFailed') return
    assert.strictEqual(result.failure.reason.cause, cause)
  }),
)

it.effect('translates synchronously throwing shim-cache writes with cache-stage provenance', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const cause = Object.freeze({ injected: 'cache-write' })
    const cache: NativeToolchain.ShimCache = Object.freeze({
      _tag: 'ShimCache',
      get: () => Effect.succeed(undefined),
      set: () => {
        throw cause
      },
      stats: () => Object.freeze({ entries: 0, hits: 0, misses: 0 }),
    })
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope('cache-write-failure', (scope) =>
        NativeToolchain.compileShim(
          Object.freeze({ ...toolchain, shimCache: cache }),
          scope,
          target,
          termination(),
        ),
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure.stage, 'cache-write')
    assert.strictEqual(result.failure.operation, 'NativeToolchain.ShimCache.set')
    assert.strictEqual(result.failure.reason._tag, 'StorageFailed')
    if (result.failure.reason._tag !== 'StorageFailed') return
    assert.strictEqual(result.failure.reason.cause, cause)
  }),
)

it.effect('returns committed bytes in memory and makes cached native artifacts executable', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const destination = join(testRoot, 'cached-program')
    const bytes = Uint8Array.from([0x7f, 0x45, 0x4c, 0x46])
    const committed = yield* NativeToolchain.commitCachedArtifact(
      bytes,
      'NativeExecutable',
      target,
      destination,
    )
    assert.deepEqual(committed.bytes, bytes)
    assert.deepEqual(readFileSync(destination), bytes)
  }),
)

it.effect('rejects a missing linker input in the typed link channel', () =>
  Effect.gen(function* () {
    const target = yield* NativeToolchain.hostTarget()
    const result = yield* Effect.result(
      NativeToolchain.withBuildScope('missing-link-input', (scope) =>
        NativeToolchain.ClangLinker.link(
          toolchain,
          scope,
          target,
          [
            Object.freeze({
              _tag: 'PathArtifact',
              scope: scope.name,
              path: join(scope.root, 'missing.o'),
              target,
            }),
          ],
          [],
          join(testRoot, 'never-written'),
        ),
      ),
    )
    assert.strictEqual(result._tag, 'Failure')
    if (result._tag !== 'Failure') return
    assert.strictEqual(result.failure.reason._tag, 'LinkFailed')
    if (result.failure.reason._tag !== 'LinkFailed') return
    assert.include(result.failure.reason.output, 'missing linker input')
  }),
)

it.effect(
  'links the shim and program while returning the executable bytes in memory',
  () =>
    Effect.gen(function* () {
      const target = yield* NativeToolchain.hostTarget()
      const artifact = yield* artifactFor(target, 'release')
      const destination = join(testRoot, 'linked-program')
      const linked = yield* NativeToolchain.withBuildScope('link-run', (scope) =>
        Effect.gen(function* () {
          const object = yield* NativeToolchain.emitObject(
            toolchain,
            scope,
            artifact,
            target,
            'release',
          )
          const shim = yield* NativeToolchain.compileShim(
            toolchain,
            scope,
            target,
            artifact.termination,
          )
          return yield* NativeToolchain.ClangLinker.link(
            toolchain,
            scope,
            target,
            [object.artifact, shim.artifact],
            [],
            destination,
          )
        }),
      )
      assert.isAbove(linked.bytes.length, 0)
      assert.deepEqual(linked.bytes, readFileSync(linked.path))
      const run = spawnSync(linked.path, [], { encoding: 'utf8' })
      assert.strictEqual(run.status, 42)
    }),
  15_000,
)
