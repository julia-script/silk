import { spawnSync } from 'node:child_process'
import { existsSync } from 'node:fs'
import { join } from 'node:path'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as Target from '../src/Target.js'
import * as ToolchainPlan from '../src/ToolchainPlan.js'

const clang =
  process.env.SILK_TEST_CLANG ??
  (existsSync('/opt/homebrew/opt/llvm/bin/clang')
    ? '/opt/homebrew/opt/llvm/bin/clang'
    : existsSync('/usr/local/opt/llvm/bin/clang')
      ? '/usr/local/opt/llvm/bin/clang'
      : 'clang')
const toolchain: NativeToolchain.Toolchain = Object.freeze({ _tag: 'Toolchain', clang })

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nestedSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`

const artifactFor = Effect.fnUntraced(function* (
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
) {
  const snapshot = yield* Analysis.ofSourceRealized('memory/native', ascii(nestedSource), target.id)
  return yield* Analysis.codegen(snapshot, { mode: ToolchainPlan.codegenModeFor(profile) })
})

it('plans fixed profile arguments and nothing else varies', () => {
  const target = Target.aarch64AppleDarwin
  const debug = ToolchainPlan.objectCommand(clang, target, 'debug', 'in.bc', 'out.o')
  const release = ToolchainPlan.objectCommand(clang, target, 'release', 'in.bc', 'out.o')
  const releaseDebug = ToolchainPlan.objectCommand(
    clang,
    target,
    'release-with-debug',
    'in.bc',
    'out.o',
  )

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
  assert.deepEqual(releaseDebug.arguments, [
    '--target=aarch64-apple-darwin',
    '-c',
    '-x',
    'ir',
    'in.bc',
    '-O2',
    '-g',
    '-o',
    'out.o',
  ])
})

it('generates effect-reporting shims from byte arrays with closed status handling', () => {
  const source = ToolchainPlan.shimSource({
    _tag: 'EffectReports',
    reports: ['module.Error"\\name'],
  })
  const expected = Array.from(new TextEncoder().encode('Error: module.Error"\\name\n')).join(', ')
  assert.include(source, `{ ${expected} }`)
  assert.notInclude(source, 'Error: module.Error"\\name')
  assert.include(source, 'if (written <= 0) return 0;')
  assert.include(source, 'default:\n      return 2;')
})

it('plans every native profile for each canonical native target', () => {
  for (const target of Target.native) {
    const object = ToolchainPlan.objectCommand(clang, target, 'release', 'in.bc', 'out.o')
    const link = ToolchainPlan.linkCommand(clang, target, ['in.o'], [], 'program')
    assert.strictEqual(object.target, target)
    assert.strictEqual(link.target, target)
    assert.strictEqual(object.arguments.at(0), `--target=${target.triple}`)
    assert.strictEqual(link.arguments.at(0), `--target=${target.triple}`)
  }
})

it('plans standalone LLVM-Wasm finalization without native shim or host libraries', () => {
  const planned = ToolchainPlan.wasmCommand(
    clang,
    Target.wasm32UnknownUnknown,
    'release',
    'program.bc',
    'program.wasm',
  )
  assert.deepEqual(planned.arguments, [
    '--target=wasm32-unknown-unknown',
    '-nostdlib',
    '-x',
    'ir',
    'program.bc',
    '-O2',
    '-Wl,--no-entry',
    '-Wl,--export=silk_main',
    '-o',
    'program.wasm',
  ])
  assert.notInclude(planned.arguments, 'silk_shim.c')
  assert.strictEqual(
    planned.arguments.some((argument) => argument.startsWith('-l')),
    false,
  )
})

it.effect('retains LLVM-Wasm command provenance on finalization failure', () =>
  Effect.gen(function* () {
    const artifact = yield* artifactFor(Target.wasm32UnknownUnknown, 'release')
    NativeToolchain.withBuildScope('wasm-failure', (scope) => {
      const outcome = NativeToolchain.finalizeWasm(
        { _tag: 'Toolchain', clang: '/nonexistent/clang' },
        scope,
        artifact,
        Target.wasm32UnknownUnknown,
        'release',
        join(scope.root, 'program.wasm'),
      )
      assert.strictEqual(outcome._tag, 'ToolchainFailure')
      if (outcome._tag !== 'ToolchainFailure') return
      assert.strictEqual(outcome.planned.command, '/nonexistent/clang')
      assert.include(outcome.planned.arguments, '-Wl,--export=silk_main')
      assert.isAbove(outcome.output.length, 0)
    })
  }),
)

it.effect(
  'emits a release object through the pinned Clang with provenance',
  () =>
    Effect.gen(function* () {
      const target = yield* Target.host()
      const artifact = yield* artifactFor(target, 'release')
      NativeToolchain.withBuildScope('emit-object', (scope) => {
        const outcome = NativeToolchain.emitObject(toolchain, scope, artifact, target, 'release')

        assert.strictEqual(outcome._tag, 'ObjectArtifact')
        if (outcome._tag !== 'ObjectArtifact') return
        assert.strictEqual(existsSync(outcome.artifact.path), true)
        assert.strictEqual(outcome.planned.command, clang)
        assert.include(outcome.planned.arguments, '-c')
        assert.include(outcome.planned.arguments, '-O2')
      })
    }),
  15_000,
)

it.effect(
  'reuses a compiled shim across build scopes without retaining a scope-owned path',
  () =>
    Effect.gen(function* () {
      const target = yield* Target.host()
      const shimCache = NativeToolchain.makeShimCache()
      const cachedToolchain: NativeToolchain.Toolchain = Object.freeze({
        ...toolchain,
        shimCache,
      })

      NativeToolchain.withBuildScope('shim-cache-miss', (scope) => {
        const outcome = NativeToolchain.compileShim(cachedToolchain, scope, target, {
          _tag: 'PassThrough',
        })
        assert.strictEqual(outcome._tag, 'ObjectArtifact')
        if (outcome._tag !== 'ObjectArtifact') return
        assert.strictEqual(existsSync(outcome.artifact.path), true)
      })
      assert.deepEqual(NativeToolchain.shimCacheStats(shimCache), {
        entries: 1,
        hits: 0,
        misses: 1,
      })

      NativeToolchain.withBuildScope('shim-cache-hit', (scope) => {
        const outcome = NativeToolchain.compileShim(cachedToolchain, scope, target, {
          _tag: 'PassThrough',
        })
        assert.strictEqual(outcome._tag, 'ObjectArtifact')
        if (outcome._tag !== 'ObjectArtifact') return
        assert.strictEqual(existsSync(outcome.artifact.path), true)
        assert.strictEqual(outcome.artifact.scope, scope.name)
      })
      assert.deepEqual(NativeToolchain.shimCacheStats(shimCache), {
        entries: 1,
        hits: 1,
        misses: 1,
      })
    }),
  15_000,
)

it.effect('surfaces a failed process as data with command provenance', () =>
  Effect.gen(function* () {
    const target = yield* Target.host()
    const artifact = yield* artifactFor(target, 'release')
    const missing: NativeToolchain.Toolchain = Object.freeze({
      _tag: 'Toolchain',
      clang: '/nonexistent/clang',
    })
    NativeToolchain.withBuildScope('emit-failure', (scope) => {
      const outcome = NativeToolchain.emitObject(missing, scope, artifact, target, 'release')

      assert.strictEqual(outcome._tag, 'ToolchainFailure')
      if (outcome._tag !== 'ToolchainFailure') return
      assert.strictEqual(outcome.planned.command, '/nonexistent/clang')
      assert.isAbove(outcome.output.length, 0)
    })
  }),
)

it.effect('removes scope-owned intermediates at exit and honors promotion', () =>
  Effect.gen(function* () {
    const target = yield* Target.host()
    const artifact = yield* artifactFor(target, 'release')
    let scopeRoot = ''
    let promoted = ''
    NativeToolchain.withBuildScope('cleanup', (scope) => {
      scopeRoot = scope.root
      const outcome = NativeToolchain.emitObject(toolchain, scope, artifact, target, 'release')
      assert.strictEqual(outcome._tag, 'ObjectArtifact')
      if (outcome._tag !== 'ObjectArtifact') return
      promoted = NativeToolchain.promote(outcome.artifact, join(scope.root, '..', 'promoted.o'))
    })

    assert.strictEqual(existsSync(scopeRoot), false)
    assert.strictEqual(existsSync(promoted), true)
  }),
)

it.effect('rejects a missing linker input as data without invoking the driver', () =>
  Effect.gen(function* () {
    const target = yield* Target.host()
    const outcome = NativeToolchain.ClangLinker.link(
      toolchain,
      target,
      [
        Object.freeze({
          _tag: 'PathArtifact' as const,
          scope: 'x',
          path: '/nonexistent/input.o',
          target,
        }),
      ],
      [],
      '/tmp/never-written',
    )

    assert.strictEqual(outcome._tag, 'ToolchainFailure')
    if (outcome._tag !== 'ToolchainFailure') return
    assert.include(outcome.output, 'missing linker input')
  }),
)

it.effect('rejects target-mismatched bitcode and linker inputs before invoking Clang', () =>
  Effect.gen(function* () {
    const target = yield* Target.host()
    const other = Target.native.find((candidate) => candidate.id !== target.id)
    if (other === undefined) return assert.fail('expected a second native target')
    const artifact = yield* artifactFor(other, 'release')

    NativeToolchain.withBuildScope('target-mismatch', (scope) => {
      const object = NativeToolchain.emitObject(toolchain, scope, artifact, target, 'release')
      assert.strictEqual(object._tag, 'ToolchainFailure')
      if (object._tag !== 'ToolchainFailure') return
      assert.strictEqual(object.reason._tag, 'TargetMismatch')
    })

    const linked = NativeToolchain.ClangLinker.link(
      toolchain,
      target,
      [
        Object.freeze({
          _tag: 'PathArtifact' as const,
          scope: 'mismatch',
          path: '/nonexistent/other-target.o',
          target: other,
        }),
      ],
      [],
      '/tmp/never-written-target-mismatch',
    )
    assert.strictEqual(linked._tag, 'ToolchainFailure')
    if (linked._tag !== 'ToolchainFailure') return
    assert.strictEqual(linked.reason._tag, 'TargetMismatch')
  }),
)

it.effect(
  'links the shim with the program and the executable exits with the i32 result',
  () =>
    Effect.gen(function* () {
      const target = yield* Target.host()
      const artifact = yield* artifactFor(target, 'release')
      NativeToolchain.withBuildScope('link-run', (scope) => {
        const object = NativeToolchain.emitObject(toolchain, scope, artifact, target, 'release')
        const shim = NativeToolchain.compileShim(toolchain, scope, target, artifact.termination)
        assert.strictEqual(object._tag, 'ObjectArtifact')
        assert.strictEqual(shim._tag, 'ObjectArtifact')
        if (object._tag !== 'ObjectArtifact' || shim._tag !== 'ObjectArtifact') return

        const destination = join(scope.root, 'program')
        const linked = NativeToolchain.ClangLinker.link(
          toolchain,
          target,
          [object.artifact, shim.artifact],
          [],
          destination,
        )
        assert.strictEqual(linked._tag, 'Executable')
        if (linked._tag !== 'Executable') return

        const run = spawnSync(linked.path, [], { encoding: 'utf8' })
        assert.strictEqual(run.status, 42)
      })
    }),
  15_000,
)
