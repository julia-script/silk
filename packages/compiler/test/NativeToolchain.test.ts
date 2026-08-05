import { spawnSync } from 'node:child_process'
import { existsSync } from 'node:fs'
import { join } from 'node:path'
import { assert, it } from '@effect/vitest'
import * as Analysis from '../src/Analysis.js'
import * as NativeToolchain from '../src/NativeToolchain.js'
import * as ToolchainPlan from '../src/ToolchainPlan.js'

const clang = '/usr/bin/clang'
const toolchain: NativeToolchain.Toolchain = Object.freeze({ _tag: 'Toolchain', clang })

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

const bitcodeFor = (profile: ToolchainPlan.OptimizationProfile): Uint8Array => {
  const snapshot = Analysis.ofSource('memory://native.silk', ascii(nestedSource))
  return Analysis.codegen(
    snapshot,
    { mode: ToolchainPlan.codegenModeFor(profile) },
    NativeToolchain.hostLayout(),
  ).bitcode
}

it('plans fixed profile arguments and nothing else varies', () => {
  const debug = ToolchainPlan.objectCommand(clang, 'debug', 'in.bc', 'out.o')
  const release = ToolchainPlan.objectCommand(clang, 'release', 'in.bc', 'out.o')
  const releaseDebug = ToolchainPlan.objectCommand(clang, 'release-with-debug', 'in.bc', 'out.o')

  assert.deepEqual(debug.arguments, ['-c', '-x', 'ir', 'in.bc', '-O0', '-g', '-o', 'out.o'])
  assert.deepEqual(release.arguments, ['-c', '-x', 'ir', 'in.bc', '-O2', '-o', 'out.o'])
  assert.deepEqual(releaseDebug.arguments, ['-c', '-x', 'ir', 'in.bc', '-O2', '-g', '-o', 'out.o'])
})

it('emits a release object through the pinned Clang with provenance', () => {
  NativeToolchain.withBuildScope('emit-object', (scope) => {
    const outcome = NativeToolchain.emitObject(toolchain, scope, bitcodeFor('release'), 'release')

    assert.strictEqual(outcome._tag, 'ObjectArtifact')
    if (outcome._tag !== 'ObjectArtifact') return
    assert.strictEqual(existsSync(outcome.artifact.path), true)
    assert.strictEqual(outcome.planned.command, clang)
    assert.include(outcome.planned.arguments, '-c')
    assert.include(outcome.planned.arguments, '-O2')
  })
})

it('surfaces a failed process as data with command provenance', () => {
  const missing: NativeToolchain.Toolchain = Object.freeze({
    _tag: 'Toolchain',
    clang: '/nonexistent/clang',
  })
  NativeToolchain.withBuildScope('emit-failure', (scope) => {
    const outcome = NativeToolchain.emitObject(missing, scope, bitcodeFor('release'), 'release')

    assert.strictEqual(outcome._tag, 'ToolchainFailure')
    if (outcome._tag !== 'ToolchainFailure') return
    assert.strictEqual(outcome.planned.command, '/nonexistent/clang')
    assert.isAbove(outcome.output.length, 0)
  })
})

it('removes scope-owned intermediates at exit and honors promotion', () => {
  let scopeRoot = ''
  let promoted = ''
  NativeToolchain.withBuildScope('cleanup', (scope) => {
    scopeRoot = scope.root
    const outcome = NativeToolchain.emitObject(toolchain, scope, bitcodeFor('release'), 'release')
    assert.strictEqual(outcome._tag, 'ObjectArtifact')
    if (outcome._tag !== 'ObjectArtifact') return
    promoted = NativeToolchain.promote(outcome.artifact, join(scope.root, '..', 'promoted.o'))
  })

  assert.strictEqual(existsSync(scopeRoot), false)
  assert.strictEqual(existsSync(promoted), true)
})

it('rejects a missing linker input as data without invoking the driver', () => {
  const outcome = NativeToolchain.ClangLinker.link(
    toolchain,
    [Object.freeze({ _tag: 'PathArtifact' as const, scope: 'x', path: '/nonexistent/input.o' })],
    [],
    '/tmp/never-written',
  )

  assert.strictEqual(outcome._tag, 'ToolchainFailure')
  if (outcome._tag !== 'ToolchainFailure') return
  assert.include(outcome.output, 'missing linker input')
})

it('links the shim with the program and the executable exits with the I32 result', () => {
  NativeToolchain.withBuildScope('link-run', (scope) => {
    const object = NativeToolchain.emitObject(toolchain, scope, bitcodeFor('release'), 'release')
    const shim = NativeToolchain.compileShim(toolchain, scope)
    assert.strictEqual(object._tag, 'ObjectArtifact')
    assert.strictEqual(shim._tag, 'ObjectArtifact')
    if (object._tag !== 'ObjectArtifact' || shim._tag !== 'ObjectArtifact') return

    const destination = join(scope.root, 'program')
    const linked = NativeToolchain.ClangLinker.link(
      toolchain,
      [object.artifact, shim.artifact],
      [],
      destination,
    )
    assert.strictEqual(linked._tag, 'Executable')
    if (linked._tag !== 'Executable') return

    const run = spawnSync(linked.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42)
  })
})
