import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const failureSource = `pub struct SomeError { code: I32 }
impl Report for SomeError {}
pub effect fn main() -> Unit ! SomeError { fail SomeError { code: 42 } }`

const successSource = `pub struct SomeError { code: I32 }
impl Report for SomeError {}
pub effect fn main() -> Unit ! SomeError { return Unit.make() }`

const cleanupSource = `pub struct SomeError { storage: Allocation }
impl Report for SomeError {}
impl Report for OutOfMemory {}
impl Drop for SomeError {
  fn drop(self: &mut SomeError) -> Unit { return Unit.make() }
}
pub effect fn main() -> Unit ! SomeError | OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[I32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Allocator.provide(&mut allocator)
  let storage = run recipe
  fail SomeError { storage: move storage }
}`

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-effect-entry-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect('runs an effect entry once and retains deterministic unhandled-failure data', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'effect-entry/failure',
      ascii(failureSource),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const program = Analysis.loweredMir(snapshot)
    assert.strictEqual(program.entry._tag, 'EffectEntry')
    assert.deepEqual(Mir.verify(program), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'UnhandledFailure')
    if (outcome._tag !== 'UnhandledFailure') return
    assert.strictEqual(outcome.tag, 1)
    assert.strictEqual(outcome.report, 'effect-entry/failure.SomeError')
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'Call').length, 2)
  }),
)

it.effect('keeps effect-entry success and failure in LLVM/direct-Wasm parity', () =>
  Effect.gen(function* () {
    for (const [name, source, expected] of [
      ['success', successSource, 0],
      ['failure', failureSource, 1],
    ] as const) {
      const native = yield* Analysis.ofSource(
        `effect-entry/${name}`,
        ascii(source),
        'aarch64-apple-darwin',
      )
      const wasm = yield* Analysis.ofSource(
        `effect-entry/${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      const llvmArtifact = yield* Analysis.codegen(native, { mode: 'release' })
      const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
      assert.deepEqual(llvmArtifact.termination, {
        _tag: 'EffectReports',
        reports: [`effect-entry/${name}.SomeError`],
      })
      assert.deepEqual(wasmArtifact.termination, llvmArtifact.termination)
      assert.strictEqual(
        llvmArtifact.symbols.find((entry) => entry.symbol === 'silk_main')?.declaration.name,
        '$effect-entry',
      )
      const instance = new WebAssembly.Instance(
        new WebAssembly.Module(wasmArtifact.bytes.slice()),
        {},
      )
      const main = instance.exports.silk_main
      assert.isFunction(main)
      if (typeof main !== 'function') return
      assert.strictEqual(main(), expected)
    }
  }),
)

it.effect('runs the selected failure payload cleanup before exposing its tag', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSource(
      'effect-entry/cleanup',
      ascii(cleanupSource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSource(
      'effect-entry/cleanup',
      ascii(cleanupSource),
      'wasm32-unknown-unknown',
    )
    const outcome = Analysis.evaluate(logical)
    assert.strictEqual(outcome._tag, 'UnhandledFailure')
    assert.include(
      outcome.trace.flatMap((event) => (event._tag === 'Call' ? [event.target.name] : [])),
      'drop@impl#2',
    )
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    assert.isFunction(main)
    if (typeof main !== 'function') return
    assert.strictEqual(main(), 1)
  }),
)

it.effect('reports an unhandled effect entry through the native shim', () =>
  Effect.gen(function* () {
    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('effect-entry/native', ascii(failureSource)),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'native-failure'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(compiled._tag, 'Compiled')
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 1)
    assert.strictEqual(run.stderr, 'Error: effect-entry/native.SomeError\n')
    const closedStderr = spawnSync(
      '/bin/sh',
      ['-c', 'exec 2>&-; exec "$1"', 'silk-effect-entry', compiled.path],
      { encoding: 'utf8' },
    )
    assert.strictEqual(closedStderr.status, 2)

    const succeeded = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('effect-entry/native-success', ascii(successSource)),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'native-success'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(succeeded._tag, 'Compiled')
    if (succeeded._tag !== 'Compiled') return
    const successRun = spawnSync(succeeded.path, [], { encoding: 'utf8' })
    assert.strictEqual(successRun.status, 0)
    assert.strictEqual(successRun.stderr, '')
  }),
)
