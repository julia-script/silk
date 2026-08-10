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

const failureSource = `pub struct SomeError { code: i32 }
impl Report for SomeError {}
pub effect fn main() -> () ! SomeError { fail SomeError { code: 42 } }`

const successSource = `pub struct SomeError { code: i32 }
impl Report for SomeError {}
pub effect fn main() -> () ! SomeError { return () }`

const cleanupSource = `pub struct SomeError { storage: Allocation }
impl Report for SomeError {}
impl Report for OutOfMemory {}
impl Drop for SomeError {
  fn drop(self: &mut SomeError) -> () { return () }
}
pub effect fn main() -> () ! SomeError | OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.bindRequirement(&mut allocator)
  let storage = run recipe
  fail SomeError { storage: move storage }
}`

const evaluateOrderingSource = `pub struct SomeError {}
impl Report for SomeError {}
pub effect fn main() -> () ! SomeError {
  let mut counter = 0
  run effect { counter = counter + 1 return () }
  run effect { counter = counter * 10 + 2 return () }
  if counter != 12 { fail SomeError {} }
  return ()
}`

const evaluateFailureSource = `pub struct SomeError {}
pub struct Guard { storage: Allocation }
impl Report for SomeError {}
impl Report for OutOfMemory {}
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () { return () }
}
effect fn stop() -> never ! SomeError { fail SomeError {} }
pub effect fn main() -> () ! SomeError | OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout) |> Effect.bindRequirement(&mut allocator)
  let guard = Guard { storage: move storage }
  run stop()
  drop guard
  return ()
}`

const providedMapSource = `struct Clock {}
effect fn read() -> i32 ? &Clock { return 42 }
fn verify(value: i32) -> () {
  return ()
}
pub effect fn main() -> () {
  let clock = Clock {}
  return run read()
    |> Effect.provide(&clock)
    |> Effect.map(verify)
}`

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-effect-entry-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect('runs an effect entry once and retains deterministic unhandled-failure data', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
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

it.effect('composes capability provision and mapping in an effect entry', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect-entry/provided-map',
      ascii(providedMapSource),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(snapshot)), [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome))
  }),
)

it.effect('keeps effect-entry success and failure in LLVM/direct-Wasm parity', () =>
  Effect.gen(function* () {
    for (const [name, source, expected] of [
      ['success', successSource, 0],
      ['failure', failureSource, 1],
    ] as const) {
      const native = yield* Analysis.ofSourceRealized(
        `effect-entry/${name}`,
        ascii(source),
        'aarch64-apple-darwin',
      )
      const wasm = yield* Analysis.ofSourceRealized(
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
    const logical = yield* Analysis.ofSourceRealized(
      'effect-entry/cleanup',
      ascii(cleanupSource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
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

it.effect('executes Evaluate effects in order and halts on failure across every engine', () =>
  Effect.gen(function* () {
    for (const [name, source, expected] of [
      ['evaluate-success', evaluateOrderingSource, 0],
      ['evaluate-failure', evaluateFailureSource, 1],
    ] as const) {
      const module = `effect-entry/${name}`
      const logical = yield* Analysis.ofSourceRealized(
        module,
        ascii(source),
        'aarch64-apple-darwin',
      )
      const wasm = yield* Analysis.ofSourceRealized(module, ascii(source), 'wasm32-unknown-unknown')
      assert.deepEqual(Analysis.diagnostics(logical), [])
      assert.deepEqual(Analysis.diagnostics(wasm), [])
      assert.deepEqual(Mir.verify(Analysis.loweredMir(logical)), [])

      const evaluated = Analysis.evaluate(logical)
      assert.strictEqual(
        evaluated._tag,
        expected === 0 ? 'Completed' : 'UnhandledFailure',
        JSON.stringify(evaluated),
      )
      if (name === 'evaluate-failure') {
        assert.strictEqual(
          evaluated.trace.filter(
            (event) => event._tag === 'Call' && event.target.name === 'drop@impl#2',
          ).length,
          1,
        )
      }

      const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      const wasmMain = instance.exports.silk_main
      assert.isFunction(wasmMain)
      if (typeof wasmMain === 'function') assert.strictEqual(wasmMain(), expected)

      const compiled = yield* Driver.compile({
        compilation: { root: SourceFile.make(module, ascii(source)) },
        toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
        profile: 'release',
        destination: join(destinationRoot, name),
      }).pipe(Effect.provide(SourceResolver.empty))
      assert.strictEqual(compiled._tag, 'Compiled')
      if (compiled._tag !== 'Compiled') continue
      const native = spawnSync(compiled.path, [], { encoding: 'utf8' })
      assert.strictEqual(native.status, expected, native.stderr)
    }
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
