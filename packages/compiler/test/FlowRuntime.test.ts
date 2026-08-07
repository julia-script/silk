import { spawnSync } from 'node:child_process'
import { mkdtempSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Driver from '../src/Driver.js'
import * as Mir from '../src/Mir.js'
import type * as NativeToolchain from '../src/NativeToolchain.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct Problem { code: I32 }
flow fn risky<T>(value: T, selector: I32) -> T ! Problem {
  if selector == 0 { fail move Problem { code: 41 } }
  return move value
}
flow fn relay(value: I32) -> I32 ! Problem {
  let pending = risky<I32>(value, value)
  return run pending
}
flow fn recover(problem: Problem) -> I32 { return problem.code |> I32.add(1) }
pub fn main() -> I32 {
  let recipe = relay(0) |> Flow.catch<Problem>(recover)
  return run recipe
}`
const successSource = source.replace('relay(0)', 'relay(42)')
const trapSource = source.replace(
  'let pending = risky<I32>(value, value)',
  'let pending = risky<I32>(42 / value, 1)',
)

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-flow-runtime-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect('executes the same handled failure through the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSource(
      'flow-runtime/main',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSource(
      'flow-runtime/main',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42)
    assert.strictEqual(main(), 42)
    assert.include(wasm.ir, 'call')
    assert.include(wasm.ir, 'if')
  }),
)

it.effect('executes the handled failure through the native toolchain', () =>
  Effect.gen(function* () {
    const toolchain: NativeToolchain.Toolchain = Object.freeze({
      _tag: 'Toolchain',
      clang: '/usr/bin/clang',
    })
    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make('flow-runtime/main', ascii(source)) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'handled-failure'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(
      compiled._tag,
      'Compiled',
      compiled._tag === 'BackendFailed' ? compiled.error.message : undefined,
    )
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42, run.stderr)

    const succeeded = yield* Driver.compile({
      compilation: { root: SourceFile.make('flow-runtime/success-native', ascii(successSource)) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'successful-flow'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(
      succeeded._tag,
      'Compiled',
      succeeded._tag === 'BackendFailed' ? succeeded.error.message : undefined,
    )
    if (succeeded._tag !== 'Compiled') return
    const successRun = spawnSync(succeeded.executable, [], { encoding: 'utf8' })
    assert.strictEqual(successRun.status, 42, successRun.stderr)
  }),
)

it.effect('keeps the success path out of the exact handler on Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'flow-runtime/success',
      ascii(successSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const logical = Analysis.evaluate(snapshot)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42)
    assert.strictEqual(main(), 42)
    assert.strictEqual(
      logical._tag === 'Completed'
        ? logical.trace.filter((event) => event._tag === 'Call' && event.target.name === 'recover')
            .length
        : -1,
      0,
    )
  }),
)

it.effect('rejects a forged catch tag before backend realization', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'flow-runtime/malformed',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const mir = Analysis.loweredMir(snapshot)
    let changed = false
    const malformed: Mir.Module = Object.freeze({
      ...mir,
      functions: Object.freeze(
        mir.functions.map((fn) =>
          Object.freeze({
            ...fn,
            regions: Object.freeze(
              fn.regions.map(
                (region): Mir.Region =>
                  region._tag !== 'OperationRegion'
                    ? region
                    : Object.freeze({
                        ...region,
                        operations: Object.freeze(
                          region.operations.map((operation): Mir.Operation => {
                            if (operation._tag !== 'CatchFlow') return operation
                            changed = true
                            return Object.freeze({ ...operation, handledTag: 0 })
                          }),
                        ),
                      }),
              ),
            ),
          }),
        ),
      ),
    })

    assert.isTrue(changed)
    assert.include(
      Mir.verify(malformed).map((violation) => violation.rule),
      'InvalidFlowOperation',
    )
  }),
)

it.effect('keeps arithmetic traps outside the typed failure channel', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSource(
      'flow-runtime/trap-native',
      ascii(trapSource),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSource(
      'flow-runtime/trap-wasm',
      ascii(trapSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(logical._tag, 'Blocked')
    assert.strictEqual(logical._tag === 'Blocked' ? logical.reason._tag : undefined, 'Trap')
    assert.throws(() => main(), WebAssembly.RuntimeError)
  }),
)
