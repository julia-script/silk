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
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `struct Problem { code: I32 }
effect fn risky<T>(value: T, selector: I32) -> T ! Problem {
  if selector == 0 { fail move Problem { code: 41 } }
  return move value
}
effect fn relay(value: I32) -> I32 ! Problem {
  let pending = risky<I32>(value, value)
  return run pending
}
effect fn recover(problem: Problem) -> I32 { return problem.code |> I32.add(1) }
pub fn main() -> I32 {
  let recipe = relay(0) |> Effect.catch<Problem>(recover)
  return run recipe
}`
const successSource = source.replace('relay(0)', 'relay(42)')
const trapSource = source.replace(
  'let pending = risky<I32>(value, value)',
  'let pending = risky<I32>(42 / value, 1)',
)
const exclusiveCaptureSource = `pub fn main() -> I32 {
  let mut counter = 0
  let pending = effect { counter = counter + 1 return counter }
  let first = run pending
  let second = run pending
  return first * 10 + second
}`
const retrySource = `struct Problem { code: I32 }
effect fn retrying() -> I32 ! Problem {
  let mut counter = 0
  let work = effect {
    counter = counter + 1
    if counter < 3 { fail Problem { code: counter } }
    return counter
  }
  let retried = work |> Effect.retry(2)
  return run retried
}
effect fn recover(problem: Problem) -> I32 { return 99 }
pub fn main() -> I32 {
  let handled = retrying() |> Effect.catch<Problem>(recover)
  return run handled
}`
const providerSource = `struct Clock {}
effect fn read() -> I32 ? &Clock@Primary { return 42 }
pub fn main() -> I32 {
  let clock = Clock {}
  let provided = read() |> Clock.provide(&clock, @Primary)
  return run provided
}`
const callableMapSource = `effect fn succeed(value: I32) -> I32 { return value }
pub fn main() -> I32 { return run succeed(2) |> Effect.map(I32.add(40)) }`
const outOfMemorySource = `effect fn exhaust() -> I32 ! OutOfMemory {
  fail OutOfMemory {}
}
effect fn recover(error: OutOfMemory) -> I32 { return 42 }
pub fn main() -> I32 {
  return run exhaust() |> Effect.catch<OutOfMemory>(recover)
}`

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-effect-runtime-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect('provides an existing borrowed capability across evaluator and Wasm', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSource(
      'effect-runtime/provider-logical',
      ascii(providerSource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSource(
      'effect-runtime/provider-wasm',
      ascii(providerSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logical), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    const evaluated = Analysis.evaluate(logical)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated))
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42)
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('constructs allocation-free OutOfMemory and recovers across evaluator and Wasm', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSource(
      'effect-runtime/oom-logical',
      ascii(outOfMemorySource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSource(
      'effect-runtime/oom-wasm',
      ascii(outOfMemorySource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logical), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    const layout = Analysis.layoutOf(logical)
    const oom =
      layout._tag === 'Available' ? Analysis.callingShapeOf(logical, Type.outOfMemory) : undefined
    assert.strictEqual(oom?.lanes.length, 0)
    const evaluated = Analysis.evaluate(logical)
    assert.strictEqual(evaluated._tag, 'Completed', JSON.stringify(evaluated))
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42)
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bitcode.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('executes the same handled failure through the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSource(
      'effect-runtime/main',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSource(
      'effect-runtime/main',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(wasmSnapshot)), [])
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

it.effect('keeps callable Effect mapping in evaluator, LLVM, and Wasm parity', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSource(
      'effect-runtime/callable-map',
      ascii(callableMapSource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSource(
      'effect-runtime/callable-map',
      ascii(callableMapSource),
      'wasm32-unknown-unknown',
    )
    const logical = Analysis.evaluate(native)
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bitcode.slice()), {})
    const main = instance.exports.silk_main

    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42)
    assert.include(llvm.ir, 'callable_arith')
    assert.isFunction(main)
    if (typeof main === 'function') assert.strictEqual(main(), 42)
  }),
)

it.effect('executes the handled failure through the native toolchain', () =>
  Effect.gen(function* () {
    const toolchain: NativeToolchain.Toolchain = Object.freeze({
      _tag: 'Toolchain',
      clang: '/usr/bin/clang',
    })
    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make('effect-runtime/main', ascii(source)) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'handled-failure'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(
      compiled._tag,
      'Compiled',
      compiled._tag === 'BackendFailed'
        ? `${compiled.error.message}: ${compiled.error.reason._tag === 'WrappedFailure' ? String(compiled.error.reason.cause) : compiled.error.reason._tag}`
        : undefined,
    )
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 42, run.stderr)

    const succeeded = yield* Driver.compile({
      compilation: { root: SourceFile.make('effect-runtime/success-native', ascii(successSource)) },
      toolchain,
      profile: 'release',
      destination: join(destinationRoot, 'successful-effect'),
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
      'effect-runtime/success',
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
      'effect-runtime/malformed',
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
                            if (operation._tag !== 'CatchEffect') return operation
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
      'InvalidEffectOperation',
    )
  }),
)

it.effect('keeps arithmetic traps outside the typed failure channel', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSource(
      'effect-runtime/trap-native',
      ascii(trapSource),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSource(
      'effect-runtime/trap-wasm',
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

it.effect('preserves exclusive capture state across evaluator, native, and Wasm runs', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSource(
      'effect-runtime/exclusive-logical',
      ascii(exclusiveCaptureSource),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSource(
      'effect-runtime/exclusive-wasm',
      ascii(exclusiveCaptureSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 12)
    assert.strictEqual(main(), 12)

    const compiled = yield* Driver.compile({
      compilation: {
        root: SourceFile.make('effect-runtime/exclusive-native', ascii(exclusiveCaptureSource)),
      },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'exclusive-capture'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(
      compiled._tag,
      'Compiled',
      compiled._tag === 'BackendFailed'
        ? `${compiled.error.message}: ${compiled.error.reason._tag === 'WrappedFailure' ? String(compiled.error.reason.cause) : compiled.error.reason._tag}`
        : undefined,
    )
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 12, run.stderr)
  }),
)

it.effect('retries with fresh locals and persistent captures across all runtimes', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSource(
      'effect-runtime/retry-logical',
      ascii(retrySource),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSource(
      'effect-runtime/retry-wasm',
      ascii(retrySource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(wasmSnapshot)), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bitcode.slice()), {})
    const main = instance.exports.silk_main as () => number
    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 3)
    assert.strictEqual(main(), 3)

    const compiled = yield* Driver.compile({
      compilation: { root: SourceFile.make('effect-runtime/retry-native', ascii(retrySource)) },
      toolchain: Object.freeze({ _tag: 'Toolchain', clang: '/usr/bin/clang' }),
      profile: 'release',
      destination: join(destinationRoot, 'effect-retry'),
    }).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(
      compiled._tag,
      'Compiled',
      compiled._tag === 'BackendFailed'
        ? `${compiled.error.message}: ${compiled.error.reason._tag === 'WrappedFailure' ? String(compiled.error.reason.cause) : compiled.error.reason._tag}`
        : undefined,
    )
    if (compiled._tag !== 'Compiled') return
    const run = spawnSync(compiled.executable, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 3, run.stderr)
  }),
)
