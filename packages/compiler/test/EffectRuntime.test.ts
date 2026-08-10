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

const source = `struct Problem { code: i32 }
effect fn risky<T>(value: T, selector: i32) -> T ! Problem {
  if selector == 0 { fail move Problem { code: 41 } }
  return move value
}
effect fn relay(value: i32) -> i32 ! Problem {
  let pending = risky<i32>(value, value)
  return run pending
}
effect fn recover(problem: Problem) -> i32 { return problem.code |> i32.add(1) }
pub fn main() -> i32 {
  let recipe = relay(0) |> Effect.catch(recover)
  return run recipe
}`
const successSource = source.replace('relay(0)', 'relay(42)')
const trapSource = source.replace(
  'let pending = risky<i32>(value, value)',
  'let pending = risky<i32>(42 / value, 1)',
)
const exclusiveCaptureSource = `pub fn main() -> i32 {
  let mut counter = 0
  let pending = effect { counter = counter + 1 return counter }
  let first = run pending
  let second = run pending
  return first * 10 + second
}`
const retrySource = `struct Problem { code: i32 }
effect fn retrying() -> i32 ! Problem {
  let mut counter = 0
  let work = effect {
    counter = counter + 1
    if counter < 3 { fail Problem { code: counter } }
    return counter
  }
  let retried = move work |> Effect.retry(2)
  return run retried
}
effect fn recover(problem: Problem) -> i32 { return 99 }
pub fn main() -> i32 {
  let handled = retrying() |> Effect.catch(recover)
  return run handled
}`
const providerSource = `struct Clock {}
effect fn read() -> i32 ? &Clock@Primary { return 42 }
pub fn main() -> i32 {
  let clock = Clock {}
  let provided = read() |> Effect.bindRequirement(&clock, @Primary)
  return run provided
}`
const callableMapSource = `effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(2) |> Effect.map(i32.add(40)) }`
const pipelinePrelude = `struct Clock {}
effect fn read() -> i32 ? &Clock { return 20 }
fn add(value: i32) -> i32 { return value + 1 }
fn double(value: i32) -> i32 { return value * 2 }
effect fn increment(value: i32) -> i32 { return value + 1 }
effect fn observe(value: i32) -> i32 { return value }`
const pipelineSources = [
  {
    name: 'grouped',
    expected: 42,
    source: `${pipelinePrelude}
pub fn main() -> i32 {
  let clock = Clock {}
  return run ((read() |> Effect.provide(&clock)) |> Effect.map(add)) |> Effect.map(double)
}`,
  },
  {
    name: 'reverse',
    expected: 42,
    source: `${pipelinePrelude}
pub fn main() -> i32 {
  let clock = Clock {}
  return run read()
    |> Effect.map(add)
    |> Effect.provide(&clock)
    |> Effect.map(double)
}`,
  },
  {
    name: 'provided-last',
    expected: 42,
    source: `${pipelinePrelude}
pub fn main() -> i32 {
  let clock = Clock {}
  return run read()
    |> Effect.map(add)
    |> Effect.map(double)
    |> Effect.provide(&clock)
}`,
  },
  {
    name: 'data-first',
    expected: 42,
    source: `${pipelinePrelude}
pub fn main() -> i32 {
  let clock = Clock {}
  return run Effect.map(Effect.provide(Effect.map(read(), add), &clock), double)
}`,
  },
  {
    name: 'stored',
    expected: 42,
    source: `${pipelinePrelude}
pub fn main() -> i32 {
  let clock = Clock {}
  let mapped = read() |> Effect.map(add)
  let provided = mapped |> Effect.provide(&clock)
  let mappedAgain = provided |> Effect.map(double)
  return run mappedAgain
}`,
  },
] as const
const effectOperatorPipelineSource = `${pipelinePrelude}
pub fn main() -> i32 {
  let clock = Clock {}
  return run read()
    |> Effect.flatMap(increment)
    |> Effect.tap(observe)
    |> Effect.map(double)
    |> Effect.provide(&clock)
}`
const storedEffectOperatorPipelineSource = `${pipelinePrelude}
pub fn main() -> i32 {
  let clock = Clock {}
  let flatMapped = read() |> Effect.flatMap(increment)
  let tapped = flatMapped |> Effect.tap(observe)
  let mapped = tapped |> Effect.map(double)
  let provided = mapped |> Effect.provide(&clock)
  return run provided
}`
const recoveryPipelineSource = `struct Problem { code: i32 }
effect fn failValue() -> i32 ! Problem { fail Problem { code: 21 } }
effect fn recover(problem: Problem) -> i32 { return problem.code }
fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 {
  return run failValue()
    |> Effect.catch(recover)
    |> Effect.map(double)
}`
const retryMapSource = `${retrySource.replace(
  'let handled = retrying() |> Effect.catch(recover)',
  'let handled = retrying() |> Effect.catch(recover) |> Effect.map(i32.add(39))',
)}`
const outOfMemorySource = `effect fn exhaust() -> i32 ! OutOfMemory {
  fail OutOfMemory {}
}
effect fn recover(error: OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 {
  return run exhaust() |> Effect.catch(recover)
}`
const higherOrderEffectSource = `effect fn succeed(value: i32) -> i32 { return value }
effect fn alternate(value: i32) -> i32 { return value }
fn pass(self: once Effect<i32>) -> once Effect<i32> { return move self }
fn specialize<A, !E, ?R>(self: once Effect<A ! E ? R>) -> once Effect<A ! E ? R> { return move self }
fn wrap(self: once Effect<i32>) -> once Effect<i32> {
  return effect { return run self }
}
pub fn main() -> i32 {
  let stored = specialize(pass(succeed(20)))
  let distinct = pass(alternate(0))
  let captured = wrap(succeed(22))
  return (run stored) + (run distinct) + (run captured)
}`
const exclusiveHigherOrderEffectSource = `fn forward(self: mut Effect<i32>) -> mut Effect<i32> { return move self }
pub fn main() -> i32 {
  let mut counter = 40
  let pending = effect { counter = counter + 1 return counter }
  let forwarded = forward(move pending)
  let first = run forwarded
  let second = run forwarded
  return first * 100 + second
}`
const takeHigherOrderEffectSource = `struct Payload { value: i32 }
fn forward(self: once Effect<Payload>) -> once Effect<Payload> { return move self }
pub fn main() -> i32 {
  let payload = Payload { value: 42 }
  let pending = effect { return move payload }
  let forwarded = forward(move pending)
  let result = run forwarded
  return result.value
}`
const droppedHigherOrderEffectSource = `struct Payload { storage: Allocation }
impl Drop for Payload {
  fn drop(self: &mut Payload) -> () { return () }
}
impl Report for OutOfMemory {}
fn discard(self: once Effect<Payload>) -> () {
  drop self
  return ()
}
pub effect fn main() -> () ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let payload = Payload { storage: move storage }
  let pending = effect { return move payload }
  discard(move pending)
  return ()
}`

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-effect-runtime-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))

it.effect('passes, returns, stores, captures, and specializes closed Effect values', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSourceRealized(
      'effect-runtime/higher-order-logical',
      ascii(higherOrderEffectSource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
      'effect-runtime/higher-order-wasm',
      ascii(higherOrderEffectSource),
      'wasm32-unknown-unknown',
    )

    assert.deepEqual(Analysis.diagnostics(logical), [])
    assert.deepEqual(Analysis.diagnostics(wasm), [])
    const passInstances = Analysis.instancesOf(logical).instances.filter(
      (instance) => instance.key.declaration.name === 'pass',
    )
    assert.strictEqual(passInstances.length, 2)
    assert.strictEqual(
      new Set(
        passInstances.flatMap((instance) =>
          instance.key.typeArguments
            .filter(Type.isEffectIdentityArgument)
            .map((argument) => argument.identity),
        ),
      ).size,
      2,
    )
    assert.deepEqual(Mir.verify(Analysis.loweredMir(wasm)), [])
    const evaluated = Analysis.evaluate(logical)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      `${JSON.stringify(evaluated)}\n${Mir.encode(Analysis.loweredMir(logical))}`,
    )
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42)
    const native = yield* Analysis.codegen(logical, { mode: 'release' })
    assert.include(native.ir, 'define')
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('preserves exclusive and take-once Effect access across ordinary calls', () =>
  Effect.gen(function* () {
    for (const [name, source, expected] of [
      ['exclusive', exclusiveHigherOrderEffectSource, 4142],
      ['take', takeHigherOrderEffectSource, 42],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `effect-runtime/higher-order-${name}`,
        ascii(source),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
      assert.deepEqual(Mir.verify(Analysis.loweredMir(snapshot)), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        `${name}: ${JSON.stringify(evaluated)}\n${Mir.encode(Analysis.loweredMir(snapshot))}`,
      )
      assert.strictEqual(
        evaluated._tag === 'Completed' ? evaluated.result.value : undefined,
        expected,
        name,
      )
      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), expected, name)
    }
  }),
)

it.effect('releases a dropped unrun Effect environment once across an ordinary call', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/higher-order-drop',
      ascii(droppedHigherOrderEffectSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(
      evaluated.trace.filter(
        (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl#'),
      ).length,
      1,
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 0)
  }),
)

it.effect('provides an existing borrowed capability across evaluator and Wasm', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSourceRealized(
      'effect-runtime/provider-logical',
      ascii(providerSource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
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
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('constructs allocation-free OutOfMemory and recovers across evaluator and Wasm', () =>
  Effect.gen(function* () {
    const logical = yield* Analysis.ofSourceRealized(
      'effect-runtime/oom-logical',
      ascii(outOfMemorySource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
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
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('executes the same handled failure through the evaluator and Wasm', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/main',
      ascii(source),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/main',
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(wasmSnapshot)), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42)
    assert.strictEqual(main(), 42)
    assert.include(wasm.wat, 'call')
    assert.include(wasm.wat, 'if')
  }),
)

it.effect('keeps callable Effect mapping in evaluator, LLVM, and Wasm parity', () =>
  Effect.gen(function* () {
    const native = yield* Analysis.ofSourceRealized(
      'effect-runtime/callable-map',
      ascii(callableMapSource),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
      'effect-runtime/callable-map',
      ascii(callableMapSource),
      'wasm32-unknown-unknown',
    )
    const logical = Analysis.evaluate(native)
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main

    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42)
    assert.include(llvm.ir, 'callable_arith')
    assert.isFunction(main)
    if (typeof main === 'function') assert.strictEqual(main(), 42)
  }),
)

it.effect('keeps grouped, reverse, data-first, and stored pipelines equivalent', () =>
  Effect.gen(function* () {
    for (const sample of pipelineSources) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `effect-runtime/pipeline-${sample.name}`,
        ascii(sample.source),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], sample.name)
      assert.deepEqual(Mir.verify(Analysis.loweredMir(snapshot)), [], sample.name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        `${sample.name}: ${JSON.stringify(evaluated)}`,
      )
      assert.strictEqual(
        evaluated._tag === 'Completed' ? evaluated.result.value : undefined,
        sample.expected,
        sample.name,
      )
    }
  }),
)

it.effect('composes flatMap and tap with mapping and provision', () =>
  Effect.gen(function* () {
    for (const [name, source] of [
      ['direct', effectOperatorPipelineSource],
      ['stored', storedEffectOperatorPipelineSource],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `effect-runtime/operator-pipeline-${name}`,
        ascii(source),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
      assert.deepEqual(Mir.verify(Analysis.loweredMir(snapshot)), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        `${name}: ${JSON.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? value.toString() : value))}`,
      )
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, 42, name)
      assert.isAtLeast(
        evaluated.trace.filter((event) => event._tag === 'Call' && event.target.name === 'observe')
          .length,
        1,
        name,
      )
    }
  }),
)

it.effect('continues transforming recovered and retried effects', () =>
  Effect.gen(function* () {
    for (const [name, source] of [
      ['catch-map', recoveryPipelineSource],
      ['retry-map', retryMapSource],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `effect-runtime/${name}`,
        ascii(source),
        'aarch64-apple-darwin',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [], name)
      assert.deepEqual(Mir.verify(Analysis.loweredMir(snapshot)), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        `${name}: ${JSON.stringify(evaluated, (_, value) => (typeof value === 'bigint' ? value.toString() : value))}`,
      )
      assert.strictEqual(
        evaluated._tag === 'Completed' ? evaluated.result.value : undefined,
        42,
        name,
      )
    }
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
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
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
    const successRun = spawnSync(succeeded.path, [], { encoding: 'utf8' })
    assert.strictEqual(successRun.status, 42, successRun.stderr)
  }),
)

it.effect('keeps the success path out of the exact handler on Wasm', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/success',
      ascii(successSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const logical = Analysis.evaluate(snapshot)
    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
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

it.effect('keeps arithmetic traps outside the typed failure channel', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/trap-native',
      ascii(trapSource),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/trap-wasm',
      ascii(trapSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(logical._tag, 'Blocked')
    assert.strictEqual(logical._tag === 'Blocked' ? logical.reason._tag : undefined, 'Trap')
    assert.throws(() => main(), WebAssembly.RuntimeError)
  }),
)

it.effect('preserves exclusive capture state across evaluator, native, and Wasm runs', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/exclusive-logical',
      ascii(exclusiveCaptureSource),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/exclusive-wasm',
      ascii(exclusiveCaptureSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
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
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 12, run.stderr)
  }),
)

it.effect('retries with fresh locals and persistent captures across all runtimes', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/retry-logical',
      ascii(retrySource),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/retry-wasm',
      ascii(retrySource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])
    assert.deepEqual(Mir.verify(Analysis.loweredMir(wasmSnapshot)), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
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
    const run = spawnSync(compiled.path, [], { encoding: 'utf8' })
    assert.strictEqual(run.status, 3, run.stderr)
  }),
)
