import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Intrinsic from '../src/Intrinsic.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as Type from '../src/Type.js'
import * as Json from './support/Json.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const golden = (name: string): string =>
  readFileSync(new URL(`./goldens/${name}`, import.meta.url), 'utf8')

const source = `import silk.effect { Effect }
import silk.i32 as i32
struct Problem { code: i32 }
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
  let recipe = relay(0) |> Effect.catchAll(recover)
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
const retrySource = `import silk.effect { Effect }
import silk.i32 as i32
struct Problem { code: i32 }
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
  let handled = retrying() |> Effect.catchAll(recover)
  return run handled
}`
const providerSource = `role Primary
service Clock { effect fn tick() -> i32 ? &Clock }
struct FixedClock { marker: i32 }
effect fn tick(self: &FixedClock) -> i32 { return self.marker }
impl Clock for FixedClock { tick: FixedClock.tick }
effect fn read() -> i32 ? &Clock at Primary { return 42 }
pub fn main() -> i32 {
  let clock = FixedClock { marker: 0 }
  let provided = read() |> Intrinsic.bindRequirement<Clock at Primary>(&clock)
  return run provided
}`
const callableMapSource = `import silk.effect { Effect }
import silk.i32 as i32
effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(2) |> Effect.map(i32.add(40)) }`
const ofSource = `import silk.effect { Effect }
struct Token { value: i32 }
pub fn main() -> i32 {
  let copied = Effect.of(20)
  let token = Token { value: 22 }
  let transferred = Effect.of(move token)
  let captured = run transferred
  return (run copied) + captured.value
}`
const flattenPrelude = `effect fn inner(value: i32) -> i32 { return value * 2 }
effect fn outer(value: i32) -> Effect<i32> { return inner(value) }`
const flattenSource = `import silk.effect { Effect }
${flattenPrelude}
pub fn main() -> i32 {
  let nested = outer(21)
  let flattened = Effect.flatten(move nested)
  return run flattened
}`
const flattenPipedSource = `import silk.effect { Effect }
${flattenPrelude}
pub fn main() -> i32 { return run (outer(21) |> Effect.flatten) }`
const flattenRowsSource = `import silk.effect { Effect }
struct Outer { code: i32 }
struct Inner { code: i32 }
service Clock {}
service Meter {}
effect fn inner() -> i32 ! Inner ? &Clock | &Meter { return 21 }
effect fn outer() -> Effect<i32 ! Inner ? &Clock | &Meter> ! Outer ? &Clock { return inner() }
pub fn main() -> i32 {
  let nested = outer()
  let flattened = Effect.flatten(move nested)
  return 0
}`
const pipelinePrelude = `service Clock {}
struct FixedClock { marker: i32 }
impl Clock for FixedClock {}
effect fn read() -> i32 ? &Clock { return 20 }
fn add(value: i32) -> i32 { return value + 1 }
fn double(value: i32) -> i32 { return value * 2 }
effect fn increment(value: i32) -> i32 { return value + 1 }
effect fn observe(value: i32) -> i32 { return value }`
const pipelineSources = [
  {
    name: 'grouped',
    expected: 42,
    source: `import silk.effect { Effect }
${pipelinePrelude}
pub fn main() -> i32 {
  let clock = FixedClock { marker: 0 }
  return run ((read() |> Effect.provide(&clock)) |> Effect.map(add)) |> Effect.map(double)
}`,
  },
  {
    name: 'reverse',
    expected: 42,
    source: `import silk.effect { Effect }
${pipelinePrelude}
pub fn main() -> i32 {
  let clock = FixedClock { marker: 0 }
  return run read()
    |> Effect.map(add)
    |> Effect.provide(&clock)
    |> Effect.map(double)
}`,
  },
  {
    name: 'provided-last',
    expected: 42,
    source: `import silk.effect { Effect }
${pipelinePrelude}
pub fn main() -> i32 {
  let clock = FixedClock { marker: 0 }
  return run read()
    |> Effect.map(add)
    |> Effect.map(double)
    |> Effect.provide(&clock)
}`,
  },
  {
    name: 'data-first',
    expected: 42,
    source: `import silk.effect { Effect }
${pipelinePrelude}
pub fn main() -> i32 {
  let clock = FixedClock { marker: 0 }
  return run Effect.map(Effect.provide(Effect.map(read(), add), &clock), double)
}`,
  },
  {
    name: 'stored',
    expected: 42,
    source: `import silk.effect { Effect }
${pipelinePrelude}
pub fn main() -> i32 {
  let clock = FixedClock { marker: 0 }
  let mapped = read() |> Effect.map(add)
  let provided = mapped |> Effect.provide(&clock)
  let mappedAgain = provided |> Effect.map(double)
  return run mappedAgain
}`,
  },
] as const
const effectOperatorPipelineSource = `import silk.effect { Effect }
${pipelinePrelude}
pub fn main() -> i32 {
  let clock = FixedClock { marker: 0 }
  return run read()
    |> Effect.flatMap(increment)
    |> Effect.tap(observe)
    |> Effect.map(double)
    |> Effect.provide(&clock)
}`
const storedEffectOperatorPipelineSource = `import silk.effect { Effect }
${pipelinePrelude}
pub fn main() -> i32 {
  let clock = FixedClock { marker: 0 }
  let flatMapped = read() |> Effect.flatMap(increment)
  let tapped = flatMapped |> Effect.tap(observe)
  let mapped = tapped |> Effect.map(double)
  let provided = mapped |> Effect.provide(&clock)
  return run provided
}`
const recoveryPipelineSource = `import silk.effect { Effect }
struct Problem { code: i32 }
effect fn failValue() -> i32 ! Problem { fail Problem { code: 21 } }
effect fn recover(problem: Problem) -> i32 { return problem.code }
fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 {
  return run failValue()
    |> Effect.catchAll(recover)
    |> Effect.map(double)
}`
const retryMapSource = `${retrySource.replace(
  'let handled = retrying() |> Effect.catchAll(recover)',
  'let handled = retrying() |> Effect.catchAll(recover) |> Effect.map(i32.add(39))',
)}`
const outOfMemoryErrorSource = `import silk.effect { Effect }
import silk.allocator { OutOfMemoryError }

effect fn exhaust() -> i32 ! OutOfMemoryError {
  fail OutOfMemoryError {}
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 {
  return run exhaust() |> Effect.catchAll(recover)
}`
const higherOrderEffectSource = `effect fn succeed(value: i32) -> i32 { return value }
effect fn alternate(value: i32) -> i32 { return value }
fn pass(self: once Effect<i32>) -> once Effect<i32> { return move self }
fn specialize<A, E, ?R>(self: once Effect<A ! E ? R>) -> once Effect<A ! E ? R> { return move self }
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
const droppedHigherOrderEffectSource = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
struct Payload { storage: Allocation }
impl Drop for Payload {
  fn drop(self: &mut Payload) -> () { return () }
}
fn discard(self: once Effect<Payload>) -> () {
  drop self
  return ()
}
pub effect fn main() -> () ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<i32>()
  let storage = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let payload = Payload { storage: move storage }
  let pending = effect { return move payload }
  discard(move pending)
  return ()
}`
const unusedOwnedParameterCleanupSource = `import silk.layout { Layout }
struct Payload { storage: Allocation }
impl Drop for Payload {
  fn drop(self: &mut Payload) -> () { return () }
}
effect fn payload() -> Payload ! Intrinsic.StorageFailure {
  let layout = Layout.of<i32>()
  let storage = run Intrinsic.systemAllocationAcquire(move layout)
  return Payload { storage: move storage }
}
effect fn hold(first: Payload, middle: Payload, last: Payload) -> () {
  drop middle
  return ()
}
pub effect fn main() -> () ! Intrinsic.StorageFailure {
  let pending = hold(run payload(), run payload(), run payload())
  drop pending
  return ()
}`

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
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(wasm)), [])
    const evaluated = Analysis.evaluate(logical)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      `${Json.stringify(evaluated)}\n${MirEncoding.encode(Analysis.loweredMir(logical))}`,
    )
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)
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
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        `${name}: ${Json.stringify(evaluated)}\n${MirEncoding.encode(Analysis.loweredMir(snapshot))}`,
      )
      assert.strictEqual(
        evaluated._tag === 'Completed' ? evaluated.result.value : undefined,
        BigInt(expected),
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

it.effect('releases unused owned effect-function arguments in reverse declaration order', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/unused-owned-arguments',
      ascii(unusedOwnedParameterCleanupSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.deepEqual(
      evaluated.trace.flatMap((event): ReadonlyArray<number> =>
        event._tag === 'AllocationRelease' ? [event.ticket] : [],
      ),
      [2, 1, 0],
    )
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
    assert.strictEqual(evaluated._tag, 'Completed', Json.stringify(evaluated))
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'constructs allocation-free OutOfMemoryError and recovers across evaluator and Wasm',
  () =>
    Effect.gen(function* () {
      const logical = yield* Analysis.ofSourceRealized(
        'effect-runtime/oom-logical',
        ascii(outOfMemoryErrorSource),
        'aarch64-apple-darwin',
      )
      const wasm = yield* Analysis.ofSourceRealized(
        'effect-runtime/oom-wasm',
        ascii(outOfMemoryErrorSource),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(logical), [])
      assert.deepEqual(Analysis.diagnostics(wasm), [])
      const layout = Analysis.layoutOf(logical)
      const oom =
        layout._tag === 'Available'
          ? Projections.callingShapeOf(logical, Type.nominal('silk/allocator', 'OutOfMemoryError'))
          : undefined
      assert.strictEqual(oom?.lanes.length, 0)
      const evaluated = Analysis.evaluate(logical)
      assert.strictEqual(evaluated._tag, 'Completed', Json.stringify(evaluated))
      assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)
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
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(wasmSnapshot)), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const main = instance.exports.silk_main as () => number

    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42n)
    if (logical._tag === 'Completed') {
      assert.isAbove(logical.history.length, 0)
      assert.isTrue(logical.history.every((failure) => failure.recovered))
      assert.isTrue(logical.history.some((failure) => failure.logicalPath.length > 0))
    }
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

    const encodedMir = MirEncoding.encode(Analysis.loweredMir(native))
    assert.strictEqual(encodedMir, golden('effect.mir.txt'))
    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42n)
    assert.include(llvm.ir, '@silk_silk_i32_add')
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
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [], sample.name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        `${sample.name}: ${Json.stringify(evaluated)}`,
      )
      assert.strictEqual(
        evaluated._tag === 'Completed' ? evaluated.result.value : undefined,
        BigInt(sample.expected),
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
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', `${name}: ${Json.stringify(evaluated)}`)
      if (evaluated._tag !== 'Completed') continue
      assert.strictEqual(evaluated.result.value, 42n, name)
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
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [], name)
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', `${name}: ${Json.stringify(evaluated)}`)
      assert.strictEqual(
        evaluated._tag === 'Completed' ? evaluated.result.value : undefined,
        42n,
        name,
      )
    }
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
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42n)
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

    assert.strictEqual(logical._tag, 'Trap')
    assert.throws(() => main(), WebAssembly.RuntimeError)
  }),
)

it.effect('preserves exclusive capture state across evaluator and Wasm runs', () =>
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
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 12n)
    assert.strictEqual(main(), 12)
  }),
)

it.effect('lifts Copy and affine values through ordinary Effect.of source', () =>
  Effect.gen(function* () {
    const logicalSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/of-logical',
      ascii(ofSource),
      'aarch64-apple-darwin',
    )
    const wasmSnapshot = yield* Analysis.ofSourceRealized(
      'effect-runtime/of-wasm',
      ascii(ofSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(logicalSnapshot), [])
    assert.deepEqual(Analysis.diagnostics(wasmSnapshot), [])

    const occurrence = Analysis.semanticOccurrenceAt(
      logicalSnapshot,
      'effect-runtime/of-logical',
      ofSource.indexOf('Effect.of') + 'Effect.'.length,
    )
    assert.strictEqual(occurrence?.role, 'Value')
    assert.strictEqual(occurrence?.declaration?.module, 'silk/effect')
    assert.include(
      occurrence === undefined
        ? ''
        : (Analysis.occurrencePresentation(logicalSnapshot, 'effect-runtime/of-logical', occurrence)
            ?.text ?? ''),
      'pub effect fn of',
    )

    const logical = Analysis.evaluate(logicalSnapshot)
    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42n)

    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('retries with fresh locals and persistent captures across evaluator and Wasm', () =>
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
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(wasmSnapshot)), [])
    const logical = Analysis.evaluate(logicalSnapshot)
    const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    const main = instance.exports.silk_main as () => number
    assert.strictEqual(logical._tag, 'Completed')
    assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 3n)
    assert.strictEqual(main(), 3)
  }),
)

it.effect('flattens one nested Effect layer across evaluator, LLVM, and Wasm', () =>
  Effect.gen(function* () {
    for (const [name, text] of [
      ['direct', flattenSource],
      ['piped', flattenPipedSource],
    ] as const) {
      const native = yield* Analysis.ofSourceRealized(
        `effect-runtime/flatten-${name}`,
        ascii(text),
        'aarch64-apple-darwin',
      )
      const wasm = yield* Analysis.ofSourceRealized(
        `effect-runtime/flatten-${name}`,
        ascii(text),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(native), [], name)
      assert.deepEqual(Analysis.diagnostics(wasm), [], name)
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(wasm)), [], name)

      const logical = Analysis.evaluate(native)
      assert.strictEqual(logical._tag, 'Completed', `${name}: ${Json.stringify(logical)}`)
      assert.strictEqual(logical._tag === 'Completed' ? logical.result.value : undefined, 42n, name)
      const llvm = yield* Analysis.codegen(native, { mode: 'release' })
      assert.include(llvm.ir, 'define', name)
      const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42, name)
    }
  }),
)

it.effect('unions both failure rows and both requirement rows through flatten', () =>
  Effect.gen(function* () {
    const module = 'effect-runtime/flatten-rows'
    const snapshot = yield* Analysis.ofSourceRealized(module, ascii(flattenRowsSource))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const encoded = Analysis.expressionsOf(snapshot, module).flatMap((expression) =>
      expression._tag === 'Call' && expression.type._tag === 'Available'
        ? [Type.encode(expression.type.type)]
        : [],
    )
    assert.deepEqual(encoded, [
      `Effect<i32 ! ${module}.Inner ? &${module}.Clock | &${module}.Meter>`,
      `Effect<Effect<i32 ! ${module}.Inner ? &${module}.Clock | &${module}.Meter> ! ${module}.Outer ? &${module}.Clock>`,
      `Effect<i32 ! ${module}.Inner | ${module}.Outer ? &${module}.Clock | &${module}.Meter>`,
    ])
  }),
)

it.effect('resolves flatten through the ordinary declaration path without an intrinsic', () =>
  Effect.gen(function* () {
    const module = 'effect-runtime/flatten-declaration'
    const snapshot = yield* Analysis.ofSourceRealized(module, ascii(flattenSource))
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const occurrence = Analysis.semanticOccurrenceAt(
      snapshot,
      module,
      flattenSource.indexOf('Effect.flatten') + 'Effect.'.length,
    )
    assert.strictEqual(occurrence?.role, 'Value')
    assert.strictEqual(occurrence?.declaration?.module, 'silk/effect')
    assert.include(
      occurrence === undefined
        ? ''
        : (Analysis.occurrencePresentation(snapshot, module, occurrence)?.text ?? ''),
      'pub effect fn flatten',
    )

    const constructed = (Projections.hirOf(snapshot, module)?.functions ?? []).flatMap((fn) =>
      fn.statements.flatMap((statement) =>
        statement._tag === 'Bind' && statement.initializer._tag === 'EffectConstruct'
          ? [
              {
                module: statement.initializer.target.module,
                name: statement.initializer.target.name,
              },
            ]
          : [],
      ),
    )
    assert.deepEqual(constructed, [
      { module, name: 'outer' },
      { module: 'silk/effect', name: 'Effect.flatten' },
    ])

    const intrinsics = new Set(
      (snapshot.semanticOccurrences.modules.get(module)?.occurrences ?? []).flatMap((candidate) =>
        candidate.resolution._tag === 'Available' &&
        candidate.resolution.identity._tag === 'IntrinsicOperationIdentity'
          ? [`${candidate.resolution.identity.id.actor}.${candidate.resolution.identity.id.name}`]
          : [],
      ),
    )
    assert.deepEqual([...intrinsics], ['Intrinsic.i32Multiply'])

    const catalog = Intrinsic.all().flatMap((actor) =>
      actor.operations.map((operation) => operation.spelling),
    )
    assert.notInclude(catalog, 'effectResult')
    assert.notInclude(catalog, 'flatten')
  }),
)
