import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target?: string) =>
  Analysis.ofSourceRealized('user-services/main', encoder.encode(source), target)

const evaluate = (source: string) =>
  Effect.map(snapshot(source), (self) => ({ self, outcome: Analysis.evaluate(self) }))

const sharedSource = `service Counter {
  effect fn get() -> i32 ? &Counter
}
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
impl Counter for Fixed { get: Fixed.get }
effect fn read() -> i32 ? &Counter { return run Counter.get() }
pub fn main() -> i32 {
  let fixed = Fixed { value: 42 }
  return run Effect.provide(read(), &fixed)
}`

it.effect('dispatches a shared source service through its complete witness', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(sharedSource)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, undefined, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)

    const hir = Analysis.hirOf(self, 'user-services/main')
    assert.include(
      hir === undefined ? '' : Hir.encode(hir),
      'service-call user-services/main.Counter.get',
    )
  }),
)

it.effect('dispatches an exclusive source service and preserves provider mutation', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`service Counter {
  effect fn increment() -> i32 ? &mut Counter
}
struct Cell { value: i32 }
effect fn increment(self: &mut Cell) -> i32 {
  self.value = self.value + 1
  return self.value
}
impl Counter for Cell { increment: Cell.increment }
effect fn twice() -> i32 ? &mut Counter {
  let first = run Counter.increment()
  let second = run Counter.increment()
  return first + second
}
pub fn main() -> i32 {
  let mut cell = Cell { value: 20 }
  return run Effect.provideMut(twice(), &mut cell)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, undefined, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 43)
  }),
)

it.effect('selects service roles and provider replacements without dynamic lookup', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`service Values {
  effect fn left() -> i32 ? &Values@Left
  effect fn right() -> i32 ? &Values@Right
}
struct Fixed { value: i32 }
effect fn left(self: &Fixed) -> i32 { return self.value }
effect fn right(self: &Fixed) -> i32 { return self.value }
impl Values for Fixed { left: Fixed.left right: Fixed.right }
effect fn total() -> i32 ? &Values@Left | &Values@Right {
  let leftValue = run Values.left()
  let rightValue = run Values.right()
  return leftValue * 10 + rightValue
}
pub fn main() -> i32 {
  let left = Fixed { value: 4 }
  let right = Fixed { value: 2 }
  let selected = total()
    |> Intrinsic.bindRequirement(&left, @Left)
    |> Intrinsic.bindRequirement(&right, @Right)
  return run selected
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, undefined, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('uses a nested provider override only for its lexical provision', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`service Value {
  effect fn get() -> i32 ? &Value
}
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
impl Value for Fixed { get: Fixed.get }
effect fn read() -> i32 ? &Value { return run Value.get() }
effect fn nested(inner: &Fixed) -> i32 ? &Value {
  let innerValue = run Effect.provide(read(), inner)
  let outerValue = run read()
  return innerValue * 10 + outerValue
}
pub fn main() -> i32 {
  let inner = Fixed { value: 4 }
  let outer = Fixed { value: 2 }
  return run Effect.provide(nested(&inner), &outer)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, undefined, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42)
  }),
)

it.effect('retains an unprovided user service requirement instead of inventing a provider', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`service Value { effect fn get() -> i32 ? &Value }
effect fn read() -> i32 ? &Value { return run Value.get() }
pub fn main() -> i32 { return run read() }`)
    const outcome = Analysis.evaluate(self)
    assert.notStrictEqual(outcome._tag, 'Completed')
  }),
)

it.effect('keeps ordinary Report conformance static and out of requirement rows', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`pub struct Problem {}
impl Report for Problem {}
pub effect fn main() -> () ! Problem { return () }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const entry = Analysis.instancesOf(self).entry
    assert.strictEqual(entry._tag, 'Resolved')
    if (entry._tag === 'Resolved' && entry.kind === 'Effect')
      assert.deepEqual(entry.requirements, [])
  }),
)

it.effect('ends the provider loan after the provided effect completes', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`service Value {
  effect fn get() -> i32 ? &Value
}
struct Provider { value: i32 }
effect fn get(self: &Provider) -> i32 { return self.value }
impl Value for Provider { get: Provider.get }
effect fn read() -> i32 ? &Value { return run Value.get() }
pub fn main() -> i32 {
  let mut provider = Provider { value: 42 }
  let observed = run Effect.provide(read(), &provider)
  provider.value = 1
  return observed + provider.value
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, undefined, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 43)
  }),
)

it.effect('lowers shared source service dispatch through LLVM and direct Wasm', () =>
  Effect.gen(function* () {
    const native = yield* snapshot(sharedSource, 'aarch64-apple-darwin')
    assert.deepEqual(Analysis.diagnostics(native), [])
    const llvm = yield* Analysis.codegen(native, { mode: 'release' })
    assert.include(llvm.ir, 'define')
    assert.notInclude(llvm.ir, 'Counter')

    const wasm = yield* snapshot(sharedSource, 'wasm32-unknown-unknown')
    const artifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    assert.deepEqual(artifact.hostImports, [])
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    const main = instance.exports.silk_main
    if (typeof main !== 'function') throw new Error('source-service Wasm lost silk_main')
    assert.strictEqual(main(), 42)
  }),
)
