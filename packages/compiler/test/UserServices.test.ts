import { assert, it } from '@effect/vitest'
import * as WasmError from '@silk-effect/wasm/WasmError'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as Type from '../src/Type.js'
import * as Json from './support/Json.js'
import {
  mixedServiceProviderSuspension,
  ownedProviderSuspendedFailure,
  ownedProviderSuspendedSuccess,
} from './support/ownedAllocatorSuspension.js'
import * as WasmMain from './support/WasmMain.js'

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
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)

    const hir = Analysis.hirOf(self, 'user-services/main')
    assert.include(
      hir === undefined ? '' : Hir.encode(hir),
      'service-call user-services/main.Counter.get',
    )
  }),
)

it.effect(
  'specializes a conditional generic service witness and its unused proof dependency',
  () =>
    Effect.gen(function* () {
      const source = `interface Marker { fn mark(value: &Self) -> i32 }

struct Token {}
fn markToken(value: &Token) -> i32 { return 1 }
impl Marker for Token { mark: Token.markToken }

interface Decoder { fn decode(value: &Self) -> i32 }
struct Schema { tag: i32 }
fn schemaDecode(value: &Schema) -> i32 { return value.tag }
impl Decoder for Schema { decode: Schema.schemaDecode }

struct Mapped<S> { source: S }
fn mappedDecode<S: Decoder>(value: &Mapped<S>) -> i32 {
  return Decoder.decode(&value.source) + 1
}
impl<S: Decoder> Decoder for Mapped<S> { decode: Mapped.mappedDecode }

struct Optional<S> { source: S }
fn optionalDecode<S: Decoder>(value: &Optional<S>) -> i32 {
  return Decoder.decode(&value.source) + 1
}
impl<S: Decoder> Decoder for Optional<S> { decode: Optional.optionalDecode }

fn decodeOf<T: Decoder>(value: T) -> i32 { return Decoder.decode(&value) }

service Counter<Prefix, Value> {
  effect fn get(value: &Value) -> i32 ? &Counter<Prefix, Value>
}

struct Fixed<S> {}
effect fn get<S: Marker>(self: &Fixed<S>, value: &S) -> i32 {
  return decodeOf(Optional<Mapped<Schema>> {
    source: Mapped<Schema> { source: Schema { tag: 40 } }
  })
}
impl<S: Marker> Counter<i32, S> for Fixed<S> { get: Fixed.get }

effect fn read(value: &Token) -> i32 ? &Counter<i32, Token> {
  return run Counter.get<i32, Token>(value)
}

pub fn main() -> i32 {
  let provider = Fixed<Token> {}
  let token = Token {}
  return run Effect.provide(read(&token), &provider)
}`
      const self = yield* snapshot(source)
      assert.deepEqual(
        Analysis.diagnostics(self).map((diagnostic) => `${diagnostic.code}: ${diagnostic.message}`),
        [],
      )
      const outcome = Analysis.evaluate(self)
      assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)

      const instances = Analysis.instancesOf(self).instances
      const get = instances.filter((instance) => instance.key.declaration.name === 'get')
      assert.strictEqual(get.length, 1)
      assert.deepEqual(get.at(0)?.key.typeArguments.map(Type.encodeGenericArgument), [
        'user-services/main.Token',
      ])
      assert.strictEqual(
        instances.filter((instance) => instance.key.declaration.name === 'markToken').length,
        1,
      )
      const mir = Analysis.loweredMir(self)
      const loweredTargets = mir.functions.filter(
        (fn) => fn.id.name === 'get' || fn.id.name === 'get$effect$-1',
      )
      assert.strictEqual(loweredTargets.length, 2, Mir.encode(mir))
      for (const target of loweredTargets)
        assert.deepEqual(target.instance.typeArguments.map(Type.encodeGenericArgument), [
          'user-services/main.Token',
        ])
      for (const name of ['optionalDecode', 'mappedDecode', 'schemaDecode'])
        assert.strictEqual(
          mir.functions.filter((fn) => fn.id.name === name).length,
          1,
          `${name} did not reach MIR exactly once`,
        )
      const encoded = Mir.encode(mir)
      for (const spelling of ['dictionary', 'vtable', 'witnessTable', 'interfaceTag', 'typeTag'])
        assert.isFalse(encoded.includes(spelling), `${spelling} reached MIR`)

      const wasmSnapshot = yield* snapshot(source, 'wasm32-unknown-unknown')
      const wasm = yield* Analysis.codegenWasm(wasmSnapshot, { mode: 'release' })
      const wasmInstance = yield* Effect.try({
        try: () => new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {}),
        catch: (cause) =>
          WasmError.wrappedFailure({
            operation: 'UserServices.instantiateWasm',
            message: 'The host could not instantiate the generated WebAssembly module',
            cause,
          }),
      })
      const main = wasmInstance.exports.silk_main
      assert.strictEqual(typeof main, 'function')
      if (typeof main === 'function') assert.strictEqual(main(), 42)
    }),
  180_000,
)

it.effect('rejects a generic service witness bound its header never promises', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`interface Marker<T> { fn mark(value: T) -> i32 }
interface Other<T> { fn mark(value: T) -> i32 }
service Counter<Value> { effect fn get(value: &Value) -> i32 ? &Counter<Value> }
struct Fixed<S> {}
effect fn get<S: Other>(self: &Fixed<S>, value: &S) -> i32 { return 42 }
impl<S: Marker<S>> Counter<S> for Fixed<S> { get: Fixed.get }
pub fn main() -> i32 { return 0 }`)
    const invalid = Analysis.diagnostics(self).filter((diagnostic) => diagnostic.code === 'SEM0083')
    assert.strictEqual(invalid.length, 1)
    assert.include(invalid.at(0)?.message ?? '', 'does not require')
    assert.include(invalid.at(0)?.message ?? '', 'Other')
  }),
)

it.effect('accepts failure and requirement rows promised by a generic service header', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`interface Marker<T> { fn mark(value: T) -> i32 }

service Counter<E, ?R, Value> {
  effect fn get(value: &Value) -> i32 ! E ? R | &Counter<E, R, Value>
}

struct Fixed<S, E, ?R> {}
effect fn get<S: Marker, E, ?R>(self: &Fixed<S, E, R>, value: &S) -> i32 ! E ? R {
  return 42
}
impl<S: Marker<S>, E, ?R> Counter<E, R, S> for Fixed<S, E, R> { get: Fixed.get }

pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => `${diagnostic.code}: ${diagnostic.message}`),
      [],
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
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 43n)
  }),
)

for (const provider of [
  Object.freeze({
    label: 'direct shared intrinsic',
    call: 'Intrinsic.bindRequirement',
    access: 'Shared',
    expected: 262,
  }),
  Object.freeze({
    label: 'ordinary shared wrapper',
    call: 'Effect.provide',
    access: 'Shared',
    expected: 262,
  }),
  Object.freeze({
    label: 'direct exclusive intrinsic',
    call: 'Intrinsic.bindRequirementMut',
    access: 'Exclusive',
    expected: 308,
  }),
  Object.freeze({
    label: 'ordinary exclusive wrapper',
    call: 'Effect.provideMut',
    access: 'Exclusive',
    expected: 308,
  }),
])
  it.effect(
    `transfers ${provider.label} Effect recipes and protected borrows through move aliases`,
    () =>
      Effect.gen(function* () {
        const source = `import silk.result { Result, Success, Failure }
struct Token { value: i32 }
service Counter {
  effect fn increment(token: ${provider.access === 'Exclusive' ? '&mut Token' : '&Token'}) -> i32 ? ${provider.access === 'Exclusive' ? '&mut Counter' : '&Counter'}
}
struct Cell { value: i32 }
effect fn increment(self: ${provider.access === 'Exclusive' ? '&mut Cell' : '&Cell'}, token: ${provider.access === 'Exclusive' ? '&mut Token' : '&Token'}) -> i32 {
  ${provider.access === 'Exclusive' ? 'self.value = self.value + 1' : ''}
  return self.value + token.value
}
impl Counter for Cell { increment: Cell.increment }
effect fn read(token: ${provider.access === 'Exclusive' ? '&mut Token' : '&Token'}) -> i32 ? ${provider.access === 'Exclusive' ? '&mut Counter' : '&Counter'} {
  return run Counter.increment(${provider.access === 'Exclusive' ? 'move ' : ''}token)
}
fn branchRead(flag: bool, seed: i32) -> i32 {
  let ${provider.access === 'Exclusive' ? 'mut ' : ''}token = Token { value: 0 }
  let mut cell = Cell { value: seed }
  let bound = ${provider.call}(read(${provider.access === 'Exclusive' ? '&mut token' : '&token'}), ${provider.access === 'Exclusive' ? '&mut cell' : '&cell'})
  if flag {
    let alias = move bound
    return run move alias
  }
  return run move bound
}
fn swappedBranchRead(flag: bool, seed: i32) -> i32 {
  let ${provider.access === 'Exclusive' ? 'mut ' : ''}token = Token { value: 0 }
  let mut cell = Cell { value: seed }
  let bound = ${provider.call}(read(${provider.access === 'Exclusive' ? '&mut token' : '&token'}), ${provider.access === 'Exclusive' ? '&mut cell' : '&cell'})
  if flag {
    return run move bound
  }
  let firstAlias = move bound
  let secondAlias = move firstAlias
  return run move secondAlias
}
pub fn main() -> i32 {
  let ${provider.access === 'Exclusive' ? 'mut ' : ''}firstToken = Token { value: 0 }
  let mut firstCell = Cell { value: ${provider.access === 'Exclusive' ? '10' : '11'} }
  let firstBound = ${provider.call}(read(${provider.access === 'Exclusive' ? '&mut firstToken' : '&firstToken'}), ${provider.access === 'Exclusive' ? '&mut firstCell' : '&firstCell'})
  let firstAlias = move firstBound
  let first = run move firstAlias

  let ${provider.access === 'Exclusive' ? 'mut ' : ''}secondToken = Token { value: 0 }
  let mut secondCell = Cell { value: ${provider.access === 'Exclusive' ? '30' : '31'} }
  let secondBound = ${provider.call}(read(${provider.access === 'Exclusive' ? '&mut secondToken' : '&secondToken'}), ${provider.access === 'Exclusive' ? '&mut secondCell' : '&secondCell'})
  let secondHop = move secondBound
  let secondAlias = move secondHop
  let reified = Effect.result(move secondAlias)
  let reifiedHop = move reified
  let reifiedAlias = move reifiedHop
  let completed = run move reifiedAlias
  let second = match move completed {
    Result<i32, never> { value: outcome } => match move outcome {
      Success<i32> { value: answer } => answer
      Failure<never> { error: impossible } => 0
    }
  }
  let branches = branchRead(true, 40)
    + branchRead(false, 50)
    + swappedBranchRead(true, 60)
    + swappedBranchRead(false, 70)
  return ${provider.access === 'Exclusive' ? 'first + second + firstCell.value + secondCell.value' : 'first + second'} + branches
}`
        const self = yield* snapshot(source, 'wasm32-unknown-unknown')
        const hir = Analysis.hirOf(self, 'user-services/main')
        assert.deepEqual(Analysis.diagnostics(self), [], hir === undefined ? '' : Hir.encode(hir))
        assert.deepEqual(Mir.verify(Analysis.loweredMir(self)), [])

        const outcome = Analysis.evaluate(self)
        assert.strictEqual(
          outcome._tag,
          'Completed',
          JSON.stringify(outcome, Json.bigIntReplacer, 2),
        )
        if (outcome._tag === 'Completed')
          assert.strictEqual(outcome.result.value, BigInt(provider.expected))

        const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
        assert.strictEqual(
          yield* WasmMain.invoke(wasm.bytes, 'UserServices.invokeAliasedProviderWasm'),
          provider.expected,
        )
      }),
    180_000,
  )

it.effect('dispatches an owned source service provider exactly once', () =>
  Effect.gen(function* () {
    const source = `service Counter {
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
  let cell = Cell { value: 20 }
  return run Effect.bindRequirementOwned<&mut Counter>(twice(), move cell)
}`
    const self = yield* snapshot(source, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 43n)

    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    assert.strictEqual(
      yield* WasmMain.invoke(wasm.bytes, 'UserServices.invokeOwnedProviderWasm'),
      43,
    )
  }),
)

it.effect('retains an affine owned provider while a pre-read scalar suspends and succeeds', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(ownedProviderSuspendedSuccess, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    const providedRead = mir.functions.find((fn) =>
      fn.id.name.startsWith('read$effect$-1$provided$'),
    )
    assert.isDefined(providedRead)
    const provisional = Analysis.provisionalMirOf(self)
    assert.strictEqual(provisional._tag, 'Available')
    const providedExecution =
      provisional._tag === 'Available' && providedRead !== undefined
        ? ProvisionalMir.executionOf(provisional.value, providedRead.instance)
        : undefined
    assert.isDefined(providedExecution)
    assert.isAbove(providedExecution?.regions.length ?? 0, 0)
    assert.isAbove(providedRead?.suspension?.regions.length ?? 0, 0)
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Blocked' ? JSON.stringify(outcome.reason) : outcome._tag,
    )
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 42n)
      const acquired = outcome.trace.filter((event) => event._tag === 'AllocationAcquire').length
      const released = outcome.trace.filter((event) => event._tag === 'AllocationRelease').length
      assert.isAbove(acquired, 0)
      assert.strictEqual(released, acquired)
    }
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    assert.strictEqual(
      yield* WasmMain.invoke(wasm.bytes, 'UserServices.invokeSuspendedOwnedProviderSuccessWasm'),
      42,
    )
  }),
)

it.effect('releases an affine owned provider after a pre-read scalar suspends and fails', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(ownedProviderSuspendedFailure, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Blocked' ? JSON.stringify(outcome.reason) : outcome._tag,
    )
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 7n)
      const release = outcome.trace.findIndex((event) => event._tag === 'AllocationRelease')
      const recovery = outcome.trace.findIndex(
        (event) => event._tag === 'Call' && event.target.name === 'recover',
      )
      assert.isAtLeast(release, 0)
      assert.isAbove(recovery, release)
    }
    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    assert.strictEqual(
      yield* WasmMain.invoke(wasm.bytes, 'UserServices.invokeSuspendedOwnedProviderFailureWasm'),
      7,
    )
  }),
)

it.effect('keeps a synchronous service with an allocator requirement synchronous', () =>
  Effect.gen(function* () {
    const source = `service Value {
  effect fn read() -> i32 ? &mut Value | &mut Allocator
}
struct Fixed { value: i32 }
effect fn read(self: &mut Fixed) -> i32 ? &mut Allocator { return self.value }
impl Value for Fixed { read: Fixed.read }
effect fn program() -> i32 ? &mut Value | &mut Allocator {
  return run Value.read()
}
pub fn main() -> i32 {
  let mut fixed = Fixed { value: 42 }
  let mut allocator = SystemAllocator.make()
  return run Effect.provideMut<&mut Allocator>(
    Effect.provideMut<&mut Value>(program(), &mut fixed),
    &mut allocator,
  )
}`
    const self = yield* snapshot(source, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(Mir.verify(mir), [])
    const main = mir.functions.find(
      (fn) => fn.id.module === 'user-services/main' && fn.id.name === 'main',
    )
    assert.strictEqual(main?.suspension?.classification ?? 'Synchronous', 'Synchronous')
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('keeps mixed provider specializations exact at one service site', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(mixedServiceProviderSuspension, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    const specialized = mir.functions.filter((fn) =>
      fn.id.name.startsWith('use$effect$-1$provided$'),
    )
    const provisional = Analysis.provisionalMirOf(self)
    assert.isAtLeast(specialized.length, 2)
    const synchronous = specialized.filter(
      (fn) => fn.suspension?.classification === 'Synchronous' || fn.suspension === undefined,
    )
    const suspendable = specialized.filter((fn) => fn.suspension?.classification === 'Suspendable')
    assert.isAtLeast(synchronous.length, 1)
    assert.isAtLeast(suspendable.length, 1)
    assert.isTrue(synchronous.every((fn) => (fn.suspension?.regions.length ?? 0) === 0))
    assert.isTrue(suspendable.some((fn) => (fn.suspension?.regions.length ?? 0) > 0))
    assert.strictEqual(provisional._tag, 'Available')

    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', outcome._tag)
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(self, { mode: 'release' })
    assert.strictEqual(
      yield* WasmMain.invoke(wasm.bytes, 'UserServices.invokeMixedProviderSuspensionWasm'),
      42,
    )
  }),
)

it.effect('releases an owned source provider after the protected Effect completes', () =>
  Effect.gen(function* () {
    const source = `import silk.effects as Effect
struct Problem { code: i32 }
service Value { effect fn read() -> i32 ? &mut Value }
struct Provider { storage: Allocation }
effect fn read(self: &mut Provider) -> i32 { return 42 }
impl Value for Provider { read: Provider.read }
effect fn open() -> Provider ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let allocation = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  return Provider { storage: move allocation }
}
effect fn exhausted(error: OutOfMemoryError) -> Provider ! Problem { fail Problem { code: 9 } }
effect fn acquire() -> Provider ! Problem { return run Effect.catchAll(open(), exhausted) }
effect fn use() -> i32 ? &mut Value { return run Value.read() }
effect fn body() -> i32 ! Problem {
  let provider = run acquire()
  return run Intrinsic.bindRequirementOwned<&mut Value>(use(), move provider)
}
effect fn recover(error: Problem) -> i32 { return -1 }
pub fn main() -> i32 { return run Effect.catchAll(body(), recover) }`
    const self = yield* snapshot(source)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed')
    assert.deepEqual(
      outcome.trace.flatMap((event) =>
        event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease'
          ? [event._tag]
          : [],
      ),
      ['AllocationAcquire', 'AllocationRelease'],
    )
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
    |> Intrinsic.bindRequirement<&Values@Left>(&left)
    |> Intrinsic.bindRequirement<&Values@Right>(&right)
  return run selected
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
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
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
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

it.effect('rejects an ordinary interface as an Effect dependency', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`interface Clock { fn now(value: &Self) -> i32 }
effect fn read() -> i32 ? &Clock { return 42 }
pub fn main() -> i32 { return 0 }`)
    assert.isTrue(
      Analysis.diagnostics(self).some(
        (diagnostic) => diagnostic.code === 'SEM0070' && diagnostic.message.includes('Clock'),
      ),
    )
  }),
)

it.effect('allows a service to participate in an ordinary compile-time bound', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`service Clock { effect fn now() -> i32 ? &Clock }
struct Fixed {}
effect fn now(self: &Fixed) -> i32 { return 42 }
impl Clock for Fixed { now: Fixed.now }
fn preserve<T: Clock>(value: T) -> T { return move value }
pub fn main() -> i32 { return 0 }`)
    assert.deepEqual(Analysis.diagnostics(self), [])
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
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 43n)
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
