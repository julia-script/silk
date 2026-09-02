import { assert, it } from '@effect/vitest'
import * as WasmError from '@silklang/wasm/WasmError'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as MirVerification from '../src/MirVerification.js'
import * as ProvisionalMir from '../src/ProvisionalMir.js'
import * as Type from '../src/Type.js'
import * as Json from './support/Json.js'
import {
  mixedServiceProviderSuspension,
  ownedProviderSuspendedFailure,
  ownedProviderSuspendedSuccess,
} from './support/ownedAllocatorSuspension.js'
import * as Projections from './support/projections.js'
import * as WasmMain from './support/WasmMain.js'

const encoder = new TextEncoder()

const snapshot = (source: string, target?: string) =>
  Analysis.ofSourceRealized('user-services/main', encoder.encode(source), target)

const evaluate = (source: string) =>
  Effect.map(snapshot(source), (self) => ({ self, outcome: Analysis.evaluate(self) }))

const sharedSource = `import silk.effect { Effect }
service Counter {
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
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)

    const hir = Projections.hirOf(self, 'user-services/main')
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
      const source = `import silk.effect { Effect }
interface Marker { fn mark(value: &Self) -> i32 }

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
      assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
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
      assert.strictEqual(loweredTargets.length, 2, MirEncoding.encode(mir))
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
      const encoded = MirEncoding.encode(mir)
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
    const { self, outcome } = yield* evaluate(`import silk.effect { Effect }
service Counter {
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
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 43n)
  }),
)

const randomServiceSource = `import silk.effect { Effect }
import silk.option { Option }
import silk.insecure_random { InsecureRandom }
import silk.u64 as u64
import silk.u8 as u8
import silk.usize as usize

struct Scripted {
  first: u64
  second: u64
  third: u64
  index: usize
}

fn scripted(first: u64, second: u64, third: u64) -> Scripted {
  return Scripted { first: first, second: second, third: third, index: usize.ZERO }
}

effect fn scriptedNext(self: &mut Scripted) -> u64 {
  let mut value = self.third
  if self.index == usize.ZERO { value = self.first }
  if self.index == usize.ONE { value = self.second }
  self.index = self.index + usize.ONE
  return value
}

impl InsecureRandom for Scripted { nextU64: Scripted.scriptedNext }

// The operation is provided at each call site from a local: a service effect provided from a
// \`&mut\` parameter inside a section is blocked by the MIR verifier as an invalid call shape, a
// pre-existing lowering hole that the deleted root wrapper used to mask (follow-up task "Fix silent
// trap when a service effect is provided from a &mut parameter"; the characterization test below
// pins it).
effect fn next() -> u64 ? &mut InsecureRandom {
  return run InsecureRandom.nextU64()
}

fn matches(seed: u64, expected: &[u64]) -> bool {
  let mut provider = InsecureRandom.seeded(seed)
  let mut index = usize.ZERO
  while index < expected.length {
    let actual = run InsecureRandom.nextU64()
      |> Effect.provideMut<InsecureRandom>(&mut provider)
    if actual != expected[index] { return false }
    index = index + usize.ONE
  }
  return true
}

fn knownAnswers() -> bool {
  let zero = [
    u64.toU64(0x99ec5f36cb75f2b4),
    u64.toU64(0xbf6e1f784956452a),
    u64.toU64(0x1a5f849d4933e6e0),
    u64.toU64(0x6aa594f1262d2d2c)
  ]
  let fortyTwo = [
    u64.toU64(0x15780b2e0c2ec716),
    u64.toU64(0x6104d9866d113a7e),
    u64.toU64(0xae17533239e499a1),
    u64.toU64(0xecb8ad4703b360a1),
    u64.toU64(0xfde6dc7fe2ec5e64),
    u64.toU64(0xc50da53101795238),
    u64.toU64(0xb82154855a65ddb2),
    u64.toU64(0xd99a2743ebe60087)
  ]
  return matches(0, &zero) && matches(42, &fortyTwo)
}

fn reproducible() -> bool {
  let mut left = InsecureRandom.seeded(42)
  let mut right = InsecureRandom.seeded(42)
  let mut index = usize.ZERO
  while index < 16 {
    let leftWord = run InsecureRandom.nextU64()
      |> Effect.provideMut<InsecureRandom>(&mut left)
    let rightWord = run InsecureRandom.nextU64()
      |> Effect.provideMut<InsecureRandom>(&mut right)
    if leftWord != rightWord { return false }
    index = index + usize.ONE
  }
  return true
}

fn emptyBytes() -> [u8; 0] { return [] }

fn zeroBytes8() -> [u8; 8] {
  return [
    u8.toU8(0),
    u8.toU8(0),
    u8.toU8(0),
    u8.toU8(0),
    u8.toU8(0),
    u8.toU8(0),
    u8.toU8(0),
    u8.toU8(0)
  ]
}

fn zeroBytes3() -> [u8; 3] {
  return [u8.toU8(0), u8.toU8(0), u8.toU8(0)]
}

fn derivedOperations() -> bool {
  let mut direct = scripted(20, 22, 99)
  let first = run next() |> Effect.provideMut<InsecureRandom>(&mut direct)
  let second = run next() |> Effect.provideMut<InsecureRandom>(&mut direct)
  if first + second != 42 { return false }
  if direct.index != 2 { return false }

  let mut booleans = scripted(0, 0x8000000000000000, 99)
  let low = run InsecureRandom.nextBool()
    |> Effect.provideMut<InsecureRandom>(&mut booleans)
  let high = run InsecureRandom.nextBool()
    |> Effect.provideMut<InsecureRandom>(&mut booleans)
  if low || !high || booleans.index != 2 { return false }

  let mut zeroBound = scripted(41, 99, 99)
  let absent = run InsecureRandom.below(0)
    |> Effect.provideMut<InsecureRandom>(&mut zeroBound)
  if Option.unwrapOr<u64>(move absent, 42) != 42 { return false }
  let bound = run next() |> Effect.provideMut<InsecureRandom>(&mut zeroBound)
  if bound != 41 || zeroBound.index != 1 { return false }

  let mut rejected = scripted(5, 17, 99)
  let bounded = run InsecureRandom.below(10)
    |> Effect.provideMut<InsecureRandom>(&mut rejected)
  if Option.unwrapOr<u64>(move bounded, 99) != 7 { return false }
  if rejected.index != 2 { return false }

  let mut bytesProvider = scripted(0x0807060504030201, 0x11100f0e0d0c0b0a, 99)
  let mut empty = emptyBytes()
  run InsecureRandom.fillBytes(&mut empty)
    |> Effect.provideMut<InsecureRandom>(&mut bytesProvider)
  if bytesProvider.index != usize.ZERO { return false }

  let mut full = zeroBytes8()
  run InsecureRandom.fillBytes(&mut full)
    |> Effect.provideMut<InsecureRandom>(&mut bytesProvider)
  if bytesProvider.index != usize.ONE { return false }
  let expectedFull = [
    u8.toU8(1),
    u8.toU8(2),
    u8.toU8(3),
    u8.toU8(4),
    u8.toU8(5),
    u8.toU8(6),
    u8.toU8(7),
    u8.toU8(8)
  ]
  let mut index = usize.ZERO
  while index < 8 {
    if full[index] != expectedFull[index] { return false }
    index = index + usize.ONE
  }

  let mut partial = zeroBytes3()
  run InsecureRandom.fillBytes(&mut partial)
    |> Effect.provideMut<InsecureRandom>(&mut bytesProvider)
  if bytesProvider.index != 2 { return false }
  if partial[0] != u8.toU8(10) || partial[1] != u8.toU8(11) || partial[2] != u8.toU8(12) {
    return false
  }
  return true
}

pub fn main() -> i32 {
  if !knownAnswers() { return 1 }
  if !reproducible() { return 2 }
  if !derivedOperations() { return 3 }
  return 42
}`

it.effect(
  'runs seeded and scripted InsecureRandom providers through ordinary exclusive service dispatch',
  () =>
    Effect.gen(function* () {
      const { self, outcome } = yield* evaluate(randomServiceSource)
      assert.deepEqual(Analysis.diagnostics(self), [])
      assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)

      const randomHir = Projections.hirOf(self, 'silk/insecure_random')
      assert.include(
        randomHir === undefined ? '' : Hir.encode(randomHir),
        'service-call silk/insecure_random.InsecureRandom.nextU64',
      )
      assert.notInclude(randomServiceSource, 'Xoshiro256StarStar')
    }),
)

const randomCapabilitiesSource = `import silk.effect { Effect }
import silk.insecure_seed { InsecureSeed }
import silk.option { Option }
import silk.random { Random }
import silk.u64 as u64
import silk.u8 as u8
import silk.usize as usize

struct ScriptedRandom {
  first: u64
  second: u64
  third: u64
  fourth: u64
  fifth: u64
  sixth: u64
  index: usize
}

fn scriptedRandom() -> ScriptedRandom {
  return ScriptedRandom {
    first: u64.toU64(0x0807060504030201),
    second: u64.toU64(0x8000000000000000),
    third: u64.toU64(5),
    fourth: u64.toU64(17),
    fifth: u64.toU64(20),
    sixth: u64.toU64(22),
    index: usize.ZERO,
  }
}

effect fn scriptedFill(self: &mut ScriptedRandom, output: &mut [u8]) -> () {
  if output.length == usize.ZERO { return () }
  let mut word = self.sixth
  if self.index == usize.ZERO { word = self.first }
  if self.index == usize.ONE { word = self.second }
  if self.index == 2 { word = self.third }
  if self.index == 3 { word = self.fourth }
  if self.index == 4 { word = self.fifth }
  self.index = self.index + usize.ONE
  let mut cursor = usize.ZERO
  while cursor < output.length {
    output[cursor] = u64.toU8(u64.bitAnd(word, 255))
    word = u64.shiftRight(word, 8)
    cursor = cursor + usize.ONE
  }
  return ()
}

impl Random for ScriptedRandom {
  fillBytes: ScriptedRandom.scriptedFill
}

fn emptyBytes() -> [u8; 0] { return [] }

fn values() -> bool {
  let mut provider = scriptedRandom()
  let mut firstEmpty = emptyBytes()
  run Random.fillBytes(&mut firstEmpty)
    |> Effect.provideMut<Random>(&mut provider)
  let mut secondEmpty = emptyBytes()
  run Random.fillBytes(&mut secondEmpty)
    |> Effect.provideMut<Random>(&mut provider)
  if provider.index != usize.ZERO { return false }

  let word = run Random.nextU64()
    |> Effect.provideMut<Random>(&mut provider)
  if word != u64.toU64(0x0807060504030201) { return false }
  let flag = run Random.nextBool()
    |> Effect.provideMut<Random>(&mut provider)
  if !flag { return false }
  let absent = run Random.below(0)
    |> Effect.provideMut<Random>(&mut provider)
  if Option.unwrapOr<u64>(move absent, 42) != 42 { return false }
  if provider.index != 2 { return false }
  let bounded = run Random.below(10)
    |> Effect.provideMut<Random>(&mut provider)
  if Option.unwrapOr<u64>(move bounded, 99) != 7 { return false }
  if provider.index != 4 { return false }

  let seedProvider = run InsecureSeed.fromRandom()
    |> Effect.provideMut<Random>(&mut provider)
  if provider.index != 6 { return false }
  let firstSeed = run InsecureSeed.get()
    |> Effect.provide<InsecureSeed>(&seedProvider)
  let secondSeed = run InsecureSeed.get()
    |> Effect.provide<InsecureSeed>(&seedProvider)
  if InsecureSeed.first(&firstSeed) != 20 { return false }
  if InsecureSeed.second(&firstSeed) != 22 { return false }
  if InsecureSeed.first(&secondSeed) != 20 { return false }
  if InsecureSeed.second(&secondSeed) != 22 { return false }
  if provider.index != 6 { return false }

  let fixed = InsecureSeed.fixed(40, 2)
  let fixedSeed = run InsecureSeed.get()
    |> Effect.provide<InsecureSeed>(&fixed)
  return InsecureSeed.first(&fixedSeed) + InsecureSeed.second(&fixedSeed) == 42
}

pub fn main() -> i32 {
  if !values() { return 1 }
  return 42
}`

it.effect('derives secure random values and initializes one stable shared insecure seed', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(randomCapabilitiesSource)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    const randomHir = Projections.hirOf(self, 'silk/random')
    assert.include(
      randomHir === undefined ? '' : Hir.encode(randomHir),
      'service-call silk/random.Random.fillBytes',
    )
  }),
)

it.effect('keeps InsecureSeed fields private', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'insecure-seed/private',
      encoder.encode(`import silk.insecure_seed { InsecureSeed }
pub fn main() -> i32 {
  let provider = InsecureSeed.fixed(1, 2)
  return provider.seed.first
}`),
    )
    assert.deepEqual(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      ['SEM0028'],
    )
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
        const source = `import silk.effect { Effect }
import silk.result { Result }
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
  let secondReified = Effect.result(read(${provider.access === 'Exclusive' ? '&mut secondToken' : '&secondToken'}))
  let secondBound = ${provider.call}(move secondReified, ${provider.access === 'Exclusive' ? '&mut secondCell' : '&secondCell'})
  let secondHop = move secondBound
  let secondAlias = move secondHop
  let completed = run move secondAlias
  let second = match move completed {
      Result<i32, never>.Success { value: answer } => answer
      Result<i32, never>.Failure { error: impossible } => 0
  }
  let branches = branchRead(true, 40)
    + branchRead(false, 50)
    + swappedBranchRead(true, 60)
    + swappedBranchRead(false, 70)
  return ${provider.access === 'Exclusive' ? 'first + second + firstCell.value + secondCell.value' : 'first + second'} + branches
}`
        const self = yield* snapshot(source, 'wasm32-unknown-unknown')
        const hir = Projections.hirOf(self, 'user-services/main')
        assert.deepEqual(Analysis.diagnostics(self), [], hir === undefined ? '' : Hir.encode(hir))
        assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])

        const outcome = Analysis.evaluate(self)
        assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
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
    const source = `import silk.effect { Effect }
service Counter {
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
  return run Effect.bindRequirementOwned<Counter>(twice(), move cell)
}`
    const self = yield* snapshot(source, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
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
    const provisional = Projections.provisionalMirOf(self)
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
      outcome._tag === 'Blocked' ? Json.stringify(outcome.reason) : outcome._tag,
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
    const catchRunner = Analysis.loweredMir(self).functions.find((fn) =>
      fn.id.name.startsWith('Effect.catchAll$effect$'),
    )
    const caught = catchRunner?.suspension?.regions.find(
      (region) =>
        region._tag === 'RunSuspendableEffectRegion' && region.completion._tag === 'Reify',
    )
    assert.isDefined(caught)
    assert.strictEqual(caught?.operation._tag, 'CatchEffect')
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(
      outcome._tag,
      'Completed',
      outcome._tag === 'Blocked' ? Json.stringify(outcome.reason) : outcome._tag,
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
    const source = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
service Value {
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
  let mut allocator = Allocator.systemAllocatorProvider()
  return run Effect.provideMut<Allocator>(
    Effect.provideMut<Value>(program(), &mut fixed),
    &mut allocator,
  )
}`
    const self = yield* snapshot(source, 'wasm32-unknown-unknown')
    assert.deepEqual(Analysis.diagnostics(self), [])
    const mir = Analysis.loweredMir(self)
    assert.deepEqual(MirVerification.verify(mir), [])
    const main = mir.functions.find(
      (fn) => fn.id.module === 'user-services/main' && fn.id.name === 'main',
    )
    assert.strictEqual(main?.suspension?.classification ?? 'Synchronous', 'Synchronous')
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome))
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
    const provisional = Projections.provisionalMirOf(self)
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
    const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.layout { Layout }
import silk.effect { Effect }
struct Problem { code: i32 }
service Value { effect fn read() -> i32 ? &mut Value }
struct Provider { storage: Allocation }
effect fn read(self: &mut Provider) -> i32 { return 42 }
impl Value for Provider { read: Provider.read }
effect fn open() -> Provider ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 2]>()
  let allocation = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  return Provider { storage: move allocation }
}
effect fn exhausted(error: OutOfMemoryError) -> Provider ! Problem { fail Problem { code: 9 } }
effect fn acquire() -> Provider ! Problem { return run Effect.catchAll(open(), exhausted) }
effect fn use() -> i32 ? &mut Value { return run Value.read() }
effect fn body() -> i32 ! Problem {
  let provider = run acquire()
  return run Intrinsic.bindRequirementOwned<Value>(use(), move provider)
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
    const { self, outcome } = yield* evaluate(`role Left
role Right
service Values {
  effect fn left() -> i32 ? &Values at Left
  effect fn right() -> i32 ? &Values at Right
}
struct Fixed { value: i32 }
effect fn left(self: &Fixed) -> i32 { return self.value }
effect fn right(self: &Fixed) -> i32 { return self.value }
impl Values for Fixed { left: Fixed.left right: Fixed.right }
effect fn total() -> i32 ? &Values at Left | &Values at Right {
  let leftValue = run Values.left()
  let rightValue = run Values.right()
  return leftValue * 10 + rightValue
}
pub fn main() -> i32 {
  let left = Fixed { value: 4 }
  let right = Fixed { value: 2 }
  let selected = total()
    |> Intrinsic.bindRequirement<Values at Left>(&left)
    |> Intrinsic.bindRequirement<Values at Right>(&right)
  return run selected
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('uses a nested provider override only for its lexical provision', () =>
  Effect.gen(function* () {
    const { self, outcome } = yield* evaluate(`import silk.effect { Effect }
service Value {
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
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('selects each lexical provider of the same service capability', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`import silk.effect { Effect }
service Value {
  effect fn get() -> i32 ? &Value
}
struct Tens { value: i32 }
effect fn tensGet(self: &Tens) -> i32 { return self.value }
impl Value for Tens { get: Tens.tensGet }
struct Ones { value: i32 }
effect fn onesGet(self: &Ones) -> i32 { return self.value }
impl Value for Ones { get: Ones.onesGet }
pub fn main() -> i32 {
  let tens = Tens { value: 4 }
  let ones = Ones { value: 2 }
  let left = run Effect.provide(Value.get(), &tens)
  let right = run Effect.provide(Value.get(), &ones)
  return left * 10 + right
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(self)), [])
    const outcome = Analysis.evaluate(self)
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
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
    const { self, outcome } = yield* evaluate(`import silk.effect { Effect }
service Value {
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
    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
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

const parameterProvidedSource = `import silk.effect { Effect }
import silk.insecure_random { InsecureRandom }
import silk.usize as usize

struct Scripted { first: u64 index: usize }

effect fn scriptedNext(self: &mut Scripted) -> u64 {
  self.index = self.index + usize.ONE
  return self.first
}

impl InsecureRandom for Scripted { nextU64: Scripted.scriptedNext }

fn next(provider: &mut Scripted) -> u64 {
  return run InsecureRandom.nextU64() |> Effect.provideMut<InsecureRandom>(provider)
}

pub fn main() -> i32 {
  let mut direct = Scripted { first: 21, index: usize.ZERO }
  if next(&mut direct) + next(&mut direct) != 42 { return 1 }
  return 42
}`

it.effect('characterizes the trap of a service effect provided from a borrowed parameter', () =>
  Effect.gen(function* () {
    // Known lowering hole, kept visible on purpose: analysis reports nothing, the provider's
    // operation is lowered as an unavailable body, and the verifier blocks the run as an invalid
    // call shape instead of completing with 42. Delete this case together with the hole.
    const self = yield* Analysis.ofSourceRealized(
      'user-services/parameter-provided',
      new TextEncoder().encode(parameterProvidedSource),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
    const evaluated = Analysis.evaluate(self)
    assert.strictEqual(evaluated._tag, 'Blocked')
    if (evaluated._tag !== 'Blocked') return
    assert.strictEqual(evaluated.reason._tag, 'InvalidMir')
    if (evaluated.reason._tag !== 'InvalidMir') return
    // Both calls of `next` are refused for the same reason.
    assert.deepEqual(
      evaluated.reason.violations.map((violation) => violation.rule),
      ['InvalidCallShape', 'InvalidCallShape'],
    )
  }),
)
