import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const copyPayloads = `import silk.result { Result, succeed, failResult }

fn observe(result: Result<i32, i32>) -> i32 {
  return match move result {
      Result<i32, i32>.Success { value: successValue } => successValue
      Result<i32, i32>.Failure { error: failureValue } => failureValue
  }
}

pub fn main() -> i32 {
  let success = succeed<i32, i32>(40)
  let failure = failResult<i32, i32>(2)
  return observe(move success) + observe(move failure)
}`

const affinePayload = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.result { Result, succeed }

struct Token { storage: Allocation }

fn consumeStorage(storage: Allocation) -> i32 {
  drop storage
  return 42
}

fn consume(token: Token) -> i32 {
  return match move token {
    Token { storage } => consumeStorage(move storage)
  }
}

fn observe(result: Result<Token, i32>) -> i32 {
  return match move result {
      Result<Token, i32>.Success { value: token } => consume(move token)
      Result<Token, i32>.Failure { error: failureValue } => failureValue
  }
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<i32>()
  let storage = run Intrinsic.bindRequirementMut(Allocator.allocate(move layout), &mut allocator)
  let token = Token { storage: move storage }
  let result = succeed<Token, i32>(move token)
  return observe(move result)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

const reifiedEffect = `import silk.effect as Effect
import silk.result { Result }

struct First { code: i32 }
struct Second { code: i32 }

effect fn choose(first: bool) -> i32 ! First | Second {
  if first { fail First { code: 20 } }
  fail Second { code: 22 }
}

effect fn inspect(first: bool) -> i32 {
  let completed = run Effect.result(choose(first))
  return match move completed {
      Result<i32, First | Second>.Success { value: successValue } => successValue
      Result<i32, First | Second>.Failure { error } => match move error {
        First { code: firstCode } => firstCode
        Second { code: secondCode } => secondCode
      }
  }
}

pub fn main() -> i32 {
  let first = run inspect(true)
  let second = run inspect(false)
  return first + second
}`

const reifiedRequirement = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.result { Result }

effect fn allocateOne() -> Allocation ! OutOfMemoryError ? &mut Allocator {
  let layout = Layout.of<i32>()
  return run Allocator.allocate(move layout)
}

effect fn attempt() -> Result<Allocation, OutOfMemoryError> ? &mut Allocator {
  return run Effect.result(allocateOne())
}

effect fn build() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  let completed = run Intrinsic.bindRequirementMut(attempt(), &mut allocator)
  return match move completed {
      Result<Allocation, OutOfMemoryError>.Success { value: storage } => release(move storage)
      Result<Allocation, OutOfMemoryError>.Failure { error: ignored } => 0
  }
}

fn release(storage: Allocation) -> i32 {
  drop storage
  return 42
}

pub fn main() -> i32 { return run build() }`

const reifiedFailureInsideNestedProvider = `import silk.effect { Effect }
import silk.result { Result }

struct Problem { code: i32 }

service Writer {
  effect fn write() -> i32 ? &mut Writer
}

service Sink {
  effect fn act() -> () ! Problem ? &mut Sink
}

struct SinkImpl {}

effect fn ignoreError<A, E, ?R>(protected: once Effect<A ! E ? R>) -> A | () ? R {
  let completed = run Effect.result(move protected)
  return match move completed {
    Result<A, E>.Success { value } => move value
    Result<A, E>.Failure { error } => ()
  }
}

impl Sink for SinkImpl {
  effect fn act(self: &Self) -> () ! Problem ? &mut Sink {
    fail Problem { code: 42 }
  }
}

struct StdoutWriter {}

impl Writer for StdoutWriter {
  effect fn write(self: &Self) -> i32 ? &mut Writer {
    let mut sink = SinkImpl {}
    run Sink.act()
      |> ignoreError
      |> Effect.provideMut(&mut sink)
    return 42
  }
}

effect fn program() -> i32 ? &mut Writer {
  return run Writer.write()
}

pub fn main() -> i32 {
  let mut writer = StdoutWriter {}
  return run Effect.provideMut(program(), &mut writer)
}`

const alternateResultLikeUnion = `import silk.effect as Effect

union Outcome<A, E> {
  Good { value: A },
  Bad { error: E },
}

struct First { code: i32 }
struct Second { code: i32 }

fn good<A, E>(value: A) -> Outcome<A, E> {
  return Outcome<A, E>.Good { value: move value }
}

effect fn bad<A, E>(error: E) -> Outcome<A, E> {
  return Outcome<A, E>.Bad { error: move error }
}

effect fn outcome<A, E, ?R>(
  protected: once Effect<A ! E ? R>
) -> Outcome<A, E> ? R {
  let succeeded = Effect.map<A, Outcome<A, E>, E>(move protected, good)
  return run Effect.catchAll<Outcome<A, E>, Outcome<A, E>, E, never>(
    move succeeded,
    bad
  )
}

effect fn choose(kind: i32) -> i32 ! First | Second {
  if kind == 0 { return 5 }
  if kind == 1 { fail First { code: 20 } }
  fail Second { code: 22 }
}

effect fn inspect(kind: i32) -> i32 {
  let completed = run outcome(choose(kind))
  return match move completed {
    Outcome<i32, First | Second>.Good { value } => value
    Outcome<i32, First | Second>.Bad { error } => match move error {
      First { code } => code
      Second { code } => code
    }
  }
}

pub fn main() -> i32 {
  let success = run inspect(0)
  let first = run inspect(1)
  let second = run inspect(2)
  return success + first + second - 5
}`

const reifiedTrap = `import silk.effect as Effect
import silk.result { Result }

effect fn explode() -> i32 {
  return 1 / 0
}

pub fn main() -> i32 {
  let completed = run Effect.result(explode())
  return 42
}`

const boundRequirements = `role Primary
service Clock { effect fn value() -> i32 ? &Clock }
service Config { effect fn value() -> i32 ? &Config }
struct FixedClock { value: i32 }
struct FixedConfig { value: i32 }
effect fn clockValue(self: &FixedClock) -> i32 { return self.value }
effect fn configValue(self: &FixedConfig) -> i32 { return self.value }
impl Clock for FixedClock { value: FixedClock.clockValue }
impl Config for FixedConfig { value: FixedConfig.configValue }

effect fn read() -> i32 ? &Clock at Primary | &Config { return 42 }

pub fn main() -> i32 {
  let clock = FixedClock { value: 1 }
  let config = FixedConfig { value: 2 }
  let rest = Intrinsic.bindRequirement<Clock at Primary>(read(), &clock)
  let closed = rest |> Intrinsic.bindRequirement(&config)
  return run closed
}`

const sourceDefinedMaps = `import silk.effect as Effect
import silk.result { Result }

struct First { code: i32 }
struct Second { code: i32 }

effect fn succeed() -> i32 ! First { return 40 }
effect fn failFirst() -> i32 ! First { fail First { code: 2 } }
fn addTwo(value: i32) -> i32 { return value + 2 }
fn toSecond(error: First) -> Second { return Second { code: error.code + 40 } }

fn observe(result: Result<i32, Second>) -> i32 {
  return match move result {
      Result<i32, Second>.Success { value: answer } => answer
      Result<i32, Second>.Failure { error } => error.code
  }
}

pub fn main() -> i32 {
  let success = run Effect.result(succeed() |> Effect.mapError(toSecond) |> Effect.map(addTwo))
  let failure = run Effect.result(failFirst() |> Effect.mapBoth(addTwo, toSecond))
  return observe(move success) + observe(move failure) - 42
}`

const sourceDefinedEffectfulCombinators = `import silk.effect as Effect
import silk.result { Result }

struct First { code: i32 }
struct Second { code: i32 }

effect fn succeed() -> i32 ! First { return 40 }
effect fn failFirst() -> i32 ! First { fail First { code: 2 } }
effect fn addTwo(value: i32) -> i32 ! Second { return value + 2 }
effect fn preserve(value: i32) -> i32 ! Second { return value }
effect fn recover(error: First) -> i32 ! Second { return error.code + 40 }

fn observeBoth(result: Result<i32, First | Second>) -> i32 {
  return match move result {
      Result<i32, First | Second>.Success { value } => value
      Result<i32, First | Second>.Failure { error } => match move error {
        First { code } => code
        Second { code } => code
      }
  }
}

fn observeSecond(result: Result<i32, Second>) -> i32 {
  return match move result {
      Result<i32, Second>.Success { value } => value
      Result<i32, Second>.Failure { error } => error.code
  }
}

pub fn main() -> i32 {
  let chained = run Effect.result(succeed() |> Effect.flatMap(addTwo))
  let observed = run Effect.result(succeed() |> Effect.tap(preserve))
  let recovered = run Effect.result(failFirst() |> Effect.catchAll(recover))
  return observeBoth(move chained) + observeBoth(move observed) + observeSecond(move recovered) - 82
}`

const sourceDefinedRetry = `import silk.effect as Effect
import silk.result { Result }

struct Problem { code: i32 }

effect fn succeed() -> i32 ! Problem { return 42 }
effect fn failAlways() -> i32 ! Problem { fail Problem { code: 2 } }

fn observe(result: Result<i32, Problem>) -> i32 {
  return match move result {
      Result<i32, Problem>.Success { value } => value
      Result<i32, Problem>.Failure { error } => error.code
  }
}

pub fn main() -> i32 {
  let success = run Effect.result(succeed() |> Effect.retry(3))
  let failure = run Effect.result(failAlways() |> Effect.retry(2))
  return observe(move success) + observe(move failure) - 2
}`

const sourceDefinedProvide = `import silk.effect as Effect
service Clock { effect fn value() -> i32 ? &mut Clock }
struct FixedClock { value: i32 }
effect fn clockValue(self: &mut FixedClock) -> i32 { return self.value }
impl Clock for FixedClock { value: FixedClock.clockValue }

effect fn read() -> i32 ? &Clock { return 42 }
effect fn makeClock() -> FixedClock { return FixedClock { value: 42 } }

pub fn main() -> i32 {
  let clock = FixedClock { value: 42 }
  let direct = run read() |> Effect.provide(&clock)
  let acquired = run read() |> Effect.provideEffect(makeClock())
  return direct + acquired - 42
}`

const evaluateAndRunWasm = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `stdlib/result/${name}`,
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot),
      [],
      Analysis.diagnostics(snapshot)
        .map((diagnostic) => `${diagnostic.code}: ${diagnostic.message}`)
        .join('\n'),
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      ),
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)

    const native = yield* Analysis.ofSourceRealized(
      `stdlib/result/${name}`,
      ascii(source),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(native), [])
    yield* Analysis.codegen(native, { mode: 'release' })
  })

it.effect('matches canonical Result with Copy success and failure payloads', () =>
  evaluateAndRunWasm('copy', copyPayloads),
)

it.effect('moves an affine payload through canonical Result exactly once', () =>
  evaluateAndRunWasm('affine', affinePayload),
)

it.effect('reifies every typed Effect branch as ordinary Result data', () =>
  evaluateAndRunWasm('reified-effect', reifiedEffect),
)

it.effect('preserves requirements while moving affine channel data into Result', () =>
  evaluateAndRunWasm('reified-requirement', reifiedRequirement),
)

it.effect('reifies failure inside a nested mutable service provider', () =>
  evaluateAndRunWasm('reified-failure-inside-nested-provider', reifiedFailureInsideNestedProvider),
)

it.effect('composes an alternate generic result-like union from map and catchAll', () =>
  evaluateAndRunWasm('alternate-result-like-union', alternateResultLikeUnion),
)

it.effect('keeps runtime traps outside the typed Result error channel', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'stdlib/result/reified-trap',
      ascii(reifiedTrap),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Trap')
  }),
)

it.effect('binds one typed requirement while retaining an unknown remainder', () =>
  evaluateAndRunWasm('bound-requirements', boundRequirements),
)

it.effect('runs source-defined success and failure channel maps', () =>
  evaluateAndRunWasm('source-defined-maps', sourceDefinedMaps),
)

it.effect('runs source-defined effectful channel combinators', () =>
  evaluateAndRunWasm('source-defined-effectful', sourceDefinedEffectfulCombinators),
)

it.effect('retries a repeatable Effect in source-defined library code', () =>
  evaluateAndRunWasm('source-defined-retry', sourceDefinedRetry),
)

it.effect('binds a generic requirement in source-defined library code', () =>
  evaluateAndRunWasm('source-defined-provide', sourceDefinedProvide),
)
