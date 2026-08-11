import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const copyPayloads = `import silk.result { Result, Success, Failure, succeed, failResult }

fn observe(result: Result<i32, i32>) -> i32 {
  return match move result {
    Result<i32, i32> { value: outcome } => match move outcome {
      Success<i32> { value: successValue } => successValue
      Failure<i32> { error: failureValue } => failureValue
    }
  }
}

pub fn main() -> i32 {
  let success = succeed<i32, i32>(40)
  let failure = failResult<i32, i32>(2)
  return observe(move success) + observe(move failure)
}`

const affinePayload = `import silk.result { Result, Success, Failure, succeed }

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
    Result<Token, i32> { value: outcome } => match move outcome {
      Success<Token> { value: token } => consume(move token)
      Failure<i32> { error: failureValue } => failureValue
    }
  }
}

effect fn build() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<i32>()
  let storage = run Intrinsic.bindRequirement(Allocator.allocate(move layout), &mut allocator)
  let token = Token { storage: move storage }
  let result = succeed<Token, i32>(move token)
  return observe(move result)
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catch(build(), recover) }`

const reifiedEffect = `import silk.result { Result, Success, Failure }

struct First { code: i32 }
struct Second { code: i32 }

effect fn choose(first: bool) -> i32 ! First | Second {
  if first { fail First { code: 20 } }
  fail Second { code: 22 }
}

effect fn inspect(first: bool) -> i32 {
  let completed = run Effect.result(choose(first))
  return match move completed {
    Result<i32, First | Second> { value: outcome } => match move outcome {
      Success<i32> { value: successValue } => successValue
      Failure<First | Second> { error } => match move error {
        First { code: firstCode } => firstCode
        Second { code: secondCode } => secondCode
      }
    }
  }
}

pub fn main() -> i32 {
  let first = run inspect(true)
  let second = run inspect(false)
  return first + second
}`

const reifiedRequirement = `import silk.result { Result, Success, Failure }

effect fn allocateOne() -> Allocation ! OutOfMemory ? &mut Allocator {
  let layout = Layout.of<i32>()
  return run Allocator.allocate(move layout)
}

effect fn attempt() -> Result<Allocation, OutOfMemory> ? &mut Allocator {
  return run Effect.result(allocateOne())
}

effect fn build() -> i32 {
  let mut allocator = SystemAllocator.make()
  let completed = run Intrinsic.bindRequirement(attempt(), &mut allocator)
  return match move completed {
    Result<Allocation, OutOfMemory> { value: outcome } => match move outcome {
      Success<Allocation> { value: storage } => release(move storage)
      Failure<OutOfMemory> { error: ignored } => 0
    }
  }
}

fn release(storage: Allocation) -> i32 {
  drop storage
  return 42
}

pub fn main() -> i32 { return run build() }`

const reifiedTrap = `import silk.result { Result }

effect fn explode() -> i32 {
  return 1 / 0
}

pub fn main() -> i32 {
  let completed = run Effect.result(explode())
  return 42
}`

const boundRequirements = `struct Clock {}
struct Config {}

effect fn read() -> i32 ? &Clock@Primary | &Config { return 42 }

pub fn main() -> i32 {
  let clock = Clock {}
  let config = Config {}
  let rest = Intrinsic.bindRequirement(read(), &clock, @Primary)
  let closed = rest |> Intrinsic.bindRequirement(&config)
  return run closed
}`

const sourceDefinedMaps = `import silk.result { Result, Success, Failure }

struct First { code: i32 }
struct Second { code: i32 }

effect fn succeed() -> i32 ! First { return 40 }
effect fn failFirst() -> i32 ! First { fail First { code: 2 } }
fn addTwo(value: i32) -> i32 { return value + 2 }
fn toSecond(error: First) -> Second { return Second { code: error.code + 40 } }

fn observe(result: Result<i32, Second>) -> i32 {
  return match move result {
    Result<i32, Second> { value: outcome } => match move outcome {
      Success<i32> { value: answer } => answer
      Failure<Second> { error } => error.code
    }
  }
}

pub fn main() -> i32 {
  let success = run Effect.result(succeed() |> Effect.mapError(toSecond) |> Effect.map(addTwo))
  let failure = run Effect.result(failFirst() |> Effect.mapBoth(addTwo, toSecond))
  return observe(move success) + observe(move failure) - 42
}`

const sourceDefinedEffectfulCombinators = `import silk.result { Result, Success, Failure }

struct First { code: i32 }
struct Second { code: i32 }

effect fn succeed() -> i32 ! First { return 40 }
effect fn failFirst() -> i32 ! First { fail First { code: 2 } }
effect fn addTwo(value: i32) -> i32 ! Second { return value + 2 }
effect fn preserve(value: i32) -> i32 ! Second { return value }
effect fn recover(error: First) -> i32 ! Second { return error.code + 40 }

fn observeBoth(result: Result<i32, First | Second>) -> i32 {
  return match move result {
    Result<i32, First | Second> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<First | Second> { error } => match move error {
        First { code } => code
        Second { code } => code
      }
    }
  }
}

fn observeSecond(result: Result<i32, Second>) -> i32 {
  return match move result {
    Result<i32, Second> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<Second> { error } => error.code
    }
  }
}

pub fn main() -> i32 {
  let chained = run Effect.result(succeed() |> Effect.flatMap(addTwo))
  let observed = run Effect.result(succeed() |> Effect.tap(preserve))
  let recovered = run Effect.result(failFirst() |> Effect.catch(recover))
  return observeBoth(move chained) + observeBoth(move observed) + observeSecond(move recovered) - 82
}`

const sourceDefinedRetry = `import silk.result { Result, Success, Failure }

struct Problem { code: i32 }

effect fn succeed() -> i32 ! Problem { return 42 }
effect fn failAlways() -> i32 ! Problem { fail Problem { code: 2 } }

fn observe(result: Result<i32, Problem>) -> i32 {
  return match move result {
    Result<i32, Problem> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<Problem> { error } => error.code
    }
  }
}

pub fn main() -> i32 {
  let success = run Effect.result(succeed() |> Effect.retry(3))
  let failure = run Effect.result(failAlways() |> Effect.retry(2))
  return observe(move success) + observe(move failure) - 2
}`

const sourceDefinedProvide = `struct Clock {}

effect fn read() -> i32 ? &Clock { return 42 }
effect fn makeClock() -> Clock { return Clock {} }

pub fn main() -> i32 {
  let clock = Clock {}
  let direct = run read() |> Effect.provide(&clock)
  let acquired = run read() |> Effect.provideWith(makeClock())
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
    assert.strictEqual(evaluated.result.value, 42)

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

it.effect('keeps runtime traps outside the typed Result error channel', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'stdlib/result/reified-trap',
      ascii(reifiedTrap),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Blocked')
    assert.strictEqual(evaluated._tag === 'Blocked' ? evaluated.reason._tag : undefined, 'Trap')
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
