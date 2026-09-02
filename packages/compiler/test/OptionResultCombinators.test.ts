import { readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Json from './support/Json.js'
import * as Analysis from '../src/Analysis.js'
import * as Projections from './support/projections.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

/**
 * `map` applies its transform to a present value and leaves an absent value absent, so the
 * transform runs on exactly one of the two arms. `unwrapOr` then answers with the fallback only
 * for the absent option.
 */
const optionMap = `import silk.option { Option }
fn double(value: i32) -> i32 { return value * 2 }

pub fn main() -> i32 {
  let present = Option.some<i32>(20)
  let mappedPresent = Option.map<i32, i32>(move present, double)
  let absent = Option.none<i32>()
  let mappedAbsent = Option.map<i32, i32>(move absent, double)
  if Option.unwrapOr<i32>(move mappedPresent, 0) != 40 { return 1 }
  return 40 + Option.unwrapOr<i32>(move mappedAbsent, 2)
}`

/**
 * `flatMap` keeps the outcome one Option deep. `oneLayer` names `Option<i32>` exactly, so a
 * combinator that nested its result into `Option<Option<i32>>` would not typecheck here.
 */
const optionFlatMap = `import silk.option { Option }

fn halve(value: i32) -> Option<i32> {
  if value == 0 { return Option.none<i32>() }
  return Option.some<i32>(value / 2)
}

fn oneLayer(self: Option<i32>) -> Option<i32> { return move self }

pub fn main() -> i32 {
  let present = Option.some<i32>(80)
  let chained = oneLayer(Option.flatMap<i32, i32>(move present, halve))
  let zero = Option.some<i32>(0)
  let rejected = oneLayer(Option.flatMap<i32, i32>(move zero, halve))
  return Option.unwrapOr<i32>(move chained, 0) + Option.unwrapOr<i32>(move rejected, 2)
}`

/**
 * `mapError` rewrites a failure value and carries a success through untouched, including across a
 * change of failure type: `widen` never runs on the success arm.
 */
const resultMapError = `import silk.result { Result }

struct Wide { code: i32 }

fn widen(error: i32) -> Wide {
  return Wide { code: error + 30 }
}

fn observe(self: Result<i32, Wide>) -> i32 {
  return match move self {
    Result<i32, Wide>.Success { value } => value
    Result<i32, Wide>.Failure { error } => error.code
  }
}

pub fn main() -> i32 {
  let succeeded = Result.succeed<i32, i32>(8)
  let keptSuccess = Result.mapError<i32, i32, Wide>(move succeeded, widen)
  let failed = Result.failResult<i32, i32>(4)
  let changedFailure = Result.mapError<i32, i32, Wide>(move failed, widen)
  return observe(move keptSuccess) + observe(move changedFailure)
}`

/**
 * `map`, `flatMap`, and `unwrapOr` over both channels, with borrowed matches testing an outcome
 * while leaving it usable afterwards.
 */
const resultCombinators = `import silk.result { Result }

fn addTwo(value: i32) -> i32 { return value + 2 }

fn halve(value: i32) -> Result<i32, i32> {
  if value == 0 { return Result.failResult<i32, i32>(9) }
  return Result.succeed<i32, i32>(value / 2)
}

pub fn main() -> i32 {
  let succeeded = Result.succeed<i32, i32>(36)
  let mapped = Result.map<i32, i32, i32>(move succeeded, addTwo)
  let successCheck = match &mapped {
    Result<i32, i32>.Success { value } => true
    Result<i32, i32>.Failure { error } => false
  }
  if successCheck {} else { return 1 }
  let chained = Result.flatMap<i32, i32, i32>(move mapped, halve)
  let failed = Result.failResult<i32, i32>(7)
  let mappedFailure = Result.map<i32, i32, i32>(move failed, addTwo)
  let failureCheck = match &mappedFailure {
    Result<i32, i32>.Success { value } => false
    Result<i32, i32>.Failure { error } => true
  }
  if failureCheck {} else { return 3 }
  return Result.unwrapOr<i32, i32>(move chained, 0)
    + Result.unwrapOr<i32, i32>(move mappedFailure, 23)
}`

/**
 * The same combinators over a move-only value type. `Token` owns an `Allocation`, so nothing here
 * is Copy: `map` moves the token through, the present arm of `unwrapOr` drops the fallback it did
 * not need, and the absent arm answers with the fallback instead. Three allocations, three
 * releases, no leak and no double drop.
 */
const moveOnly = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.option { Option }

struct Token { storage: Allocation }

fn release(storage: Allocation) -> i32 {
  drop storage
  return 14
}

fn consume(self: Token) -> i32 {
  return match move self {
    Token { storage } => release(move storage)
  }
}

fn identity(self: Token) -> Token { return move self }

effect fn acquire() -> Allocation ! OutOfMemoryError ? &mut Allocator {
  let layout = Layout.of<i32>()
  return run Allocator.allocate(move layout)
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let first = run Intrinsic.bindRequirementMut(acquire(), &mut allocator)
  let second = run Intrinsic.bindRequirementMut(acquire(), &mut allocator)
  let third = run Intrinsic.bindRequirementMut(acquire(), &mut allocator)
  let carried = Token { storage: move first }
  let spare = Token { storage: move second }
  let fallback = Token { storage: move third }
  let present = Option.some<Token>(move carried)
  let mapped = Option.map<Token, Token>(move present, identity)
  let held = Option.unwrapOr<Token>(move mapped, move spare)
  let absent = Option.none<Token>()
  let recovered = Option.unwrapOr<Token>(move absent, move fallback)
  return consume(move held) + consume(move recovered) + 14
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`

/** Evaluator and wasm must agree on 42 for every program. */
const acrossEngines = (name: string, source: string) =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      `stdlib/combinators/${name}`,
      ascii(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [],
      name,
    )

    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      `${name}: ${Json.stringify(evaluated, (_, value) =>
        typeof value === 'bigint' ? value.toString() : value,
      )}`,
    )
    if (evaluated._tag !== 'Completed') return evaluated
    assert.strictEqual(evaluated.result.value, 42n, `${name} evaluator`)

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42, `${name} wasm`)
    return evaluated
  })

it.effect(
  'maps a present value and keeps an absent value absent',
  () => acrossEngines('option-map', optionMap),
  180_000,
)

it.effect(
  'flat-maps without nesting the outcome type',
  () => acrossEngines('option-flat-map', optionFlatMap),
  180_000,
)

it.effect(
  'rewrites a failure value and carries a success through unchanged',
  () => acrossEngines('result-map-error', resultMapError),
  180_000,
)

it.effect(
  'maps, chains, unwraps, and tests a Result on both channels',
  () => acrossEngines('result-combinators', resultCombinators),
  180_000,
)

it.effect(
  'moves a move-only value through the combinators without leaking',
  () =>
    Effect.gen(function* () {
      const evaluated = yield* acrossEngines('option-move-only', moveOnly)
      if (evaluated._tag !== 'Completed') return
      // Every combinator drops exactly one of its two arms: three acquired allocations release
      // exactly three times, so the fallback the present arm discarded neither leaked nor
      // double-dropped.
      const tags = Projections.allocationTraceEventsOf(evaluated).map((event) => event._tag)
      assert.strictEqual(tags.filter((tag) => tag === 'AllocationAcquire').length, 3)
      assert.strictEqual(tags.filter((tag) => tag === 'AllocationRelease').length, 3)
    }),
  180_000,
)

const stdlibSource = (module: string): string =>
  readFileSync(fileURLToPath(new URL(`../stdlib/silk/${module}.silk`, import.meta.url)), 'utf8')

it('keeps both modules free of unsafe blocks and intrinsic calls', () => {
  for (const module of ['option', 'result']) {
    const source = stdlibSource(module)
    assert.notMatch(source, /\bunsafe\b/, `${module}.silk declares an unsafe block`)
    assert.notMatch(source, /\bIntrinsic\./, `${module}.silk calls an intrinsic`)
  }
})
