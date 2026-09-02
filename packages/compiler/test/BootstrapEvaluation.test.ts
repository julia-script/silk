import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as Instances from '../src/Instances.js'
import * as Mir from '../src/Mir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as Target from '../src/Target.js'
import { scalarEnumSignedAcceptance } from './support/corpus.js'
import * as Json from './support/Json.js'
import * as MirSamples from './support/mirSamples.js'

// UTF-8, not charCodeAt: corpus programs may carry non-ASCII literals, and for ASCII sources the
// bytes are identical.
const encoder = new TextEncoder()
const ascii = (value: string): Uint8Array => encoder.encode(value)

const evaluateSource = (
  text: string,
  options: BootstrapEvaluation.Options = {},
): Effect.Effect<BootstrapEvaluation.Outcome> =>
  Effect.map(Analysis.ofSourceRealized('memory/evaluation', ascii(text)), (snapshot) =>
    Analysis.evaluate(snapshot, options),
  )

it.effect('evaluates scalar enums as immutable logical member values', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(scalarEnumSignedAcceptance)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    const binding = outcome.trace.find(
      (event) => event._tag === 'Binding' && event.target.name === 'copyStatus',
    )
    assert.strictEqual(binding?._tag, 'Binding')
    if (binding?._tag !== 'Binding') return
    assert.strictEqual(binding.value._tag, 'EnumValue')
    if (binding.value._tag !== 'EnumValue') return
    assert.strictEqual(binding.value.enum.name, 'Status')
    assert.strictEqual(binding.value.member.name, 'Unknown')
    assert.strictEqual(binding.value.discriminant, -1n)
    assert.strictEqual(binding.value.representation.scalar, 'i8')
  }),
)

it.effect('traces the identity program in order with bound and returned values', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    assert.deepEqual(
      outcome.trace.map((event) => event._tag),
      ['Entry', 'RegionEntry', 'Call', 'Binding', 'Entry', 'RegionEntry', 'Return', 'Return'],
    )
    const binding = outcome.trace.at(3)
    assert.strictEqual(binding?._tag, 'Binding')
    if (binding?._tag !== 'Binding') return
    assert.strictEqual(binding.value._tag, 'IntegerValue')
    if (binding.value._tag !== 'IntegerValue') return
    assert.strictEqual(binding.value.value, 42n)
    assert.strictEqual(binding.parameterOrdinal, 0)
    assert.strictEqual(binding.fromCall, false)
    assert.strictEqual(binding.frame, 1)
    assert.strictEqual(binding.depth, 2)
    const entries = outcome.trace.filter((event) => event._tag === 'Entry')
    assert.deepEqual(
      entries.map(({ frame, depth }) => [frame, depth]),
      [
        [0, 1],
        [1, 2],
      ],
    )
  }),
)

it.effect('runs a hidden Effect environment returned across an ordinary function boundary', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`fn delayed(value: i32) -> Effect<i32> {
  let eager = value + 1
  return effect { return eager + 1 }
}
pub fn main() -> i32 {
  let pending = delayed(40)
  return run pending
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.isTrue(
      outcome.trace.some(
        (event) => event._tag === 'Call' && event.target.name.includes('$effect$'),
      ),
    )
  }),
)

it.effect('dispatches an arbitrary source service through a provided source witness', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'memory/evaluation',
      ascii(`import silk.effect as Effect
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
}`),
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const instances = Analysis.instancesOf(snapshot)
    assert.deepEqual(instances.violations, [])
    const mir = Analysis.mirOf(snapshot)
    assert.strictEqual(
      mir._tag,
      'Available',
      mir._tag === 'Unavailable' ? mir.error.message : undefined,
    )
    const outcome = Analysis.evaluate(snapshot)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('executes named function values and stored automatic sections', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`import silk.i32 as i32
fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let named = identity
  let plusTwo = i32.add(2)
  return named(plusTwo(40))
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(
      outcome.trace.filter((event) => event._tag === 'CallableConstruct').length,
      2,
    )
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'CallableApply').length, 2)
  }),
)

it.effect('applies a trailing callable section in successive stages', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`fn combine(a: i32, b: i32, c: i32) -> i32 {
  return a * 100 + b * 10 + c
}
pub fn main() -> i32 { return combine(3)(2)(1) }`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 123n)
  }),
)

it.effect('moves affine staged captures exactly once', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`struct Token { value: i32 }
fn combine(a: i32, b: Token, c: Token) -> i32 {
  return a * 100 + b.value * 10 + c.value
}
pub fn main() -> i32 {
  let two = Token { value: 2 }
  let three = Token { value: 3 }
  return combine(move three)(move two)(1)
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 123n)
  }),
)

it.effect('reuses an exclusive callable after each synchronous invocation completes', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`fn write(value: i32, values: &mut [i32]) -> i32 {
  values[0] = value
  return values[0]
}
pub fn main() -> i32 {
  let mut values = [0]
  let mut callback = write(&mut values)
  let first = callback(20)
  let second = callback(first + 2)
  drop callback
  return second
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 22n)
    assert.strictEqual(
      outcome.trace.filter((event) => event._tag === 'CallableApply' && event.mode === 'Exclusive')
        .length,
      2,
    )
  }),
)

it.effect('blocks a second evaluator invocation of a consumed callable identity', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'memory/consumed-callable',
      ascii(`struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let callback = consume(move token)
  return callback(40)
}`),
    )
    const original = Analysis.loweredMir(self)
    const repeated: Mir.Module = {
      ...original,
      functions: original.functions.map((fn) => ({
        ...fn,
        regions: fn.regions.map((region) => {
          if (region._tag !== 'OperationRegion') return region
          return {
            ...region,
            operations: region.operations.flatMap<Mir.Operation>((operation) => {
              if (operation._tag === 'ApplyCallable') return [operation, operation]
              return [operation]
            }),
          }
        }),
      })),
    }

    const outcome = BootstrapEvaluation.evaluate(self.instances, repeated)
    assert.strictEqual(outcome._tag, 'Blocked', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'InvalidCallableReuse')
    assert.strictEqual(outcome.trace.at(-1)?._tag, 'CallableRejected')
  }),
)

it.effect('maps, flatMaps, and taps Effect successes through ordinary callable values', () =>
  Effect.gen(function* () {
    const mapped = yield* evaluateSource(`import silk.effect as Effect
import silk.i32 as i32
effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(2) |> Effect.map(i32.add(2)) }`)
    const flatMapped = yield* evaluateSource(`import silk.effect as Effect
effect fn succeed(value: i32) -> i32 { return value }
effect fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return run succeed(21) |> Effect.flatMap(double) }`)
    const tapped = yield* evaluateSource(`import silk.effect as Effect
effect fn succeed(value: i32) -> i32 { return value }
effect fn observe(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(42) |> Effect.tap(observe) }`)

    for (const [outcome, expected] of [
      [mapped, 4],
      [flatMapped, 42],
      [tapped, 42],
    ] as const) {
      assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, BigInt(expected))
    }
  }),
)

it.effect('keeps an Effect returned from map nested until a second run', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`import silk.effect as Effect
effect fn succeed(value: i32) -> i32 { return value }
effect fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return run (run (succeed(21) |> Effect.map(double))) }`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('retains an exclusive mapper environment across repeated mapped Effect runs', () =>
  Effect.gen(function* () {
    const source = `import silk.effect as Effect
effect fn succeed(value: i32) -> i32 { return value }
fn increment(value: i32, state: &mut [i32]) -> i32 {
  state[0] = state[0] + 1
  return value + state[0]
}
pub fn main() -> i32 {
  let mut state = [0]
  let mapped = succeed(20) |> Effect.map(increment(&mut state))
  let first = run mapped
  let second = run mapped
  drop mapped
  return first + second
}`
    const self = yield* Analysis.ofSourceRealized('memory/mutable-map', ascii(source))
    const outcome = Analysis.evaluate(self)

    assert.strictEqual(
      outcome._tag,
      'Completed',
      `${Json.stringify({ diagnostics: self.diagnostics, outcome }, 2)}\n${MirEncoding.encode(Analysis.loweredMir(self))}`,
    )
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 43n)
    assert.strictEqual(
      outcome.trace.filter(
        (event) =>
          event._tag === 'CallableApply' &&
          event.mode === 'Exclusive' &&
          event.function.module === 'silk/effect',
      ).length,
      2,
    )
  }),
)

it.effect('drops an unrun mapped Effect owned callback environment exactly once', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`import silk.effect as Effect
struct Token { value: i32 }
effect fn succeed(value: i32) -> i32 { return value }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let mapped = succeed(40) |> Effect.map(consume(move token))
  drop mapped
  return 42
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'CallableCleanup').length, 1)
  }),
)

it.effect('catches a typed failure through an automatic effect-handler section', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`import silk.effect as Effect
struct Problem { code: i32 }
effect fn failNow() -> i32 ! Problem { fail Problem { code: 41 } }
effect fn recover(problem: Problem, adjustment: i32) -> i32 {
  return problem.code + adjustment
}
pub fn main() -> i32 {
  let handled = failNow() |> Effect.catchAll(recover(1))
  return run handled
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.include(
      outcome.trace.map((event) => event._tag),
      'CallableApply',
    )
  }),
)

it.effect('preserves exclusive capture state across repeated Effect runs', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub fn main() -> i32 {
  let mut counter = 0
  let pending = effect { counter = counter + 1 return counter }
  let first = run pending
  let second = run pending
  return first * 10 + second
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 12n)
  }),
)

it.effect('constructs effect calls lazily and executes them only at run', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`effect fn delayed(value: i32) -> i32 { return value + 1 }
pub fn main() -> i32 {
  let recipe = delayed(41)
  return run recipe
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42n)
    assert.deepEqual(
      outcome.trace.map((event) => event._tag),
      [
        'Entry',
        'RegionEntry',
        'RegionEntry',
        'Call',
        'Entry',
        'RegionEntry',
        'EffectSuccess',
        'Return',
        'RegionEntry',
        'Cleanup',
        'Return',
      ],
    )
  }),
)

it.effect('catches one exact owned effect failure without using traps', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`import silk.effect as Effect
struct Problem { code: i32 }
effect fn risky(value: i32) -> i32 ! Problem {
  if value == 0 { fail move Problem { code: 41 } }
  return value
}
effect fn recover(problem: Problem) -> i32 { return problem.code + 1 }
pub fn main() -> i32 {
  let recipe = Effect.catchAll(risky(0), recover)
  return run recipe
}`)

    assert.strictEqual(outcome._tag, 'Completed', Json.stringify(outcome, 2))
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42n)
    assert.isTrue(outcome.trace.some((event) => event._tag === 'EffectFailure'))
    assert.isTrue(outcome.trace.some((event) => event._tag === 'EffectSuccess'))
  }),
)

it.effect('marks nested-call bindings and orders inner events before outer bindings', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`)

    assert.strictEqual(outcome._tag, 'Completed')
    const tags = outcome.trace.map((event) => event._tag)
    assert.deepEqual(tags, [
      'Entry',
      'RegionEntry',
      'Call',
      'Binding',
      'Entry',
      'RegionEntry',
      'Return',
      'Call',
      'Binding',
      'Entry',
      'RegionEntry',
      'Return',
      'Return',
    ])
    const outerBinding = outcome.trace.at(8)
    assert.strictEqual(outerBinding?._tag, 'Binding')
    if (outerBinding?._tag !== 'Binding') return
    assert.strictEqual(outerBinding.fromCall, true)
    const innerCall = outcome.trace.at(2)
    const outerCall = outcome.trace.at(7)
    assert.strictEqual(innerCall?._tag, 'Call')
    assert.strictEqual(outerCall?._tag, 'Call')
    if (innerCall?._tag !== 'Call' || outerCall?._tag !== 'Call') return
    assert.notDeepEqual(innerCall.span, outerCall.span)
  }),
)

it.effect('retains the completed prefix before a trap without fabricated events', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(identity(1), missing(2)) }`)

    assert.strictEqual(outcome._tag, 'Trap')
    if (outcome._tag !== 'Trap') return
    assert.deepEqual(
      outcome.trace.map((event) => event._tag),
      ['Entry', 'RegionEntry'],
    )
  }),
)

it.effect('bounds direct and mutual recursion by active call depth', () =>
  Effect.gen(function* () {
    for (const source of [
      'pub fn main() -> i32 { return main() }',
      `pub fn main() -> i32 { return other() }
fn other() -> i32 { return main() }`,
    ]) {
      const outcome = yield* evaluateSource(source, { maxCallDepth: 4 })
      assert.strictEqual(outcome._tag, 'Blocked')
      if (outcome._tag !== 'Blocked') continue
      assert.strictEqual(outcome.reason._tag, 'EvaluationLimit')
      if (outcome.reason._tag !== 'EvaluationLimit') continue
      assert.strictEqual(outcome.reason.kind, 'CallDepth')
      assert.strictEqual(outcome.reason.limit, 4)
      assert.strictEqual(outcome.reason.count, 4)
      assert.lengthOf(outcome.reason.activeFrames, 4)
      assert.isAbove(outcome.reason.span.end, outcome.reason.span.start)
    }
  }),
)

it.effect('unwinds a typed failure through recursive effect activations', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`import silk.effect as Effect
struct Problem { code: i32 }
effect fn countdown(value: i32) -> i32 ! Problem {
  if value == 0 { fail Problem { code: 41 } }
  return run countdown(value - 1)
}
effect fn recover(problem: Problem) -> i32 { return problem.code + 1 }
pub fn main() -> i32 { return run Effect.catchAll(countdown(4), recover) }`)

    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.isAtLeast(outcome.trace.filter((event) => event._tag === 'EffectFailure').length, 1)
  }),
)

it.effect('stops an infinite loop at the exact configured operation count', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource('pub fn main() -> i32 { while true {} return 0 }', {
      maxSteps: 8,
    })
    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'EvaluationLimit')
    if (outcome.reason._tag !== 'EvaluationLimit') return
    assert.strictEqual(outcome.reason.kind, 'Steps')
    assert.strictEqual(outcome.reason.limit, 8)
    assert.strictEqual(outcome.reason.count, 8)
    assert.isAbove(outcome.reason.span.end, outcome.reason.span.start)
  }),
)

it.effect('validates resource limits before evaluation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'memory/invalid-limits',
      ascii('pub fn main() -> i32 { return 0 }'),
    )
    assert.throws(() => Analysis.evaluate(snapshot, { maxSteps: 0 }), RangeError)
    assert.throws(() => Analysis.evaluate(snapshot, { maxCallDepth: 1.5 }), RangeError)
  }),
)

it.effect('evaluates identically across repeated fresh runs', () =>
  Effect.gen(function* () {
    const source = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`
    const first = yield* evaluateSource(source)
    const second = yield* evaluateSource(source)

    assert.deepEqual(first, second)
  }),
)

it.effect('refuses malformed target-aware MIR before executing any operation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'memory/invalid-layout',
      ascii(
        'import silk.i32 as i32\npub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }',
      ),
      'wasm32-unknown-unknown',
    )
    const mir = Analysis.mirOf(snapshot)
    if (mir._tag !== 'Available') return assert.fail('expected target-aware MIR')
    const bool = mir.value.layout.entries.find((entry) => entry.type === 'bool')
    if (bool === undefined) return assert.fail('expected bool layout')
    const malformed = {
      ...mir.value,
      layout: {
        ...mir.value.layout,
        entries: mir.value.layout.entries.map((entry) =>
          entry.type === 'bool' ? { ...bool, size: 1 } : entry,
        ),
      },
    }

    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, malformed)
    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'InvalidMir')
    assert.deepEqual(outcome.trace, [])
  }),
)

it.effect('evaluates operator precedence, prefix operations, and pipeline chains', () =>
  Effect.gen(function* () {
    const arithmetic = yield* evaluateSource(
      'import silk.i32 as i32\npub fn main() -> i32 { return 2 + 3 * 4 |> i32.add(1) }',
    )
    const boolean = yield* evaluateSource(
      'pub fn main() -> i32 { if !(2 * 3 == 7) { return 42 } return 0 }',
    )

    assert.strictEqual(arithmetic._tag, 'Completed')
    assert.strictEqual(arithmetic._tag === 'Completed' ? arithmetic.result.value : undefined, 15n)
    assert.strictEqual(boolean._tag, 'Completed')
    assert.strictEqual(boolean._tag === 'Completed' ? boolean.result.value : undefined, 42n)
  }),
)

it.effect('preserves trapping arithmetic through operator sugar', () =>
  Effect.gen(function* () {
    const division = yield* evaluateSource('pub fn main() -> i32 { return 1 / 0 }')
    const negation = yield* evaluateSource('pub fn main() -> i32 { return -(-2147483648) }')

    assert.strictEqual(division._tag, 'Trap')
    assert.strictEqual(division._tag === 'Trap' ? division.classification : undefined, 'Trap')
    assert.strictEqual(negation._tag, 'Trap')
    assert.strictEqual(negation._tag === 'Trap' ? negation.classification : undefined, 'Trap')
  }),
)

it.effect('evaluates logical union matches with source-ordered guarded fallthrough', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`import silk.i32 as i32
pub struct Left { value: i32 }
pub struct Right { value: i32 }
pub fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => i32.add(answer, 1)
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 41 }) }`)

    assert.strictEqual(outcome._tag, 'Completed')
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42n)
    assert.deepEqual(
      outcome.trace.filter((event) => event._tag.startsWith('Match')).map((event) => event._tag),
      [
        'MatchDispatch',
        'MatchCandidate',
        'MatchCandidate',
        'MatchCandidate',
        'MatchCandidate',
        'MatchSelected',
        'MatchBorrowEnd',
      ],
    )
  }),
)

it.effect('evaluates nested move matches and traces selected-path cleanup exactly once', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub struct Leaf { value: i32 }
pub struct Box { answer: i32 leaf: Leaf }
pub fn main() -> i32 {
  let box = Box { answer: 42, leaf: Leaf { value: 0 } }
  return match move box { Box { answer, .. } => answer }
}`)

    assert.strictEqual(outcome._tag, 'Completed')
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42n)
    const cleanup = outcome.trace.filter((event) => event._tag === 'MatchCleanup')
    assert.strictEqual(cleanup.length, 1)
    const first = cleanup.at(0)
    if (first?._tag !== 'MatchCleanup') return assert.fail('expected match cleanup')
    assert.deepEqual(
      first.path?.map((field) => field.ordinal),
      [1],
    )
  }),
)

it.effect('executes explicit drop exactly once before the following statement', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`struct Token { value: i32 }
pub fn main() -> i32 {
  let token = Token { value: 1 }
  drop token
  return 42
}`)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'Cleanup').length, 1)
  }),
)

it.effect('evaluates verified Copy match access without consuming the logical payload', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSourceRealized(
      'memory/copy-match',
      ascii(`struct Token { value: i32 }
fn inspect(input: Token) -> i32 { return match &input { Token { value } => value } }
pub fn main() -> i32 { return inspect(Token { value: 42 }) }`),
    )
    const original = Analysis.loweredMir(self)
    const copied: Mir.Module = {
      ...original,
      functions: original.functions.map((fn) => ({
        ...fn,
        regions: fn.regions.map((region) => {
          if (region._tag !== 'OperationRegion') return region
          return {
            ...region,
            operations: region.operations.map((operation) => {
              if (operation._tag !== 'Match') return operation
              return {
                ...operation,
                access: 'Copy',
                arms: operation.arms.map((arm) => ({
                  ...arm,
                  bindings: arm.bindings.map((binding) => ({
                    ...binding,
                    access: 'Copy',
                  })),
                  selected: {
                    ...arm.selected,
                    access: 'Copy',
                    cleanup: [],
                    endBorrow: false,
                  },
                })),
              }
            }),
          }
        }),
      })),
    }

    const outcome = BootstrapEvaluation.evaluate(self.instances, copied)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag !== 'Completed') return
    assert.deepEqual(outcome.result, {
      _tag: 'IntegerValue',
      type: 'i32',
      value: 42n,
    })
  }),
)

it.effect('blocks a reachable foreign call before execution starts', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`unsafe extern "C" fn abs(value: i32) -> i32
pub fn main() -> i32 { return unsafe abs(-42) }`)
    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag !== 'Blocked' || outcome.reason._tag !== 'ForeignTargetUnavailable')
      return assert.fail('expected foreign availability to block the evaluator')
    assert.deepEqual(
      outcome.reason.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0193'],
    )
    assert.deepEqual(outcome.trace, [])
  }),
)

it('throws when target validation lets a ForeignCall reach the evaluator', () => {
  const program = MirSamples.foreignCallSample(Target.aarch64AppleDarwin)
  const discovery: Instances.Discovery = Object.freeze({
    ...Instances.invalid(program.module),
    entry: Object.freeze({
      _tag: 'Resolved',
      kind: 'Ordinary',
      result: 'Status',
      key: Mir.machineEntry(program),
    }),
  })
  assert.throws(() => BootstrapEvaluation.evaluate(discovery, program), RangeError)
})
