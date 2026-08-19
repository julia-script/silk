import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as Mir from '../src/Mir.js'
import { corpus } from './support/corpus.js'
import * as Json from './support/Json.js'

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

it.effect(
  'reproduces every pinned corpus outcome',
  () =>
    Effect.gen(function* () {
      for (const program of corpus) {
        const outcome = yield* evaluateSource(program.source)
        switch (program.expected._tag) {
          case 'Completes':
            assert.strictEqual(outcome._tag, 'Completed', program.name)
            if (outcome._tag === 'Completed') {
              assert.strictEqual(
                outcome.result.value,
                BigInt(program.expected.result),
                program.name,
              )
            }
            break
          case 'Trap':
            assert.strictEqual(outcome._tag, 'Blocked', program.name)
            if (outcome._tag === 'Blocked') {
              assert.strictEqual(outcome.reason._tag, 'Trap', program.name)
            }
            break
          case 'UnavailableEntry':
            assert.strictEqual(outcome._tag, 'Blocked', program.name)
            if (outcome._tag === 'Blocked' && outcome.reason._tag === 'UnavailableEntry') {
              assert.strictEqual(outcome.reason.reason, program.expected.reason, program.name)
            } else {
              assert.fail(`${program.name} expected an unavailable entry`)
            }
            break
        }
      }
    }),
  // The corpus replays every pinned program; it outgrew the default 60s ceiling on CI hosts.
  300_000,
)

it.effect('traces the identity program in order with bound and returned values', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`)

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
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

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
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
      ascii(`service Counter {
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

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('executes named function values and stored automatic sections', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let named = identity
  let plusTwo = i32.add(2)
  return named(plusTwo(40))
}`)

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(
      outcome.trace.filter((event) => event._tag === 'CallableConstruct').length,
      2,
    )
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'CallableApply').length, 2)
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

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
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
        regions: fn.regions.map((region) =>
          region._tag !== 'OperationRegion'
            ? region
            : {
                ...region,
                operations: region.operations.flatMap<Mir.Operation>((operation) =>
                  operation._tag === 'ApplyCallable'
                    ? ([operation, operation] as const)
                    : ([operation] as const),
                ),
              },
        ),
      })),
    }

    const outcome = BootstrapEvaluation.evaluate(self.instances, repeated)
    assert.strictEqual(outcome._tag, 'Blocked', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'InvalidCallableReuse')
    assert.strictEqual(outcome.trace.at(-1)?._tag, 'CallableRejected')
  }),
)

it.effect('maps, flatMaps, and taps Effect successes through ordinary callable values', () =>
  Effect.gen(function* () {
    const mapped = yield* evaluateSource(`effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(2) |> Effect.map(i32.add(2)) }`)
    const flatMapped = yield* evaluateSource(`effect fn succeed(value: i32) -> i32 { return value }
effect fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return run succeed(21) |> Effect.flatMap(double) }`)
    const tapped = yield* evaluateSource(`effect fn succeed(value: i32) -> i32 { return value }
effect fn observe(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(42) |> Effect.tap(observe) }`)

    for (const [outcome, expected] of [
      [mapped, 4],
      [flatMapped, 42],
      [tapped, 42],
    ] as const) {
      assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, BigInt(expected))
    }
  }),
)

it.effect('keeps an Effect returned from map nested until a second run', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`effect fn succeed(value: i32) -> i32 { return value }
effect fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return run (run (succeed(21) |> Effect.map(double))) }`)

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('retains an exclusive mapper environment across repeated mapped Effect runs', () =>
  Effect.gen(function* () {
    const source = `effect fn succeed(value: i32) -> i32 { return value }
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
      `${JSON.stringify({ diagnostics: self.diagnostics, outcome }, Json.bigIntReplacer, 2)}\n${Mir.encode(Analysis.loweredMir(self))}`,
    )
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 43n)
    assert.strictEqual(
      outcome.trace.filter(
        (event) =>
          event._tag === 'CallableApply' &&
          event.mode === 'Exclusive' &&
          event.function.module === 'silk/effects',
      ).length,
      2,
    )
  }),
)

it.effect('drops an unrun mapped Effect owned callback environment exactly once', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`struct Token { value: i32 }
effect fn succeed(value: i32) -> i32 { return value }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let mapped = succeed(40) |> Effect.map(consume(move token))
  drop mapped
  return 42
}`)

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
    if (outcome._tag !== 'Completed') return
    assert.strictEqual(outcome.result.value, 42n)
    assert.strictEqual(outcome.trace.filter((event) => event._tag === 'CallableCleanup').length, 1)
  }),
)

it.effect('catches a typed failure through an automatic effect-handler section', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`struct Problem { code: i32 }
effect fn failNow() -> i32 ! Problem { fail Problem { code: 41 } }
effect fn recover(problem: Problem, adjustment: i32) -> i32 {
  return problem.code + adjustment
}
pub fn main() -> i32 {
  let handled = failNow() |> Effect.catchAll(recover(1))
  return run handled
}`)

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
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

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
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

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
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
    const outcome = yield* evaluateSource(`struct Problem { code: i32 }
effect fn risky(value: i32) -> i32 ! Problem {
  if value == 0 { fail move Problem { code: 41 } }
  return value
}
effect fn recover(problem: Problem) -> i32 { return problem.code + 1 }
pub fn main() -> i32 {
  let recipe = Effect.catchAll(risky(0), recover)
  return run recipe
}`)

    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer, 2))
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

    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'Trap')
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
    const outcome = yield* evaluateSource(`struct Problem { code: i32 }
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
      ascii('pub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }'),
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
      'pub fn main() -> i32 { return 2 + 3 * 4 |> i32.add(1) }',
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

    assert.strictEqual(division._tag, 'Blocked')
    assert.strictEqual(division._tag === 'Blocked' ? division.reason._tag : undefined, 'Trap')
    assert.strictEqual(negation._tag, 'Blocked')
    assert.strictEqual(negation._tag === 'Blocked' ? negation.reason._tag : undefined, 'Trap')
  }),
)

it.effect('evaluates logical union matches with source-ordered guarded fallthrough', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub struct Left { value: i32 }
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
        regions: fn.regions.map((region) =>
          region._tag !== 'OperationRegion'
            ? region
            : {
                ...region,
                operations: region.operations.map((operation) =>
                  operation._tag !== 'Match'
                    ? operation
                    : {
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
                      },
                ),
              },
        ),
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
