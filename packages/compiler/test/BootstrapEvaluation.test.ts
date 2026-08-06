import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import type * as Mir from '../src/Mir.js'
import { corpus } from './support/corpus.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const evaluateSource = (text: string): Effect.Effect<BootstrapEvaluation.Outcome> =>
  Effect.map(Analysis.ofSource('memory/evaluation', ascii(text)), Analysis.evaluate)

it.effect('reproduces every pinned corpus outcome', () =>
  Effect.gen(function* () {
    for (const program of corpus) {
      const outcome = yield* evaluateSource(program.source)
      switch (program.expected._tag) {
        case 'Completes':
          assert.strictEqual(outcome._tag, 'Completed', program.name)
          if (outcome._tag === 'Completed') {
            assert.strictEqual(outcome.result.value, program.expected.result, program.name)
          }
          break
        case 'Trap':
          assert.strictEqual(outcome._tag, 'Blocked', program.name)
          if (outcome._tag === 'Blocked') {
            assert.strictEqual(outcome.reason._tag, 'Trap', program.name)
          }
          break
        case 'RecursiveCycle':
          assert.strictEqual(outcome._tag, 'Blocked', program.name)
          if (outcome._tag === 'Blocked' && outcome.reason._tag === 'RecursiveCycle') {
            assert.deepEqual(
              outcome.reason.cycle.map((instance) => instance.declaration.name),
              program.expected.cycle,
              program.name,
            )
          } else {
            assert.fail(`${program.name} expected a recursive cycle`)
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
)

it.effect('traces the identity program in order with bound and returned values', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`)

    assert.strictEqual(outcome._tag, 'Completed')
    assert.deepEqual(
      outcome.trace.map((event) => event._tag),
      ['Entry', 'RegionEntry', 'Call', 'Binding', 'RegionEntry', 'Return', 'Return'],
    )
    const binding = outcome.trace.at(3)
    assert.strictEqual(binding?._tag, 'Binding')
    if (binding?._tag !== 'Binding') return
    assert.strictEqual(binding.value._tag, 'I32Value')
    if (binding.value._tag !== 'I32Value') return
    assert.strictEqual(binding.value.value, 42)
    assert.strictEqual(binding.parameterOrdinal, 0)
    assert.strictEqual(binding.fromCall, false)
  }),
)

it.effect('marks nested-call bindings and orders inner events before outer bindings', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`)

    assert.strictEqual(outcome._tag, 'Completed')
    const tags = outcome.trace.map((event) => event._tag)
    assert.deepEqual(tags, [
      'Entry',
      'RegionEntry',
      'Call',
      'Binding',
      'RegionEntry',
      'Return',
      'Call',
      'Binding',
      'RegionEntry',
      'Return',
      'Return',
    ])
    const outerBinding = outcome.trace.at(7)
    assert.strictEqual(outerBinding?._tag, 'Binding')
    if (outerBinding?._tag !== 'Binding') return
    assert.strictEqual(outerBinding.fromCall, true)
    const innerCall = outcome.trace.at(2)
    const outerCall = outcome.trace.at(6)
    assert.strictEqual(innerCall?._tag, 'Call')
    assert.strictEqual(outerCall?._tag, 'Call')
    if (innerCall?._tag !== 'Call' || outerCall?._tag !== 'Call') return
    assert.notDeepEqual(innerCall.span, outerCall.span)
  }),
)

it.effect('retains the completed prefix before a trap without fabricated events', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), missing(2)) }`)

    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'Trap')
    assert.deepEqual(
      outcome.trace.map((event) => event._tag),
      ['Entry', 'RegionEntry'],
    )
  }),
)

it.effect('blocks recursive cycles with the closing call span', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource('pub fn main() -> I32 { return main() }')

    assert.strictEqual(outcome._tag, 'Blocked')
    if (outcome._tag !== 'Blocked') return
    assert.strictEqual(outcome.reason._tag, 'RecursiveCycle')
    if (outcome.reason._tag !== 'RecursiveCycle') return
    assert.isAbove(outcome.reason.closingCallSpan.end, outcome.reason.closingCallSpan.start)
  }),
)

it.effect('evaluates identically across repeated fresh runs', () =>
  Effect.gen(function* () {
    const source = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`
    const first = yield* evaluateSource(source)
    const second = yield* evaluateSource(source)

    assert.deepEqual(first, second)
  }),
)

it.effect('refuses malformed target-aware MIR before executing any operation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'memory/invalid-layout',
      ascii('pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'),
      'wasm32-unknown-unknown',
    )
    const mir = Analysis.mirOf(snapshot)
    if (mir._tag !== 'Available') return assert.fail('expected target-aware MIR')
    const bool = mir.value.layout.entries.find((entry) => entry.type === 'Bool')
    if (bool === undefined) return assert.fail('expected Bool layout')
    const malformed = {
      ...mir.value,
      layout: {
        ...mir.value.layout,
        entries: mir.value.layout.entries.map((entry) =>
          entry.type === 'Bool' ? { ...bool, size: 1 } : entry,
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
      'pub fn main() -> I32 { return 2 + 3 * 4 |> I32.add(1) }',
    )
    const boolean = yield* evaluateSource(
      'pub fn main() -> I32 { if !(2 * 3 == 7) { return 42 } return 0 }',
    )

    assert.strictEqual(arithmetic._tag, 'Completed')
    assert.strictEqual(arithmetic._tag === 'Completed' ? arithmetic.result.value : undefined, 15)
    assert.strictEqual(boolean._tag, 'Completed')
    assert.strictEqual(boolean._tag === 'Completed' ? boolean.result.value : undefined, 42)
  }),
)

it.effect('preserves trapping arithmetic through operator sugar', () =>
  Effect.gen(function* () {
    const division = yield* evaluateSource('pub fn main() -> I32 { return 1 / 0 }')
    const negation = yield* evaluateSource('pub fn main() -> I32 { return -(-2147483648) }')

    assert.strictEqual(division._tag, 'Blocked')
    assert.strictEqual(division._tag === 'Blocked' ? division.reason._tag : undefined, 'Trap')
    assert.strictEqual(negation._tag, 'Blocked')
    assert.strictEqual(negation._tag === 'Blocked' ? negation.reason._tag : undefined, 'Trap')
  }),
)

it.effect('evaluates logical union matches with source-ordered guarded fallthrough', () =>
  Effect.gen(function* () {
    const outcome = yield* evaluateSource(`pub struct Left { value: I32 }
pub struct Right { value: I32 }
pub fn inspect(input: Left | Right) -> I32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => I32.add(answer, 1)
    Right { value } => value
  }
}
pub fn main() -> I32 { return inspect(Left { value: 41 }) }`)

    assert.strictEqual(outcome._tag, 'Completed')
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42)
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
    const outcome = yield* evaluateSource(`pub struct Leaf { value: I32 }
pub struct Box { answer: I32 leaf: Leaf }
pub fn main() -> I32 {
  let box = Box { answer: 42, leaf: Leaf { value: 0 } }
  return match move box { Box { answer, .. } => answer }
}`)

    assert.strictEqual(outcome._tag, 'Completed')
    assert.strictEqual(outcome._tag === 'Completed' ? outcome.result.value : undefined, 42)
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

it.effect('evaluates verified Copy match access without consuming the logical payload', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'memory/copy-match',
      ascii(`struct Token { value: I32 }
fn inspect(input: Token) -> I32 { return match &input { Token { value } => value } }
pub fn main() -> I32 { return inspect(Token { value: 42 }) }`),
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
      _tag: 'I32Value',
      value: 42,
    })
  }),
)
