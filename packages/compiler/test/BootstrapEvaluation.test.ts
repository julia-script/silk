import { assert, it } from '@effect/vitest'
import * as Analysis from '../src/Analysis.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import { corpus } from './support/corpus.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const evaluateSource = (text: string): BootstrapEvaluation.Outcome =>
  Analysis.evaluate(Analysis.ofSource('memory/evaluation', ascii(text)))

it('reproduces every pinned corpus outcome', () => {
  for (const program of corpus) {
    const outcome = evaluateSource(program.source)
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
            outcome.reason.cycle.map((id) => id.name),
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
})

it('traces the identity program in order with bound and returned values', () => {
  const outcome = evaluateSource(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`)

  assert.strictEqual(outcome._tag, 'Completed')
  assert.deepEqual(
    outcome.trace.map((event) => event._tag),
    ['Entry', 'Call', 'Binding', 'Return', 'Return'],
  )
  const binding = outcome.trace.at(2)
  assert.strictEqual(binding?._tag, 'Binding')
  if (binding?._tag !== 'Binding') return
  assert.strictEqual(binding.value.value, 42)
  assert.strictEqual(binding.parameterOrdinal, 0)
  assert.strictEqual(binding.fromCall, false)
})

it('marks nested-call bindings and orders inner events before outer bindings', () => {
  const outcome = evaluateSource(`pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`)

  assert.strictEqual(outcome._tag, 'Completed')
  const tags = outcome.trace.map((event) => event._tag)
  assert.deepEqual(tags, [
    'Entry',
    'Call',
    'Binding',
    'Return',
    'Call',
    'Binding',
    'Return',
    'Return',
  ])
  const outerBinding = outcome.trace.at(5)
  assert.strictEqual(outerBinding?._tag, 'Binding')
  if (outerBinding?._tag !== 'Binding') return
  assert.strictEqual(outerBinding.fromCall, true)
  const innerCall = outcome.trace.at(1)
  const outerCall = outcome.trace.at(4)
  assert.strictEqual(innerCall?._tag, 'Call')
  assert.strictEqual(outerCall?._tag, 'Call')
  if (innerCall?._tag !== 'Call' || outerCall?._tag !== 'Call') return
  assert.notDeepEqual(innerCall.span, outerCall.span)
})

it('retains the completed prefix before a trap without fabricated events', () => {
  const outcome = evaluateSource(`pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), missing(2)) }`)

  assert.strictEqual(outcome._tag, 'Blocked')
  if (outcome._tag !== 'Blocked') return
  assert.strictEqual(outcome.reason._tag, 'Trap')
  assert.deepEqual(
    outcome.trace.map((event) => event._tag),
    ['Entry'],
  )
})

it('blocks recursive cycles with the closing call span', () => {
  const outcome = evaluateSource('pub fn main() -> I32 { return main() }')

  assert.strictEqual(outcome._tag, 'Blocked')
  if (outcome._tag !== 'Blocked') return
  assert.strictEqual(outcome.reason._tag, 'RecursiveCycle')
  if (outcome.reason._tag !== 'RecursiveCycle') return
  assert.isAbove(outcome.reason.closingCallSpan.end, outcome.reason.closingCallSpan.start)
})

it('evaluates identically across repeated fresh runs', () => {
  const source = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`
  const first = evaluateSource(source)
  const second = evaluateSource(source)

  assert.deepEqual(first, second)
})

it('refuses malformed target-aware MIR before executing any operation', () => {
  const snapshot = Analysis.ofSource(
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
})

it('evaluates operator precedence, prefix operations, and pipeline chains', () => {
  const arithmetic = evaluateSource('pub fn main() -> I32 { return 2 + 3 * 4 |> I32.add(1) }')
  const boolean = evaluateSource('pub fn main() -> I32 { if !(2 * 3 == 7) { return 42 } return 0 }')

  assert.strictEqual(arithmetic._tag, 'Completed')
  assert.strictEqual(arithmetic._tag === 'Completed' ? arithmetic.result.value : undefined, 15)
  assert.strictEqual(boolean._tag, 'Completed')
  assert.strictEqual(boolean._tag === 'Completed' ? boolean.result.value : undefined, 42)
})

it('preserves trapping arithmetic through operator sugar', () => {
  const division = evaluateSource('pub fn main() -> I32 { return 1 / 0 }')
  const negation = evaluateSource('pub fn main() -> I32 { return -(-2147483648) }')

  assert.strictEqual(division._tag, 'Blocked')
  assert.strictEqual(division._tag === 'Blocked' ? division.reason._tag : undefined, 'Trap')
  assert.strictEqual(negation._tag, 'Blocked')
  assert.strictEqual(negation._tag === 'Blocked' ? negation.reason._tag : undefined, 'Trap')
})
