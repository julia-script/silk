import { assert, it } from '@effect/vitest'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SemanticAnalysis from '../src/SemanticAnalysis.js'
import * as SourceFile from '../src/SourceFile.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const analyze = (id: string, source: string): SemanticAnalysis.Result =>
  SemanticAnalysis.analyze(Parser.parse(Lexer.lex(SourceFile.make(id, ascii(source)))))

const evaluate = (id: string, source: string): BootstrapEvaluation.Outcome =>
  BootstrapEvaluation.evaluate(analyze(id, source))

const identitySource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`

it('evaluates a literal main to an exact immutable I32 result', () => {
  const outcome = evaluate(
    'fixture://evaluation-literal.silk',
    'pub fn main() -> I32 { return 42 }',
  )

  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.deepEqual(outcome.result, { _tag: 'I32Value', value: 42 })
  assert.deepEqual(
    outcome.trace.map((event) => event._tag),
    ['Entry', 'Return'],
  )
  assert.strictEqual(outcome.trace[0]?.span.sourceId, 'fixture://evaluation-literal.silk')
  assert.strictEqual(Object.isFrozen(outcome), true)
  assert.strictEqual(Object.isFrozen(outcome.result), true)
  assert.strictEqual(Object.isFrozen(outcome.trace), true)
})

it('evaluates identity with deterministic call, binding, read, and return trace order', () => {
  const outcome = evaluate('fixture://evaluation-identity.silk', identitySource)

  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.strictEqual(outcome.result.value, 42)
  assert.deepEqual(
    outcome.trace.map((event) => event._tag),
    ['Entry', 'Call', 'Binding', 'ParameterRead', 'Return', 'Return'],
  )
  const binding = outcome.trace.find((event) => event._tag === 'Binding')
  const read = outcome.trace.find((event) => event._tag === 'ParameterRead')
  assert.strictEqual(binding?._tag, 'Binding')
  assert.strictEqual(read?._tag, 'ParameterRead')
  if (binding?._tag !== 'Binding' || read?._tag !== 'ParameterRead') return
  assert.strictEqual(binding.argument.id.ordinal, 0)
  assert.strictEqual(binding.parameter.id.ordinal, 0)
  assert.strictEqual(binding.value.value, 42)
  assert.deepEqual(read.parameter.id, binding.parameter.id)
  assert.strictEqual(read.value.value, 42)
})

it('binds two arguments positionally and returns the second parameter', () => {
  const outcome = evaluate(
    'fixture://evaluation-second.silk',
    `pub fn second(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return second(10, 42) }`,
  )

  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.strictEqual(outcome.result.value, 42)
  assert.deepEqual(
    outcome.trace
      .filter((event) => event._tag === 'Binding')
      .map((event) => [event.argument.id.ordinal, event.parameter.id.ordinal, event.value.value]),
    [
      [0, 0, 10],
      [1, 1, 42],
    ],
  )
})

it('blocks at a reachable analyzed nested expression with exact provenance', () => {
  const outcome = evaluate(
    'fixture://evaluation-nested.silk',
    `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  )

  assert.strictEqual(outcome._tag, 'Blocked')
  if (outcome._tag !== 'Blocked') return
  assert.strictEqual(outcome.reason._tag, 'UnsupportedNestedExpression')
  if (outcome.reason._tag !== 'UnsupportedNestedExpression') return
  assert.strictEqual(outcome.reason.expression._tag, 'Call')
  assert.strictEqual(outcome.reason.argument.expression, outcome.reason.expression)
  assert.deepEqual(outcome.reason.expressionSpan, outcome.reason.expression.syntax.span)
  assert.deepEqual(outcome.reason.argument.id.callSpan, outcome.reason.call.syntax.span)
  assert.deepEqual(
    outcome.trace.map((event) => event._tag),
    ['Entry', 'Call'],
  )
})

it('ignores nested expressions outside the reachable declaration graph', () => {
  const outcome = evaluate(
    'fixture://evaluation-unreachable-nested.silk',
    `pub fn identity(value: I32) -> I32 { return value }
pub fn unused() -> I32 { return identity(identity(1)) }
pub fn main() -> I32 { return 42 }`,
  )

  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.strictEqual(outcome.result.value, 42)
})

it('repeats the transitional nested-expression outcome deterministically', () => {
  const source = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`
  const first = evaluate('fixture://evaluation-repeat-nested.silk', source)
  const second = evaluate('fixture://evaluation-repeat-nested.silk', source)

  assert.deepEqual(first, second)
})

it('returns precise closed entry reasons', () => {
  const missing = evaluate(
    'fixture://evaluation-missing-entry.silk',
    'pub fn answer() -> I32 { return 42 }',
  )
  const ambiguous = evaluate(
    'fixture://evaluation-ambiguous-entry.silk',
    `pub fn main() -> I32 { return 1 }
pub fn main() -> I32 { return 2 }`,
  )
  const parameterized = evaluate(
    'fixture://evaluation-parameterized-entry.silk',
    'pub fn main(value: I32) -> I32 { return value }',
  )
  const unavailableType = evaluate(
    'fixture://evaluation-entry-type.silk',
    'pub fn main() -> Mystery { return 42 }',
  )

  assert.strictEqual(missing._tag, 'Blocked')
  assert.strictEqual(ambiguous._tag, 'Blocked')
  assert.strictEqual(parameterized._tag, 'Blocked')
  assert.strictEqual(unavailableType._tag, 'Blocked')
  if (
    missing._tag !== 'Blocked' ||
    ambiguous._tag !== 'Blocked' ||
    parameterized._tag !== 'Blocked' ||
    unavailableType._tag !== 'Blocked'
  ) {
    return
  }
  assert.strictEqual(missing.reason._tag, 'MissingEntry')
  assert.strictEqual(ambiguous.reason._tag, 'AmbiguousEntry')
  assert.strictEqual(parameterized.reason._tag, 'ParameterizedEntry')
  assert.strictEqual(unavailableType.reason._tag, 'UnavailableEntryType')
  if (ambiguous.reason._tag === 'AmbiguousEntry') {
    assert.deepEqual(
      ambiguous.reason.lookup.declarations.map((declaration) => declaration.id.ordinal),
      [0, 1],
    )
  }
  if (parameterized.reason._tag === 'ParameterizedEntry') {
    assert.strictEqual(parameterized.reason.actualCount, 1)
  }
})

it('blocks wrong arity at the reachable call with exact counts', () => {
  const outcome = evaluate(
    'fixture://evaluation-arity.silk',
    `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity() }`,
  )

  assert.strictEqual(outcome._tag, 'Blocked')
  if (outcome._tag !== 'Blocked') return
  assert.strictEqual(outcome.reason._tag, 'ArityMismatch')
  if (outcome.reason._tag !== 'ArityMismatch') return
  assert.strictEqual(outcome.reason.expectedCount, 1)
  assert.strictEqual(outcome.reason.actualCount, 0)
  assert.deepEqual(
    outcome.trace.map((event) => event._tag),
    ['Entry'],
  )
})

it('retains a successful prefix when a reachable parameter reference is unavailable', () => {
  const outcome = evaluate(
    'fixture://evaluation-unavailable.silk',
    `pub fn identity(value: I32) -> I32 { return missing }
pub fn main() -> I32 { return identity(42) }`,
  )

  assert.strictEqual(outcome._tag, 'Blocked')
  if (outcome._tag !== 'Blocked') return
  assert.strictEqual(outcome.reason._tag, 'MissingParameterReference')
  assert.deepEqual(
    outcome.trace.map((event) => event._tag),
    ['Entry', 'Call', 'Binding'],
  )
})

it('ignores unavailable facts outside the reachable declaration graph', () => {
  const outcome = evaluate(
    'fixture://evaluation-unreachable.silk',
    `pub fn main() -> I32 { return 42 }
pub fn broken() -> Mystery { return missing }`,
  )

  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.strictEqual(outcome.result.value, 42)
})

it('bounds a direct recursive cycle at its closing call site', () => {
  const outcome = evaluate(
    'fixture://evaluation-direct-cycle.silk',
    'pub fn main() -> I32 { return main() }',
  )

  assert.strictEqual(outcome._tag, 'Blocked')
  if (outcome._tag !== 'Blocked') return
  assert.strictEqual(outcome.reason._tag, 'RecursiveCycle')
  if (outcome.reason._tag !== 'RecursiveCycle') return
  assert.deepEqual(
    outcome.reason.cycle.map((declaration) => declaration.id.ordinal),
    [0, 0],
  )
  assert.deepEqual(
    outcome.trace.map((event) => event._tag),
    ['Entry', 'Call'],
  )
  assert.deepEqual(outcome.reason.closingCallSpan, outcome.reason.closingCall.syntax.span)
})

it('reports mutual recursion deterministically without overflowing', () => {
  const source = `pub fn main() -> I32 { return other() }
pub fn other() -> I32 { return main() }`
  const first = evaluate('fixture://evaluation-mutual-cycle.silk', source)
  const second = evaluate('fixture://evaluation-mutual-cycle.silk', source)

  assert.deepEqual(first, second)
  assert.strictEqual(first._tag, 'Blocked')
  if (first._tag !== 'Blocked') return
  assert.strictEqual(first.reason._tag, 'RecursiveCycle')
  if (first.reason._tag !== 'RecursiveCycle') return
  assert.deepEqual(
    first.reason.cycle.map((declaration) =>
      declaration.name._tag === 'Present' ? declaration.name.spelling : 'Unavailable',
    ),
    ['main', 'other', 'main'],
  )
  assert.deepEqual(
    first.trace.map((event) => event._tag),
    ['Entry', 'Call', 'Call'],
  )
})
