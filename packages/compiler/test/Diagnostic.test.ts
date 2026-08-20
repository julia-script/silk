import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Diagnostic from '../src/Diagnostic.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceSpan from '../src/SourceSpan.js'
import { raise } from './support/raise.js'

const source = SourceFile.make('memory://diagnostic.silk', new Uint8Array(64))

const spanAt = (start: number, end: number): SourceSpan.SourceSpan =>
  Option.getOrThrow(SourceSpan.make(source, start, end))

const build = () => {
  const lexical = [Diagnostic.unsupportedBytes(spanAt(10, 12))]
  const parser = [
    Diagnostic.missingToken('RightParenthesis', spanAt(4, 4)),
    Diagnostic.unexpectedTokens(['Invalid'], 'syntax', ['identifier'], spanAt(10, 12)),
  ]
  const semantic = [
    Diagnostic.unknownType('Bogus', spanAt(2, 7)),
    Diagnostic.unknownFunction('missing', spanAt(10, 12)),
  ]
  return { lexical, parser, semantic }
}

it('merges phase collections into one deterministic driver order', () => {
  const { lexical, parser, semantic } = build()
  const merged = Diagnostic.merge(lexical, parser, semantic)

  assert.strictEqual(Object.isFrozen(merged), true)
  assert.deepEqual(
    merged.map((diagnostic) => ({
      phase: diagnostic.phase,
      code: diagnostic.code,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    [
      { phase: 'semantic', code: 'SEM0001', start: 2, end: 7 },
      { phase: 'parser', code: 'PAR0001', start: 4, end: 4 },
      { phase: 'lexical', code: 'LEX0001', start: 10, end: 12 },
      { phase: 'parser', code: 'PAR0002', start: 10, end: 12 },
      { phase: 'semantic', code: 'SEM0004', start: 10, end: 12 },
    ],
  )
})

it('produces an identical merged sequence across repeated fresh constructions', () => {
  const first = build()
  const second = build()

  assert.deepEqual(
    Diagnostic.merge(first.lexical, first.parser, first.semantic),
    Diagnostic.merge(second.lexical, second.parser, second.semantic),
  )
})

it('orders same-span same-phase diagnostics by stable code', () => {
  const span = spanAt(3, 5)
  const merged = Diagnostic.merge([
    Diagnostic.unexpectedTokens(['Invalid'], 'syntax', ['identifier'], span),
    Diagnostic.missingToken('Identifier', span),
  ])

  assert.deepEqual(
    merged.map((diagnostic) => diagnostic.code),
    ['PAR0001', 'PAR0002'],
  )
})

it('describes reserved template syntax with a stable parser diagnostic', () => {
  const diagnostic = Diagnostic.reservedTemplateSyntax(spanAt(8, 18))

  assert.deepEqual(
    {
      phase: diagnostic.phase,
      code: diagnostic.code,
      reason: diagnostic.reason,
    },
    {
      phase: 'parser',
      code: 'PAR0003',
      reason: { _tag: 'ReservedTemplateSyntax' },
    },
  )
})

it('publishes stable lexical identities for modifier and delimiter failures', () => {
  const span = spanAt(8, 24)
  const diagnostics = [
    Diagnostic.unknownLiteralModifier('future', span),
    Diagnostic.unterminatedStaticLiteral('b', 3, span),
  ]
  assert.deepEqual(
    diagnostics.map(({ phase, code, reason }) => ({ phase, code, reason })),
    [
      {
        phase: 'lexical',
        code: 'LEX0002',
        reason: { _tag: 'UnknownLiteralModifier', modifier: 'future' },
      },
      {
        phase: 'lexical',
        code: 'LEX0003',
        reason: {
          _tag: 'UnterminatedStaticLiteral',
          modifier: 'b',
          delimiter: '"',
          delimiterWidth: 3,
        },
      },
    ],
  )
  assert.deepEqual(
    Diagnostic.merge(diagnostics).map((diagnostic) => diagnostic.code),
    ['LEX0002', 'LEX0003'],
  )
})

it('describes missing tokens with source-language spelling', () => {
  const keyword = Diagnostic.missingToken('ReturnKeyword', spanAt(4, 4))
  const punctuation = Diagnostic.missingToken('Equals', spanAt(8, 8))

  assert.deepEqual(
    [keyword, punctuation].map((diagnostic) => ({
      message: diagnostic.message,
      reason: diagnostic.reason,
    })),
    [
      {
        message: 'Expected `return`',
        reason: { _tag: 'MissingToken', expected: 'ReturnKeyword' },
      },
      { message: 'Expected `=`', reason: { _tag: 'MissingToken', expected: 'Equals' } },
    ],
  )
})

it('describes unexpected syntax with the encountered token and parser context', () => {
  const diagnostic = Diagnostic.unexpectedTokens(
    ['Semicolon'],
    'statement',
    ['`let`', '`return`', 'an expression', '`}`'],
    spanAt(4, 5),
  )

  assert.deepEqual(
    {
      message: diagnostic.message,
      reason: diagnostic.reason,
      notes: diagnostic.notes,
    },
    {
      message: 'Unexpected `;` while parsing a statement',
      reason: {
        _tag: 'UnexpectedTokens',
        unexpected: ['Semicolon'],
        context: 'statement',
        expected: ['`let`', '`return`', 'an expression', '`}`'],
      },
      notes: ['Expected one of: `let`, `return`, an expression, `}`'],
    },
  )
})

it('describes return contract failures with stable semantic diagnostics', () => {
  const missing = Diagnostic.missingReturn('i32', spanAt(12, 12))
  const mismatch = Diagnostic.returnTypeMismatch('i32', 'Effect<i32>', spanAt(20, 31))

  assert.deepEqual(
    [missing, mismatch].map((diagnostic) => ({
      phase: diagnostic.phase,
      code: diagnostic.code,
      reason: diagnostic.reason,
    })),
    [
      {
        phase: 'semantic',
        code: 'SEM0130',
        reason: { _tag: 'MissingReturn', expected: 'i32' },
      },
      {
        phase: 'semantic',
        code: 'SEM0129',
        reason: { _tag: 'ReturnTypeMismatch', expected: 'i32', actual: 'Effect<i32>' },
      },
    ],
  )
})

it('assigns identity ordinals among equal phase, code, and span', () => {
  const span = spanAt(0, 1)
  const equal = Diagnostic.unknownFunction('twice', span)
  const other = Diagnostic.unknownType('Other', spanAt(2, 3))
  const identities = Diagnostic.identify([equal, other, equal])
  const first = identities.at(0) ?? raise('expected first diagnostic identity')
  const repeated = identities.at(2) ?? raise('expected repeated diagnostic identity')

  assert.deepEqual(
    identities.map((identity) => identity.ordinal),
    [0, 0, 1],
  )
  assert.strictEqual(Diagnostic.identityEquals(first, Diagnostic.identity(equal)), true)
  assert.strictEqual(Diagnostic.identityEquals(first, repeated), false)
  assert.isBelow(Diagnostic.compareIdentity(first, repeated), 0)
})

it('merges ownership diagnostics after the semantic phase at the same span', () => {
  const span = spanAt(10, 12)
  const merged = Diagnostic.merge(
    [Diagnostic.useAfterMove('value', spanAt(2, 7), span)],
    [Diagnostic.unknownFunction('missing', span)],
  )

  assert.deepEqual(
    merged.map((diagnostic) => ({ phase: diagnostic.phase, code: diagnostic.code })),
    [
      { phase: 'ownership', code: 'OWN0001' },
      { phase: 'semantic', code: 'SEM0004' },
    ],
  )
  const ownership = merged.at(0)
  assert.strictEqual(ownership?.relatedSpans?.at(0)?.label, 'moved here')
  assert.strictEqual(ownership?.reason._tag, 'UseAfterMove')
})

it('carries the original declaration span on a rebinding diagnostic', () => {
  const diagnostic = Diagnostic.rebindingName('value', spanAt(2, 7), spanAt(10, 15))

  assert.strictEqual(diagnostic.phase, 'semantic')
  assert.strictEqual(diagnostic.code, 'SEM0008')
  assert.strictEqual(diagnostic.relatedSpans?.at(0)?.label, 'first declared here')
  assert.strictEqual(diagnostic.relatedSpans?.at(0)?.span.start, 2)
})
