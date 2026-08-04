import { assert, it } from '@effect/vitest'
import { pipe } from 'effect/Function'
import * as Option from 'effect/Option'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SemanticAnalysis from '../src/SemanticAnalysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import type * as Token from '../src/Token.js'
import {
  acceptedSource,
  beyondSafeIntegerSource,
  damagedTypeSource,
  i32BoundarySource,
  missingIntegerSource,
  missingNameSource,
  mixedDamageSource,
  overflowSource,
  unknownTypeSource,
} from './fixtures/BootstrapSemanticFixture.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parseText = (id: string, source: string): Parser.ParseResult =>
  Parser.parse(Lexer.lex(SourceFile.make(id, ascii(source))))

const analyzeText = (id: string, source: string): SemanticAnalysis.Result =>
  SemanticAnalysis.analyze(parseText(id, source))

const diagnosticView = (result: SemanticAnalysis.Result) =>
  result.diagnostics.map((diagnostic) => ({
    code: diagnostic.code,
    start: diagnostic.span.start,
    end: diagnostic.span.end,
    reason: diagnostic.reason,
  }))

const directToken = (node: SyntaxTree.Node, kind: Token.TokenKind): Token.Token | undefined =>
  node.children.find(
    (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === kind,
  )

it('publishes the accepted declaration, I32 value, compatibility, and exact provenance', () => {
  const parse = parseText('fixture://semantic-accepted.silk', acceptedSource)
  const result = SemanticAnalysis.analyze(parse)
  const name = result.declaration.name
  const returnType = result.declaration.returnType
  const integer = result.integerExpression

  assert.strictEqual(result.parse, parse)
  assert.deepEqual(result.declaration.id, {
    _tag: 'DeclarationId',
    sourceId: 'fixture://semantic-accepted.silk',
    ordinal: 0,
  })
  assert.strictEqual(result.declaration.visibility, 'Public')
  assert.strictEqual(result.declaration.parameterCount, 0)
  assert.strictEqual(name._tag, 'Present')
  if (name._tag !== 'Present') return
  assert.strictEqual(name.spelling, 'main')
  assert.strictEqual(name.token, directToken(result.declaration.syntax, 'Identifier'))
  assert.strictEqual(returnType._tag, 'Resolved')
  if (returnType._tag !== 'Resolved') return
  assert.strictEqual(returnType.type, 'I32')
  assert.strictEqual(returnType.spelling, 'I32')
  assert.strictEqual(integer._tag, 'Available')
  if (integer._tag !== 'Available') return
  assert.strictEqual(integer.type, 'I32')
  assert.strictEqual(integer.value, 42)
  assert.deepEqual(result.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(
    Option.getOrThrow(SemanticAnalysis.declarationByName(result, 'main')),
    result.declaration,
  )
  assert.strictEqual(
    Option.getOrThrow(pipe(result, SemanticAnalysis.declarationByName('main'))),
    result.declaration,
  )
  assert.strictEqual(Option.isNone(SemanticAnalysis.declarationByName(result, 'other')), true)
  assert.strictEqual(Object.isFrozen(result), true)
  assert.strictEqual(Object.isFrozen(result.declaration), true)
  assert.strictEqual(Object.isFrozen(result.declaration.id), true)
  assert.strictEqual(Object.isFrozen(result.diagnostics), true)
})

it('uses deterministic source-local declaration identity', () => {
  const first = analyzeText('fixture://same.silk', acceptedSource)
  const second = analyzeText('fixture://same.silk', acceptedSource)
  const other = analyzeText('fixture://other.silk', acceptedSource)

  assert.deepEqual(first.declaration.id, second.declaration.id)
  assert.notDeepEqual(first.declaration.id, other.declaration.id)
})

it('keeps a missing declaration name unavailable without semantic duplication', () => {
  const result = analyzeText('fixture://missing-name.silk', missingNameSource)

  assert.strictEqual(result.declaration.name._tag, 'Unavailable')
  assert.strictEqual(Option.isNone(SemanticAnalysis.declarationByName(result, 'main')), true)
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(
    result.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
})

it('diagnoses a present unknown type and withholds return compatibility', () => {
  const result = analyzeText('fixture://unknown-type.silk', unknownTypeSource)

  assert.strictEqual(result.declaration.returnType._tag, 'Unresolved')
  assert.deepEqual(result.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(diagnosticView(result), [
    {
      code: 'SEM0001',
      start: 17,
      end: 24,
      reason: { _tag: 'UnknownType', spelling: 'Mystery' },
    },
  ])
  assert.deepEqual(result.parse.diagnostics, [])
})

it('keeps a syntax-damaged return type unavailable under parser diagnostic ownership', () => {
  const result = analyzeText('fixture://damaged-type.silk', damagedTypeSource)

  assert.strictEqual(result.declaration.returnType._tag, 'Unavailable')
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(
    result.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002', 'PAR0001'],
  )
})

it('accepts the I32 boundary and rejects overflow without host-number precision loss', () => {
  const boundary = analyzeText('fixture://i32-boundary.silk', i32BoundarySource)
  const overflow = analyzeText('fixture://i32-overflow.silk', overflowSource)
  const beyondSafe = analyzeText('fixture://beyond-safe.silk', beyondSafeIntegerSource)

  assert.strictEqual(boundary.integerExpression._tag, 'Available')
  if (boundary.integerExpression._tag !== 'Available') return
  assert.strictEqual(boundary.integerExpression.value, 2147483647)
  assert.deepEqual(boundary.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(overflow.integerExpression._tag, 'OutOfRange')
  assert.deepEqual(
    overflow.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0002'],
  )
  assert.deepEqual(overflow.returnCompatibility, { _tag: 'Unavailable' })
  assert.strictEqual(beyondSafe.integerExpression._tag, 'OutOfRange')
  assert.deepEqual(
    beyondSafe.diagnostics.map((diagnostic) => diagnostic.reason),
    [
      {
        _tag: 'IntegerOutOfRange',
        spelling: '90071992547409931234567890',
        maximum: 2147483647,
      },
    ],
  )
})

it('keeps a missing integer unavailable without a semantic diagnostic', () => {
  const result = analyzeText('fixture://missing-integer.silk', missingIntegerSource)

  assert.strictEqual(result.integerExpression._tag, 'Unavailable')
  assert.deepEqual(result.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(
    result.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
})

it('keeps parser and semantic diagnostics in their owning collections', () => {
  const result = analyzeText('fixture://mixed-damage.silk', mixedDamageSource)

  assert.deepEqual(
    result.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001'],
  )
  assert.strictEqual(result.declaration.name._tag, 'Unavailable')
  assert.strictEqual(result.declaration.returnType._tag, 'Unresolved')
})

it('is deterministic across repeated fresh semantic results', () => {
  const first = analyzeText('fixture://semantic-deterministic.silk', mixedDamageSource)
  const second = analyzeText('fixture://semantic-deterministic.silk', mixedDamageSource)

  assert.deepEqual(first.declaration.id, second.declaration.id)
  assert.deepEqual(first.declaration.name, second.declaration.name)
  assert.deepEqual(first.declaration.returnType, second.declaration.returnType)
  assert.deepEqual(first.integerExpression, second.integerExpression)
  assert.deepEqual(first.returnCompatibility, second.returnCompatibility)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
})
