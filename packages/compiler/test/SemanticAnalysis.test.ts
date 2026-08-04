import { assert, it } from '@effect/vitest'
import { pipe } from 'effect/Function'
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
  duplicateNameSource,
  i32BoundarySource,
  missingIntegerSource,
  missingNameSource,
  missingSecondNameSource,
  mixedFunctionDamageSource,
  overflowSource,
  parserAndSemanticDamageSource,
  threeFunctionSource,
  tripleDuplicateNameSource,
  twoFunctionSource,
  unknownTypeSource,
} from './fixtures/BootstrapSemanticFixture.js'
import { raise } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parseText = (id: string, source: string): Parser.ParseResult =>
  Parser.parse(Lexer.lex(SourceFile.make(id, ascii(source))))

const analyzeText = (id: string, source: string): SemanticAnalysis.Result =>
  SemanticAnalysis.analyze(parseText(id, source))

const functionAt = (
  result: SemanticAnalysis.Result,
  index: number,
): SemanticAnalysis.FunctionFact =>
  result.functions.at(index) ?? raise(`expected function fact at index ${index}`)

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

it('publishes one immutable function fact with exact accepted provenance', () => {
  const parse = parseText('fixture://semantic-accepted.silk', acceptedSource)
  const result = SemanticAnalysis.analyze(parse)
  const fact = functionAt(result, 0)
  const declaration = fact.declaration
  const name = declaration.name
  const returnType = declaration.returnType
  const integer = fact.integerExpression

  assert.strictEqual(result.parse, parse)
  assert.strictEqual(result.functions.length, 1)
  assert.deepEqual(declaration.id, {
    _tag: 'DeclarationId',
    sourceId: 'fixture://semantic-accepted.silk',
    ordinal: 0,
  })
  assert.strictEqual(declaration.visibility, 'Public')
  assert.strictEqual(declaration.parameterCount, 0)
  assert.strictEqual(name._tag, 'Present')
  if (name._tag !== 'Present') return
  assert.strictEqual(name.spelling, 'main')
  assert.strictEqual(name.token, directToken(declaration.syntax, 'Identifier'))
  assert.strictEqual(returnType._tag, 'Resolved')
  if (returnType._tag !== 'Resolved') return
  assert.strictEqual(returnType.type, 'I32')
  assert.strictEqual(returnType.spelling, 'I32')
  assert.strictEqual(integer._tag, 'Available')
  if (integer._tag !== 'Available') return
  assert.strictEqual(integer.type, 'I32')
  assert.strictEqual(integer.value, 42)
  assert.deepEqual(fact.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(result.diagnostics, [])

  const directLookup = SemanticAnalysis.declarationByName(result, 'main')
  const pipedLookup = pipe(result, SemanticAnalysis.declarationByName('main'))
  assert.strictEqual(directLookup._tag, 'Resolved')
  assert.strictEqual(pipedLookup._tag, 'Resolved')
  if (directLookup._tag !== 'Resolved' || pipedLookup._tag !== 'Resolved') return
  assert.strictEqual(directLookup.declaration, declaration)
  assert.strictEqual(pipedLookup.declaration, declaration)
  assert.deepEqual(SemanticAnalysis.declarationByName(result, 'other'), {
    _tag: 'Missing',
    spelling: 'other',
  })
  assert.strictEqual(Object.isFrozen(result), true)
  assert.strictEqual(Object.isFrozen(result.functions), true)
  assert.strictEqual(Object.isFrozen(fact), true)
  assert.strictEqual(Object.isFrozen(declaration), true)
  assert.strictEqual(Object.isFrozen(declaration.id), true)
  assert.strictEqual(Object.isFrozen(directLookup), true)
  assert.strictEqual(Object.isFrozen(result.diagnostics), true)
})

it('collects two and three declarations with deterministic source-order identities', () => {
  const two = analyzeText('fixture://two-functions.silk', twoFunctionSource)
  const three = analyzeText('fixture://three-functions.silk', threeFunctionSource)

  assert.deepEqual(
    two.functions.map((fact) => fact.declaration.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    three.functions.map((fact) => fact.declaration.id.ordinal),
    [0, 1, 2],
  )
  assert.deepEqual(
    three.functions.map((fact) =>
      fact.declaration.name._tag === 'Present' ? fact.declaration.name.spelling : 'Unavailable',
    ),
    ['one', 'two', 'three'],
  )
  assert.deepEqual(
    three.functions.map((fact) =>
      fact.integerExpression._tag === 'Available' ? fact.integerExpression.value : undefined,
    ),
    [1, 2, 3],
  )
  assert.strictEqual(
    functionAt(two, 0).declaration.syntax.span.start <
      functionAt(two, 1).declaration.syntax.span.start,
    true,
  )
})

it('uses deterministic source-local identities across fresh results', () => {
  const first = analyzeText('fixture://same.silk', twoFunctionSource)
  const second = analyzeText('fixture://same.silk', twoFunctionSource)
  const other = analyzeText('fixture://other.silk', twoFunctionSource)

  assert.deepEqual(
    first.functions.map((fact) => fact.declaration.id),
    second.functions.map((fact) => fact.declaration.id),
  )
  assert.notDeepEqual(
    first.functions.map((fact) => fact.declaration.id),
    other.functions.map((fact) => fact.declaration.id),
  )
})

it('keeps missing declaration names unavailable and out of lookup', () => {
  const single = analyzeText('fixture://missing-name.silk', missingNameSource)
  const multiple = analyzeText('fixture://missing-second-name.silk', missingSecondNameSource)

  assert.strictEqual(functionAt(single, 0).declaration.name._tag, 'Unavailable')
  assert.strictEqual(functionAt(multiple, 1).declaration.name._tag, 'Unavailable')
  assert.deepEqual(SemanticAnalysis.declarationByName(multiple, ''), {
    _tag: 'Missing',
    spelling: '',
  })
  assert.deepEqual(single.diagnostics, [])
  assert.deepEqual(multiple.diagnostics, [])
  assert.deepEqual(
    multiple.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
})

it('resolves unique names and reports every duplicate as ambiguous', () => {
  const unique = analyzeText('fixture://unique.silk', twoFunctionSource)
  const duplicate = analyzeText('fixture://duplicate.silk', duplicateNameSource)
  const answer = SemanticAnalysis.declarationByName(unique, 'answer')
  const main = SemanticAnalysis.declarationByName(unique, 'main')
  const same = SemanticAnalysis.declarationByName(duplicate, 'same')

  assert.strictEqual(answer._tag, 'Resolved')
  assert.strictEqual(main._tag, 'Resolved')
  assert.strictEqual(same._tag, 'Ambiguous')
  if (answer._tag !== 'Resolved' || main._tag !== 'Resolved' || same._tag !== 'Ambiguous') return
  assert.strictEqual(answer.declaration, functionAt(unique, 0).declaration)
  assert.strictEqual(main.declaration, functionAt(unique, 1).declaration)
  assert.deepEqual(
    same.declarations,
    duplicate.functions.map((fact) => fact.declaration),
  )
  assert.strictEqual(Object.isFrozen(same), true)
  assert.strictEqual(Object.isFrozen(same.declarations), true)
})

it('diagnoses later duplicate names at their exact spans with original provenance', () => {
  const result = analyzeText('fixture://duplicate.silk', duplicateNameSource)
  const firstName = functionAt(result, 0).declaration.name
  const secondName = functionAt(result, 1).declaration.name

  assert.strictEqual(firstName._tag, 'Present')
  assert.strictEqual(secondName._tag, 'Present')
  if (firstName._tag !== 'Present' || secondName._tag !== 'Present') return
  assert.strictEqual(result.diagnostics.length, 1)
  const diagnostic = result.diagnostics.at(0) ?? raise('expected duplicate diagnostic')
  assert.strictEqual(diagnostic.code, 'SEM0003')
  assert.strictEqual(diagnostic.span, secondName.token.span)
  assert.deepEqual(diagnostic.reason, {
    _tag: 'DuplicateDeclarationName',
    spelling: 'same',
    originalSpan: firstName.token.span,
  })
  assert.strictEqual(Object.isFrozen(diagnostic), true)
  assert.strictEqual(Object.isFrozen(diagnostic.reason), true)
})

it('diagnoses the second and third occurrence of one name', () => {
  const result = analyzeText('fixture://triple-duplicate.silk', tripleDuplicateNameSource)
  const lookup = SemanticAnalysis.declarationByName(result, 'same')

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0003', 'SEM0003'],
  )
  assert.strictEqual(lookup._tag, 'Ambiguous')
  if (lookup._tag !== 'Ambiguous') return
  assert.strictEqual(lookup.declarations.length, 3)
  assert.deepEqual(
    lookup.declarations.map((declaration) => declaration.id.ordinal),
    [0, 1, 2],
  )
})

it('analyzes return types, integers, and compatibility independently', () => {
  const result = analyzeText('fixture://mixed-function-damage.silk', mixedFunctionDamageSource)
  const valid = functionAt(result, 0)
  const damaged = functionAt(result, 1)

  assert.strictEqual(valid.declaration.returnType._tag, 'Resolved')
  assert.strictEqual(valid.integerExpression._tag, 'Available')
  assert.deepEqual(valid.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(damaged.declaration.returnType._tag, 'Unresolved')
  assert.strictEqual(damaged.integerExpression._tag, 'OutOfRange')
  assert.deepEqual(damaged.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001', 'SEM0002'],
  )
})

it('preserves existing type and integer edge behavior per function', () => {
  const unknown = functionAt(analyzeText('fixture://unknown-type.silk', unknownTypeSource), 0)
  const damaged = analyzeText('fixture://damaged-type.silk', damagedTypeSource)
  const boundary = functionAt(analyzeText('fixture://i32-boundary.silk', i32BoundarySource), 0)
  const overflow = analyzeText('fixture://i32-overflow.silk', overflowSource)
  const beyondSafe = analyzeText('fixture://beyond-safe.silk', beyondSafeIntegerSource)
  const missingInteger = analyzeText('fixture://missing-integer.silk', missingIntegerSource)

  assert.strictEqual(unknown.declaration.returnType._tag, 'Unresolved')
  assert.deepEqual(unknown.returnCompatibility, { _tag: 'Unavailable' })
  assert.strictEqual(functionAt(damaged, 0).declaration.returnType._tag, 'Unavailable')
  assert.deepEqual(damaged.diagnostics, [])
  assert.deepEqual(
    damaged.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002', 'PAR0001'],
  )
  assert.strictEqual(boundary.integerExpression._tag, 'Available')
  if (boundary.integerExpression._tag !== 'Available') return
  assert.strictEqual(boundary.integerExpression.value, 2147483647)
  assert.deepEqual(boundary.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(functionAt(overflow, 0).integerExpression._tag, 'OutOfRange')
  assert.strictEqual(functionAt(beyondSafe, 0).integerExpression._tag, 'OutOfRange')
  assert.deepEqual(
    diagnosticView(beyondSafe).map((diagnostic) => diagnostic.reason),
    [
      {
        _tag: 'IntegerOutOfRange',
        spelling: '90071992547409931234567890',
        maximum: 2147483647,
      },
    ],
  )
  assert.strictEqual(functionAt(missingInteger, 0).integerExpression._tag, 'Unavailable')
  assert.deepEqual(missingInteger.diagnostics, [])
})

it('keeps parser and semantic diagnostics in their owning ordered collections', () => {
  const result = analyzeText(
    'fixture://parser-and-semantic-damage.silk',
    parserAndSemanticDamageSource,
  )

  assert.deepEqual(
    result.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001', 'SEM0003', 'SEM0002'],
  )
  assert.strictEqual(functionAt(result, 1).declaration.name._tag, 'Unavailable')
  assert.strictEqual(functionAt(result, 1).declaration.returnType._tag, 'Unresolved')
  assert.strictEqual(functionAt(result, 2).integerExpression._tag, 'OutOfRange')
})

it('is deterministic across repeated fresh multi-function results', () => {
  const first = analyzeText('fixture://semantic-deterministic.silk', parserAndSemanticDamageSource)
  const second = analyzeText('fixture://semantic-deterministic.silk', parserAndSemanticDamageSource)

  assert.deepEqual(first.functions, second.functions)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
  assert.deepEqual(
    SemanticAnalysis.declarationByName(first, 'same'),
    SemanticAnalysis.declarationByName(second, 'same'),
  )
})
