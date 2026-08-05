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
  ambiguousCallSource,
  beyondSafeIntegerSource,
  damagedTargetBodyCallSource,
  damagedTypeSource,
  duplicateNameSource,
  forwardCallSource,
  i32BoundarySource,
  missingCallCalleeSource,
  missingCallRightParenthesisSource,
  missingIntegerSource,
  missingNameSource,
  missingSecondNameSource,
  mixedFunctionDamageSource,
  mixedResolutionDamageSource,
  overflowSource,
  parserAndSemanticDamageSource,
  selfCallSource,
  threeFunctionSource,
  tripleDuplicateNameSource,
  twoFunctionSource,
  unknownCallSource,
  unknownTypeSource,
  unresolvedTargetTypeCallSource,
  unsupportedCallArgumentSource,
  validCallSource,
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

const integerFact = (
  fact: SemanticAnalysis.FunctionFact,
): SemanticAnalysis.IntegerExpressionFact =>
  fact.returnedExpression._tag === 'Integer'
    ? fact.returnedExpression.integer
    : raise('expected an integer returned expression')

const callFact = (
  fact: SemanticAnalysis.FunctionFact,
): Extract<SemanticAnalysis.ReturnedExpressionFact, { readonly _tag: 'Call' }> =>
  fact.returnedExpression._tag === 'Call'
    ? fact.returnedExpression
    : raise('expected a call returned expression')

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
  const integer = integerFact(fact)

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
    three.functions.map((fact) => {
      const integer = integerFact(fact)
      return integer._tag === 'Available' ? integer.value : undefined
    }),
    [1, 2, 3],
  )
  assert.strictEqual(
    functionAt(two, 0).declaration.syntax.span.start <
      functionAt(two, 1).declaration.syntax.span.start,
    true,
  )
})

it('resolves a call to an earlier declaration and propagates its type', () => {
  const result = analyzeText('fixture://valid-call.silk', validCallSource)
  const answer = functionAt(result, 0)
  const main = functionAt(result, 1)
  const returned = callFact(main)

  assert.strictEqual(answer.returnedExpression._tag, 'Integer')
  assert.strictEqual(integerFact(answer)._tag, 'Available')
  assert.deepEqual(answer.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(returned.syntax.kind, 'CallExpression')
  assert.strictEqual(returned.reference._tag, 'Resolved')
  if (returned.reference._tag !== 'Resolved') return
  assert.strictEqual(returned.reference.spelling, 'answer')
  assert.strictEqual(returned.reference.token, directToken(returned.syntax, 'Identifier'))
  assert.strictEqual(returned.reference.declaration, answer.declaration)
  assert.deepEqual(returned.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(main.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(result.parse.diagnostics, [])
  assert.strictEqual(Object.isFrozen(returned), true)
  assert.strictEqual(Object.isFrozen(returned.reference), true)
})

it('resolves forward and self calls without evaluating them', () => {
  const forward = analyzeText('fixture://forward-call.silk', forwardCallSource)
  const self = analyzeText('fixture://self-call.silk', selfCallSource)
  const forwardCall = callFact(functionAt(forward, 0))
  const selfCall = callFact(functionAt(self, 0))

  assert.strictEqual(forwardCall.reference._tag, 'Resolved')
  assert.strictEqual(selfCall.reference._tag, 'Resolved')
  if (forwardCall.reference._tag !== 'Resolved' || selfCall.reference._tag !== 'Resolved') return
  assert.strictEqual(forwardCall.reference.declaration, functionAt(forward, 1).declaration)
  assert.strictEqual(selfCall.reference.declaration, functionAt(self, 0).declaration)
  assert.deepEqual(forwardCall.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(selfCall.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(functionAt(forward, 0).returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(functionAt(self, 0).returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(forward.diagnostics, [])
  assert.deepEqual(self.diagnostics, [])
})

it('diagnoses an unknown call target at the exact callee span', () => {
  const result = analyzeText('fixture://unknown-call.silk', unknownCallSource)
  const fact = functionAt(result, 0)
  const returned = callFact(fact)

  assert.strictEqual(returned.reference._tag, 'Missing')
  if (returned.reference._tag !== 'Missing') return
  assert.strictEqual(returned.reference.spelling, 'missing')
  assert.deepEqual(returned.type, { _tag: 'Unavailable' })
  assert.deepEqual(fact.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(diagnosticView(result), [
    {
      code: 'SEM0004',
      start: returned.reference.token.span.start,
      end: returned.reference.token.span.end,
      reason: { _tag: 'UnknownFunction', spelling: 'missing' },
    },
  ])
})

it('preserves every ambiguous target without adding a call-site diagnostic', () => {
  const result = analyzeText('fixture://ambiguous-call.silk', ambiguousCallSource)
  const fact = functionAt(result, 2)
  const returned = callFact(fact)

  assert.strictEqual(returned.reference._tag, 'Ambiguous')
  if (returned.reference._tag !== 'Ambiguous') return
  assert.deepEqual(
    returned.reference.declarations.map((declaration) => declaration.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(returned.type, { _tag: 'Unavailable' })
  assert.deepEqual(fact.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0003'],
  )
  assert.strictEqual(Object.isFrozen(returned.reference.declarations), true)
})

it('keeps target type resolution separate from target body compatibility', () => {
  const unresolvedType = analyzeText(
    'fixture://unresolved-target-type-call.silk',
    unresolvedTargetTypeCallSource,
  )
  const damagedBody = analyzeText(
    'fixture://damaged-target-body-call.silk',
    damagedTargetBodyCallSource,
  )
  const unresolvedCall = callFact(functionAt(unresolvedType, 1))
  const damagedBodyCall = callFact(functionAt(damagedBody, 1))

  assert.strictEqual(unresolvedCall.reference._tag, 'Resolved')
  assert.deepEqual(unresolvedCall.type, { _tag: 'Unavailable' })
  assert.deepEqual(functionAt(unresolvedType, 1).returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    unresolvedType.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001'],
  )

  assert.strictEqual(damagedBodyCall.reference._tag, 'Resolved')
  assert.deepEqual(damagedBodyCall.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(functionAt(damagedBody, 0).returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(functionAt(damagedBody, 1).returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(
    damagedBody.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0002'],
  )
})

it('keeps a recovered call callee unavailable and parser-owned', () => {
  const result = analyzeText('fixture://missing-call-callee.silk', missingCallCalleeSource)
  const fact = functionAt(result, 0)
  const returned = fact.returnedExpression

  assert.strictEqual(returned._tag, 'Call')
  if (returned._tag !== 'Call') return
  assert.strictEqual(returned.reference._tag, 'Unavailable')
  if (returned.reference._tag !== 'Unavailable') return
  assert.strictEqual(SyntaxTree.isMissingToken(returned.reference.syntax), true)
  assert.deepEqual(fact.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    result.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('resolves present callees but withholds types through damaged call punctuation', () => {
  const missingParenthesis = analyzeText(
    'fixture://missing-call-right-parenthesis.silk',
    missingCallRightParenthesisSource,
  )
  const unsupportedArgument = analyzeText(
    'fixture://unsupported-call-argument.silk',
    unsupportedCallArgumentSource,
  )

  for (const result of [missingParenthesis, unsupportedArgument]) {
    const fact = functionAt(result, 1)
    const returned = callFact(fact)
    assert.strictEqual(returned.reference._tag, 'Resolved')
    if (returned.reference._tag !== 'Resolved') continue
    assert.strictEqual(returned.reference.spelling, 'answer')
    assert.strictEqual(returned.reference.declaration, functionAt(result, 0).declaration)
    assert.deepEqual(returned.type, { _tag: 'Unavailable' })
    assert.deepEqual(fact.returnCompatibility, { _tag: 'Unavailable' })
    assert.deepEqual(result.diagnostics, [])
  }

  assert.deepEqual(
    missingParenthesis.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assert.deepEqual(
    unsupportedArgument.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
})

it('publishes deterministic resolved and damaged call facts across fresh analyses', () => {
  const first = analyzeText('fixture://deterministic-call.silk', ambiguousCallSource)
  const second = analyzeText('fixture://deterministic-call.silk', ambiguousCallSource)

  assert.deepEqual(first.functions, second.functions)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
})

it('orders type, integer, duplicate, and unknown-call diagnostics by source span', () => {
  const result = analyzeText('fixture://mixed-resolution-damage.silk', mixedResolutionDamageSource)

  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001', 'SEM0002', 'SEM0003', 'SEM0004'],
  )
  assert.deepEqual(result.parse.diagnostics, [])
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
  assert.strictEqual(integerFact(valid)._tag, 'Available')
  assert.deepEqual(valid.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(damaged.declaration.returnType._tag, 'Unresolved')
  assert.strictEqual(integerFact(damaged)._tag, 'OutOfRange')
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
  const boundaryInteger = integerFact(boundary)
  assert.strictEqual(boundaryInteger._tag, 'Available')
  if (boundaryInteger._tag !== 'Available') return
  assert.strictEqual(boundaryInteger.value, 2147483647)
  assert.deepEqual(boundary.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(integerFact(functionAt(overflow, 0))._tag, 'OutOfRange')
  assert.strictEqual(integerFact(functionAt(beyondSafe, 0))._tag, 'OutOfRange')
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
  assert.strictEqual(integerFact(functionAt(missingInteger, 0))._tag, 'Unavailable')
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
  assert.strictEqual(integerFact(functionAt(result, 2))._tag, 'OutOfRange')
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
