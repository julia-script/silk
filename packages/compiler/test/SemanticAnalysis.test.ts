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
  crossFunctionParameterSource,
  damagedIdentifierSource,
  damagedNestedCallSource,
  damagedTargetBodyCallSource,
  damagedTypeSource,
  duplicateNameSource,
  duplicateParameterSource,
  forwardCallSource,
  i32BoundarySource,
  identityCallSource,
  incompatibleNestedCallSource,
  missingCallCalleeSource,
  missingCallRightParenthesisSource,
  missingIntegerSource,
  missingNameSource,
  missingParameterNameSource,
  missingParameterTypeSource,
  missingSecondNameSource,
  mixedFunctionDamageSource,
  mixedResolutionDamageSource,
  nestedCallSource,
  nestedSiblingCallsSource,
  overflowSource,
  parserAndSemanticDamageSource,
  recoveredArgumentSource,
  sameParameterNamesSource,
  selfCallSource,
  threeFunctionSource,
  tooFewArgumentsSource,
  tooManyArgumentsSource,
  tripleDuplicateNameSource,
  tripleDuplicateParameterSource,
  twoArgumentCallSource,
  twoFunctionSource,
  twoParameterSource,
  unavailableArgumentContractSource,
  unavailableParameterContractSource,
  unknownCallSource,
  unknownParameterReferenceSource,
  unknownParameterTypeSource,
  unknownTypeSource,
  unresolvedNestedCallSource,
  unresolvedTargetTypeCallSource,
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
): Extract<SemanticAnalysis.ExpressionFact, { readonly _tag: 'Call' }> =>
  fact.returnedExpression._tag === 'Call'
    ? fact.returnedExpression
    : raise('expected a call returned expression')

const identifierFact = (
  fact: SemanticAnalysis.FunctionFact,
): Extract<SemanticAnalysis.ExpressionFact, { readonly _tag: 'Identifier' }> =>
  fact.returnedExpression._tag === 'Identifier'
    ? fact.returnedExpression
    : raise('expected an identifier returned expression')

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

it('collects typed parameters and resolves returned identifiers', () => {
  const identity = analyzeText('fixture://identity-parameters.silk', identityCallSource)
  const multiple = analyzeText('fixture://two-parameters.silk', twoParameterSource)
  const identityFunction = functionAt(identity, 0)
  const main = functionAt(identity, 1)
  const returnedIdentifier = identifierFact(identityFunction)
  const returnedCall = callFact(main)

  assert.strictEqual(identityFunction.declaration.parameterCount, 1)
  assert.strictEqual(functionAt(multiple, 0).declaration.parameterCount, 2)
  assert.strictEqual(returnedIdentifier.reference._tag, 'Resolved')
  assert.deepEqual(returnedIdentifier.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(identityFunction.returnCompatibility, { _tag: 'Compatible' })
  assert.strictEqual(returnedCall.reference._tag, 'Resolved')
  assert.deepEqual(returnedCall.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(main.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(identity.diagnostics, [])
  assert.deepEqual(identity.parse.diagnostics, [])
})

it('publishes ordered function-local parameter identities, types, and lookup', () => {
  const result = analyzeText('fixture://same-parameter-names.silk', sameParameterNamesSource)
  const first = functionAt(result, 0).declaration
  const second = functionAt(result, 1).declaration
  const firstParameter = first.parameters.at(0) ?? raise('expected first parameter')
  const secondParameter = second.parameters.at(0) ?? raise('expected second parameter')
  const directLookup = SemanticAnalysis.parameterByName(first, 'value')
  const pipedLookup = pipe(second, SemanticAnalysis.parameterByName('value'))

  assert.deepEqual(firstParameter.id, {
    _tag: 'ParameterId',
    function: first.id,
    ordinal: 0,
  })
  assert.deepEqual(secondParameter.id, {
    _tag: 'ParameterId',
    function: second.id,
    ordinal: 0,
  })
  assert.notDeepEqual(firstParameter.id.function, secondParameter.id.function)
  assert.strictEqual(firstParameter.name._tag, 'Present')
  assert.strictEqual(firstParameter.declaredType._tag, 'Resolved')
  assert.strictEqual(firstParameter.syntax.kind, 'ParameterDeclaration')
  assert.strictEqual(directLookup._tag, 'Resolved')
  assert.strictEqual(pipedLookup._tag, 'Resolved')
  if (directLookup._tag !== 'Resolved' || pipedLookup._tag !== 'Resolved') return
  assert.strictEqual(directLookup.parameter, firstParameter)
  assert.strictEqual(pipedLookup.parameter, secondParameter)
  assert.deepEqual(SemanticAnalysis.parameterByName(first, 'missing'), {
    _tag: 'Missing',
    spelling: 'missing',
  })
  assert.strictEqual(Object.isFrozen(first.parameters), true)
  assert.strictEqual(Object.isFrozen(firstParameter), true)
  assert.strictEqual(Object.isFrozen(firstParameter.id), true)
})

it('resolves identifier arguments against the caller parameter collection', () => {
  const result = analyzeText(
    'fixture://identifier-argument.silk',
    `pub fn identity(value: I32) -> I32 { return value }
pub fn forward(value: I32) -> I32 { return identity(value) }`,
  )
  const forward = functionAt(result, 1)
  const call = callFact(forward)
  const argument = call.arguments.at(0) ?? raise('expected argument')
  const expression = argument.expression

  assert.strictEqual(expression._tag, 'Identifier')
  if (expression._tag !== 'Identifier') return
  assert.strictEqual(expression.reference._tag, 'Resolved')
  if (expression.reference._tag !== 'Resolved') return
  assert.strictEqual(expression.reference.parameter, forward.declaration.parameters.at(0))
  assert.deepEqual(expression.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(result.diagnostics, [])
})

it('publishes ordered argument identities, expressions, mappings, and compatible contracts', () => {
  const one = analyzeText('fixture://one-argument-contract.silk', identityCallSource)
  const two = analyzeText('fixture://two-argument-contract.silk', twoArgumentCallSource)
  const oneCall = callFact(functionAt(one, 1))
  const twoCall = callFact(functionAt(two, 1))
  const firstArgument = oneCall.arguments.at(0) ?? raise('expected first argument')

  assert.deepEqual(firstArgument.id, {
    _tag: 'ArgumentId',
    function: functionAt(one, 1).declaration.id,
    callSpan: oneCall.syntax.span,
    ordinal: 0,
  })
  assert.strictEqual(firstArgument.expression._tag, 'Integer')
  if (firstArgument.expression._tag !== 'Integer') return
  assert.strictEqual(firstArgument.expression.integer._tag, 'Available')
  if (firstArgument.expression.integer._tag !== 'Available') return
  assert.strictEqual(firstArgument.expression.integer.value, 42)
  assert.deepEqual(firstArgument.type, { _tag: 'Available', type: 'I32' })
  assert.strictEqual(firstArgument.syntax, firstArgument.expression.syntax)
  assert.strictEqual(oneCall.mappings.at(0)?.argument, firstArgument)
  assert.strictEqual(
    oneCall.mappings.at(0)?.parameter,
    functionAt(one, 0).declaration.parameters.at(0),
  )
  assert.deepEqual(oneCall.contract, {
    _tag: 'Compatible',
    expectedCount: 1,
    actualCount: 1,
  })
  assert.deepEqual(
    twoCall.arguments.map((argument) => argument.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    twoCall.mappings.map((mapping) => [mapping.argument.id.ordinal, mapping.parameter.id.ordinal]),
    [
      [0, 0],
      [1, 1],
    ],
  )
  assert.deepEqual(twoCall.contract, {
    _tag: 'Compatible',
    expectedCount: 2,
    actualCount: 2,
  })
  assert.strictEqual(Object.isFrozen(oneCall.arguments), true)
  assert.strictEqual(Object.isFrozen(firstArgument), true)
  assert.strictEqual(Object.isFrozen(oneCall.mappings), true)
  assert.strictEqual(Object.isFrozen(oneCall.contract), true)
})

it('analyzes nested call arguments recursively from their leaves outward', () => {
  const result = analyzeText('fixture://nested-call-semantic.silk', nestedCallSource)
  const main = functionAt(result, 1)
  const call = callFact(main)
  const argument = call.arguments.at(0) ?? raise('expected nested argument')

  assert.strictEqual(argument.expression._tag, 'Call')
  if (argument.expression._tag !== 'Call') return
  const inner = argument.expression
  const innerArgument = inner.arguments.at(0) ?? raise('expected inner argument')
  assert.strictEqual(argument.syntax, inner.syntax)
  assert.strictEqual(innerArgument.expression._tag, 'Integer')
  assert.deepEqual(innerArgument.type, { _tag: 'Available', type: 'I32' })
  assert.strictEqual(inner.reference._tag, 'Resolved')
  assert.strictEqual(inner.contract._tag, 'Compatible')
  assert.strictEqual(inner.mappings.at(0)?.argument, innerArgument)
  assert.strictEqual(inner.mappings.at(0)?.parameter.id.ordinal, 0)
  assert.deepEqual(inner.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(argument.type, { _tag: 'Available', type: 'I32' })
  assert.strictEqual(call.contract._tag, 'Compatible')
  assert.deepEqual(call.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(main.returnCompatibility, { _tag: 'Compatible' })
  assert.notDeepEqual(inner.syntax.span, call.syntax.span)
  assert.deepEqual(argument.id.callSpan, call.syntax.span)
  assert.deepEqual(innerArgument.id.callSpan, inner.syntax.span)
  assert.deepEqual(result.parse.diagnostics, [])
  assert.deepEqual(result.diagnostics, [])
})

it('preserves nested sibling order and call-local argument identities', () => {
  const result = analyzeText('fixture://nested-siblings.silk', nestedSiblingCallsSource)
  const outer = callFact(functionAt(result, 2))
  const expressions = outer.arguments.map((argument) => argument.expression)

  assert.deepEqual(
    outer.arguments.map((argument) => argument.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    expressions.map((expression) => expression._tag),
    ['Call', 'Call'],
  )
  const left = expressions.at(0) ?? raise('expected left nested call')
  const right = expressions.at(1) ?? raise('expected right nested call')
  if (left._tag !== 'Call' || right._tag !== 'Call') return
  assert.ok(left.syntax.span.start < right.syntax.span.start)
  assert.deepEqual(
    left.arguments.map((argument) => argument.id.ordinal),
    [0],
  )
  assert.deepEqual(
    right.arguments.map((argument) => argument.id.ordinal),
    [0],
  )
  assert.deepEqual(left.arguments.at(0)?.id.callSpan, left.syntax.span)
  assert.deepEqual(right.arguments.at(0)?.id.callSpan, right.syntax.span)
  assert.strictEqual(left.contract._tag, 'Compatible')
  assert.strictEqual(right.contract._tag, 'Compatible')
  assert.strictEqual(outer.contract._tag, 'Compatible')
})

it('propagates an unresolved inner target only to dependent outer facts', () => {
  const result = analyzeText('fixture://unresolved-nested.silk', unresolvedNestedCallSource)
  const outer = callFact(functionAt(result, 1))
  const inner = outer.arguments.at(0)?.expression ?? raise('expected nested expression')

  assert.strictEqual(inner._tag, 'Call')
  if (inner._tag !== 'Call') return
  assert.strictEqual(inner.reference._tag, 'Missing')
  assert.deepEqual(inner.type, { _tag: 'Unavailable' })
  assert.strictEqual(inner.contract._tag, 'Unavailable')
  if (inner.contract._tag !== 'Unavailable') return
  assert.strictEqual(inner.contract.reason._tag, 'UnavailableCallTarget')
  assert.strictEqual(outer.mappings.length, 1)
  assert.strictEqual(outer.contract._tag, 'Unavailable')
  if (outer.contract._tag !== 'Unavailable') return
  assert.strictEqual(outer.contract.reason._tag, 'UnavailableMappedType')
  assert.deepEqual(outer.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0004'],
  )
})

it('diagnoses an incompatible inner call exactly once without inventing an outer mismatch', () => {
  const result = analyzeText('fixture://incompatible-nested.silk', incompatibleNestedCallSource)
  const outer = callFact(functionAt(result, 1))
  const inner = outer.arguments.at(0)?.expression ?? raise('expected nested expression')

  assert.strictEqual(inner._tag, 'Call')
  if (inner._tag !== 'Call') return
  assert.strictEqual(inner.contract._tag, 'ArityMismatch')
  assert.deepEqual(inner.type, { _tag: 'Available', type: 'I32' })
  assert.strictEqual(outer.contract._tag, 'Compatible')
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0007'],
  )
})

it('analyzes representative deep nesting deterministically', () => {
  const depth = 64
  const expression = `${'identity('.repeat(depth)}42${')'.repeat(depth)}`
  const source = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return ${expression} }`
  const first = analyzeText('fixture://deep-nested-semantic.silk', source)
  const second = analyzeText('fixture://deep-nested-semantic.silk', source)
  let current: SemanticAnalysis.ExpressionFact = functionAt(first, 1).returnedExpression
  let calls = 0

  while (current._tag === 'Call') {
    calls += 1
    current = current.arguments.at(0)?.expression ?? raise('expected nested argument')
  }

  assert.strictEqual(calls, depth)
  assert.strictEqual(current._tag, 'Integer')
  assert.deepEqual(first, second)
  assert.deepEqual(first.diagnostics, [])
})

it('keeps damaged nested syntax parser-owned while retaining recursive facts', () => {
  const result = analyzeText('fixture://damaged-nested-call.silk', damagedNestedCallSource)
  const main = functionAt(result, 1)
  const call = callFact(main)
  const argument = call.arguments.at(0) ?? raise('expected damaged nested argument')

  assert.strictEqual(argument.expression._tag, 'Call')
  if (argument.expression._tag !== 'Call') return
  assert.strictEqual(argument.expression.contract._tag, 'Unavailable')
  if (argument.expression.contract._tag !== 'Unavailable') return
  assert.strictEqual(argument.expression.contract.reason._tag, 'UnavailableCallSyntax')
  assert.deepEqual(argument.type, { _tag: 'Unavailable' })
  assert.strictEqual(call.mappings.length, 1)
  assert.strictEqual(call.contract._tag, 'Unavailable')
  if (call.contract._tag !== 'Unavailable') return
  assert.strictEqual(call.contract.reason._tag, 'UnavailableMappedType')
  assert.deepEqual(
    result.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002', 'PAR0001'],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('keeps flat argument semantics unchanged beside recursive nested facts', () => {
  const flat = analyzeText('fixture://flat-call-regression.silk', identityCallSource)
  const nested = analyzeText('fixture://nested-call-regression.silk', nestedCallSource)
  const flatCall = callFact(functionAt(flat, 1))
  const nestedCall = callFact(functionAt(nested, 1))

  assert.strictEqual(flatCall.arguments.at(0)?.expression._tag, 'Integer')
  assert.strictEqual(flatCall.contract._tag, 'Compatible')
  assert.deepEqual(flatCall.type, { _tag: 'Available', type: 'I32' })
  assert.strictEqual(nestedCall.arguments.at(0)?.expression._tag, 'Call')
  assert.strictEqual(nestedCall.contract._tag, 'Compatible')
  assert.deepEqual(nested.diagnostics, [])
})

it('checks zero-, one-, and two-argument compatible calls', () => {
  const zero = callFact(functionAt(analyzeText('fixture://zero-contract.silk', validCallSource), 1))
  const one = callFact(
    functionAt(analyzeText('fixture://one-contract.silk', identityCallSource), 1),
  )
  const two = callFact(
    functionAt(analyzeText('fixture://two-contract.silk', twoArgumentCallSource), 1),
  )

  assert.strictEqual(zero.contract._tag, 'Compatible')
  assert.strictEqual(one.contract._tag, 'Compatible')
  assert.strictEqual(two.contract._tag, 'Compatible')
})

it('retains partial mappings and diagnoses too few and too many arguments', () => {
  const tooFew = analyzeText('fixture://too-few.silk', tooFewArgumentsSource)
  const tooMany = analyzeText('fixture://too-many.silk', tooManyArgumentsSource)
  const fewCall = callFact(functionAt(tooFew, 1))
  const manyFunction = functionAt(tooMany, 1)
  const manyCall = callFact(manyFunction)

  assert.deepEqual(fewCall.contract, {
    _tag: 'ArityMismatch',
    expectedCount: 2,
    actualCount: 1,
  })
  assert.strictEqual(fewCall.mappings.length, 1)
  assert.deepEqual(manyCall.contract, {
    _tag: 'ArityMismatch',
    expectedCount: 1,
    actualCount: 2,
  })
  assert.strictEqual(manyCall.mappings.length, 1)
  assert.deepEqual(manyFunction.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(manyCall.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(
    diagnosticView(tooFew).map((diagnostic) => ({
      code: diagnostic.code,
      reason: diagnostic.reason,
    })),
    [
      {
        code: 'SEM0007',
        reason: {
          _tag: 'WrongCallArity',
          target: functionAt(tooFew, 0).declaration.id,
          expectedCount: 2,
          actualCount: 1,
        },
      },
    ],
  )
  assert.deepEqual(
    tooMany.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0007'],
  )
  assert.strictEqual(tooMany.diagnostics.at(0)?.span, manyCall.syntax.span)
})

it('withholds contracts without cascading when a prerequisite is unavailable', () => {
  const parameterType = analyzeText(
    'fixture://unavailable-parameter-contract.silk',
    unavailableParameterContractSource,
  )
  const argumentType = analyzeText(
    'fixture://unavailable-argument-contract.silk',
    unavailableArgumentContractSource,
  )
  const missingTarget = analyzeText('fixture://missing-contract-target.silk', unknownCallSource)
  const ambiguousTarget = analyzeText(
    'fixture://ambiguous-contract-target.silk',
    ambiguousCallSource,
  )
  const recovered = analyzeText('fixture://recovered-argument.silk', recoveredArgumentSource)
  const parameterCall = callFact(functionAt(parameterType, 1))
  const argumentCall = callFact(functionAt(argumentType, 1))
  const missingCall = callFact(functionAt(missingTarget, 0))
  const ambiguousCall = callFact(functionAt(ambiguousTarget, 2))
  const recoveredCall = callFact(functionAt(recovered, 1))

  assert.strictEqual(parameterCall.contract._tag, 'Unavailable')
  assert.strictEqual(parameterCall.mappings.length, 1)
  assert.strictEqual(argumentCall.contract._tag, 'Unavailable')
  assert.strictEqual(argumentCall.mappings.length, 1)
  assert.strictEqual(missingCall.contract._tag, 'Unavailable')
  assert.strictEqual(missingCall.mappings.length, 0)
  assert.strictEqual(ambiguousCall.contract._tag, 'Unavailable')
  assert.strictEqual(ambiguousCall.mappings.length, 0)
  assert.strictEqual(recoveredCall.contract._tag, 'Unavailable')
  assert.strictEqual(recoveredCall.arguments.length, 0)
  assert.strictEqual(recoveredCall.mappings.length, 0)
  assert.deepEqual(
    parameterType.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001'],
  )
  assert.deepEqual(
    argumentType.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0006'],
  )
  assert.deepEqual(
    missingTarget.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0004'],
  )
  assert.deepEqual(
    recovered.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002', 'PAR0001'],
  )
  assert.deepEqual(recovered.diagnostics, [])
})

it('keeps call contracts deterministic across fresh analyses', () => {
  const first = analyzeText('fixture://contract-determinism.silk', tooManyArgumentsSource)
  const second = analyzeText('fixture://contract-determinism.silk', tooManyArgumentsSource)

  assert.deepEqual(first.functions, second.functions)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
})

it('keeps parameter lookup isolated to its owning function', () => {
  const result = analyzeText(
    'fixture://cross-function-parameter.silk',
    crossFunctionParameterSource,
  )
  const owner = functionAt(result, 0)
  const other = functionAt(result, 1)
  const ownerReference = identifierFact(owner)
  const otherReference = identifierFact(other)

  assert.strictEqual(ownerReference.reference._tag, 'Resolved')
  assert.strictEqual(otherReference.reference._tag, 'Missing')
  assert.deepEqual(other.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0006'],
  )
})

it('diagnoses unknown local names at their exact reference spans', () => {
  const result = analyzeText(
    'fixture://unknown-parameter-reference.silk',
    unknownParameterReferenceSource,
  )
  const reference = identifierFact(functionAt(result, 0)).reference

  assert.strictEqual(reference._tag, 'Missing')
  if (reference._tag !== 'Missing') return
  assert.deepEqual(diagnosticView(result), [
    {
      code: 'SEM0006',
      start: reference.token.span.start,
      end: reference.token.span.end,
      reason: { _tag: 'UnknownParameterReference', spelling: 'missing' },
    },
  ])
})

it('preserves duplicate parameters and reports declaration-owned ambiguity', () => {
  const duplicate = analyzeText('fixture://duplicate-parameter.silk', duplicateParameterSource)
  const triple = analyzeText(
    'fixture://triple-duplicate-parameter.silk',
    tripleDuplicateParameterSource,
  )
  const declaration = functionAt(duplicate, 0).declaration
  const lookup = SemanticAnalysis.parameterByName(declaration, 'value')
  const reference = identifierFact(functionAt(duplicate, 0)).reference

  assert.strictEqual(lookup._tag, 'Ambiguous')
  assert.strictEqual(reference._tag, 'Ambiguous')
  if (lookup._tag !== 'Ambiguous' || reference._tag !== 'Ambiguous') return
  assert.deepEqual(
    lookup.parameters.map((parameter) => parameter.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    reference.parameters.map((parameter) => parameter.id.ordinal),
    [0, 1],
  )
  assert.deepEqual(
    duplicate.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0005'],
  )
  assert.deepEqual(
    triple.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0005', 'SEM0005'],
  )
})

it('resolves parameter types and preserves parser-owned damaged states', () => {
  const unknown = analyzeText('fixture://unknown-parameter-type.silk', unknownParameterTypeSource)
  const missingName = analyzeText(
    'fixture://missing-parameter-name.silk',
    missingParameterNameSource,
  )
  const missingType = analyzeText(
    'fixture://missing-parameter-type.silk',
    missingParameterTypeSource,
  )
  const damagedReference = analyzeText('fixture://damaged-identifier.silk', damagedIdentifierSource)
  const unknownParameter = functionAt(unknown, 0).declaration.parameters.at(0)
  const missingNameParameter = functionAt(missingName, 0).declaration.parameters.at(0)
  const missingTypeParameter = functionAt(missingType, 0).declaration.parameters.at(0)

  assert.strictEqual(unknownParameter?.declaredType._tag, 'Unresolved')
  assert.deepEqual(
    unknown.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0001'],
  )
  assert.strictEqual(missingNameParameter?.name._tag, 'Unavailable')
  assert.strictEqual(missingTypeParameter?.declaredType._tag, 'Unavailable')
  assert.deepEqual(missingName.diagnostics, [])
  assert.deepEqual(missingType.diagnostics, [])
  assert.strictEqual(identifierFact(functionAt(damagedReference, 0)).reference._tag, 'Unavailable')
  assert.deepEqual(damagedReference.diagnostics, [])
  assert.deepEqual(
    damagedReference.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
})

it('keeps parameter facts and diagnostics deterministic across fresh analyses', () => {
  const source = `${duplicateParameterSource}\n${unknownParameterReferenceSource}`
  const first = analyzeText('fixture://parameter-determinism.silk', source)
  const second = analyzeText('fixture://parameter-determinism.silk', source)

  assert.deepEqual(first.functions, second.functions)
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
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

it('resolves present callees, accepts unchecked arguments, and withholds damaged calls', () => {
  const missingParenthesis = analyzeText(
    'fixture://missing-call-right-parenthesis.silk',
    missingCallRightParenthesisSource,
  )
  const uncheckedArgument = analyzeText(
    'fixture://unchecked-call-argument.silk',
    identityCallSource,
  )

  const damagedFact = functionAt(missingParenthesis, 1)
  const damagedCall = callFact(damagedFact)
  assert.strictEqual(damagedCall.reference._tag, 'Resolved')
  assert.deepEqual(damagedCall.type, { _tag: 'Unavailable' })
  assert.deepEqual(damagedFact.returnCompatibility, { _tag: 'Unavailable' })
  assert.deepEqual(missingParenthesis.diagnostics, [])

  const uncheckedFact = functionAt(uncheckedArgument, 1)
  const uncheckedCall = callFact(uncheckedFact)
  assert.strictEqual(uncheckedCall.reference._tag, 'Resolved')
  assert.deepEqual(uncheckedCall.type, { _tag: 'Available', type: 'I32' })
  assert.deepEqual(uncheckedFact.returnCompatibility, { _tag: 'Compatible' })
  assert.deepEqual(uncheckedArgument.parse.diagnostics, [])
  assert.deepEqual(uncheckedArgument.diagnostics, [])

  assert.deepEqual(
    missingParenthesis.parse.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
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
