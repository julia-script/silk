import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import type * as Token from '../src/Token.js'
import {
  acceptedShape,
  acceptedSource,
  damagedCallBeforeNextFunctionSource,
  denseTriviaSource,
  type ExpectedNodeShape,
  emptySource,
  identifierCallArgumentSource,
  identitySource,
  interFunctionPunctuationSource,
  invalidUtf8Source,
  malformedArgumentSource,
  missingCallCalleeSource,
  missingCallRightParenthesisSource,
  missingFirstRightBraceSource,
  missingNameSource,
  missingParameterCommaSource,
  missingParameterTypeSource,
  missingRightBraceSource,
  threeFunctionSource,
  trailingTriviaSource,
  triviaCallSource,
  twoFunctionSource,
  twoParameterSource,
  unexpectedPunctuationSource,
  validCallSource,
  valueCallArgumentSource,
  whollyUnrelatedSource,
} from './fixtures/BootstrapParserFixture.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parseBytes = (id: string, bytes: Uint8Array): Parser.ParseResult =>
  Parser.parse(Lexer.lex(SourceFile.make(id, bytes)))

const parseText = (id: string, source: string): Parser.ParseResult => parseBytes(id, ascii(source))

const nodeShape = (node: SyntaxTree.Node): ExpectedNodeShape => ({
  kind: node.kind,
  children: node.children.map((child): string | ExpectedNodeShape => {
    if (SyntaxTree.isNode(child)) return nodeShape(child)
    if (SyntaxTree.isToken(child)) return child.kind
    return `Missing(${child.expected})`
  }),
})

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Element> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Element> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [child],
  )

const missingLeaves = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.MissingToken> =>
  descendants(node).filter(SyntaxTree.isMissingToken)

const errorNodes = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  descendants(node).filter(
    (element): element is SyntaxTree.Node => SyntaxTree.isNode(element) && element.kind === 'Error',
  )

const directFunctionDeclarations = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'FunctionDeclaration',
  )

const directTokenText = (
  result: Parser.ParseResult,
  node: SyntaxTree.Node,
  kind: Token.TokenKind,
): string | undefined => {
  const token = node.children.find(
    (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === kind,
  )
  if (token === undefined) return undefined
  return Array.from(
    Option.getOrThrow(SourceFile.slice(result.lexical.source, token.span)),
    (byte) => String.fromCharCode(byte),
  ).join('')
}

const assertOriginalTokenTraversal = (result: Parser.ParseResult): void => {
  const flattened = SyntaxTree.tokens(result.root)
  assert.strictEqual(flattened.length, result.lexical.tokens.length)
  for (const [index, token] of flattened.entries()) {
    assert.strictEqual(token, result.lexical.tokens.at(index))
  }
}

const reconstructedBytes = (result: Parser.ParseResult): Uint8Array => {
  const bytes = SyntaxTree.tokens(result.root)
    .filter((token) => token.kind !== 'EndOfFile')
    .flatMap((token) =>
      Array.from(Option.getOrThrow(SourceFile.slice(result.lexical.source, token.span))),
    )
  return Uint8Array.from(bytes)
}

const diagnosticView = (result: Parser.ParseResult) =>
  result.diagnostics.map((diagnostic) => ({
    code: diagnostic.code,
    start: diagnostic.span.start,
    end: diagnostic.span.end,
    reason: diagnostic.reason,
  }))

it('parses the accepted function into the exact first concrete node shape', () => {
  const lexical = Lexer.lex(SourceFile.make('fixture://accepted.silk', ascii(acceptedSource)))
  const result = Parser.parse(lexical)

  assert.deepEqual(nodeShape(result.root), acceptedShape)
  assert.deepEqual(result.diagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(acceptedSource))
  assert.strictEqual(result.lexical, lexical)
})

it('parses dense whitespace and line-comment trivia without changing the grammar nodes', () => {
  const result = parseText('fixture://dense-trivia.silk', denseTriviaSource)
  const kinds = descendants(result.root)
    .filter(SyntaxTree.isNode)
    .map((node) => node.kind)

  assert.deepEqual(kinds, [
    'FunctionDeclaration',
    'ParameterList',
    'ReturnType',
    'Block',
    'ReturnStatement',
    'IntegerLiteralExpression',
  ])
  assert.deepEqual(result.diagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(denseTriviaSource))
})

it('parses two declarations as separate direct branches in source order', () => {
  const result = parseText('fixture://two-functions.silk', twoFunctionSource)
  const declarations = directFunctionDeclarations(result.root)

  assert.strictEqual(declarations.length, 2)
  assert.deepEqual(
    declarations.map((declaration) => directTokenText(result, declaration, 'Identifier')),
    ['answer', 'main'],
  )
  const secondLeading = declarations.at(1)?.children.at(0)
  assert.strictEqual(
    secondLeading === undefined ? undefined : SyntaxTree.isToken(secondLeading),
    true,
  )
  if (secondLeading === undefined || !SyntaxTree.isToken(secondLeading)) return
  assert.strictEqual(secondLeading.kind, 'Whitespace')
  assert.strictEqual(Object.isFrozen(result.root), true)
  assert.strictEqual(Object.isFrozen(result.root.children), true)
  assert.strictEqual(Object.isFrozen(declarations.at(0)), true)
  assert.strictEqual(Object.isFrozen(declarations.at(1)), true)
  assert.strictEqual(Object.isFrozen(result.diagnostics), true)
  assert.deepEqual(result.diagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(twoFunctionSource))
})

it('parses three declarations without imposing a temporary source-file limit', () => {
  const result = parseText('fixture://three-functions.silk', threeFunctionSource)

  assert.deepEqual(
    directFunctionDeclarations(result.root).map((declaration) =>
      directTokenText(result, declaration, 'Identifier'),
    ),
    ['one', 'two', 'three'],
  )
  assert.deepEqual(result.diagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(threeFunctionSource))
})

it('parses a zero-argument call as one lossless concrete expression', () => {
  const result = parseText('fixture://valid-call.silk', validCallSource)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const call = calls.at(0)

  assert.strictEqual(calls.length, 1)
  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  assert.deepEqual(nodeShape(call), {
    kind: 'CallExpression',
    children: [
      'Whitespace',
      'Identifier',
      { kind: 'ArgumentList', children: ['LeftParenthesis', 'RightParenthesis'] },
    ],
  })
  assert.strictEqual(directTokenText(result, call, 'Identifier'), 'answer')
  assert.deepEqual(result.diagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(validCallSource))
})

it('retains trivia between every concrete call element', () => {
  const result = parseText('fixture://trivia-call.silk', triviaCallSource)
  const call = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )

  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  const argumentsList = call.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
  )
  assert.notStrictEqual(argumentsList, undefined)
  if (argumentsList === undefined) return
  assert.deepEqual(
    argumentsList.children.map((element) =>
      SyntaxTree.isNode(element)
        ? element.kind
        : SyntaxTree.isToken(element)
          ? element.kind
          : `Missing(${element.expected})`,
    ),
    [
      'Whitespace',
      'LineComment',
      'Whitespace',
      'LeftParenthesis',
      'Whitespace',
      'LineComment',
      'Whitespace',
      'LineComment',
      'Whitespace',
      'RightParenthesis',
    ],
  )
  assert.deepEqual(result.diagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(triviaCallSource))
})

it('recovers a missing call callee without inventing a name', () => {
  const result = parseText('fixture://missing-call-callee.silk', missingCallCalleeSource)
  const call = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )

  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  assert.deepEqual(
    missingLeaves(call).map((leaf) => ({
      expected: leaf.expected,
      start: leaf.span.start,
      end: leaf.span.end,
    })),
    [
      {
        expected: 'Identifier',
        start: missingCallCalleeSource.lastIndexOf('()'),
        end: missingCallCalleeSource.lastIndexOf('()'),
      },
    ],
  )
  assert.deepEqual(
    SyntaxTree.tokens(call).map((token) => token.kind),
    ['Whitespace', 'LeftParenthesis', 'RightParenthesis'],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('inserts a missing call parenthesis without consuming the block brace', () => {
  const result = parseText(
    'fixture://missing-call-right-parenthesis.silk',
    missingCallRightParenthesisSource,
  )
  const call = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const block = descendants(result.root).find(
    (element): element is SyntaxTree.Node => SyntaxTree.isNode(element) && element.kind === 'Block',
  )

  assert.notStrictEqual(call, undefined)
  assert.notStrictEqual(block, undefined)
  if (call === undefined || block === undefined) return
  assert.deepEqual(
    missingLeaves(call).map((leaf) => leaf.expected),
    ['RightParenthesis'],
  )
  assert.strictEqual(
    block.children.some((element) => SyntaxTree.isToken(element) && element.kind === 'RightBrace'),
    true,
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('parses decimal and identifier call arguments as ordered concrete expressions', () => {
  const literal = parseText('fixture://value-call-argument.silk', valueCallArgumentSource)
  const identifier = parseText(
    'fixture://identifier-call-argument.silk',
    identifierCallArgumentSource,
  )
  const literalArguments = descendants(literal.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
  )
  const identifierArguments = descendants(identifier.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
  )

  assert.deepEqual(nodeShape(literalArguments.at(0) ?? literal.root), {
    kind: 'ArgumentList',
    children: [
      'LeftParenthesis',
      { kind: 'IntegerLiteralExpression', children: ['DecimalInteger'] },
      'RightParenthesis',
    ],
  })
  assert.deepEqual(nodeShape(identifierArguments.at(0) ?? identifier.root), {
    kind: 'ArgumentList',
    children: [
      'LeftParenthesis',
      { kind: 'IdentifierExpression', children: ['Identifier'] },
      'RightParenthesis',
    ],
  })
  assert.deepEqual(literal.diagnostics, [])
  assert.deepEqual(identifier.diagnostics, [])
  assertOriginalTokenTraversal(literal)
  assertOriginalTokenTraversal(identifier)
})

it('parses typed parameters and bare identifier return expressions', () => {
  const identity = parseText('fixture://identity.silk', identitySource)
  const multiple = parseText('fixture://two-parameters.silk', twoParameterSource)
  const identityParameters = descendants(identity.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ParameterDeclaration',
  )
  const multipleParameters = descendants(multiple.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ParameterDeclaration',
  )

  assert.strictEqual(identityParameters.length, 1)
  assert.strictEqual(multipleParameters.length, 2)
  assert.deepEqual(nodeShape(identityParameters.at(0) ?? identity.root), {
    kind: 'ParameterDeclaration',
    children: ['Identifier', 'Colon', 'Whitespace', 'Identifier'],
  })
  assert.strictEqual(
    descendants(identity.root).some(
      (element) => SyntaxTree.isNode(element) && element.kind === 'IdentifierExpression',
    ),
    true,
  )
  assert.deepEqual(identity.diagnostics, [])
  assert.deepEqual(multiple.diagnostics, [])
  assertOriginalTokenTraversal(identity)
  assertOriginalTokenTraversal(multiple)
})

it('recovers missing parameter types and commas without losing later syntax', () => {
  const missingType = parseText('fixture://missing-parameter-type.silk', missingParameterTypeSource)
  const missingComma = parseText(
    'fixture://missing-parameter-comma.silk',
    missingParameterCommaSource,
  )

  assert.deepEqual(
    missingLeaves(missingType.root).map((leaf) => leaf.expected),
    ['Identifier'],
  )
  assert.deepEqual(
    missingLeaves(missingComma.root).map((leaf) => leaf.expected),
    ['Comma'],
  )
  assert.strictEqual(
    descendants(missingComma.root).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'ParameterDeclaration',
    ).length,
    2,
  )
  assertOriginalTokenTraversal(missingType)
  assertOriginalTokenTraversal(missingComma)
})

it('keeps malformed arguments explicit and resumes at the next comma', () => {
  const result = parseText('fixture://malformed-call-argument.silk', malformedArgumentSource)
  const call = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )

  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  assert.deepEqual(
    errorNodes(call)
      .flatMap((node) => SyntaxTree.tokens(node))
      .map((token) => token.kind),
    ['Invalid'],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002', 'PAR0001'],
  )
  assert.strictEqual(
    descendants(call).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'IdentifierExpression',
    ).length,
    2,
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(malformedArgumentSource))
})

it('bounds damaged call recovery before the following function', () => {
  const result = parseText(
    'fixture://damaged-call-before-next-function.silk',
    damagedCallBeforeNextFunctionSource,
  )
  const declarations = directFunctionDeclarations(result.root)
  const first = declarations.at(0)
  const second = declarations.at(1)

  assert.notStrictEqual(first, undefined)
  assert.notStrictEqual(second, undefined)
  if (first === undefined || second === undefined) return
  assert.deepEqual(
    missingLeaves(first).map((leaf) => leaf.expected),
    ['RightParenthesis', 'RightBrace'],
  )
  assert.deepEqual(missingLeaves(second), [])
  assert.strictEqual(directTokenText(result, second, 'Identifier'), 'after')
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001', 'PAR0001'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(damagedCallBeforeNextFunctionSource))
})

it('keeps trailing trivia with the end-of-file expectation', () => {
  const result = parseText('fixture://trailing-trivia.silk', trailingTriviaSource)
  const directTokens = result.root.children.filter(SyntaxTree.isToken)

  assert.deepEqual(
    directTokens.map((token) => token.kind),
    ['Whitespace', 'LineComment', 'Whitespace', 'EndOfFile'],
  )
  assert.strictEqual(directFunctionDeclarations(result.root).length, 1)
  assert.deepEqual(result.diagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(trailingTriviaSource))
})

it('inserts a missing first brace without consuming the second declaration', () => {
  const result = parseText('fixture://missing-first-brace.silk', missingFirstRightBraceSource)
  const declarations = directFunctionDeclarations(result.root)
  const first = declarations.at(0)
  const second = declarations.at(1)

  assert.notStrictEqual(first, undefined)
  assert.notStrictEqual(second, undefined)
  if (first === undefined || second === undefined) return
  assert.deepEqual(
    missingLeaves(first).map((leaf) => ({
      expected: leaf.expected,
      start: leaf.span.start,
      end: leaf.span.end,
    })),
    [
      {
        expected: 'RightBrace',
        start: missingFirstRightBraceSource.indexOf('pub fn main'),
        end: missingFirstRightBraceSource.indexOf('pub fn main'),
      },
    ],
  )
  assert.deepEqual(missingLeaves(second), [])
  assert.strictEqual(directTokenText(result, second, 'Identifier'), 'main')
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(missingFirstRightBraceSource))
})

it('retains unexpected punctuation at a function boundary and parses the next declaration', () => {
  const result = parseText(
    'fixture://inter-function-punctuation.silk',
    interFunctionPunctuationSource,
  )
  const declarations = directFunctionDeclarations(result.root)
  const boundaryErrors = errorNodes(result.root)
  const second = declarations.at(1)

  assert.strictEqual(declarations.length, 2)
  assert.notStrictEqual(second, undefined)
  if (second === undefined) return
  assert.strictEqual(directTokenText(result, second, 'Identifier'), 'main')
  assert.deepEqual(missingLeaves(second), [])
  assert.strictEqual(boundaryErrors.length, 1)
  assert.deepEqual(
    boundaryErrors.flatMap((node) => SyntaxTree.tokens(node)).map((token) => token.kind),
    ['Invalid', 'Whitespace'],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(interFunctionPunctuationSource))
})

it('inserts a missing function name before the opening parenthesis', () => {
  const result = parseText('fixture://missing-name.silk', missingNameSource)
  const missing = missingLeaves(result.root)

  assert.deepEqual(
    missing.map((leaf) => ({
      expected: leaf.expected,
      start: leaf.span.start,
      end: leaf.span.end,
    })),
    [{ expected: 'Identifier', start: 7, end: 7 }],
  )
  assert.deepEqual(diagnosticView(result), [
    {
      code: 'PAR0001',
      start: 7,
      end: 7,
      reason: { _tag: 'MissingToken', expected: 'Identifier' },
    },
  ])
  assertOriginalTokenTraversal(result)
})

it('inserts a missing right brace at end-of-file', () => {
  const result = parseText('fixture://missing-brace.silk', missingRightBraceSource)
  const missing = missingLeaves(result.root)

  assert.deepEqual(
    missing.map((leaf) => ({
      expected: leaf.expected,
      start: leaf.span.start,
      end: leaf.span.end,
    })),
    [
      {
        expected: 'RightBrace',
        start: missingRightBraceSource.length,
        end: missingRightBraceSource.length,
      },
    ],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('groups unexpected punctuation and following trivia before the function name', () => {
  const result = parseText('fixture://unexpected.silk', unexpectedPunctuationSource)
  const errors = errorNodes(result.root)

  assert.strictEqual(errors.length, 1)
  const error = errors.at(0)
  assert.notStrictEqual(error, undefined)
  if (error === undefined) return
  assert.deepEqual(
    SyntaxTree.tokens(error).map((token) => token.kind),
    ['Invalid', 'Whitespace'],
  )
  assert.deepEqual(diagnosticView(result), [
    { code: 'PAR0002', start: 7, end: 9, reason: { _tag: 'UnexpectedTokens' } },
  ])
  assert.deepEqual(
    result.lexical.diagnostics.map((diagnostic) => diagnostic.code),
    ['LEX0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('terminates with explicit missing structure for empty input', () => {
  const result = parseBytes('fixture://empty.silk', emptySource)

  assert.strictEqual(missingLeaves(result.root).length, 11)
  assert.strictEqual(result.diagnostics.length, 11)
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    Array.from({ length: 11 }, () => 'PAR0001'),
  )
  assertOriginalTokenTraversal(result)
})

it('terminates on wholly unrelated input and retains it in one error region', () => {
  const result = parseText('fixture://unrelated.silk', whollyUnrelatedSource)
  const errors = errorNodes(result.root)

  assert.strictEqual(errors.length, 1)
  assert.deepEqual(errors.at(0)?.span, result.lexical.tokens.at(0)?.span)
  assert.strictEqual(missingLeaves(result.root).length, 11)
  assert.strictEqual(result.diagnostics.at(0)?.code, 'PAR0002')
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(whollyUnrelatedSource))
})

it('retains invalid UTF-8 bytes and lexical diagnostics inside concrete recovery', () => {
  const result = parseBytes('fixture://invalid-utf8.silk', invalidUtf8Source)
  const errors = errorNodes(result.root)

  assert.deepEqual(
    result.lexical.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    [{ code: 'LEX0001', start: 7, end: 9 }],
  )
  assert.deepEqual(
    errors.flatMap((node) => SyntaxTree.tokens(node)).map((token: Token.Token) => token.kind),
    ['Invalid', 'Whitespace'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), invalidUtf8Source)
})

it('is deterministic across repeated fresh lexical results', () => {
  const first = parseText('fixture://deterministic.silk', interFunctionPunctuationSource)
  const second = parseText('fixture://deterministic.silk', interFunctionPunctuationSource)

  assert.deepEqual(nodeShape(first.root), nodeShape(second.root))
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
  assert.deepEqual(first.lexical.diagnostics, second.lexical.diagnostics)
  assert.deepEqual(reconstructedBytes(first), reconstructedBytes(second))
})
