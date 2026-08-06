import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import type * as SyntaxFile from '../src/SyntaxFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import type * as Token from '../src/Token.js'
import {
  acceptedShape,
  acceptedSource,
  damagedCallBeforeNextFunctionSource,
  damagedNestedBeforeNextFunctionSource,
  damagedNestedSiblingSource,
  damagedStructSource,
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
  missingNestedRightParenthesisSource,
  missingParameterCommaSource,
  missingParameterTypeSource,
  missingRightBraceSource,
  nestedCallSource,
  nestedSiblingCallSource,
  threeFunctionSource,
  trailingTriviaSource,
  triviaCallSource,
  twoFunctionSource,
  twoParameterSource,
  unexpectedPunctuationSource,
  validCallSource,
  validStructSource,
  valueCallArgumentSource,
  whollyUnrelatedSource,
} from './fixtures/BootstrapParserFixture.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const parseBytes = (id: string, bytes: Uint8Array): SyntaxFile.SyntaxFile =>
  Parser.parse(Lexer.lex(SourceFile.make(id, bytes)))

const parseText = (id: string, source: string): SyntaxFile.SyntaxFile =>
  parseBytes(id, ascii(source))

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
  result: SyntaxFile.SyntaxFile,
  node: SyntaxTree.Node,
  kind: Token.TokenKind,
): string | undefined => {
  const token = node.children.find(
    (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === kind,
  )
  if (token === undefined) return undefined
  return Array.from(Option.getOrThrow(SourceFile.slice(result.source, token.span)), (byte) =>
    String.fromCharCode(byte),
  ).join('')
}

const assertOriginalTokenTraversal = (result: SyntaxFile.SyntaxFile): void => {
  const flattened = SyntaxTree.tokens(result.root)
  assert.strictEqual(flattened.length, result.tokens.length)
  for (const [index, token] of flattened.entries()) {
    assert.strictEqual(token, result.tokens.at(index))
  }
}

const reconstructedBytes = (result: SyntaxFile.SyntaxFile): Uint8Array => {
  const bytes = SyntaxTree.tokens(result.root)
    .filter((token) => token.kind !== 'EndOfFile')
    .flatMap((token) => Array.from(Option.getOrThrow(SourceFile.slice(result.source, token.span))))
  return Uint8Array.from(bytes)
}

const diagnosticView = (result: SyntaxFile.SyntaxFile) =>
  result.parserDiagnostics.map((diagnostic) => ({
    code: diagnostic.code,
    start: diagnostic.span.start,
    end: diagnostic.span.end,
    reason: diagnostic.reason,
  }))

it('parses the accepted function into the exact first concrete node shape', () => {
  const lexical = Lexer.lex(SourceFile.make('fixture://accepted.silk', ascii(acceptedSource)))
  const result = Parser.parse(lexical)

  assert.deepEqual(nodeShape(result.root), acceptedShape)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(acceptedSource))
  assert.strictEqual(result.source, lexical.source)
  assert.strictEqual(result.tokens, lexical.tokens)
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
    'TypePath',
    'Block',
    'ReturnStatement',
    'IntegerLiteralExpression',
  ])
  assert.deepEqual(result.parserDiagnostics, [])
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
  assert.strictEqual(Object.isFrozen(result.parserDiagnostics), true)
  assert.deepEqual(result.parserDiagnostics, [])
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
  assert.deepEqual(result.parserDiagnostics, [])
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
  assert.deepEqual(result.parserDiagnostics, [])
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
  assert.deepEqual(result.parserDiagnostics, [])
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
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
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
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
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
  assert.deepEqual(literal.parserDiagnostics, [])
  assert.deepEqual(identifier.parserDiagnostics, [])
  assertOriginalTokenTraversal(literal)
  assertOriginalTokenTraversal(identifier)
})

it('parses nested calls as lossless argument expressions', () => {
  const result = parseText('fixture://nested-call.silk', nestedCallSource)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const outer = calls.at(0)
  const inner = calls.at(1)

  assert.strictEqual(calls.length, 2)
  assert.notStrictEqual(outer, undefined)
  assert.notStrictEqual(inner, undefined)
  if (outer === undefined || inner === undefined) return
  assert.deepEqual(nodeShape(outer), {
    kind: 'CallExpression',
    children: [
      'Whitespace',
      'Identifier',
      {
        kind: 'ArgumentList',
        children: [
          'LeftParenthesis',
          {
            kind: 'CallExpression',
            children: [
              'Identifier',
              {
                kind: 'ArgumentList',
                children: [
                  'LeftParenthesis',
                  { kind: 'IntegerLiteralExpression', children: ['DecimalInteger'] },
                  'RightParenthesis',
                ],
              },
            ],
          },
          'RightParenthesis',
        ],
      },
    ],
  })
  assert.strictEqual(inner.span.start, nestedCallSource.lastIndexOf('identity(42)'))
  assert.strictEqual(inner.span.end, nestedCallSource.lastIndexOf('identity(42)') + 12)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(nestedCallSource))
})

it('preserves sibling nested calls and their outer comma', () => {
  const result = parseText('fixture://nested-siblings.silk', nestedSiblingCallSource)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const outerArguments = calls
    .at(0)
    ?.children.find(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
    )

  assert.strictEqual(calls.length, 3)
  assert.notStrictEqual(outerArguments, undefined)
  if (outerArguments === undefined) return
  assert.deepEqual(
    outerArguments.children.map((element) =>
      SyntaxTree.isNode(element)
        ? element.kind
        : SyntaxTree.isToken(element)
          ? element.kind
          : `Missing(${element.expected})`,
    ),
    ['LeftParenthesis', 'CallExpression', 'Comma', 'CallExpression', 'RightParenthesis'],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(nestedSiblingCallSource))
})

it('reserves the outer closing parenthesis when the inner call is damaged', () => {
  const result = parseText(
    'fixture://missing-nested-right-parenthesis.silk',
    missingNestedRightParenthesisSource,
  )
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const outer = calls.at(0)
  const inner = calls.at(1)

  assert.notStrictEqual(outer, undefined)
  assert.notStrictEqual(inner, undefined)
  if (outer === undefined || inner === undefined) return
  assert.deepEqual(
    missingLeaves(inner).map((leaf) => leaf.expected),
    ['RightParenthesis'],
  )
  assert.deepEqual(
    missingLeaves(outer).map((leaf) => leaf.expected),
    ['RightParenthesis'],
  )
  assert.strictEqual(
    outer.children
      .flatMap((element) => (SyntaxTree.isNode(element) ? element.children : []))
      .some((element) => SyntaxTree.isToken(element) && element.kind === 'RightParenthesis'),
    true,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(missingNestedRightParenthesisSource))
})

it('keeps a sibling argument after damaged nested syntax', () => {
  const result = parseText('fixture://damaged-nested-sibling.silk', damagedNestedSiblingSource)
  const calls = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'CallExpression',
  )
  const outerArguments = calls
    .at(0)
    ?.children.find(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'ArgumentList',
    )

  assert.strictEqual(calls.length, 3)
  assert.notStrictEqual(outerArguments, undefined)
  if (outerArguments === undefined) return
  assert.strictEqual(
    outerArguments.children.filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'CallExpression',
    ).length,
    2,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0002', 'PAR0001'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(damagedNestedSiblingSource))
})

it('bounds nested recovery before the following declaration', () => {
  const result = parseText(
    'fixture://damaged-nested-before-next-function.silk',
    damagedNestedBeforeNextFunctionSource,
  )
  const declarations = directFunctionDeclarations(result.root)
  const after = declarations.at(2)

  assert.strictEqual(declarations.length, 3)
  assert.notStrictEqual(after, undefined)
  if (after === undefined) return
  assert.deepEqual(missingLeaves(after), [])
  assert.strictEqual(directTokenText(result, after, 'Identifier'), 'after')
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(damagedNestedBeforeNextFunctionSource))
})

it('parses representative deep nested calls deterministically', () => {
  const depth = 64
  const expression = `${'identity('.repeat(depth)}42${')'.repeat(depth)}`
  const source = `pub fn identity(value: I32) -> I32 { return value }\npub fn main() -> I32 { return ${expression} }`
  const first = parseText('fixture://deep-nested-call.silk', source)
  const second = parseText('fixture://deep-nested-call.silk', source)

  assert.strictEqual(
    descendants(first.root).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'CallExpression',
    ).length,
    depth,
  )
  assert.deepEqual(nodeShape(first.root), nodeShape(second.root))
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
  assertOriginalTokenTraversal(first)
  assert.deepEqual(reconstructedBytes(first), ascii(source))
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
    children: ['Identifier', 'Colon', { kind: 'TypePath', children: ['Whitespace', 'Identifier'] }],
  })
  assert.strictEqual(
    descendants(identity.root).some(
      (element) => SyntaxTree.isNode(element) && element.kind === 'IdentifierExpression',
    ),
    true,
  )
  assert.deepEqual(identity.parserDiagnostics, [])
  assert.deepEqual(multiple.parserDiagnostics, [])
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
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
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
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
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
  assert.deepEqual(result.parserDiagnostics, [])
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
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
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
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
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
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
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
    result.lexicalDiagnostics.map((diagnostic) => diagnostic.code),
    ['LEX0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('terminates with explicit missing structure for empty input', () => {
  const result = parseBytes('fixture://empty.silk', emptySource)

  assert.strictEqual(missingLeaves(result.root).length, 10)
  assert.strictEqual(result.parserDiagnostics.length, 10)
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    Array.from({ length: 10 }, () => 'PAR0001'),
  )
  assertOriginalTokenTraversal(result)
})

it('terminates on wholly unrelated input and retains it in one error region', () => {
  const result = parseText('fixture://unrelated.silk', whollyUnrelatedSource)
  const errors = errorNodes(result.root)

  assert.strictEqual(errors.length, 1)
  assert.deepEqual(errors.at(0)?.span, result.tokens.at(0)?.span)
  assert.strictEqual(missingLeaves(result.root).length, 10)
  assert.strictEqual(result.parserDiagnostics.at(0)?.code, 'PAR0002')
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(whollyUnrelatedSource))
})

it('retains invalid UTF-8 bytes and lexical diagnostics inside concrete recovery', () => {
  const result = parseBytes('fixture://invalid-utf8.silk', invalidUtf8Source)
  const errors = errorNodes(result.root)

  assert.deepEqual(
    result.lexicalDiagnostics.map((diagnostic) => ({
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
  assert.deepEqual(first.lexicalDiagnostics, second.lexicalDiagnostics)
  assert.deepEqual(reconstructedBytes(first), reconstructedBytes(second))
})

it('parses import declarations before functions as separate lossless branches', () => {
  const result = parseText(
    'fixture://imports.silk',
    'import math\nimport io\npub fn main() -> I32 { return 42 }',
  )
  const kinds = result.root.children.flatMap((element) =>
    SyntaxTree.isNode(element) ? [element.kind] : [],
  )

  assert.deepEqual(kinds, ['ImportDeclaration', 'ImportDeclaration', 'FunctionDeclaration'])
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(
    reconstructedBytes(result),
    ascii('import math\nimport io\npub fn main() -> I32 { return 42 }'),
  )
})

it('recovers a missing import name and keeps the following function parseable', () => {
  const result = parseText(
    'fixture://missing-import-name.silk',
    'import\npub fn main() -> I32 { return 42 }',
  )
  const importNode = result.root.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'ImportDeclaration',
  )
  const functionNode = result.root.children.find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'FunctionDeclaration',
  )

  assert.notStrictEqual(importNode, undefined)
  assert.notStrictEqual(functionNode, undefined)
  assert.strictEqual(
    SyntaxTree.directNode(importNode ?? result.root, 'ImportPath')?.children.some(
      (element) => SyntaxTree.isMissingToken(element) && element.expected === 'Identifier',
    ),
    true,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('keeps import as a keyword only when spelled completely', () => {
  const lexical = Lexer.lex(
    SourceFile.make('fixture://import-keyword.silk', ascii('import importer')),
  )

  assert.deepEqual(
    lexical.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['ImportKeyword', 'Identifier', 'EndOfFile'],
  )
})

it('parses namespace, selective, member-alias, and hybrid imports losslessly', () => {
  const source = `import compiler.Syntax
import compiler.Tree as Ast
import compiler.Parse { Node, parse, encode as encodeSyntax }
import compiler.Hir as Ir { lower, inspect as show }
pub fn main() -> I32 { return 42 }`
  const result = parseText('fixture://full-imports.silk', source)
  const imports = SyntaxTree.directNodes(result.root, 'ImportDeclaration')
  assert.strictEqual(imports.length, 4)
  assert.deepEqual(
    imports.map((node) => {
      const list = SyntaxTree.directNode(node, 'ImportMemberList')
      return list === undefined ? 0 : SyntaxTree.directNodes(list, 'ImportMember').length
    }),
    [0, 0, 3, 2],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('parses private functions without fabricating a public modifier', () => {
  const result = parseText('fixture://private.silk', 'fn helper() -> I32 { return 42 }')
  const declaration = SyntaxTree.directNode(result.root, 'FunctionDeclaration')
  assert.notStrictEqual(declaration, undefined)
  assert.strictEqual(
    declaration === undefined ? undefined : SyntaxTree.directToken(declaration, 'PubKeyword'),
    undefined,
  )
  assert.deepEqual(result.parserDiagnostics, [])
})

it('bounds import recovery and preserves the following declaration', () => {
  const cases = [
    'import compiler. as Tree\nfn helper() -> I32 { return 1 }',
    'import compiler.Syntax as\nfn helper() -> I32 { return 1 }',
    'import compiler.Syntax { Node, , parse }\nfn helper() -> I32 { return 1 }',
    'import compiler.Syntax { Node parse }\nfn helper() -> I32 { return 1 }',
    'import compiler.Syntax { Node\nfn helper() -> I32 { return 1 }',
  ]
  for (const [ordinal, source] of cases.entries()) {
    const result = parseText(`fixture://damaged-import-${ordinal}.silk`, source)
    assert.strictEqual(SyntaxTree.directNodes(result.root, 'ImportDeclaration').length, 1)
    assert.strictEqual(SyntaxTree.directNodes(result.root, 'FunctionDeclaration').length, 1)
    assert.isAtLeast(result.parserDiagnostics.length, 1)
    assertOriginalTokenTraversal(result)
  }
})

it('parses a binding sequence as ordered statement branches', () => {
  const result = parseText(
    'fixture://bindings.silk',
    'pub fn main() -> I32 { let value = 42 return value }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  assert.notStrictEqual(block, undefined)
  if (block === undefined) return

  const statements = block.children.filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['BindingStatement', 'ReturnStatement'])
  const binding = SyntaxTree.directNode(block, 'BindingStatement')
  assert.notStrictEqual(binding, undefined)
  if (binding === undefined) return
  assert.strictEqual(directTokenText(result, binding, 'Identifier'), 'value')
  assert.notStrictEqual(SyntaxTree.directToken(binding, 'Equals'), undefined)
  assert.notStrictEqual(SyntaxTree.directNode(binding, 'IntegerLiteralExpression'), undefined)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(
    reconstructedBytes(result),
    ascii('pub fn main() -> I32 { let value = 42 return value }'),
  )
})

it('parses a move operand with its keyword and name', () => {
  const result = parseText(
    'fixture://move.silk',
    'pub fn main() -> I32 { let value = 42 return move value }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const returnStatement =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ReturnStatement')
  const move =
    returnStatement === undefined
      ? undefined
      : SyntaxTree.directNode(returnStatement, 'MoveExpression')

  assert.notStrictEqual(move, undefined)
  if (move === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(move, 'MoveKeyword'), undefined)
  const subject = SyntaxTree.directNode(move, 'IdentifierExpression')
  assert.notStrictEqual(subject, undefined)
  if (subject === undefined) return
  assert.strictEqual(directTokenText(result, subject, 'Identifier'), 'value')
  assert.deepEqual(result.parserDiagnostics, [])
})

it('recovers a missing initializer at the return boundary', () => {
  const result = parseText(
    'fixture://missing-initializer.silk',
    'pub fn main() -> I32 { let value = return 42 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const binding = block === undefined ? undefined : SyntaxTree.directNode(block, 'BindingStatement')

  assert.notStrictEqual(binding, undefined)
  if (binding === undefined) return
  assert.strictEqual(missingLeaves(binding).length, 1)
  const returnStatement =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ReturnStatement')
  assert.notStrictEqual(returnStatement, undefined)
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('recovers a missing binding name before the equals token', () => {
  const result = parseText(
    'fixture://missing-binding-name.silk',
    'pub fn main() -> I32 { let = 42 return 0 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const binding = block === undefined ? undefined : SyntaxTree.directNode(block, 'BindingStatement')

  assert.notStrictEqual(binding, undefined)
  if (binding === undefined) return
  assert.strictEqual(
    binding.children.some(
      (element) => SyntaxTree.isMissingToken(element) && element.expected === 'Identifier',
    ),
    true,
  )
  const statements = (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['BindingStatement', 'ReturnStatement'])
  assertOriginalTokenTraversal(result)
})

it('recovers a block with only bindings by inserting the missing return', () => {
  const result = parseText(
    'fixture://missing-return.silk',
    'pub fn main() -> I32 { let value = 42 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')

  assert.notStrictEqual(block, undefined)
  if (block === undefined) return
  const statements = block.children.filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['BindingStatement', 'ReturnStatement'])
  const returnStatement = SyntaxTree.directNode(block, 'ReturnStatement')
  assert.strictEqual(
    returnStatement?.children.some(
      (element) => SyntaxTree.isMissingToken(element) && element.expected === 'ReturnKeyword',
    ),
    true,
  )
  assertOriginalTokenTraversal(result)
})

it('recovers a bare move with a missing identifier', () => {
  const result = parseText(
    'fixture://bare-move.silk',
    'pub fn main() -> I32 { let value = move return 0 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const binding = block === undefined ? undefined : SyntaxTree.directNode(block, 'BindingStatement')
  const move = binding === undefined ? undefined : SyntaxTree.directNode(binding, 'MoveExpression')

  assert.notStrictEqual(move, undefined)
  if (move === undefined) return
  assert.strictEqual(
    missingLeaves(move).some((element) => element.expected === 'Identifier'),
    true,
  )
  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('keeps statements after the return statement as concrete branches', () => {
  const result = parseText(
    'fixture://trailing-statement.silk',
    'pub fn main() -> I32 { return 0 let late = 1 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')

  assert.notStrictEqual(block, undefined)
  if (block === undefined) return
  const statements = block.children.filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['ReturnStatement', 'BindingStatement'])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(
    reconstructedBytes(result),
    ascii('pub fn main() -> I32 { return 0 let late = 1 }'),
  )
})

it('parses signed literals and qualified callees', () => {
  const result = parseText(
    'fixture://arith.silk',
    'pub fn main() -> I32 { return I32.add(-8, 50) }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const returnStatement =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ReturnStatement')
  const call =
    returnStatement === undefined
      ? undefined
      : SyntaxTree.directNode(returnStatement, 'CallExpression')

  assert.notStrictEqual(call, undefined)
  if (call === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(call, 'Dot'), undefined)
  const identifiers = call.children.filter(
    (element) => SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
  assert.strictEqual(identifiers.length, 2)
  const argumentList = SyntaxTree.directNode(call, 'ArgumentList')
  const firstArgument =
    argumentList === undefined
      ? undefined
      : SyntaxTree.directNode(argumentList, 'IntegerLiteralExpression')
  assert.notStrictEqual(firstArgument, undefined)
  if (firstArgument === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(firstArgument, 'Minus'), undefined)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
})

it('recovers a missing operation name after the dot', () => {
  const result = parseText(
    'fixture://missing-operation.silk',
    'pub fn main() -> I32 { return I32.(1, 2) }',
  )

  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('recovers a dangling minus before the closing brace', () => {
  const result = parseText('fixture://dangling-minus.silk', 'pub fn main() -> I32 { return - }')

  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('parses conditionals with both arms and boolean literals', () => {
  const result = parseText(
    'fixture://conditional.silk',
    'pub fn main() -> I32 { if flag { return 1 } else { return 2 } return 0 }',
  )
  const fn = directFunctionDeclarations(result.root).at(0)
  const block = fn === undefined ? undefined : SyntaxTree.directNode(fn, 'Block')
  const conditional =
    block === undefined ? undefined : SyntaxTree.directNode(block, 'ConditionalStatement')

  assert.notStrictEqual(conditional, undefined)
  if (conditional === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(conditional, 'IfKeyword'), undefined)
  assert.notStrictEqual(SyntaxTree.directToken(conditional, 'ElseKeyword'), undefined)
  assert.strictEqual(SyntaxTree.directNodes(conditional, 'Block').length, 2)
  assert.notStrictEqual(SyntaxTree.directNode(conditional, 'IdentifierExpression'), undefined)
  const statements = (block?.children ?? []).filter(SyntaxTree.isNode).map((node) => node.kind)
  assert.deepEqual(statements, ['ConditionalStatement', 'ReturnStatement'])
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)

  const booleans = parseText(
    'fixture://booleans.silk',
    'pub fn main() -> I32 { let flag = false return true }',
  )
  const boolFn = directFunctionDeclarations(booleans.root).at(0)
  const boolBlock = boolFn === undefined ? undefined : SyntaxTree.directNode(boolFn, 'Block')
  const binding =
    boolBlock === undefined ? undefined : SyntaxTree.directNode(boolBlock, 'BindingStatement')
  assert.notStrictEqual(
    binding === undefined ? undefined : SyntaxTree.directNode(binding, 'BooleanLiteralExpression'),
    undefined,
  )
  assert.deepEqual(booleans.parserDiagnostics, [])
})

it('recovers a missing condition before the arm brace', () => {
  const result = parseText(
    'fixture://missing-condition.silk',
    'pub fn main() -> I32 { if { return 1 } return 0 }',
  )

  assert.deepEqual(
    result.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
  assertOriginalTokenTraversal(result)
})

it('recovers an arm missing its closing brace before the trailing return', () => {
  const result = parseText(
    'fixture://missing-arm-brace.silk',
    'pub fn main() -> I32 { if flag { return 1 return 0 }',
  )

  assert.strictEqual(
    result.parserDiagnostics.every((diagnostic) => diagnostic.code === 'PAR0001'),
    true,
  )
  assertOriginalTokenTraversal(result)
})

it('parses arithmetic and equality by the closed precedence table', () => {
  const source = 'pub fn main() -> Bool { return 1 + 2 * 3 == 7 }'
  const result = parseText('memory/operators-precedence', source)
  const expressions = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'InfixExpression',
  )
  const [equality, addition, multiplication] = expressions

  assert.strictEqual(expressions.length, 3)
  assert.notStrictEqual(equality, undefined)
  assert.notStrictEqual(addition, undefined)
  assert.notStrictEqual(multiplication, undefined)
  if (equality === undefined || addition === undefined || multiplication === undefined) return
  assert.notStrictEqual(SyntaxTree.directToken(equality, 'EqualEqual'), undefined)
  assert.notStrictEqual(SyntaxTree.directToken(addition, 'Plus'), undefined)
  assert.notStrictEqual(SyntaxTree.directToken(multiplication, 'Star'), undefined)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses grouping and right-associative prefix expressions losslessly', () => {
  const source = 'pub fn main(value: I32) -> I32 { return -(-(value + 1)) }'
  const result = parseText('memory/operator-prefix', source)
  const prefixes = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'PrefixExpression',
  )
  const groups = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'GroupedExpression',
  )

  assert.strictEqual(prefixes.length, 2)
  assert.strictEqual(groups.length, 2)
  assert.strictEqual(
    prefixes.every((node) => SyntaxTree.directToken(node, 'Minus') !== undefined),
    true,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses qualified pipelines left-to-right with optional argument lists', () => {
  const source =
    'pub fn main() -> I32 { return 2 |> I32.add(3) |> I32.multiply(4) }\n' +
    'pub fn flag() -> Bool { return true |> Bool.not }'
  const result = parseText('memory/operator-pipelines', source)
  const pipelines = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'PipelineExpression',
  )
  const targets = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'PipelineTarget',
  )

  assert.strictEqual(pipelines.length, 3)
  assert.strictEqual(targets.length, 3)
  assert.deepEqual(
    targets.map((target) => SyntaxTree.directNodes(target, 'ArgumentList').length),
    [1, 1, 0],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('bounds operator recovery at expression and declaration boundaries', () => {
  const source =
    'pub fn missingOperand() -> I32 { return 1 + }\n' +
    'pub fn missingGroup() -> I32 { return (1 + 2 }\n' +
    'pub fn chained() -> Bool { return 1 < 2 < 3 }\n' +
    'pub fn after() -> I32 { return 4 }'
  const result = parseText('memory/operator-recovery', source)
  const declarations = directFunctionDeclarations(result.root)
  const after = declarations.at(-1)

  assert.strictEqual(declarations.length, 4)
  assert.strictEqual(
    missingLeaves(result.root).some((leaf) => leaf.expected === 'DecimalInteger'),
    true,
  )
  assert.strictEqual(
    missingLeaves(result.root).some((leaf) => leaf.expected === 'RightParenthesis'),
    true,
  )
  assert.strictEqual(errorNodes(result.root).length > 0, true)
  assert.notStrictEqual(after, undefined)
  if (after === undefined) return
  assert.strictEqual(directTokenText(result, after, 'Identifier'), 'after')
  assert.deepEqual(missingLeaves(after), [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses nominal struct declarations and qualified field types losslessly', () => {
  const source = validStructSource
  const result = parseText('memory/structs', source)
  const structs = SyntaxTree.directNodes(result.root, 'StructDeclaration')
  const fields = structs.flatMap((struct) => SyntaxTree.directNodes(struct, 'StructField'))

  assert.strictEqual(structs.length, 2)
  assert.strictEqual(fields.length, 2)
  assert.deepEqual(
    fields.map((field) =>
      SyntaxTree.directNode(field, 'TypePath')
        ?.children.filter(SyntaxTree.isToken)
        .map((token) => token.kind),
    ),
    [
      ['Whitespace', 'Identifier'],
      ['Whitespace', 'Identifier', 'Dot', 'Identifier'],
    ],
  )
  assert.notStrictEqual(
    SyntaxTree.directToken(structs.at(0) ?? result.root, 'PubKeyword'),
    undefined,
  )
  assert.strictEqual(SyntaxTree.directNodes(structs.at(1) ?? result.root, 'StructField').length, 0)
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('keeps damaged struct fields and following declarations separate', () => {
  const source = damagedStructSource
  const result = parseText('memory/damaged-structs', source)
  const structs = SyntaxTree.directNodes(result.root, 'StructDeclaration')
  const after = SyntaxTree.directNode(result.root, 'FunctionDeclaration')

  assert.strictEqual(structs.length, 2)
  assert.notStrictEqual(after, undefined)
  assert.deepEqual(
    missingLeaves(structs.at(0) ?? result.root).map((leaf) => leaf.expected),
    ['Identifier', 'Identifier'],
  )
  assert.deepEqual(
    missingLeaves(structs.at(1) ?? result.root).map((leaf) => leaf.expected),
    ['RightBrace'],
  )
  assert.deepEqual(after === undefined ? [] : missingLeaves(after), [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses mode-aware match expressions and nested patterns losslessly', () => {
  const source = `pub struct Span { start: I32 end: I32 }
pub struct Token { kind: I32 span: Span }
pub struct End {}
pub fn inspect(event: Token | End) -> I32 {
  let code = match move event {
    Token { kind, span: Span { start: offset, .. } } if true => offset
    End {} => 0
  }
  return code
}`
  const result = parseText('memory/match-expression', source)
  const match = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'MatchExpression',
  )

  assert.notStrictEqual(match, undefined)
  if (match === undefined) return
  assert.strictEqual(SyntaxTree.directNodes(match, 'MatchAccess').length, 1)
  assert.strictEqual(SyntaxTree.directNodes(match, 'MatchArm').length, 2)
  assert.strictEqual(
    descendants(match).filter(
      (element) => SyntaxTree.isNode(element) && element.kind === 'NominalPattern',
    ).length,
    3,
  )
  assert.strictEqual(
    descendants(match).some(
      (element) => SyntaxTree.isNode(element) && element.kind === 'RestPattern',
    ),
    true,
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('parses bare, shared, and exclusive matches in expression positions', () => {
  const source = `pub struct Token { kind: I32 }
pub fn bare(event: Token) -> I32 { return match event { Token { kind } => kind } }
pub fn shared(event: Token) -> I32 { return I32.add(match &event { Token { kind } => kind }, 1) }
pub fn exclusive(event: Token) -> I32 { let value = match &mut event { _ => 0 } return value }`
  const result = parseText('memory/match-modes', source)
  const matches = descendants(result.root).filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'MatchExpression',
  )

  assert.strictEqual(matches.length, 3)
  assert.deepEqual(
    matches.map((match) =>
      SyntaxTree.directNode(match, 'MatchAccess')
        ?.children.filter(SyntaxTree.isToken)
        .map((token) => token.kind),
    ),
    [[], ['Whitespace', 'Ampersand'], ['Whitespace', 'Ampersand', 'MutKeyword']],
  )
  assert.deepEqual(result.parserDiagnostics, [])
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('keeps a missing match arm arrow local to its arm', () => {
  const source = `pub struct Token { kind: I32 }
pub struct End {}
pub fn inspect(event: Token | End) -> I32 {
  return match event {
    Token { kind } kind
    End {} => 0
  }
}`
  const result = parseText('memory/damaged-match-arm', source)
  const match = descendants(result.root).find(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) && element.kind === 'MatchExpression',
  )

  assert.notStrictEqual(match, undefined)
  if (match === undefined) return
  assert.strictEqual(SyntaxTree.directNodes(match, 'MatchArm').length, 2)
  assert.deepEqual(
    missingLeaves(match).map((leaf) => leaf.expected),
    ['FatArrow'],
  )
  assertOriginalTokenTraversal(result)
  assert.deepEqual(reconstructedBytes(result), ascii(source))
})

it('bounds damaged pattern fields, nesting, braces, and guards at their arm', () => {
  const cases: ReadonlyArray<readonly [string, string, Token.TokenKind]> = [
    ['missing-pattern-name', '{ kind } => 1', 'Identifier'],
    ['missing-binding-name', 'Token { kind: , .. } => 1', 'Identifier'],
    ['missing-field-comma', 'Token { kind other } => 1', 'Comma'],
    ['missing-nested-colon', 'Token { child Inner {}, .. } => 1', 'Colon'],
    ['missing-pattern-brace', 'Token { kind, .. if true => 1', 'RightBrace'],
    ['missing-guard-expression', 'Token { kind, .. } if => 1', 'Identifier'],
  ]

  for (const [name, damagedArm, expected] of cases) {
    const source = `pub struct Inner {}
pub struct Token { kind: I32 other: I32 child: Inner }
pub struct End {}
pub fn inspect(event: Token | End) -> I32 {
  return match event {
    ${damagedArm}
    End {} => 0
  }
}
pub fn after() -> I32 { return 2 }`
    const result = parseText(`memory/${name}`, source)
    const match = descendants(result.root).find(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'MatchExpression',
    )
    const after = directFunctionDeclarations(result.root).at(-1)

    assert.notStrictEqual(match, undefined, name)
    assert.notStrictEqual(after, undefined, name)
    if (match === undefined || after === undefined) continue
    assert.strictEqual(
      missingLeaves(match).some((leaf) => leaf.expected === expected),
      true,
      name,
    )
    assert.strictEqual(SyntaxTree.directNodes(match, 'MatchArm').length, 2, name)
    assert.deepEqual(missingLeaves(after), [], name)
    assertOriginalTokenTraversal(result)
    assert.deepEqual(reconstructedBytes(result), ascii(source), name)
  }
})
