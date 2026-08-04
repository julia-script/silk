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
  denseTriviaSource,
  type ExpectedNodeShape,
  invalidUtf8Source,
  missingNameSource,
  missingRightBraceSource,
  unexpectedPunctuationSource,
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
  const result = parseBytes('fixture://empty.silk', new Uint8Array())

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
  const first = parseText('fixture://deterministic.silk', unexpectedPunctuationSource)
  const second = parseText('fixture://deterministic.silk', unexpectedPunctuationSource)

  assert.deepEqual(nodeShape(first.root), nodeShape(second.root))
  assert.deepEqual(diagnosticView(first), diagnosticView(second))
  assert.deepEqual(first.lexical.diagnostics, second.lexical.diagnostics)
  assert.deepEqual(reconstructedBytes(first), reconstructedBytes(second))
})
