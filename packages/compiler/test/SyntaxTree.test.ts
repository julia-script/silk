import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Diagnostic from '../src/Diagnostic.js'
import * as Lexer from '../src/Lexer.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SyntaxTree from '../src/SyntaxTree.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it('derives concrete and empty node spans from one source', () => {
  const source = SourceFile.make('memory://syntax-tree.silk', ascii('pub'))
  const lexical = Lexer.lex(source)
  const token = lexical.tokens.at(0)
  assert.notStrictEqual(token, undefined)
  if (token === undefined) return

  const concrete = Option.getOrThrow(SyntaxTree.make(source, 'SourceFile', [token], 0))
  const empty = Option.getOrThrow(SyntaxTree.make(source, 'ParameterList', [], 2))

  assert.deepEqual({ start: concrete.span.start, end: concrete.span.end }, { start: 0, end: 3 })
  assert.deepEqual({ start: empty.span.start, end: empty.span.end }, { start: 2, end: 2 })
  assert.strictEqual(Object.isFrozen(concrete.children), true)
  assert.strictEqual(Object.isFrozen(concrete), true)
})

it('retains original token objects and distinguishes every element family', () => {
  const source = SourceFile.make('memory://elements.silk', ascii('pub'))
  const lexical = Lexer.lex(source)
  const token = lexical.tokens.at(0)
  assert.notStrictEqual(token, undefined)
  if (token === undefined) return
  const missing = Option.getOrThrow(SyntaxTree.missingToken(source, 'FnKeyword', 3))
  const inner = Option.getOrThrow(SyntaxTree.make(source, 'FunctionDeclaration', [token], 0))
  const root = Option.getOrThrow(SyntaxTree.make(source, 'SourceFile', [inner, missing], 0))

  assert.strictEqual(SyntaxTree.isNode(inner), true)
  assert.strictEqual(SyntaxTree.isToken(token), true)
  assert.strictEqual(SyntaxTree.isMissingToken(missing), true)
  assert.strictEqual(SyntaxTree.tokens(root).at(0), token)
  assert.deepEqual(
    { expected: missing.expected, start: missing.span.start, end: missing.span.end },
    { expected: 'FnKeyword', start: 3, end: 3 },
  )
})

it('rejects foreign and overlapping child spans', () => {
  const source = SourceFile.make('memory://source.silk', ascii('pub'))
  const foreign = SourceFile.make('memory://foreign.silk', ascii('pub'))
  const sourceToken = Lexer.lex(source).tokens.at(0)
  const foreignToken = Lexer.lex(foreign).tokens.at(0)
  assert.notStrictEqual(sourceToken, undefined)
  assert.notStrictEqual(foreignToken, undefined)
  if (sourceToken === undefined || foreignToken === undefined) return

  assert.strictEqual(Option.isNone(SyntaxTree.make(source, 'SourceFile', [foreignToken], 0)), true)
  assert.strictEqual(
    Option.isNone(SyntaxTree.make(source, 'SourceFile', [sourceToken, sourceToken], 0)),
    true,
  )
})

it('models missing and unexpected parser diagnostics as ordered source data', () => {
  const source = SourceFile.make('memory://diagnostics.silk', ascii('@'))
  const lexical = Lexer.lex(source)
  const invalid = lexical.tokens.at(0)
  assert.notStrictEqual(invalid, undefined)
  if (invalid === undefined) return
  const missing = Option.getOrThrow(SyntaxTree.missingToken(source, 'Identifier', 0))
  const error = Option.getOrThrow(SyntaxTree.make(source, 'Error', [invalid], 0))

  const diagnostics = [
    Diagnostic.missingToken(missing.expected, missing.span),
    Diagnostic.unexpectedTokens(error.span),
  ]
  assert.deepEqual(
    diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      message: diagnostic.message,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    [
      { code: 'PAR0001', message: 'Expected identifier', start: 0, end: 0 },
      { code: 'PAR0002', message: 'Unexpected token sequence', start: 0, end: 1 },
    ],
  )
})
