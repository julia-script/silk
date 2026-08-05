import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as Lexer from '../src/Lexer.js'
import * as SourceFile from '../src/SourceFile.js'
import type * as Token from '../src/Token.js'
import {
  expectedRecoveryDiagnostics,
  expectedTokens,
  source as fixtureSource,
  recoverySource,
} from './fixtures/BootstrapLexerFixture.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const tokenView = (
  source: SourceFile.SourceFile,
  token: Token.Token,
): {
  readonly kind: string
  readonly start: number
  readonly end: number
  readonly slice: string
} => {
  const bytes = Option.getOrThrow(SourceFile.slice(source, token.span))
  return {
    kind: token.kind,
    start: token.span.start,
    end: token.span.end,
    slice: String.fromCharCode(...bytes),
  }
}

it('matches the bootstrap fixture with exact kinds, spans, and slices', () => {
  const source = SourceFile.make('fixture://bootstrap.silk', ascii(fixtureSource))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.map((token) => tokenView(source, token)),
    expectedTokens,
  )
  assert.deepEqual(result.diagnostics, [])
})

it('recognizes keywords only as complete identifiers', () => {
  const result = Lexer.lex(
    SourceFile.make(
      'memory://keywords.silk',
      ascii('pub publicity fn fnx return returning _x x2 I32'),
    ),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'PubKeyword',
      'Identifier',
      'FnKeyword',
      'Identifier',
      'ReturnKeyword',
      'Identifier',
      'Identifier',
      'Identifier',
      'Identifier',
      'EndOfFile',
    ],
  )
})

it('recognizes typed parameter and argument punctuation with exact spans', () => {
  const source = SourceFile.make(
    'memory://parameter-punctuation.silk',
    ascii('identity(value: I32, other: I32)'),
  )
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind === 'Colon' || token.kind === 'Comma')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'Colon', start: 14, end: 15, slice: ':' },
      { kind: 'Comma', start: 19, end: 20, slice: ',' },
      { kind: 'Colon', start: 26, end: 27, slice: ':' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('keeps comments as trivia and leaves line endings for whitespace tokens', () => {
  const withNewline = Lexer.lex(
    SourceFile.make('memory://comment-newline.silk', ascii('// note\r\nfn')),
  )
  const finalComment = Lexer.lex(
    SourceFile.make('memory://final-comment.silk', ascii('// no newline')),
  )

  assert.deepEqual(
    withNewline.tokens.map((token) => token.kind),
    ['LineComment', 'Whitespace', 'FnKeyword', 'EndOfFile'],
  )
  assert.deepEqual(
    finalComment.tokens.map((token) => token.kind),
    ['LineComment', 'EndOfFile'],
  )
})

it('uses longest match for arrows and groups maximal unsupported byte regions', () => {
  const source = SourceFile.make('memory://recovery.silk', recoverySource)
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.map((token) => ({
      kind: token.kind,
      start: token.span.start,
      end: token.span.end,
    })),
    [
      { kind: 'Invalid', start: 0, end: 2 },
      { kind: 'Whitespace', start: 2, end: 3 },
      { kind: 'Invalid', start: 3, end: 5 },
      { kind: 'Whitespace', start: 5, end: 6 },
      { kind: 'Arrow', start: 6, end: 8 },
      { kind: 'Identifier', start: 8, end: 9 },
      { kind: 'EndOfFile', start: 9, end: 9 },
    ],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      message: diagnostic.message,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    expectedRecoveryDiagnostics,
  )
})

it('reconstructs every input byte exactly once and is deterministic', () => {
  const bytes = Uint8Array.of(
    ...ascii('pub fn main() -> I32 {\nreturn 42\n}'),
    0x00,
    0xff,
    ...ascii('//tail'),
  )
  const source = SourceFile.make('memory://reconstruct.silk', bytes)
  const first = Lexer.lex(source)
  const second = Lexer.lex(source)
  const reconstructed = first.tokens
    .filter((token) => token.kind !== 'EndOfFile')
    .flatMap((token) => Array.from(Option.getOrThrow(SourceFile.slice(source, token.span))))

  assert.deepEqual(Uint8Array.from(reconstructed), bytes)
  assert.deepEqual(first, second)
  assert.strictEqual(first.tokens.at(-1)?.kind, 'EndOfFile')
  assert.strictEqual(first.tokens.at(-1)?.span.start, bytes.length)
  assert.strictEqual(first.tokens.at(-1)?.span.end, bytes.length)
})

it('terminates on empty and wholly invalid input', () => {
  const empty = Lexer.lex(SourceFile.make('memory://empty.silk', new Uint8Array()))
  const invalid = Lexer.lex(SourceFile.make('memory://invalid.silk', Uint8Array.of(0, 1, 2)))

  assert.deepEqual(
    empty.tokens.map((token) => token.kind),
    ['EndOfFile'],
  )
  assert.deepEqual(
    invalid.tokens.map((token) => token.kind),
    ['Invalid', 'EndOfFile'],
  )
  assert.strictEqual(invalid.diagnostics.length, 1)
})

it('distinguishes documentation comments from plain line comments', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://doc-comments.silk', ascii('/// doc\n// note\nfn')),
  )
  const final = Lexer.lex(SourceFile.make('memory://final-doc.silk', ascii('/// tail')))

  assert.deepEqual(
    result.tokens.map((token) => token.kind),
    ['DocComment', 'Whitespace', 'LineComment', 'Whitespace', 'FnKeyword', 'EndOfFile'],
  )
  assert.deepEqual(
    result.tokens.slice(0, 1).map((token) => tokenView(result.source, token)),
    [{ kind: 'DocComment', start: 0, end: 7, slice: '/// doc' }],
  )
  assert.deepEqual(
    final.tokens.map((token) => token.kind),
    ['DocComment', 'EndOfFile'],
  )
})
