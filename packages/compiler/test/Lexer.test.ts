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

it('recognizes struct only as a complete keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://struct-keyword.silk', ascii('struct structure structs')),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['StructKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('recognizes the effect execution and failure keywords without prefix capture', () => {
  const result = Lexer.lex(
    SourceFile.make(
      'memory://effect-keywords.silk',
      ascii('effect fn work() -> I32 ! Error { fail move error } run work() flower runner failed'),
    ),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'EffectKeyword',
      'FnKeyword',
      'Identifier',
      'LeftParenthesis',
      'RightParenthesis',
      'Arrow',
      'Identifier',
      'Bang',
      'Identifier',
      'LeftBrace',
      'FailKeyword',
      'MoveKeyword',
      'Identifier',
      'RightBrace',
      'RunKeyword',
      'Identifier',
      'LeftParenthesis',
      'RightParenthesis',
      'Identifier',
      'Identifier',
      'Identifier',
      'EndOfFile',
    ],
  )
})

it('recognizes drop only as a complete keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://drop-keyword.silk', ascii('drop dropped dropper')),
  )
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['DropKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('recognizes once only as a complete callable-mode keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://once-keyword.silk', ascii('once fn onceOnly once_more dual')),
  )
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['OnceKeyword', 'FnKeyword', 'Identifier', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('recognizes requirement-row and role punctuation', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://effect-requirements.silk', ascii('? &Allocator@Scratch')),
  )
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Question', 'Ampersand', 'Identifier', 'At', 'Identifier', 'EndOfFile'],
  )
})

it('lexes binding statements with let, move, and equals tokens', () => {
  const source = SourceFile.make('memory://bindings.silk', ascii('let answer = move value'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['LetKeyword', 'Identifier', 'Equals', 'MoveKeyword', 'Identifier', 'EndOfFile'],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('keeps let and move keyword prefixes as identifiers', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://prefixes.silk', ascii('letter movement lets moved')),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Identifier', 'Identifier', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('lexes match access, arm, and rest punctuation with longest recognition', () => {
  const source = SourceFile.make(
    'memory://match.silk',
    ascii('match &value { Token { kind, .. } if guard => kind _ => 0 }'),
  )
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'MatchKeyword',
      'Ampersand',
      'Identifier',
      'LeftBrace',
      'Identifier',
      'LeftBrace',
      'Identifier',
      'Comma',
      'DotDot',
      'RightBrace',
      'IfKeyword',
      'Identifier',
      'FatArrow',
      'Identifier',
      'Identifier',
      'FatArrow',
      'DecimalInteger',
      'RightBrace',
      'EndOfFile',
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('keeps match prefixes separate and recognizes punctuation independently', () => {
  const source = SourceFile.make('memory://match-prefixes.silk', ascii('matcher = > . .. => &'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'Identifier', start: 0, end: 7, slice: 'matcher' },
      { kind: 'Equals', start: 8, end: 9, slice: '=' },
      { kind: 'Greater', start: 10, end: 11, slice: '>' },
      { kind: 'Dot', start: 12, end: 13, slice: '.' },
      { kind: 'DotDot', start: 14, end: 16, slice: '..' },
      { kind: 'FatArrow', start: 17, end: 19, slice: '=>' },
      { kind: 'Ampersand', start: 20, end: 21, slice: '&' },
      { kind: 'EndOfFile', start: 21, end: 21, slice: '' },
    ],
  )
})

it('distinguishes the equals token from the arrow', () => {
  const source = SourceFile.make('memory://equals.silk', ascii('= ->'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace')
      .map((token) => ({ kind: token.kind, start: token.span.start, end: token.span.end })),
    [
      { kind: 'Equals', start: 0, end: 1 },
      { kind: 'Arrow', start: 2, end: 4 },
      { kind: 'EndOfFile', start: 4, end: 4 },
    ],
  )
})

it('distinguishes a type union separator from the pipeline operator', () => {
  const result = Lexer.lex(SourceFile.make('memory://unions.silk', ascii('Token | End |> inspect')))
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Identifier', 'Pipe', 'Identifier', 'PipeGreater', 'Identifier', 'EndOfFile'],
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

it('lexes the fixed-array semicolon as punctuation with its exact span', () => {
  const source = SourceFile.make('memory://fixed-array.silk', ascii('[I32; 4]'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'LeftBracket', start: 0, end: 1, slice: '[' },
      { kind: 'Identifier', start: 1, end: 4, slice: 'I32' },
      { kind: 'Semicolon', start: 4, end: 5, slice: ';' },
      { kind: 'DecimalInteger', start: 6, end: 7, slice: '4' },
      { kind: 'RightBracket', start: 7, end: 8, slice: ']' },
      { kind: 'EndOfFile', start: 8, end: 8, slice: '' },
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
      { kind: 'LeftBracket', start: 3, end: 4 },
      { kind: 'RightBracket', start: 4, end: 5 },
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

it('lexes signed literals and qualified callees with minus and dot tokens', () => {
  const result = Lexer.lex(SourceFile.make('memory://arith.silk', ascii('-42 I32.add')))

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Minus', 'DecimalInteger', 'Identifier', 'Dot', 'Identifier', 'EndOfFile'],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('distinguishes minus from the arrow', () => {
  const result = Lexer.lex(SourceFile.make('memory://minus.silk', ascii('- -> -5')))

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Minus', 'Arrow', 'Minus', 'DecimalInteger', 'EndOfFile'],
  )
})

it('lexes the closed operator vocabulary with longest match', () => {
  const source = SourceFile.make('memory/operators', ascii('+ - * / % ! < <= > >= == != |> = ->'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'Plus',
      'Minus',
      'Star',
      'Slash',
      'Percent',
      'Bang',
      'Less',
      'LessEqual',
      'Greater',
      'GreaterEqual',
      'EqualEqual',
      'BangEqual',
      'PipeGreater',
      'Equals',
      'Arrow',
      'EndOfFile',
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('distinguishes division from line comments', () => {
  const source = SourceFile.make('memory/operator-comments', ascii('/ // comment\n/'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.map((token) => token.kind),
    ['Slash', 'Whitespace', 'LineComment', 'Whitespace', 'Slash', 'EndOfFile'],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('lexes conditional keywords as complete identifiers only', () => {
  const conditional = Lexer.lex(
    SourceFile.make(
      'memory://conditional.silk',
      ascii('if flag { return true } else { return false }'),
    ),
  )
  const prefixes = Lexer.lex(
    SourceFile.make('memory://conditional-prefixes.silk', ascii('iffy elsewhere truer falsehood')),
  )

  assert.deepEqual(
    conditional.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'IfKeyword',
      'Identifier',
      'LeftBrace',
      'ReturnKeyword',
      'TrueKeyword',
      'RightBrace',
      'ElseKeyword',
      'LeftBrace',
      'ReturnKeyword',
      'FalseKeyword',
      'RightBrace',
      'EndOfFile',
    ],
  )
  assert.deepEqual(
    prefixes.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Identifier', 'Identifier', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})
