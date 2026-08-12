import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as IntegerLiteral from '../src/internal/IntegerLiteral.js'
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
      ascii('pub publicity fn fnx return returning _x x2 i32'),
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

it('reserves service only as a complete declaration keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://service-keyword.silk', ascii('service services serviceLogger')),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['ServiceKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('reserves interface only as a complete declaration keyword', () => {
  const result = Lexer.lex(
    SourceFile.make(
      'memory://interface-keyword.silk',
      ascii('interface interfaces interfaceValue'),
    ),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['InterfaceKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('recognizes all static-literal forms with exact multiline boundaries', () => {
  const text = '"one" b"two" """line 1\r\n// still text\nline 3""" b"""a \\"b\\" c""" tail'
  const source = SourceFile.make('memory://literal-forms.silk', ascii(text))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'TextLiteral', start: 0, end: 5, slice: '"one"' },
      { kind: 'ByteStringLiteral', start: 6, end: 12, slice: 'b"two"' },
      {
        kind: 'TextLiteral',
        start: 13,
        end: 47,
        slice: '"""line 1\r\n// still text\nline 3"""',
      },
      { kind: 'ByteStringLiteral', start: 48, end: 64, slice: 'b"""a \\"b\\" c"""' },
      { kind: 'Identifier', start: 65, end: 69, slice: 'tail' },
      { kind: 'EndOfFile', start: 69, end: 69, slice: '' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('diagnoses reserved modifiers and unterminated delimiters once with committed recovery', () => {
  const unknown = Lexer.lex(
    SourceFile.make('memory://unknown-modifiers.silk', ascii('future"value" br"""value"""')),
  )
  assert.deepEqual(
    unknown.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['InvalidStaticLiteral', 'InvalidStaticLiteral', 'EndOfFile'],
  )
  assert.deepEqual(
    unknown.diagnostics.map(({ code, reason }) => ({ code, reason })),
    [
      {
        code: 'LEX0002',
        reason: { _tag: 'UnknownLiteralModifier', modifier: 'future' },
      },
      { code: 'LEX0002', reason: { _tag: 'UnknownLiteralModifier', modifier: 'br' } },
    ],
  )

  const single = Lexer.lex(
    SourceFile.make('memory://unterminated-single.silk', ascii('"broken\r\nnext')),
  )
  assert.deepEqual(
    single.tokens.map((token) => token.kind),
    ['InvalidStaticLiteral', 'Whitespace', 'Identifier', 'EndOfFile'],
  )
  assert.deepEqual(
    single.diagnostics.map((diagnostic) => diagnostic.code),
    ['LEX0003'],
  )
  assert.deepEqual(
    single.tokens.map((token) => [token.span.start, token.span.end]),
    [
      [0, 7],
      [7, 9],
      [9, 13],
      [13, 13],
    ],
  )

  const multiline = Lexer.lex(
    SourceFile.make(
      'memory://unterminated-multiline.silk',
      ascii('"""code-like\nfn apparent() { return 1 }'),
    ),
  )
  assert.deepEqual(
    multiline.tokens.map((token) => token.kind),
    ['InvalidStaticLiteral', 'EndOfFile'],
  )
  assert.deepEqual(
    multiline.diagnostics.map((diagnostic) => diagnostic.code),
    ['LEX0003'],
  )
  assert.strictEqual(multiline.tokens[0]?.span.end, multiline.source.bytes.length)
})

it('retains decimal fractions and exponent spellings as exact float tokens', () => {
  const source = SourceFile.make('memory://floats.silk', ascii('1.25e-3 2E+4 3.0 4'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'DecimalFloat', start: 0, end: 7, slice: '1.25e-3' },
      { kind: 'DecimalFloat', start: 8, end: 12, slice: '2E+4' },
      { kind: 'DecimalFloat', start: 13, end: 16, slice: '3.0' },
      { kind: 'DecimalInteger', start: 17, end: 18, slice: '4' },
    ],
  )
})

it('lexes every base prefix as one integer token with its exact slice', () => {
  const source = SourceFile.make(
    'memory://base-prefixes.silk',
    ascii('0xff 0XFF 0b1010 0B1 0o777 0O7'),
  )
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'DecimalInteger', start: 0, end: 4, slice: '0xff' },
      { kind: 'DecimalInteger', start: 5, end: 9, slice: '0XFF' },
      { kind: 'DecimalInteger', start: 10, end: 16, slice: '0b1010' },
      { kind: 'DecimalInteger', start: 17, end: 20, slice: '0B1' },
      { kind: 'DecimalInteger', start: 21, end: 26, slice: '0o777' },
      { kind: 'DecimalInteger', start: 27, end: 30, slice: '0O7' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('keeps unprefixed zero and decimal fractions at their existing kinds', () => {
  const source = SourceFile.make('memory://unprefixed-zero.silk', ascii('0 0.5 00 0e2'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'DecimalInteger', start: 0, end: 1, slice: '0' },
      { kind: 'DecimalFloat', start: 2, end: 5, slice: '0.5' },
      { kind: 'DecimalInteger', start: 6, end: 8, slice: '00' },
      { kind: 'DecimalFloat', start: 9, end: 12, slice: '0e2' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('stops a prefixed literal before any fraction, exponent, or foreign digit', () => {
  const source = SourceFile.make('memory://prefixed-boundaries.silk', ascii('0xff.5 0b1e5 0o18'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'DecimalInteger', start: 0, end: 4, slice: '0xff' },
      { kind: 'Dot', start: 4, end: 5, slice: '.' },
      { kind: 'DecimalInteger', start: 5, end: 6, slice: '5' },
      { kind: 'DecimalInteger', start: 7, end: 10, slice: '0b1' },
      { kind: 'Identifier', start: 10, end: 12, slice: 'e5' },
      { kind: 'DecimalInteger', start: 13, end: 16, slice: '0o1' },
      { kind: 'DecimalInteger', start: 16, end: 17, slice: '8' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('diagnoses a base prefix without digits exactly once and resumes lexing', () => {
  const source = SourceFile.make('memory://empty-base-prefix.silk', ascii('0x tail'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens.map((token) => tokenView(source, token)),
    [
      { kind: 'Invalid', start: 0, end: 2, slice: '0x' },
      { kind: 'Whitespace', start: 2, end: 3, slice: ' ' },
      { kind: 'Identifier', start: 3, end: 7, slice: 'tail' },
      { kind: 'EndOfFile', start: 7, end: 7, slice: '' },
    ],
  )
  assert.deepEqual(
    result.diagnostics.map(({ code, reason }) => ({ code, reason })),
    [{ code: 'LEX0004', reason: { _tag: 'MissingBaseDigits', radix: 16 } }],
  )

  const others = Lexer.lex(SourceFile.make('memory://empty-base-prefixes.silk', ascii('0b 0O')))
  assert.deepEqual(
    others.diagnostics.map(({ code, reason }) => ({ code, reason })),
    [
      { code: 'LEX0004', reason: { _tag: 'MissingBaseDigits', radix: 2 } },
      { code: 'LEX0004', reason: { _tag: 'MissingBaseDigits', radix: 8 } },
    ],
  )
})

it('accepts a digit separator between two digits of every base', () => {
  const source = SourceFile.make(
    'memory://digit-separators.silk',
    ascii('1_000 1_048_576 0b1010_0000 0xff_ff 0o1_7'),
  )
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'DecimalInteger', start: 0, end: 5, slice: '1_000' },
      { kind: 'DecimalInteger', start: 6, end: 15, slice: '1_048_576' },
      { kind: 'DecimalInteger', start: 16, end: 27, slice: '0b1010_0000' },
      { kind: 'DecimalInteger', start: 28, end: 35, slice: '0xff_ff' },
      { kind: 'DecimalInteger', start: 36, end: 41, slice: '0o1_7' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
  assert.strictEqual(IntegerLiteral.magnitude('1_000'), IntegerLiteral.magnitude('1000'))
})

it('accepts a digit separator between two digits of a float literal', () => {
  const source = SourceFile.make('memory://separated-floats.silk', ascii('1_000.5 1.000_5 1e1_0'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'DecimalFloat', start: 0, end: 7, slice: '1_000.5' },
      { kind: 'DecimalFloat', start: 8, end: 15, slice: '1.000_5' },
      { kind: 'DecimalFloat', start: 16, end: 21, slice: '1e1_0' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('keeps a leading underscore an identifier rather than a separated literal', () => {
  const source = SourceFile.make('memory://leading-separator.silk', ascii('_1 x_1'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'Identifier', start: 0, end: 2, slice: '_1' },
      { kind: 'Identifier', start: 3, end: 6, slice: 'x_1' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('diagnoses every misplaced digit separator exactly once over the literal span', () => {
  const cases: ReadonlyArray<readonly [string, string]> = [
    ['1_', '1_'],
    ['1__0', '1__0'],
    ['0x_ff', '0x_ff'],
    ['1_.5', '1_.5'],
    ['1._5', '1._5'],
    ['1_e5', '1_e5'],
    ['1e_5', '1e_5'],
    ['1e+_5', '1e+_5'],
    ['1.5_', '1.5_'],
  ]
  for (const [spelling, expected] of cases) {
    const source = SourceFile.make(`memory://misplaced-${spelling}.silk`, ascii(spelling))
    const result = Lexer.lex(source)
    assert.deepEqual(
      result.diagnostics.map(({ code, reason }) => ({ code, reason })),
      [{ code: 'LEX0005', reason: { _tag: 'InvalidDigitSeparator' } }],
      spelling,
    )
    const invalid = result.tokens.filter((token) => token.kind === 'Invalid')
    assert.deepEqual(
      invalid.map((token) => tokenView(source, token).slice),
      [expected],
      spelling,
    )
    assert.deepEqual(
      result.diagnostics.map((diagnostic) => [diagnostic.span.start, diagnostic.span.end]),
      [[invalid[0]?.span.start, invalid[0]?.span.end]],
      spelling,
    )
  }
})

it('keeps a base prefix followed only by separators a missing-digits diagnostic', () => {
  const source = SourceFile.make('memory://separator-only-prefix.silk', ascii('0x_'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens.map((token) => tokenView(source, token)),
    [
      { kind: 'Invalid', start: 0, end: 3, slice: '0x_' },
      { kind: 'EndOfFile', start: 3, end: 3, slice: '' },
    ],
  )
  assert.deepEqual(
    result.diagnostics.map(({ code, reason }) => ({ code, reason })),
    [{ code: 'LEX0004', reason: { _tag: 'MissingBaseDigits', radix: 16 } }],
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
      ascii('effect fn work() -> i32 ! Error { fail move error } run work() flower runner failed'),
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

it('recognizes unsafe and conformance keywords only as complete words', () => {
  const result = Lexer.lex(
    SourceFile.make(
      'memory://ownership-keywords.silk',
      ascii('unsafe unsafely impl implement for format'),
    ),
  )
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'UnsafeKeyword',
      'Identifier',
      'ImplKeyword',
      'Identifier',
      'ForKeyword',
      'Identifier',
      'EndOfFile',
    ],
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
    ascii('identity(value: i32, other: i32)'),
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
  const source = SourceFile.make('memory://fixed-array.silk', ascii('[i32; 4]'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'LeftBracket', start: 0, end: 1, slice: '[' },
      { kind: 'Identifier', start: 1, end: 4, slice: 'i32' },
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
    ...ascii('pub fn main() -> i32 {\nreturn 42\n}'),
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

it('distinguishes declaration, module, and plain line comments', () => {
  const result = Lexer.lex(
    SourceFile.make(
      'memory://doc-comments.silk',
      ascii('/// item\n//! module\n// note\n//// plain\nfn'),
    ),
  )
  const final = Lexer.lex(SourceFile.make('memory://final-doc.silk', ascii('/// tail')))

  assert.deepEqual(
    result.tokens.map((token) => token.kind),
    [
      'DocComment',
      'Whitespace',
      'ModuleDocComment',
      'Whitespace',
      'LineComment',
      'Whitespace',
      'LineComment',
      'Whitespace',
      'FnKeyword',
      'EndOfFile',
    ],
  )
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind === 'DocComment' || token.kind === 'ModuleDocComment')
      .map((token) => tokenView(result.source, token)),
    [
      { kind: 'DocComment', start: 0, end: 8, slice: '/// item' },
      { kind: 'ModuleDocComment', start: 9, end: 19, slice: '//! module' },
    ],
  )
  assert.deepEqual(
    final.tokens.map((token) => token.kind),
    ['DocComment', 'EndOfFile'],
  )
})

it('lexes signed literals and qualified callees with minus and dot tokens', () => {
  const result = Lexer.lex(SourceFile.make('memory://arith.silk', ascii('-42 i32.add')))

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
