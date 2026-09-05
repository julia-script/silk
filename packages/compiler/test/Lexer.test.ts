import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as FloatingPoint from '../src/FloatingPoint.js'
import * as DigitSeparator from '../src/internal/DigitSeparator.js'
import * as DurationLiteral from '../src/internal/DurationLiteral.js'
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

it('parses canonical duration components into exact nanoseconds', () => {
  const parsed = DurationLiteral.parse(ascii('01h05m00s'))
  assert.strictEqual(parsed._tag, 'Valid')
  if (parsed._tag === 'Invalid') return

  assert.strictEqual(parsed.nanoseconds, 3_900_000_000_000n)
  assert.deepEqual(
    parsed.components.map(({ amount, unit }) => ({ amount, unit })),
    [
      { amount: 1n, unit: 'h' },
      { amount: 5n, unit: 'm' },
      { amount: 0n, unit: 's' },
    ],
  )
})

it('keeps duration scaling exact across separators, skipped units, and u64 edges', () => {
  const cases: ReadonlyArray<readonly [string, bigint]> = [
    ['1_000ms', 1_000_000_000n],
    ['1h00s', 3_600_000_000_000n],
    ['7d', 604_800_000_000_000n],
    ['24h', 86_400_000_000_000n],
    ['60s', 60_000_000_000n],
    ['1000ns', 1_000n],
    ['1s999ms999us999ns', 1_999_999_999n],
    ['18446744073709551615ns', 18_446_744_073_709_551_615n],
    ['18446744073709551616ns', 18_446_744_073_709_551_616n],
  ]

  for (const [spelling, expected] of cases) {
    const parsed = DurationLiteral.parse(ascii(spelling))
    assert.strictEqual(parsed._tag, 'Valid', spelling)
    if (parsed._tag === 'Valid') assert.strictEqual(parsed.nanoseconds, expected, spelling)
  }
})

it('recognizes canonical duration literals as one token', () => {
  const text = '3s 1h30m30s 300ms 1d 1w2d3h4m5s6ms7us8ns 01h05m00s 60m 1000ms'
  const source = SourceFile.make('memory://durations.silk', ascii(text))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'DurationLiteral', start: 0, end: 2, slice: '3s' },
      { kind: 'DurationLiteral', start: 3, end: 11, slice: '1h30m30s' },
      { kind: 'DurationLiteral', start: 12, end: 17, slice: '300ms' },
      { kind: 'DurationLiteral', start: 18, end: 20, slice: '1d' },
      { kind: 'DurationLiteral', start: 21, end: 40, slice: '1w2d3h4m5s6ms7us8ns' },
      { kind: 'DurationLiteral', start: 41, end: 50, slice: '01h05m00s' },
      { kind: 'DurationLiteral', start: 51, end: 54, slice: '60m' },
      { kind: 'DurationLiteral', start: 55, end: 61, slice: '1000ms' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('commits malformed numeric-plus-letter candidates to one invalid duration token', () => {
  const text = '3sec 1H 1.5s 0x10s 1e5s 1h60m 1m1h 1h1h 1h_'
  const source = SourceFile.make('memory://invalid-durations.silk', ascii(text))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'InvalidDurationLiteral', start: 0, end: 4, slice: '3sec' },
      { kind: 'InvalidDurationLiteral', start: 5, end: 7, slice: '1H' },
      { kind: 'InvalidDurationLiteral', start: 8, end: 12, slice: '1.5s' },
      { kind: 'InvalidDurationLiteral', start: 13, end: 18, slice: '0x10s' },
      { kind: 'InvalidDurationLiteral', start: 19, end: 23, slice: '1e5s' },
      { kind: 'InvalidDurationLiteral', start: 24, end: 29, slice: '1h60m' },
      { kind: 'InvalidDurationLiteral', start: 30, end: 34, slice: '1m1h' },
      { kind: 'InvalidDurationLiteral', start: 35, end: 39, slice: '1h1h' },
      { kind: 'InvalidDurationLiteral', start: 40, end: 43, slice: '1h_' },
    ],
  )
  assert.deepEqual(
    result.diagnostics.map(({ code, reason, span }) => ({
      code,
      reason,
      span: [span.start, span.end],
    })),
    [
      { code: 'LEX0009', reason: { _tag: 'UnknownDurationUnit', spelling: 'sec' }, span: [1, 4] },
      { code: 'LEX0009', reason: { _tag: 'UnknownDurationUnit', spelling: 'H' }, span: [6, 7] },
      { code: 'LEX0008', reason: { _tag: 'InvalidDurationAmount' }, span: [8, 11] },
      { code: 'LEX0008', reason: { _tag: 'InvalidDurationAmount' }, span: [13, 17] },
      { code: 'LEX0008', reason: { _tag: 'InvalidDurationAmount' }, span: [19, 22] },
      {
        code: 'LEX0012',
        reason: {
          _tag: 'SubordinateDurationOutOfRange',
          unit: 'm',
          amount: '60',
          maximum: '59',
        },
        span: [26, 29],
      },
      {
        code: 'LEX0011',
        reason: { _tag: 'OutOfOrderDurationUnit', unit: 'h', previous: 'm' },
        span: [33, 34],
      },
      {
        code: 'LEX0010',
        reason: { _tag: 'RepeatedDurationUnit', unit: 'h' },
        span: [38, 39],
      },
      { code: 'LEX0008', reason: { _tag: 'InvalidDurationAmount' }, span: [42, 43] },
    ],
  )
})

it('stops duration candidates at expression and member boundaries', () => {
  const source = SourceFile.make(
    'memory://duration-boundaries.silk',
    ascii('1e5 1h + 30m + 30s 1h 30m 1h.member'),
  )
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'DecimalFloat',
      'DurationLiteral',
      'Plus',
      'DurationLiteral',
      'Plus',
      'DurationLiteral',
      'DurationLiteral',
      'DurationLiteral',
      'DurationLiteral',
      'Dot',
      'Identifier',
      'EndOfFile',
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

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

it('reserves role only as a complete declaration keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://role-keyword.silk', ascii('role roles roleValue')),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['RoleKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
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

it('recognizes raw text literals in both delimiter widths without escape processing', () => {
  const text = 'r"\\d+\\.\\d+" r"""raw\nbody\\n""" r"path\\" tail'
  const source = SourceFile.make('memory://raw-literals.silk', ascii(text))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'TextLiteral', start: 0, end: 11, slice: 'r"\\d+\\.\\d+"' },
      { kind: 'TextLiteral', start: 12, end: 29, slice: 'r"""raw\nbody\\n"""' },
      // A raw body never consults a backslash, so this closes at its own quote.
      { kind: 'TextLiteral', start: 30, end: 38, slice: 'r"path\\"' },
      { kind: 'Identifier', start: 39, end: 43, slice: 'tail' },
      { kind: 'EndOfFile', start: 43, end: 43, slice: '' },
    ],
  )
  assert.deepEqual(result.diagnostics, [])
})

it('keeps the raw modifier spelling an identifier away from a quote delimiter', () => {
  const source = SourceFile.make('memory://raw-identifier.silk', ascii('r rb"value" return'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Identifier', 'InvalidStaticLiteral', 'ReturnKeyword', 'EndOfFile'],
  )
  assert.deepEqual(
    result.diagnostics.map(({ code, reason }) => ({ code, reason })),
    [{ code: 'LEX0002', reason: { _tag: 'UnknownLiteralModifier', modifier: 'rb' } }],
  )
})

it('recovers an unterminated raw literal with exactly one diagnostic', () => {
  const source = SourceFile.make('memory://raw-unterminated.silk', ascii('r"broken\nnext'))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens.map((token) => token.kind),
    ['InvalidStaticLiteral', 'Whitespace', 'Identifier', 'EndOfFile'],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['LEX0003'],
  )

  const multiline = Lexer.lex(
    SourceFile.make('memory://raw-unterminated-multiline.silk', ascii('r"""code-like\nfn f() { }')),
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

it('rejects an exponent marker that no digit follows', () => {
  for (const spelling of ['1e', '1e+', '1e-', '1E', '1.5e']) {
    const source = SourceFile.make('memory://bad-exponent.silk', ascii(spelling))
    const result = Lexer.lex(source)
    assert.deepEqual(
      result.tokens
        .filter((token) => token.kind !== 'EndOfFile')
        .map((token) => tokenView(source, token)),
      [{ kind: 'Invalid', start: 0, end: spelling.length, slice: spelling }],
      spelling,
    )
    assert.deepEqual(
      result.diagnostics.map((diagnostic) => diagnostic.code),
      ['LEX0006'],
      spelling,
    )
  }
})

it('keeps a well-formed exponent one float token without a diagnostic', () => {
  for (const spelling of ['1e5', '1e+5', '1e-5', '1.5e10', '0e2', '1_0e5']) {
    const source = SourceFile.make('memory://good-exponent.silk', ascii(spelling))
    const result = Lexer.lex(source)
    assert.deepEqual(
      result.tokens
        .filter((token) => token.kind !== 'EndOfFile')
        .map((token) => tokenView(source, token)),
      [{ kind: 'DecimalFloat', start: 0, end: spelling.length, slice: spelling }],
      spelling,
    )
    assert.deepEqual(result.diagnostics, [], spelling)
  }
})

it('accepts as a float token only a spelling the decimal conversion can represent', () => {
  // The lexer's float grammar and FloatingPoint.fromDecimal must agree, so no accepted
  // literal can reach elaboration as an Unavailable fact carrying no diagnostic.
  const wholes = ['0', '1', '12', '1_000']
  const fractions = ['', '.5', '.0', '.1_2']
  const exponents = ['', 'e', 'E', 'e+', 'e-', 'e5', 'E+5', 'e-5', 'e0', 'e_5', 'e5_', 'e1_0']
  let accepted = 0
  for (const whole of wholes) {
    for (const fraction of fractions) {
      for (const exponent of exponents) {
        const spelling = `${whole}${fraction}${exponent}`
        const source = SourceFile.make('memory://float-agreement.silk', ascii(spelling))
        const result = Lexer.lex(source)
        const tokens = result.tokens.filter((token) => token.kind !== 'EndOfFile')
        if (tokens[0]?.kind !== 'DecimalFloat' || result.diagnostics.length !== 0) continue
        accepted += 1
        assert.notStrictEqual(
          FloatingPoint.fromDecimal(DigitSeparator.strip(Array.from(ascii(spelling))), 64),
          undefined,
          spelling,
        )
      }
    }
  }
  assert.isAbove(accepted, 0)
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

it('stops a prefixed literal at punctuation and commits a following letter candidate', () => {
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
      { kind: 'InvalidDurationLiteral', start: 7, end: 12, slice: '0b1e5' },
      { kind: 'DecimalInteger', start: 13, end: 16, slice: '0o1' },
      { kind: 'DecimalInteger', start: 16, end: 17, slice: '8' },
    ],
  )
  assert.deepEqual(
    result.diagnostics.map(({ code, reason }) => ({ code, reason })),
    [{ code: 'LEX0008', reason: { _tag: 'InvalidDurationAmount' } }],
  )
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

it('recognizes union only as a complete keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://union-keyword.silk', ascii('union unionize unions')),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['UnionKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('recognizes type only as a complete keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://type-keyword.silk', ascii('type typeName types')),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['TypeKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('recognizes extern only as a complete keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://extern-keyword.silk', ascii('extern external externs')),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['ExternKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
  )
})

it('recognizes export only as a complete keyword', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://export-keyword.silk', ascii('export exported exports')),
  )

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['ExportKeyword', 'Identifier', 'Identifier', 'EndOfFile'],
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

it('recognizes static forms only as complete keywords', () => {
  const result = Lexer.lex(
    SourceFile.make(
      'memory://static-keywords.silk',
      ascii('static statically compileError compileErrors static for field in fields'),
    ),
  )
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'StaticKeyword',
      'Identifier',
      'CompileErrorKeyword',
      'Identifier',
      'StaticKeyword',
      'ForKeyword',
      'Identifier',
      'Identifier',
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

it('recognizes requirement-row punctuation and the at selector word', () => {
  const result = Lexer.lex(
    SourceFile.make('memory://effect-requirements.silk', ascii('? &Allocator at Scratch')),
  )
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Question', 'Ampersand', 'Identifier', 'Identifier', 'Identifier', 'EndOfFile'],
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

it('lexes all four bitwise bytes as punctuation without an unsupported-byte diagnostic', () => {
  const source = SourceFile.make('memory/bitwise-operators', ascii('& | ^ ~ |>'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    ['Ampersand', 'Pipe', 'Caret', 'Tilde', 'PipeGreater', 'EndOfFile'],
  )
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind === 'Caret' || token.kind === 'Tilde')
      .map((token) => [token.span.start, token.span.end]),
    [
      [4, 5],
      [6, 7],
    ],
  )
})

it('keeps base-prefixed and separated literals intact around the bitwise bytes', () => {
  const source = SourceFile.make('memory/bitwise-literals', ascii('0xff ^ 0x0f ~0b1010 1_0 ^ 2'))
  const result = Lexer.lex(source)

  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'Whitespace').map((token) => token.kind),
    [
      'DecimalInteger',
      'Caret',
      'DecimalInteger',
      'Tilde',
      'DecimalInteger',
      'DecimalInteger',
      'Caret',
      'DecimalInteger',
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

const utf8 = (value: string): Uint8Array => new TextEncoder().encode(value)

it('lexes a character literal and every accepted escape as one CharLiteral token', () => {
  for (const spelling of [
    "'a'",
    "' '",
    "'\\n'",
    "'\\r'",
    "'\\t'",
    "'\\0'",
    "'\\\\'",
    "'\\''",
    "'\\\"'",
    "'\"'",
    "'\\x41'",
    "'\\u{2603}'",
    "'é'",
    "'😀'",
  ]) {
    const bytes = utf8(spelling)
    const source = SourceFile.make('memory://char.silk', bytes)
    const result = Lexer.lex(source)
    assert.deepEqual(
      result.tokens.filter((token) => token.kind !== 'EndOfFile').map((token) => token.kind),
      ['CharLiteral'],
      spelling,
    )
    assert.strictEqual(result.tokens.at(0)?.span.end, bytes.length, spelling)
    assert.deepEqual(result.diagnostics, [], spelling)
  }
})

it('distinguishes lifetime names from closed characters and bounds names at a newline', () => {
  const text = "'data 'static 'a' 'ab' 'next\nfn later() {}"
  const source = SourceFile.make('memory://lifetimes.silk', ascii(text))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'Whitespace' && token.kind !== 'EndOfFile')
      .map((token) => [token.kind, tokenView(source, token).slice]),
    [
      ['Lifetime', "'data"],
      ['Lifetime', "'static"],
      ['CharLiteral', "'a'"],
      ['InvalidStaticLiteral', "'ab'"],
      ['Lifetime', "'next"],
      ['FnKeyword', 'fn'],
      ['Identifier', 'later'],
      ['LeftParenthesis', '('],
      ['RightParenthesis', ')'],
      ['LeftBrace', '{'],
      ['RightBrace', '}'],
    ],
  )
  assert.deepEqual(
    result.diagnostics.map(({ code, span }) => [code, span.start, span.end]),
    [['LEX0007', 18, 22]],
  )
})

it('diagnoses a character literal that does not hold exactly one scalar', () => {
  for (const [spelling, scalars] of [
    ["''", 0],
    ["'ab'", 2],
    ["'éé'", 2],
    ["'\\n\\t'", 2],
    ["'a\\u{2603}'", 2],
  ] as const) {
    const bytes = utf8(spelling)
    const source = SourceFile.make('memory://char-count.silk', bytes)
    const result = Lexer.lex(source)
    assert.deepEqual(
      result.tokens.filter((token) => token.kind !== 'EndOfFile').map((token) => token.kind),
      ['InvalidStaticLiteral'],
      spelling,
    )
    assert.deepEqual(
      result.diagnostics.map((diagnostic) => diagnostic.code),
      ['LEX0007'],
      spelling,
    )
    assert.deepEqual(result.diagnostics.at(0)?.reason, {
      _tag: 'CharacterLiteralScalarCount',
      scalars,
    })
  }
})

it('stops an unterminated character literal before the line ending with one diagnostic', () => {
  const source = SourceFile.make('memory://char-open.silk', ascii("'!\nnext"))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens
      .filter((token) => token.kind !== 'EndOfFile')
      .map((token) => tokenView(source, token)),
    [
      { kind: 'InvalidStaticLiteral', start: 0, end: 2, slice: "'!" },
      { kind: 'Whitespace', start: 2, end: 3, slice: '\n' },
      { kind: 'Identifier', start: 3, end: 7, slice: 'next' },
    ],
  )
  assert.deepEqual(
    result.diagnostics.map((diagnostic) => diagnostic.code),
    ['LEX0003'],
  )
  assert.strictEqual(result.diagnostics.at(0)?.message, 'Unterminated character literal')
})

it('keeps a character literal one token beside its neighbours and covers every byte', () => {
  const bytes = utf8("const space: char = ' '\nlet tab = '\\t' // 'not a literal'\n")
  const source = SourceFile.make('memory://char-neighbours.silk', bytes)
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens.filter((token) => token.kind === 'CharLiteral').map((token) => token.span.start),
    [20, 34],
  )
  assert.deepEqual(result.diagnostics, [])
  assert.deepEqual(
    Uint8Array.from(
      result.tokens
        .filter((token) => token.kind !== 'EndOfFile')
        .flatMap((token) => Array.from(Option.getOrThrow(SourceFile.slice(source, token.span)))),
    ),
    bytes,
  )
})

it('leaves an unknown modifier vocabulary to the quote delimiter alone', () => {
  const source = SourceFile.make('memory://char-modifier.silk', ascii("b'a'"))
  const result = Lexer.lex(source)
  assert.deepEqual(
    result.tokens.filter((token) => token.kind !== 'EndOfFile').map((token) => token.kind),
    ['Identifier', 'CharLiteral'],
  )
  assert.deepEqual(result.diagnostics, [])
})
