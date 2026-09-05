import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { afterAll, assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Lexer from '../src/Lexer.js'
import * as MirVerification from '../src/MirVerification.js'
import * as SourceFile from '../src/SourceFile.js'
import type * as Token from '../src/Token.js'

const sourcePath = fileURLToPath(
  new URL('../../../examples/language-pressure/lexer/main.silk', import.meta.url),
)
const pressureSource = readFileSync(sourcePath, 'utf8')
const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const tokenCode: Readonly<Record<Token.TokenKind, number>> = Object.freeze({
  Whitespace: 0,
  LineComment: 1,
  DocComment: 2,
  ModuleDocComment: 3,
  Identifier: 4,
  DecimalInteger: 5,
  DecimalFloat: 6,
  TextLiteral: 7,
  ByteStringLiteral: 8,
  PubKeyword: 9,
  StructKeyword: 10,
  TupleKeyword: 81,
  EffectKeyword: 11,
  FnKeyword: 12,
  RunKeyword: 13,
  FailKeyword: 14,
  DropKeyword: 15,
  UnsafeKeyword: 16,
  ImplKeyword: 17,
  ForKeyword: 18,
  ReturnKeyword: 19,
  ImportKeyword: 20,
  AsKeyword: 21,
  LetKeyword: 22,
  MutKeyword: 23,
  OnceKeyword: 24,
  MoveKeyword: 25,
  MatchKeyword: 26,
  IfKeyword: 27,
  ElseKeyword: 28,
  WhileKeyword: 29,
  BreakKeyword: 30,
  ContinueKeyword: 31,
  TrueKeyword: 32,
  FalseKeyword: 33,
  LeftParenthesis: 34,
  RightParenthesis: 35,
  LeftBrace: 36,
  RightBrace: 37,
  LeftBracket: 38,
  RightBracket: 39,
  Colon: 40,
  Semicolon: 41,
  Comma: 42,
  Equals: 43,
  EqualEqual: 44,
  FatArrow: 45,
  Minus: 46,
  Plus: 47,
  Star: 48,
  Slash: 49,
  Percent: 50,
  Bang: 51,
  BangEqual: 52,
  Question: 53,
  At: 54,
  Less: 55,
  LessEqual: 56,
  Greater: 57,
  GreaterEqual: 58,
  Pipe: 59,
  PipeGreater: 60,
  Ampersand: 61,
  Dot: 62,
  DotDot: 63,
  Arrow: 64,
  Invalid: 65,
  EndOfFile: 66,
  ConstKeyword: 67,
  InvalidStaticLiteral: 68,
  ServiceKeyword: 69,
  InterfaceKeyword: 70,
  Caret: 71,
  Tilde: 72,
  AmpersandAmpersand: 73,
  PipePipe: 74,
  CharLiteral: 75,
  RoleKeyword: 76,
  EnumKeyword: 77,
  UnionKeyword: 78,
  DurationLiteral: 79,
  InvalidDurationLiteral: 80,
  StaticKeyword: 82,
  CompileErrorKeyword: 83,
  TypeKeyword: 84,
  ExternKeyword: 85,
  ExportKeyword: 86,
  Lifetime: 87,
})

interface ExpectedToken {
  readonly kind: Token.TokenKind
  readonly code: number
  readonly start: number
  readonly end: number
}

interface ExpectedCase {
  readonly tokens: ReadonlyArray<ExpectedToken>
  readonly diagnostics: ReadonlyArray<{ readonly start: number; readonly end: number }>
  readonly fingerprint: number
}

/** One-based line and column of a byte offset, counting line feeds exactly as the program does. */
const positionOf = (
  bytes: Uint8Array,
  offset: number,
): { readonly line: number; readonly column: number } => {
  let line = 1
  let column = 1
  for (let index = 0; index < offset; index += 1) {
    column += 1
    if (bytes[index] === 10) {
      line += 1
      column = 1
    }
  }
  return { line, column }
}

/** The text the pressure program renders, spelled the same way here so the two can disagree. */
const diagnosticMessage = (
  input: string,
  diagnostics: ReadonlyArray<{ readonly start: number; readonly end: number }>,
): string => {
  const bytes = ascii(input)
  return diagnostics
    .map((diagnostic) => {
      const { line, column } = positionOf(bytes, diagnostic.start)
      return `LEX0001 at line ${line} column ${column}\n`
    })
    .join('')
}

const expectedCase = (input: string, id: string): ExpectedCase => {
  const lexical = Lexer.lex(SourceFile.make(id, ascii(input)))
  const tokens = lexical.tokens.map(({ kind, span }) =>
    Object.freeze({ kind, code: tokenCode[kind], start: span.start, end: span.end }),
  )
  const diagnostics = lexical.diagnostics.map(({ span }) =>
    Object.freeze({ start: span.start, end: span.end }),
  )
  let fingerprint = 0
  for (const token of tokens) {
    fingerprint = (fingerprint * 17 + token.code + token.start * 3 + token.end * 5) % 197
  }
  // The program no longer folds diagnostic offsets: it renders a message carrying the line and
  // column those offsets resolve to, and folds the message text. Mirroring the rendering here is
  // what makes a wrong line number a failing test rather than a different number.
  for (const byte of ascii(diagnosticMessage(input, diagnostics))) {
    fingerprint = (fingerprint * 19 + byte * 7) % 197
  }
  return Object.freeze({
    tokens: Object.freeze(tokens),
    diagnostics: Object.freeze(diagnostics),
    fingerprint,
  })
}

const replaceExactlyOnce = (source: string, search: string, replacement: string): string => {
  assert.strictEqual(source.split(search).length - 1, 1, search)
  return source.replace(search, replacement)
}

const byteLiteral = (input: string): string =>
  `b"${Array.from(ascii(input), (byte) => `\\x${byte.toString(16).padStart(2, '0')}`).join('')}"`

const sourceFor = (
  input: string,
  id: string,
): { readonly source: string; readonly expected: ExpectedCase } => {
  const expected = expectedCase(input, id)
  const withInput = replaceExactlyOnce(
    pressureSource,
    '  let source = b"pub fn main() -> i32 { return 42 }\\n"',
    `  let source = ${byteLiteral(input)}`,
  )
  const source = replaceExactlyOnce(
    withInput,
    '  if value != 0 { let mismatch = 1 / 0 }',
    `  if value != ${expected.fingerprint} { let mismatch = 1 / 0 }`,
  )
  return Object.freeze({ source, expected })
}

const corpus = [
  Object.freeze({ id: 'trivia', input: ' \t\r\n// line\n/// docs\n//! module\n//// plain\n' }),
  Object.freeze({
    id: 'keywords',
    input:
      'pub static compileError struct tuple enum union type service interface role effect fn run fail drop unsafe extern export impl for return import as let mut once move match if else while break continue true false const name _x2',
  }),
  Object.freeze({ id: 'numbers', input: '0 42 1.25 2e3 3E+4 4e- 5..6' }),
  Object.freeze({ id: 'durations', input: '1h30m30s 1h60m' }),
  Object.freeze({
    id: 'literals',
    input:
      '"text\\"tail" b"\\x41" """line 1\r\n// body\n\\"\\"\\"""" b"""bytes\n""" future"value" "unterminated\nnext',
  }),
  Object.freeze({
    id: 'raw-literals',
    input:
      'r"\\d+\\.\\d+" r"""raw\\n\nbody\r\n""" r"path\\" tail r"unterminated\nrb"value" r return',
  }),
  Object.freeze({
    id: 'punctuation',
    input: '( ) { } [ ] : ; , = == => - + * / % ! != ? @ < <= > >= | |> || & && ^ ~ . .. ->',
  }),
  Object.freeze({
    id: 'char-literals',
    input: "'a' '\\n' '\\'' '\\u{2603}' '' 'ab' 'unterminated\nnext",
  }),
  Object.freeze({ id: 'invalid', input: '#~ pub \u0000? $x' }),
] as const

it.effect('publishes only general MIR operations for the pressure program', () =>
  Effect.gen(function* () {
    const generated = sourceFor(corpus[1].input, 'lexer-pressure/general-mir')
    const snapshot = yield* Analysis.ofSourceRealized(
      'lexer-pressure/general-mir',
      ascii(generated.source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const tags = new Set(
      Analysis.loweredMir(snapshot).functions.flatMap((fn) =>
        MirVerification.operations(fn).map((operation) => operation._tag),
      ),
    )
    assert.strictEqual(tags.has('Allocate'), true)
    assert.strictEqual(
      [...tags].some((tag) => tag.startsWith('Lex')),
      false,
    )
    assert.strictEqual(
      [...tags].some((tag) => tag.startsWith('Token')),
      false,
    )
  }),
)

const destinationRoot = mkdtempSync(join(tmpdir(), 'silk-lexer-pressure-'))
afterAll(() => rmSync(destinationRoot, { recursive: true, force: true }))
