import * as Option from 'effect/Option'
import * as Diagnostic from './Diagnostic.js'
import type * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as Token from './Token.js'

/** The complete, deterministic result of lexing one immutable source snapshot. */
export interface LexicalResult {
  readonly source: SourceFile.SourceFile
  readonly tokens: ReadonlyArray<Token.Token>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const isWhitespace = (byte: number | undefined): boolean =>
  byte === 0x20 || byte === 0x09 || byte === 0x0a || byte === 0x0d

const isAsciiLetter = (byte: number | undefined): boolean =>
  byte !== undefined && ((byte >= 0x41 && byte <= 0x5a) || (byte >= 0x61 && byte <= 0x7a))

const isDecimalDigit = (byte: number | undefined): boolean =>
  byte !== undefined && byte >= 0x30 && byte <= 0x39

const isIdentifierStart = (byte: number | undefined): boolean =>
  byte === 0x5f || isAsciiLetter(byte)

const isIdentifierContinue = (byte: number | undefined): boolean =>
  isIdentifierStart(byte) || isDecimalDigit(byte)

const isLineCommentStart = (bytes: ReadonlyArray<number>, index: number): boolean =>
  bytes[index] === 0x2f && bytes[index + 1] === 0x2f

const isArrowStart = (bytes: ReadonlyArray<number>, index: number): boolean =>
  bytes[index] === 0x2d && bytes[index + 1] === 0x3e

const isPunctuation = (byte: number | undefined): boolean =>
  byte === 0x28 ||
  byte === 0x29 ||
  byte === 0x7b ||
  byte === 0x7d ||
  byte === 0x3a ||
  byte === 0x2c ||
  byte === 0x3d ||
  byte === 0x2d ||
  byte === 0x2e

const isSupportedTokenStart = (bytes: ReadonlyArray<number>, index: number): boolean => {
  const byte = bytes[index]
  return (
    isWhitespace(byte) ||
    isIdentifierStart(byte) ||
    isDecimalDigit(byte) ||
    isLineCommentStart(bytes, index) ||
    isArrowStart(bytes, index) ||
    isPunctuation(byte)
  )
}

const keywordKind = (bytes: ReadonlyArray<number>, start: number, end: number): Token.TokenKind => {
  if (end - start === 2 && bytes[start] === 0x66 && bytes[start + 1] === 0x6e) {
    return 'FnKeyword'
  }
  if (
    end - start === 3 &&
    bytes[start] === 0x6c &&
    bytes[start + 1] === 0x65 &&
    bytes[start + 2] === 0x74
  ) {
    return 'LetKeyword'
  }
  if (
    end - start === 4 &&
    bytes[start] === 0x6d &&
    bytes[start + 1] === 0x6f &&
    bytes[start + 2] === 0x76 &&
    bytes[start + 3] === 0x65
  ) {
    return 'MoveKeyword'
  }
  if (
    end - start === 3 &&
    bytes[start] === 0x70 &&
    bytes[start + 1] === 0x75 &&
    bytes[start + 2] === 0x62
  ) {
    return 'PubKeyword'
  }
  if (
    end - start === 6 &&
    bytes[start] === 0x72 &&
    bytes[start + 1] === 0x65 &&
    bytes[start + 2] === 0x74 &&
    bytes[start + 3] === 0x75 &&
    bytes[start + 4] === 0x72 &&
    bytes[start + 5] === 0x6e
  ) {
    return 'ReturnKeyword'
  }
  if (
    end - start === 6 &&
    bytes[start] === 0x69 &&
    bytes[start + 1] === 0x6d &&
    bytes[start + 2] === 0x70 &&
    bytes[start + 3] === 0x6f &&
    bytes[start + 4] === 0x72 &&
    bytes[start + 5] === 0x74
  ) {
    return 'ImportKeyword'
  }
  return 'Identifier'
}

const punctuationKind = (byte: number | undefined): Token.TokenKind => {
  switch (byte) {
    case 0x28:
      return 'LeftParenthesis'
    case 0x29:
      return 'RightParenthesis'
    case 0x7b:
      return 'LeftBrace'
    case 0x7d:
      return 'RightBrace'
    case 0x3a:
      return 'Colon'
    case 0x2c:
      return 'Comma'
    case 0x3d:
      return 'Equals'
    case 0x2d:
      return 'Minus'
    case 0x2e:
      return 'Dot'
    default:
      return 'Invalid'
  }
}

const spanAt = (source: SourceFile.SourceFile, start: number, end: number): SourceSpan.SourceSpan =>
  Option.getOrThrowWith(
    SourceSpan.make(source, start, end),
    () => new RangeError(`Lexer produced an invalid span [${start}, ${end})`),
  )

/**
 * Classifies every source byte exactly once and always appends an empty EOF token.
 *
 * This is intentionally one index-based imperative loop: a lexer is a measured per-byte hot path,
 * and the package benchmark records its throughput. Construction and results remain immutable.
 */
export const lex = (source: SourceFile.SourceFile): LexicalResult => {
  const bytes = source.bytes
  const tokens: Array<Token.Token> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  let index = 0

  const pushToken = (kind: Token.TokenKind, start: number, end: number): SourceSpan.SourceSpan => {
    const span = spanAt(source, start, end)
    tokens.push(Token.make(kind, span))
    return span
  }

  while (index < bytes.length) {
    const start = index
    const byte = bytes[index]

    if (isWhitespace(byte)) {
      index += 1
      while (index < bytes.length && isWhitespace(bytes[index])) index += 1
      pushToken('Whitespace', start, index)
      continue
    }

    if (isLineCommentStart(bytes, index)) {
      const kind = bytes[index + 2] === 0x2f ? 'DocComment' : 'LineComment'
      index += 2
      while (index < bytes.length && bytes[index] !== 0x0a && bytes[index] !== 0x0d) index += 1
      pushToken(kind, start, index)
      continue
    }

    if (isIdentifierStart(byte)) {
      index += 1
      while (index < bytes.length && isIdentifierContinue(bytes[index])) index += 1
      pushToken(keywordKind(bytes, start, index), start, index)
      continue
    }

    if (isDecimalDigit(byte)) {
      index += 1
      while (index < bytes.length && isDecimalDigit(bytes[index])) index += 1
      pushToken('DecimalInteger', start, index)
      continue
    }

    if (isArrowStart(bytes, index)) {
      index += 2
      pushToken('Arrow', start, index)
      continue
    }

    if (isPunctuation(byte)) {
      index += 1
      pushToken(punctuationKind(byte), start, index)
      continue
    }

    index += 1
    while (index < bytes.length && !isSupportedTokenStart(bytes, index)) index += 1
    const span = pushToken('Invalid', start, index)
    diagnostics.push(Diagnostic.unsupportedBytes(span))
  }

  pushToken('EndOfFile', bytes.length, bytes.length)
  return Object.freeze({
    source,
    tokens: Object.freeze(tokens),
    diagnostics: Object.freeze(diagnostics),
  })
}
