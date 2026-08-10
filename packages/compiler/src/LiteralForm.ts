import type * as Token from './Token.js'

/** The semantic category selected by a static-literal modifier. */
export type Category = 'Text' | 'Bytes'

/** The number of quotes in a recognized static-literal delimiter. */
export type DelimiterWidth = 1 | 3

/** The body-decoding policy selected independently from delimiter width. */
export type EscapePolicy = 'Escaped'

/** Immutable source-form metadata shared by the compiler and editor tooling. */
export interface LiteralForm {
  readonly category: Category
  readonly modifier: '' | 'b'
  readonly delimiterWidth: DelimiterWidth
  readonly escapePolicy: EscapePolicy
  readonly tokenKind: 'TextLiteral' | 'ByteStringLiteral'
}

const make = (
  category: Category,
  modifier: LiteralForm['modifier'],
  delimiterWidth: DelimiterWidth,
  tokenKind: LiteralForm['tokenKind'],
): LiteralForm =>
  Object.freeze({ category, modifier, delimiterWidth, escapePolicy: 'Escaped', tokenKind })

/**
 * Every committed form, ordered by longest introduction first. Consumers that generate matching
 * rules can retain this order without reconstructing the language's precedence contract.
 */
export const forms: ReadonlyArray<LiteralForm> = Object.freeze([
  make('Bytes', 'b', 3, 'ByteStringLiteral'),
  make('Text', '', 3, 'TextLiteral'),
  make('Bytes', 'b', 1, 'ByteStringLiteral'),
  make('Text', '', 1, 'TextLiteral'),
])

const quote = 0x22

/** Byte storage accepted from source files, tests, and generated tooling. */
export type ByteSequence = ReadonlyArray<number> | Uint8Array

const matches = (bytes: ByteSequence, index: number, form: LiteralForm): boolean => {
  let cursor = index
  for (let modifierIndex = 0; modifierIndex < form.modifier.length; modifierIndex += 1) {
    if (bytes[cursor] !== form.modifier.charCodeAt(modifierIndex)) return false
    cursor += 1
  }
  for (let delimiterIndex = 0; delimiterIndex < form.delimiterWidth; delimiterIndex += 1) {
    if (bytes[cursor + delimiterIndex] !== quote) return false
  }
  return true
}

/** Recognizes the longest committed literal introduction at `index`. */
export const recognize = (bytes: ByteSequence, index = 0): LiteralForm | undefined =>
  forms.find((form) => matches(bytes, index, form))

const isAsciiLetter = (byte: number | undefined): boolean =>
  byte !== undefined && ((byte >= 0x41 && byte <= 0x5a) || (byte >= 0x61 && byte <= 0x7a))

const isDecimalDigit = (byte: number | undefined): boolean =>
  byte !== undefined && byte >= 0x30 && byte <= 0x39

const isIdentifierStart = (byte: number | undefined): boolean =>
  byte === 0x5f || isAsciiLetter(byte)

const isIdentifierContinue = (byte: number | undefined): boolean =>
  isIdentifierStart(byte) || isDecimalDigit(byte)

/** One identifier-like spelling reserved as an unrecognized adjacent literal modifier. */
export interface UnknownIntroduction {
  readonly modifier: string
  readonly modifierWidth: number
  readonly delimiterWidth: DelimiterWidth
}

/** Recognizes a closed-vocabulary modifier failure without accepting it as a new form. */
export const recognizeUnknown = (
  bytes: ByteSequence,
  index = 0,
): UnknownIntroduction | undefined => {
  if (!isIdentifierStart(bytes[index])) return undefined
  let cursor = index + 1
  while (cursor < bytes.length && isIdentifierContinue(bytes[cursor])) cursor += 1
  if (bytes[cursor] !== quote) return undefined
  const modifier = String.fromCharCode(...bytes.slice(index, cursor))
  if (forms.some((form) => form.modifier === modifier)) return undefined
  const delimiterWidth = bytes[cursor + 1] === quote && bytes[cursor + 2] === quote ? 3 : 1
  return Object.freeze({ modifier, modifierWidth: cursor - index, delimiterWidth })
}

/** The deterministic boundary selected for a recognized or reserved introduction. */
export interface Boundary {
  readonly end: number
  readonly terminated: boolean
}

/** Scans exactly one escaped literal body after its opening delimiter. */
export const scanBoundary = (
  bytes: ByteSequence,
  contentStart: number,
  delimiterWidth: DelimiterWidth,
): Boundary => {
  let index = contentStart
  while (index < bytes.length) {
    if (delimiterWidth === 1 && (bytes[index] === 0x0a || bytes[index] === 0x0d)) {
      return Object.freeze({ end: index, terminated: false })
    }
    if (bytes[index] === quote) {
      if (delimiterWidth === 1 || (bytes[index + 1] === quote && bytes[index + 2] === quote)) {
        return Object.freeze({ end: index + delimiterWidth, terminated: true })
      }
      index += 1
      continue
    }
    if (bytes[index] === 0x5c && index + 1 < bytes.length) {
      if (delimiterWidth === 1 && (bytes[index + 1] === 0x0a || bytes[index + 1] === 0x0d)) {
        index += 1
      } else {
        index += 2
      }
      continue
    }
    index += 1
  }
  return Object.freeze({ end: bytes.length, terminated: false })
}

/** Returns the token kind selected by a valid form. */
export const tokenKind = (self: LiteralForm): Token.TokenKind => self.tokenKind
