import * as Effect from 'effect/Effect'
import * as Inspectable from 'effect/Inspectable'
import * as Diagnostic from './Diagnostic.js'
import * as Token from './Token.js'

const punctuation: ReadonlyMap<string, Token.Kind> = new Map([
  ['(', 'LeftParen'],
  [')', 'RightParen'],
  [',', 'Comma'],
  ['=', 'Equal'],
  ['+', 'Plus'],
  ['-', 'Minus'],
  ['*', 'Star'],
  ['/', 'Slash'],
  ['%', 'Percent'],
  ['<', 'Less'],
  ['>', 'Greater'],
])

const isWhitespace = (character: string): boolean =>
  character === ' ' || character === '\n' || character === '\r' || character === '\t'

const isDigit = (character: string): boolean => character >= '0' && character <= '9'

const isIdentifierStart = (character: string): boolean =>
  (character >= 'a' && character <= 'z') ||
  (character >= 'A' && character <= 'Z') ||
  character === '_'

const isIdentifierPart = (character: string): boolean =>
  isIdentifierStart(character) || isDigit(character)

export const tokenize = Effect.fn('Lexer.tokenize')(function* (
  source: string,
): Effect.fn.Return<ReadonlyArray<Token.Token>, Diagnostic.LexError> {
  const tokens: Array<Token.Token> = []
  let offset = 0

  while (offset < source.length) {
    const character = source.charAt(offset)

    if (isWhitespace(character)) {
      offset += 1
      continue
    }

    const start = offset

    if (isDigit(character)) {
      offset += 1
      while (offset < source.length && isDigit(source.charAt(offset))) offset += 1
      const lexeme = source.slice(start, offset)
      tokens.push(Token.make('Integer', lexeme, start, offset))
      continue
    }

    if (isIdentifierStart(character)) {
      offset += 1
      while (offset < source.length && isIdentifierPart(source.charAt(offset))) offset += 1
      const lexeme = source.slice(start, offset)
      tokens.push(Token.make(Token.classifyIdentifier(lexeme), lexeme, start, offset))
      continue
    }

    const kind = punctuation.get(character)
    if (kind !== undefined) {
      offset += 1
      tokens.push(Token.make(kind, character, start, offset))
      continue
    }

    return yield* new Diagnostic.LexError({
      message: `Unexpected character ${Inspectable.toStringUnknown(character)}`,
      start,
      end: start + 1,
      found: character,
    })
  }

  tokens.push(Token.make('Eof', '', source.length, source.length))
  return Object.freeze(tokens)
})
