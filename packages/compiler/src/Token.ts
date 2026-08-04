import type * as SourceSpan from './SourceSpan.js'

/** The complete set of token kinds recognized by the bootstrap lexer. */
export type TokenKind =
  | 'Whitespace'
  | 'LineComment'
  | 'Identifier'
  | 'DecimalInteger'
  | 'PubKeyword'
  | 'FnKeyword'
  | 'ReturnKeyword'
  | 'LeftParenthesis'
  | 'RightParenthesis'
  | 'LeftBrace'
  | 'RightBrace'
  | 'Arrow'
  | 'Invalid'
  | 'EndOfFile'

/** A classified source span. Trivia and invalid bytes remain explicit tokens. */
export interface Token {
  readonly _tag: 'Token'
  readonly kind: TokenKind
  readonly span: SourceSpan.SourceSpan
}

/** Creates an immutable token. */
export const make = (kind: TokenKind, span: SourceSpan.SourceSpan): Token =>
  Object.freeze({ _tag: 'Token', kind, span })
