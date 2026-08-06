import type * as SourceSpan from './SourceSpan.js'

/** The complete set of token kinds recognized by the bootstrap lexer. */
export type TokenKind =
  | 'Whitespace'
  | 'LineComment'
  | 'DocComment'
  | 'Identifier'
  | 'DecimalInteger'
  | 'PubKeyword'
  | 'StructKeyword'
  | 'FnKeyword'
  | 'ReturnKeyword'
  | 'ImportKeyword'
  | 'AsKeyword'
  | 'LetKeyword'
  | 'MutKeyword'
  | 'MoveKeyword'
  | 'MatchKeyword'
  | 'IfKeyword'
  | 'ElseKeyword'
  | 'WhileKeyword'
  | 'BreakKeyword'
  | 'ContinueKeyword'
  | 'TrueKeyword'
  | 'FalseKeyword'
  | 'LeftParenthesis'
  | 'RightParenthesis'
  | 'LeftBrace'
  | 'RightBrace'
  | 'LeftBracket'
  | 'RightBracket'
  | 'Colon'
  | 'Comma'
  | 'Equals'
  | 'EqualEqual'
  | 'FatArrow'
  | 'Minus'
  | 'Plus'
  | 'Star'
  | 'Slash'
  | 'Percent'
  | 'Bang'
  | 'BangEqual'
  | 'Less'
  | 'LessEqual'
  | 'Greater'
  | 'GreaterEqual'
  | 'Pipe'
  | 'PipeGreater'
  | 'Ampersand'
  | 'Dot'
  | 'DotDot'
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
