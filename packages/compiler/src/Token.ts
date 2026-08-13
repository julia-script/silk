import type * as SourceSpan from './SourceSpan.js'

/** The complete set of token kinds recognized by the bootstrap lexer. */
export type TokenKind =
  | 'Whitespace'
  | 'LineComment'
  | 'DocComment'
  | 'ModuleDocComment'
  | 'Identifier'
  | 'DecimalInteger'
  | 'DecimalFloat'
  | 'TextLiteral'
  | 'ByteStringLiteral'
  | 'InvalidStaticLiteral'
  | 'PubKeyword'
  | 'StructKeyword'
  | 'ServiceKeyword'
  | 'InterfaceKeyword'
  | 'EffectKeyword'
  | 'FnKeyword'
  | 'RunKeyword'
  | 'FailKeyword'
  | 'DropKeyword'
  | 'UnsafeKeyword'
  | 'ImplKeyword'
  | 'ForKeyword'
  | 'ReturnKeyword'
  | 'ImportKeyword'
  | 'AsKeyword'
  | 'LetKeyword'
  | 'ConstKeyword'
  | 'MutKeyword'
  | 'OnceKeyword'
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
  | 'Semicolon'
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
  | 'Question'
  | 'At'
  | 'Less'
  | 'LessEqual'
  | 'Greater'
  | 'GreaterEqual'
  | 'Pipe'
  | 'PipeGreater'
  | 'PipePipe'
  | 'Ampersand'
  | 'AmpersandAmpersand'
  | 'Caret'
  | 'Tilde'
  | 'Dot'
  | 'DotDot'
  | 'Arrow'
  | 'Invalid'
  | 'EndOfFile'

const descriptions: Readonly<Record<TokenKind, string>> = Object.freeze({
  Whitespace: 'whitespace',
  LineComment: 'line comment',
  DocComment: 'documentation comment',
  ModuleDocComment: 'module documentation comment',
  Identifier: 'identifier',
  DecimalInteger: 'decimal integer',
  DecimalFloat: 'decimal floating-point literal',
  TextLiteral: 'text literal',
  ByteStringLiteral: 'byte-string literal',
  InvalidStaticLiteral: 'invalid static literal',
  PubKeyword: '`pub`',
  StructKeyword: '`struct`',
  ServiceKeyword: '`service`',
  InterfaceKeyword: '`interface`',
  EffectKeyword: '`effect`',
  FnKeyword: '`fn`',
  RunKeyword: '`run`',
  FailKeyword: '`fail`',
  DropKeyword: '`drop`',
  UnsafeKeyword: '`unsafe`',
  ImplKeyword: '`impl`',
  ForKeyword: '`for`',
  ReturnKeyword: '`return`',
  ImportKeyword: '`import`',
  AsKeyword: '`as`',
  LetKeyword: '`let`',
  ConstKeyword: '`const`',
  MutKeyword: '`mut`',
  OnceKeyword: '`once`',
  MoveKeyword: '`move`',
  MatchKeyword: '`match`',
  IfKeyword: '`if`',
  ElseKeyword: '`else`',
  WhileKeyword: '`while`',
  BreakKeyword: '`break`',
  ContinueKeyword: '`continue`',
  TrueKeyword: '`true`',
  FalseKeyword: '`false`',
  LeftParenthesis: '`(`',
  RightParenthesis: '`)`',
  LeftBrace: '`{`',
  RightBrace: '`}`',
  LeftBracket: '`[`',
  RightBracket: '`]`',
  Colon: '`:`',
  Semicolon: '`;`',
  Comma: '`,`',
  Equals: '`=`',
  EqualEqual: '`==`',
  FatArrow: '`=>`',
  Minus: '`-`',
  Plus: '`+`',
  Star: '`*`',
  Slash: '`/`',
  Percent: '`%`',
  Bang: '`!`',
  BangEqual: '`!=`',
  Question: '`?`',
  At: '`@`',
  Less: '`<`',
  LessEqual: '`<=`',
  Greater: '`>`',
  GreaterEqual: '`>=`',
  Pipe: '`|`',
  PipeGreater: '`|>`',
  PipePipe: '`||`',
  Ampersand: '`&`',
  AmpersandAmpersand: '`&&`',
  Caret: '`^`',
  Tilde: '`~`',
  Dot: '`.`',
  DotDot: '`..`',
  Arrow: '`->`',
  Invalid: 'valid token',
  EndOfFile: 'end of file',
})

/** Describes a token kind using its Silk spelling or source-language role. */
export const describe = (kind: TokenKind): string => descriptions[kind]

/** A classified source span. Trivia and invalid bytes remain explicit tokens. */
export interface Token {
  readonly _tag: 'Token'
  readonly kind: TokenKind
  readonly span: SourceSpan.SourceSpan
}

/** Creates an immutable token. */
export const make = (kind: TokenKind, span: SourceSpan.SourceSpan): Token =>
  Object.freeze({ _tag: 'Token', kind, span })
