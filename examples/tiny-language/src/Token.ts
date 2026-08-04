export type Kind =
  | 'Fn'
  | 'If'
  | 'Then'
  | 'Else'
  | 'Identifier'
  | 'Integer'
  | 'LeftParen'
  | 'RightParen'
  | 'Comma'
  | 'Equal'
  | 'Plus'
  | 'Minus'
  | 'Star'
  | 'Slash'
  | 'Percent'
  | 'Less'
  | 'Greater'
  | 'Eof'

export interface Token {
  readonly kind: Kind
  readonly lexeme: string
  readonly start: number
  readonly end: number
}

const keywords: ReadonlyMap<string, Kind> = new Map([
  ['fn', 'Fn'],
  ['if', 'If'],
  ['then', 'Then'],
  ['else', 'Else'],
])

export const make = (kind: Kind, lexeme: string, start: number, end: number): Token =>
  Object.freeze({ kind, lexeme, start, end })

export const classifyIdentifier = (lexeme: string): Kind => keywords.get(lexeme) ?? 'Identifier'
