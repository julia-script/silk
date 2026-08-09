export const source = '// tiny\r\npub fn main() -> i32 {\n\treturn 42\n}\n'

export interface ExpectedToken {
  readonly kind: string
  readonly start: number
  readonly end: number
  readonly slice: string
}

export const expectedTokens: ReadonlyArray<ExpectedToken> = Object.freeze([
  { kind: 'LineComment', start: 0, end: 7, slice: '// tiny' },
  { kind: 'Whitespace', start: 7, end: 9, slice: '\r\n' },
  { kind: 'PubKeyword', start: 9, end: 12, slice: 'pub' },
  { kind: 'Whitespace', start: 12, end: 13, slice: ' ' },
  { kind: 'FnKeyword', start: 13, end: 15, slice: 'fn' },
  { kind: 'Whitespace', start: 15, end: 16, slice: ' ' },
  { kind: 'Identifier', start: 16, end: 20, slice: 'main' },
  { kind: 'LeftParenthesis', start: 20, end: 21, slice: '(' },
  { kind: 'RightParenthesis', start: 21, end: 22, slice: ')' },
  { kind: 'Whitespace', start: 22, end: 23, slice: ' ' },
  { kind: 'Arrow', start: 23, end: 25, slice: '->' },
  { kind: 'Whitespace', start: 25, end: 26, slice: ' ' },
  { kind: 'Identifier', start: 26, end: 29, slice: 'i32' },
  { kind: 'Whitespace', start: 29, end: 30, slice: ' ' },
  { kind: 'LeftBrace', start: 30, end: 31, slice: '{' },
  { kind: 'Whitespace', start: 31, end: 33, slice: '\n\t' },
  { kind: 'ReturnKeyword', start: 33, end: 39, slice: 'return' },
  { kind: 'Whitespace', start: 39, end: 40, slice: ' ' },
  { kind: 'DecimalInteger', start: 40, end: 42, slice: '42' },
  { kind: 'Whitespace', start: 42, end: 43, slice: '\n' },
  { kind: 'RightBrace', start: 43, end: 44, slice: '}' },
  { kind: 'Whitespace', start: 44, end: 45, slice: '\n' },
  { kind: 'EndOfFile', start: 45, end: 45, slice: '' },
])

export const recoverySource = Uint8Array.of(0xff, 0xfe, 0x20, 0x5b, 0x5d, 0x20, 0x2d, 0x3e, 0x61)

export interface ExpectedDiagnostic {
  readonly code: string
  readonly message: string
  readonly start: number
  readonly end: number
}

export const expectedRecoveryDiagnostics: ReadonlyArray<ExpectedDiagnostic> = Object.freeze([
  { code: 'LEX0001', message: 'Unsupported byte sequence', start: 0, end: 2 },
])
