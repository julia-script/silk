import type * as SourceSpan from './SourceSpan.js'
import type * as Token from './Token.js'

/** Stable code for one required token that is absent at its insertion position. */
export const missingTokenCode = 'PAR0001' as const

/** Stable code for one maximal region of unexpected concrete tokens. */
export const unexpectedTokensCode = 'PAR0002' as const

/** Why parsing produced a diagnostic. */
export type Reason =
  | { readonly _tag: 'MissingToken'; readonly expected: Token.TokenKind }
  | { readonly _tag: 'UnexpectedTokens' }

/** A recoverable grammatical problem discovered while building concrete syntax. */
export interface ParseDiagnostic {
  readonly _tag: 'ParseDiagnostic'
  readonly code: typeof missingTokenCode | typeof unexpectedTokensCode
  readonly severity: 'error'
  readonly message: string
  readonly reason: Reason
  readonly span: SourceSpan.SourceSpan
}

/** Creates the diagnostic associated with one missing token leaf. */
export const missingToken = (
  expected: Token.TokenKind,
  span: SourceSpan.SourceSpan,
): ParseDiagnostic =>
  Object.freeze({
    _tag: 'ParseDiagnostic',
    code: missingTokenCode,
    severity: 'error',
    message: `Expected ${expected}`,
    reason: Object.freeze({ _tag: 'MissingToken', expected }),
    span,
  })

/** Creates the diagnostic associated with one unexpected-token error node. */
export const unexpectedTokens = (span: SourceSpan.SourceSpan): ParseDiagnostic =>
  Object.freeze({
    _tag: 'ParseDiagnostic',
    code: unexpectedTokensCode,
    severity: 'error',
    message: 'Unexpected token sequence',
    reason: Object.freeze({ _tag: 'UnexpectedTokens' }),
    span,
  })
