import type * as SourceSpan from './SourceSpan.js'

/** Stable diagnostic code for a maximal unsupported byte region. */
export const unsupportedBytesCode = 'LEX0001' as const

/** A recoverable problem discovered while classifying source bytes. */
export interface LexicalDiagnostic {
  readonly _tag: 'LexicalDiagnostic'
  readonly code: typeof unsupportedBytesCode
  readonly severity: 'error'
  readonly message: 'Unsupported byte sequence'
  readonly span: SourceSpan.SourceSpan
}

/** Creates the diagnostic associated with one `Invalid` token. */
export const unsupportedBytes = (span: SourceSpan.SourceSpan): LexicalDiagnostic =>
  Object.freeze({
    _tag: 'LexicalDiagnostic',
    code: unsupportedBytesCode,
    severity: 'error',
    message: 'Unsupported byte sequence',
    span,
  })
