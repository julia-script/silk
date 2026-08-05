import type * as SourceSpan from './SourceSpan.js'

/** Stable code for a present return-type name that is not a bootstrap built-in. */
export const unknownTypeCode = 'SEM0001' as const

/** Stable code for a present decimal literal outside the positive `I32` range. */
export const integerOutOfRangeCode = 'SEM0002' as const

/** Stable code for a present declaration name repeated after its first occurrence. */
export const duplicateDeclarationNameCode = 'SEM0003' as const

/** Stable code for a present call name with no matching top-level declaration. */
export const unknownFunctionCode = 'SEM0004' as const

/** Stable code for a present parameter name repeated after its first occurrence. */
export const duplicateParameterNameCode = 'SEM0005' as const

/** Stable code for a present value name with no matching local parameter. */
export const unknownParameterReferenceCode = 'SEM0006' as const

/** Why semantic analysis produced a diagnostic. */
export type Reason =
  | { readonly _tag: 'UnknownType'; readonly spelling: string }
  | {
      readonly _tag: 'IntegerOutOfRange'
      readonly spelling: string
      readonly maximum: 2147483647
    }
  | {
      readonly _tag: 'DuplicateDeclarationName'
      readonly spelling: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'UnknownFunction'; readonly spelling: string }
  | {
      readonly _tag: 'DuplicateParameterName'
      readonly spelling: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'UnknownParameterReference'; readonly spelling: string }

/** A recoverable semantic problem attached to one exact source span. */
export interface SemanticDiagnostic {
  readonly _tag: 'SemanticDiagnostic'
  readonly code:
    | typeof unknownTypeCode
    | typeof integerOutOfRangeCode
    | typeof duplicateDeclarationNameCode
    | typeof unknownFunctionCode
    | typeof duplicateParameterNameCode
    | typeof unknownParameterReferenceCode
  readonly severity: 'error'
  readonly message: string
  readonly reason: Reason
  readonly span: SourceSpan.SourceSpan
}

/** Creates the diagnostic for one present identifier that cannot resolve as a type. */
export const unknownType = (spelling: string, span: SourceSpan.SourceSpan): SemanticDiagnostic =>
  Object.freeze({
    _tag: 'SemanticDiagnostic',
    code: unknownTypeCode,
    severity: 'error',
    message: `Unknown type ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownType', spelling }),
    span,
  })

/** Creates the diagnostic for one decimal literal above the positive `I32` maximum. */
export const integerOutOfRange = (
  spelling: string,
  span: SourceSpan.SourceSpan,
): SemanticDiagnostic =>
  Object.freeze({
    _tag: 'SemanticDiagnostic',
    code: integerOutOfRangeCode,
    severity: 'error',
    message: 'Integer literal exceeds the I32 maximum',
    reason: Object.freeze({
      _tag: 'IntegerOutOfRange',
      spelling,
      maximum: 2147483647,
    }),
    span,
  })

/** Creates the diagnostic for a declaration name repeated after its first occurrence. */
export const duplicateDeclarationName = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): SemanticDiagnostic =>
  Object.freeze({
    _tag: 'SemanticDiagnostic',
    code: duplicateDeclarationNameCode,
    severity: 'error',
    message: `Duplicate declaration name ${spelling}`,
    reason: Object.freeze({
      _tag: 'DuplicateDeclarationName',
      spelling,
      originalSpan,
    }),
    span,
  })

/** Creates the diagnostic for one present call name with no matching declaration. */
export const unknownFunction = (
  spelling: string,
  span: SourceSpan.SourceSpan,
): SemanticDiagnostic =>
  Object.freeze({
    _tag: 'SemanticDiagnostic',
    code: unknownFunctionCode,
    severity: 'error',
    message: `Unknown function ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownFunction', spelling }),
    span,
  })

/** Creates the diagnostic for a parameter name repeated after its first occurrence. */
export const duplicateParameterName = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): SemanticDiagnostic =>
  Object.freeze({
    _tag: 'SemanticDiagnostic',
    code: duplicateParameterNameCode,
    severity: 'error',
    message: `Duplicate parameter name ${spelling}`,
    reason: Object.freeze({
      _tag: 'DuplicateParameterName',
      spelling,
      originalSpan,
    }),
    span,
  })

/** Creates the diagnostic for one present value name with no matching local parameter. */
export const unknownParameterReference = (
  spelling: string,
  span: SourceSpan.SourceSpan,
): SemanticDiagnostic =>
  Object.freeze({
    _tag: 'SemanticDiagnostic',
    code: unknownParameterReferenceCode,
    severity: 'error',
    message: `Unknown parameter ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownParameterReference', spelling }),
    span,
  })
