import type * as SourceSpan from './SourceSpan.js'
import type * as Token from './Token.js'

/** The compiler phase that originated a diagnostic. */
export type Phase = 'lexical' | 'parser' | 'module' | 'semantic' | 'ownership' | 'layout'

const phaseRank: Readonly<Record<Phase, number>> = Object.freeze({
  lexical: 0,
  parser: 1,
  module: 2,
  semantic: 3,
  ownership: 4,
  layout: 5,
})

/** Stable diagnostic code for a maximal unsupported byte region. */
export const unsupportedBytesCode = 'LEX0001' as const

/** Stable code for one required token that is absent at its insertion position. */
export const missingTokenCode = 'PAR0001' as const

/** Stable code for one maximal region of unexpected concrete tokens. */
export const unexpectedTokensCode = 'PAR0002' as const

/** Stable code for a primary-expression template start reserved for future support. */
export const reservedTemplateSyntaxCode = 'PAR0003' as const

/** Stable code for an import naming a module absent from the supplied sources. */
export const unknownModuleCode = 'MOD0001' as const

/** Stable code for an import redundantly naming its own containing module. */
export const selfImportCode = 'MOD0002' as const
export const duplicateImportCode = 'MOD0003' as const

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

/** Stable code for a uniquely resolved call with the wrong number of arguments. */
export const wrongCallArityCode = 'SEM0007' as const

/** Stable code for a binding whose name repeats a parameter or an earlier binding. */
export const rebindingNameCode = 'SEM0008' as const

/** Stable code for a qualified call naming an unknown built-in actor. */
export const unknownActorCode = 'SEM0009' as const

/** Stable code for a known actor called with an unknown operation name. */
export const unknownActorOperationCode = 'SEM0010' as const

/** Stable code for a conditional whose condition is not `Bool`. */
export const conditionNotBoolCode = 'SEM0011' as const

/** Stable code for a call argument whose type mismatches its parameter. */
export const argumentTypeMismatchCode = 'SEM0012' as const
export const redundantAliasCode = 'SEM0013' as const
export const unknownImportedMemberCode = 'SEM0014' as const
export const inaccessibleImportedMemberCode = 'SEM0015' as const
export const bindingConflictCode = 'SEM0016' as const
export const duplicateFieldNameCode = 'SEM0017' as const
export const expectedTypeCode = 'SEM0018' as const
export const privateTypeExposureCode = 'SEM0019' as const
export const inlineRecursiveStructCode = 'SEM0020' as const
export const externalRawStructLiteralCode = 'SEM0021' as const
export const unknownStructFieldCode = 'SEM0022' as const
export const duplicateStructInitializerCode = 'SEM0023' as const
export const missingStructInitializerCode = 'SEM0024' as const
export const structFieldTypeMismatchCode = 'SEM0025' as const
export const projectionOnNonStructCode = 'SEM0026' as const
export const unknownProjectedFieldCode = 'SEM0027' as const
export const inaccessibleProjectedFieldCode = 'SEM0028' as const
export const emptyArrayNeedsContextCode = 'SEM0029' as const
export const arrayElementTypeMismatchCode = 'SEM0030' as const
export const arrayLengthMismatchCode = 'SEM0031' as const
export const indexOnNonArrayCode = 'SEM0032' as const
export const indexNotI32Code = 'SEM0033' as const
export const indexOutOfBoundsCode = 'SEM0034' as const
export const immutableAssignmentCode = 'SEM0035' as const
export const invalidAssignmentPlaceCode = 'SEM0036' as const
export const assignmentTypeMismatchCode = 'SEM0037' as const
export const transferOutsideLoopCode = 'SEM0038' as const
export const invalidUnionMemberCode = 'SEM0039' as const
export const incompatibleUnionConversionCode = 'SEM0040' as const
export const matchScrutineeNotNominalCode = 'SEM0041' as const
export const matchMemberNotInScrutineeCode = 'SEM0042' as const
export const unreachableMatchArmCode = 'SEM0043' as const
export const incompleteMatchCode = 'SEM0044' as const
export const matchGuardNotBoolCode = 'SEM0045' as const
export const missingPatternFieldCode = 'SEM0046' as const
export const duplicatePatternFieldCode = 'SEM0047' as const
export const patternBindingConflictCode = 'SEM0048' as const
export const incompatibleMatchResultsCode = 'SEM0049' as const
export const duplicateTypeParameterCode = 'SEM0050' as const
export const typeArgumentArityCode = 'SEM0051' as const
export const typeArgumentInferenceCode = 'SEM0052' as const
export const polymorphicRecursionCode = 'SEM0053' as const
/** Stable code for a borrowed slice type outside a direct ordinary-function parameter. */
export const sliceTypePositionCode = 'SEM0054' as const
export const invalidBorrowPositionCode = 'SEM0055' as const
export const invalidBorrowOperandCode = 'SEM0056' as const
export const exclusiveBorrowRequiresMutableCode = 'SEM0057' as const
export const invalidSliceReborrowCode = 'SEM0058' as const
export const implicitSliceDecayCode = 'SEM0059' as const
/** Stable code for a negative decimal literal contextualized as unsigned `Usize`. */
export const usizeNegativeCode = 'SEM0060' as const
/** Stable code for a non-concrete or non-nominal member of a effect failure row. */
export const invalidFailureTypeCode = 'SEM0061' as const
/** Stable code for a failure row attached to an ordinary function. */
export const failureRowOnOrdinaryCode = 'SEM0062' as const
export const failOutsideEffectCode = 'SEM0063' as const
export const undeclaredFailureCode = 'SEM0064' as const
export const runNonEffectCode = 'SEM0065' as const
export const unhandledEffectFailuresCode = 'SEM0066' as const
export const invalidEffectHandlerCode = 'SEM0067' as const
export const mutableEffectRecipeCode = 'SEM0068' as const
export const effectIdentityErasureCode = 'SEM0069' as const
/** Stable code for a non-concrete or non-nominal capability in a requirement row. */
export const invalidRequirementTypeCode = 'SEM0070' as const
export const unhandledEffectRequirementsCode = 'SEM0071' as const
export const invalidEffectRetryCode = 'SEM0072' as const
export const providerBackedFailureCode = 'SEM0073' as const
export const invalidEffectProvisionCode = 'SEM0074' as const

/** Stable code for a use of a binding after its consuming move. */
export const useAfterMoveCode = 'OWN0001' as const
export const partialMoveCode = 'OWN0002' as const
export const explicitMoveRequiredCode = 'OWN0003' as const
export const overlappingAssignmentCode = 'OWN0004' as const
export const incompatibleLoopHeaderCode = 'OWN0005' as const
export const matchBorrowEscapeCode = 'OWN0006' as const
export const exclusiveMatchRequiresMutableCode = 'OWN0007' as const
export const guardConsumesPatternCode = 'OWN0008' as const
export const invalidMatchScrutineePlaceCode = 'OWN0009' as const
export const conflictingSliceLoanCode = 'OWN0010' as const
export const ownerAccessDuringLoanCode = 'OWN0011' as const
export const borrowedMoveCode = 'OWN0012' as const

/** Stable code for an exact `Usize` magnitude outside the selected target word. */
export const usizeTargetOutOfRangeCode = 'LAY0001' as const

/** Every stable diagnostic code any phase can produce. */
export type Code =
  | typeof unsupportedBytesCode
  | typeof missingTokenCode
  | typeof unexpectedTokensCode
  | typeof reservedTemplateSyntaxCode
  | typeof unknownModuleCode
  | typeof selfImportCode
  | typeof duplicateImportCode
  | typeof unknownTypeCode
  | typeof integerOutOfRangeCode
  | typeof duplicateDeclarationNameCode
  | typeof unknownFunctionCode
  | typeof duplicateParameterNameCode
  | typeof unknownParameterReferenceCode
  | typeof wrongCallArityCode
  | typeof rebindingNameCode
  | typeof unknownActorCode
  | typeof unknownActorOperationCode
  | typeof conditionNotBoolCode
  | typeof argumentTypeMismatchCode
  | typeof redundantAliasCode
  | typeof unknownImportedMemberCode
  | typeof inaccessibleImportedMemberCode
  | typeof bindingConflictCode
  | typeof duplicateFieldNameCode
  | typeof expectedTypeCode
  | typeof privateTypeExposureCode
  | typeof inlineRecursiveStructCode
  | typeof externalRawStructLiteralCode
  | typeof unknownStructFieldCode
  | typeof duplicateStructInitializerCode
  | typeof missingStructInitializerCode
  | typeof structFieldTypeMismatchCode
  | typeof projectionOnNonStructCode
  | typeof unknownProjectedFieldCode
  | typeof inaccessibleProjectedFieldCode
  | typeof emptyArrayNeedsContextCode
  | typeof arrayElementTypeMismatchCode
  | typeof arrayLengthMismatchCode
  | typeof indexOnNonArrayCode
  | typeof indexNotI32Code
  | typeof indexOutOfBoundsCode
  | typeof immutableAssignmentCode
  | typeof invalidAssignmentPlaceCode
  | typeof assignmentTypeMismatchCode
  | typeof transferOutsideLoopCode
  | typeof invalidUnionMemberCode
  | typeof incompatibleUnionConversionCode
  | typeof matchScrutineeNotNominalCode
  | typeof matchMemberNotInScrutineeCode
  | typeof unreachableMatchArmCode
  | typeof incompleteMatchCode
  | typeof matchGuardNotBoolCode
  | typeof missingPatternFieldCode
  | typeof duplicatePatternFieldCode
  | typeof patternBindingConflictCode
  | typeof incompatibleMatchResultsCode
  | typeof duplicateTypeParameterCode
  | typeof typeArgumentArityCode
  | typeof typeArgumentInferenceCode
  | typeof polymorphicRecursionCode
  | typeof sliceTypePositionCode
  | typeof invalidBorrowPositionCode
  | typeof invalidBorrowOperandCode
  | typeof exclusiveBorrowRequiresMutableCode
  | typeof invalidSliceReborrowCode
  | typeof implicitSliceDecayCode
  | typeof usizeNegativeCode
  | typeof invalidFailureTypeCode
  | typeof failureRowOnOrdinaryCode
  | typeof failOutsideEffectCode
  | typeof undeclaredFailureCode
  | typeof runNonEffectCode
  | typeof unhandledEffectFailuresCode
  | typeof invalidEffectHandlerCode
  | typeof mutableEffectRecipeCode
  | typeof effectIdentityErasureCode
  | typeof invalidRequirementTypeCode
  | typeof unhandledEffectRequirementsCode
  | typeof invalidEffectRetryCode
  | typeof providerBackedFailureCode
  | typeof invalidEffectProvisionCode
  | typeof useAfterMoveCode
  | typeof partialMoveCode
  | typeof explicitMoveRequiredCode
  | typeof overlappingAssignmentCode
  | typeof incompatibleLoopHeaderCode
  | typeof matchBorrowEscapeCode
  | typeof exclusiveMatchRequiresMutableCode
  | typeof guardConsumesPatternCode
  | typeof invalidMatchScrutineePlaceCode
  | typeof conflictingSliceLoanCode
  | typeof ownerAccessDuringLoanCode
  | typeof borrowedMoveCode
  | typeof usizeTargetOutOfRangeCode

/** A semantic declaration identity carried structurally to avoid a module cycle. */
export interface DeclarationEntity {
  readonly _tag: 'DeclarationId'
  readonly sourceId: string
  readonly ordinal: number
}

/** A compiler-known built-in actor operation named as a diagnostic target. */
export interface BuiltinEntity {
  readonly _tag: 'BuiltinTarget'
  readonly actor: string
  readonly operation: string
}

/** Structured per-code data explaining why the originating phase diagnosed. */
export type Reason =
  | { readonly _tag: 'UnsupportedBytes' }
  | { readonly _tag: 'MissingToken'; readonly expected: Token.TokenKind }
  | { readonly _tag: 'UnexpectedTokens' }
  | { readonly _tag: 'ReservedTemplateSyntax' }
  | { readonly _tag: 'UnknownModule'; readonly module: string }
  | { readonly _tag: 'SelfImport'; readonly module: string }
  | { readonly _tag: 'DuplicateImport'; readonly module: string }
  | { readonly _tag: 'UnknownType'; readonly spelling: string }
  | {
      readonly _tag: 'IntegerOutOfRange'
      readonly spelling: string
      readonly maximum: 2147483647
      readonly minimum: -2147483648
    }
  | { readonly _tag: 'UsizeNegative'; readonly spelling: string }
  | { readonly _tag: 'InvalidFailureType'; readonly type: string }
  | { readonly _tag: 'FailureRowOnOrdinary' }
  | { readonly _tag: 'FailOutsideEffect' }
  | { readonly _tag: 'UndeclaredFailure'; readonly type: string }
  | { readonly _tag: 'RunNonEffect'; readonly type: string }
  | { readonly _tag: 'UnhandledEffectFailures'; readonly failures: ReadonlyArray<string> }
  | { readonly _tag: 'InvalidEffectHandler'; readonly detail: string }
  | { readonly _tag: 'MutableEffectRecipe' }
  | { readonly _tag: 'EffectIdentityErasure' }
  | { readonly _tag: 'InvalidRequirementType'; readonly type: string }
  | { readonly _tag: 'UnhandledEffectRequirements'; readonly requirements: ReadonlyArray<string> }
  | { readonly _tag: 'InvalidEffectRetry'; readonly detail: string }
  | { readonly _tag: 'ProviderBackedFailure'; readonly type: string }
  | { readonly _tag: 'InvalidEffectProvision'; readonly detail: string }
  | {
      readonly _tag: 'UsizeTargetOutOfRange'
      readonly spelling: string
      readonly target: string
      readonly bits: 32 | 64
      readonly maximum: string
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
  | {
      readonly _tag: 'WrongCallArity'
      readonly target: DeclarationEntity | BuiltinEntity
      readonly expectedCount: number
      readonly actualCount: number
    }
  | {
      readonly _tag: 'RebindingName'
      readonly spelling: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'UnknownActor'; readonly spelling: string }
  | {
      readonly _tag: 'UnknownActorOperation'
      readonly actor: string
      readonly spelling: string
    }
  | { readonly _tag: 'ConditionNotBool'; readonly actual: string }
  | {
      readonly _tag: 'ArgumentTypeMismatch'
      readonly expected: string
      readonly actual: string
    }
  | { readonly _tag: 'RedundantAlias'; readonly spelling: string }
  | { readonly _tag: 'UnknownImportedMember'; readonly module: string; readonly spelling: string }
  | {
      readonly _tag: 'InaccessibleImportedMember'
      readonly module: string
      readonly spelling: string
    }
  | { readonly _tag: 'BindingConflict'; readonly spelling: string }
  | {
      readonly _tag: 'DuplicateFieldName'
      readonly spelling: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'ExpectedType'; readonly spelling: string }
  | { readonly _tag: 'PrivateTypeExposure'; readonly type: string }
  | { readonly _tag: 'InlineRecursiveStruct'; readonly members: ReadonlyArray<string> }
  | { readonly _tag: 'ExternalRawStructLiteral'; readonly type: string }
  | { readonly _tag: 'UnknownStructField'; readonly type: string; readonly field: string }
  | {
      readonly _tag: 'DuplicateStructInitializer'
      readonly field: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'MissingStructInitializer'; readonly type: string; readonly field: string }
  | {
      readonly _tag: 'StructFieldTypeMismatch'
      readonly field: string
      readonly expected: string
      readonly actual: string
    }
  | { readonly _tag: 'ProjectionOnNonStruct'; readonly actual: string }
  | { readonly _tag: 'UnknownProjectedField'; readonly type: string; readonly field: string }
  | { readonly _tag: 'InaccessibleProjectedField'; readonly type: string; readonly field: string }
  | { readonly _tag: 'EmptyArrayNeedsContext' }
  | {
      readonly _tag: 'ArrayElementTypeMismatch'
      readonly expected: string
      readonly actual: string
      readonly index: number
    }
  | {
      readonly _tag: 'ArrayLengthMismatch'
      readonly expected: number
      readonly actual: number
    }
  | { readonly _tag: 'IndexOnNonArray'; readonly actual: string }
  | { readonly _tag: 'IndexNotI32'; readonly actual: string }
  | { readonly _tag: 'IndexOutOfBounds'; readonly index: number; readonly length: number }
  | { readonly _tag: 'ImmutableAssignment'; readonly spelling: string }
  | { readonly _tag: 'InvalidAssignmentPlace' }
  | {
      readonly _tag: 'AssignmentTypeMismatch'
      readonly expected: string
      readonly actual: string
    }
  | { readonly _tag: 'TransferOutsideLoop'; readonly transfer: 'break' | 'continue' }
  | { readonly _tag: 'InvalidUnionMember'; readonly type: string }
  | {
      readonly _tag: 'IncompatibleUnionConversion'
      readonly source: string
      readonly target: string
      readonly missing: ReadonlyArray<string>
    }
  | { readonly _tag: 'MatchScrutineeNotNominal'; readonly actual: string }
  | {
      readonly _tag: 'MatchMemberNotInScrutinee'
      readonly member: string
      readonly scrutinee: string
    }
  | { readonly _tag: 'UnreachableMatchArm'; readonly member: string }
  | { readonly _tag: 'IncompleteMatch'; readonly missing: ReadonlyArray<string> }
  | { readonly _tag: 'MatchGuardNotBool'; readonly actual: string }
  | { readonly _tag: 'MissingPatternField'; readonly type: string; readonly field: string }
  | {
      readonly _tag: 'DuplicatePatternField'
      readonly field: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'PatternBindingConflict'
      readonly spelling: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'IncompatibleMatchResults'; readonly types: ReadonlyArray<string> }
  | {
      readonly _tag: 'DuplicateTypeParameter'
      readonly spelling: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'TypeArgumentArity'
      readonly target: string
      readonly expected: number
      readonly actual: number
    }
  | { readonly _tag: 'TypeArgumentInference'; readonly target: string }
  | {
      readonly _tag: 'PolymorphicRecursion'
      readonly caller: string
      readonly target: string
    }
  | {
      readonly _tag: 'SliceTypePosition'
      readonly position: 'parameter' | 'return' | 'field' | 'type argument'
    }
  | { readonly _tag: 'InvalidBorrowPosition' }
  | { readonly _tag: 'InvalidBorrowOperand' }
  | { readonly _tag: 'ExclusiveBorrowRequiresMutable'; readonly spelling: string }
  | {
      readonly _tag: 'InvalidSliceReborrow'
      readonly parent: 'Shared' | 'Exclusive'
      readonly requested: 'Shared' | 'Exclusive'
    }
  | { readonly _tag: 'ImplicitSliceDecay'; readonly expected: string }
  | {
      readonly _tag: 'UseAfterMove'
      readonly spelling: string
      readonly moveSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'PartialMove' }
  | { readonly _tag: 'ExplicitMoveRequired'; readonly spelling: string }
  | { readonly _tag: 'OverlappingAssignment'; readonly spelling: string }
  | { readonly _tag: 'IncompatibleLoopHeader'; readonly loop: number }
  | { readonly _tag: 'MatchBorrowEscape'; readonly spelling: string }
  | { readonly _tag: 'ExclusiveMatchRequiresMutable'; readonly spelling: string }
  | { readonly _tag: 'GuardConsumesPattern'; readonly spelling: string }
  | { readonly _tag: 'InvalidMatchScrutineePlace'; readonly access: 'Move' | 'Exclusive' }
  | {
      readonly _tag: 'ConflictingSliceLoan'
      readonly existing: 'Shared' | 'Exclusive'
      readonly requested: 'Shared' | 'Exclusive'
      readonly loanSpan: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'OwnerAccessDuringLoan'
      readonly spelling: string
      readonly access: 'Read' | 'Write' | 'Move'
      readonly loanSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'BorrowedMove' }

/** One additional source span labeled with its relationship to the diagnostic. */
export interface RelatedSpan {
  readonly label: string
  readonly span: SourceSpan.SourceSpan
}

/** One unambiguous machine-applicable replacement. Modeled; no phase emits one yet. */
export interface Edit {
  readonly span: SourceSpan.SourceSpan
  readonly replacement: string
}

/**
 * A deterministic diagnostic identity: derived from the phase, code, primary span, and the
 * ordinal among equal (phase, code, span) diagnostics within one phase result. Reproducible
 * across runs because every phase is deterministic.
 */
export interface Identity {
  readonly _tag: 'DiagnosticIdentity'
  readonly phase: Phase
  readonly code: Code
  readonly span: SourceSpan.SourceSpan
  readonly ordinal: number
}

/** A recoverable source mistake published by one compiler phase as ordinary data. */
export interface Diagnostic {
  readonly _tag: 'Diagnostic'
  readonly phase: Phase
  readonly code: Code
  readonly severity: 'error'
  readonly message: string
  readonly reason: Reason
  readonly span: SourceSpan.SourceSpan
  readonly relatedSpans?: ReadonlyArray<RelatedSpan>
  readonly notes?: ReadonlyArray<string>
  readonly edits?: ReadonlyArray<Edit>
  readonly entity?: DeclarationEntity
  readonly cause?: Identity
}

/** Tests whether a diagnostic collection contains an emission-blocking error. */
export const hasErrors = (diagnostics: ReadonlyArray<Diagnostic>): boolean =>
  diagnostics.some((diagnostic) => diagnostic.severity === 'error')

/** Tests whether target-dependent specialization must stop for a generic source error. */
export const hasGenericSpecializationErrors = (diagnostics: ReadonlyArray<Diagnostic>): boolean =>
  diagnostics.some(
    (diagnostic) =>
      diagnostic.code === duplicateTypeParameterCode ||
      diagnostic.code === typeArgumentArityCode ||
      diagnostic.code === typeArgumentInferenceCode ||
      diagnostic.code === polymorphicRecursionCode,
  )

/** Derives the identity of one diagnostic given its ordinal among equals. */
export const identity = (self: Diagnostic, ordinal = 0): Identity =>
  Object.freeze({
    _tag: 'DiagnosticIdentity',
    phase: self.phase,
    code: self.code,
    span: self.span,
    ordinal,
  })

/** Derives every identity for one phase result, assigning ordinals among equals in order. */
export const identify = (diagnostics: ReadonlyArray<Diagnostic>): ReadonlyArray<Identity> => {
  const seen = new Map<string, number>()
  return Object.freeze(
    diagnostics.map((diagnostic) => {
      const key = `${diagnostic.phase}\0${diagnostic.code}\0${diagnostic.span.sourceId}\0${diagnostic.span.start}\0${diagnostic.span.end}`
      const ordinal = seen.get(key) ?? 0
      seen.set(key, ordinal + 1)
      return identity(diagnostic, ordinal)
    }),
  )
}

/** Tests structural identity equality. */
export const identityEquals = (self: Identity, other: Identity): boolean =>
  self.phase === other.phase &&
  self.code === other.code &&
  self.span.sourceId === other.span.sourceId &&
  self.span.start === other.span.start &&
  self.span.end === other.span.end &&
  self.ordinal === other.ordinal

const compareStrings = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0

/**
 * The single cross-phase ordering: module identity, primary span, code, then phase rank.
 *
 * ponytail: span.sourceId stands in for canonical module identity until load-module-closure.
 */
export const compare = (left: Diagnostic, right: Diagnostic): number =>
  compareStrings(left.span.sourceId, right.span.sourceId) ||
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  compareStrings(left.code, right.code) ||
  phaseRank[left.phase] - phaseRank[right.phase]

/** Orders identities with the same keys as {@link compare}, then by ordinal. */
export const compareIdentity = (left: Identity, right: Identity): number =>
  compareStrings(left.span.sourceId, right.span.sourceId) ||
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  compareStrings(left.code, right.code) ||
  phaseRank[left.phase] - phaseRank[right.phase] ||
  left.ordinal - right.ordinal

/**
 * Merges per-phase collections into the one deterministic driver-side sequence.
 *
 * The stable sort preserves each phase's within-result order as the final tie-breaker.
 */
export const merge = (
  ...collections: ReadonlyArray<ReadonlyArray<Diagnostic>>
): ReadonlyArray<Diagnostic> => Object.freeze(collections.flat().sort(compare))

/** Creates the diagnostic associated with one `Invalid` token. */
export const unsupportedBytes = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: unsupportedBytesCode,
    severity: 'error',
    message: 'Unsupported byte sequence',
    reason: Object.freeze({ _tag: 'UnsupportedBytes' }),
    span,
  })

/** Creates the diagnostic associated with one missing token leaf. */
export const missingToken = (expected: Token.TokenKind, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'parser',
    code: missingTokenCode,
    severity: 'error',
    message: `Expected ${expected}`,
    reason: Object.freeze({ _tag: 'MissingToken', expected }),
    span,
  })

/** Creates the diagnostic associated with one unexpected-token error node. */
export const unexpectedTokens = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'parser',
    code: unexpectedTokensCode,
    severity: 'error',
    message: 'Unexpected token sequence',
    reason: Object.freeze({ _tag: 'UnexpectedTokens' }),
    span,
  })

/** Creates the diagnostic for a future template expression start in primary position. */
export const reservedTemplateSyntax = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'parser',
    code: reservedTemplateSyntaxCode,
    severity: 'error',
    message: 'Template syntax is reserved but not implemented',
    reason: Object.freeze({ _tag: 'ReservedTemplateSyntax' }),
    span,
  })

/** Creates the diagnostic for an import whose target module is not supplied. */
export const unknownModule = (module: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'module',
    code: unknownModuleCode,
    severity: 'error',
    message: `Unknown module ${module}`,
    reason: Object.freeze({ _tag: 'UnknownModule', module }),
    span,
  })

/** Creates the diagnostic for an import redundantly naming its own module. */
export const selfImport = (module: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'module',
    code: selfImportCode,
    severity: 'error',
    message: `Module ${module} imports itself`,
    reason: Object.freeze({ _tag: 'SelfImport', module }),
    span,
  })

export const duplicateImport = (module: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'module',
    code: duplicateImportCode,
    severity: 'error',
    message: `Module ${module} is imported more than once`,
    reason: Object.freeze({ _tag: 'DuplicateImport', module }),
    span,
  })

export const redundantAlias = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: redundantAliasCode,
    severity: 'error',
    message: `Alias ${spelling} does not change the name`,
    reason: Object.freeze({ _tag: 'RedundantAlias', spelling }),
    span,
  })

export const unknownImportedMember = (
  module: string,
  spelling: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownImportedMemberCode,
    severity: 'error',
    message: `Module ${module} has no member ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownImportedMember', module, spelling }),
    span,
  })

export const inaccessibleImportedMember = (
  module: string,
  spelling: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: inaccessibleImportedMemberCode,
    severity: 'error',
    message: `${module}.${spelling} is private`,
    reason: Object.freeze({ _tag: 'InaccessibleImportedMember', module, spelling }),
    span,
  })

export const bindingConflict = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: bindingConflictCode,
    severity: 'error',
    message: `Multiple bindings claim ${spelling}`,
    reason: Object.freeze({ _tag: 'BindingConflict', spelling }),
    span,
  })

/** Creates the diagnostic for a field name repeated within one struct. */
export const duplicateFieldName = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateFieldNameCode,
    severity: 'error',
    message: `Duplicate field name ${spelling}`,
    reason: Object.freeze({ _tag: 'DuplicateFieldName', spelling, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

/** Creates the diagnostic for a value declaration used as a declared type. */
export const expectedType = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: expectedTypeCode,
    severity: 'error',
    message: `Expected a type, found ${spelling}`,
    reason: Object.freeze({ _tag: 'ExpectedType', spelling }),
    span,
  })

/** Creates the diagnostic for a public contract exposing a private nominal type. */
export const privateTypeExposure = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: privateTypeExposureCode,
    severity: 'error',
    message: `Public declaration exposes private type ${type}`,
    reason: Object.freeze({ _tag: 'PrivateTypeExposure', type }),
    span,
  })

/** Creates the one canonical diagnostic for an inline recursive struct component. */
export const inlineRecursiveStruct = (
  members: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: inlineRecursiveStructCode,
    severity: 'error',
    message: `Inline recursive struct layout: ${members.join(' -> ')}`,
    reason: Object.freeze({ _tag: 'InlineRecursiveStruct', members: Object.freeze([...members]) }),
    span,
  })

export const externalRawStructLiteral = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: externalRawStructLiteralCode,
    severity: 'error',
    message: `Raw construction of ${type} is limited to its defining module`,
    reason: Object.freeze({ _tag: 'ExternalRawStructLiteral', type }),
    span,
  })

export const unknownStructField = (
  type: string,
  field: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownStructFieldCode,
    severity: 'error',
    message: `${type} has no field ${field}`,
    reason: Object.freeze({ _tag: 'UnknownStructField', type, field }),
    span,
  })

export const duplicateStructInitializer = (
  field: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateStructInitializerCode,
    severity: 'error',
    message: `Field ${field} is initialized more than once`,
    reason: Object.freeze({ _tag: 'DuplicateStructInitializer', field, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first initialized here', span: originalSpan }),
    ]),
  })

export const missingStructInitializer = (
  type: string,
  field: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: missingStructInitializerCode,
    severity: 'error',
    message: `Missing initializer for ${type}.${field}`,
    reason: Object.freeze({ _tag: 'MissingStructInitializer', type, field }),
    span,
  })

export const structFieldTypeMismatch = (
  field: string,
  expected: string,
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: structFieldTypeMismatchCode,
    severity: 'error',
    message: `Field ${field} expects ${expected} but received ${actual}`,
    reason: Object.freeze({ _tag: 'StructFieldTypeMismatch', field, expected, actual }),
    span,
  })

export const projectionOnNonStruct = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: projectionOnNonStructCode,
    severity: 'error',
    message: `Cannot project a field from ${actual}`,
    reason: Object.freeze({ _tag: 'ProjectionOnNonStruct', actual }),
    span,
  })

export const unknownProjectedField = (
  type: string,
  field: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownProjectedFieldCode,
    severity: 'error',
    message: `${type} has no field ${field}`,
    reason: Object.freeze({ _tag: 'UnknownProjectedField', type, field }),
    span,
  })

export const inaccessibleProjectedField = (
  type: string,
  field: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: inaccessibleProjectedFieldCode,
    severity: 'error',
    message: `${type}.${field} is private`,
    reason: Object.freeze({ _tag: 'InaccessibleProjectedField', type, field }),
    span,
  })

export const emptyArrayNeedsContext = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: emptyArrayNeedsContextCode,
    severity: 'error',
    message: 'An empty array literal needs an expected Array type',
    reason: Object.freeze({ _tag: 'EmptyArrayNeedsContext' }),
    span,
  })

export const arrayElementTypeMismatch = (
  expected: string,
  actual: string,
  index: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: arrayElementTypeMismatchCode,
    severity: 'error',
    message: `Array element ${index} expects ${expected} but received ${actual}`,
    reason: Object.freeze({ _tag: 'ArrayElementTypeMismatch', expected, actual, index }),
    span,
  })

export const arrayLengthMismatch = (
  expected: number,
  actual: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: arrayLengthMismatchCode,
    severity: 'error',
    message: `Array literal expects ${expected} elements but received ${actual}`,
    reason: Object.freeze({ _tag: 'ArrayLengthMismatch', expected, actual }),
    span,
  })

export const indexOnNonArray = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: indexOnNonArrayCode,
    severity: 'error',
    message: `Cannot index ${actual}`,
    reason: Object.freeze({ _tag: 'IndexOnNonArray', actual }),
    span,
  })

export const indexNotI32 = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: indexNotI32Code,
    severity: 'error',
    message: `Array index must be I32, found ${actual}`,
    reason: Object.freeze({ _tag: 'IndexNotI32', actual }),
    span,
  })

export const indexOutOfBounds = (
  index: number,
  length: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: indexOutOfBoundsCode,
    severity: 'error',
    message: `Array index ${index} is outside length ${length}`,
    reason: Object.freeze({ _tag: 'IndexOutOfBounds', index, length }),
    span,
  })

/** Creates the diagnostic for one present identifier that cannot resolve as a type. */
export const unknownType = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownTypeCode,
    severity: 'error',
    message: `Unknown type ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownType', spelling }),
    span,
  })

/** Creates the diagnostic for one decimal literal outside the signed `I32` range. */
export const integerOutOfRange = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: integerOutOfRangeCode,
    severity: 'error',
    message: 'Integer literal exceeds the I32 range',
    reason: Object.freeze({
      _tag: 'IntegerOutOfRange',
      spelling,
      maximum: 2147483647,
      minimum: -2147483648,
    }),
    span,
  })

/** Creates the target-independent diagnostic for a negative `Usize` literal. */
export const usizeNegative = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: usizeNegativeCode,
    severity: 'error',
    message: 'Usize literals cannot be negative',
    reason: Object.freeze({ _tag: 'UsizeNegative', spelling }),
    span,
  })

/** Creates the diagnostic for a failure-row member that cannot be an owned nominal error. */
export const invalidFailureType = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidFailureTypeCode,
    severity: 'error',
    message: `Effect failure ${type} must be one concrete nominal type`,
    reason: Object.freeze({ _tag: 'InvalidFailureType', type }),
    span,
  })

/** Creates the diagnostic for a requirement that cannot name one concrete capability type. */
export const invalidRequirementType = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidRequirementTypeCode,
    severity: 'error',
    message: `Effect requirement ${type} must be one concrete nominal capability type`,
    reason: Object.freeze({ _tag: 'InvalidRequirementType', type }),
    span,
  })

/** Creates the diagnostic for spelling a failure row on a direct ordinary function. */
export const failureRowOnOrdinary = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: failureRowOnOrdinaryCode,
    severity: 'error',
    message: 'Only effect functions may declare a failure row',
    reason: Object.freeze({ _tag: 'FailureRowOnOrdinary' }),
    span,
  })

export const failOutsideEffect = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: failOutsideEffectCode,
    severity: 'error',
    message: 'Only effect functions may originate a typed failure',
    reason: Object.freeze({ _tag: 'FailOutsideEffect' }),
    span,
  })

export const undeclaredFailure = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: undeclaredFailureCode,
    severity: 'error',
    message: `Failure ${type} is not declared by this effect function`,
    reason: Object.freeze({ _tag: 'UndeclaredFailure', type }),
    span,
  })

export const runNonEffect = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: runNonEffectCode,
    severity: 'error',
    message: `Cannot run non-effect value ${type}`,
    reason: Object.freeze({ _tag: 'RunNonEffect', type }),
    span,
  })

export const unhandledEffectFailures = (
  failures: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unhandledEffectFailuresCode,
    severity: 'error',
    message: `Run leaves unhandled failures: ${failures.join(' | ')}`,
    reason: Object.freeze({ _tag: 'UnhandledEffectFailures', failures: Object.freeze(failures) }),
    span,
  })

export const unhandledEffectRequirements = (
  requirements: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unhandledEffectRequirementsCode,
    severity: 'error',
    message: `Run leaves unsatisfied requirements: ${requirements.join(' | ')}`,
    reason: Object.freeze({
      _tag: 'UnhandledEffectRequirements',
      requirements: Object.freeze(requirements),
    }),
    span,
  })

export const invalidEffectRetry = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidEffectRetryCode,
    severity: 'error',
    message: `Invalid Effect.retry input: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidEffectRetry', detail }),
    span,
  })

export const providerBackedFailure = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: providerBackedFailureCode,
    severity: 'error',
    message: `Failure ${type} is not detached because it contains a lexical borrow`,
    reason: Object.freeze({ _tag: 'ProviderBackedFailure', type }),
    span,
  })

export const invalidEffectProvision = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidEffectProvisionCode,
    severity: 'error',
    message: `Invalid Effect provider: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidEffectProvision', detail }),
    span,
  })

export const invalidEffectHandler = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidEffectHandlerCode,
    severity: 'error',
    message: `Invalid Effect.catch handler: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidEffectHandler', detail }),
    span,
  })

export const mutableEffectRecipe = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: mutableEffectRecipeCode,
    severity: 'error',
    message: 'Effect recipe bindings are immutable',
    reason: Object.freeze({ _tag: 'MutableEffectRecipe' }),
    span,
  })

export const effectIdentityErasure = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: effectIdentityErasureCode,
    severity: 'error',
    message:
      'Cannot merge Effect values from different construction sites without explicit erasure',
    reason: Object.freeze({ _tag: 'EffectIdentityErasure' }),
    span,
  })

/** Creates the target-owned diagnostic for a `Usize` literal outside its selected word. */
export const usizeTargetOutOfRange = (
  spelling: string,
  target: string,
  bits: 32 | 64,
  span: SourceSpan.SourceSpan,
): Diagnostic => {
  const maximum = bits === 32 ? '4294967295' : '18446744073709551615'
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'layout',
    code: usizeTargetOutOfRangeCode,
    severity: 'error',
    message: `Usize literal ${spelling} exceeds the ${bits}-bit range for ${target}`,
    reason: Object.freeze({
      _tag: 'UsizeTargetOutOfRange',
      spelling,
      target,
      bits,
      maximum,
    }),
    span,
  })
}

/** Creates the diagnostic for a qualified call naming an unknown built-in actor. */
export const unknownActor = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownActorCode,
    severity: 'error',
    message: `Unknown actor ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownActor', spelling }),
    span,
  })

/** Creates the diagnostic for a known actor called with an unknown operation. */
export const unknownActorOperation = (
  actor: string,
  spelling: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownActorOperationCode,
    severity: 'error',
    message: `${actor} has no operation ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownActorOperation', actor, spelling }),
    span,
  })

/** Creates the diagnostic for a declaration name repeated after its first occurrence. */
export const duplicateDeclarationName = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateDeclarationNameCode,
    severity: 'error',
    message: `Duplicate declaration name ${spelling}`,
    reason: Object.freeze({
      _tag: 'DuplicateDeclarationName',
      spelling,
      originalSpan,
    }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

/** Creates the diagnostic for one present call name with no matching declaration. */
export const unknownFunction = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
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
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateParameterNameCode,
    severity: 'error',
    message: `Duplicate parameter name ${spelling}`,
    reason: Object.freeze({
      _tag: 'DuplicateParameterName',
      spelling,
      originalSpan,
    }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

/** Creates the diagnostic for one present value name with no matching local parameter. */
export const unknownParameterReference = (
  spelling: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownParameterReferenceCode,
    severity: 'error',
    message: `Unknown parameter ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownParameterReference', spelling }),
    span,
  })

/** Creates the diagnostic for a binding name that repeats an existing local declaration. */
export const rebindingName = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: rebindingNameCode,
    severity: 'error',
    message: `Cannot rebind ${spelling}`,
    reason: Object.freeze({ _tag: 'RebindingName', spelling, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

/** Creates the diagnostic for a conditional whose condition is not `Bool`. */
export const conditionNotBool = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: conditionNotBoolCode,
    severity: 'error',
    message: `Condition must be Bool, found ${actual}`,
    reason: Object.freeze({ _tag: 'ConditionNotBool', actual }),
    span,
  })

export const immutableAssignment = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: immutableAssignmentCode,
    severity: 'error',
    message: `Cannot assign through immutable binding ${spelling}`,
    reason: Object.freeze({ _tag: 'ImmutableAssignment', spelling }),
    span,
  })

export const invalidAssignmentPlace = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidAssignmentPlaceCode,
    severity: 'error',
    message: 'Assignment requires a writable binding, field, or indexed place',
    reason: Object.freeze({ _tag: 'InvalidAssignmentPlace' }),
    span,
  })

export const assignmentTypeMismatch = (
  expected: string,
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: assignmentTypeMismatchCode,
    severity: 'error',
    message: `Assignment expected ${expected} but received ${actual}`,
    reason: Object.freeze({ _tag: 'AssignmentTypeMismatch', expected, actual }),
    span,
  })

export const transferOutsideLoop = (
  transfer: 'break' | 'continue',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: transferOutsideLoopCode,
    severity: 'error',
    message: `${transfer} is only valid inside a loop`,
    reason: Object.freeze({ _tag: 'TransferOutsideLoop', transfer }),
    span,
  })

export const invalidUnionMember = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidUnionMemberCode,
    severity: 'error',
    message: `Structural union members must be nominal types, found ${type}`,
    reason: Object.freeze({ _tag: 'InvalidUnionMember', type }),
    span,
  })

export const incompatibleUnionConversion = (
  source: string,
  target: string,
  missing: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: incompatibleUnionConversionCode,
    severity: 'error',
    message: `${source} cannot widen to ${target}; missing ${missing.join(', ')}`,
    reason: Object.freeze({
      _tag: 'IncompatibleUnionConversion',
      source,
      target,
      missing: Object.freeze([...missing]),
    }),
    span,
  })

export const matchScrutineeNotNominal = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: matchScrutineeNotNominalCode,
    severity: 'error',
    message: `Cannot match non-nominal type ${actual}`,
    reason: Object.freeze({ _tag: 'MatchScrutineeNotNominal', actual }),
    span,
  })

export const matchMemberNotInScrutinee = (
  member: string,
  scrutinee: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: matchMemberNotInScrutineeCode,
    severity: 'error',
    message: `${member} is not a member of ${scrutinee}`,
    reason: Object.freeze({ _tag: 'MatchMemberNotInScrutinee', member, scrutinee }),
    span,
  })

export const unreachableMatchArm = (member: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unreachableMatchArmCode,
    severity: 'error',
    message: `Unreachable match arm ${member}`,
    reason: Object.freeze({ _tag: 'UnreachableMatchArm', member }),
    span,
  })

export const incompleteMatch = (
  missing: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: incompleteMatchCode,
    severity: 'error',
    message: `Match does not cover ${missing.join(', ')}`,
    reason: Object.freeze({ _tag: 'IncompleteMatch', missing: Object.freeze([...missing]) }),
    span,
  })

export const matchGuardNotBool = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: matchGuardNotBoolCode,
    severity: 'error',
    message: `Match guard must be Bool, found ${actual}`,
    reason: Object.freeze({ _tag: 'MatchGuardNotBool', actual }),
    span,
  })

export const missingPatternField = (
  type: string,
  field: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: missingPatternFieldCode,
    severity: 'error',
    message: `Pattern for ${type} is missing field ${field}; add it or use ..`,
    reason: Object.freeze({ _tag: 'MissingPatternField', type, field }),
    span,
  })

export const duplicatePatternField = (
  field: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicatePatternFieldCode,
    severity: 'error',
    message: `Pattern field ${field} appears more than once`,
    reason: Object.freeze({ _tag: 'DuplicatePatternField', field, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first matched here', span: originalSpan }),
    ]),
  })

export const patternBindingConflict = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: patternBindingConflictCode,
    severity: 'error',
    message: `Pattern binding ${spelling} conflicts with an existing declaration`,
    reason: Object.freeze({ _tag: 'PatternBindingConflict', spelling, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

export const incompatibleMatchResults = (
  types: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: incompatibleMatchResultsCode,
    severity: 'error',
    message: `Match arms have incompatible result types: ${types.join(', ')}`,
    reason: Object.freeze({
      _tag: 'IncompatibleMatchResults',
      types: Object.freeze([...types]),
    }),
    span,
  })

export const duplicateTypeParameter = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateTypeParameterCode,
    severity: 'error',
    message: `Duplicate type parameter ${spelling}`,
    reason: Object.freeze({ _tag: 'DuplicateTypeParameter', spelling, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

export const typeArgumentArity = (
  target: string,
  expected: number,
  actual: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: typeArgumentArityCode,
    severity: 'error',
    message: `${target} expects ${expected} type argument${expected === 1 ? '' : 's'}, received ${actual}`,
    reason: Object.freeze({ _tag: 'TypeArgumentArity', target, expected, actual }),
    span,
  })

export const typeArgumentInference = (target: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: typeArgumentInferenceCode,
    severity: 'error',
    message: `Cannot infer all type arguments for ${target} from supplied values`,
    reason: Object.freeze({ _tag: 'TypeArgumentInference', target }),
    span,
  })

export const polymorphicRecursion = (
  caller: string,
  target: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: polymorphicRecursionCode,
    severity: 'error',
    message: `Recursive specialization changes type arguments from ${caller} to ${target}`,
    reason: Object.freeze({ _tag: 'PolymorphicRecursion', caller, target }),
    span,
  })

export const sliceTypePosition = (
  position: 'parameter' | 'return' | 'field' | 'type argument',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: sliceTypePositionCode,
    severity: 'error',
    message:
      position === 'parameter'
        ? 'A slice must be the complete type of an ordinary function parameter'
        : `A slice cannot appear in a ${position} type`,
    reason: Object.freeze({ _tag: 'SliceTypePosition', position }),
    span,
  })

export const invalidBorrowPosition = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidBorrowPositionCode,
    severity: 'error',
    message: 'A slice borrow is only valid as an immediate ordinary-call argument',
    reason: Object.freeze({ _tag: 'InvalidBorrowPosition' }),
    span,
  })

export const invalidBorrowOperand = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidBorrowOperandCode,
    severity: 'error',
    message: 'A slice borrow requires a direct stable array binding or slice parameter',
    reason: Object.freeze({ _tag: 'InvalidBorrowOperand' }),
    span,
  })

export const exclusiveBorrowRequiresMutable = (
  spelling: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: exclusiveBorrowRequiresMutableCode,
    severity: 'error',
    message: `Exclusive borrowing requires mutable binding ${spelling}`,
    reason: Object.freeze({ _tag: 'ExclusiveBorrowRequiresMutable', spelling }),
    span,
  })

export const invalidSliceReborrow = (
  parent: 'Shared' | 'Exclusive',
  requested: 'Shared' | 'Exclusive',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidSliceReborrowCode,
    severity: 'error',
    message: 'A shared slice cannot be reborrowed exclusively',
    reason: Object.freeze({ _tag: 'InvalidSliceReborrow', parent, requested }),
    span,
  })

export const implicitSliceDecay = (expected: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: implicitSliceDecayCode,
    severity: 'error',
    message: `Passing an array as ${expected} requires an explicit borrow`,
    reason: Object.freeze({ _tag: 'ImplicitSliceDecay', expected }),
    span,
  })

/** Creates the diagnostic for a call argument whose type mismatches its parameter. */
export const argumentTypeMismatch = (
  expected: string,
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: argumentTypeMismatchCode,
    severity: 'error',
    message: `Expected ${expected} but received ${actual}`,
    reason: Object.freeze({ _tag: 'ArgumentTypeMismatch', expected, actual }),
    span,
  })

/** Creates the diagnostic for a binding used again after its consuming move. */
export const useAfterMove = (
  spelling: string,
  moveSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: useAfterMoveCode,
    severity: 'error',
    message: `${spelling} was moved and cannot be used again`,
    reason: Object.freeze({ _tag: 'UseAfterMove', spelling, moveSpan }),
    span,
    relatedSpans: Object.freeze([Object.freeze({ label: 'moved here', span: moveSpan })]),
  })

export const partialMove = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: partialMoveCode,
    severity: 'error',
    message: 'Struct fields cannot be moved independently',
    reason: Object.freeze({ _tag: 'PartialMove' }),
    span,
  })

export const explicitMoveRequired = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: explicitMoveRequiredCode,
    severity: 'error',
    message: `Moving ${spelling} requires an explicit move`,
    reason: Object.freeze({ _tag: 'ExplicitMoveRequired', spelling }),
    span,
  })

export const overlappingAssignment = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: overlappingAssignmentCode,
    severity: 'error',
    message: `Assignment to ${spelling} consumes the same owner before replacement commits`,
    reason: Object.freeze({ _tag: 'OverlappingAssignment', spelling }),
    span,
  })

export const incompatibleLoopHeader = (loop: number, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: incompatibleLoopHeaderCode,
    severity: 'error',
    message: `Loop ${loop} repeats with incompatible owner liveness`,
    reason: Object.freeze({ _tag: 'IncompatibleLoopHeader', loop }),
    span,
  })

export const matchBorrowEscape = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: matchBorrowEscapeCode,
    severity: 'error',
    message: `Borrowed pattern binding ${spelling} cannot escape its match arm`,
    reason: Object.freeze({ _tag: 'MatchBorrowEscape', spelling }),
    span,
  })

export const exclusiveMatchRequiresMutable = (
  spelling: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: exclusiveMatchRequiresMutableCode,
    severity: 'error',
    message: `Exclusive match requires mutable binding ${spelling}`,
    reason: Object.freeze({ _tag: 'ExclusiveMatchRequiresMutable', spelling }),
    span,
  })

export const guardConsumesPattern = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: guardConsumesPatternCode,
    severity: 'error',
    message: `Match guard cannot consume pattern binding ${spelling}`,
    reason: Object.freeze({ _tag: 'GuardConsumesPattern', spelling }),
    span,
  })

export const invalidMatchScrutineePlace = (
  access: 'Move' | 'Exclusive',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: invalidMatchScrutineePlaceCode,
    severity: 'error',
    message: `${access} match requires a complete binding place`,
    reason: Object.freeze({ _tag: 'InvalidMatchScrutineePlace', access }),
    span,
  })

export const conflictingSliceLoan = (
  existing: 'Shared' | 'Exclusive',
  requested: 'Shared' | 'Exclusive',
  loanSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: conflictingSliceLoanCode,
    severity: 'error',
    message: `${requested} slice loan conflicts with an active ${existing.toLowerCase()} loan`,
    reason: Object.freeze({ _tag: 'ConflictingSliceLoan', existing, requested, loanSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'active loan begins here', span: loanSpan }),
    ]),
  })

export const ownerAccessDuringLoan = (
  spelling: string,
  access: 'Read' | 'Write' | 'Move',
  loanSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: ownerAccessDuringLoanCode,
    severity: 'error',
    message: `${access.toLowerCase()} access to ${spelling} conflicts with an active slice loan`,
    reason: Object.freeze({ _tag: 'OwnerAccessDuringLoan', spelling, access, loanSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'active loan begins here', span: loanSpan }),
    ]),
  })

export const borrowedMove = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: borrowedMoveCode,
    severity: 'error',
    message: 'A non-Copy value cannot be moved out through a borrowed slice place',
    reason: Object.freeze({ _tag: 'BorrowedMove' }),
    span,
  })

/** Creates the diagnostic for a uniquely resolved call with the wrong arity. */
export const wrongCallArity = (
  target: DeclarationEntity | BuiltinEntity,
  expectedCount: number,
  actualCount: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: wrongCallArityCode,
    severity: 'error',
    message: `Expected ${expectedCount} ${expectedCount === 1 ? 'argument' : 'arguments'} but received ${actualCount}`,
    reason: Object.freeze({
      _tag: 'WrongCallArity',
      target,
      expectedCount,
      actualCount,
    }),
    span,
    ...(target._tag === 'DeclarationId' ? { entity: target } : {}),
  })
