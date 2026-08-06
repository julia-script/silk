import type * as SourceSpan from './SourceSpan.js'
import type * as Token from './Token.js'

/** The compiler phase that originated a diagnostic. */
export type Phase = 'lexical' | 'parser' | 'module' | 'semantic' | 'ownership'

const phaseRank: Readonly<Record<Phase, number>> = Object.freeze({
  lexical: 0,
  parser: 1,
  module: 2,
  semantic: 3,
  ownership: 4,
})

/** Stable diagnostic code for a maximal unsupported byte region. */
export const unsupportedBytesCode = 'LEX0001' as const

/** Stable code for one required token that is absent at its insertion position. */
export const missingTokenCode = 'PAR0001' as const

/** Stable code for one maximal region of unexpected concrete tokens. */
export const unexpectedTokensCode = 'PAR0002' as const

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

/** Stable code for a use of a binding after its consuming move. */
export const useAfterMoveCode = 'OWN0001' as const

/** Every stable diagnostic code any phase can produce. */
export type Code =
  | typeof unsupportedBytesCode
  | typeof missingTokenCode
  | typeof unexpectedTokensCode
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
  | typeof useAfterMoveCode

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
  | {
      readonly _tag: 'UseAfterMove'
      readonly spelling: string
      readonly moveSpan: SourceSpan.SourceSpan
    }

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
