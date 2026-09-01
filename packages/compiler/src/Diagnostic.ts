import type * as ProviderSelection from './ProviderSelection.js'
import * as SourceSpan from './SourceSpan.js'
import * as Token from './Token.js'

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

/** Stable code for an identifier-like modifier outside the closed literal vocabulary. */
export const unknownLiteralModifierCode = 'LEX0002' as const

/** Stable code for a literal whose matching closing delimiter is absent. */
export const unterminatedStaticLiteralCode = 'LEX0003' as const

/** Stable code for an integer-literal base prefix that no digit follows. */
export const missingBaseDigitsCode = 'LEX0004' as const

/** Stable code for a number-literal digit separator outside a position between two digits. */
export const invalidDigitSeparatorCode = 'LEX0005' as const

/** Stable code for a float-literal exponent marker that no exponent digit follows. */
export const missingExponentDigitsCode = 'LEX0006' as const

/** Stable code for a character literal that denotes a number of scalars other than one. */
export const characterLiteralScalarCountCode = 'LEX0007' as const

/** Stable code for a duration component amount that is not a whole decimal integer. */
export const invalidDurationAmountCode = 'LEX0008' as const

/** Stable code for an identifier-like suffix outside the duration unit vocabulary. */
export const unknownDurationUnitCode = 'LEX0009' as const

/** Stable code for a duration unit repeated within one compact literal. */
export const repeatedDurationUnitCode = 'LEX0010' as const

/** Stable code for duration components that do not descend from larger to smaller units. */
export const outOfOrderDurationUnitCode = 'LEX0011' as const

/** Stable code for a subordinate duration component outside its canonical field bound. */
export const subordinateDurationOutOfRangeCode = 'LEX0012' as const

/** Stable code for one required token that is absent at its insertion position. */
export const missingTokenCode = 'PAR0001' as const

/** Stable code for one maximal region of unexpected concrete tokens. */
export const unexpectedTokensCode = 'PAR0002' as const

/** Stable code for a primary-expression template start reserved for future support. */
export const reservedTemplateSyntaxCode = 'PAR0003' as const

/** Stable code for an import whose reserved final path segment cannot form an implicit binding. */
export const reservedImportBindingCode = 'PAR0004' as const
/** Stable code for an expression child beyond the parser's supported nesting depth. */
export const expressionNestingLimitExceededCode = 'PAR0005' as const

/** Stable code for an import naming a module absent from the supplied sources. */
export const unknownModuleCode = 'MOD0001' as const

/** Stable code for an import redundantly naming its own containing module. */
export const selfImportCode = 'MOD0002' as const

/** Stable code for a user module claiming the reserved standard-library namespace. */
export const reservedModuleIdentityCode = 'MOD0004' as const

/** Stable code for a present return-type name that is not a bootstrap built-in. */
export const unknownTypeCode = 'SEM0001' as const

/** Stable code for a present decimal literal outside the positive `i32` range. */
export const integerOutOfRangeCode = 'SEM0002' as const

/** Stable code for a present declaration name repeated after its first occurrence. */
export const duplicateDeclarationNameCode = 'SEM0003' as const

/** Stable code for a present call name with no matching top-level declaration. */
export const unknownFunctionCode = 'SEM0004' as const

/** Stable code for a present parameter name repeated after its first occurrence. */
export const duplicateParameterNameCode = 'SEM0005' as const

/** Stable code for a present value name with no matching local declaration. */
export const unknownValueReferenceCode = 'SEM0006' as const

/** Stable code for a uniquely resolved call with the wrong number of arguments. */
export const wrongCallArityCode = 'SEM0007' as const

/** Stable code for a binding whose name repeats a parameter or an earlier binding. */
export const rebindingNameCode = 'SEM0008' as const

/** Stable code for a qualified call naming an unknown built-in actor. */
export const unknownActorCode = 'SEM0009' as const

/** Stable code for a known actor called with an unknown operation name. */
export const unknownActorOperationCode = 'SEM0010' as const

/** Stable code for a conditional whose condition is not `bool`. */
export const conditionNotBoolCode = 'SEM0011' as const

/** Stable code for a call argument whose type mismatches its parameter. */
export const argumentTypeMismatchCode = 'SEM0012' as const
export const unknownImportedMemberCode = 'SEM0014' as const
export const inaccessibleImportedMemberCode = 'SEM0015' as const
export const bindingConflictCode = 'SEM0016' as const
export const duplicateFieldNameCode = 'SEM0017' as const
export const expectedTypeCode = 'SEM0018' as const
export const privateTypeExposureCode = 'SEM0019' as const
export const inlineRecursiveAggregateCode = 'SEM0020' as const
export const inaccessibleStructConstructionCode = 'SEM0021' as const
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
export const indexNotUsizeCode = 'SEM0033' as const
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
/** Stable code for a borrowed view outside an allowed direct type position. */
export const borrowedViewTypePositionCode = 'SEM0054' as const
export const invalidBorrowPositionCode = 'SEM0055' as const
export const invalidBorrowOperandCode = 'SEM0056' as const
export const exclusiveBorrowRequiresMutableCode = 'SEM0057' as const
export const invalidSliceReborrowCode = 'SEM0058' as const
export const implicitSliceDecayCode = 'SEM0059' as const
/** Stable code for a negative decimal literal contextualized as unsigned `usize`. */
export const usizeNegativeCode = 'SEM0060' as const
/** Stable code for a type that cannot inhabit an Effect failure channel. */
export const invalidFailureTypeCode = 'SEM0061' as const
/** Stable code for a failure channel attached to an ordinary function. */
export const failureChannelOnOrdinaryCode = 'SEM0062' as const
export const failOutsideEffectCode = 'SEM0063' as const
export const undeclaredFailureCode = 'SEM0064' as const
export const runNonEffectCode = 'SEM0065' as const
export const unhandledEffectFailuresCode = 'SEM0066' as const
export const invalidEffectHandlerCode = 'SEM0067' as const
export const mutableEffectRecipeCode = 'SEM0068' as const
/** Stable code for a non-concrete or non-nominal capability in a requirement row. */
export const invalidRequirementTypeCode = 'SEM0070' as const
export const unhandledEffectRequirementsCode = 'SEM0071' as const
export const providerBackedFailureCode = 'SEM0073' as const
export const invalidEffectProvisionCode = 'SEM0074' as const
export const nonCallableApplicationCode = 'SEM0075' as const
export const incompatibleCallableSignatureCode = 'SEM0076' as const
export const invalidCallableInvocationAccessCode = 'SEM0077' as const
export const redundantUnaryEmptyCallCode = 'SEM0078' as const
export const callableIdentityErasureCode = 'SEM0080' as const
export const unknownOwnedCallableReturnCode = 'SEM0081' as const
/** Stable code for an Effect join whose alternatives cannot be represented as a finite composite. */
export const nonFiniteEffectJoinCode = 'SEM0132' as const
/** Stable code for a refutable pattern in an unconditional local binding. */
export const refutableLetPatternCode = 'SEM0133' as const
/** Stable code for an operator marker that cannot describe its interface operation. */
export const invalidOperatorContractCode = 'SEM0134' as const
/** Stable code for operator syntax with no marked operation accepting its operands. */
export const operatorNotApplicableCode = 'SEM0135' as const
/** Stable code for operator syntax matched by more than one marked operation. */
export const ambiguousOperatorCode = 'SEM0136' as const
/** Stable code for an unsafe acknowledgement that does not complete an unsafe invocation. */
export const misplacedUnsafeAcknowledgementCode = 'SEM0137' as const
/** Stable code for a statically known allocation/layout specialization mismatch. */
export const localSharedLayoutMismatchCode = 'SEM0138' as const
/** Stable code for a concrete executable that fails one sealed static-property obligation. */
export const unsatisfiedExecutablePropertyCode = 'SEM0139' as const
/** Stable code for an externally parking entry with no explicit Execution owner. */
export const missingExplicitExecutionOwnerCode = 'SEM0140' as const
/** Stable code for an ordinary capability conjoined with one exact executable bound. */
export const invalidExecutablePropertyConjunctCode = 'SEM0141' as const
/** Stable code for a statically known execution-package allocation/layout mismatch. */
export const executionLayoutMismatchCode = 'SEM0142' as const
/** Stable code for `mut` where no mutable owned parameter storage exists. */
export const invalidMutableParameterCode = 'SEM0143' as const
/** Stable code for applying a callable whose borrowed result has no exact source identity. */
export const unknownCallableBorrowSourceCode = 'SEM0144' as const
/** Stable code for mutating an outer callable from a deferred effect recipe. */
export const deferredCallableMutationCode = 'SEM0145' as const
/** Stable code for a scalar enum with no declared members. */
export const emptyEnumCode = 'SEM0146' as const
/** Stable code for a scalar enum representation outside the fixed-width integer set. */
export const unsupportedEnumRepresentationCode = 'SEM0147' as const
/** Stable code for a scalar enum member name repeated after its first declaration. */
export const duplicateEnumMemberNameCode = 'SEM0148' as const
/** Stable code for a scalar enum discriminant repeated after its first declaration. */
export const duplicateEnumDiscriminantCode = 'SEM0149' as const
/** Stable code for an explicit scalar enum discriminant outside its representation range. */
export const enumDiscriminantOutOfRangeCode = 'SEM0150' as const
/** Stable code for an implicit scalar enum successor outside its representation range. */
export const enumImplicitDiscriminantOverflowCode = 'SEM0151' as const
/** Stable code for a negative discriminant under an unsigned scalar enum representation. */
export const unsignedEnumNegativeDiscriminantCode = 'SEM0152' as const
/** Stable code for a member missing from a resolved scalar enum. */
export const unknownEnumMemberCode = 'SEM0153' as const
/** Stable code for a canonical member used through or required by another enum. */
export const wrongEnumMemberCode = 'SEM0154' as const
/** Stable code for implicit mixing between a scalar enum and an integer. */
export const enumIntegerMismatchCode = 'SEM0155' as const
/** Stable code for equality between distinct canonical scalar enums. */
export const crossEnumEqualityCode = 'SEM0156' as const
/** Stable code for direct ordering of scalar enum values. */
export const enumOrderingCode = 'SEM0157' as const
/** Stable code for a scalar enum match that leaves canonical members uncovered. */
export const incompleteEnumMatchCode = 'SEM0158' as const
/** Stable code for a repeated unguarded scalar enum member arm. */
export const duplicateEnumMatchArmCode = 'SEM0159' as const
/** Stable code for a scalar enum arm following an unguarded wildcard. */
export const enumMatchArmAfterWildcardCode = 'SEM0160' as const
/** Stable code for a scalar enum pattern naming a member of another enum. */
export const foreignEnumPatternCode = 'SEM0161' as const
/** Stable code for an integer literal pattern used against a scalar enum. */
export const integerPatternAgainstEnumCode = 'SEM0162' as const
/** Stable code for a raw storage operation outside lexical unsafe authority. */
export const missingUnsafeBoundaryCode = 'SEM0082' as const
/** Stable code for an invalid source-declared capability implementation. */
export const invalidConformanceCode = 'SEM0083' as const
/** Stable code for a Drop implementation outside the compiler-sealed hook contract. */
export const invalidDropHookCode = 'SEM0084' as const
/** Stable code for malformed escapes, invalid UTF-8, or non-byte literal values. */
export const invalidStaticLiteralCode = 'SEM0085' as const
/** Stable code for a typed constant whose type or literal is outside the constant contract. */
export const invalidConstantCode = 'SEM0086' as const
/** Stable code for an expression statement whose result cannot be intentionally ignored. */
export const expressionStatementResultCode = 'SEM0087' as const
/** Stable code for using a generic binder in a value, failure-row, or requirement-row position of another kind. */
export const genericParameterKindMismatchCode = 'SEM0088' as const
/** Stable code for a failure or requirement row that cannot be finitely decomposed. */
export const contractRowInferenceCode = 'SEM0089' as const
/** Stable code for storage, bodies, or defaults inside a source service contract. */
export const invalidServiceDeclarationCode = 'SEM0090' as const
export const invalidReturnedBorrowSignatureCode = 'SEM0091' as const
export const invalidReturnedBorrowOriginCode = 'SEM0092' as const
/** Stable code for one reachable intrinsic unavailable on the requested execution target. */
export const intrinsicTargetUnavailableCode = 'SEM0093' as const
/** Stable code for a float literal spelling no floating-point value can represent. */
export const invalidFloatLiteralCode = 'SEM0095' as const
/** Stable code for a bound operation call whose receiver names more than one bounded parameter. */
export const ambiguousBoundOperationCode = 'SEM0097' as const
/** Stable code for one named type parameter left undetermined by an explicit prefix and the arguments. */
export const uninferredTypeParameterCode = 'SEM0099' as const
/** Stable code for an explicit type argument contradicting the type its value arguments imply. */
export const typeArgumentConflictCode = 'SEM0100' as const

/** Stable code for a bound operation whose selected witness has no lowering. */
export const unlowerableBoundWitnessCode = 'SEM0101' as const

/** Stable code for constructing an aggregate that stores a bare callable value. */
export const storedCallableConstructionCode = 'SEM0103' as const
/** Stable code for the first struct initializer that contradicts an inferred representation. */
export const conflictingInitializerRepresentationCode = 'SEM0104' as const
/** Stable code for the first exact representation that diverges at a static value join. */
export const divergentRepresentationJoinCode = 'SEM0105' as const
/** Stable code for a representation argument whose contract cannot satisfy its required bound. */
export const incompatibleRepresentationBoundCode = 'SEM0106' as const
/** Stable code for storing a represented Effect before its runtime layout is supported. */
export const storedRepresentedEffectConstructionCode = 'SEM0107' as const
/** Stable code for two conformance heads that may name one provider under one interface. */
export const overlappingConformanceCode = 'SEM0119' as const
/** Stable code for a conformance requirement that does not descend toward a base witness. */
export const nonTerminatingConformanceCode = 'SEM0120' as const
/** Stable code for a concrete specialization whose conditional requirements cannot be proved. */
export const unprovenConformanceCode = 'SEM0121' as const
/** Stable code for a complete application that reaches the instance frontier with open rows or evidence. */
export const nonConcreteSpecializationCode = 'SEM0122' as const
/** Stable code for a provider that matches no member of its concrete source requirement row. */
export const providerNoMatchCode = 'SEM0123' as const
/** Stable code for provider relations sharing a selector but retaining disjoint candidate sets. */
export const jointProviderSelectionConflictCode = 'SEM0124' as const
/** Stable code for provider selection that retains more than one common requirement member. */
export const providerAmbiguityCode = 'SEM0125' as const
/** Stable code for an explicitly or independently selected row that is not exactly one member. */
export const selectedRowCardinalityCode = 'SEM0126' as const
/** Stable code for a surviving provider candidate with more than one conformance witness. */
export const providerConformanceAmbiguityCode = 'SEM0127' as const
/** Stable code for a surviving provider candidate whose conformance mapping is invalid. */
export const invalidProviderConformanceCode = 'SEM0128' as const
/** Stable code for an explicit return whose value violates the declaration result. */
export const returnTypeMismatchCode = 'SEM0129' as const
/** Stable code for a reachable non-unit function fallthrough. */
export const missingReturnCode = 'SEM0130' as const
/** Stable code for a provider whose key matches but whose access cannot satisfy the requirement. */
export const providerAccessMismatchCode = 'SEM0131' as const
/** Stable code for a `typeof` item that resolves to no declaration in scope. */
export const unresolvedExactRepresentationItemCode = 'SEM0108' as const
/** Stable code for a `typeof` item whose name belongs to more than one declaration. */
export const ambiguousExactRepresentationItemCode = 'SEM0109' as const
/** Stable code for a `typeof` item that names something other than an ordinary callable. */
export const uncallableExactRepresentationItemCode = 'SEM0110' as const
/** Stable code for a `typeof` item whose generic parameters are not all supplied. */
export const openExactRepresentationItemCode = 'SEM0111' as const
/** Stable code for a public contract exposing the exact identity of a private item. */
export const privateExactRepresentationLeakCode = 'SEM0112' as const
/** Stable code for one opaque producer specialization yielding multiple exact identities. */
export const divergentOpaqueRealizationCode = 'SEM0113' as const
/** Stable code for an opaque family whose representation evidence contains no local construction. */
export const opaqueRealizationCycleCode = 'SEM0114' as const
/** Stable code for an opaque realization whose inline captures contain that same family. */
export const inlineOpaqueLayoutCycleCode = 'SEM0115' as const
/** Stable code for an opaque result binder whose bound is not callable or Effect representation. */
export const invalidOpaqueResultBinderCode = 'SEM0116' as const
/** Stable code for an opaque producer whose reachable returns establish no representation. */
export const missingOpaqueRealizationCode = 'SEM0117' as const
/** Stable code for an opaque result declared where no producer body can establish its identity. */
export const bodylessOpaqueResultCode = 'SEM0118' as const

/** Stable code for effect-block return sites whose success types disagree. */
export const effectBlockReturnMismatchCode = 'SEM0163' as const
/** Stable code for a nominal union declaration with no variants. */
export const emptyNominalUnionCode = 'SEM0164' as const
/** Stable code for a repeated variant name within one nominal union. */
export const duplicateUnionVariantCode = 'SEM0165' as const
/** Stable code for a named-field variant whose braces contain no field. */
export const emptyUnionVariantCode = 'SEM0166' as const
/** Stable code for a variant selector absent from its resolved nominal union. */
export const unknownUnionVariantCode = 'SEM0167' as const
/** Stable code for a variant qualifier that does not name a nominal union. */
export const expectedNominalUnionCode = 'SEM0168' as const
/** Stable code for construction through an incomplete nominal union declaration. */
export const invalidNominalUnionConstructionCode = 'SEM0169' as const

/** Stable code for a duration literal whose exact nanosecond total exceeds `u64`. */
export const durationOutOfRangeCode = 'SEM0170' as const
/** Stable code for postfix referent projection whose subject is not a reference. */
export const invalidReferentProjectionCode = 'SEM0171' as const

/** Stable code for positional construction with the wrong number of tuple elements. */
export const tupleArityMismatchCode = 'SEM0172' as const

/** Stable code for using tuple syntax with a named struct or record syntax with a tuple. */
export const contextualAggregateKindMismatchCode = 'SEM0173' as const

/** Stable code for attempting to join distinct anonymous aggregate occurrences. */
export const anonymousAggregateJoinMismatchCode = 'SEM0174' as const

/** Stable code for attempting named-field construction of a positional aggregate. */
export const positionalFieldConstructionCode = 'SEM0175' as const

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
export const conflictingViewLoanCode = 'OWN0010' as const
export const ownerAccessDuringLoanCode = 'OWN0011' as const
export const borrowedMoveCode = 'OWN0012' as const

/** Stable code for invoking a stored callable through too weak an aggregate receiver access. */
export const storedCallableInvocationAccessCode = 'OWN0014' as const

/** Stable code for running a stored Effect through too weak an aggregate receiver access. */
export const storedEffectRunAccessCode = 'OWN0015' as const
/** Stable code for an access-scoped local-shared borrow escaping or crossing suspension. */
export const localSharedAccessEscapeCode = 'OWN0016' as const
/** Stable code for an owner consumed in only some arms of a branch merge. */
export const incompatibleArmMergeCode = 'OWN0017' as const

/** Stable code for an exact `usize` magnitude outside the selected target word. */
export const usizeTargetOutOfRangeCode = 'LAY0001' as const

/** Every stable diagnostic code any phase can produce. */
export type Code =
  | typeof unsupportedBytesCode
  | typeof unknownLiteralModifierCode
  | typeof unterminatedStaticLiteralCode
  | typeof missingBaseDigitsCode
  | typeof invalidDigitSeparatorCode
  | typeof missingExponentDigitsCode
  | typeof characterLiteralScalarCountCode
  | typeof invalidDurationAmountCode
  | typeof unknownDurationUnitCode
  | typeof repeatedDurationUnitCode
  | typeof outOfOrderDurationUnitCode
  | typeof subordinateDurationOutOfRangeCode
  | typeof missingTokenCode
  | typeof unexpectedTokensCode
  | typeof reservedTemplateSyntaxCode
  | typeof reservedImportBindingCode
  | typeof expressionNestingLimitExceededCode
  | typeof unknownModuleCode
  | typeof selfImportCode
  | typeof reservedModuleIdentityCode
  | typeof unknownTypeCode
  | typeof integerOutOfRangeCode
  | typeof duplicateDeclarationNameCode
  | typeof unknownFunctionCode
  | typeof duplicateParameterNameCode
  | typeof unknownValueReferenceCode
  | typeof wrongCallArityCode
  | typeof rebindingNameCode
  | typeof unknownActorCode
  | typeof unknownActorOperationCode
  | typeof conditionNotBoolCode
  | typeof argumentTypeMismatchCode
  | typeof unknownImportedMemberCode
  | typeof inaccessibleImportedMemberCode
  | typeof bindingConflictCode
  | typeof duplicateFieldNameCode
  | typeof expectedTypeCode
  | typeof privateTypeExposureCode
  | typeof inlineRecursiveAggregateCode
  | typeof inaccessibleStructConstructionCode
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
  | typeof indexNotUsizeCode
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
  | typeof borrowedViewTypePositionCode
  | typeof invalidBorrowPositionCode
  | typeof invalidBorrowOperandCode
  | typeof exclusiveBorrowRequiresMutableCode
  | typeof invalidSliceReborrowCode
  | typeof implicitSliceDecayCode
  | typeof usizeNegativeCode
  | typeof invalidFailureTypeCode
  | typeof failureChannelOnOrdinaryCode
  | typeof failOutsideEffectCode
  | typeof undeclaredFailureCode
  | typeof runNonEffectCode
  | typeof unhandledEffectFailuresCode
  | typeof invalidEffectHandlerCode
  | typeof mutableEffectRecipeCode
  | typeof invalidRequirementTypeCode
  | typeof unhandledEffectRequirementsCode
  | typeof providerBackedFailureCode
  | typeof invalidEffectProvisionCode
  | typeof nonCallableApplicationCode
  | typeof incompatibleCallableSignatureCode
  | typeof invalidCallableInvocationAccessCode
  | typeof redundantUnaryEmptyCallCode
  | typeof callableIdentityErasureCode
  | typeof unknownOwnedCallableReturnCode
  | typeof nonFiniteEffectJoinCode
  | typeof refutableLetPatternCode
  | typeof invalidOperatorContractCode
  | typeof operatorNotApplicableCode
  | typeof ambiguousOperatorCode
  | typeof misplacedUnsafeAcknowledgementCode
  | typeof localSharedLayoutMismatchCode
  | typeof unsatisfiedExecutablePropertyCode
  | typeof missingExplicitExecutionOwnerCode
  | typeof invalidExecutablePropertyConjunctCode
  | typeof executionLayoutMismatchCode
  | typeof invalidMutableParameterCode
  | typeof unknownCallableBorrowSourceCode
  | typeof deferredCallableMutationCode
  | typeof emptyEnumCode
  | typeof unsupportedEnumRepresentationCode
  | typeof duplicateEnumMemberNameCode
  | typeof duplicateEnumDiscriminantCode
  | typeof enumDiscriminantOutOfRangeCode
  | typeof enumImplicitDiscriminantOverflowCode
  | typeof unsignedEnumNegativeDiscriminantCode
  | typeof unknownEnumMemberCode
  | typeof wrongEnumMemberCode
  | typeof enumIntegerMismatchCode
  | typeof crossEnumEqualityCode
  | typeof enumOrderingCode
  | typeof incompleteEnumMatchCode
  | typeof duplicateEnumMatchArmCode
  | typeof enumMatchArmAfterWildcardCode
  | typeof foreignEnumPatternCode
  | typeof integerPatternAgainstEnumCode
  | typeof missingUnsafeBoundaryCode
  | typeof invalidConformanceCode
  | typeof invalidDropHookCode
  | typeof invalidStaticLiteralCode
  | typeof invalidConstantCode
  | typeof expressionStatementResultCode
  | typeof genericParameterKindMismatchCode
  | typeof contractRowInferenceCode
  | typeof invalidServiceDeclarationCode
  | typeof invalidReturnedBorrowSignatureCode
  | typeof invalidReturnedBorrowOriginCode
  | typeof intrinsicTargetUnavailableCode
  | typeof invalidFloatLiteralCode
  | typeof ambiguousBoundOperationCode
  | typeof uninferredTypeParameterCode
  | typeof typeArgumentConflictCode
  | typeof unlowerableBoundWitnessCode
  | typeof storedCallableConstructionCode
  | typeof conflictingInitializerRepresentationCode
  | typeof divergentRepresentationJoinCode
  | typeof incompatibleRepresentationBoundCode
  | typeof storedRepresentedEffectConstructionCode
  | typeof overlappingConformanceCode
  | typeof nonTerminatingConformanceCode
  | typeof unprovenConformanceCode
  | typeof nonConcreteSpecializationCode
  | typeof providerNoMatchCode
  | typeof jointProviderSelectionConflictCode
  | typeof providerAmbiguityCode
  | typeof selectedRowCardinalityCode
  | typeof providerConformanceAmbiguityCode
  | typeof invalidProviderConformanceCode
  | typeof returnTypeMismatchCode
  | typeof missingReturnCode
  | typeof providerAccessMismatchCode
  | typeof unresolvedExactRepresentationItemCode
  | typeof ambiguousExactRepresentationItemCode
  | typeof uncallableExactRepresentationItemCode
  | typeof openExactRepresentationItemCode
  | typeof privateExactRepresentationLeakCode
  | typeof divergentOpaqueRealizationCode
  | typeof opaqueRealizationCycleCode
  | typeof inlineOpaqueLayoutCycleCode
  | typeof invalidOpaqueResultBinderCode
  | typeof missingOpaqueRealizationCode
  | typeof bodylessOpaqueResultCode
  | typeof effectBlockReturnMismatchCode
  | typeof emptyNominalUnionCode
  | typeof duplicateUnionVariantCode
  | typeof emptyUnionVariantCode
  | typeof unknownUnionVariantCode
  | typeof expectedNominalUnionCode
  | typeof invalidNominalUnionConstructionCode
  | typeof durationOutOfRangeCode
  | typeof invalidReferentProjectionCode
  | typeof tupleArityMismatchCode
  | typeof contextualAggregateKindMismatchCode
  | typeof anonymousAggregateJoinMismatchCode
  | typeof positionalFieldConstructionCode
  | typeof useAfterMoveCode
  | typeof partialMoveCode
  | typeof explicitMoveRequiredCode
  | typeof overlappingAssignmentCode
  | typeof incompatibleLoopHeaderCode
  | typeof matchBorrowEscapeCode
  | typeof exclusiveMatchRequiresMutableCode
  | typeof guardConsumesPatternCode
  | typeof invalidMatchScrutineePlaceCode
  | typeof conflictingViewLoanCode
  | typeof ownerAccessDuringLoanCode
  | typeof borrowedMoveCode
  | typeof storedCallableInvocationAccessCode
  | typeof storedEffectRunAccessCode
  | typeof localSharedAccessEscapeCode
  | typeof incompatibleArmMergeCode
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

export type ParserContext = 'syntax' | 'statement' | 'expression' | 'parameter' | 'delimiter'

/** Structured per-code data explaining why the originating phase diagnosed. */
export type Reason =
  | { readonly _tag: 'UnsupportedBytes' }
  | { readonly _tag: 'UnknownLiteralModifier'; readonly modifier: string }
  | {
      readonly _tag: 'UnterminatedStaticLiteral'
      readonly modifier: string
      readonly delimiter: '"' | "'"
      readonly delimiterWidth: 1 | 3
    }
  | { readonly _tag: 'MissingBaseDigits'; readonly radix: 2 | 8 | 16 }
  | { readonly _tag: 'InvalidDigitSeparator' }
  | { readonly _tag: 'MissingExponentDigits' }
  | { readonly _tag: 'CharacterLiteralScalarCount'; readonly scalars: number }
  | { readonly _tag: 'InvalidDurationAmount' }
  | { readonly _tag: 'UnknownDurationUnit'; readonly spelling: string }
  | { readonly _tag: 'RepeatedDurationUnit'; readonly unit: string }
  | {
      readonly _tag: 'OutOfOrderDurationUnit'
      readonly unit: string
      readonly previous: string
    }
  | {
      readonly _tag: 'SubordinateDurationOutOfRange'
      readonly unit: string
      readonly amount: string
      readonly maximum: string
    }
  | { readonly _tag: 'MissingToken'; readonly expected: Token.TokenKind }
  | {
      readonly _tag: 'UnexpectedTokens'
      readonly unexpected: ReadonlyArray<Token.TokenKind>
      readonly context: ParserContext
      readonly expected: ReadonlyArray<string>
    }
  | { readonly _tag: 'ReservedTemplateSyntax' }
  | { readonly _tag: 'ReservedImportBinding'; readonly spelling: string }
  | {
      readonly _tag: 'ExpressionNestingLimitExceeded'
      readonly limit: number
      readonly attemptedDepth: number
    }
  | { readonly _tag: 'EmptyNominalUnion'; readonly union: string }
  | {
      readonly _tag: 'DuplicateUnionVariant'
      readonly spelling: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'EmptyUnionVariant'; readonly variant: string }
  | { readonly _tag: 'UnknownUnionVariant'; readonly union: string; readonly variant: string }
  | { readonly _tag: 'ExpectedNominalUnion'; readonly actual: string }
  | { readonly _tag: 'InvalidNominalUnionConstruction'; readonly union: string }
  | {
      readonly _tag: 'DurationOutOfRange'
      readonly spelling: string
      readonly maximum: '18446744073709551615'
    }
  | {
      readonly _tag: 'TupleArityMismatch'
      readonly type: string
      readonly expected: number
      readonly actual: number
    }
  | {
      readonly _tag: 'ContextualAggregateKindMismatch'
      readonly expected: 'record' | 'tuple'
      readonly actual: string
    }
  | {
      readonly _tag: 'AnonymousAggregateJoinMismatch'
      readonly types: ReadonlyArray<string>
    }
  | { readonly _tag: 'PositionalFieldConstruction'; readonly type: string }
  | { readonly _tag: 'UnknownModule'; readonly module: string }
  | { readonly _tag: 'SelfImport'; readonly module: string }
  | { readonly _tag: 'ReservedModuleIdentity'; readonly module: string }
  | { readonly _tag: 'UnknownType'; readonly spelling: string }
  | {
      readonly _tag: 'IntegerOutOfRange'
      readonly spelling: string
      readonly maximum: 2147483647
      readonly minimum: -2147483648
    }
  | { readonly _tag: 'UsizeNegative'; readonly spelling: string }
  | { readonly _tag: 'InvalidFailureType'; readonly type: string }
  | { readonly _tag: 'FailureChannelOnOrdinary' }
  | { readonly _tag: 'FailOutsideEffect' }
  | { readonly _tag: 'UndeclaredFailure'; readonly type: string }
  | { readonly _tag: 'RunNonEffect'; readonly type: string }
  | { readonly _tag: 'UnhandledEffectFailures'; readonly failures: ReadonlyArray<string> }
  | { readonly _tag: 'InvalidEffectHandler'; readonly detail: string }
  | { readonly _tag: 'MutableEffectRecipe' }
  | { readonly _tag: 'NonFiniteEffectJoin'; readonly detail: string }
  | { readonly _tag: 'CallableIdentityErasure' }
  | { readonly _tag: 'UnknownOwnedCallableReturn' }
  | { readonly _tag: 'MissingUnsafeBoundary'; readonly operation: string }
  | { readonly _tag: 'MisplacedUnsafeAcknowledgement' }
  | {
      readonly _tag: 'LocalSharedLayoutMismatch'
      readonly expected: string
      readonly actual: string
    }
  | {
      readonly _tag: 'ExecutionLayoutMismatch'
      readonly expected: string
      readonly actual: string
    }
  | {
      readonly _tag: 'UnsatisfiedExecutableProperty'
      readonly property: 'Intrinsic.Detached' | 'Intrinsic.NonParking'
      readonly causes: ReadonlyArray<string>
    }
  | { readonly _tag: 'MissingExplicitExecutionOwner'; readonly summary: string }
  | { readonly _tag: 'InvalidExecutablePropertyConjunct'; readonly conjunct: string }
  | { readonly _tag: 'InvalidConformance'; readonly detail: string }
  | { readonly _tag: 'InvalidOperatorContract'; readonly detail: string }
  | {
      readonly _tag: 'OperatorNotApplicable'
      readonly operator: string
      readonly operands: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'AmbiguousOperator'
      readonly operator: string
      readonly candidates: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'AmbiguousBoundOperation'
      readonly spelling: string
      readonly parameters: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'UnlowerableBoundWitness'
      readonly spelling: string
      readonly provider: string
    }
  | { readonly _tag: 'InvalidServiceDeclaration'; readonly detail: string }
  | { readonly _tag: 'InvalidReturnedBorrowSignature' }
  | { readonly _tag: 'InvalidReturnedBorrowOrigin' }
  | {
      readonly _tag: 'InvalidMutableParameter'
      readonly context: 'BorrowedView' | 'Contract'
    }
  | { readonly _tag: 'UnknownCallableBorrowSource' }
  | { readonly _tag: 'DeferredCallableMutation'; readonly spelling: string }
  | {
      readonly _tag: 'IntrinsicTargetUnavailable'
      readonly operation: string
      readonly target: 'Evaluator' | 'LLVM' | 'Wasm'
    }
  | { readonly _tag: 'InvalidDropHook'; readonly detail: string }
  | { readonly _tag: 'InvalidStaticLiteral'; readonly detail: string }
  | { readonly _tag: 'InvalidFloatLiteral'; readonly spelling: string }
  | {
      readonly _tag: 'StoredCallableConstruction'
      readonly aggregate: string
      readonly field?: string
      readonly callable: string
    }
  | {
      readonly _tag: 'ConflictingInitializerRepresentation'
      readonly parameter: string
      readonly expected: string
      readonly actual: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'DivergentRepresentationJoin'
      readonly expected: string
      readonly actual: string
      readonly originSpans: readonly [SourceSpan.SourceSpan, SourceSpan.SourceSpan]
    }
  | {
      readonly _tag: 'IncompatibleRepresentationBound'
      readonly parameter: string
      readonly required: string
      readonly actual: string
      readonly requiredDeclarationSpan?: SourceSpan.SourceSpan
      readonly actualDeclarationSpan?: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'StoredRepresentedEffectConstruction'
      readonly aggregate: string
      readonly field?: string
      readonly effect: string
    }
  | {
      readonly _tag: 'OverlappingConformance'
      readonly head: string
      readonly other: string
    }
  | {
      readonly _tag: 'NonTerminatingConformance'
      readonly head: string
      readonly failures: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'UnprovenConformance'
      readonly goal: string
      readonly detail: string
      readonly trace: ReadonlyArray<string>
    }
  | { readonly _tag: 'NonConcreteSpecialization'; readonly declaration: string }
  | {
      readonly _tag: 'ProviderSelection'
      readonly problem: ProviderSelection.SelectionProblem
    }
  | { readonly _tag: 'UnresolvedExactRepresentationItem'; readonly item: string }
  | {
      readonly _tag: 'AmbiguousExactRepresentationItem'
      readonly item: string
      readonly count: number
    }
  | {
      readonly _tag: 'UncallableExactRepresentationItem'
      readonly item: string
      readonly subject: UncallableExactRepresentationSubject
    }
  | {
      readonly _tag: 'OpenExactRepresentationItem'
      readonly item: string
      readonly expected: number
      readonly actual: number
    }
  | { readonly _tag: 'PrivateExactRepresentationLeak'; readonly item: string }
  | {
      readonly _tag: 'DivergentOpaqueRealization'
      readonly family: string
      readonly realizations: ReadonlyArray<string>
    }
  | { readonly _tag: 'OpaqueRealizationCycle'; readonly families: ReadonlyArray<string> }
  | { readonly _tag: 'InlineOpaqueLayoutCycle'; readonly families: ReadonlyArray<string> }
  | {
      readonly _tag: 'InvalidOpaqueResultBinder'
      readonly binder: string
      readonly actual: 'Value' | 'RequirementRow'
    }
  | { readonly _tag: 'MissingOpaqueRealization'; readonly family: string }
  | {
      readonly _tag: 'BodylessOpaqueResult'
      readonly declaration: string
      readonly context: 'ServiceOperation' | 'InterfaceOperation'
    }
  | { readonly _tag: 'InvalidConstant'; readonly detail: string }
  | { readonly _tag: 'ExpressionStatementResult'; readonly actual: string }
  | { readonly _tag: 'InvalidRequirementType'; readonly type: string }
  | { readonly _tag: 'UnhandledEffectRequirements'; readonly requirements: ReadonlyArray<string> }
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
  | { readonly _tag: 'UnknownValueReference'; readonly spelling: string }
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
  | {
      readonly _tag: 'ReturnTypeMismatch'
      readonly expected: string
      readonly actual: string
    }
  | { readonly _tag: 'MissingReturn'; readonly expected: string }
  | { readonly _tag: 'NonCallableApplication'; readonly actual: string }
  | {
      readonly _tag: 'IncompatibleCallableSignature'
      readonly expected: string
      readonly actual: string
    }
  | {
      readonly _tag: 'InvalidCallableInvocationAccess'
      readonly required: 'Shared' | 'Exclusive' | 'Take'
    }
  | { readonly _tag: 'RedundantUnaryEmptyCall'; readonly target: string }
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
  | { readonly _tag: 'EmptyEnum'; readonly enum: string }
  | {
      readonly _tag: 'UnsupportedEnumRepresentation'
      readonly spelling: string
      readonly allowed: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'DuplicateEnumMemberName'
      readonly spelling: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'DuplicateEnumDiscriminant'
      readonly value: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EnumDiscriminantOutOfRange'
      readonly representation: string
      readonly value: string
      readonly minimum: string
      readonly maximum: string
    }
  | {
      readonly _tag: 'EnumImplicitDiscriminantOverflow'
      readonly representation: string
      readonly predecessor: string
      readonly maximum: string
    }
  | {
      readonly _tag: 'UnsignedEnumNegativeDiscriminant'
      readonly representation: string
      readonly value: string
    }
  | { readonly _tag: 'UnknownEnumMember'; readonly enum: string; readonly member: string }
  | { readonly _tag: 'WrongEnumMember'; readonly expected: string; readonly actual: string }
  | {
      readonly _tag: 'EnumIntegerMismatch'
      readonly enum: string
      readonly integer: string
      readonly direction: 'IntegerToEnum' | 'EnumToInteger'
    }
  | { readonly _tag: 'CrossEnumEquality'; readonly left: string; readonly right: string }
  | { readonly _tag: 'EnumOrdering'; readonly enum: string; readonly operator: string }
  | {
      readonly _tag: 'IncompleteEnumMatch'
      readonly enum: string
      readonly missing: ReadonlyArray<string>
    }
  | {
      readonly _tag: 'DuplicateEnumMatchArm'
      readonly member: string
      readonly originalSpan: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EnumMatchArmAfterWildcard'
      readonly wildcardSpan: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'ForeignEnumPattern'; readonly expected: string; readonly actual: string }
  | { readonly _tag: 'IntegerPatternAgainstEnum'; readonly enum: string; readonly value: string }
  | { readonly _tag: 'ExpectedType'; readonly spelling: string }
  | { readonly _tag: 'PrivateTypeExposure'; readonly type: string }
  | { readonly _tag: 'InlineRecursiveAggregate'; readonly members: ReadonlyArray<string> }
  | { readonly _tag: 'InaccessibleStructConstruction'; readonly type: string }
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
  | { readonly _tag: 'InvalidReferentProjection'; readonly actual: string }
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
  | { readonly _tag: 'IndexNotUsize'; readonly actual: string }
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
  | {
      readonly _tag: 'RefutableLetPattern'
      readonly actual: string
      readonly missing: ReadonlyArray<string>
    }
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
  | { readonly _tag: 'EffectBlockReturnMismatch'; readonly types: ReadonlyArray<string> }
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
      readonly _tag: 'UninferredTypeParameter'
      readonly target: string
      readonly parameter: string
    }
  | {
      readonly _tag: 'TypeArgumentConflict'
      readonly target: string
      readonly parameter: string
      readonly written: string
      readonly implied: string
    }
  | {
      readonly _tag: 'PolymorphicRecursion'
      readonly caller: string
      readonly target: string
    }
  | {
      readonly _tag: 'GenericParameterKindMismatch'
      readonly spelling: string
      readonly expected:
        | 'Value'
        | 'RequirementRow'
        | 'CallableRepresentation'
        | 'EffectRepresentation'
      readonly actual:
        | 'Value'
        | 'RequirementRow'
        | 'CallableRepresentation'
        | 'EffectRepresentation'
    }
  | {
      readonly _tag: 'ContractRowInference'
      readonly problem:
        | { readonly _tag: 'AbsentFailureMember'; readonly member: string }
        | {
            readonly _tag: 'AbsentRequirementMember'
            readonly capability: string
            readonly role: string
            readonly access: 'Shared' | 'Exclusive'
          }
        | {
            readonly _tag: 'IncompatibleRequirementRole'
            readonly capability: string
            readonly expected: string
            readonly actual: ReadonlyArray<string>
          }
        | {
            readonly _tag: 'IncompatibleRequirementAccess'
            readonly capability: string
            readonly role: string
            readonly expected: 'Shared' | 'Exclusive'
            readonly actual: ReadonlyArray<'Shared' | 'Exclusive'>
          }
        | {
            readonly _tag: 'AmbiguousRequirementRemainder'
            readonly parameters: ReadonlyArray<string>
          }
        | { readonly _tag: 'NonFiniteRequirementRow' }
    }
  | {
      readonly _tag: 'BorrowedViewTypePosition'
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
  | {
      readonly _tag: 'StoredCallableInvocationAccess'
      readonly aggregate: string
      readonly field: string
      readonly contract: string
      readonly receiver: 'Shared' | 'Exclusive' | 'Take'
      readonly required: 'Shared' | 'Exclusive' | 'Take'
    }
  | {
      readonly _tag: 'StoredEffectRunAccess'
      readonly aggregate: string
      readonly field: string
      readonly contract: string
      readonly receiver: 'Shared' | 'Exclusive' | 'Take'
      readonly required: 'Shared' | 'Exclusive' | 'Take'
    }
  | { readonly _tag: 'ExplicitMoveRequired'; readonly spelling: string }
  | { readonly _tag: 'OverlappingAssignment'; readonly spelling: string }
  | { readonly _tag: 'IncompatibleLoopHeader'; readonly loop: number }
  | { readonly _tag: 'IncompatibleArmMerge'; readonly spelling: string }
  | { readonly _tag: 'MatchBorrowEscape'; readonly spelling: string }
  | { readonly _tag: 'ExclusiveMatchRequiresMutable'; readonly spelling: string }
  | { readonly _tag: 'GuardConsumesPattern'; readonly spelling: string }
  | { readonly _tag: 'InvalidMatchScrutineePlace'; readonly access: 'Move' | 'Exclusive' }
  | {
      readonly _tag: 'ConflictingViewLoan'
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
  | {
      readonly _tag: 'LocalSharedAccessEscape'
      readonly kind: 'Callback' | 'Result' | 'Suspension'
    }

/** One additional source span labeled with its relationship to the diagnostic. */
export interface RelatedSpan {
  readonly label: string
  readonly span: SourceSpan.SourceSpan
}

/**
 * One unambiguous machine-applicable replacement: the corrected bytes for one source range.
 *
 * A phase emits an edit only where the correction needs no name choice and no type decision from
 * the author; every other diagnostic carries none. Applying every edit of one diagnostic to its
 * source removes that diagnostic. All edits of one diagnostic address the source that owns the
 * diagnostic's primary span, and no two of them overlap, so a consumer may apply them in any
 * order. Every edit is derived from byte offsets alone, so repeated compilations of the same
 * source produce byte-identical edits.
 */
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
      diagnostic.code === uninferredTypeParameterCode ||
      diagnostic.code === typeArgumentConflictCode ||
      diagnostic.code === genericParameterKindMismatchCode ||
      diagnostic.code === incompatibleRepresentationBoundCode ||
      diagnostic.code === contractRowInferenceCode ||
      diagnostic.code === invalidEffectProvisionCode ||
      diagnostic.code === providerNoMatchCode ||
      diagnostic.code === jointProviderSelectionConflictCode ||
      diagnostic.code === providerAmbiguityCode ||
      diagnostic.code === selectedRowCardinalityCode ||
      diagnostic.code === providerConformanceAmbiguityCode ||
      diagnostic.code === invalidProviderConformanceCode ||
      diagnostic.code === nonConcreteSpecializationCode ||
      diagnostic.code === polymorphicRecursionCode,
  )

/**
 * Tests whether a reachable-instance fence diagnostic denies target realization.
 *
 * A stored-callable construction (#184) is exactly a program the layout planner cannot serve: the
 * violating aggregates would receive unavailable layout entries and MIR validation would fail with
 * `MissingTypeLayout`/`InvalidAggregateOperation`. Realization stops here so the source diagnostic
 * is the only reported failure, instead of being followed by an `InvalidMir` echo of itself.
 */
export const hasInstanceFenceErrors = (diagnostics: ReadonlyArray<Diagnostic>): boolean =>
  diagnostics.some(
    (diagnostic) =>
      diagnostic.code === storedCallableConstructionCode ||
      diagnostic.code === storedRepresentedEffectConstructionCode,
  )

/** Tests whether source return-contract errors must stop every target-dependent phase. */
export const hasReturnContractErrors = (diagnostics: ReadonlyArray<Diagnostic>): boolean =>
  diagnostics.some(
    (diagnostic) =>
      diagnostic.code === returnTypeMismatchCode || diagnostic.code === missingReturnCode,
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

const compareStrings = (left: string, right: string): number => {
  if (left < right) {
    return -1
  }
  if (left > right) {
    return 1
  }
  return 0
}

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

/** Creates the lexical diagnostic for one reserved but unrecognized literal modifier. */
export const unknownLiteralModifier = (modifier: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: unknownLiteralModifierCode,
    severity: 'error',
    message: `Unknown static-literal modifier: ${modifier}`,
    reason: Object.freeze({ _tag: 'UnknownLiteralModifier', modifier }),
    span,
  })

/** Creates the lexical diagnostic for one deterministic unterminated-literal recovery range. */
export const unterminatedStaticLiteral = (
  modifier: string,
  delimiterWidth: 1 | 3,
  span: SourceSpan.SourceSpan,
  delimiter: '"' | "'" = '"',
): Diagnostic => {
  let subject: string
  if (delimiter === "'") {
    subject = 'character'
  } else {
    subject = `${delimiterWidth === 3 ? 'multiline ' : ''}static`
  }
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: unterminatedStaticLiteralCode,
    severity: 'error',
    message: `Unterminated ${subject} literal`,
    reason: Object.freeze({
      _tag: 'UnterminatedStaticLiteral',
      modifier,
      delimiter,
      delimiterWidth,
    }),
    span,
  })
}

/**
 * Creates the lexical diagnostic for one character literal that does not denote one scalar.
 *
 * The rule counts Unicode scalars rather than bytes, so a multi-byte scalar such as `'é'` is one
 * character and never a length error.
 */
export const characterLiteralScalarCount = (
  scalars: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: characterLiteralScalarCountCode,
    severity: 'error',
    message: `Character literal must hold exactly one Unicode scalar, but holds ${scalars}`,
    reason: Object.freeze({ _tag: 'CharacterLiteralScalarCount', scalars }),
    span,
  })

/** Creates the lexical diagnostic for a duration amount that is not a whole decimal integer. */
export const invalidDurationAmount = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: invalidDurationAmountCode,
    severity: 'error',
    message: 'Duration components require whole decimal amounts',
    reason: Object.freeze({ _tag: 'InvalidDurationAmount' }),
    span,
  })

/** Creates the lexical diagnostic for one unknown duration unit suffix. */
export const unknownDurationUnit = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: unknownDurationUnitCode,
    severity: 'error',
    message: `Unknown duration unit ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownDurationUnit', spelling }),
    span,
  })

/** Creates the lexical diagnostic for one duration unit repeated in a compact literal. */
export const repeatedDurationUnit = (unit: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: repeatedDurationUnitCode,
    severity: 'error',
    message: `Duration unit ${unit} may appear only once`,
    reason: Object.freeze({ _tag: 'RepeatedDurationUnit', unit }),
    span,
  })

/** Creates the lexical diagnostic for a duration unit written after a smaller unit. */
export const outOfOrderDurationUnit = (
  unit: string,
  previous: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: outOfOrderDurationUnitCode,
    severity: 'error',
    message: `Duration unit ${unit} must not follow ${previous}`,
    reason: Object.freeze({ _tag: 'OutOfOrderDurationUnit', unit, previous }),
    span,
  })

/** Creates the lexical diagnostic for a non-leading duration component outside its field bound. */
export const subordinateDurationOutOfRange = (
  unit: string,
  amount: bigint,
  maximum: bigint,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: subordinateDurationOutOfRangeCode,
    severity: 'error',
    message: `Subordinate ${unit} component ${amount} exceeds ${maximum}`,
    reason: Object.freeze({
      _tag: 'SubordinateDurationOutOfRange',
      unit,
      amount: amount.toString(),
      maximum: maximum.toString(),
    }),
    span,
  })

/** Creates the lexical diagnostic for one base prefix that no digit of its base follows. */
export const missingBaseDigits = (radix: 2 | 8 | 16, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: missingBaseDigitsCode,
    severity: 'error',
    message: `Base-${radix} integer literal without digits`,
    reason: Object.freeze({ _tag: 'MissingBaseDigits', radix }),
    span,
  })

/** Creates the lexical diagnostic for one number literal whose `_` is not between two digits. */
export const invalidDigitSeparator = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: invalidDigitSeparatorCode,
    severity: 'error',
    message: 'Digit separator must sit between two digits',
    reason: Object.freeze({ _tag: 'InvalidDigitSeparator' }),
    span,
  })

/** Creates the lexical diagnostic for one exponent marker that no exponent digit follows. */
export const missingExponentDigits = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'lexical',
    code: missingExponentDigitsCode,
    severity: 'error',
    message: 'Float literal exponent must have at least one digit',
    reason: Object.freeze({ _tag: 'MissingExponentDigits' }),
    span,
  })

/** Creates the semantic diagnostic for a static literal that cannot decode atomically. */
export const invalidStaticLiteral = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidStaticLiteralCode,
    severity: 'error',
    message: `Invalid static literal: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidStaticLiteral', detail }),
    span,
  })

/** Creates the semantic diagnostic for a float spelling no floating-point value can represent. */
export const invalidFloatLiteral = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidFloatLiteralCode,
    severity: 'error',
    message: `Invalid float literal: ${spelling}`,
    reason: Object.freeze({ _tag: 'InvalidFloatLiteral', spelling }),
    span,
  })

/** Creates the semantic diagnostic for a constant outside the literal scalar contract. */
export const invalidConstant = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidConstantCode,
    severity: 'error',
    message: `Invalid constant: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidConstant', detail }),
    span,
  })

/** Creates the declaration diagnostic for an invalid interface operator marker. */
export const invalidOperatorContract = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidOperatorContractCode,
    severity: 'error',
    message: `Invalid operator contract: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidOperatorContract', detail }),
    span,
  })

/** Creates the operator-site diagnostic when no marked operation accepts the operands. */
export const operatorNotApplicable = (
  operator: string,
  operands: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: operatorNotApplicableCode,
    severity: 'error',
    message: `Operator ${operator} does not accept (${operands.join(', ')})`,
    reason: Object.freeze({ _tag: 'OperatorNotApplicable', operator, operands }),
    span,
  })

/** Creates the operator-site diagnostic when static conformance leaves multiple candidates. */
export const ambiguousOperator = (
  operator: string,
  candidates: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: ambiguousOperatorCode,
    severity: 'error',
    message: `Operator ${operator} is ambiguous between ${candidates.join(', ')}`,
    reason: Object.freeze({ _tag: 'AmbiguousOperator', operator, candidates }),
    span,
  })

/** Creates the semantic diagnostic for an unused non-unit expression-statement result. */
export const expressionStatementResult = (
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: expressionStatementResultCode,
    severity: 'error',
    message: `Expression statement produces ${actual}, but only () or never may be ignored`,
    reason: Object.freeze({ _tag: 'ExpressionStatementResult', actual }),
    span,
    notes: Object.freeze([
      'Bind the value with `let`, return it, or consume it explicitly with `drop`.',
    ]),
  })

/** Creates the diagnostic associated with one missing token leaf. */
export const missingToken = (expected: Token.TokenKind, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'parser',
    code: missingTokenCode,
    severity: 'error',
    message: `Expected ${Token.describe(expected)}`,
    reason: Object.freeze({ _tag: 'MissingToken', expected }),
    span,
  })

const unexpectedTokensMessage = (
  encountered: string,
  context: ParserContext,
  expectation: string | undefined,
): string => {
  if (context === 'syntax') {
    if (expectation === undefined) return `Unexpected ${encountered}; expected valid syntax`
    return `Unexpected ${encountered}; expected ${expectation}`
  }
  if (context === 'statement') return `Unexpected ${encountered} while parsing a statement`
  return `Unexpected ${encountered} while parsing a ${context}`
}

/** Creates the diagnostic associated with one unexpected-token error node. */
export const unexpectedTokens = (
  unexpected: ReadonlyArray<Token.TokenKind>,
  context: ParserContext,
  expected: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic => {
  const firstUnexpected = unexpected[0]
  const encountered =
    firstUnexpected === undefined || firstUnexpected === 'Invalid'
      ? 'invalid token'
      : Token.describe(firstUnexpected)
  const expectations = Object.freeze([...expected])
  const expectation = expectations[0]
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'parser',
    code: unexpectedTokensCode,
    severity: 'error',
    message: unexpectedTokensMessage(encountered, context, expectation),
    reason: Object.freeze({
      _tag: 'UnexpectedTokens',
      unexpected: Object.freeze([...unexpected]),
      context,
      expected: expectations,
    }),
    span,
    ...(context === 'syntax' || expectations.length === 0
      ? {}
      : { notes: Object.freeze([`Expected one of: ${expectations.join(', ')}`]) }),
  })
}

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

/** Creates the parser diagnostic for the first token of an over-budget child expression. */
export const expressionNestingLimitExceeded = (
  limit: number,
  attemptedDepth: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'parser',
    code: expressionNestingLimitExceededCode,
    severity: 'error',
    message: `Expression nesting exceeds the supported limit of ${limit}`,
    reason: Object.freeze({ _tag: 'ExpressionNestingLimitExceeded', limit, attemptedDepth }),
    span,
  })

/** Creates the diagnostic for a reserved final import segment without a usable binding form. */
export const reservedImportBinding = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'parser',
    code: reservedImportBindingCode,
    severity: 'error',
    message: `Reserved module segment ${spelling} requires an explicit alias or selected-member list`,
    reason: Object.freeze({ _tag: 'ReservedImportBinding', spelling }),
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

export const reservedModuleIdentity = (module: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'module',
    code: reservedModuleIdentityCode,
    severity: 'error',
    message: `Module ${module} claims the reserved standard-library namespace silk/; user modules must live outside it`,
    reason: Object.freeze({ _tag: 'ReservedModuleIdentity', module }),
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

/** Creates the diagnostic for a scalar enum with no members. */
export const emptyEnum = (enumName: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: emptyEnumCode,
    severity: 'error',
    message: `Enum ${enumName} must declare at least one member`,
    reason: Object.freeze({ _tag: 'EmptyEnum', enum: enumName }),
    span,
  })

/** Creates the diagnostic for a nominal union with no variants. */
export const emptyNominalUnion = (unionName: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: emptyNominalUnionCode,
    severity: 'error',
    message: `Union ${unionName} must declare at least one variant`,
    reason: Object.freeze({ _tag: 'EmptyNominalUnion', union: unionName }),
    span,
  })

/** Creates the diagnostic for a repeated variant name within one nominal union. */
export const duplicateUnionVariant = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateUnionVariantCode,
    severity: 'error',
    message: `Duplicate union variant ${spelling}`,
    reason: Object.freeze({ _tag: 'DuplicateUnionVariant', spelling, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

/** Creates the diagnostic for braces used without any named variant field. */
export const emptyUnionVariant = (variantName: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: emptyUnionVariantCode,
    severity: 'error',
    message: `Union variant ${variantName} must omit braces or declare at least one field`,
    reason: Object.freeze({ _tag: 'EmptyUnionVariant', variant: variantName }),
    span,
  })

/** Creates the diagnostic for selecting a missing variant from a resolved nominal union. */
export const unknownUnionVariant = (
  unionName: string,
  variantName: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownUnionVariantCode,
    severity: 'error',
    message: `Union ${unionName} has no variant ${variantName}`,
    reason: Object.freeze({
      _tag: 'UnknownUnionVariant',
      union: unionName,
      variant: variantName,
    }),
    span,
  })

/** Creates the diagnostic for a variant qualifier that is not a nominal union. */
export const expectedNominalUnion = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: expectedNominalUnionCode,
    severity: 'error',
    message: `Expected a nominal union, found ${actual}`,
    reason: Object.freeze({ _tag: 'ExpectedNominalUnion', actual }),
    span,
  })

/** Creates the construction fence for a nominal union with invalid declaration facts. */
export const invalidNominalUnionConstruction = (
  unionName: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidNominalUnionConstructionCode,
    severity: 'error',
    message: `Cannot construct invalid nominal union ${unionName}`,
    reason: Object.freeze({ _tag: 'InvalidNominalUnionConstruction', union: unionName }),
    span,
  })

export const unsupportedEnumRepresentation = (
  spelling: string,
  allowed: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unsupportedEnumRepresentationCode,
    severity: 'error',
    message: `${spelling} is not a scalar enum representation`,
    reason: Object.freeze({
      _tag: 'UnsupportedEnumRepresentation',
      spelling,
      allowed: Object.freeze([...allowed]),
    }),
    span,
  })

export const duplicateEnumMemberName = (
  spelling: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateEnumMemberNameCode,
    severity: 'error',
    message: `Duplicate enum member name ${spelling}`,
    reason: Object.freeze({ _tag: 'DuplicateEnumMemberName', spelling, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

export const duplicateEnumDiscriminant = (
  value: bigint,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateEnumDiscriminantCode,
    severity: 'error',
    message: `Duplicate enum discriminant ${value}`,
    reason: Object.freeze({
      _tag: 'DuplicateEnumDiscriminant',
      value: value.toString(),
      originalSpan,
    }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first declared here', span: originalSpan }),
    ]),
  })

export const enumDiscriminantOutOfRange = (
  representation: string,
  value: bigint,
  minimum: bigint,
  maximum: bigint,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: enumDiscriminantOutOfRangeCode,
    severity: 'error',
    message: `Enum discriminant ${value} is outside ${representation}`,
    reason: Object.freeze({
      _tag: 'EnumDiscriminantOutOfRange',
      representation,
      value: value.toString(),
      minimum: minimum.toString(),
      maximum: maximum.toString(),
    }),
    span,
  })

export const enumImplicitDiscriminantOverflow = (
  representation: string,
  predecessor: bigint,
  maximum: bigint,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: enumImplicitDiscriminantOverflowCode,
    severity: 'error',
    message: `Implicit enum discriminant after ${predecessor} exceeds ${representation}`,
    reason: Object.freeze({
      _tag: 'EnumImplicitDiscriminantOverflow',
      representation,
      predecessor: predecessor.toString(),
      maximum: maximum.toString(),
    }),
    span,
  })

export const unsignedEnumNegativeDiscriminant = (
  representation: string,
  value: bigint,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unsignedEnumNegativeDiscriminantCode,
    severity: 'error',
    message: `Unsigned enum representation ${representation} cannot hold ${value}`,
    reason: Object.freeze({
      _tag: 'UnsignedEnumNegativeDiscriminant',
      representation,
      value: value.toString(),
    }),
    span,
  })

export const unknownEnumMember = (
  enumName: string,
  member: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownEnumMemberCode,
    severity: 'error',
    message: `Enum ${enumName} has no member ${member}`,
    reason: Object.freeze({ _tag: 'UnknownEnumMember', enum: enumName, member }),
    span,
  })

export const wrongEnumMember = (
  expected: string,
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: wrongEnumMemberCode,
    severity: 'error',
    message: `Enum member of ${actual} cannot be used as ${expected}`,
    reason: Object.freeze({ _tag: 'WrongEnumMember', expected, actual }),
    span,
  })

export const enumIntegerMismatch = (
  enumName: string,
  integer: string,
  direction: 'IntegerToEnum' | 'EnumToInteger',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: enumIntegerMismatchCode,
    severity: 'error',
    message:
      direction === 'IntegerToEnum'
        ? `${integer} does not implicitly construct ${enumName}`
        : `${enumName} does not implicitly convert to ${integer}`,
    reason: Object.freeze({ _tag: 'EnumIntegerMismatch', enum: enumName, integer, direction }),
    span,
  })

export const crossEnumEquality = (
  left: string,
  right: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: crossEnumEqualityCode,
    severity: 'error',
    message: `Equality requires one enum type, not ${left} and ${right}`,
    reason: Object.freeze({ _tag: 'CrossEnumEquality', left, right }),
    span,
  })

export const enumOrdering = (
  enumName: string,
  operator: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: enumOrderingCode,
    severity: 'error',
    message: `Enum ${enumName} does not support ${operator}; compare backing values explicitly`,
    reason: Object.freeze({ _tag: 'EnumOrdering', enum: enumName, operator }),
    span,
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

/** Creates the one canonical diagnostic for an inline recursive nominal-aggregate component. */
export const inlineRecursiveAggregate = (
  members: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: inlineRecursiveAggregateCode,
    severity: 'error',
    message: `Inline recursive aggregate layout: ${members.join(' -> ')}`,
    reason: Object.freeze({
      _tag: 'InlineRecursiveAggregate',
      members: Object.freeze([...members]),
    }),
    span,
  })

export const inaccessibleStructConstruction = (
  type: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: inaccessibleStructConstructionCode,
    severity: 'error',
    message: `Cannot construct ${type} because its raw constructor is not available at this site`,
    reason: Object.freeze({ _tag: 'InaccessibleStructConstruction', type }),
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

export const conflictingInitializerRepresentation = (
  parameter: string,
  expected: string,
  actual: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: conflictingInitializerRepresentationCode,
    severity: 'error',
    message: `Representation ${parameter} was inferred as ${expected}, but this initializer uses ${actual}`,
    reason: Object.freeze({
      _tag: 'ConflictingInitializerRepresentation',
      parameter,
      expected,
      actual,
      originalSpan,
    }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'representation first inferred here', span: originalSpan }),
    ]),
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

export const invalidReferentProjection = (
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidReferentProjectionCode,
    severity: 'error',
    message: `Cannot project a referent from ${actual}; the subject must be a reference`,
    reason: Object.freeze({ _tag: 'InvalidReferentProjection', actual }),
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

export const indexNotUsize = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: indexNotUsizeCode,
    severity: 'error',
    message: `Array index must be usize, found ${actual}`,
    reason: Object.freeze({ _tag: 'IndexNotUsize', actual }),
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

/** Creates the diagnostic for one decimal literal outside the signed `i32` range. */
export const integerOutOfRange = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: integerOutOfRangeCode,
    severity: 'error',
    message: 'Integer literal exceeds the i32 range',
    reason: Object.freeze({
      _tag: 'IntegerOutOfRange',
      spelling,
      maximum: 2147483647,
      minimum: -2147483648,
    }),
    span,
  })

/** Creates the semantic diagnostic for a duration total outside the fixed `u64` domain. */
export const durationOutOfRange = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: durationOutOfRangeCode,
    severity: 'error',
    message: 'Duration literal exceeds the u64 nanosecond range',
    reason: Object.freeze({
      _tag: 'DurationOutOfRange',
      spelling,
      maximum: '18446744073709551615',
    }),
    span,
  })

export const tupleArityMismatch = (
  type: string,
  expected: number,
  actual: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: tupleArityMismatchCode,
    severity: 'error',
    message: `${type} expects ${expected} tuple elements but received ${actual}`,
    reason: Object.freeze({ _tag: 'TupleArityMismatch', type, expected, actual }),
    span,
  })

export const contextualAggregateKindMismatch = (
  expected: 'record' | 'tuple',
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: contextualAggregateKindMismatchCode,
    severity: 'error',
    message: `A contextual ${expected} literal cannot construct ${actual}`,
    reason: Object.freeze({ _tag: 'ContextualAggregateKindMismatch', expected, actual }),
    span,
  })

export const anonymousAggregateJoinMismatch = (
  types: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: anonymousAggregateJoinMismatchCode,
    severity: 'error',
    message: 'Separate anonymous aggregate occurrences do not acquire a common type',
    reason: Object.freeze({ _tag: 'AnonymousAggregateJoinMismatch', types }),
    span,
  })

export const positionalFieldConstruction = (
  type: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: positionalFieldConstructionCode,
    severity: 'error',
    message: `${type} is positional and cannot be constructed with named fields`,
    reason: Object.freeze({ _tag: 'PositionalFieldConstruction', type }),
    span,
  })

/** Creates the target-independent diagnostic for a negative `usize` literal. */
export const usizeNegative = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: usizeNegativeCode,
    severity: 'error',
    message: 'usize literals cannot be negative',
    reason: Object.freeze({ _tag: 'UsizeNegative', spelling }),
    span,
  })

/** Creates the diagnostic for a type that cannot inhabit an Effect failure channel. */
export const invalidFailureType = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidFailureTypeCode,
    severity: 'error',
    message: `Effect failure ${type} must be a detached ordinary value type`,
    reason: Object.freeze({ _tag: 'InvalidFailureType', type }),
    span,
  })

/** Creates the diagnostic for a requirement that cannot name one dependency-eligible service. */
export const invalidRequirementType = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidRequirementTypeCode,
    severity: 'error',
    message: `Effect requirement ${type} must be one concrete service type`,
    reason: Object.freeze({ _tag: 'InvalidRequirementType', type }),
    span,
  })

/** Creates the diagnostic for spelling a failure channel on a direct ordinary function. */
export const failureChannelOnOrdinary = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: failureChannelOnOrdinaryCode,
    severity: 'error',
    message: 'Only effect functions may declare a failure channel',
    reason: Object.freeze({ _tag: 'FailureChannelOnOrdinary' }),
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

/**
 * Rejects a reachable construction that would store a bare callable value inside an aggregate.
 *
 * A direct callable value works because the compiler still holds its hidden concrete identity; an
 * aggregate type such as `Parser` carries only the declared signature, so layout planning cannot
 * size the callable's environment (#184). Until nominal values can carry that identity — or stored
 * callables get a uniform runtime representation — the construction is reported here, at the source
 * site, instead of surfacing later as an `InvalidMir` failure with no user-facing diagnostic.
 *
 * When the aggregate stores a callable only because a generic specialization chose one — the
 * declared field type is a bare type parameter — the primary span is the specializing call site,
 * because that is where the concrete callable argument was written, and the generic body's
 * construction is retained as `constructedAt` related provenance.
 */
export const storedCallableConstruction = (
  aggregate: string,
  field: string | undefined,
  callable: string,
  span: SourceSpan.SourceSpan,
  constructedAt?: SourceSpan.SourceSpan,
  represented = false,
): Diagnostic => {
  const site = field === undefined ? 'its element' : `field ${field}`
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: storedCallableConstructionCode,
    severity: 'error',
    message: represented
      ? `Cannot construct ${aggregate}: ${site} retains the static identity of ${callable}, but represented callable storage has no supported runtime layout`
      : `Cannot construct ${aggregate}: ${site} would store the callable ${callable}, whose environment layout depends on a hidden concrete identity that ${aggregate} does not carry`,
    reason: Object.freeze({
      _tag: 'StoredCallableConstruction',
      aggregate,
      ...(field === undefined ? {} : { field }),
      callable,
    }),
    span,
    ...(constructedAt === undefined
      ? {}
      : {
          relatedSpans: Object.freeze([
            Object.freeze({ label: 'constructed here', span: constructedAt }),
          ]),
        }),
  })
}

/** What one rejected `typeof` item names instead of an ordinary callable declaration. */
export type UncallableExactRepresentationSubject =
  | 'EffectDeclaration'
  | 'LocalBinding'
  | 'CallableSection'
  | 'NonCallableDeclaration'

const uncallableSubjectProse = (subject: UncallableExactRepresentationSubject): string => {
  switch (subject) {
    case 'EffectDeclaration':
      return 'an Effect declaration rather than an ordinary callable'
    case 'LocalBinding':
      return 'a local binding, which exists only where it is written'
    case 'CallableSection':
      return 'a callable section, whose identity belongs to its construction site'
    case 'NonCallableDeclaration':
      return 'a declaration that is not callable'
  }
}

const opaqueResultNote =
  'Return an opaque representation result instead when the concrete identity must stay private.'

/** Rejects one `typeof` item that names no declaration in the enclosing scope. */
export const unresolvedExactRepresentationItem = (
  item: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unresolvedExactRepresentationItemCode,
    severity: 'error',
    message: `Cannot name the exact representation of ${item}: no declaration of that name is in scope`,
    reason: Object.freeze({ _tag: 'UnresolvedExactRepresentationItem', item }),
    span,
    notes: Object.freeze([opaqueResultNote]),
  })

/** Rejects one `typeof` item whose name belongs to more than one declaration. */
export const ambiguousExactRepresentationItem = (
  item: string,
  count: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: ambiguousExactRepresentationItemCode,
    severity: 'error',
    message: `Cannot name the exact representation of ${item}: ${count} declarations carry that name, so no single item is resolved`,
    reason: Object.freeze({ _tag: 'AmbiguousExactRepresentationItem', item, count }),
    span,
    notes: Object.freeze([opaqueResultNote]),
  })

/**
 * Rejects one `typeof` item that names something other than an ordinary callable declaration.
 *
 * Local bindings, callable sections, and Effect construction sites are values created where they
 * are written. They have no declaration-owned identity a contract can name, so their
 * representation can only cross a boundary behind an opaque result.
 */
export const uncallableExactRepresentationItem = (
  item: string,
  subjectKind: UncallableExactRepresentationSubject,
  span: SourceSpan.SourceSpan,
): Diagnostic => {
  const subject = uncallableSubjectProse(subjectKind)
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: uncallableExactRepresentationItemCode,
    severity: 'error',
    message: `Cannot name the exact representation of ${item}: it names ${subject}, which has no source-nameable exact identity`,
    reason: Object.freeze({
      _tag: 'UncallableExactRepresentationItem',
      item,
      subject: subjectKind,
    }),
    span,
    notes: Object.freeze([opaqueResultNote]),
  })
}

/** Rejects one `typeof` item whose generic parameters are not all supplied. */
export const openExactRepresentationItem = (
  item: string,
  expected: number,
  actual: number,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: openExactRepresentationItemCode,
    severity: 'error',
    message: `Cannot name the exact representation of ${item}: an exact representation names one construction, but ${expected} generic parameters were declared and ${actual} concrete arguments were supplied`,
    reason: Object.freeze({ _tag: 'OpenExactRepresentationItem', item, expected, actual }),
    span,
    notes: Object.freeze([opaqueResultNote]),
  })

/** Rejects a public contract that exposes the exact identity of a less visible item. */
export const privateExactRepresentationLeak = (
  item: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: privateExactRepresentationLeakCode,
    severity: 'error',
    message: `Public contract exposes the exact representation of private ${item}`,
    reason: Object.freeze({ _tag: 'PrivateExactRepresentationLeak', item }),
    span,
    notes: Object.freeze([opaqueResultNote]),
  })

/** Rejects one opaque family whose reachable returns select more than one exact realization. */
export const divergentOpaqueRealization = (
  family: string,
  realizations: ReadonlyArray<string>,
  related: ReadonlyArray<SourceSpan.SourceSpan>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: divergentOpaqueRealizationCode,
    severity: 'error',
    message: `Opaque result ${family} has divergent reachable realizations: ${realizations.join(', ')}`,
    reason: Object.freeze({
      _tag: 'DivergentOpaqueRealization',
      family,
      realizations: Object.freeze([...realizations]),
    }),
    span,
    ...(related.length === 0
      ? {}
      : {
          relatedSpans: Object.freeze(
            related.map((candidate) =>
              Object.freeze({ label: 'conflicting realization returned here', span: candidate }),
            ),
          ),
        }),
  })

/** Rejects opaque families whose only representation evidence is another unresolved family. */
export const opaqueRealizationCycle = (
  families: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: opaqueRealizationCycleCode,
    severity: 'error',
    message: `Opaque realization cycle has no local concrete construction: ${families.join(' -> ')}`,
    reason: Object.freeze({
      _tag: 'OpaqueRealizationCycle',
      families: Object.freeze([...families]),
    }),
    span,
  })

/** Rejects a capture layout that would contain the opaque family it is defining inline. */
export const inlineOpaqueLayoutCycle = (
  families: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: inlineOpaqueLayoutCycleCode,
    severity: 'error',
    message: `Opaque results form an infinite inline layout cycle: ${families.join(' -> ')}`,
    reason: Object.freeze({
      _tag: 'InlineOpaqueLayoutCycle',
      families: Object.freeze([...families]),
    }),
    span,
  })

/** Rejects an opaque result binder whose bound is not a callable or Effect representation. */
export const invalidOpaqueResultBinder = (
  binder: string,
  actual: 'Value' | 'RequirementRow',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidOpaqueResultBinderCode,
    severity: 'error',
    message: `Opaque result binder ${binder} must have a callable or Effect representation bound, but its kind is ${actual}`,
    reason: Object.freeze({ _tag: 'InvalidOpaqueResultBinder', binder, actual }),
    span,
  })

/** Rejects an opaque producer whose reachable returns select no representation construction. */
export const missingOpaqueRealization = (family: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: missingOpaqueRealizationCode,
    severity: 'error',
    message: `Opaque result ${family} has no reachable callable or Effect representation construction`,
    reason: Object.freeze({ _tag: 'MissingOpaqueRealization', family }),
    span,
  })

/** Rejects an opaque result in a contract-only declaration that has no producer body. */
export const bodylessOpaqueResult = (
  declaration: string,
  contextKind: 'ServiceOperation' | 'InterfaceOperation',
  span: SourceSpan.SourceSpan,
): Diagnostic => {
  const context = contextKind === 'ServiceOperation' ? 'service' : 'interface'
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: bodylessOpaqueResultCode,
    severity: 'error',
    message: `Opaque result ${declaration} is not permitted on a ${context} operation because no producer body can establish one static representation`,
    reason: Object.freeze({
      _tag: 'BodylessOpaqueResult',
      declaration,
      context: contextKind,
    }),
    span,
  })
}

/** Rejects represented Effect storage until a downstream runtime layout has been proven. */
export const storedRepresentedEffectConstruction = (
  aggregate: string,
  field: string | undefined,
  effect: string,
  span: SourceSpan.SourceSpan,
  constructedAt?: SourceSpan.SourceSpan,
): Diagnostic => {
  const site = field === undefined ? 'its element' : `field ${field}`
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: storedRepresentedEffectConstructionCode,
    severity: 'error',
    message: `Cannot construct ${aggregate}: ${site} retains the static identity of ${effect}, but represented Effect storage has no supported runtime layout`,
    reason: Object.freeze({
      _tag: 'StoredRepresentedEffectConstruction',
      aggregate,
      ...(field === undefined ? {} : { field }),
      effect,
    }),
    span,
    ...(constructedAt === undefined
      ? {}
      : {
          relatedSpans: Object.freeze([
            Object.freeze({ label: 'constructed here', span: constructedAt }),
          ]),
        }),
  })
}

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

export const nonFiniteEffectJoin = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: nonFiniteEffectJoinCode,
    severity: 'error',
    message: `Cannot form a finite Effect join: ${detail}`,
    reason: Object.freeze({ _tag: 'NonFiniteEffectJoin', detail }),
    span,
  })

export const callableIdentityErasure = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: callableIdentityErasureCode,
    severity: 'error',
    message:
      'Cannot merge callable values from different construction sites without explicit erasure',
    reason: Object.freeze({ _tag: 'CallableIdentityErasure' }),
    span,
  })

export const unknownOwnedCallableReturn = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownOwnedCallableReturnCode,
    severity: 'error',
    message: 'Cannot return an owned callable whose concrete environment identity is unknown',
    reason: Object.freeze({ _tag: 'UnknownOwnedCallableReturn' }),
    span,
  })

/** Creates the target-owned diagnostic for a `usize` literal outside its selected word. */
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
    message: `usize literal ${spelling} exceeds the ${bits}-bit range for ${target}`,
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

/** Creates the diagnostic for one present value name with no matching local declaration. */
export const unknownValueReference = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownValueReferenceCode,
    severity: 'error',
    message: `Unknown value ${spelling}`,
    reason: Object.freeze({ _tag: 'UnknownValueReference', spelling }),
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

/** Creates the diagnostic for a conditional whose condition is not `bool`. */
export const conditionNotBool = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: conditionNotBoolCode,
    severity: 'error',
    message: `Condition must be bool, found ${actual}`,
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

/** Creates the diagnostic for an explicit return that violates its declaration result. */
export const returnTypeMismatch = (
  expected: string,
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: returnTypeMismatchCode,
    severity: 'error',
    message: `Return expected ${expected} but received ${actual}`,
    reason: Object.freeze({ _tag: 'ReturnTypeMismatch', expected, actual }),
    span,
  })

/** Creates the diagnostic for a reachable closing brace in a non-unit body. */
export const missingReturn = (expected: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: missingReturnCode,
    severity: 'error',
    message: `A reachable path must return ${expected}`,
    reason: Object.freeze({ _tag: 'MissingReturn', expected }),
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
    message: `Structural union members must be detached ordinary values with finite storage, found ${type}`,
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

export const incompleteEnumMatch = (
  enum_: string,
  missing: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: incompleteEnumMatchCode,
    severity: 'error',
    message: `Match over ${enum_} does not cover ${missing.join(', ')}`,
    reason: Object.freeze({
      _tag: 'IncompleteEnumMatch',
      enum: enum_,
      missing: Object.freeze([...missing]),
    }),
    span,
  })

export const duplicateEnumMatchArm = (
  member: string,
  originalSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: duplicateEnumMatchArmCode,
    severity: 'error',
    message: `Duplicate enum match arm ${member}`,
    reason: Object.freeze({ _tag: 'DuplicateEnumMatchArm', member, originalSpan }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first covering arm', span: originalSpan }),
    ]),
  })

export const enumMatchArmAfterWildcard = (
  wildcardSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: enumMatchArmAfterWildcardCode,
    severity: 'error',
    message: 'Enum match arm is unreachable after `_`',
    reason: Object.freeze({ _tag: 'EnumMatchArmAfterWildcard', wildcardSpan }),
    span,
    relatedSpans: Object.freeze([Object.freeze({ label: 'wildcard arm', span: wildcardSpan })]),
  })

export const foreignEnumPattern = (
  expected: string,
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: foreignEnumPatternCode,
    severity: 'error',
    message: `Enum pattern from ${actual} cannot match ${expected}`,
    reason: Object.freeze({ _tag: 'ForeignEnumPattern', expected, actual }),
    span,
  })

export const integerPatternAgainstEnum = (
  enum_: string,
  value: bigint,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: integerPatternAgainstEnumCode,
    severity: 'error',
    message: `Integer pattern ${value} cannot match enum ${enum_}`,
    reason: Object.freeze({
      _tag: 'IntegerPatternAgainstEnum',
      enum: enum_,
      value: value.toString(),
    }),
    span,
  })

export const refutableLetPattern = (
  actual: string,
  missing: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: refutableLetPatternCode,
    severity: 'error',
    message: `Let pattern is refutable for ${actual}; it does not cover ${missing.join(', ')}. Use if let or match`,
    reason: Object.freeze({
      _tag: 'RefutableLetPattern',
      actual,
      missing: Object.freeze([...missing]),
    }),
    span,
  })

export const matchGuardNotBool = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: matchGuardNotBoolCode,
    severity: 'error',
    message: `Match guard must be bool, found ${actual}`,
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

export const inaccessiblePatternFields = (type: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: missingPatternFieldCode,
    severity: 'error',
    message: `Pattern for ${type} must use .. to omit inaccessible fields`,
    reason: Object.freeze({ _tag: 'MissingPatternField', type, field: '<inaccessible>' }),
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

/** Creates the diagnostic for an effect-block return whose type disagrees with the block's. */
export const effectBlockReturnMismatch = (
  types: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: effectBlockReturnMismatchCode,
    severity: 'error',
    message: `Effect block return sites have incompatible types: ${types.join(', ')}`,
    reason: Object.freeze({
      _tag: 'EffectBlockReturnMismatch',
      types: Object.freeze([...types]),
    }),
    span,
  })

export const divergentRepresentationJoin = (
  expected: string,
  actual: string,
  originSpans: readonly [SourceSpan.SourceSpan, SourceSpan.SourceSpan],
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: divergentRepresentationJoinCode,
    severity: 'error',
    message: `Cannot join ${expected} with ${actual}; consume each represented value inside its branch before joining`,
    reason: Object.freeze({
      _tag: 'DivergentRepresentationJoin',
      expected,
      actual,
      originSpans: Object.freeze(originSpans),
    }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'first representation originates here', span: originSpans[0] }),
      Object.freeze({ label: 'divergent representation originates here', span: originSpans[1] }),
    ]),
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

export const genericParameterKindMismatch = (
  spelling: string,
  expected: 'Value' | 'RequirementRow' | 'CallableRepresentation' | 'EffectRepresentation',
  actual: 'Value' | 'RequirementRow' | 'CallableRepresentation' | 'EffectRepresentation',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: genericParameterKindMismatchCode,
    severity: 'error',
    message: `Generic parameter ${spelling} has kind ${actual}, expected ${expected}`,
    reason: Object.freeze({
      _tag: 'GenericParameterKindMismatch',
      spelling,
      expected,
      actual,
    }),
    span,
  })

export const incompatibleRepresentationBound = (
  parameter: string,
  required: string,
  actual: string,
  span: SourceSpan.SourceSpan,
  provenance: {
    readonly requiredDeclarationSpan?: SourceSpan.SourceSpan
    readonly actualDeclarationSpan?: SourceSpan.SourceSpan
  } = {},
): Diagnostic => {
  const relatedSpans: Array<RelatedSpan> = []
  if (provenance.requiredDeclarationSpan !== undefined)
    relatedSpans.push(
      Object.freeze({
        label: 'required representation bound declared here',
        span: provenance.requiredDeclarationSpan,
      }),
    )
  if (provenance.actualDeclarationSpan !== undefined)
    relatedSpans.push(
      Object.freeze({
        label: 'supplied representation bound declared here',
        span: provenance.actualDeclarationSpan,
      }),
    )
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: incompatibleRepresentationBoundCode,
    severity: 'error',
    message: `Representation ${parameter} requires ${required}, but the supplied bound ${actual} is not admissible`,
    reason: Object.freeze({
      _tag: 'IncompatibleRepresentationBound',
      parameter,
      required,
      actual,
      ...(provenance.requiredDeclarationSpan === undefined
        ? {}
        : { requiredDeclarationSpan: provenance.requiredDeclarationSpan }),
      ...(provenance.actualDeclarationSpan === undefined
        ? {}
        : { actualDeclarationSpan: provenance.actualDeclarationSpan }),
    }),
    span,
    ...(relatedSpans.length === 0 ? {} : { relatedSpans: Object.freeze(relatedSpans) }),
  })
}

type ContractRowInferenceProblem = Extract<
  Reason,
  { readonly _tag: 'ContractRowInference' }
>['problem']

const contractRowInferenceMessage = (problem: ContractRowInferenceProblem): string => {
  switch (problem._tag) {
    case 'AbsentFailureMember':
      return `Failure type does not contain selected member ${problem.member}`
    case 'AbsentRequirementMember':
      // Two complete templates rather than one with the access marker interpolated, so the
      // generated diagnostic catalog pins both wordings this problem can report.
      return problem.access === 'Exclusive'
        ? `Requirement row does not contain &mut ${problem.capability}@${problem.role}`
        : `Requirement row does not contain &${problem.capability}@${problem.role}`
    case 'IncompatibleRequirementRole':
      return `Requirement ${problem.capability} has role ${problem.actual.join(' or ')}, expected ${problem.expected}`
    case 'IncompatibleRequirementAccess':
      return `Requirement ${problem.capability}@${problem.role} has access ${problem.actual.join(' or ')}, expected ${problem.expected}`
    case 'AmbiguousRequirementRemainder':
      return `Requirement row remainder is ambiguous across ${problem.parameters.join(', ')}`
    case 'NonFiniteRequirementRow':
      return 'Requirement row specialization is not finite and concrete'
  }
}

export const contractRowInference = (
  problem: ContractRowInferenceProblem,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: contractRowInferenceCode,
    severity: 'error',
    message: contractRowInferenceMessage(problem),
    reason: Object.freeze({ _tag: 'ContractRowInference', problem: Object.freeze(problem) }),
    span,
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

export const uninferredTypeParameter = (
  target: string,
  parameter: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: uninferredTypeParameterCode,
    severity: 'error',
    message: `Cannot infer type argument ${parameter} of ${target} from supplied values`,
    reason: Object.freeze({ _tag: 'UninferredTypeParameter', target, parameter }),
    span,
  })

export const typeArgumentConflict = (
  target: string,
  parameter: string,
  written: string,
  implied: string,
  span: SourceSpan.SourceSpan,
  firstConstraint?: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: typeArgumentConflictCode,
    severity: 'error',
    message: `Type argument ${parameter} of ${target} is ${written}, but the supplied values imply ${implied}`,
    reason: Object.freeze({ _tag: 'TypeArgumentConflict', target, parameter, written, implied }),
    span,
    ...(firstConstraint === undefined
      ? {}
      : {
          relatedSpans: Object.freeze([
            Object.freeze({ label: 'type argument first constrained here', span: firstConstraint }),
          ]),
        }),
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

export const borrowedViewTypePosition = (
  position: 'parameter' | 'return' | 'field' | 'type argument',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: borrowedViewTypePositionCode,
    severity: 'error',
    message:
      position === 'parameter'
        ? 'A borrowed view must be the complete type of an ordinary function parameter'
        : `A borrowed view cannot appear in a ${position} type`,
    reason: Object.freeze({ _tag: 'BorrowedViewTypePosition', position }),
    span,
  })

export const invalidReturnedBorrowSignature = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidReturnedBorrowSignatureCode,
    severity: 'error',
    message:
      'A returned borrowed view must belong to an ordinary function with exactly one borrowed parameter; an exclusive result requires an exclusive parameter',
    reason: Object.freeze({ _tag: 'InvalidReturnedBorrowSignature' }),
    span,
  })

export const invalidReturnedBorrowOrigin = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidReturnedBorrowOriginCode,
    severity: 'error',
    message:
      "The returned borrowed view does not originate from the function's single borrowed parameter",
    reason: Object.freeze({ _tag: 'InvalidReturnedBorrowOrigin' }),
    span,
  })

/** Diagnoses a reachable sealed operation before its unsupported execution surface is entered. */
export const intrinsicTargetUnavailable = (
  operation: string,
  target: 'Evaluator' | 'LLVM' | 'Wasm',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: intrinsicTargetUnavailableCode,
    severity: 'error',
    message: `${operation} is unavailable for ${target}`,
    reason: Object.freeze({ _tag: 'IntrinsicTargetUnavailable', operation, target }),
    span,
  })

/** Diagnoses a known mismatch before MIR consumes either affine initializer argument. */
export const localSharedLayoutMismatch = (
  expected: string,
  actual: string,
  allocationSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: localSharedLayoutMismatchCode,
    severity: 'error',
    message: `Local-shared allocation was planned for ${actual}, not ${expected}`,
    reason: Object.freeze({ _tag: 'LocalSharedLayoutMismatch', expected, actual }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({
        label: 'allocation layout provenance originates here',
        span: allocationSpan,
      }),
    ]),
  })

/** Diagnoses a mismatched execution-package allocation before initializer publication. */
export const executionLayoutMismatch = (
  expected: string,
  actual: string,
  allocationSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: executionLayoutMismatchCode,
    severity: 'error',
    message: `Execution allocation was planned for ${actual}, not ${expected}`,
    reason: Object.freeze({ _tag: 'ExecutionLayoutMismatch', expected, actual }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({
        label: 'allocation layout provenance originates here',
        span: allocationSpan,
      }),
    ]),
  })

/** Diagnoses one failed sealed-property check at its concrete application obligation. */
export const unsatisfiedExecutableProperty = (
  property: 'Intrinsic.Detached' | 'Intrinsic.NonParking',
  causes: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unsatisfiedExecutablePropertyCode,
    severity: 'error',
    message: `${property} is unsatisfied: ${causes.join('; ')}`,
    reason: Object.freeze({
      _tag: 'UnsatisfiedExecutableProperty',
      property,
      causes: Object.freeze(Array.from(causes)),
    }),
    span,
  })

/** Diagnoses external parking whose complete entry has no explicit owner delimiter. */
export const missingExplicitExecutionOwner = (
  summary: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: missingExplicitExecutionOwnerCode,
    severity: 'error',
    message: 'External parking requires an explicit Intrinsic.Execution owner',
    reason: Object.freeze({ _tag: 'MissingExplicitExecutionOwner', summary }),
    span,
  })

/** Rejects ordinary interface/service bounds in the sealed exact-executable conjunction lane. */
export const invalidExecutablePropertyConjunct = (
  conjunct: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidExecutablePropertyConjunctCode,
    severity: 'error',
    message: `${conjunct} is not a sealed executable property`,
    reason: Object.freeze({ _tag: 'InvalidExecutablePropertyConjunct', conjunct }),
    span,
  })

export const invalidBorrowPosition = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidBorrowPositionCode,
    severity: 'error',
    message: 'A borrowed view is not valid in this expression position',
    reason: Object.freeze({ _tag: 'InvalidBorrowPosition' }),
    span,
  })

export const missingUnsafeBoundary = (operation: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: missingUnsafeBoundaryCode,
    severity: 'error',
    message: `${operation} requires unsafe acknowledgement`,
    reason: Object.freeze({ _tag: 'MissingUnsafeBoundary', operation }),
    span,
  })

export const misplacedUnsafeAcknowledgement = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: misplacedUnsafeAcknowledgementCode,
    severity: 'error',
    message: '`unsafe` must acknowledge a complete unsafe invocation',
    reason: Object.freeze({ _tag: 'MisplacedUnsafeAcknowledgement' }),
    span,
  })

export const invalidConformance = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidConformanceCode,
    severity: 'error',
    message: `Invalid conformance: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidConformance', detail }),
    span,
  })

/**
 * Creates the diagnostic for two conformance heads that may name one provider under one interface.
 *
 * Overlap is decided on shapes alone, so this fires even when the two declarations' requirements
 * look mutually exclusive: whether a requirement is satisfiable depends on the whole program and
 * changes as declarations are added, and a coherence answer that moved with the program would let
 * one specialization silently change which witness it selects.
 */
export const overlappingConformance = (
  head: string,
  other: string,
  span: SourceSpan.SourceSpan,
  originalSpan?: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: overlappingConformanceCode,
    severity: 'error',
    message: `${head} may overlap ${other}`,
    reason: Object.freeze({ _tag: 'OverlappingConformance', head, other }),
    span,
    ...(originalSpan === undefined
      ? {}
      : {
          relatedSpans: Object.freeze([
            Object.freeze({ label: 'overlapping implementation', span: originalSpan }),
          ]),
        }),
    notes: Object.freeze([
      'Conformance overlap is decided without consulting bounds, because whether a bound is satisfiable changes as a program grows.',
    ]),
  })

/**
 * Creates the diagnostic for a conformance requirement that does not descend toward a base witness.
 *
 * Each listed failure names one condition the header broke. Together the three conditions make the
 * provider term a well-founded measure, which is why proof search needs no fuel: a requirement that
 * satisfies them can only be followed finitely many times.
 */
export const nonTerminatingConformance = (
  head: string,
  failures: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: nonTerminatingConformanceCode,
    severity: 'error',
    message: `${head} declares a requirement that does not descend`,
    reason: Object.freeze({
      _tag: 'NonTerminatingConformance',
      head,
      failures: Object.freeze([...failures]),
    }),
    span,
    notes: Object.freeze([...failures]),
  })

/**
 * Creates the diagnostic for a specialization whose conditional requirements cannot be proved.
 *
 * The trace is the useful half: a missing base witness reported alone says only that some type
 * lacks a conformance, while the chain says which wrapper asked for it and through which
 * requirement, which is what tells the author where to declare the missing implementation.
 */
export const unprovenConformance = (
  goal: string,
  detail: string,
  trace: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unprovenConformanceCode,
    severity: 'error',
    message: `${goal} cannot be proved: ${detail}`,
    reason: Object.freeze({
      _tag: 'UnprovenConformance',
      goal,
      detail,
      trace: Object.freeze([...trace]),
    }),
    span,
    ...(trace.length === 0 ? {} : { notes: Object.freeze([...trace]) }),
  })

export const nonConcreteSpecialization = (
  declaration: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: nonConcreteSpecializationCode,
    severity: 'error',
    message: `${declaration} reaches a complete application with unresolved contract rows or evidence`,
    reason: Object.freeze({ _tag: 'NonConcreteSpecialization', declaration }),
    span,
  })

const providerSelectionFields = (
  problem: ProviderSelection.SelectionProblem,
  locations: ProviderSelection.DiagnosticLocations,
) => {
  const primarySpan = locations.primary
  const primaryKey = SourceSpan.key(primarySpan)
  const related = locations.relations
    .flatMap((relation) => relation.origins)
    .filter((origin) => SourceSpan.key(origin) !== primaryKey)
    .map((span) => Object.freeze({ label: 'contributing provider constraint', span }))
  return Object.freeze({
    reason: Object.freeze({ _tag: 'ProviderSelection', problem }),
    span: primarySpan,
    ...(related.length === 0 ? {} : { relatedSpans: Object.freeze(related) }),
  })
}

const providerNoMatch = (
  problem: Extract<ProviderSelection.SelectionProblem, { readonly _tag: 'ProviderNoMatch' }>,
  locations: ProviderSelection.DiagnosticLocations,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: providerNoMatchCode,
    severity: 'error',
    message: 'The provider matches no compatible requirement',
    ...providerSelectionFields(problem, locations),
  })

const providerAccessMismatch = (
  problem: Extract<ProviderSelection.SelectionProblem, { readonly _tag: 'ProviderAccessMismatch' }>,
  locations: ProviderSelection.DiagnosticLocations,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: providerAccessMismatchCode,
    severity: 'error',
    message: `${problem.provider.toLowerCase()} provider access cannot satisfy an ${problem.required.toLowerCase()} requirement`,
    ...providerSelectionFields(problem, locations),
  })

const jointProviderSelectionConflict = (
  problem: Extract<ProviderSelection.SelectionProblem, { readonly _tag: 'JointSelectionConflict' }>,
  locations: ProviderSelection.DiagnosticLocations,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: jointProviderSelectionConflictCode,
    severity: 'error',
    message: 'Provider constraints select incompatible requirement members',
    ...providerSelectionFields(problem, locations),
  })

const providerAmbiguity = (
  problem: Extract<ProviderSelection.SelectionProblem, { readonly _tag: 'ProviderAmbiguity' }>,
  locations: ProviderSelection.DiagnosticLocations,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: providerAmbiguityCode,
    severity: 'error',
    message: 'The provider matches more than one requirement; select one explicitly',
    ...providerSelectionFields(problem, locations),
  })

const selectedRowCardinality = (
  problem: Extract<ProviderSelection.SelectionProblem, { readonly _tag: 'SelectedRowCardinality' }>,
  locations: ProviderSelection.DiagnosticLocations,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: selectedRowCardinalityCode,
    severity: 'error',
    message: `Selected requirement row has ${problem.count} members; exactly one is required`,
    ...providerSelectionFields(problem, locations),
  })

const providerConformanceAmbiguity = (
  problem: Extract<ProviderSelection.SelectionProblem, { readonly _tag: 'ConformanceAmbiguity' }>,
  locations: ProviderSelection.DiagnosticLocations,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: providerConformanceAmbiguityCode,
    severity: 'error',
    message: 'More than one conformance witness can provide the selected requirement',
    ...providerSelectionFields(problem, locations),
  })

const invalidProviderConformance = (
  problem: Extract<ProviderSelection.SelectionProblem, { readonly _tag: 'InvalidConformance' }>,
  locations: ProviderSelection.DiagnosticLocations,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidProviderConformanceCode,
    severity: 'error',
    message: `The provider's conformance mapping is invalid: ${problem.reason}`,
    ...providerSelectionFields(problem, locations),
  })

/** Preserves the solver's span-free semantic payload separately from ordered source locations. */
export const providerSelection = (
  diagnostic: ProviderSelection.SelectionDiagnostic,
): Diagnostic => {
  const problem = diagnostic.problem
  switch (problem._tag) {
    case 'ProviderNoMatch':
      return providerNoMatch(problem, diagnostic.locations)
    case 'ProviderAccessMismatch':
      return providerAccessMismatch(problem, diagnostic.locations)
    case 'JointSelectionConflict':
      return jointProviderSelectionConflict(problem, diagnostic.locations)
    case 'ProviderAmbiguity':
      return providerAmbiguity(problem, diagnostic.locations)
    case 'SelectedRowCardinality':
      return selectedRowCardinality(problem, diagnostic.locations)
    case 'ConformanceAmbiguity':
      return providerConformanceAmbiguity(problem, diagnostic.locations)
    case 'InvalidConformance':
      return invalidProviderConformance(problem, diagnostic.locations)
  }
}

/**
 * Creates the diagnostic for a bound operation reachable through more than one bounded parameter.
 *
 * The receiver of a bound operation call is the bound's own name, so one declaration bounding two
 * of its parameters by the same interface leaves the call naming no single parameter. The operation
 * is real and the bound is satisfied; what is missing is which parameter's witness answers it.
 */
export const ambiguousBoundOperation = (
  spelling: string,
  parameters: ReadonlyArray<string>,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: ambiguousBoundOperationCode,
    severity: 'error',
    message: `${spelling} is ambiguous across bounded type parameters ${parameters.join(', ')}`,
    reason: Object.freeze({
      _tag: 'AmbiguousBoundOperation',
      spelling,
      parameters: Object.freeze([...parameters]),
    }),
    span,
  })

/**
 * Creates the diagnostic for a bound operation whose specialization selects a witness that has no
 * lowering.
 *
 * A witness answers with a sealed intrinsic or with a function of the provider's own actor, and a
 * bound operation call reaches both. A conformance that names neither leaves the call with nothing
 * to run: it would lower to nothing, and the specialized instance would fail MIR validation with no
 * user-visible cause. A call that passes analysis and produces no code is a reported error, because
 * the alternative is a silent miscompile.
 */
export const unlowerableBoundWitness = (
  spelling: string,
  provider: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unlowerableBoundWitnessCode,
    severity: 'error',
    message: `${spelling} has no witness that can be lowered for ${provider}`,
    reason: Object.freeze({ _tag: 'UnlowerableBoundWitness', spelling, provider }),
    span,
  })

export const invalidServiceDeclaration = (
  detail: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidServiceDeclarationCode,
    severity: 'error',
    message: `Invalid service declaration: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidServiceDeclaration', detail }),
    span,
  })

export const invalidMutableParameter = (
  context: 'BorrowedView' | 'Contract',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidMutableParameterCode,
    severity: 'error',
    message:
      context === 'Contract'
        ? '`mut` declares function-local owned parameter storage and is not valid in a service or interface contract'
        : '`mut` declares mutable owned parameter storage; use `&mut` for exclusive borrowed access',
    reason: Object.freeze({ _tag: 'InvalidMutableParameter', context }),
    span,
  })

/** Rejects a borrowed callable result when no unchanged exact function or section identifies it. */
export const unknownCallableBorrowSource = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: unknownCallableBorrowSourceCode,
    severity: 'error',
    message:
      'A callable returning a borrowed view requires one unchanged exact function or section identity',
    reason: Object.freeze({ _tag: 'UnknownCallableBorrowSource' }),
    span,
  })

/** Rejects mutation whose execution time cannot preserve the outer callable's exact recipe. */
export const deferredCallableMutation = (
  spelling: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: deferredCallableMutationCode,
    severity: 'error',
    message: `A deferred effect cannot mutate captured callable binding ${spelling}`,
    reason: Object.freeze({ _tag: 'DeferredCallableMutation', spelling }),
    span,
  })

export const invalidDropHook = (detail: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidDropHookCode,
    severity: 'error',
    message: `Invalid Drop hook: ${detail}`,
    reason: Object.freeze({ _tag: 'InvalidDropHook', detail }),
    span,
  })

export const invalidBorrowOperand = (span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidBorrowOperandCode,
    severity: 'error',
    message: 'A borrowed view requires a direct stable owner or borrowed view',
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

export const nonCallableApplication = (actual: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: nonCallableApplicationCode,
    severity: 'error',
    message: `Cannot call non-callable value ${actual}`,
    reason: Object.freeze({ _tag: 'NonCallableApplication', actual }),
    span,
  })

export const incompatibleCallableSignature = (
  expected: string,
  actual: string,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: incompatibleCallableSignatureCode,
    severity: 'error',
    message: `Callable ${actual} cannot satisfy ${expected}`,
    reason: Object.freeze({ _tag: 'IncompatibleCallableSignature', expected, actual }),
    span,
  })

export const invalidCallableInvocationAccess = (
  required: 'Shared' | 'Exclusive' | 'Take',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: invalidCallableInvocationAccessCode,
    severity: 'error',
    message: `Callable invocation requires ${required.toLowerCase()} access`,
    reason: Object.freeze({ _tag: 'InvalidCallableInvocationAccess', required }),
    span,
  })

export const redundantUnaryEmptyCall = (target: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'semantic',
    code: redundantUnaryEmptyCallCode,
    severity: 'error',
    message: `${target} is unary; name it directly instead of calling it with no arguments`,
    reason: Object.freeze({ _tag: 'RedundantUnaryEmptyCall', target }),
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

/**
 * A stored callable is reached through its enclosing aggregate, so the aggregate's own access bounds
 * the modes its environment admits: a shared receiver invokes only `fn`, an exclusive receiver also
 * invokes `mut fn`, and only a whole-owner receiver may consume a `once fn`.
 */
export const storedCallableInvocationAccess = (
  aggregate: string,
  field: string,
  contract: string,
  receiver: 'Shared' | 'Exclusive' | 'Take',
  required: 'Shared' | 'Exclusive' | 'Take',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: storedCallableInvocationAccessCode,
    severity: 'error',
    message: `Cannot invoke field ${field} of ${aggregate} through ${receiver.toLowerCase()} aggregate access: ${contract} requires ${required.toLowerCase()} access to the whole aggregate`,
    reason: Object.freeze({
      _tag: 'StoredCallableInvocationAccess',
      aggregate,
      field,
      contract,
      receiver,
      required,
    }),
    span,
  })

/**
 * A stored Effect is reached through its enclosing aggregate, so the aggregate's own access bounds
 * its run mode: a shared receiver runs only `Effect`, an exclusive receiver also runs `mut Effect`,
 * and only a whole-owner receiver may consume a `once Effect`.
 */
export const storedEffectRunAccess = (
  aggregate: string,
  field: string,
  contract: string,
  receiver: 'Shared' | 'Exclusive' | 'Take',
  required: 'Shared' | 'Exclusive' | 'Take',
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: storedEffectRunAccessCode,
    severity: 'error',
    message: `Cannot run field ${field} of ${aggregate} through ${receiver.toLowerCase()} aggregate access: ${contract} requires ${required.toLowerCase()} access to the whole aggregate`,
    reason: Object.freeze({
      _tag: 'StoredEffectRunAccess',
      aggregate,
      field,
      contract,
      receiver,
      required,
    }),
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

export const incompatibleArmMerge = (spelling: string, span: SourceSpan.SourceSpan): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: incompatibleArmMergeCode,
    severity: 'error',
    message: `Branches merge with incompatible owner liveness for ${spelling}`,
    reason: Object.freeze({ _tag: 'IncompatibleArmMerge', spelling }),
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

export const conflictingViewLoan = (
  existing: 'Shared' | 'Exclusive',
  requested: 'Shared' | 'Exclusive',
  loanSpan: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic =>
  Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: conflictingViewLoanCode,
    severity: 'error',
    message: `${requested} borrowed-view loan conflicts with an active ${existing.toLowerCase()} loan`,
    reason: Object.freeze({ _tag: 'ConflictingViewLoan', existing, requested, loanSpan }),
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
    message: `${access.toLowerCase()} access to ${spelling} conflicts with an active borrowed-view loan`,
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
    message: 'A non-Copy value cannot be moved out through a borrowed-view place',
    reason: Object.freeze({ _tag: 'BorrowedMove' }),
    span,
  })

const localSharedAccessEscapeMessage = (kind: 'Callback' | 'Result' | 'Suspension'): string => {
  if (kind === 'Suspension') {
    return 'Local-shared access cannot suspend while its exclusive borrow is live'
  }
  if (kind === 'Callback') {
    return 'Local-shared access cannot invoke an external readiness callback while its exclusive borrow is live'
  }
  return 'Local-shared access callback cannot return a value that retains its exclusive borrow'
}

/** Relates one access-scoped escape to the sealed boundary that created the exclusive loan. */
export const localSharedAccessEscape = (
  kind: 'Callback' | 'Result' | 'Suspension',
  span: SourceSpan.SourceSpan,
  boundary: SourceSpan.SourceSpan,
): Diagnostic => {
  return Object.freeze({
    _tag: 'Diagnostic',
    phase: 'ownership',
    code: localSharedAccessEscapeCode,
    severity: 'error',
    message: localSharedAccessEscapeMessage(kind),
    reason: Object.freeze({ _tag: 'LocalSharedAccessEscape', kind }),
    span,
    relatedSpans: Object.freeze([
      Object.freeze({ label: 'local-shared access boundary', span: boundary }),
    ]),
  })
}

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
