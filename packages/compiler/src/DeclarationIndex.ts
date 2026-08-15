import * as Option from 'effect/Option'
import * as ConformanceGoal from './ConformanceGoal.js'
import * as ConformanceHead from './ConformanceHead.js'
import * as Diagnostic from './Diagnostic.js'
import * as InterfaceWitnessCompatibility from './InterfaceWitnessCompatibility.js'
import * as InterfaceWitnessInference from './InterfaceWitnessInference.js'
import * as Intrinsic from './Intrinsic.js'
import * as DigitSeparator from './internal/DigitSeparator.js'
import * as IntegerLiteral from './internal/IntegerLiteral.js'
import * as LiteralForm from './LiteralForm.js'
import type * as ModuleClosure from './ModuleClosure.js'
import * as ResolutionSeams from './ResolutionSeams.js'
import * as SourceFile from './SourceFile.js'
import type * as SourceSpan from './SourceSpan.js'
import * as Specialization from './Specialization.js'
import * as StaticText from './StaticText.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as TargetConstant from './TargetConstant.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/** The semantic types recognized in declaration and executable analysis. */
export type SemanticType = Type.Type

/** A deterministic declaration identity local to one analyzed source snapshot. */
export interface DeclarationId {
  readonly _tag: 'DeclarationId'
  readonly sourceId: string
  readonly ordinal: number
}

/** A deterministic parameter identity nested under its owning function declaration. */
export interface ParameterId {
  readonly _tag: 'ParameterId'
  readonly function: DeclarationId
  readonly ordinal: number
}

/** A deterministic field identity nested under its owning struct declaration. */
export interface FieldId {
  readonly _tag: 'FieldId'
  readonly struct: DeclarationId
  readonly ordinal: number
}

/** The canonical identity of one declaration: canonical module identity plus name. */
export interface CanonicalId {
  readonly _tag: 'CanonicalDeclarationId'
  readonly module: string
  readonly name: string
}

/**
 * The interface one type parameter is bounded by. A bound starts as the bare spelling its syntax
 * retains and becomes `ResolvedBound` once header completion finds the interface that spelling
 * names in the bounded declaration's own module scope — which is what lets a bound name an
 * interface another module declares. A resolved bound carries the interface's complete ordered
 * operation contract, so every consumer reads one fact instead of re-deriving it from a name.
 */
export type BoundFact =
  | {
      readonly _tag: 'ResolvedBound'
      readonly spelling: string
      readonly path: TypePathFact
      readonly application: InterfaceApplicationFact
    }
  | {
      readonly _tag: 'UnresolvedBound'
      readonly spelling: string
      readonly path: TypePathFact
      readonly application: DeclaredTypeFact
    }

/** One ordered, declaration-owned generic type parameter with exact source provenance. */
export interface TypeParameterFact {
  readonly _tag: 'TypeParameterDeclaration'
  readonly type: Type.Parameter
  readonly name: DeclaredName
  readonly syntax: SyntaxTree.Node
  readonly duplicateOf?: Type.Parameter
  readonly bound?: BoundFact
  readonly representationBound?: {
    readonly _tag: 'RepresentationBound'
    readonly kind: 'Callable' | 'Effect'
    readonly contract: DeclaredTypeFact
    readonly syntax: SyntaxTree.Node
  }
}

/** The one declaration-owned representation binder introduced by an opaque result. */
export interface OpaqueResultFact {
  readonly _tag: 'OpaqueResult'
  readonly binder: TypeParameterFact
  readonly family: Type.OpaqueFamilyKey
  readonly publicSignature: {
    readonly bound: string
    readonly result: string
    readonly enclosingKinds: ReadonlyArray<Type.ParameterKind>
  }
  readonly syntax: SyntaxTree.Node
}

/** The canonical, duplicate, or unidentified canonical-identity state of one header. */
export type CanonicalState =
  | { readonly _tag: 'Canonical'; readonly id: CanonicalId }
  | {
      readonly _tag: 'Duplicate'
      readonly original: CanonicalId
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unidentified' }

/** A declaration or field name supplied by syntax or explicitly unavailable after recovery. */
export type DeclaredName =
  | { readonly _tag: 'Present'; readonly spelling: string; readonly token: Token.Token }
  | { readonly _tag: 'Unavailable'; readonly syntax: SyntaxTree.Element }

/** The exact one- or two-segment syntax retained for a declared type lookup. */
export interface TypePathFact {
  readonly _tag: 'TypePath'
  readonly spelling: string
  readonly segments: ReadonlyArray<{ readonly spelling: string; readonly token: Token.Token }>
  readonly syntax: SyntaxTree.Node
}

/** The normalized or unavailable decimal length retained by fixed-array type syntax. */
export type ArrayLengthFact =
  | {
      readonly _tag: 'Available'
      readonly value: number
      readonly spelling: string
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'OutOfRange'
      readonly spelling: string
      readonly token: Token.Token
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unavailable'; readonly syntax: SyntaxTree.Element }

/** The resolved, unresolved, or syntax-unavailable declared type. */
export type DeclaredTypeFact =
  | {
      readonly _tag: 'Resolved'
      readonly type: SemanticType
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Element
      readonly path?: TypePathFact
      readonly components?: ReadonlyArray<DeclaredTypeFact>
      readonly exposureCause?: Diagnostic.Identity
      readonly unionSource?: UnionSourceFact
      readonly exactItem?: {
        readonly path: TypePathFact
        readonly declaration: CanonicalId
      }
    }
  | {
      readonly _tag: 'Unresolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Element
      readonly path: TypePathFact
      readonly cause?: Diagnostic.Identity
      readonly candidate?: Type.Nominal
    }
  | {
      readonly _tag: 'FixedArray'
      readonly element: DeclaredTypeFact
      readonly length: ArrayLengthFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Slice'
      readonly access: Type.Slice['access']
      readonly element: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Reference'
      readonly access: 'Shared' | 'Exclusive'
      readonly target: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Callable'
      readonly mode: Type.CallableMode
      readonly parameters: ReadonlyArray<DeclaredTypeFact>
      readonly result: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Applied'
      readonly target: DeclaredTypeFact
      readonly arguments: ReadonlyArray<DeclaredTypeFact>
      readonly failureRow?: {
        readonly failures: ReadonlyArray<DeclaredTypeFact>
        readonly parameters: ReadonlyArray<Type.Parameter>
        readonly syntax: SyntaxTree.Node
      }
      readonly requirementRow?: {
        readonly requirements: ReadonlyArray<{
          readonly capability: DeclaredTypeFact
          readonly role: string
          readonly access: Type.Requirement['access']
          readonly syntax: SyntaxTree.Node
        }>
        readonly parameters: ReadonlyArray<Type.Parameter>
        readonly syntax: SyntaxTree.Node
      }
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Effect'
      readonly access: Type.Effect['access']
      readonly success: DeclaredTypeFact
      readonly failures: ReadonlyArray<DeclaredTypeFact>
      readonly failureParameters: ReadonlyArray<Type.Parameter>
      readonly requirements: ReadonlyArray<{
        readonly capability: DeclaredTypeFact
        readonly role: string
        readonly access: Type.Requirement['access']
        readonly syntax: SyntaxTree.Node
      }>
      readonly requirementParameters: ReadonlyArray<Type.Parameter>
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Union'
      readonly members: ReadonlyArray<DeclaredTypeFact>
      readonly separators: ReadonlyArray<Token.Token>
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'ExactRepresentation'
      readonly item: TypePathFact
      readonly arguments: ReadonlyArray<DeclaredTypeFact>
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
      readonly itemCandidate?: CanonicalId
    }
  | {
      readonly _tag: 'RepresentationParameter'
      readonly parameter: Type.Parameter
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly path: TypePathFact
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
      readonly cause?: Diagnostic.Identity
    }

/** Source-ordered union syntax retained beside one normalized resolved outcome. */
export interface UnionSourceFact {
  readonly _tag: 'UnionSource'
  readonly members: ReadonlyArray<DeclaredTypeFact>
  readonly separators: ReadonlyArray<Token.Token>
  readonly syntax: SyntaxTree.Node
}

export type ReturnTypeFact = DeclaredTypeFact

/** A source-retained and canonically normalized effect failure row. */
export interface FailureRowFact {
  readonly _tag: 'FailureRow'
  readonly members: ReadonlyArray<DeclaredTypeFact>
  readonly parameters: ReadonlyArray<Type.Parameter>
  readonly failures: ReadonlyArray<Type.Nominal>
  readonly syntax?: SyntaxTree.Node
  readonly available: boolean
}

/** A source-retained and canonically normalized Effect capability requirement row. */
export interface RequirementRowFact {
  readonly _tag: 'RequirementRow'
  readonly entries: ReadonlyArray<{
    readonly capability: DeclaredTypeFact
    readonly role: string
    readonly access: Type.Requirement['access']
    readonly syntax: SyntaxTree.Node
  }>
  readonly parameters: ReadonlyArray<Type.Parameter>
  readonly requirements: ReadonlyArray<Type.Requirement>
  readonly syntax?: SyntaxTree.Node
  readonly available: boolean
}

/** One ordered parameter declaration with exact concrete provenance. */
export interface ParameterFact {
  readonly _tag: 'ParameterDeclaration'
  readonly id: ParameterId
  readonly name: DeclaredName
  readonly declaredType: DeclaredTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One function declaration header and its syntax-owned semantic facts. */
export interface DeclarationFact {
  readonly _tag: 'FunctionDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly functionKind: 'Ordinary' | 'Effect'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly parameterCount: number
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
  readonly opaqueResult?: OpaqueResultFact
  readonly failureRow: FailureRowFact
  readonly requirementRow: RequirementRowFact
  readonly syntax: SyntaxTree.Node
}

/** The single borrowed parameter an ordinary slice result is lexically tied to. */
export interface ReturnedBorrowFact {
  readonly _tag: 'ReturnedBorrow'
  readonly parameter: ParameterFact
  readonly access: Type.Slice['access']
}

/**
 * Derives the deliberately conservative returned-view contract from an ordinary function header.
 * No lifetime names are inferred: a valid header has one, and only one, possible borrowed source.
 */
export const returnedBorrow = (declaration: DeclarationFact): ReturnedBorrowFact | undefined => {
  const result =
    declaration.returnType._tag === 'Resolved' ? declaration.returnType.type : undefined
  if (
    declaration.functionKind !== 'Ordinary' ||
    result === undefined ||
    !Type.containsViewBorrow(result)
  ) {
    return undefined
  }
  const borrowed = declaration.parameters.filter(
    (parameter) =>
      parameter.declaredType._tag === 'Resolved' &&
      (Type.isReference(parameter.declaredType.type) ||
        Type.containsViewBorrow(parameter.declaredType.type)),
  )
  const parameter = borrowed.length === 1 ? borrowed[0] : undefined
  if (parameter?.declaredType._tag !== 'Resolved') return undefined
  const source = parameter.declaredType.type
  const sourceAccess: 'Shared' | 'Exclusive' =
    Type.isSlice(source) || Type.isReference(source) ? source.access : 'Shared'
  const resultAccess: 'Shared' | 'Exclusive' = Type.isSlice(result) ? result.access : 'Shared'
  if (resultAccess === 'Exclusive' && sourceAccess !== 'Exclusive') {
    return undefined
  }
  return Object.freeze({
    _tag: 'ReturnedBorrow',
    parameter,
    access: resultAccess,
  })
}

/** The source-retained literal carried by one compile-time constant header. */
export type ConstantLiteralFact =
  | { readonly _tag: 'BooleanLiteral'; readonly value: boolean; readonly token: Token.Token }
  | { readonly _tag: 'CharacterLiteral'; readonly value: number; readonly token: Token.Token }
  | {
      readonly _tag: 'IntegerLiteral'
      readonly value: bigint
      readonly spelling: string
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'FloatingLiteral'
      readonly spelling: string
      readonly token: Token.Token
    }
  | {
      readonly _tag: 'StringLiteral'
      readonly data: StaticText.Data
      readonly token: Token.Token
    }
  // One pointer-width fact named below `Target`. It is not a literal the source spells; it selects
  // between literals the compiler already holds, because no single number bounds a pointer-width
  // integer on both a 32-bit and a 64-bit target.
  | {
      readonly _tag: 'TargetConstant'
      readonly selector: TargetConstant.Selector
      readonly token: Token.Token
    }
  // A literal the lexer accepted but no value can be decoded from; it carries its own detail so
  // the reference site reports the real cause instead of a literal-kind mismatch.
  | { readonly _tag: 'Malformed'; readonly detail: string; readonly syntax: SyntaxTree.Element }
  | { readonly _tag: 'Unavailable'; readonly syntax: SyntaxTree.Element }

/** One explicitly typed, compile-time scalar or static-text declaration. */
export interface ConstantFact {
  readonly _tag: 'ConstantDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly declaredType: DeclaredTypeFact
  readonly literal: ConstantLiteralFact
  readonly initializer: SyntaxTree.Node
  readonly syntax: SyntaxTree.Node
}

/** The unique, duplicate, or unidentified state of one field name. */
export type FieldState =
  | { readonly _tag: 'Unique'; readonly id: FieldId }
  | { readonly _tag: 'Duplicate'; readonly original: FieldId; readonly cause: Diagnostic.Identity }
  | { readonly _tag: 'Unidentified' }

/** One ordered nominal struct field header. */
export interface FieldFact {
  readonly _tag: 'StructField'
  readonly id: FieldId
  readonly state: FieldState
  readonly visibility: 'Public' | 'Private'
  readonly name: DeclaredName
  readonly declaredType: DeclaredTypeFact
  readonly syntax: SyntaxTree.Node
}

/** The finite dependency state of one nominal struct declaration. */
export type StructDependency =
  | { readonly _tag: 'Available'; readonly types: ReadonlyArray<Type.Nominal> }
  | {
      readonly _tag: 'Unavailable'
      readonly types: ReadonlyArray<Type.Nominal>
      readonly cause?: Diagnostic.Identity
    }

/** One nominal struct declaration header and its ordered fields. */
export interface StructFact {
  readonly _tag: 'StructDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly fields: ReadonlyArray<FieldFact>
  readonly dependency: StructDependency
  readonly syntax: SyntaxTree.Node
}

/** Canonical identity of one operation nested beneath a source service declaration. */
export interface ServiceOperationId {
  readonly _tag: 'ServiceOperationId'
  readonly service: DeclarationId
  readonly name: string
}

export type ServiceOperationState =
  | { readonly _tag: 'Unique'; readonly id: ServiceOperationId }
  | {
      readonly _tag: 'Duplicate'
      readonly original: ServiceOperationId
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unidentified' }

/** One complete operation contract owned by a source-declared service. */
export interface ServiceOperationFact {
  readonly _tag: 'ServiceOperation'
  readonly id: DeclarationId
  readonly state: ServiceOperationState
  readonly functionKind: 'Ordinary' | 'Effect'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly parameterCount: number
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
  readonly opaqueResult?: OpaqueResultFact
  readonly failureRow: FailureRowFact
  readonly requirementRow: RequirementRowFact
  readonly syntax: SyntaxTree.Node
}

/** The literal ownership promised by one source interface operand. */
export type InterfaceOperandAccess = 'Shared' | 'Exclusive' | 'Take' | 'Unavailable'

/** One source interface operand with its authored type shape and ownership retained together. */
export interface InterfaceOperandFact {
  readonly _tag: 'InterfaceOperand'
  readonly parameter: ParameterFact
  readonly type: DeclaredTypeFact
  readonly access: InterfaceOperandAccess
}

/**
 * One complete operation contract owned by a source interface declaration.
 *
 * Interface operations reuse service-operation syntax, but this fact gives the static interface
 * path its own vocabulary: literal operands, flow kind, success, exact rows, and the access of the
 * operand that denotes the provider. It remains compile-time data and creates no service slot.
 */
export interface InterfaceOperationContractFact {
  readonly _tag: 'InterfaceOperationContract'
  readonly declaration: ServiceOperationFact
  readonly provider?: Type.Type
  readonly functionKind: ServiceOperationFact['functionKind']
  readonly operands: ReadonlyArray<InterfaceOperandFact>
  readonly success: ReturnTypeFact
  readonly failureRow: FailureRowFact
  readonly requirementRow: RequirementRowFact
  readonly receiverAccess: InterfaceOperandAccess
}

/** One complete interface operation after substituting a particular interface application. */
export interface InterfaceOperationApplicationFact
  extends Omit<InterfaceOperationContractFact, '_tag'> {
  readonly _tag: 'InterfaceOperationApplication'
  readonly capability: Type.Nominal
  readonly provider: Type.Type
  readonly source: InterfaceOperationContractFact
}

/**
 * One interface application retained on a bound.
 *
 * `providerMatches` is an explicit fact rather than an assumption: complete contracts may only be
 * consumed when the interface's first argument is the bounded provider. Damaged applications stay
 * visible with `available: false` and never collapse into an operation-free interface.
 */
export interface InterfaceApplicationFact {
  readonly _tag: 'InterfaceApplication'
  readonly declaration: CanonicalId
  readonly capability: Type.Nominal
  readonly provider: Type.Type
  readonly providerMatches: boolean
  readonly visibility: InterfaceFact['visibility']
  readonly operations: ReadonlyArray<InterfaceOperationApplicationFact>
  readonly available: boolean
}

/** One nominal runtime-service contract declared in ordinary Silk source. */
export interface ServiceFact {
  readonly _tag: 'ServiceDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly operations: ReadonlyArray<ServiceOperationFact>
  readonly syntax: SyntaxTree.Node
}

/** One nominal static interface contract declared in ordinary Silk source. */
export interface InterfaceFact {
  readonly _tag: 'InterfaceDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly operations: ReadonlyArray<ServiceOperationFact>
  readonly operationContracts: ReadonlyArray<InterfaceOperationContractFact>
  readonly syntax: SyntaxTree.Node
}

const interfaceOperandAccess = (type: DeclaredTypeFact): InterfaceOperandAccess => {
  if (type._tag === 'Reference' || type._tag === 'Slice') return type.access
  if (type._tag !== 'Resolved') return 'Unavailable'
  if (Type.isReference(type.type) || Type.isSlice(type.type)) return type.type.access
  return 'Take'
}

const interfaceReceiverAccess = (
  operands: ReadonlyArray<InterfaceOperandFact>,
  provider: Type.Type | undefined,
): InterfaceOperandAccess => {
  if (provider === undefined) return 'Unavailable'
  return (
    operands.find((operand) => {
      if (operand.type._tag !== 'Resolved') return false
      const type = operand.type.type
      return Type.equals(Type.isReference(type) ? type.target : type, provider)
    })?.access ?? 'Unavailable'
  )
}

const interfaceOperationContract = (
  operation: ServiceOperationFact,
  provider: Type.Type | undefined,
): InterfaceOperationContractFact => {
  const operands = Object.freeze(
    operation.parameters.map(
      (parameter): InterfaceOperandFact =>
        Object.freeze({
          _tag: 'InterfaceOperand',
          parameter,
          type: parameter.declaredType,
          access: interfaceOperandAccess(parameter.declaredType),
        }),
    ),
  )
  return Object.freeze({
    _tag: 'InterfaceOperationContract',
    declaration: operation,
    ...(provider === undefined ? {} : { provider }),
    functionKind: operation.functionKind,
    operands,
    success: operation.returnType,
    failureRow: operation.failureRow,
    requirementRow: operation.requirementRow,
    receiverAccess: interfaceReceiverAccess(operands, provider),
  })
}

const interfaceOperationContracts = (
  parameters: ReadonlyArray<TypeParameterFact>,
  operations: ReadonlyArray<ServiceOperationFact>,
): ReadonlyArray<InterfaceOperationContractFact> => {
  const provider = parameters.at(0)?.type
  return Object.freeze(
    operations.map((operation) => interfaceOperationContract(operation, provider)),
  )
}

const substituteDeclaredTypeFact = (
  fact: DeclaredTypeFact,
  substitution: Type.Substitution,
): DeclaredTypeFact => {
  if (fact._tag !== 'Resolved') return fact
  const type = Type.substitute(fact.type, substitution)
  return Object.freeze({ ...fact, type, spelling: Type.encode(type) })
}

const substituteFailureRowFact = (
  fact: FailureRowFact,
  substitution: Type.Substitution,
  rows: Type.Effect,
): FailureRowFact => {
  const members = Object.freeze(
    fact.members.map((member) => substituteDeclaredTypeFact(member, substitution)),
  )
  return Object.freeze({
    ...fact,
    members,
    parameters: rows.failureParameters,
    failures: rows.failures,
    available:
      rows.failureParameters.length === 0 &&
      members.every(
        (member) =>
          member._tag === 'Resolved' && Type.isNominal(member.type) && Type.isConcrete(member.type),
      ),
  })
}

const substituteRequirementRowFact = (
  fact: RequirementRowFact,
  substitution: Type.Substitution,
  rows: Type.Effect,
): RequirementRowFact => {
  const entries = Object.freeze(
    fact.entries.map((entry) =>
      Object.freeze({
        ...entry,
        capability: substituteDeclaredTypeFact(entry.capability, substitution),
      }),
    ),
  )
  return Object.freeze({
    ...fact,
    entries,
    parameters: rows.requirementParameters,
    requirements: rows.requirements,
    available:
      rows.requirementParameters.length === 0 &&
      entries.every(
        (entry) =>
          entry.capability._tag === 'Resolved' &&
          (Type.isNominal(entry.capability.type) ||
            (Type.isParameter(entry.capability.type) && entry.capability.type.kind === 'Value')),
      ),
  })
}

const applyInterfaceOperation = (
  source: InterfaceOperationContractFact,
  capability: Type.Nominal,
  provider: Type.Type,
  substitution: Type.Substitution | undefined,
): InterfaceOperationApplicationFact => {
  if (substitution === undefined)
    return Object.freeze({
      _tag: 'InterfaceOperationApplication',
      declaration: source.declaration,
      capability,
      provider,
      source,
      functionKind: source.functionKind,
      operands: source.operands,
      success: source.success,
      failureRow: source.failureRow,
      requirementRow: source.requirementRow,
      receiverAccess: source.receiverAccess,
    })
  const operands = Object.freeze(
    source.operands.map((operand): InterfaceOperandFact => {
      const type = substituteDeclaredTypeFact(operand.type, substitution)
      return Object.freeze({
        ...operand,
        parameter: Object.freeze({ ...operand.parameter, declaredType: type }),
        type,
        access: interfaceOperandAccess(type),
      })
    }),
  )
  const substitutedRows = Type.substitute(
    Type.effect(
      Type.unit,
      source.failureRow.failures,
      'Shared',
      source.requirementRow.requirements,
      source.failureRow.parameters,
      source.requirementRow.parameters,
    ),
    substitution,
  )
  const rows = Type.isEffect(substitutedRows)
    ? substitutedRows
    : Type.effect(Type.unit, [], 'Shared')
  return Object.freeze({
    _tag: 'InterfaceOperationApplication',
    declaration: source.declaration,
    capability,
    provider,
    source,
    functionKind: source.functionKind,
    operands,
    success: substituteDeclaredTypeFact(source.success, substitution),
    failureRow: substituteFailureRowFact(source.failureRow, substitution, rows),
    requirementRow: substituteRequirementRowFact(source.requirementRow, substitution, rows),
    receiverAccess: interfaceReceiverAccess(operands, provider),
  })
}

const interfaceOperationAvailable = (operation: InterfaceOperationApplicationFact): boolean =>
  operation.operands.every((operand) => operand.type._tag === 'Resolved') &&
  operation.success._tag === 'Resolved' &&
  operation.failureRow.members.every(
    (member) => member._tag === 'Resolved' && Type.isNominal(member.type),
  ) &&
  operation.requirementRow.entries.every(
    (entry) =>
      entry.capability._tag === 'Resolved' &&
      (Type.isNominal(entry.capability.type) ||
        (Type.isParameter(entry.capability.type) && entry.capability.type.kind === 'Value')),
  ) &&
  operation.receiverAccess !== 'Unavailable'

const interfaceApplication = (
  declaration: InterfaceFact,
  capability: Type.Nominal,
  provider: Type.Type,
): InterfaceApplicationFact | undefined => {
  if (declaration.canonical._tag !== 'Canonical') return undefined
  const declaredProvider = capability.arguments.at(0)
  const providerMatches =
    declaredProvider !== undefined &&
    Type.isTypeArgument(declaredProvider) &&
    Type.equals(declaredProvider, provider)
  const substitution = Type.substitution(
    declaration.typeParameters.map((parameter) => parameter.type),
    capability.arguments,
  )
  const sourceContracts =
    declaration.operationContracts.length === declaration.operations.length
      ? declaration.operationContracts
      : interfaceOperationContracts(declaration.typeParameters, declaration.operations)
  const operations = Object.freeze(
    sourceContracts.map((operation) =>
      applyInterfaceOperation(operation, capability, provider, substitution),
    ),
  )
  const available =
    providerMatches && substitution !== undefined && operations.every(interfaceOperationAvailable)
  return Object.freeze({
    _tag: 'InterfaceApplication',
    declaration: declaration.canonical.id,
    capability,
    provider,
    providerMatches,
    visibility: declaration.visibility,
    operations,
    available,
  })
}

/**
 * One interface application a conditional conformance must prove before it admits a witness.
 *
 * The requirement is written in the header's own parameter list, as the bound of the binder it
 * constrains, and it states its provider explicitly: `impl<S: Decoder<S>>` requires a decoder for
 * `S`, not for some implicit `Self`. Retaining the applied capability rather than the bound's bare
 * spelling is what lets a requirement name a different specialization of the same interface.
 */
export interface ConformanceRequirementFact {
  readonly _tag: 'ConformanceRequirement'
  readonly parameter: Type.Parameter
  readonly spelling: string
  readonly capability: DeclaredTypeFact
  readonly syntax: SyntaxTree.Node
}

/** Whether one conformance head is free of possible ambiguity with every other head. */
export type ConformanceCoherence =
  | { readonly _tag: 'Coherent' }
  | {
      readonly _tag: 'Overlapping'
      readonly module: string
      readonly ordinal: number
    }

/** Whether following one conformance's requirements can only descend toward a base witness. */
export type ConformanceTermination =
  | { readonly _tag: 'Terminating' }
  | {
      readonly _tag: 'NonTerminating'
      readonly failures: ReadonlyArray<ConformanceHead.TerminationFailure>
    }
  | { readonly _tag: 'UnavailableTermination' }

export type ConformanceValidity =
  | { readonly _tag: 'UncheckedConformance' }
  | { readonly _tag: 'ValidConformance' }
  | { readonly _tag: 'InvalidConformance' }

/** One source-retained capability conformance witness. */
export interface ConformanceFact {
  readonly _tag: 'ConformanceDeclaration'
  readonly module: string
  readonly ordinal: number
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly requirements: ReadonlyArray<ConformanceRequirementFact>
  readonly capability: DeclaredTypeFact
  readonly provider: DeclaredTypeFact
  /**
   * Conformance coherence is a property of the whole program rather than of one module, so a
   * conformance is visible wherever its provider and interface are. The field records that
   * decision explicitly rather than leaving it implicit in the absence of a modifier.
   */
  readonly visibility: 'Public'
  readonly operations: ReadonlyArray<{
    readonly name: DeclaredName
    readonly target:
      | TypePathFact
      | { readonly _tag: 'Unavailable'; readonly syntax: SyntaxTree.Element }
    readonly contract?: InterfaceOperationApplicationFact
    /** The mapped declaration's binders expressed over this conformance header. */
    readonly targetArguments?: ReadonlyArray<Type.GenericArgument>
    readonly syntax: SyntaxTree.Node
  }>
  readonly hook?: DropHookFact
  /** The alpha-normalized head, present once the capability and provider both resolve. */
  readonly head?: ConformanceHead.ConformanceHead
  readonly coherence: ConformanceCoherence
  readonly termination: ConformanceTermination
  /** Whether complete header/body validation admitted this declaration as a witness candidate. */
  readonly validity: ConformanceValidity
  readonly syntax: SyntaxTree.Node
}

/** The source-retained header of one compiler-sealed Drop hook. */
export interface DropHookFact {
  readonly _tag: 'DropHookDeclaration'
  readonly name: DeclaredName
  readonly functionKind: 'Ordinary' | 'Effect'
  readonly typeParameterCount: number
  readonly parameterCount: number
  readonly parameterName: DeclaredName
  readonly parameterType: DeclaredTypeFact
  readonly returnType: DeclaredTypeFact
  readonly failureRow: FailureRowFact
  readonly requirementRow: RequirementRowFact
  readonly syntax: SyntaxTree.Node
}

/** One canonical nominal capability witness selected without erasing its provider type. */
export type ConformanceWitness =
  | {
      readonly _tag: 'IdentityConformanceWitness'
      readonly capability: Type.Nominal
      readonly provider: Type.Nominal
    }
  | {
      readonly _tag: 'IntrinsicConformanceWitness'
      readonly capability: Type.Nominal
      readonly provider: Type.Nominal
    }
  | {
      readonly _tag: 'SourceConformanceWitness'
      readonly module: string
      readonly ordinal: number
      readonly capability: Type.Nominal
      readonly provider: Type.Nominal
      readonly operations: ReadonlyArray<{
        readonly name: string
        readonly implementation: CanonicalId
      }>
      /**
       * The header's binders as this specialization bound them, in declaration order.
       *
       * This is the whole of what a conditional witness carries. The goals it was proved from are
       * reached through their own instances, so nothing about a requirement travels with the
       * witness that selected it.
       */
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
    }

/** Any declaration kind occupying the shared module-level namespace. */
export type MemberFact = DeclarationFact | StructFact | ServiceFact | InterfaceFact | ConstantFact

export type ParameterLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly parameter: ParameterFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly parameters: ReadonlyArray<ParameterFact>
    }

export type DeclarationLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: DeclarationFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<DeclarationFact>
    }

export type MemberLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: MemberFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<MemberFact>
    }

export type StructLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: StructFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<StructFact>
    }

export type FieldLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly field: FieldFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly fields: ReadonlyArray<FieldFact>
    }

/** One module's collected headers with their header-level diagnostics. */
export interface ModuleHeaders {
  readonly _tag: 'ModuleHeaders'
  readonly module: string
  readonly members: ReadonlyArray<MemberFact>
  readonly declarations: ReadonlyArray<DeclarationFact>
  readonly structs: ReadonlyArray<StructFact>
  readonly services: ReadonlyArray<ServiceFact>
  readonly interfaces: ReadonlyArray<InterfaceFact>
  readonly constants: ReadonlyArray<ConstantFact>
  readonly conformances: ReadonlyArray<ConformanceFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

/** The immutable declaration index of one loaded closure. */
export interface Index {
  readonly _tag: 'DeclarationIndex'
  readonly stage: 'Collected' | 'Complete'
  readonly modules: ReadonlyArray<ModuleHeaders>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export interface TypeResolution {
  readonly fact: DeclaredTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export type TypeResolver = (module: string, path: TypePathFact) => TypeResolution

export type ItemResolution =
  | { readonly _tag: 'Resolved'; readonly declaration: MemberFact }
  | { readonly _tag: 'Missing' }
  | { readonly _tag: 'Ambiguous'; readonly count: number; readonly cause?: Diagnostic.Identity }
  | {
      readonly _tag: 'Inaccessible'
      readonly declaration: MemberFact
      readonly cause: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Unavailable'
      readonly declaration?: MemberFact
      readonly cause?: Diagnostic.Identity
    }

export type ItemResolver = (module: string, path: TypePathFact) => ItemResolution

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Header token span does not belong to source ${source.id}`),
  )

const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = SyntaxTree.directNode(parent, kind)
  if (child === undefined)
    throw new RangeError(`Header collection expected ${kind} below ${parent.kind}`)
  return child
}

const isDeclaredTypeNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  SyntaxTree.isNode(element) &&
  (element.kind === 'TypePath' ||
    element.kind === 'AppliedType' ||
    element.kind === 'FixedArrayType' ||
    element.kind === 'SliceType' ||
    element.kind === 'ReferenceType' ||
    element.kind === 'CallableType' ||
    element.kind === 'UnitType' ||
    element.kind === 'ParenthesizedType' ||
    element.kind === 'ExactRepresentationType' ||
    element.kind === 'OpaqueResultType' ||
    element.kind === 'UnionType')

const declaredTypeNode = (parent: SyntaxTree.Node): SyntaxTree.Node => {
  const child = parent.children.find((element): element is SyntaxTree.Node =>
    isDeclaredTypeNode(element),
  )
  if (child === undefined) throw new RangeError(`Header collection expected a declared type`)
  return child
}

const presentName = (source: SourceFile.SourceFile, node: SyntaxTree.Node): DeclaredName => {
  const token = SyntaxTree.directToken(node, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(node, 'Identifier'),
      })
    : Object.freeze({ _tag: 'Present', spelling: spelling(source, token), token })
}

const constantLiteral = (
  source: SourceFile.SourceFile,
  initializer: SyntaxTree.Node,
): ConstantLiteralFact => {
  if (initializer.kind === 'BooleanLiteralExpression') {
    const token =
      SyntaxTree.directToken(initializer, 'TrueKeyword') ??
      SyntaxTree.directToken(initializer, 'FalseKeyword')
    return token === undefined
      ? Object.freeze({ _tag: 'Unavailable', syntax: initializer })
      : Object.freeze({ _tag: 'BooleanLiteral', value: token.kind === 'TrueKeyword', token })
  }
  if (initializer.kind === 'CharacterLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'CharLiteral')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    if (bytes === undefined || form === undefined)
      return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const decoded = StaticText.decodeScalar(Array.from(bytes), form)
    return decoded._tag === 'Scalar'
      ? Object.freeze({ _tag: 'CharacterLiteral', value: decoded.value, token })
      : Object.freeze({ _tag: 'Malformed', detail: decoded.detail, syntax: initializer })
  }
  if (initializer.kind === 'IntegerLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'DecimalInteger')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const digits = spelling(source, token)
    const negative = SyntaxTree.directToken(initializer, 'Minus') !== undefined
    const magnitude = IntegerLiteral.magnitude(digits)
    return Object.freeze({
      _tag: 'IntegerLiteral',
      value: negative ? -magnitude : magnitude,
      spelling: `${negative ? '-' : ''}${digits}`,
      token,
    })
  }
  if (initializer.kind === 'FloatingLiteralExpression') {
    const token = SyntaxTree.directToken(initializer, 'DecimalFloat')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const literal = DigitSeparator.strip(spelling(source, token))
    return Object.freeze({
      _tag: 'FloatingLiteral',
      spelling: `${SyntaxTree.directToken(initializer, 'Minus') === undefined ? '' : '-'}${literal}`,
      token,
    })
  }
  if (initializer.kind === 'StaticTextLiteralExpression') {
    const token =
      SyntaxTree.directToken(initializer, 'TextLiteral') ??
      SyntaxTree.directToken(initializer, 'ByteStringLiteral')
    if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    const bytes = Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    if (bytes === undefined || form === undefined)
      return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
    // The header decodes once so every reference — in this module or an importing one — shares
    // the exact bytes the equivalent `let` binding would produce.
    const decoded = StaticText.decode(Array.from(bytes), form)
    return decoded._tag === 'Decoded'
      ? Object.freeze({ _tag: 'StringLiteral', data: decoded.data, token })
      : Object.freeze({ _tag: 'Malformed', detail: decoded.detail, syntax: initializer })
  }
  const target = targetConstant(source, initializer)
  if (target !== undefined) return target
  return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
}

/**
 * Recognizes `Target.<fact>`, the one initializer form that is not a source literal. The projection
 * is matched on syntax alone — `Target` names no declaration and resolves to nothing — so a form
 * that is already rejected today is the only form whose meaning changes.
 */
const targetConstant = (
  source: SourceFile.SourceFile,
  initializer: SyntaxTree.Node,
): ConstantLiteralFact | undefined => {
  if (initializer.kind !== 'FieldProjectionExpression') return undefined
  const base = SyntaxTree.directNode(initializer, 'IdentifierExpression')
  const baseToken = base === undefined ? undefined : SyntaxTree.directToken(base, 'Identifier')
  if (baseToken === undefined || spelling(source, baseToken) !== TargetConstant.root)
    return undefined
  const member = SyntaxTree.directToken(initializer, 'Identifier')
  if (member === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: initializer })
  const memberSpelling = spelling(source, member)
  const selector = TargetConstant.find(memberSpelling)
  return selector === undefined
    ? Object.freeze({
        _tag: 'Malformed',
        detail: `${TargetConstant.root}.${memberSpelling} names no target fact; the target facts are ${TargetConstant.all.map((candidate) => `${TargetConstant.root}.${candidate}`).join(', ')}`,
        syntax: initializer,
      })
    : Object.freeze({ _tag: 'TargetConstant', selector, token: member })
}

/** Retains and initially resolves one concrete type path against built-in types only. */
export const analyzeDeclaredType = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter> = new Map(),
  genericArgumentPosition = false,
): TypeResolution => {
  if (syntax.kind === 'UnitType') {
    const token = SyntaxTree.directToken(syntax, 'LeftParenthesis')
    if (token === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: Type.unit,
        spelling: '()',
        token,
        syntax,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (syntax.kind === 'CallableType') {
    const token = SyntaxTree.directToken(syntax, 'FnKeyword')
    const typeNodes = syntax.children.filter(isDeclaredTypeNode)
    const resultSyntax = typeNodes.at(-1)
    if (token === undefined || resultSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const mode: Type.CallableMode =
      SyntaxTree.directToken(syntax, 'OnceKeyword') !== undefined
        ? 'Take'
        : SyntaxTree.directToken(syntax, 'MutKeyword') !== undefined
          ? 'Exclusive'
          : 'Shared'
    const analyzed = typeNodes.map((node) => analyzeDeclaredType(source, node, typeParameters))
    const result = analyzed.at(-1)
    const parameters = analyzed.slice(0, -1)
    const diagnostics = Object.freeze(analyzed.flatMap((entry) => entry.diagnostics))
    if (
      result?.fact._tag === 'Resolved' &&
      parameters.every((entry) => entry.fact._tag === 'Resolved')
    ) {
      const type = Type.callable(
        parameters.flatMap((entry) => (entry.fact._tag === 'Resolved' ? [entry.fact.type] : [])),
        result.fact.type,
        mode,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token,
          syntax,
          components: Object.freeze([
            ...parameters.map((parameter) => parameter.fact),
            result.fact,
          ]),
        }),
        diagnostics,
      })
    }
    const resultFact = result?.fact ?? Object.freeze({ _tag: 'Unavailable' as const, syntax })
    const cause = [...parameters.map((entry) => entry.fact), resultFact]
      .flatMap((fact) => ('cause' in fact && fact.cause !== undefined ? [fact.cause] : []))
      .at(-1)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Callable',
        mode,
        parameters: Object.freeze(parameters.map((entry) => entry.fact)),
        result: resultFact,
        spelling: `${mode === 'Exclusive' ? 'mut ' : mode === 'Take' ? 'once ' : ''}fn(...)`,
        token,
        syntax,
        ...(cause === undefined ? {} : { cause }),
      }),
      diagnostics,
    })
  }
  if (syntax.kind === 'ParenthesizedType') {
    const inner = syntax.children.find(isDeclaredTypeNode)
    if (inner === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    const analyzed = analyzeDeclaredType(source, inner, typeParameters, genericArgumentPosition)
    return Object.freeze({
      fact: Object.freeze({ ...analyzed.fact, syntax }),
      diagnostics: analyzed.diagnostics,
    })
  }
  if (syntax.kind === 'UnionType') {
    const members = syntax.children
      .filter(isDeclaredTypeNode)
      .map((member) => analyzeDeclaredType(source, member, typeParameters))
    const diagnostics: Array<Diagnostic.Diagnostic> = members.flatMap((member) =>
      Array.from(member.diagnostics),
    )
    const facts = Object.freeze(members.map((member) => member.fact))
    const separators = Object.freeze(
      syntax.children.filter(
        (element): element is Token.Token => SyntaxTree.isToken(element) && element.kind === 'Pipe',
      ),
    )
    const firstResolved = facts.find(
      (fact): fact is Extract<DeclaredTypeFact, { readonly _tag: 'Resolved' }> =>
        fact._tag === 'Resolved',
    )
    const firstToken = SyntaxTree.tokens(syntax).find((token) => token.kind === 'Identifier')
    if (firstToken === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    if (facts.every((fact) => fact._tag === 'Resolved')) {
      const resolved = facts.filter(
        (fact): fact is Extract<DeclaredTypeFact, { readonly _tag: 'Resolved' }> =>
          fact._tag === 'Resolved',
      )
      const normalized = Type.union(resolved.map((fact) => fact.type))
      if (normalized._tag === 'Normalized') {
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            type: normalized.type,
            spelling: Type.encode(normalized.type),
            token: firstResolved?.token ?? firstToken,
            syntax,
            unionSource: Object.freeze({
              _tag: 'UnionSource',
              members: facts,
              separators,
              syntax,
            }),
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      for (const invalid of normalized.members) {
        const sourceFact = resolved.find((fact) => Type.equals(fact.type, invalid))
        diagnostics.push(
          Diagnostic.invalidUnionMember(
            Type.encode(invalid),
            sourceFact?.syntax.span ?? syntax.span,
          ),
        )
      }
    }
    const cause = diagnostics.at(-1)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Union',
        members: facts,
        separators,
        spelling: facts
          .map((fact) => (fact._tag === 'Resolved' ? Type.encode(fact.type) : 'unavailable'))
          .join(' | '),
        token: firstResolved?.token ?? firstToken,
        syntax,
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (syntax.kind === 'SliceType') {
    const token = SyntaxTree.directToken(syntax, 'Ampersand')
    const elementSyntax = syntax.children.find(isDeclaredTypeNode)
    if (token === undefined || elementSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const access: Type.Slice['access'] =
      SyntaxTree.directToken(syntax, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
    const element = analyzeDeclaredType(source, elementSyntax, typeParameters)
    if (element.fact._tag === 'Resolved' && Type.isString(element.fact.type)) {
      const diagnostic = Diagnostic.invalidStringViewType('slice', syntax.span)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax,
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostics: Object.freeze([...element.diagnostics, diagnostic]),
      })
    }
    if (element.fact._tag === 'Resolved') {
      const type = Type.slice(access, element.fact.type)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token,
          syntax,
          components: Object.freeze([element.fact]),
        }),
        diagnostics: element.diagnostics,
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Slice',
        access,
        element: element.fact,
        spelling: `${access === 'Exclusive' ? '&mut' : '&'}[unavailable]`,
        token,
        syntax,
        ...('cause' in element.fact && element.fact.cause !== undefined
          ? { cause: element.fact.cause }
          : {}),
      }),
      diagnostics: element.diagnostics,
    })
  }
  if (syntax.kind === 'ReferenceType') {
    const token = SyntaxTree.directToken(syntax, 'Ampersand')
    const targetSyntax = syntax.children.find(isDeclaredTypeNode)
    if (token === undefined || targetSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const access: 'Shared' | 'Exclusive' =
      SyntaxTree.directToken(syntax, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
    const target = analyzeDeclaredType(source, targetSyntax, typeParameters)
    if (target.fact._tag === 'Resolved' && Type.isString(target.fact.type)) {
      const diagnostic = Diagnostic.invalidStringViewType('reference', syntax.span)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax,
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostics: Object.freeze([...target.diagnostics, diagnostic]),
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Reference',
        access,
        target: target.fact,
        spelling: `${access === 'Exclusive' ? '&mut ' : '&'}unavailable`,
        token,
        syntax,
        ...('cause' in target.fact && target.fact.cause !== undefined
          ? { cause: target.fact.cause }
          : {}),
      }),
      diagnostics: target.diagnostics,
    })
  }
  if (syntax.kind === 'FixedArrayType') {
    const arrayToken = SyntaxTree.directToken(syntax, 'LeftBracket')
    const elementSyntax = syntax.children.find(isDeclaredTypeNode)
    const lengthToken = SyntaxTree.directToken(syntax, 'DecimalInteger')
    if (arrayToken === undefined || elementSyntax === undefined) {
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax: SyntaxTree.unavailableChild(syntax, 'LeftBracket'),
        }),
        diagnostics: Object.freeze([]),
      })
    }
    const element = analyzeDeclaredType(source, elementSyntax, typeParameters)
    let length: ArrayLengthFact
    const diagnostics: Array<Diagnostic.Diagnostic> = [...element.diagnostics]
    if (lengthToken === undefined) {
      length = Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(syntax, 'DecimalInteger'),
      })
    } else {
      const lengthSpelling = spelling(source, lengthToken)
      const value = Number(IntegerLiteral.magnitude(lengthSpelling))
      if (!Number.isSafeInteger(value) || value > 2147483647) {
        const diagnostic = Diagnostic.integerOutOfRange(lengthSpelling, lengthToken.span)
        diagnostics.push(diagnostic)
        length = Object.freeze({
          _tag: 'OutOfRange',
          spelling: lengthSpelling,
          token: lengthToken,
          cause: Diagnostic.identity(diagnostic),
        })
      } else {
        length = Object.freeze({
          _tag: 'Available',
          value,
          spelling: lengthSpelling,
          token: lengthToken,
        })
      }
    }
    if (element.fact._tag === 'Resolved' && length._tag === 'Available') {
      const type = Type.fixedArray(element.fact.type, length.value)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: arrayToken,
          syntax,
          components: Object.freeze([element.fact]),
        }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'FixedArray',
        element: element.fact,
        length,
        spelling: `Array<${
          element.fact._tag === 'Resolved' ? Type.encode(element.fact.type) : 'unavailable'
        }, ${length._tag === 'Available' ? length.value : 'unavailable'}>`,
        token: arrayToken,
        syntax,
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (syntax.kind === 'OpaqueResultType') {
    // The binder is owned by the declaration that carries it, so its representation parameters and
    // family key can only be minted where that canonical identity is known. Until the declaration
    // site supplies it, the result stays deterministically unavailable rather than resolving to a
    // parameter with a fabricated owner.
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax }),
      diagnostics: Object.freeze([]),
    })
  }
  if (syntax.kind === 'ExactRepresentationType') {
    const item = syntax.children.find(isDeclaredTypeNode)
    const pathSyntax =
      item === undefined
        ? undefined
        : item.kind === 'TypePath'
          ? item
          : SyntaxTree.directNode(item, 'TypePath')
    const keyword = SyntaxTree.directToken(syntax, 'Identifier')
    if (item === undefined || pathSyntax === undefined || keyword === undefined)
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    const segments = SyntaxTree.tokens(pathSyntax)
      .filter((token) => token.kind === 'Identifier')
      .map((token) => Object.freeze({ spelling: spelling(source, token), token }))
    if (segments.length === 0 || !SyntaxTree.isAvailableSyntax(syntax))
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax: SyntaxTree.unavailableChild(syntax, 'Identifier'),
        }),
        diagnostics: Object.freeze([]),
      })
    const list =
      item.kind === 'AppliedType' ? SyntaxTree.directNode(item, 'TypeArgumentList') : undefined
    const arguments_ = (list?.children.filter(isDeclaredTypeNode) ?? []).map((argument) =>
      analyzeDeclaredType(source, argument, typeParameters, true),
    )
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'ExactRepresentation',
        item: Object.freeze({
          _tag: 'TypePath',
          spelling: segments.map((segment) => segment.spelling).join('.'),
          segments: Object.freeze(segments),
          syntax: pathSyntax,
        }),
        arguments: Object.freeze(arguments_.map((argument) => argument.fact)),
        spelling: `typeof(${segments.map((segment) => segment.spelling).join('.')})`,
        token: keyword,
        syntax,
      }),
      diagnostics: Object.freeze(arguments_.flatMap((argument) => argument.diagnostics)),
    })
  }
  if (syntax.kind === 'AppliedType') {
    const pathSyntax = SyntaxTree.directNode(syntax, 'TypePath')
    const list = SyntaxTree.directNode(syntax, 'TypeArgumentList')
    const firstToken = SyntaxTree.tokens(syntax).find((token) => token.kind === 'Identifier')
    if (pathSyntax === undefined || list === undefined || firstToken === undefined) {
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze([]),
      })
    }
    const target = analyzeDeclaredType(source, pathSyntax, typeParameters)
    const arguments_ = list.children
      .filter(isDeclaredTypeNode)
      .map((argument) => analyzeDeclaredType(source, argument, typeParameters, true))
    const pathSegments = SyntaxTree.tokens(pathSyntax)
      .filter((token) => token.kind === 'Identifier')
      .map((token) => spelling(source, token))
    if (pathSegments.length === 1 && pathSegments.at(0) === 'Row') {
      const failureRow = SyntaxTree.directNode(list, 'FailureRow')
      const failureType = failureRow?.children.find(isDeclaredTypeNode)
      const failureNodes =
        failureType?.kind === 'UnionType'
          ? failureType.children.filter(isDeclaredTypeNode)
          : failureType === undefined
            ? []
            : [failureType]
      const diagnostics: Array<Diagnostic.Diagnostic> = [
        ...arguments_.flatMap((argument) => argument.diagnostics),
      ]
      if (arguments_.length !== 0)
        diagnostics.push(Diagnostic.typeArgumentArity('Row', 0, arguments_.length, firstToken.span))
      if (failureNodes.length === 1) {
        const member = failureNodes.at(0)
        const parameter =
          member === undefined ? undefined : parameterAtTypePath(source, member, typeParameters)
        const token =
          member === undefined ? undefined : SyntaxTree.directToken(member, 'Identifier')
        if (parameter?.kind === 'FailureRow') {
          const type = Type.failureProjection(parameter)
          return Object.freeze({
            fact: Object.freeze({
              _tag: 'Resolved',
              type,
              spelling: Type.encode(type),
              token: firstToken,
              syntax,
              components: Object.freeze([]),
            }),
            diagnostics: Object.freeze(diagnostics),
          })
        }
        if (parameter !== undefined && token !== undefined)
          diagnostics.push(
            Diagnostic.genericParameterKindMismatch(
              spelling(source, token),
              'FailureRow',
              parameter.kind,
              token.span,
            ),
          )
      }
      return Object.freeze({
        fact: Object.freeze({ _tag: 'Unavailable', syntax }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    if (pathSegments.length === 1 && pathSegments.at(0) === 'Effect') {
      const access: Type.Effect['access'] =
        SyntaxTree.directToken(syntax, 'OnceKeyword') !== undefined
          ? 'Take'
          : SyntaxTree.directToken(syntax, 'MutKeyword') !== undefined
            ? 'Exclusive'
            : 'Shared'
      const failureRow = SyntaxTree.directNode(list, 'FailureRow')
      const failureType =
        failureRow === undefined ? undefined : failureRow.children.find(isDeclaredTypeNode)
      const failureNodes =
        failureType?.kind === 'UnionType'
          ? failureType.children.filter(isDeclaredTypeNode)
          : failureType === undefined
            ? []
            : [failureType]
      const failureParameters: Array<Type.Parameter> = []
      const rowDiagnostics: Array<Diagnostic.Diagnostic> = []
      const failures = failureNodes.flatMap((member): ReadonlyArray<TypeResolution> => {
        const parameter = parameterAtTypePath(source, member, typeParameters)
        if (parameter?.kind === 'FailureRow') {
          failureParameters.push(parameter)
          return []
        }
        if (parameter?.kind === 'RequirementRow') {
          const token = SyntaxTree.directToken(member, 'Identifier')
          if (token !== undefined)
            rowDiagnostics.push(
              Diagnostic.genericParameterKindMismatch(
                spelling(source, token),
                'FailureRow',
                parameter.kind,
                token.span,
              ),
            )
          return []
        }
        return [analyzeDeclaredType(source, member, typeParameters)]
      })
      const requirements =
        SyntaxTree.directNode(list, 'RequirementRow')
          ?.children.filter(
            (element): element is SyntaxTree.Node =>
              SyntaxTree.isNode(element) && element.kind === 'Requirement',
          )
          .map((requirement) => {
            const capability = requirement.children.find(isDeclaredTypeNode)
            const analyzed =
              capability === undefined
                ? Object.freeze({
                    fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: requirement }),
                    diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                  })
                : analyzeDeclaredType(source, capability, typeParameters)
            const role = SyntaxTree.directToken(requirement, 'Identifier')
            return Object.freeze({
              capability: analyzed,
              role: role === undefined ? 'DefaultRole' : spelling(source, role),
              access:
                SyntaxTree.directToken(requirement, 'MutKeyword') === undefined
                  ? ('Shared' as const)
                  : ('Exclusive' as const),
              syntax: requirement,
            })
          }) ?? []
      const requirementParameters =
        SyntaxTree.directNode(list, 'RequirementRow')
          ?.children.filter(
            (element): element is SyntaxTree.Node =>
              SyntaxTree.isNode(element) && element.kind === 'TypePath',
          )
          .flatMap((path): ReadonlyArray<Type.Parameter> => {
            const token = SyntaxTree.directToken(path, 'Identifier')
            const parameter = parameterAtTypePath(source, path, typeParameters)
            if (parameter?.kind === 'RequirementRow') return [parameter]
            if (token !== undefined) {
              if (parameter === undefined)
                rowDiagnostics.push(Diagnostic.unknownType(spelling(source, token), token.span))
              else
                rowDiagnostics.push(
                  Diagnostic.genericParameterKindMismatch(
                    spelling(source, token),
                    'RequirementRow',
                    parameter.kind,
                    token.span,
                  ),
                )
            }
            return []
          }) ?? []
      const diagnostics = [
        ...rowDiagnostics,
        ...arguments_.flatMap((argument) => argument.diagnostics),
        ...failures.flatMap((failure) => failure.diagnostics),
        ...requirements.flatMap((requirement) => requirement.capability.diagnostics),
      ]
      const success = arguments_.at(0)?.fact
      if (arguments_.length !== 1) {
        diagnostics.push(
          Diagnostic.typeArgumentArity('Effect', 1, arguments_.length, firstToken.span),
        )
      }
      const resolvedFailures = failures.flatMap((failure) =>
        failure.fact._tag === 'Resolved' && Type.isNominal(failure.fact.type)
          ? [failure.fact.type]
          : [],
      )
      const resolvedRequirements = requirements.flatMap((requirement) =>
        requirement.capability.fact._tag === 'Resolved' &&
        Type.isNominal(requirement.capability.fact.type)
          ? [
              Object.freeze({
                capability: requirement.capability.fact.type,
                role: requirement.role,
                access: requirement.access,
              }),
            ]
          : [],
      )
      const failuresAvailable = failures.every(
        (failure) =>
          failure.fact._tag === 'Resolved' &&
          (Type.isNominal(failure.fact.type) || Type.isNever(failure.fact.type)),
      )
      if (
        arguments_.length === 1 &&
        success?._tag === 'Resolved' &&
        failuresAvailable &&
        resolvedRequirements.length === requirements.length
      ) {
        const type = Type.effect(
          success.type,
          resolvedFailures,
          access,
          resolvedRequirements,
          failureParameters,
          requirementParameters,
        )
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            type,
            spelling: Type.encode(type),
            token: firstToken,
            syntax,
            components: Object.freeze([
              target.fact,
              ...arguments_.map((argument) => argument.fact),
              ...failures.map((failure) => failure.fact),
              ...requirements.map((requirement) => requirement.capability.fact),
            ]),
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Effect',
          access,
          success: success ?? Object.freeze({ _tag: 'Unavailable', syntax: list }),
          failures: Object.freeze(failures.map((failure) => failure.fact)),
          failureParameters: Object.freeze(failureParameters),
          requirements: Object.freeze(
            requirements.map((requirement) =>
              Object.freeze({
                capability: requirement.capability.fact,
                role: requirement.role,
                access: requirement.access,
                syntax: requirement.syntax,
              }),
            ),
          ),
          requirementParameters: Object.freeze(requirementParameters),
          spelling: 'Effect',
          token: firstToken,
          syntax,
        }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    const failureRowSyntax = SyntaxTree.directNode(list, 'FailureRow')
    const failureType = failureRowSyntax?.children.find(isDeclaredTypeNode)
    const failureNodes =
      failureType?.kind === 'UnionType'
        ? failureType.children.filter(isDeclaredTypeNode)
        : failureType === undefined
          ? []
          : [failureType]
    const failureParameters: Array<Type.Parameter> = []
    const rowDiagnostics: Array<Diagnostic.Diagnostic> = []
    const failures = failureNodes.flatMap((member): ReadonlyArray<TypeResolution> => {
      const parameter = parameterAtTypePath(source, member, typeParameters)
      if (parameter?.kind === 'FailureRow') {
        failureParameters.push(parameter)
        return []
      }
      if (parameter?.kind === 'RequirementRow') {
        const token = SyntaxTree.directToken(member, 'Identifier')
        if (token !== undefined)
          rowDiagnostics.push(
            Diagnostic.genericParameterKindMismatch(
              spelling(source, token),
              'FailureRow',
              parameter.kind,
              token.span,
            ),
          )
        return []
      }
      return [analyzeDeclaredType(source, member, typeParameters)]
    })
    const requirementRowSyntax = SyntaxTree.directNode(list, 'RequirementRow')
    const requirements =
      requirementRowSyntax?.children
        .filter(
          (element): element is SyntaxTree.Node =>
            SyntaxTree.isNode(element) && element.kind === 'Requirement',
        )
        .map((requirement) => {
          const capability = requirement.children.find(isDeclaredTypeNode)
          const analyzed =
            capability === undefined
              ? Object.freeze({
                  fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: requirement }),
                  diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                })
              : analyzeDeclaredType(source, capability, typeParameters)
          const role = SyntaxTree.directToken(requirement, 'Identifier')
          return Object.freeze({
            capability: analyzed,
            role: role === undefined ? 'DefaultRole' : spelling(source, role),
            access:
              SyntaxTree.directToken(requirement, 'MutKeyword') === undefined
                ? ('Shared' as const)
                : ('Exclusive' as const),
            syntax: requirement,
          })
        }) ?? []
    const requirementParameters =
      requirementRowSyntax?.children
        .filter(
          (element): element is SyntaxTree.Node =>
            SyntaxTree.isNode(element) && element.kind === 'TypePath',
        )
        .flatMap((path): ReadonlyArray<Type.Parameter> => {
          const token = SyntaxTree.directToken(path, 'Identifier')
          const parameter = parameterAtTypePath(source, path, typeParameters)
          if (parameter?.kind === 'RequirementRow') return [parameter]
          if (token !== undefined) {
            if (parameter === undefined)
              rowDiagnostics.push(Diagnostic.unknownType(spelling(source, token), token.span))
            else
              rowDiagnostics.push(
                Diagnostic.genericParameterKindMismatch(
                  spelling(source, token),
                  'RequirementRow',
                  parameter.kind,
                  token.span,
                ),
              )
          }
          return []
        }) ?? []
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Applied',
        target: target.fact,
        arguments: Object.freeze(arguments_.map((argument) => argument.fact)),
        ...(failureRowSyntax === undefined
          ? {}
          : {
              failureRow: Object.freeze({
                failures: Object.freeze(failures.map((failure) => failure.fact)),
                parameters: Object.freeze(failureParameters),
                syntax: failureRowSyntax,
              }),
            }),
        ...(requirementRowSyntax === undefined
          ? {}
          : {
              requirementRow: Object.freeze({
                requirements: Object.freeze(
                  requirements.map((requirement) =>
                    Object.freeze({
                      ...requirement,
                      capability: requirement.capability.fact,
                    }),
                  ),
                ),
                parameters: Object.freeze(requirementParameters),
                syntax: requirementRowSyntax,
              }),
            }),
        spelling: SyntaxTree.tokens(syntax)
          .filter(
            (token) =>
              !['Whitespace', 'LineComment', 'DocComment', 'ModuleDocComment'].includes(token.kind),
          )
          .map((token) => spelling(source, token))
          .join(''),
        token: firstToken,
        syntax,
      }),
      diagnostics: Diagnostic.merge(
        target.diagnostics,
        ...arguments_.map((argument) => argument.diagnostics),
        ...failures.map((failure) => failure.diagnostics),
        ...requirements.map((requirement) => requirement.capability.diagnostics),
        rowDiagnostics,
      ),
    })
  }
  const tokens = syntax.children.filter(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
  const segments = tokens.map((token) =>
    Object.freeze({ spelling: spelling(source, token), token }),
  )
  const first = segments.at(0)
  if (first === undefined || !SyntaxTree.isAvailableSyntax(syntax)) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable',
        syntax: SyntaxTree.unavailableChild(syntax, 'Identifier'),
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const path: TypePathFact = Object.freeze({
    _tag: 'TypePath',
    spelling: segments.map((segment) => segment.spelling).join('.'),
    segments: Object.freeze(segments),
    syntax,
  })
  if (
    segments.length === 1 &&
    (Type.isBuiltin(first.spelling) || Type.isString(first.spelling) || first.spelling === 'never')
  ) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: first.spelling,
        spelling: first.spelling,
        token: first.token,
        syntax,
        path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const intrinsicNominal =
    segments.length === 1 ? Type.intrinsicNominals.get(first.spelling) : undefined
  if (intrinsicNominal !== undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: intrinsicNominal,
        spelling: first.spelling,
        token: first.token,
        syntax,
        path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const parameterType = segments.length === 1 ? typeParameters.get(first.spelling) : undefined
  if (parameterType !== undefined) {
    if (
      parameterType.kind === 'CallableRepresentation' ||
      parameterType.kind === 'EffectRepresentation'
    ) {
      const bound = parameterType.representationBound
      if (bound === undefined) {
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'RepresentationParameter',
            parameter: parameterType,
            spelling: first.spelling,
            token: first.token,
            syntax,
            path,
          }),
          diagnostics: Object.freeze([]),
        })
      }
      const type = Type.represented(
        bound,
        bound,
        Type.representationParameterArgument(parameterType),
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: first.spelling,
          token: first.token,
          syntax,
          path,
        }),
        diagnostics: Object.freeze([]),
      })
    }
    if (parameterType.kind !== 'Value' && !genericArgumentPosition) {
      const diagnostic = Diagnostic.genericParameterKindMismatch(
        first.spelling,
        'Value',
        parameterType.kind,
        first.token.span,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable',
          syntax,
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostics: Object.freeze([diagnostic]),
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type: parameterType,
        spelling: first.spelling,
        token: first.token,
        syntax,
        path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Unresolved',
      spelling: path.spelling,
      token: first.token,
      syntax,
      path,
    }),
    diagnostics: Object.freeze([]),
  })
}

const isSeparator = (element: SyntaxTree.Element, kind: Token.TokenKind): boolean =>
  (SyntaxTree.isToken(element) && element.kind === kind) ||
  (SyntaxTree.isMissingToken(element) && element.expected === kind)

const identifierToken = (elements: ReadonlyArray<SyntaxTree.Element>): Token.Token | undefined =>
  elements.every(SyntaxTree.isAvailableSyntax)
    ? elements.find(
        (element): element is Token.Token =>
          SyntaxTree.isToken(element) && element.kind === 'Identifier',
      )
    : undefined

const analyzeParameter = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  functionId: DeclarationId,
  ordinal: number,
  typeParameters: ReadonlyMap<string, Type.Parameter> = new Map(),
): { readonly fact: ParameterFact; readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic> } => {
  const colonIndex = node.children.findIndex((element) => isSeparator(element, 'Colon'))
  const nameElements = colonIndex < 0 ? node.children : node.children.slice(0, colonIndex)
  const nameToken = identifierToken(nameElements)
  const name: DeclaredName =
    nameToken === undefined
      ? Object.freeze({
          _tag: 'Unavailable',
          syntax: SyntaxTree.unavailableElement(nameElements, node),
        })
      : Object.freeze({ _tag: 'Present', spelling: spelling(source, nameToken), token: nameToken })
  const type = analyzeDeclaredType(source, declaredTypeNode(node), typeParameters)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ParameterDeclaration',
      id: Object.freeze({ _tag: 'ParameterId', function: functionId, ordinal }),
      name,
      declaredType: type.fact,
      syntax: node,
    }),
    diagnostics: type.diagnostics,
  })
}

const presentParameterEntries = (parameters: ReadonlyArray<ParameterFact>) =>
  parameters.flatMap((parameter) =>
    parameter.name._tag === 'Present'
      ? [
          Object.freeze({
            spelling: parameter.name.spelling,
            token: parameter.name.token,
            parameter,
          }),
        ]
      : [],
  )

const duplicateParameterDiagnostics = (parameters: ReadonlyArray<ParameterFact>) => {
  const first = new Map<string, ReturnType<typeof presentParameterEntries>[number]>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  for (const entry of presentParameterEntries(parameters)) {
    const original = first.get(entry.spelling)
    if (original === undefined) first.set(entry.spelling, entry)
    else
      diagnostics.push(
        Diagnostic.duplicateParameterName(entry.spelling, original.token.span, entry.token.span),
      )
  }
  return Object.freeze(diagnostics)
}

const collectFields = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  structId: DeclarationId,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
) => {
  const first = new Map<string, { readonly id: FieldId; readonly token: Token.Token }>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const fields = SyntaxTree.directNodes(node, 'StructField').map(
    (fieldNode, ordinal): FieldFact => {
      const id: FieldId = Object.freeze({ _tag: 'FieldId', struct: structId, ordinal })
      const name = presentName(source, fieldNode)
      const type = analyzeDeclaredType(source, declaredTypeNode(fieldNode), typeParameters)
      diagnostics.push(...type.diagnostics)
      let state: FieldState
      if (name._tag !== 'Present') state = Object.freeze({ _tag: 'Unidentified' })
      else {
        const original = first.get(name.spelling)
        if (original === undefined) {
          first.set(name.spelling, Object.freeze({ id, token: name.token }))
          state = Object.freeze({ _tag: 'Unique', id })
        } else {
          const diagnostic = Diagnostic.duplicateFieldName(
            name.spelling,
            original.token.span,
            name.token.span,
          )
          diagnostics.push(diagnostic)
          state = Object.freeze({
            _tag: 'Duplicate',
            original: original.id,
            cause: Diagnostic.identity(diagnostic),
          })
        }
      }
      return Object.freeze({
        _tag: 'StructField',
        id,
        state,
        visibility:
          SyntaxTree.directToken(fieldNode, 'PubKeyword') === undefined ? 'Private' : 'Public',
        name,
        declaredType: type.fact,
        syntax: fieldNode,
      })
    },
  )
  return Object.freeze({ fields: Object.freeze(fields), diagnostics: Object.freeze(diagnostics) })
}

const compareDiagnostics = (left: Diagnostic.Diagnostic, right: Diagnostic.Diagnostic): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

const collectTypeParameters = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  ownerName: string,
  ordinalOffset = 0,
  enclosing: ReadonlyArray<TypeParameterFact> = [],
): {
  readonly facts: ReadonlyArray<TypeParameterFact>
  readonly environment: ReadonlyMap<string, Type.Parameter>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const list = SyntaxTree.directNode(node, 'TypeParameterList')
  if (list === undefined) {
    return Object.freeze({
      facts: Object.freeze([]),
      environment: new Map(),
      diagnostics: Object.freeze([]),
    })
  }
  const environment = new Map<string, Type.Parameter>(
    enclosing.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const originals = new Map<string, SourceSpan.SourceSpan>(
    enclosing.flatMap((parameter) =>
      parameter.name._tag === 'Present'
        ? [[parameter.name.spelling, parameter.name.token.span] as const]
        : [],
    ),
  )
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const facts = SyntaxTree.directNodes(list, 'TypeParameter').map((parameterNode, ordinal) => {
    const name = presentName(source, parameterNode)
    // A bound is the one type node the parameter carries after its colon. Taking the node rather
    // than the parameter's own identifier keeps a damaged or applied bound from being read as a
    // bound on the parameter's own name.
    const boundNode = parameterNode.children.find(SyntaxTree.isNode)
    const boundResolution =
      boundNode === undefined ? undefined : analyzeDeclaredType(source, boundNode, environment)
    const effectBoundTarget =
      boundNode?.kind === 'AppliedType' ? SyntaxTree.directNode(boundNode, 'TypePath') : undefined
    const effectBoundSegments =
      effectBoundTarget === undefined
        ? []
        : SyntaxTree.tokens(effectBoundTarget).filter((token) => token.kind === 'Identifier')
    const effectBoundSegment = effectBoundSegments.at(0)
    const effectBound =
      effectBoundSegments.length === 1 &&
      effectBoundSegment !== undefined &&
      spelling(source, effectBoundSegment) === 'Effect'
    const representationKind: Type.ParameterKind | undefined =
      boundNode?.kind === 'CallableType'
        ? 'CallableRepresentation'
        : effectBound
          ? 'EffectRepresentation'
          : undefined
    const representationContract =
      boundResolution?.fact._tag === 'Resolved' &&
      (Type.isCallable(boundResolution.fact.type) || Type.isEffect(boundResolution.fact.type))
        ? boundResolution.fact.type
        : undefined
    if (representationKind !== undefined && boundResolution !== undefined)
      diagnostics.push(...boundResolution.diagnostics)
    const boundToken =
      boundNode === undefined
        ? undefined
        : SyntaxTree.tokens(boundNode).find((token) => token.kind === 'Identifier')
    const bound: BoundFact | undefined =
      representationKind !== undefined ||
      SyntaxTree.directToken(parameterNode, 'Colon') === undefined ||
      boundNode === undefined ||
      boundToken === undefined
        ? undefined
        : Object.freeze({
            _tag: 'UnresolvedBound' as const,
            spelling: spelling(source, boundToken),
            path: Object.freeze({
              _tag: 'TypePath' as const,
              spelling: spelling(source, boundToken),
              segments: Object.freeze([
                Object.freeze({ spelling: spelling(source, boundToken), token: boundToken }),
              ]),
              syntax: boundNode,
            }),
            application:
              boundResolution?.fact ??
              Object.freeze({ _tag: 'Unavailable' as const, syntax: boundNode }),
          })
    const duplicateOf = name._tag === 'Present' ? environment.get(name.spelling) : undefined
    const type =
      duplicateOf ??
      Type.parameter(
        { module: source.id, name: ownerName },
        ordinalOffset + ordinal,
        name._tag === 'Present' ? name.spelling : `#${ordinal}`,
        SyntaxTree.directToken(parameterNode, 'Bang') !== undefined
          ? 'FailureRow'
          : SyntaxTree.directToken(parameterNode, 'Question') !== undefined
            ? 'RequirementRow'
            : (representationKind ?? 'Value'),
        representationContract,
      )
    if (name._tag === 'Present' && duplicateOf === undefined) {
      environment.set(name.spelling, type)
      originals.set(name.spelling, name.token.span)
    } else if (name._tag === 'Present') {
      const originalSpan = originals.get(name.spelling)
      if (originalSpan !== undefined) {
        diagnostics.push(
          Diagnostic.duplicateTypeParameter(name.spelling, originalSpan, name.token.span),
        )
      }
    }
    return Object.freeze({
      _tag: 'TypeParameterDeclaration' as const,
      type,
      name,
      syntax: parameterNode,
      ...(duplicateOf === undefined ? {} : { duplicateOf }),
      ...(bound === undefined ? {} : { bound }),
      ...(representationKind === undefined ||
      boundNode === undefined ||
      boundResolution === undefined
        ? {}
        : {
            representationBound: Object.freeze({
              _tag: 'RepresentationBound' as const,
              kind:
                representationKind === 'CallableRepresentation'
                  ? ('Callable' as const)
                  : ('Effect' as const),
              contract: boundResolution.fact,
              syntax: boundNode,
            }),
          }),
    })
  })
  return Object.freeze({
    facts: Object.freeze(facts),
    environment,
    diagnostics: Object.freeze(diagnostics),
  })
}

const collectReturnType = (
  source: SourceFile.SourceFile,
  returnSyntax: SyntaxTree.Node,
  ownerName: string,
  typeParameters: ReadonlyArray<TypeParameterFact>,
): {
  readonly fact: ReturnTypeFact
  readonly opaqueResult?: OpaqueResultFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const syntax = declaredTypeNode(returnSyntax)
  if (syntax.kind !== 'OpaqueResultType') {
    const analyzed = analyzeDeclaredType(
      source,
      syntax,
      new Map(
        typeParameters.flatMap((parameter) =>
          parameter.name._tag === 'Present'
            ? [[parameter.name.spelling, parameter.type] as const]
            : [],
        ),
      ),
    )
    return Object.freeze({ fact: analyzed.fact, diagnostics: analyzed.diagnostics })
  }
  const collected = collectTypeParameters(
    source,
    syntax,
    ownerName,
    typeParameters.length,
    typeParameters,
  )
  const binder = collected.facts.at(0)
  const resultSyntax = syntax.children.find(isDeclaredTypeNode)
  if (binder === undefined || resultSyntax === undefined) {
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Unavailable', syntax }),
      diagnostics: collected.diagnostics,
    })
  }
  const analyzed = analyzeDeclaredType(source, resultSyntax, collected.environment)
  return Object.freeze({
    fact: analyzed.fact,
    opaqueResult: Object.freeze({
      _tag: 'OpaqueResult',
      binder,
      family: Object.freeze({
        _tag: 'OpaqueFamilyKey',
        producer: Object.freeze({ module: source.id, name: ownerName }),
        binderOrdinal: 0,
      }),
      publicSignature: Object.freeze({
        bound:
          binder.type.representationBound === undefined
            ? 'unavailable'
            : Type.key(binder.type.representationBound),
        result:
          analyzed.fact._tag === 'Resolved' ? Type.key(analyzed.fact.type) : analyzed.fact._tag,
        enclosingKinds: Object.freeze(typeParameters.map((parameter) => parameter.type.kind)),
      }),
      syntax,
    }),
    diagnostics: Object.freeze([...collected.diagnostics, ...analyzed.diagnostics]),
  })
}

const parameterAtTypePath = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
): Type.Parameter | undefined => {
  if (syntax.kind !== 'TypePath') return undefined
  const identifiers = SyntaxTree.tokens(syntax).filter((token) => token.kind === 'Identifier')
  const identifier = identifiers.at(0)
  return identifiers.length === 1 && identifier !== undefined
    ? typeParameters.get(spelling(source, identifier))
    : undefined
}

const collectFailureRow = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
): {
  readonly fact: FailureRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const syntax = SyntaxTree.directNode(node, 'FailureRow')
  if (syntax === undefined)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'FailureRow',
        members: Object.freeze([]),
        parameters: Object.freeze([]),
        failures: Object.freeze([]),
        available: true,
      }),
      diagnostics: Object.freeze([]),
    })
  const declared = declaredTypeNode(syntax)
  const syntaxMembers =
    declared.kind === 'UnionType'
      ? declared.children.filter(isDeclaredTypeNode)
      : Object.freeze([declared])
  const parameters: Array<Type.Parameter> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const members = syntaxMembers.flatMap((member): ReadonlyArray<DeclaredTypeFact> => {
    const parameter = parameterAtTypePath(source, member, typeParameters)
    if (parameter?.kind === 'FailureRow') {
      parameters.push(parameter)
      return []
    }
    if (parameter?.kind === 'RequirementRow') {
      const token = SyntaxTree.directToken(member, 'Identifier')
      if (token !== undefined)
        diagnostics.push(
          Diagnostic.genericParameterKindMismatch(
            spelling(source, token),
            'FailureRow',
            parameter.kind,
            token.span,
          ),
        )
      return []
    }
    const analyzed = analyzeDeclaredType(source, member, typeParameters)
    diagnostics.push(...analyzed.diagnostics)
    return [analyzed.fact]
  })
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FailureRow',
      members: Object.freeze(members),
      parameters: Object.freeze(parameters),
      failures: Object.freeze([]),
      syntax,
      available: false,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const collectRequirementRow = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  typeParameters: ReadonlyMap<string, Type.Parameter>,
): {
  readonly fact: RequirementRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const syntax = SyntaxTree.directNode(node, 'RequirementRow')
  if (syntax === undefined)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'RequirementRow',
        entries: Object.freeze([]),
        parameters: Object.freeze([]),
        requirements: Object.freeze([]),
        available: true,
      }),
      diagnostics: Object.freeze([]),
    })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const entries = SyntaxTree.directNodes(syntax, 'Requirement').map((requirement) => {
    const capabilitySyntax = requirement.children.find(isDeclaredTypeNode)
    const analyzed =
      capabilitySyntax === undefined
        ? Object.freeze({
            fact: Object.freeze({ _tag: 'Unavailable' as const, syntax: requirement }),
            diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
          })
        : analyzeDeclaredType(source, capabilitySyntax, typeParameters)
    diagnostics.push(...analyzed.diagnostics)
    const roleToken = SyntaxTree.directToken(requirement, 'Identifier')
    return Object.freeze({
      capability: analyzed.fact,
      role: roleToken === undefined ? 'DefaultRole' : spelling(source, roleToken),
      access:
        SyntaxTree.directToken(requirement, 'MutKeyword') === undefined
          ? ('Shared' as const)
          : ('Exclusive' as const),
      syntax: requirement,
    })
  })
  const parameters = SyntaxTree.directNodes(syntax, 'TypePath').flatMap(
    (path): ReadonlyArray<Type.Parameter> => {
      const token = SyntaxTree.directToken(path, 'Identifier')
      const parameter = parameterAtTypePath(source, path, typeParameters)
      if (parameter?.kind === 'RequirementRow') return [parameter]
      if (token !== undefined) {
        if (parameter === undefined)
          diagnostics.push(Diagnostic.unknownType(spelling(source, token), token.span))
        else
          diagnostics.push(
            Diagnostic.genericParameterKindMismatch(
              spelling(source, token),
              'RequirementRow',
              parameter.kind,
              token.span,
            ),
          )
      }
      return []
    },
  )
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'RequirementRow',
      entries: Object.freeze(entries),
      parameters: Object.freeze(parameters),
      requirements: Object.freeze([]),
      syntax,
      available: false,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const collectModule = (syntax: SyntaxFile.SyntaxFile): ModuleHeaders => {
  const source = syntax.source
  const nodes = syntax.root.children.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) &&
      (element.kind === 'FunctionDeclaration' ||
        element.kind === 'StructDeclaration' ||
        element.kind === 'ServiceDeclaration' ||
        element.kind === 'InterfaceDeclaration' ||
        element.kind === 'ConstantDeclaration'),
  )
  const first = new Map<string, { readonly id: CanonicalId; readonly token: Token.Token }>()
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const conformances = syntax.root.children
    .filter(
      (element): element is SyntaxTree.Node =>
        SyntaxTree.isNode(element) && element.kind === 'ImplDeclaration',
    )
    .map((node, ordinal): ConformanceFact => {
      const collected = collectTypeParameters(source, node, `impl#${ordinal}`)
      diagnostics.push(...collected.diagnostics)
      const environment = collected.environment
      const types = node.children.filter(isDeclaredTypeNode)
      const capabilitySyntax = types.at(0)
      const providerSyntax = types.at(1)
      const capability =
        capabilitySyntax === undefined
          ? Object.freeze({ _tag: 'Unavailable' as const, syntax: node })
          : analyzeDeclaredType(source, capabilitySyntax, environment).fact
      const provider =
        providerSyntax === undefined
          ? Object.freeze({ _tag: 'Unavailable' as const, syntax: node })
          : analyzeDeclaredType(source, providerSyntax, environment).fact
      // A binder's bound is re-analyzed here rather than reused from the parameter collection,
      // because a conditional requirement may name any binder the header declares — including the
      // one it bounds — and only the completed environment can resolve those occurrences.
      const requirements = collected.facts.flatMap(
        (parameter): ReadonlyArray<ConformanceRequirementFact> => {
          const bound = parameter.bound
          if (bound === undefined || parameter.duplicateOf !== undefined) return []
          return Object.freeze([
            Object.freeze({
              _tag: 'ConformanceRequirement' as const,
              parameter: parameter.type,
              spelling: bound.spelling,
              capability: analyzeDeclaredType(source, bound.path.syntax, environment).fact,
              syntax: bound.path.syntax,
            }),
          ])
        },
      )
      const operations = SyntaxTree.directNodes(node, 'ImplOperation').map((operation) => {
        const name = presentName(source, operation)
        const targetSyntax = SyntaxTree.directNode(operation, 'TypePath')
        const target =
          targetSyntax === undefined
            ? Object.freeze({ _tag: 'Unavailable' as const, syntax: operation })
            : (() => {
                const tokens = SyntaxTree.tokens(targetSyntax).filter(
                  (token) => token.kind === 'Identifier',
                )
                return tokens.length === 0
                  ? Object.freeze({ _tag: 'Unavailable' as const, syntax: targetSyntax })
                  : Object.freeze({
                      _tag: 'TypePath' as const,
                      spelling: tokens.map((token) => spelling(source, token)).join('.'),
                      segments: Object.freeze(
                        tokens.map((token) =>
                          Object.freeze({ spelling: spelling(source, token), token }),
                        ),
                      ),
                      syntax: targetSyntax,
                    })
              })()
        return Object.freeze({ name, target, syntax: operation })
      })
      const hookSyntax = SyntaxTree.directNode(node, 'FunctionDeclaration')
      const hook =
        hookSyntax === undefined
          ? undefined
          : (() => {
              const parameterList = SyntaxTree.directNode(hookSyntax, 'ParameterList')
              const parameters =
                parameterList === undefined
                  ? []
                  : SyntaxTree.directNodes(parameterList, 'ParameterDeclaration')
              const parameter = parameters.at(0)
              const parameterTypeSyntax =
                parameter === undefined
                  ? undefined
                  : parameter.children.find((element): element is SyntaxTree.Node =>
                      isDeclaredTypeNode(element),
                    )
              const returnSyntax = SyntaxTree.directNode(hookSyntax, 'ReturnType')
              const returnTypeSyntax = returnSyntax?.children.find(
                (element): element is SyntaxTree.Node => isDeclaredTypeNode(element),
              )
              const failure = collectFailureRow(source, hookSyntax, environment)
              const requirements = collectRequirementRow(source, hookSyntax, environment)
              const hookNameToken = SyntaxTree.directToken(hookSyntax, 'DropKeyword')
              diagnostics.push(...failure.diagnostics, ...requirements.diagnostics)
              return Object.freeze({
                _tag: 'DropHookDeclaration' as const,
                name:
                  hookNameToken === undefined
                    ? presentName(source, hookSyntax)
                    : Object.freeze({
                        _tag: 'Present' as const,
                        spelling: 'drop',
                        token: hookNameToken,
                      }),
                functionKind:
                  SyntaxTree.directToken(hookSyntax, 'EffectKeyword') === undefined
                    ? ('Ordinary' as const)
                    : ('Effect' as const),
                typeParameterCount:
                  SyntaxTree.directNode(hookSyntax, 'TypeParameterList') === undefined
                    ? 0
                    : SyntaxTree.directNodes(
                        childNode(hookSyntax, 'TypeParameterList'),
                        'TypeParameter',
                      ).length,
                parameterCount: parameters.length,
                parameterName:
                  parameter === undefined
                    ? Object.freeze({ _tag: 'Unavailable' as const, syntax: hookSyntax })
                    : presentName(source, parameter),
                parameterType:
                  parameterTypeSyntax === undefined
                    ? Object.freeze({
                        _tag: 'Unavailable' as const,
                        syntax: parameter ?? hookSyntax,
                      })
                    : analyzeDeclaredType(source, parameterTypeSyntax, environment).fact,
                returnType:
                  returnTypeSyntax === undefined
                    ? Object.freeze({
                        _tag: 'Unavailable' as const,
                        syntax: returnSyntax ?? hookSyntax,
                      })
                    : analyzeDeclaredType(source, returnTypeSyntax, environment).fact,
                failureRow: failure.fact,
                requirementRow: requirements.fact,
                syntax: hookSyntax,
              })
            })()
      return Object.freeze({
        _tag: 'ConformanceDeclaration',
        module: source.id,
        ordinal,
        typeParameters: collected.facts,
        requirements: Object.freeze(requirements),
        capability,
        provider,
        visibility: 'Public',
        operations: Object.freeze(operations),
        ...(hook === undefined ? {} : { hook }),
        // Coherence and termination are program-wide questions, so both stay unanswered until
        // every module's headers have resolved.
        coherence: Object.freeze({ _tag: 'Coherent' as const }),
        termination: Object.freeze({ _tag: 'UnavailableTermination' as const }),
        validity: Object.freeze({ _tag: 'UncheckedConformance' as const }),
        syntax: node,
      })
    })
  let nestedDeclarationOrdinal = nodes.length
  const ownMembers = nodes.map((node, ordinal): MemberFact => {
    const id: DeclarationId = Object.freeze({ _tag: 'DeclarationId', sourceId: source.id, ordinal })
    const name = presentName(source, node)
    let canonical: CanonicalState
    if (name._tag !== 'Present') canonical = Object.freeze({ _tag: 'Unidentified' })
    else {
      const original = first.get(name.spelling)
      if (original === undefined) {
        const canonicalId: CanonicalId = Object.freeze({
          _tag: 'CanonicalDeclarationId',
          module: source.id,
          name: name.spelling,
        })
        first.set(name.spelling, Object.freeze({ id: canonicalId, token: name.token }))
        canonical = Object.freeze({ _tag: 'Canonical', id: canonicalId })
      } else {
        const diagnostic = Diagnostic.duplicateDeclarationName(
          name.spelling,
          original.token.span,
          name.token.span,
        )
        diagnostics.push(diagnostic)
        canonical = Object.freeze({
          _tag: 'Duplicate',
          original: original.id,
          cause: Diagnostic.identity(diagnostic),
        })
      }
    }
    const visibility: 'Private' | 'Public' =
      SyntaxTree.directToken(node, 'PubKeyword') === undefined ? 'Private' : 'Public'
    const typeParameters = collectTypeParameters(
      source,
      node,
      name._tag === 'Present' ? name.spelling : `#${ordinal}`,
    )
    diagnostics.push(...typeParameters.diagnostics)
    if (node.kind === 'ConstantDeclaration') {
      const initializer =
        node.children.find(
          (element): element is SyntaxTree.Node =>
            SyntaxTree.isNode(element) && !isDeclaredTypeNode(element),
        ) ?? node
      const declaredType = analyzeDeclaredType(source, declaredTypeNode(node))
      diagnostics.push(...declaredType.diagnostics)
      return Object.freeze({
        _tag: 'ConstantDeclaration',
        id,
        canonical,
        visibility,
        typeParameters: Object.freeze([]),
        name,
        declaredType: declaredType.fact,
        literal: constantLiteral(source, initializer),
        initializer,
        syntax: node,
      })
    }
    if (node.kind === 'StructDeclaration') {
      const collected = collectFields(source, node, id, typeParameters.environment)
      diagnostics.push(...collected.diagnostics)
      return Object.freeze({
        _tag: 'StructDeclaration',
        id,
        canonical,
        visibility,
        typeParameters: typeParameters.facts,
        name,
        fields: collected.fields,
        dependency: Object.freeze({ _tag: 'Available', types: Object.freeze([]) }),
        syntax: node,
      })
    }
    if (node.kind === 'ServiceDeclaration' || node.kind === 'InterfaceDeclaration') {
      const operationFirst = new Map<
        string,
        { readonly id: ServiceOperationId; readonly token: Token.Token }
      >()
      const operations = SyntaxTree.directNodes(node, 'ServiceOperation').map(
        (operation, operationOrdinal): ServiceOperationFact => {
          const operationId: DeclarationId = Object.freeze({
            _tag: 'DeclarationId',
            sourceId: source.id,
            ordinal: nestedDeclarationOrdinal,
          })
          nestedDeclarationOrdinal += 1
          const operationName = presentName(source, operation)
          let operationState: ServiceOperationState
          if (operationName._tag !== 'Present') {
            operationState = Object.freeze({ _tag: 'Unidentified' })
          } else {
            const original = operationFirst.get(operationName.spelling)
            if (original === undefined) {
              const serviceOperationId: ServiceOperationId = Object.freeze({
                _tag: 'ServiceOperationId',
                service: id,
                name: operationName.spelling,
              })
              operationFirst.set(
                operationName.spelling,
                Object.freeze({ id: serviceOperationId, token: operationName.token }),
              )
              operationState = Object.freeze({ _tag: 'Unique', id: serviceOperationId })
            } else {
              const diagnostic = Diagnostic.duplicateDeclarationName(
                operationName.spelling,
                original.token.span,
                operationName.token.span,
              )
              diagnostics.push(diagnostic)
              operationState = Object.freeze({
                _tag: 'Duplicate',
                original: original.id,
                cause: Diagnostic.identity(diagnostic),
              })
            }
          }
          const operationTypeParameters = collectTypeParameters(
            source,
            operation,
            `${name._tag === 'Present' ? name.spelling : `#${ordinal}`}.$${operationOrdinal}`,
          )
          diagnostics.push(...operationTypeParameters.diagnostics)
          const environment = new Map<string, Type.Parameter>([
            ...typeParameters.environment,
            ...operationTypeParameters.environment,
          ])
          const parameterList = childNode(operation, 'ParameterList')
          const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
            (parameter, parameterOrdinal) =>
              analyzeParameter(source, parameter, operationId, parameterOrdinal, environment),
          )
          const returnSyntax = SyntaxTree.directNode(operation, 'ReturnType')
          const returnType: {
            readonly fact: ReturnTypeFact
            readonly opaqueResult?: OpaqueResultFact
            readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
          } =
            returnSyntax === undefined
              ? (() => {
                  const token = SyntaxTree.directToken(parameterList, 'RightParenthesis')
                  if (token === undefined)
                    return Object.freeze({
                      fact: Object.freeze({
                        _tag: 'Unavailable' as const,
                        syntax: parameterList,
                      }),
                      diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                    })
                  return Object.freeze({
                    fact: Object.freeze({
                      _tag: 'Resolved' as const,
                      type: Type.unit,
                      spelling: '()',
                      token,
                      syntax: parameterList,
                    }),
                    diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
                  })
                })()
              : collectReturnType(
                  source,
                  returnSyntax,
                  `${name._tag === 'Present' ? name.spelling : `#${ordinal}`}.$${operationOrdinal}`,
                  [...typeParameters.facts, ...operationTypeParameters.facts],
                )
          const failureRow = collectFailureRow(source, operation, environment)
          const requirementRow = collectRequirementRow(source, operation, environment)
          const body = SyntaxTree.directNode(operation, 'Block')
          const parameterFacts = Object.freeze(parameters.map((parameter) => parameter.fact))
          diagnostics.push(
            ...parameters.flatMap((parameter) => parameter.diagnostics),
            ...duplicateParameterDiagnostics(parameterFacts),
            ...returnType.diagnostics,
            ...failureRow.diagnostics,
            ...requirementRow.diagnostics,
          )
          if (body !== undefined)
            diagnostics.push(
              Diagnostic.invalidServiceDeclaration(
                'service operations declare contracts and cannot contain bodies',
                body.span,
              ),
            )
          if (
            SyntaxTree.directToken(operation, 'EffectKeyword') === undefined &&
            failureRow.fact.syntax !== undefined
          )
            diagnostics.push(Diagnostic.failureRowOnOrdinary(failureRow.fact.syntax.span))
          return Object.freeze({
            _tag: 'ServiceOperation',
            id: operationId,
            state: operationState,
            functionKind:
              SyntaxTree.directToken(operation, 'EffectKeyword') === undefined
                ? 'Ordinary'
                : 'Effect',
            typeParameters: operationTypeParameters.facts,
            parameterCount: parameterFacts.length,
            parameters: parameterFacts,
            name: operationName,
            returnType: returnType.fact,
            ...(returnType.opaqueResult === undefined
              ? {}
              : { opaqueResult: returnType.opaqueResult }),
            failureRow: failureRow.fact,
            requirementRow: requirementRow.fact,
            syntax: operation,
          })
        },
      )
      const shared = {
        id,
        canonical,
        visibility,
        typeParameters: typeParameters.facts,
        name,
        operations: Object.freeze(operations),
        syntax: node,
      }
      return node.kind === 'InterfaceDeclaration'
        ? Object.freeze({
            _tag: 'InterfaceDeclaration',
            ...shared,
            operationContracts: interfaceOperationContracts(typeParameters.facts, operations),
          })
        : Object.freeze({ _tag: 'ServiceDeclaration', ...shared })
    }
    const parameterList = childNode(node, 'ParameterList')
    const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
      (parameter, parameterOrdinal) =>
        analyzeParameter(source, parameter, id, parameterOrdinal, typeParameters.environment),
    )
    const returnSyntax = SyntaxTree.directNode(node, 'ReturnType')
    const returnType: {
      readonly fact: ReturnTypeFact
      readonly opaqueResult?: OpaqueResultFact
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
    } =
      returnSyntax === undefined
        ? (() => {
            const parameterList = childNode(node, 'ParameterList')
            const token = SyntaxTree.directToken(parameterList, 'RightParenthesis')
            if (token === undefined)
              return Object.freeze({
                fact: Object.freeze({
                  _tag: 'Unavailable' as const,
                  syntax: parameterList,
                }),
                diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
              })
            return Object.freeze({
              fact: Object.freeze({
                _tag: 'Resolved' as const,
                type: Type.unit,
                spelling: '()',
                token,
                syntax: parameterList,
              }),
              diagnostics: Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([]),
            })
          })()
        : collectReturnType(
            source,
            returnSyntax,
            name._tag === 'Present' ? name.spelling : `#${ordinal}`,
            typeParameters.facts,
          )
    const functionKind =
      SyntaxTree.directToken(node, 'EffectKeyword') === undefined ? 'Ordinary' : 'Effect'
    const failureRow = collectFailureRow(source, node, typeParameters.environment)
    const requirementRow = collectRequirementRow(source, node, typeParameters.environment)
    const facts = Object.freeze(parameters.map((parameter) => parameter.fact))
    diagnostics.push(
      ...parameters.flatMap((parameter) => parameter.diagnostics),
      ...duplicateParameterDiagnostics(facts),
      ...returnType.diagnostics,
      ...failureRow.diagnostics,
      ...requirementRow.diagnostics,
    )
    if (functionKind === 'Ordinary' && failureRow.fact.syntax !== undefined)
      diagnostics.push(Diagnostic.failureRowOnOrdinary(failureRow.fact.syntax.span))
    return Object.freeze({
      _tag: 'FunctionDeclaration',
      id,
      canonical,
      visibility,
      functionKind,
      typeParameters: typeParameters.facts,
      parameterCount: facts.length,
      parameters: facts,
      name,
      returnType: returnType.fact,
      ...(returnType.opaqueResult === undefined ? {} : { opaqueResult: returnType.opaqueResult }),
      failureRow: failureRow.fact,
      requirementRow: requirementRow.fact,
      syntax: node,
    })
  })
  // Drop hook bodies elaborate as hidden generic functions: each accepted hook joins the member
  // list under a non-identifier canonical name, carrying the impl's type parameters, so ordinary
  // elaboration, ownership, and lowering machinery compile it without a hook-shaped special case.
  const hookMembers = conformances.flatMap((conformance, hookIndex): ReadonlyArray<MemberFact> => {
    const hook = conformance.hook
    if (hook === undefined) return []
    const node = hook.syntax
    const id: DeclarationId = Object.freeze({
      _tag: 'DeclarationId',
      sourceId: source.id,
      ordinal: nestedDeclarationOrdinal + hookIndex,
    })
    const environment = new Map<string, Type.Parameter>(
      conformance.typeParameters.flatMap((parameter) =>
        parameter.duplicateOf === undefined && parameter.name._tag === 'Present'
          ? [[parameter.name.spelling, parameter.type] as const]
          : [],
      ),
    )
    const parameterList = childNode(node, 'ParameterList')
    const parameters = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration').map(
      (parameter, parameterOrdinal) =>
        analyzeParameter(source, parameter, id, parameterOrdinal, environment),
    )
    const returnType = analyzeDeclaredType(
      source,
      declaredTypeNode(childNode(node, 'ReturnType')),
      environment,
    )
    const facts = Object.freeze(parameters.map((parameter) => parameter.fact))
    return [
      Object.freeze({
        _tag: 'FunctionDeclaration' as const,
        id,
        canonical: Object.freeze({
          _tag: 'Canonical' as const,
          id: Object.freeze({
            _tag: 'CanonicalDeclarationId' as const,
            module: source.id,
            name: `drop@impl#${conformance.ordinal}`,
          }),
        }),
        visibility: 'Private' as const,
        functionKind: 'Ordinary' as const,
        typeParameters: conformance.typeParameters,
        parameterCount: facts.length,
        parameters: facts,
        name: hook.name,
        returnType: returnType.fact,
        failureRow: hook.failureRow,
        requirementRow: hook.requirementRow,
        syntax: node,
      }),
    ]
  })
  const members = [...ownMembers, ...hookMembers]
  return Object.freeze({
    _tag: 'ModuleHeaders',
    module: source.id,
    members: Object.freeze(members),
    declarations: Object.freeze(
      members.filter((member): member is DeclarationFact => member._tag === 'FunctionDeclaration'),
    ),
    structs: Object.freeze(
      members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
    ),
    services: Object.freeze(
      members.filter((member): member is ServiceFact => member._tag === 'ServiceDeclaration'),
    ),
    interfaces: Object.freeze(
      members.filter((member): member is InterfaceFact => member._tag === 'InterfaceDeclaration'),
    ),
    constants: Object.freeze(
      members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
    ),
    conformances: Object.freeze(conformances),
    diagnostics: Object.freeze(diagnostics.sort(compareDiagnostics)),
  })
}

/** Collects identities and raw type paths for the complete closure before scope resolution. */
export const collect = (closure: ModuleClosure.Facts): Index => {
  const modules = Object.freeze(closure.modules.map((module) => collectModule(module.syntax)))
  return Object.freeze({
    _tag: 'DeclarationIndex',
    stage: 'Collected',
    modules,
    diagnostics: Diagnostic.merge(...modules.map((module) => module.diagnostics)),
  })
}

/**
 * Resolves one `typeof` item to the exact representation of a named callable declaration.
 *
 * The item must resolve to exactly one callable declaration whose generic parameters are all
 * supplied, because an exact representation names one construction, not a family. The resulting
 * identity is built from the declaration's canonical module and name plus its canonical argument
 * keys, so it never depends on spelling, span, or source path.
 */
const resolveExactRepresentation = (
  module: string,
  fact: Extract<DeclaredTypeFact, { readonly _tag: 'ExactRepresentation' }>,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): TypeResolution => {
  const arguments_ = fact.arguments.map((argument) =>
    resolveDeclaredType(module, argument, resolvers, modules),
  )
  const argumentDiagnostics = arguments_.flatMap((argument) => argument.diagnostics)
  const reject = (diagnostic: Diagnostic.Diagnostic, candidate?: MemberFact): TypeResolution => {
    const canonical = candidate?.canonical._tag === 'Canonical' ? candidate.canonical.id : undefined
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        cause: Diagnostic.identity(diagnostic),
        ...(canonical === undefined ? {} : { itemCandidate: canonical }),
      }),
      diagnostics: Object.freeze([...argumentDiagnostics, diagnostic]),
    })
  }
  const unresolved = () =>
    Diagnostic.unresolvedExactRepresentationItem(fact.item.spelling, fact.token.span)
  const open = (expected: number, actual = arguments_.length) =>
    Diagnostic.openExactRepresentationItem(fact.item.spelling, expected, actual, fact.token.span)
  const lookup = resolvers.item(module, fact.item)
  if (lookup._tag === 'Ambiguous')
    return reject(
      Diagnostic.ambiguousExactRepresentationItem(
        fact.item.spelling,
        lookup.count,
        fact.token.span,
      ),
    )
  if (lookup._tag !== 'Resolved')
    return reject(
      unresolved(),
      lookup._tag === 'Inaccessible' || lookup._tag === 'Unavailable'
        ? lookup.declaration
        : undefined,
    )
  const declaration = lookup.declaration
  if (declaration._tag !== 'FunctionDeclaration' || declaration.functionKind !== 'Ordinary')
    return reject(
      Diagnostic.uncallableExactRepresentationItem(
        fact.item.spelling,
        declaration._tag === 'FunctionDeclaration' ? 'EffectDeclaration' : 'NonCallableDeclaration',
        fact.token.span,
      ),
      declaration,
    )
  if (declaration.typeParameters.length !== arguments_.length)
    return reject(open(declaration.typeParameters.length), declaration)
  const supplied = arguments_.map((argument, ordinal) =>
    argument.fact._tag === 'Resolved'
      ? genericArgumentForParameter(
          declaration.typeParameters.at(ordinal)?.type,
          argument.fact.type,
        )
      : undefined,
  )
  if (supplied.some((argument) => argument === undefined))
    return reject(open(declaration.typeParameters.length), declaration)
  const concrete = supplied.filter(
    (argument): argument is Type.GenericArgument => argument !== undefined,
  )
  const concreteCount = concrete.filter(Type.isConcreteGenericArgument).length
  if (concreteCount !== concrete.length)
    return reject(open(declaration.typeParameters.length, concreteCount), declaration)
  const substitution = Type.substitution(
    declaration.typeParameters.map((parameter) => parameter.type),
    concrete,
  )
  if (substitution === undefined)
    return reject(open(declaration.typeParameters.length), declaration)
  const canonical =
    declaration.canonical._tag === 'Canonical' ? declaration.canonical.id : undefined
  if (canonical === undefined) return reject(unresolved(), declaration)
  const declaredReturn = resolveDeclaredType(
    canonical.module,
    declaration.returnType,
    resolvers,
    modules,
  )
  if (declaredReturn.fact._tag !== 'Resolved') return reject(unresolved(), declaration)
  const declaredParameters = declaration.parameters.map(
    (parameter) =>
      resolveDeclaredType(canonical.module, parameter.declaredType, resolvers, modules).fact,
  )
  if (declaredParameters.some((parameter) => parameter._tag !== 'Resolved'))
    return reject(unresolved(), declaration)
  const structural = Type.callable(
    declaredParameters.flatMap((parameter) =>
      parameter._tag === 'Resolved' ? [Type.substitute(parameter.type, substitution)] : [],
    ),
    Type.substitute(declaredReturn.fact.type, substitution),
  )
  const identity = Type.callableIdentityArgument(
    `declaration:${canonical.module}:${canonical.name}`,
    Object.freeze({ _tag: 'Declaration', module: canonical.module, name: canonical.name }),
    concrete,
  )
  const type = Type.represented(
    structural,
    structural,
    Type.exactRepresentationArgument(identity, structural),
  )
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Resolved',
      type,
      spelling: fact.spelling,
      token: fact.token,
      syntax: fact.syntax,
      components: Object.freeze(arguments_.map((argument) => argument.fact)),
      exactItem: Object.freeze({ path: fact.item, declaration: canonical }),
    }),
    diagnostics: Object.freeze(argumentDiagnostics),
  })
}

const resolveDeclaredType = (
  module: string,
  fact: DeclaredTypeFact,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): TypeResolution => {
  if (fact._tag === 'RepresentationParameter') {
    const parameter =
      resolvers.representationBindings?.get(Type.key(fact.parameter)) ?? fact.parameter
    const bound = parameter.representationBound
    if (bound === undefined) return Object.freeze({ fact, diagnostics: Object.freeze([]) })
    const type = Type.represented(bound, bound, Type.representationParameterArgument(parameter))
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Resolved',
        type,
        spelling: fact.spelling,
        token: fact.token,
        syntax: fact.syntax,
        path: fact.path,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (fact._tag === 'ExactRepresentation')
    return resolveExactRepresentation(module, fact, resolvers, modules)
  if (fact._tag === 'Unresolved') {
    const resolved = resolvers.type(module, fact.path)
    if (resolved.fact._tag !== 'Resolved' || !Type.isNominal(resolved.fact.type)) return resolved
    const declaration = memberByNominal(modules, resolved.fact.type)
    const expected =
      declaration?.typeParameters.length ?? Type.intrinsicNominalArity(resolved.fact.type)
    if (expected === 0) return resolved
    const diagnostic = Diagnostic.typeArgumentArity(fact.spelling, expected, 0, fact.token.span)
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        cause: Diagnostic.identity(diagnostic),
        candidate: resolved.fact.type,
      }),
      diagnostics: Object.freeze([diagnostic]),
    })
  }
  if (fact._tag === 'Callable') {
    const parameters = fact.parameters.map((parameter) =>
      resolveDeclaredType(module, parameter, resolvers, modules),
    )
    const result = resolveDeclaredType(module, fact.result, resolvers, modules)
    const diagnostics = Object.freeze([
      ...parameters.flatMap((parameter) => parameter.diagnostics),
      ...result.diagnostics,
    ])
    if (
      result.fact._tag === 'Resolved' &&
      parameters.every((parameter) => parameter.fact._tag === 'Resolved')
    ) {
      const type = Type.callable(
        parameters.flatMap((parameter) =>
          parameter.fact._tag === 'Resolved' ? [parameter.fact.type] : [],
        ),
        result.fact.type,
        fact.mode,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([
            ...parameters.map((parameter) => parameter.fact),
            result.fact,
          ]),
        }),
        diagnostics,
      })
    }
    const resolvedFacts = [...parameters.map((parameter) => parameter.fact), result.fact]
    const cause = resolvedFacts
      .flatMap((resolved) =>
        'cause' in resolved && resolved.cause !== undefined ? [resolved.cause] : [],
      )
      .at(-1)
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        parameters: Object.freeze(parameters.map((parameter) => parameter.fact)),
        result: result.fact,
        ...(cause === undefined ? {} : { cause }),
      }),
      diagnostics,
    })
  }
  if (fact._tag === 'Effect') {
    const success = resolveDeclaredType(module, fact.success, resolvers, modules)
    const failures = fact.failures.map((failure) =>
      resolveDeclaredType(module, failure, resolvers, modules),
    )
    const requirements = fact.requirements.map((requirement) =>
      Object.freeze({
        ...requirement,
        capability: resolveDeclaredType(module, requirement.capability, resolvers, modules),
      }),
    )
    const diagnostics: Array<Diagnostic.Diagnostic> = [
      ...success.diagnostics,
      ...failures.flatMap((failure) => failure.diagnostics),
      ...requirements.flatMap((requirement) => requirement.capability.diagnostics),
    ]
    const failureTypes: Array<Type.Nominal> = []
    let failuresAvailable = true
    for (const failure of failures) {
      if (failure.fact._tag === 'Resolved' && Type.isNominal(failure.fact.type)) {
        failureTypes.push(failure.fact.type)
      } else if (!(failure.fact._tag === 'Resolved' && Type.isNever(failure.fact.type))) {
        failuresAvailable = false
        if (failure.fact._tag === 'Resolved')
          diagnostics.push(
            Diagnostic.invalidFailureType(Type.encode(failure.fact.type), failure.fact.syntax.span),
          )
      }
    }
    const requirementTypes: Array<Type.Requirement> = []
    let requirementsAvailable = true
    for (const requirement of requirements) {
      if (
        requirement.capability.fact._tag === 'Resolved' &&
        (Type.isNominal(requirement.capability.fact.type) ||
          (Type.isParameter(requirement.capability.fact.type) &&
            requirement.capability.fact.type.kind === 'Value'))
      ) {
        requirementTypes.push(
          Object.freeze({
            capability: requirement.capability.fact.type,
            role: requirement.role,
            access: requirement.access,
          }),
        )
      } else {
        requirementsAvailable = false
        if (requirement.capability.fact._tag === 'Resolved')
          diagnostics.push(
            Diagnostic.invalidRequirementType(
              Type.encode(requirement.capability.fact.type),
              requirement.syntax.span,
            ),
          )
      }
    }
    if (success.fact._tag === 'Resolved' && failuresAvailable && requirementsAvailable) {
      const type = Type.effect(
        success.fact.type,
        failureTypes,
        fact.access,
        requirementTypes,
        fact.failureParameters,
        fact.requirementParameters,
      )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([
            success.fact,
            ...failures.map((failure) => failure.fact),
            ...requirements.map((requirement) => requirement.capability.fact),
          ]),
        }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    const cause = diagnostics.at(-1)
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        success: success.fact,
        failures: Object.freeze(failures.map((failure) => failure.fact)),
        requirements: Object.freeze(
          requirements.map((requirement) =>
            Object.freeze({ ...requirement, capability: requirement.capability.fact }),
          ),
        ),
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (fact._tag === 'Applied') {
    const target =
      fact.target._tag === 'Unresolved'
        ? resolvers.type(module, fact.target.path)
        : resolveDeclaredType(module, fact.target, resolvers, modules)
    const arguments_ = fact.arguments.map((argument) =>
      resolveDeclaredType(module, argument, resolvers, modules),
    )
    const failures =
      fact.failureRow?.failures.map((failure) =>
        resolveDeclaredType(module, failure, resolvers, modules),
      ) ?? []
    const requirements =
      fact.requirementRow?.requirements.map((requirement) =>
        Object.freeze({
          ...requirement,
          capability: resolveDeclaredType(module, requirement.capability, resolvers, modules),
        }),
      ) ?? []
    const diagnostics = [
      ...target.diagnostics,
      ...arguments_.flatMap((argument) => argument.diagnostics),
      ...failures.flatMap((failure) => failure.diagnostics),
      ...requirements.flatMap((requirement) => requirement.capability.diagnostics),
    ]
    if (target.fact._tag === 'Resolved' && Type.isNominal(target.fact.type)) {
      const declaration = memberByNominal(modules, target.fact.type)
      const expected =
        declaration?.typeParameters.length ?? Type.intrinsicNominalArity(target.fact.type)
      const declaredParameters = declaration?.typeParameters.map((parameter) => parameter.type)
      const valueArguments = arguments_.map(
        (argument, ordinal): Type.GenericArgument | undefined => {
          if (argument.fact._tag !== 'Resolved') return undefined
          return genericArgumentForParameter(declaredParameters?.at(ordinal), argument.fact.type)
        },
      )
      const failureTypes = failures.flatMap(
        (failure): ReadonlyArray<Type.Nominal> =>
          failure.fact._tag === 'Resolved' && Type.isNominal(failure.fact.type)
            ? [failure.fact.type]
            : [],
      )
      const failuresAvailable = failures.every(
        (failure) =>
          failure.fact._tag === 'Resolved' &&
          (Type.isNominal(failure.fact.type) || Type.isNever(failure.fact.type)),
      )
      const requirementTypes = requirements.flatMap(
        (requirement): ReadonlyArray<Type.Requirement> =>
          requirement.capability.fact._tag === 'Resolved' &&
          (Type.isNominal(requirement.capability.fact.type) ||
            (Type.isParameter(requirement.capability.fact.type) &&
              requirement.capability.fact.type.kind === 'Value'))
            ? [
                Object.freeze({
                  capability: requirement.capability.fact.type,
                  role: requirement.role,
                  access: requirement.access,
                }),
              ]
            : [],
      )
      const requirementsAvailable = requirementTypes.length === requirements.length
      const rowArguments: ReadonlyArray<Type.GenericArgument | undefined> = Object.freeze([
        ...(fact.failureRow === undefined
          ? []
          : [
              failuresAvailable
                ? Type.failureRowArgument(failureTypes, fact.failureRow.parameters)
                : undefined,
            ]),
        ...(fact.requirementRow === undefined
          ? []
          : [
              requirementsAvailable
                ? Type.requirementRowArgument(requirementTypes, fact.requirementRow.parameters)
                : undefined,
            ]),
      ])
      const available = Object.freeze([...valueArguments, ...rowArguments])
      const suppliedCount = available.length
      if (expected === suppliedCount && available.every((argument) => argument !== undefined)) {
        const concrete = available.filter(
          (argument): argument is Type.GenericArgument => argument !== undefined,
        )
        const substitution =
          declaredParameters === undefined
            ? concrete.every(Type.isTypeArgument)
              ? new Map<string, Type.GenericArgument>()
              : undefined
            : Type.substitution(declaredParameters, concrete)
        if (substitution === undefined) {
          const incompatibleBound = concrete.findIndex((argument, ordinal) => {
            const parameter = declaredParameters?.at(ordinal)
            if (
              parameter === undefined ||
              (parameter.kind !== 'CallableRepresentation' &&
                parameter.kind !== 'EffectRepresentation') ||
              !Type.isRepresentationArgument(argument) ||
              Type.representationArgumentKind(argument) !== parameter.kind ||
              parameter.representationBound === undefined
            )
              return false
            const prior = Type.prefixSubstitution(
              declaredParameters?.slice(0, ordinal) ?? [],
              concrete.slice(0, ordinal),
            )
            if (prior === undefined) return false
            const required = Type.substitute(parameter.representationBound, prior)
            const actual =
              argument._tag === 'RepresentationParameterArgument'
                ? argument.parameter.representationBound
                : argument.contract
            return (
              actual !== undefined &&
              (Type.isCallable(required) || Type.isEffect(required)) &&
              Type.representationAdmissibility(actual, required)._tag === 'Unavailable'
            )
          })
          const incompatibleParameter =
            incompatibleBound < 0 ? undefined : declaredParameters?.at(incompatibleBound)
          const incompatibleArgument =
            incompatibleBound < 0 ? undefined : concrete.at(incompatibleBound)
          const incompatibleSupplied =
            incompatibleBound < 0 ? undefined : arguments_.at(incompatibleBound)
          if (
            incompatibleParameter !== undefined &&
            incompatibleParameter.representationBound !== undefined &&
            incompatibleArgument !== undefined &&
            Type.isRepresentationArgument(incompatibleArgument) &&
            incompatibleSupplied !== undefined
          ) {
            const prior = Type.prefixSubstitution(
              declaredParameters?.slice(0, incompatibleBound) ?? [],
              concrete.slice(0, incompatibleBound),
            )
            const required =
              prior === undefined
                ? incompatibleParameter.representationBound
                : Type.substitute(incompatibleParameter.representationBound, prior)
            const actual =
              incompatibleArgument._tag === 'RepresentationParameterArgument'
                ? incompatibleArgument.parameter.representationBound
                : incompatibleArgument.contract
            const actualParameter =
              incompatibleArgument._tag === 'RepresentationParameterArgument'
                ? modules
                    .flatMap((candidateModule) => candidateModule.members)
                    .flatMap((member) => ('typeParameters' in member ? member.typeParameters : []))
                    .find(
                      (candidateParameter) =>
                        Type.key(candidateParameter.type) ===
                        Type.key(incompatibleArgument.parameter),
                    )
                : undefined
            const requiredParameter = declaration?.typeParameters.at(incompatibleBound)
            if ((Type.isCallable(required) || Type.isEffect(required)) && actual !== undefined)
              diagnostics.push(
                Diagnostic.incompatibleRepresentationBound(
                  incompatibleParameter.name,
                  Type.encode(required),
                  Type.encode(actual),
                  incompatibleSupplied.fact.syntax.span,
                  {
                    ...(requiredParameter === undefined
                      ? {}
                      : { requiredDeclarationSpan: requiredParameter.syntax.span }),
                    ...(actualParameter === undefined
                      ? {}
                      : { actualDeclarationSpan: actualParameter.syntax.span }),
                  },
                ),
              )
          }
          const mismatch = concrete.findIndex((argument, ordinal) => {
            const parameter = declaredParameters?.at(ordinal)
            if (parameter === undefined) return false
            if (parameter.kind === 'Value') return !Type.isTypeArgument(argument)
            if (parameter.kind === 'FailureRow') return !Type.isFailureRowArgument(argument)
            if (parameter.kind === 'RequirementRow') return !Type.isRequirementRowArgument(argument)
            return (
              !Type.isRepresentationArgument(argument) ||
              Type.representationArgumentKind(argument) !== parameter.kind
            )
          })
          const parameter = declaredParameters?.at(mismatch)
          const supplied = arguments_.at(mismatch)
          if (incompatibleBound < 0 && parameter !== undefined && supplied !== undefined) {
            diagnostics.push(
              Diagnostic.genericParameterKindMismatch(
                parameter.name,
                parameter.kind,
                supplied.fact._tag === 'Resolved' && Type.isRepresented(supplied.fact.type)
                  ? supplied.fact.type.contract._tag === 'CallableType'
                    ? 'CallableRepresentation'
                    : 'EffectRepresentation'
                  : supplied.fact._tag === 'Resolved' &&
                      Type.isParameter(supplied.fact.type) &&
                      (supplied.fact.type.kind === 'FailureRow' ||
                        supplied.fact.type.kind === 'RequirementRow')
                    ? supplied.fact.type.kind
                    : 'Value',
                supplied.fact.syntax.span,
              ),
            )
          }
          const causeDiagnostic = diagnostics.at(-1)
          return Object.freeze({
            fact: Object.freeze({
              ...fact,
              ...(causeDiagnostic === undefined
                ? {}
                : { cause: Diagnostic.identity(causeDiagnostic) }),
            }),
            diagnostics: Object.freeze(diagnostics),
          })
        }
        const firstConcrete = concrete.at(0)
        const type =
          target.fact.type.module === 'silk/option' &&
          target.fact.type.name === 'Option' &&
          concrete.length === 1 &&
          firstConcrete !== undefined &&
          Type.isTypeArgument(firstConcrete)
            ? Type.option(firstConcrete)
            : Type.nominal(target.fact.type.module, target.fact.type.name, concrete)
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            type,
            spelling: Type.encode(type),
            token: fact.token,
            syntax: fact.syntax,
            components: Object.freeze([
              target.fact,
              ...arguments_.map((argument) => argument.fact),
              ...failures.map((failure) => failure.fact),
              ...requirements.map((requirement) => requirement.capability.fact),
            ]),
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      if (expected === suppliedCount) {
        const unavailable = [
          ...arguments_,
          ...failures,
          ...requirements.map((requirement) => requirement.capability),
        ].find((argument) => argument.fact._tag !== 'Resolved')
        const cause =
          unavailable !== undefined && 'cause' in unavailable.fact
            ? unavailable.fact.cause
            : undefined
        return Object.freeze({
          fact: Object.freeze({ ...fact, ...(cause === undefined ? {} : { cause }) }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      const diagnostic = Diagnostic.typeArgumentArity(
        fact.spelling,
        expected,
        suppliedCount,
        fact.token.span,
      )
      diagnostics.push(diagnostic)
      return Object.freeze({
        fact: Object.freeze({ ...fact, cause: Diagnostic.identity(diagnostic) }),
        diagnostics: Object.freeze(diagnostics),
      })
    }
    return Object.freeze({ fact, diagnostics: Object.freeze(diagnostics) })
  }
  if (fact._tag === 'Union') {
    const resolvedMembers = fact.members.map((member) =>
      resolveDeclaredType(module, member, resolvers, modules),
    )
    const diagnostics: Array<Diagnostic.Diagnostic> = resolvedMembers.flatMap((member) =>
      Array.from(member.diagnostics),
    )
    const members = Object.freeze(resolvedMembers.map((member) => member.fact))
    if (members.every((member) => member._tag === 'Resolved')) {
      const available = members.filter(
        (member): member is Extract<DeclaredTypeFact, { readonly _tag: 'Resolved' }> =>
          member._tag === 'Resolved',
      )
      const normalized = Type.union(available.map((member) => member.type))
      if (normalized._tag === 'Normalized') {
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved' as const,
            type: normalized.type,
            spelling: Type.encode(normalized.type),
            token: fact.token,
            syntax: fact.syntax,
            unionSource: Object.freeze({
              _tag: 'UnionSource' as const,
              members,
              separators: fact.separators,
              syntax: fact.syntax,
            }),
          }),
          diagnostics: Object.freeze(diagnostics),
        })
      }
      for (const invalid of normalized.members) {
        const sourceFact = available.find((member) => Type.equals(member.type, invalid))
        diagnostics.push(
          Diagnostic.invalidUnionMember(
            Type.encode(invalid),
            sourceFact?.syntax.span ?? fact.syntax.span,
          ),
        )
      }
    }
    const cause = diagnostics.at(-1)
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        members,
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      diagnostics: Object.freeze(diagnostics),
    })
  }
  if (fact._tag === 'Slice') {
    const element = resolveDeclaredType(module, fact.element, resolvers, modules)
    if (element.fact._tag === 'Resolved') {
      const type = Type.slice(fact.access, element.fact.type)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([element.fact]),
          ...(element.fact.exposureCause === undefined
            ? {}
            : { exposureCause: element.fact.exposureCause }),
        }),
        diagnostics: element.diagnostics,
      })
    }
    const cause = 'cause' in element.fact ? element.fact.cause : undefined
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        element: element.fact,
        ...(cause === undefined ? {} : { cause }),
      }),
      diagnostics: element.diagnostics,
    })
  }
  if (fact._tag === 'Reference') {
    const target = resolveDeclaredType(module, fact.target, resolvers, modules)
    if (target.fact._tag === 'Resolved') {
      const type = Type.reference(fact.access, target.fact.type)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([target.fact]),
        }),
        diagnostics: target.diagnostics,
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        ...fact,
        target: target.fact,
        ...('cause' in target.fact && target.fact.cause !== undefined
          ? { cause: target.fact.cause }
          : {}),
      }),
      diagnostics: target.diagnostics,
    })
  }
  if (fact._tag !== 'FixedArray') return Object.freeze({ fact, diagnostics: Object.freeze([]) })
  return (() => {
    const element = resolveDeclaredType(module, fact.element, resolvers, modules)
    if (fact.length._tag !== 'Available') {
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Unavailable' as const,
          syntax: fact.syntax,
          ...(fact.length._tag === 'OutOfRange' ? { cause: fact.length.cause } : {}),
        }),
        diagnostics: element.diagnostics,
      })
    }
    if (element.fact._tag === 'Resolved') {
      const type = Type.fixedArray(element.fact.type, fact.length.value)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved' as const,
          type,
          spelling: Type.encode(type),
          token: fact.token,
          syntax: fact.syntax,
          components: Object.freeze([element.fact]),
          ...(element.fact.exposureCause === undefined
            ? {}
            : { exposureCause: element.fact.exposureCause }),
        }),
        diagnostics: element.diagnostics,
      })
    }
    if (element.fact._tag === 'Unresolved') {
      return Object.freeze({
        fact: Object.freeze({
          ...element.fact,
          spelling: fact.spelling,
          token: fact.token,
          syntax: fact.syntax,
        }),
        diagnostics: element.diagnostics,
      })
    }
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable' as const,
        syntax: fact.syntax,
        ...(element.fact._tag === 'Unavailable' && element.fact.cause !== undefined
          ? { cause: element.fact.cause }
          : {}),
      }),
      diagnostics: element.diagnostics,
    })
  })()
}

const canonicalKey = (id: CanonicalId): string => `${id.module}.${id.name}`

const memberByNominal = (
  modules: ReadonlyArray<ModuleHeaders>,
  type: Type.Nominal,
): StructFact | ServiceFact | InterfaceFact | undefined => {
  const module = modules.find((candidate) => candidate.module === type.module)
  return [
    ...(module?.structs ?? []),
    ...(module?.services ?? []),
    ...(module?.interfaces ?? []),
  ].find(
    (member) => member.canonical._tag === 'Canonical' && member.canonical.id.name === type.name,
  )
}

/** Converts one resolved source type to the erased argument kind its declaration parameter owns. */
const genericArgumentForParameter = (
  parameter: Type.Parameter | undefined,
  type: Type.Type,
): Type.GenericArgument => {
  if (parameter?.kind === 'CallableRepresentation' || parameter?.kind === 'EffectRepresentation')
    return Type.isRepresented(type) ? type.representation.argument : type
  if (parameter?.kind === 'FailureRow') {
    if (Type.isParameter(type) && type.kind === 'FailureRow')
      return Type.failureRowArgument([], [type])
    if (Type.isNever(type)) return Type.failureRowArgument([])
    if (Type.isNominal(type)) return Type.failureRowArgument([type])
    if (Type.isUnion(type) && type.members.every(Type.isNominal))
      return Type.failureRowArgument(type.members)
  }
  if (parameter?.kind === 'RequirementRow') {
    if (Type.isParameter(type) && type.kind === 'RequirementRow')
      return Type.requirementRowArgument([], [type])
    if (Type.isNominal(type) || (Type.isParameter(type) && type.kind === 'Value'))
      return Type.requirementRowArgument([
        Object.freeze({ capability: type, role: 'DefaultRole', access: 'Shared' }),
      ])
  }
  return type
}

/**
 * Resolves every type parameter's bound to the interface its spelling names in the bounded
 * declaration's own module scope, recording that interface's ordered operation contract.
 *
 * The resolver's own diagnostics are deliberately dropped: a bound that names nothing, or names a
 * declaration that is not an interface, stays `UnresolvedBound` and is reported once at the
 * specialization that would have had to satisfy it, where the type argument is known.
 */
const resolveBounds = (
  module: string,
  typeParameters: ReadonlyArray<TypeParameterFact>,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
  diagnostics: Array<Diagnostic.Diagnostic>,
): ReadonlyArray<TypeParameterFact> => {
  if (
    typeParameters.every(
      (parameter) => parameter.bound === undefined && parameter.representationBound === undefined,
    )
  )
    return typeParameters
  return Object.freeze(
    typeParameters.map((parameter): TypeParameterFact => {
      const representation = parameter.representationBound
      if (representation !== undefined) {
        const resolved = resolveDeclaredType(module, representation.contract, resolvers, modules)
        diagnostics.push(...resolved.diagnostics)
        const contract =
          resolved.fact._tag === 'Resolved' &&
          (Type.isCallable(resolved.fact.type) || Type.isEffect(resolved.fact.type))
            ? resolved.fact.type
            : undefined
        return Object.freeze({
          ...parameter,
          type:
            contract === undefined
              ? parameter.type
              : Type.parameter(
                  parameter.type.owner,
                  parameter.type.ordinal,
                  parameter.type.name,
                  parameter.type.kind,
                  contract,
                ),
          representationBound: Object.freeze({
            ...representation,
            contract: resolved.fact,
          }),
        })
      }
      const bound = parameter.bound
      if (bound === undefined) return parameter
      const parameterName = parameter.name._tag === 'Present' ? parameter.name : undefined
      const boundResolvers: ResolutionSeams.ResolutionSeams =
        parameterName === undefined
          ? resolvers
          : Object.freeze({
              ...resolvers,
              type: (candidateModule: string, path: TypePathFact): TypeResolution =>
                path.segments.length === 1 && path.spelling === parameterName.spelling
                  ? Object.freeze({
                      fact: Object.freeze({
                        _tag: 'Resolved' as const,
                        type: parameter.type,
                        spelling: parameterName.spelling,
                        token: parameterName.token,
                        syntax: path.syntax,
                        components: Object.freeze([]),
                      }),
                      diagnostics: Object.freeze([]),
                    })
                  : resolvers.type(candidateModule, path),
            })
      const unresolvedCapability =
        bound._tag === 'ResolvedBound'
          ? bound.application.capability
          : (() => {
              const resolved = resolveDeclaredType(
                module,
                bound.application,
                boundResolvers,
                modules,
              ).fact
              if (resolved._tag === 'Resolved' && Type.isNominal(resolved.type))
                return resolved.type
              return resolved._tag === 'Unresolved' &&
                resolved.candidate !== undefined &&
                Type.isNominal(resolved.candidate)
                ? resolved.candidate
                : undefined
            })()
      const declaration =
        unresolvedCapability === undefined
          ? undefined
          : memberByNominal(modules, unresolvedCapability)
      if (
        unresolvedCapability === undefined ||
        declaration?._tag !== 'InterfaceDeclaration' ||
        declaration.canonical._tag !== 'Canonical'
      )
        return Object.freeze({ ...parameter, bound })
      const capability =
        unresolvedCapability.arguments.length === 0 && declaration.typeParameters.length === 1
          ? Type.nominal(unresolvedCapability.module, unresolvedCapability.name, [parameter.type])
          : unresolvedCapability
      const application = interfaceApplication(declaration, capability, parameter.type)
      if (application === undefined) return Object.freeze({ ...parameter, bound })
      return Object.freeze({
        ...parameter,
        bound: Object.freeze({
          _tag: 'ResolvedBound' as const,
          spelling: bound.spelling,
          path: bound.path,
          application,
        }),
      })
    }),
  )
}

/** Refreshes resolved bound applications once every interface header has its completed contracts. */
const refreshInterfaceApplications = (
  typeParameters: ReadonlyArray<TypeParameterFact>,
  modules: ReadonlyArray<ModuleHeaders>,
): ReadonlyArray<TypeParameterFact> =>
  Object.freeze(
    typeParameters.map((parameter): TypeParameterFact => {
      const bound = parameter.bound
      if (bound?._tag !== 'ResolvedBound') return parameter
      const declaration = memberByNominal(modules, bound.application.capability)
      if (declaration?._tag !== 'InterfaceDeclaration') return parameter
      const application = interfaceApplication(
        declaration,
        bound.application.capability,
        parameter.type,
      )
      return application === undefined
        ? parameter
        : Object.freeze({
            ...parameter,
            bound: Object.freeze({ ...bound, application }),
          })
    }),
  )

/**
 * Reads one conformance's requirements as the interface applications proof search will follow.
 *
 * A requirement that resolved to something other than an applied interface, or that never stated
 * its provider, contributes nothing here: header validation reports it, and admitting it as a
 * descent step would let a damaged fact stand in for a proof obligation.
 */
const declaredRequirements = (
  modules: ReadonlyArray<ModuleHeaders>,
  conformance: ConformanceFact,
): ReadonlyArray<ConformanceHead.Requirement> =>
  Object.freeze(
    conformance.requirements.flatMap((requirement): ReadonlyArray<ConformanceHead.Requirement> => {
      if (requirement.capability._tag !== 'Resolved') return []
      const capability = requirement.capability.type
      if (!Type.isNominal(capability)) return []
      if (memberByNominal(modules, capability)?._tag !== 'InterfaceDeclaration') return []
      const provider = capability.arguments.at(ConformanceHead.providerOrdinal)
      if (provider === undefined || !Type.isTypeArgument(provider)) return []
      return Object.freeze([Object.freeze({ capability, provider })])
    }),
  )

/** Positional specialization shared by interface and service witness validation. */
const witnessBinding = (
  implementation: DeclarationFact,
  declaredParameters: ReadonlyArray<Type.Parameter>,
): {
  readonly binders: ReadonlyArray<TypeParameterFact>
  readonly parameters: ReadonlyArray<Type.Parameter>
  readonly substitution: Type.Substitution | undefined
} => {
  const binders = implementation.typeParameters.filter(
    (parameter) => parameter.duplicateOf === undefined,
  )
  const parameters = binders.map((parameter) => parameter.type)
  return Object.freeze({
    binders,
    parameters,
    substitution:
      parameters.length === 0
        ? new Map<string, Type.GenericArgument>()
        : Type.substitution(parameters, declaredParameters.map(Type.parameterArgument)),
  })
}

/** Infers an interface witness declaration's own binders without assuming header position. */
const inferInterfaceWitnessTarget = (
  implementation: DeclarationFact,
  contract: InterfaceOperationApplicationFact | undefined,
): InterfaceWitnessInference.Inference | undefined => {
  if (
    contract === undefined ||
    implementation.parameters.length !== contract.operands.length ||
    implementation.returnType._tag !== 'Resolved'
  )
    return undefined
  const binders = implementation.typeParameters
    .filter((parameter) => parameter.duplicateOf === undefined)
    .map((parameter) => parameter.type)
  if (binders.length === 0)
    return Object.freeze({
      _tag: 'Inferred',
      arguments: Object.freeze([]),
      substitution: new Map<string, Type.GenericArgument>(),
    })
  const constraints: Array<InterfaceWitnessInference.Constraint> = []
  for (const [ordinal, operand] of contract.operands.entries()) {
    const pattern = implementation.parameters.at(ordinal)?.declaredType
    if (pattern?._tag !== 'Resolved' || operand.type._tag !== 'Resolved') return undefined
    const name =
      operand.parameter.name._tag === 'Present'
        ? operand.parameter.name.spelling
        : `#${ordinal + 1}`
    constraints.push(
      Object.freeze({
        label: Type.equals(
          Type.isReference(operand.type.type) ? operand.type.type.target : operand.type.type,
          contract.provider,
        )
          ? `receiver ${name}`
          : `parameter ${name}`,
        pattern: pattern.type,
        actual: operand.type.type,
      }),
    )
  }
  constraints.push(
    Object.freeze({
      label: 'success',
      pattern: implementation.returnType.type,
      actual: contract.success._tag === 'Resolved' ? contract.success.type : 'never',
    }),
  )
  constraints.push(
    Object.freeze({
      label: 'failure and requirement rows',
      pattern: Type.effect(
        Type.unit,
        implementation.failureRow.failures,
        'Shared',
        implementation.requirementRow.requirements.map((requirement) =>
          Object.freeze({ ...requirement, access: 'Shared' as const }),
        ),
        implementation.failureRow.parameters,
        implementation.requirementRow.parameters,
      ),
      actual: Type.effect(
        Type.unit,
        contract.failureRow.failures,
        'Shared',
        contract.requirementRow.requirements.map((requirement) =>
          Object.freeze({ ...requirement, access: 'Shared' as const }),
        ),
        contract.failureRow.parameters,
        contract.requirementRow.parameters,
      ),
    }),
  )
  return InterfaceWitnessInference.infer(binders, constraints)
}

const compatibilityOperand = (
  parameter: ParameterFact,
  type: Type.Type,
  provider: Type.Type,
): InterfaceWitnessCompatibility.Operand =>
  Object.freeze({
    name: parameter.name._tag === 'Present' ? parameter.name.spelling : '_',
    type,
    receiver: Type.equals(Type.isReference(type) ? type.target : type, provider),
  })

/** Checks one source witness against the complete applied interface contract it will implement. */
const interfaceWitnessCompatibility = (
  contract: InterfaceOperationApplicationFact | undefined,
  implementation: DeclarationFact,
  substitution: Type.Substitution,
): InterfaceWitnessCompatibility.Compatibility | undefined => {
  if (contract === undefined || contract.success._tag !== 'Resolved') return undefined
  const contractOperands = contract.operands.flatMap((operand) =>
    operand.type._tag === 'Resolved'
      ? [compatibilityOperand(operand.parameter, operand.type.type, contract.provider)]
      : [],
  )
  const witnessOperands = implementation.parameters.flatMap((parameter) =>
    parameter.declaredType._tag === 'Resolved'
      ? [
          compatibilityOperand(
            parameter,
            Type.substitute(parameter.declaredType.type, substitution),
            contract.provider,
          ),
        ]
      : [],
  )
  if (
    contractOperands.length !== contract.operands.length ||
    witnessOperands.length !== implementation.parameters.length ||
    implementation.returnType._tag !== 'Resolved'
  )
    return undefined
  const witnessRows = Type.substitute(
    Type.effect(
      Type.unit,
      implementation.failureRow.failures,
      'Shared',
      implementation.requirementRow.requirements,
      implementation.failureRow.parameters,
      implementation.requirementRow.parameters,
    ),
    substitution,
  )
  if (!Type.isEffect(witnessRows)) return undefined
  return InterfaceWitnessCompatibility.check(
    Object.freeze({
      functionKind: contract.functionKind,
      operands: Object.freeze(contractOperands),
      success: contract.success.type,
      failures: contract.failureRow.failures,
      failureParameters: contract.failureRow.parameters,
      requirements: contract.requirementRow.requirements,
      requirementParameters: contract.requirementRow.parameters,
    }),
    Object.freeze({
      functionKind: implementation.functionKind,
      operands: Object.freeze(witnessOperands),
      success: Type.substitute(implementation.returnType.type, substitution),
      failures: witnessRows.failures,
      failureParameters: witnessRows.failureParameters,
      requirements: witnessRows.requirements,
      requirementParameters: witnessRows.requirementParameters,
    }),
  )
}

/** The one legacy bridge: sealed scalar intrinsics consume what ordinary interfaces now borrow. */
const intrinsicWitnessOperand = (type: Type.Type): Type.Type =>
  Type.isReference(type) && type.access === 'Shared' ? type.target : type

const intrinsicWitnessCompatibility = (
  contract: InterfaceOperationApplicationFact | undefined,
  parameters: ReadonlyArray<Type.Type>,
  result: Type.Type,
): InterfaceWitnessCompatibility.Compatibility | undefined => {
  if (contract === undefined || contract.success._tag !== 'Resolved') return undefined
  const operands = contract.operands.flatMap((operand) =>
    operand.type._tag === 'Resolved'
      ? [
          compatibilityOperand(
            operand.parameter,
            intrinsicWitnessOperand(operand.type.type),
            contract.provider,
          ),
        ]
      : [],
  )
  if (operands.length !== contract.operands.length) return undefined
  return InterfaceWitnessCompatibility.check(
    Object.freeze({
      functionKind: contract.functionKind,
      operands: Object.freeze(operands),
      success: contract.success.type,
      failures: contract.failureRow.failures,
      failureParameters: contract.failureRow.parameters,
      requirements: contract.requirementRow.requirements,
      requirementParameters: contract.requirementRow.parameters,
    }),
    Object.freeze({
      functionKind: 'Ordinary',
      operands: Object.freeze(
        parameters.map((type, ordinal) =>
          Object.freeze({ name: operands.at(ordinal)?.name ?? '_', type, receiver: false }),
        ),
      ),
      success: result,
      failures: Object.freeze([]),
      failureParameters: Object.freeze([]),
      requirements: Object.freeze([]),
      requirementParameters: Object.freeze([]),
    }),
  )
}

/** Finds the first implementation bound the conformance header never promises. */
const unpromisedWitnessBound = (
  binding: ReturnType<typeof witnessBinding>,
  arguments_: ReadonlyArray<Type.GenericArgument>,
  conformance: ConformanceFact,
): TypeParameterFact | undefined =>
  binding.binders.find((binder, position) => {
    const bound = binder.bound
    if (bound?._tag !== 'ResolvedBound') return bound !== undefined
    const argument = arguments_.at(position)
    const header =
      argument !== undefined && Type.isTypeArgument(argument) && Type.isParameter(argument)
        ? conformance.typeParameters.find((parameter) => Type.equals(parameter.type, argument))
            ?.type
        : undefined
    return (
      header === undefined ||
      !conformance.requirements.some(
        (requirement) =>
          requirement.capability._tag === 'Resolved' &&
          Type.isNominal(requirement.capability.type) &&
          requirement.capability.type.module === bound.application.capability.module &&
          requirement.capability.type.name === bound.application.capability.name &&
          Type.equals(requirement.parameter, header),
      )
    )
  })

/** Resolves one retained type fact through a supplied module resolver and complete index. */
export const resolveTypeFact = (
  index: Index,
  module: string,
  fact: DeclaredTypeFact,
  resolver: TypeResolver,
): TypeResolution =>
  resolveDeclaredType(
    module,
    fact,
    ResolutionSeams.make(resolver, () => Object.freeze({ _tag: 'Missing' })),
    index.modules,
  )

const resolveFailureRow = (
  module: string,
  row: FailureRowFact,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): {
  readonly fact: FailureRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  if (row.syntax === undefined) return Object.freeze({ fact: row, diagnostics: Object.freeze([]) })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const members = row.members.map((member) => {
    const resolved = resolveDeclaredType(module, member, resolvers, modules)
    diagnostics.push(...resolved.diagnostics)
    return resolved.fact
  })
  const failures = new Map<string, Type.Nominal>()
  let available = row.parameters.length === 0
  for (const member of members) {
    if (
      member._tag !== 'Resolved' ||
      !Type.isNominal(member.type) ||
      !Type.isConcrete(member.type)
    ) {
      available = false
      if (member._tag === 'Resolved')
        diagnostics.push(
          Diagnostic.invalidFailureType(Type.encode(member.type), member.syntax.span),
        )
      continue
    }
    failures.set(Type.key(member.type), member.type)
  }
  return Object.freeze({
    fact: Object.freeze({
      ...row,
      members: Object.freeze(members),
      failures: Object.freeze([...failures.values()].sort(Type.compare)),
      available,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const resolveRequirementRow = (
  module: string,
  row: RequirementRowFact,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
): {
  readonly fact: RequirementRowFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  if (row.syntax === undefined) return Object.freeze({ fact: row, diagnostics: Object.freeze([]) })
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const entries = row.entries.map((entry) => {
    const capability = resolveDeclaredType(module, entry.capability, resolvers, modules)
    diagnostics.push(...capability.diagnostics)
    return Object.freeze({ ...entry, capability: capability.fact })
  })
  const requirements: Array<Type.Requirement> = []
  let available = row.parameters.length === 0
  for (const entry of entries) {
    if (
      entry.capability._tag === 'Resolved' &&
      (Type.isNominal(entry.capability.type) ||
        (Type.isParameter(entry.capability.type) && entry.capability.type.kind === 'Value'))
    ) {
      requirements.push(
        Object.freeze({
          capability: entry.capability.type,
          role: entry.role,
          access: entry.access,
        }),
      )
    } else {
      available = false
      if (entry.capability._tag === 'Resolved')
        diagnostics.push(
          Diagnostic.invalidRequirementType(Type.encode(entry.capability.type), entry.syntax.span),
        )
    }
  }
  const normalized = Type.effect('never', [], 'Shared', requirements).requirements
  return Object.freeze({
    fact: Object.freeze({
      ...row,
      entries: Object.freeze(entries),
      requirements: normalized,
      available,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const attachExposure = (
  fact: DeclaredTypeFact,
  modules: ReadonlyArray<ModuleHeaders>,
  diagnostics: Array<Diagnostic.Diagnostic>,
): DeclaredTypeFact => {
  if (fact._tag !== 'Resolved') return fact
  // An exact representation names a callable declaration rather than a nominal, so the private
  // leak it can create is invisible to the nominal walk below and is reported on its own terms.
  const leaked = Type.exactRepresentationDeclarations(fact.type).find((target) => {
    const owner = modules.find((candidate) => candidate.module === target.module)
    const found = lookupDeclaration(owner?.declarations ?? [], target.name)
    return found._tag === 'Resolved' && found.declaration.visibility === 'Private'
  })
  if (leaked !== undefined) {
    const diagnostic = Diagnostic.privateExactRepresentationLeak(leaked.name, fact.token.span)
    diagnostics.push(diagnostic)
    return Object.freeze({ ...fact, exposureCause: Diagnostic.identity(diagnostic) })
  }
  const nominal = Type.nominals(fact.type).find(
    (candidate) => memberByNominal(modules, candidate)?.visibility === 'Private',
  )
  if (nominal === undefined) return fact
  const target = memberByNominal(modules, nominal)
  if (target?.visibility !== 'Private') return fact
  const diagnostic = Diagnostic.privateTypeExposure(Type.encode(nominal), fact.token.span)
  diagnostics.push(diagnostic)
  return Object.freeze({ ...fact, exposureCause: Diagnostic.identity(diagnostic) })
}

/**
 * The type parameters each canonical struct reaches inline, keyed by canonical struct key. A
 * parameter absent from a struct's set is one the struct only ever holds behind an indirection.
 * A struct missing from the map is one this index cannot see, and its arguments are all treated
 * as inline.
 */
type InlineParameters = ReadonlyMap<string, ReadonlySet<number>>

/**
 * Walks the nominals and parameters whose layout one type's layout actually requires.
 *
 * A struct embeds its fields, so a field reaches everything its type names outside an indirecting
 * position. `RawBuffer<T>` and `Slot<T>` are the compiler-owned indirections: their
 * representations are `{$allocation, count}` and `{$address}` whatever `T` is, so the walk names
 * them and stops rather than descending into the element. Every other nominal is entered only
 * through the arguments its own declaration reaches inline, which `inlineParameters` supplies.
 * Arrays, slices, references, callables, effects, and unions descend exactly as `Type.nominals`
 * does, so this graph is narrower than the reported dependency graph and never wider.
 */
const inlineReach = (
  self: Type.Type,
  inlineParameters: InlineParameters,
  visit: (reached: Type.Nominal | Type.Parameter) => void,
): void => {
  const descend = (type: Type.Type): void => {
    if (Type.isNominal(type)) {
      visit(type)
      if (Type.isRawBuffer(type) || Type.isSlot(type)) return
      const inline = inlineParameters.get(`${type.module}.${type.name}`)
      for (const [ordinal, argument] of type.arguments.entries())
        if ((inline === undefined || inline.has(ordinal)) && Type.isTypeArgument(argument))
          descend(argument)
      return
    }
    if (Type.isParameter(type)) {
      visit(type)
      return
    }
    if (Type.isFailureProjection(type)) {
      visit(type.parameter)
      return
    }
    if (Type.isFixedArray(type) || Type.isSlice(type)) {
      descend(type.element)
      return
    }
    if (Type.isReference(type)) {
      descend(type.target)
      return
    }
    if (Type.isCallable(type)) {
      for (const parameter of type.parameters) descend(parameter)
      descend(type.result)
      return
    }
    if (Type.isEffect(type)) {
      descend(type.success)
      for (const failure of type.failures) descend(failure)
      for (const requirement of type.requirements) descend(requirement.capability)
      return
    }
    if (Type.isUnion(type)) for (const member of type.members) descend(member)
  }
  descend(self)
}

/**
 * The monotone least fixed point of "struct `S` reaches its own parameter `i` inline".
 *
 * Every parameter starts indirected. A round marks a parameter inline as soon as one of the
 * struct's own fields reaches it under `inlineReach`, and marking a parameter can only open more
 * descents, so the sets only grow and the loop terminates. Because it is the least fixed point,
 * the answer does not depend on the order structs or modules arrive in.
 */
const inlineParametersOf = (structs: ReadonlyArray<StructFact>): InlineParameters => {
  const declarations = new Map<string, StructFact>()
  for (const struct of structs)
    if (struct.canonical._tag === 'Canonical')
      declarations.set(canonicalKey(struct.canonical.id), struct)
  const inline = new Map<string, Set<number>>()
  for (const key of declarations.keys()) inline.set(key, new Set())
  for (let growing = true; growing; ) {
    growing = false
    for (const [key, struct] of declarations) {
      const reached = inline.get(key)
      if (reached === undefined || struct.typeParameters.length === 0) continue
      // Keyed by position, matching how `Type.substitution` binds arguments to parameters.
      const own = new Map(
        struct.typeParameters.map(
          (parameter, position) => [Type.key(parameter.type), position] as const,
        ),
      )
      for (const field of struct.fields) {
        if (field.declaredType._tag !== 'Resolved') continue
        inlineReach(field.declaredType.type, inline, (member) => {
          if (!Type.isParameter(member)) return
          const ordinal = own.get(Type.key(member))
          if (ordinal === undefined || reached.has(ordinal)) return
          reached.add(ordinal)
          growing = true
        })
      }
    }
  }
  return inline
}

/** Names every canonical struct one field reaches inline, for cycle detection only. */
const inlineNeighbors = (
  field: FieldFact,
  inlineParameters: InlineParameters,
): ReadonlyArray<string> => {
  if (field.declaredType._tag !== 'Resolved') return Object.freeze([])
  const reached: Array<string> = []
  inlineReach(field.declaredType.type, inlineParameters, (member) => {
    if (Type.isParameter(member)) return
    reached.push(`${member.module}.${member.name}`)
  })
  return Object.freeze(reached)
}

const stronglyConnected = (
  structs: ReadonlyArray<StructFact>,
  inlineParameters: InlineParameters,
): ReadonlyArray<ReadonlyArray<StructFact>> => {
  const canonical = structs
    .filter((struct) => struct.canonical._tag === 'Canonical')
    .sort((left, right) => {
      const leftId = left.canonical._tag === 'Canonical' ? left.canonical.id : undefined
      const rightId = right.canonical._tag === 'Canonical' ? right.canonical.id : undefined
      return leftId === undefined || rightId === undefined
        ? 0
        : canonicalKey(leftId).localeCompare(canonicalKey(rightId))
    })
  const byKey = new Map(
    canonical.flatMap((struct) =>
      struct.canonical._tag === 'Canonical'
        ? [[canonicalKey(struct.canonical.id), struct] as const]
        : [],
    ),
  )
  let nextIndex = 0
  const indices = new Map<string, number>()
  const lows = new Map<string, number>()
  const stack: Array<string> = []
  const stacked = new Set<string>()
  const components: Array<ReadonlyArray<StructFact>> = []
  const visit = (key: string): void => {
    indices.set(key, nextIndex)
    lows.set(key, nextIndex)
    nextIndex += 1
    stack.push(key)
    stacked.add(key)
    const struct = byKey.get(key)
    const neighbors = (struct?.fields ?? [])
      .flatMap((field) => inlineNeighbors(field, inlineParameters))
      .filter((neighbor) => byKey.has(neighbor))
      .sort()
    for (const neighbor of neighbors) {
      if (!indices.has(neighbor)) {
        visit(neighbor)
        lows.set(key, Math.min(lows.get(key) ?? 0, lows.get(neighbor) ?? 0))
      } else if (stacked.has(neighbor)) {
        lows.set(key, Math.min(lows.get(key) ?? 0, indices.get(neighbor) ?? 0))
      }
    }
    if (lows.get(key) !== indices.get(key)) return
    const component: Array<StructFact> = []
    for (;;) {
      const memberKey = stack.pop()
      if (memberKey === undefined) break
      stacked.delete(memberKey)
      const member = byKey.get(memberKey)
      if (member !== undefined) component.push(member)
      if (memberKey === key) break
    }
    components.push(
      Object.freeze(
        component.sort((left, right) => {
          if (left.canonical._tag !== 'Canonical' || right.canonical._tag !== 'Canonical') return 0
          return canonicalKey(left.canonical.id).localeCompare(canonicalKey(right.canonical.id))
        }),
      ),
    )
  }
  for (const key of byKey.keys()) if (!indices.has(key)) visit(key)
  return Object.freeze(components)
}

const resolveOpaqueResult = (
  module: string,
  opaqueResult: OpaqueResultFact | undefined,
  resolvers: ResolutionSeams.ResolutionSeams,
  modules: ReadonlyArray<ModuleHeaders>,
  diagnostics: Array<Diagnostic.Diagnostic>,
): OpaqueResultFact | undefined => {
  if (opaqueResult === undefined) return undefined
  const binder = resolveBounds(module, [opaqueResult.binder], resolvers, modules, diagnostics).at(0)
  if (binder === undefined) return undefined
  if (binder.type.kind !== 'CallableRepresentation' && binder.type.kind !== 'EffectRepresentation')
    diagnostics.push(
      Diagnostic.invalidOpaqueResultBinder(
        binder.name._tag === 'Present' ? binder.name.spelling : binder.type.name,
        binder.type.kind,
        binder.syntax.span,
      ),
    )
  return Object.freeze({ ...opaqueResult, binder })
}

const opaqueEnclosingArgument = (parameter: Type.Parameter): Type.GenericArgument => {
  if (parameter.kind === 'FailureRow') return Type.failureRowArgument([], [parameter])
  if (parameter.kind === 'RequirementRow') return Type.requirementRowArgument([], [parameter])
  if (parameter.kind === 'CallableRepresentation' || parameter.kind === 'EffectRepresentation')
    return Type.representationParameterArgument(parameter)
  return parameter
}

const closeOpaqueReturnType = (
  fact: ReturnTypeFact,
  opaqueResult: OpaqueResultFact | undefined,
  enclosing: ReadonlyArray<TypeParameterFact>,
): { readonly fact: ReturnTypeFact; readonly opaqueResult?: OpaqueResultFact } => {
  const bound = opaqueResult?.binder.type.representationBound
  if (fact._tag !== 'Resolved' || opaqueResult === undefined || bound === undefined)
    return Object.freeze({ fact, ...(opaqueResult === undefined ? {} : { opaqueResult }) })
  const argument = Type.opaqueRepresentationArgument(
    opaqueResult.family,
    bound,
    enclosing.map((parameter) => opaqueEnclosingArgument(parameter.type)),
  )
  const closed = Type.substitute(
    fact.type,
    new Map([[Type.key(opaqueResult.binder.type), argument]]),
  )
  return Object.freeze({
    fact: Object.freeze({ ...fact, type: closed, spelling: Type.encode(closed) }),
    opaqueResult: Object.freeze({
      ...opaqueResult,
      publicSignature: Object.freeze({
        bound: Type.key(bound),
        result: Type.key(closed),
        enclosingKinds: Object.freeze(enclosing.map((parameter) => parameter.type.kind)),
      }),
    }),
  })
}

/** Resolves all retained type paths and validates public exposure and inline dependencies. */
export const complete = (self: Index, resolvers: ResolutionSeams.ResolutionSeams): Index => {
  const diagnostics: Array<Diagnostic.Diagnostic> = [...self.diagnostics]
  let modules = self.modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member._tag === 'ConstantDeclaration') {
        const resolved = resolveDeclaredType(
          module.module,
          member.declaredType,
          resolvers,
          self.modules,
        )
        diagnostics.push(...resolved.diagnostics)
        return Object.freeze({ ...member, declaredType: resolved.fact })
      }
      if (member._tag === 'FunctionDeclaration') {
        const resolvedTypeParameters = resolveBounds(
          module.module,
          member.typeParameters,
          resolvers,
          self.modules,
          diagnostics,
        )
        const opaqueResult = resolveOpaqueResult(
          module.module,
          member.opaqueResult,
          resolvers,
          self.modules,
          diagnostics,
        )
        const memberResolvers: ResolutionSeams.ResolutionSeams =
          member.opaqueResult === undefined || opaqueResult === undefined
            ? resolvers
            : ResolutionSeams.withRepresentationBinding(
                resolvers,
                member.opaqueResult.binder.type,
                opaqueResult.binder.type,
              )
        const parameters = member.parameters.map((parameter) => {
          const resolved = resolveDeclaredType(
            module.module,
            parameter.declaredType,
            resolvers,
            self.modules,
          )
          diagnostics.push(...resolved.diagnostics)
          return Object.freeze({ ...parameter, declaredType: resolved.fact })
        })
        const resolvedResult = resolveDeclaredType(
          module.module,
          member.returnType,
          memberResolvers,
          self.modules,
        )
        diagnostics.push(...resolvedResult.diagnostics)
        const result = closeOpaqueReturnType(
          resolvedResult.fact,
          opaqueResult,
          resolvedTypeParameters,
        )
        const failureRow = resolveFailureRow(
          module.module,
          member.failureRow,
          resolvers,
          self.modules,
        )
        diagnostics.push(...failureRow.diagnostics)
        const requirementRow = resolveRequirementRow(
          module.module,
          member.requirementRow,
          resolvers,
          self.modules,
        )
        diagnostics.push(...requirementRow.diagnostics)
        return Object.freeze({
          ...member,
          typeParameters: resolvedTypeParameters,
          parameters: Object.freeze(parameters),
          returnType: result.fact,
          ...(result.opaqueResult === undefined ? {} : { opaqueResult: result.opaqueResult }),
          failureRow: failureRow.fact,
          requirementRow: requirementRow.fact,
        })
      }
      if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        const resolvedMemberTypeParameters = resolveBounds(
          module.module,
          member.typeParameters,
          resolvers,
          self.modules,
          diagnostics,
        )
        const operations = member.operations.map((operation) => {
          if (operation.opaqueResult !== undefined) {
            const owner = member.name._tag === 'Present' ? member.name.spelling : '<anonymous>'
            const name = operation.name._tag === 'Present' ? operation.name.spelling : '<anonymous>'
            diagnostics.push(
              Diagnostic.bodylessOpaqueResult(
                `${owner}.${name}`,
                member._tag === 'ServiceDeclaration' ? 'ServiceOperation' : 'InterfaceOperation',
                operation.opaqueResult.syntax.span,
              ),
            )
          }
          const resolvedOperationTypeParameters = resolveBounds(
            module.module,
            operation.typeParameters,
            resolvers,
            self.modules,
            diagnostics,
          )
          const opaqueResult = resolveOpaqueResult(
            module.module,
            operation.opaqueResult,
            resolvers,
            self.modules,
            diagnostics,
          )
          const operationResolvers: ResolutionSeams.ResolutionSeams =
            operation.opaqueResult === undefined || opaqueResult === undefined
              ? resolvers
              : ResolutionSeams.withRepresentationBinding(
                  resolvers,
                  operation.opaqueResult.binder.type,
                  opaqueResult.binder.type,
                )
          const parameters = operation.parameters.map((parameter) => {
            const resolved = resolveDeclaredType(
              module.module,
              parameter.declaredType,
              resolvers,
              self.modules,
            )
            diagnostics.push(...resolved.diagnostics)
            return Object.freeze({ ...parameter, declaredType: resolved.fact })
          })
          const resolvedResult = resolveDeclaredType(
            module.module,
            operation.returnType,
            operationResolvers,
            self.modules,
          )
          const result = closeOpaqueReturnType(resolvedResult.fact, opaqueResult, [
            ...resolvedMemberTypeParameters,
            ...resolvedOperationTypeParameters,
          ])
          const failureRow = resolveFailureRow(
            module.module,
            operation.failureRow,
            resolvers,
            self.modules,
          )
          const requirementRow = resolveRequirementRow(
            module.module,
            operation.requirementRow,
            resolvers,
            self.modules,
          )
          diagnostics.push(
            ...resolvedResult.diagnostics,
            ...failureRow.diagnostics,
            ...requirementRow.diagnostics,
          )
          return Object.freeze({
            ...operation,
            typeParameters: resolvedOperationTypeParameters,
            parameters: Object.freeze(parameters),
            returnType: result.fact,
            ...(result.opaqueResult === undefined ? {} : { opaqueResult: result.opaqueResult }),
            failureRow: failureRow.fact,
            requirementRow: requirementRow.fact,
          })
        })
        const completed = Object.freeze({
          ...member,
          typeParameters: resolvedMemberTypeParameters,
          operations: Object.freeze(operations),
        })
        return completed._tag === 'InterfaceDeclaration'
          ? Object.freeze({
              ...completed,
              operationContracts: interfaceOperationContracts(
                resolvedMemberTypeParameters,
                operations,
              ),
            })
          : completed
      }
      const fields = member.fields.map((field) => {
        const resolved = resolveDeclaredType(
          module.module,
          field.declaredType,
          resolvers,
          self.modules,
        )
        diagnostics.push(...resolved.diagnostics)
        return Object.freeze({ ...field, declaredType: resolved.fact })
      })
      return Object.freeze({
        ...member,
        typeParameters: resolveBounds(
          module.module,
          member.typeParameters,
          resolvers,
          self.modules,
          diagnostics,
        ),
        fields: Object.freeze(fields),
      })
    })
    const conformances = module.conformances.map((conformance) => {
      const capability = resolveDeclaredType(
        module.module,
        conformance.capability,
        resolvers,
        self.modules,
      )
      const provider = resolveDeclaredType(
        module.module,
        conformance.provider,
        resolvers,
        self.modules,
      )
      diagnostics.push(...capability.diagnostics, ...provider.diagnostics)
      const hook =
        conformance.hook === undefined
          ? undefined
          : (() => {
              const parameterType = resolveDeclaredType(
                module.module,
                conformance.hook.parameterType,
                resolvers,
                self.modules,
              )
              const returnType = resolveDeclaredType(
                module.module,
                conformance.hook.returnType,
                resolvers,
                self.modules,
              )
              const failureRow = resolveFailureRow(
                module.module,
                conformance.hook.failureRow,
                resolvers,
                self.modules,
              )
              const requirementRow = resolveRequirementRow(
                module.module,
                conformance.hook.requirementRow,
                resolvers,
                self.modules,
              )
              diagnostics.push(
                ...parameterType.diagnostics,
                ...returnType.diagnostics,
                ...failureRow.diagnostics,
                ...requirementRow.diagnostics,
              )
              return Object.freeze({
                ...conformance.hook,
                parameterType: parameterType.fact,
                returnType: returnType.fact,
                failureRow: failureRow.fact,
                requirementRow: requirementRow.fact,
              })
            })()
      const requirements = conformance.requirements.map((requirement) => {
        const resolved = resolveDeclaredType(
          module.module,
          requirement.capability,
          resolvers,
          self.modules,
        )
        diagnostics.push(...resolved.diagnostics)
        return Object.freeze({ ...requirement, capability: resolved.fact })
      })
      return Object.freeze({
        ...conformance,
        requirements: Object.freeze(requirements),
        capability: capability.fact,
        provider: provider.fact,
        ...(hook === undefined ? {} : { hook }),
      })
    })
    return Object.freeze({
      ...module,
      members: Object.freeze(members),
      declarations: Object.freeze(
        members.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
      services: Object.freeze(
        members.filter((member): member is ServiceFact => member._tag === 'ServiceDeclaration'),
      ),
      interfaces: Object.freeze(
        members.filter((member): member is InterfaceFact => member._tag === 'InterfaceDeclaration'),
      ),
      constants: Object.freeze(
        members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
      ),
      conformances: Object.freeze(conformances),
    })
  })

  // Bounds may precede the interface they name, and the first completion pass intentionally reads
  // the old immutable module graph. Refresh only their applications now that every interface owns
  // resolved operation contracts; no witness selection or executable behavior is decided here.
  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      const typeParameters = refreshInterfaceApplications(member.typeParameters, modules)
      if (member._tag !== 'ServiceDeclaration' && member._tag !== 'InterfaceDeclaration')
        return Object.freeze({ ...member, typeParameters })
      const operations = Object.freeze(
        member.operations.map((operation) =>
          Object.freeze({
            ...operation,
            typeParameters: refreshInterfaceApplications(operation.typeParameters, modules),
          }),
        ),
      )
      return member._tag === 'InterfaceDeclaration'
        ? Object.freeze({
            ...member,
            typeParameters,
            operations,
            operationContracts: interfaceOperationContracts(typeParameters, operations),
          })
        : Object.freeze({ ...member, typeParameters, operations })
    })
    const conformances = Object.freeze(
      module.conformances.map((conformance) => {
        const capability =
          conformance.capability._tag === 'Resolved' && Type.isNominal(conformance.capability.type)
            ? conformance.capability.type
            : undefined
        const provider =
          conformance.provider._tag === 'Resolved' ? conformance.provider.type : undefined
        const declaration =
          capability === undefined ? undefined : memberByNominal(modules, capability)
        const application =
          capability !== undefined &&
          provider !== undefined &&
          declaration?._tag === 'InterfaceDeclaration'
            ? interfaceApplication(declaration, capability, provider)
            : undefined
        return Object.freeze({
          ...conformance,
          typeParameters: refreshInterfaceApplications(conformance.typeParameters, modules),
          operations: Object.freeze(
            conformance.operations.map((operation) => {
              const contract = application?.operations.find(
                (candidate) =>
                  operation.name._tag === 'Present' &&
                  candidate.declaration.name._tag === 'Present' &&
                  candidate.declaration.name.spelling === operation.name.spelling,
              )
              return Object.freeze({
                ...operation,
                ...(contract === undefined ? {} : { contract }),
              })
            }),
          ),
        })
      }),
    )
    return Object.freeze({
      ...module,
      members: Object.freeze(members),
      declarations: Object.freeze(
        members.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
      services: Object.freeze(
        members.filter((member): member is ServiceFact => member._tag === 'ServiceDeclaration'),
      ),
      interfaces: Object.freeze(
        members.filter((member): member is InterfaceFact => member._tag === 'InterfaceDeclaration'),
      ),
      constants: Object.freeze(
        members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
      ),
      conformances,
    })
  })

  // Coherence and termination are program-wide questions, so both are answered once every module's
  // headers have resolved and before any conformance body is validated. Overlap is decided on head
  // shapes alone: no bound is consulted, because whether a bound is satisfiable depends on the whole
  // program and would let one specialization silently change which witness it selects.
  const acceptedHeads: Array<{
    readonly module: string
    readonly ordinal: number
    readonly head: ConformanceHead.ConformanceHead
    readonly span: SourceSpan.SourceSpan
  }> = []
  modules = modules.map((module) =>
    Object.freeze({
      ...module,
      conformances: Object.freeze(
        module.conformances.map((conformance): ConformanceFact => {
          if (
            conformance.capability._tag !== 'Resolved' ||
            !Type.isNominal(conformance.capability.type) ||
            conformance.provider._tag !== 'Resolved'
          )
            return conformance
          const requirements = declaredRequirements(modules, conformance)
          const head = ConformanceHead.make(
            conformance.capability.type,
            conformance.provider.type,
            requirements,
          )
          // A damaged requirement is retained on the conformance fact for diagnostics, but cannot
          // be shortened into a zero-obligation head. Header validation below reports the source
          // error; leaving termination unavailable keeps the fact out of coherence and proof search.
          if (requirements.length !== conformance.requirements.length)
            return Object.freeze({ ...conformance, head })
          const failures = ConformanceHead.terminationFailures(head)
          if (failures.length > 0)
            diagnostics.push(
              Diagnostic.nonTerminatingConformance(
                ConformanceHead.encode(head),
                failures.map(ConformanceHead.describeTermination),
                conformance.syntax.span,
              ),
            )
          // This is the one authority on whether two conformances may cover one provider. Two
          // unbounded headers with one identical shape are the case a reader recognizes as a
          // duplicate and are named that way; two headers a bound is the only difference between
          // are reported as the overlap they are, because calling them duplicates would suggest the
          // bounds were compared. Comparing the alpha-normalized heads is what makes the two tests
          // agree — keying the duplicate check on unnormalized capabilities, as an earlier version
          // did, let exactly the bound-distinguished pair match neither and survive.
          const headKey = ConformanceHead.key(head)
          const overlapping = acceptedHeads.find((candidate) =>
            ConformanceHead.mayOverlap(candidate.head, head),
          )
          if (overlapping === undefined)
            acceptedHeads.push(
              Object.freeze({
                module: module.module,
                ordinal: conformance.ordinal,
                head,
                span: conformance.syntax.span,
              }),
            )
          else if (
            ConformanceHead.key(overlapping.head) === headKey &&
            head.requirements.length === 0 &&
            overlapping.head.requirements.length === 0
          )
            diagnostics.push(
              Object.freeze({
                ...Diagnostic.invalidConformance(
                  `duplicate ${conformance.capability.type.name} implementation for ${Type.encode(conformance.provider.type)}`,
                  conformance.syntax.span,
                ),
                relatedSpans: Object.freeze([
                  Object.freeze({ label: 'first implementation', span: overlapping.span }),
                ]),
              }),
            )
          else
            diagnostics.push(
              Diagnostic.overlappingConformance(
                ConformanceHead.encode(head),
                ConformanceHead.encode(overlapping.head),
                conformance.syntax.span,
                overlapping.span,
              ),
            )
          return Object.freeze({
            ...conformance,
            head,
            coherence:
              overlapping === undefined
                ? Object.freeze({ _tag: 'Coherent' as const })
                : Object.freeze({
                    _tag: 'Overlapping' as const,
                    module: overlapping.module,
                    ordinal: overlapping.ordinal,
                  }),
            termination:
              failures.length === 0
                ? Object.freeze({ _tag: 'Terminating' as const })
                : Object.freeze({ _tag: 'NonTerminating' as const, failures }),
          })
        }),
      ),
    }),
  )

  const copyMemo = new Map<string, boolean>()
  const containsPositionRestrictedBorrow = Type.containsPositionRestrictedBorrow
  const isCopyType = (type: Type.Type, visiting = new Set<string>()): boolean => {
    if (Type.isBuiltin(type) || Type.isString(type) || Type.isReference(type) || Type.isSlice(type))
      return true
    if (Type.isFixedArray(type)) return isCopyType(type.element, visiting)
    if (Type.isUnion(type)) return type.members.every((member) => isCopyType(member, visiting))
    if (Type.equals(type, Type.unit)) return true
    if (!Type.isNominal(type) || Type.isIntrinsicNominal(type)) return false
    const key = Type.key(type)
    const remembered = copyMemo.get(key)
    if (remembered !== undefined) return remembered
    if (visiting.has(key)) return false
    const declaration = modules
      .flatMap((module) => module.structs)
      .find(
        (struct) =>
          struct.canonical._tag === 'Canonical' &&
          struct.canonical.id.module === type.module &&
          struct.canonical.id.name === type.name,
      )
    if (declaration === undefined) return false
    const next = new Set(visiting).add(key)
    const result = declaration.fields.every(
      (field) =>
        field.declaredType._tag === 'Resolved' && isCopyType(field.declaredType.type, next),
    )
    copyMemo.set(key, result)
    return result
  }

  const invalidConformances = new Set<ConformanceFact>()
  const inferredWitnessArguments = new Map<
    ConformanceFact['operations'][number],
    ReadonlyArray<Type.GenericArgument>
  >()
  for (const module of modules) {
    for (const conformance of module.conformances) {
      const markInvalid = (): void => {
        invalidConformances.add(conformance)
      }
      const rejectConformance = <A extends Diagnostic.Diagnostic>(diagnostic: A): A => {
        markInvalid()
        return diagnostic
      }
      const invalidDiagnostic = (
        ...args: Parameters<typeof Diagnostic.invalidConformance>
      ): ReturnType<typeof Diagnostic.invalidConformance> => {
        return rejectConformance(Diagnostic.invalidConformance(...args))
      }
      if (
        conformance.capability._tag !== 'Resolved' ||
        !Type.isNominal(conformance.capability.type) ||
        conformance.provider._tag !== 'Resolved'
      ) {
        diagnostics.push(
          invalidDiagnostic(
            'the capability must resolve to a nominal type and the provider must resolve to a type',
            conformance.syntax.span,
          ),
        )
        continue
      }
      const capability = conformance.capability.type
      const provider = conformance.provider.type
      const sourceInterface = modules
        .find((candidate) => candidate.module === capability.module)
        ?.interfaces.find(
          (interface_) =>
            interface_.canonical._tag === 'Canonical' &&
            interface_.canonical.id.name === capability.name,
        )
      const sourceService = modules
        .find((candidate) => candidate.module === capability.module)
        ?.services.find(
          (service) =>
            service.canonical._tag === 'Canonical' && service.canonical.id.name === capability.name,
        )
      if (sourceInterface === undefined && !Type.isNominal(provider)) {
        diagnostics.push(
          invalidDiagnostic(
            'service and compiler-sealed capability providers must be nominal types',
            conformance.syntax.span,
          ),
        )
        continue
      }
      const declaredParameters = conformance.typeParameters
        .filter((parameter) => parameter.duplicateOf === undefined)
        .map((parameter) => parameter.type)
      // Source interface and service arguments may mention the header's binders, because one
      // parametric provider may implement one equally parametric source capability. Compiler-sealed
      // capabilities remain concrete.
      if (
        !Type.isConcrete(capability) &&
        ((sourceInterface === undefined && sourceService === undefined) ||
          declaredParameters.length === 0)
      ) {
        diagnostics.push(
          invalidDiagnostic(
            'the capability must be concrete; impl type parameters may only bind the provider',
            conformance.syntax.span,
          ),
        )
        continue
      }
      // Every interface application states its provider explicitly rather than through an implicit
      // `Self`, so the head's own first argument has to be the type it is declared `for`.
      if (sourceInterface !== undefined) {
        const declaredProvider = capability.arguments.at(ConformanceHead.providerOrdinal)
        if (
          declaredProvider === undefined ||
          !Type.isTypeArgument(declaredProvider) ||
          !Type.equals(declaredProvider, provider)
        ) {
          diagnostics.push(
            invalidDiagnostic(
              `${capability.name} must be applied to its own provider ${Type.encode(provider)}`,
              conformance.syntax.span,
            ),
          )
          continue
        }
      }
      // This predicate is the exact complement of what `declaredRequirements` admits. They have to
      // agree: a requirement the reader accepts but the reader of obligations drops would be a
      // bound nothing ever proves.
      const unstatedRequirement = conformance.requirements.find((requirement) => {
        if (requirement.capability._tag !== 'Resolved') return true
        const applied = requirement.capability.type
        if (!Type.isNominal(applied)) return true
        if (memberByNominal(modules, applied)?._tag !== 'InterfaceDeclaration') return true
        const stated = applied.arguments.at(ConformanceHead.providerOrdinal)
        return stated === undefined || !Type.isTypeArgument(stated)
      })
      if (unstatedRequirement !== undefined) {
        diagnostics.push(
          invalidDiagnostic(
            `requirement ${unstatedRequirement.spelling} must be an interface applied to its own provider`,
            unstatedRequirement.syntax.span,
          ),
        )
        continue
      }
      if (conformance.termination._tag === 'NonTerminating') continue
      if (conformance.coherence._tag === 'Overlapping') continue
      const usedParameterKeys = new Set(
        Type.parameters(provider).map((parameter) => Type.key(parameter)),
      )
      const unused = declaredParameters.filter(
        (parameter) => !usedParameterKeys.has(Type.key(parameter)),
      )
      if (unused.length > 0) {
        diagnostics.push(
          invalidDiagnostic(
            `impl type parameter ${unused.map((parameter) => parameter.name).join(', ')} is not used by the provider type`,
            conformance.syntax.span,
          ),
        )
        continue
      }
      if (sourceInterface !== undefined) {
        if (conformance.hook !== undefined) {
          diagnostics.push(
            invalidDiagnostic(
              `${capability.name} implementations use operation mappings, not a hook body`,
              conformance.hook.syntax.span,
            ),
          )
          continue
        }
        const mapped = new Map<string, ConformanceFact['operations'][number]>()
        let invalid = false
        for (const mapping of conformance.operations) {
          if (mapping.name._tag !== 'Present') {
            markInvalid()
            invalid = true
            continue
          }
          if (mapped.has(mapping.name.spelling)) {
            diagnostics.push(
              invalidDiagnostic(
                `duplicate ${capability.name}.${mapping.name.spelling} operation mapping`,
                mapping.syntax.span,
              ),
            )
            invalid = true
          } else mapped.set(mapping.name.spelling, mapping)
        }
        const operationNames = new Set(
          sourceInterface.operations.flatMap((operation) =>
            operation.name._tag === 'Present' ? [operation.name.spelling] : [],
          ),
        )
        const missing = [...operationNames].filter((name) => !mapped.has(name))
        const extra = [...mapped.keys()].filter((name) => !operationNames.has(name))
        if (missing.length > 0 || extra.length > 0) {
          diagnostics.push(
            invalidDiagnostic(
              [
                ...(missing.length === 0 ? [] : [`missing ${missing.join(', ')}`]),
                ...(extra.length === 0 ? [] : [`unknown ${extra.join(', ')}`]),
              ].join('; '),
              conformance.syntax.span,
            ),
          )
          invalid = true
        }
        const substitution = Type.substitution(
          sourceInterface.typeParameters.map((parameter) => parameter.type),
          capability.arguments,
        )
        if (substitution === undefined) {
          diagnostics.push(
            invalidDiagnostic(
              `${capability.name} implementation has the wrong interface type-argument arity`,
              conformance.syntax.span,
            ),
          )
          continue
        }
        if (invalid) continue
        const interfaceProviderModule = Type.isNominal(provider)
          ? modules.find((candidate) => candidate.module === provider.module)
          : undefined
        for (const contract of sourceInterface.operations) {
          if (contract.name._tag !== 'Present') continue
          const mapping = mapped.get(contract.name.spelling)
          if (mapping === undefined) continue
          const target = mapping.target
          const contractName = contract.name.spelling
          const rejectIncompatibleMapping = (detail?: string): void => {
            diagnostics.push(
              invalidDiagnostic(
                `${target._tag === 'TypePath' ? target.spelling : '_'} is incompatible with ${capability.name}.${contractName}${detail === undefined ? '' : `: ${detail}`}`,
                mapping.syntax.span,
              ),
            )
          }
          // A source witness uses the applied contract's literal operands. Only a sealed intrinsic
          // crosses the narrowly scoped scalar value/shared-borrow bridge below.
          if (
            Type.isNominal(provider) &&
            target._tag === 'TypePath' &&
            target.segments.length === 2 &&
            target.segments.at(0)?.spelling === provider.name &&
            target.segments.at(0)?.spelling !== 'Intrinsic'
          ) {
            const targetName = target.segments.at(1)?.spelling
            const implementation = interfaceProviderModule?.declarations.find(
              (declaration) =>
                targetName !== undefined &&
                declaration.name._tag === 'Present' &&
                declaration.name.spelling === targetName,
            )
            if (implementation === undefined) {
              diagnostics.push(
                invalidDiagnostic(
                  `mapped operation ${provider.name}.${targetName ?? '_'} does not exist`,
                  mapping.syntax.span,
                ),
              )
              continue
            }
            const inference = inferInterfaceWitnessTarget(implementation, mapping.contract)
            if (inference === undefined || inference._tag === 'Failed') {
              if (inference?._tag === 'Failed') {
                const problem = inference.problem
                if (problem._tag === 'IncompatibleConstraint') {
                  rejectIncompatibleMapping()
                  continue
                }
                const detail =
                  problem._tag === 'UnresolvedBinder'
                    ? `cannot infer witness target binder ${problem.binder.name}`
                    : problem._tag === 'ConflictingBinder'
                      ? `witness target binder ${problem.binder.name} is ${Type.encodeGenericArgument(problem.previous)} from ${problem.previousConstraint} but ${Type.encodeGenericArgument(problem.conflicting)} from ${problem.conflictingConstraint}`
                      : `witness target binder ${problem.binder.name} cannot accept ${Type.encodeGenericArgument(problem.argument)}`
                diagnostics.push(
                  invalidDiagnostic(`${target.spelling}: ${detail}`, mapping.syntax.span),
                )
              } else rejectIncompatibleMapping()
              continue
            }
            const binding = witnessBinding(implementation, declaredParameters)
            // A witness may only ask for what the header already promises. Its own bounds are the
            // obligations its body will discharge, so a bound the header never requires would be
            // proved nowhere and would surface as a call with no lowering rather than a diagnostic.
            const unpromisedBound = unpromisedWitnessBound(
              binding,
              inference.arguments,
              conformance,
            )
            if (unpromisedBound !== undefined) {
              diagnostics.push(
                invalidDiagnostic(
                  `${target.spelling} requires ${unpromisedBound.bound?.spelling ?? 'a bound'} for ${unpromisedBound.type.name}, which ${capability.name} for ${Type.encode(provider)} does not require`,
                  mapping.syntax.span,
                ),
              )
              continue
            }
            const compatibility = interfaceWitnessCompatibility(
              mapping.contract,
              implementation,
              inference.substitution,
            )
            if (compatibility === undefined || compatibility._tag === 'Incompatible')
              rejectIncompatibleMapping(
                compatibility === undefined
                  ? undefined
                  : InterfaceWitnessCompatibility.describe(compatibility),
              )
            else inferredWitnessArguments.set(mapping, inference.arguments)
            continue
          }
          const operation =
            target._tag === 'TypePath' &&
            target.segments.length === 2 &&
            target.segments.at(0)?.spelling === 'Intrinsic'
              ? Intrinsic.findOperation('Intrinsic', target.segments.at(1)?.spelling ?? '')
              : undefined
          const rule = operation?.rule
          const parameters = rule?._tag === 'BuiltinRule' ? rule.parameters : undefined
          const result = rule?._tag === 'BuiltinRule' ? rule.result : undefined
          const compatibility =
            parameters === undefined || result === undefined
              ? undefined
              : intrinsicWitnessCompatibility(mapping.contract, parameters, result)
          if (
            operation === undefined ||
            operation.unsafe ||
            compatibility === undefined ||
            compatibility._tag === 'Incompatible'
          )
            rejectIncompatibleMapping(
              compatibility === undefined
                ? undefined
                : InterfaceWitnessCompatibility.describe(compatibility),
            )
        }
        continue
      }

      if (!Type.isNominal(provider)) continue

      if (sourceService !== undefined) {
        if (conformance.hook !== undefined) {
          diagnostics.push(
            invalidDiagnostic(
              `${capability.name} implementations use operation mappings, not a hook body`,
              conformance.hook.syntax.span,
            ),
          )
          continue
        }
        const mapped = new Map<string, ConformanceFact['operations'][number]>()
        let invalidMappingSet = false
        for (const mapping of conformance.operations) {
          if (mapping.name._tag !== 'Present') {
            markInvalid()
            invalidMappingSet = true
            continue
          }
          if (mapped.has(mapping.name.spelling)) {
            diagnostics.push(
              invalidDiagnostic(
                `duplicate ${capability.name}.${mapping.name.spelling} operation mapping`,
                mapping.syntax.span,
              ),
            )
            invalidMappingSet = true
          } else mapped.set(mapping.name.spelling, mapping)
        }
        const serviceNames = new Set(
          sourceService.operations.flatMap((operation) =>
            operation.name._tag === 'Present' ? [operation.name.spelling] : [],
          ),
        )
        const missing = [...serviceNames].filter((operation) => !mapped.has(operation))
        const extra = [...mapped.keys()].filter((operation) => !serviceNames.has(operation))
        if (missing.length > 0 || extra.length > 0) {
          diagnostics.push(
            invalidDiagnostic(
              [
                ...(missing.length === 0 ? [] : [`missing ${missing.join(', ')}`]),
                ...(extra.length === 0 ? [] : [`unknown ${extra.join(', ')}`]),
              ].join('; '),
              conformance.syntax.span,
            ),
          )
          invalidMappingSet = true
        }
        if (invalidMappingSet) continue
        const serviceSubstitution = Type.substitution(
          sourceService.typeParameters.map((parameter) => parameter.type),
          capability.arguments,
        )
        if (serviceSubstitution === undefined) {
          diagnostics.push(
            invalidDiagnostic(
              `${capability.name} implementation has the wrong service type-argument arity`,
              conformance.syntax.span,
            ),
          )
          continue
        }
        const providerModule = modules.find((candidate) => candidate.module === provider.module)
        for (const contract of sourceService.operations) {
          if (contract.name._tag !== 'Present') continue
          const mapping = mapped.get(contract.name.spelling)
          if (mapping === undefined) continue
          const target = mapping.target
          if (
            target._tag !== 'TypePath' ||
            target.segments.length !== 2 ||
            target.segments.at(0)?.spelling !== provider.name
          ) {
            diagnostics.push(
              invalidDiagnostic(
                `${contract.name.spelling} must map to an operation in the ${provider.name} actor`,
                mapping.syntax.span,
              ),
            )
            continue
          }
          const targetName = target.segments.at(1)?.spelling
          const implementation = providerModule?.declarations.find(
            (declaration) =>
              targetName !== undefined &&
              declaration.name._tag === 'Present' &&
              declaration.name.spelling === targetName,
          )
          if (implementation === undefined) {
            diagnostics.push(
              invalidDiagnostic(
                `mapped operation ${provider.name}.${targetName ?? '_'} does not exist`,
                mapping.syntax.span,
              ),
            )
            continue
          }
          const binding = witnessBinding(implementation, declaredParameters)
          const unpromisedBound = unpromisedWitnessBound(
            binding,
            declaredParameters.map(Type.parameterArgument),
            conformance,
          )
          if (unpromisedBound !== undefined) {
            diagnostics.push(
              invalidDiagnostic(
                `${target.spelling} requires ${unpromisedBound.bound?.spelling ?? 'a bound'} for ${unpromisedBound.type.name}, which ${capability.name} for ${Type.encode(provider)} does not require`,
                mapping.syntax.span,
              ),
            )
            continue
          }
          const specialize = (type: Type.Type): Type.Type =>
            binding.substitution === undefined ? type : Type.substitute(type, binding.substitution)
          const self = implementation.parameters.at(0)?.declaredType
          const validSelf =
            binding.substitution !== undefined &&
            self?._tag === 'Resolved' &&
            Type.isReference(self.type) &&
            Type.equals(specialize(self.type.target), provider)
          const implementationParameters = implementation.parameters.slice(1)
          const validParameters =
            implementationParameters.length === contract.parameters.length &&
            implementationParameters.every((parameter, parameterOrdinal) => {
              const expected = contract.parameters.at(parameterOrdinal)?.declaredType
              return (
                parameter.declaredType._tag === 'Resolved' &&
                expected?._tag === 'Resolved' &&
                TypeCompatibility.isCompatible(
                  TypeCompatibility.check(
                    Type.substitute(expected.type, serviceSubstitution),
                    specialize(parameter.declaredType.type),
                  ),
                )
              )
            })
          const validResult =
            implementation.returnType._tag === 'Resolved' &&
            contract.returnType._tag === 'Resolved' &&
            TypeCompatibility.isCompatible(
              TypeCompatibility.check(
                specialize(implementation.returnType.type),
                Type.substitute(contract.returnType.type, serviceSubstitution),
              ),
            )
          const implementationRows = specialize(
            Type.effect(
              Type.unit,
              implementation.failureRow.failures,
              'Shared',
              implementation.requirementRow.requirements,
              implementation.failureRow.parameters,
              implementation.requirementRow.parameters,
            ),
          )
          const contractRows = Type.substitute(
            Type.effect(
              Type.unit,
              contract.failureRow.failures,
              'Shared',
              contract.requirementRow.requirements,
              contract.failureRow.parameters,
              contract.requirementRow.parameters,
            ),
            serviceSubstitution,
          )
          const validFailures =
            Type.isEffect(implementationRows) &&
            Type.isEffect(contractRows) &&
            implementationRows.failures.every((failure) =>
              contractRows.failures.some((allowed) => Type.equals(failure, allowed)),
            ) &&
            implementationRows.failureParameters.every((parameter) =>
              contractRows.failureParameters.some((allowed) => Type.equals(parameter, allowed)),
            )
          const contractRequirements = !Type.isEffect(contractRows)
            ? []
            : contractRows.requirements.filter(
                (requirement) => !Type.equals(requirement.capability, capability),
              )
          const validRequirements =
            Type.isEffect(implementationRows) &&
            Type.isEffect(contractRows) &&
            implementationRows.requirements.every((requirement) =>
              contractRequirements.some(
                (allowed) =>
                  Type.equals(requirement.capability, allowed.capability) &&
                  requirement.role === allowed.role &&
                  (requirement.access === 'Shared' || allowed.access === 'Exclusive'),
              ),
            ) &&
            implementationRows.requirementParameters.every((parameter) =>
              contractRows.requirementParameters.some((allowed) => Type.equals(parameter, allowed)),
            )
          const contractSelf = !Type.isEffect(contractRows)
            ? undefined
            : contractRows.requirements.find((requirement) =>
                Type.equals(requirement.capability, capability),
              )
          const validSelfAccess =
            validSelf &&
            self._tag === 'Resolved' &&
            Type.isReference(self.type) &&
            (self.type.access === 'Shared' || contractSelf?.access === 'Exclusive')
          if (
            implementation.functionKind !== contract.functionKind ||
            binding.parameters.length !== declaredParameters.length ||
            !validSelfAccess ||
            !validParameters ||
            !validResult ||
            !validFailures ||
            !validRequirements ||
            !Type.isEffect(implementationRows) ||
            !Type.isEffect(contractRows)
          )
            diagnostics.push(
              invalidDiagnostic(
                `${provider.name}.${targetName ?? '_'} is incompatible with ${capability.name}.${contract.name.spelling}`,
                mapping.syntax.span,
              ),
            )
        }
        continue
      }

      if (Type.equals(capability, Type.dropCapability)) {
        const hook = conformance.hook
        if (conformance.operations.length !== 0 || hook === undefined) {
          diagnostics.push(
            rejectConformance(
              Diagnostic.invalidDropHook(
                'Drop requires one inline fn drop hook and no operation mappings',
                conformance.syntax.span,
              ),
            ),
          )
          continue
        }
        const parameter = hook.parameterType
        const validSelf =
          parameter._tag === 'Resolved' &&
          Type.isReference(parameter.type) &&
          parameter.type.access === 'Exclusive' &&
          Type.equals(parameter.type.target, provider)
        if (
          hook.name._tag !== 'Present' ||
          hook.name.spelling !== 'drop' ||
          hook.functionKind !== 'Ordinary' ||
          hook.typeParameterCount !== 0 ||
          hook.parameterCount !== 1 ||
          hook.parameterName._tag !== 'Present' ||
          hook.parameterName.spelling !== 'self' ||
          !validSelf ||
          hook.returnType._tag !== 'Resolved' ||
          !Type.equals(hook.returnType.type, Type.unit) ||
          hook.failureRow.failures.length !== 0 ||
          hook.requirementRow.requirements.length !== 0
        ) {
          diagnostics.push(
            rejectConformance(
              Diagnostic.invalidDropHook(
                'the hook must be fn drop(self: &mut Provider) -> () with no generics, failures, or requirements',
                hook.syntax.span,
              ),
            ),
          )
        } else if (Type.isConcrete(provider) && isCopyType(provider)) {
          // A parametric provider's Copy-ness depends on its arguments, so the prohibition is
          // enforced per instantiation during monomorphization instead of at the header.
          diagnostics.push(
            rejectConformance(
              Diagnostic.invalidDropHook(
                `Copy type ${Type.encode(provider)} cannot implement Drop`,
                conformance.syntax.span,
              ),
            ),
          )
        }
        continue
      }

      if (Type.equals(capability, Type.reportCapability)) {
        if (conformance.operations.length !== 0 || conformance.hook !== undefined) {
          diagnostics.push(
            invalidDiagnostic(
              'Report is an operation-free marker capability',
              conformance.syntax.span,
            ),
          )
        }
        continue
      }

      diagnostics.push(
        invalidDiagnostic(
          `unsupported compiler-sealed capability ${Type.encode(capability)}`,
          conformance.syntax.span,
        ),
      )
    }
  }

  modules = modules.map((module) =>
    Object.freeze({
      ...module,
      conformances: Object.freeze(
        module.conformances.map((conformance) =>
          Object.freeze({
            ...conformance,
            operations: Object.freeze(
              conformance.operations.map((operation) => {
                const targetArguments = inferredWitnessArguments.get(operation)
                return targetArguments === undefined
                  ? operation
                  : Object.freeze({ ...operation, targetArguments })
              }),
            ),
            validity:
              invalidConformances.has(conformance) ||
              conformance.coherence._tag !== 'Coherent' ||
              conformance.termination._tag !== 'Terminating'
                ? Object.freeze({ _tag: 'InvalidConformance' as const })
                : Object.freeze({ _tag: 'ValidConformance' as const }),
          }),
        ),
      ),
    }),
  )

  for (const module of modules) {
    for (const member of module.members) {
      if (member._tag === 'ConstantDeclaration') continue
      if (member._tag === 'FunctionDeclaration') {
        for (const parameter of member.parameters) {
          if (
            parameter.declaredType._tag === 'Resolved' &&
            containsPositionRestrictedBorrow(parameter.declaredType.type) &&
            (!(
              Type.isSlice(parameter.declaredType.type) ||
              Type.isReference(parameter.declaredType.type) ||
              Type.isSlot(parameter.declaredType.type)
            ) ||
              containsPositionRestrictedBorrow(
                Type.isSlice(parameter.declaredType.type)
                  ? parameter.declaredType.type.element
                  : Type.isReference(parameter.declaredType.type)
                    ? parameter.declaredType.type.target
                    : (Type.typeArgumentAt(parameter.declaredType.type, 0) ?? 'never'),
              ))
          ) {
            diagnostics.push(
              Diagnostic.sliceTypePosition('parameter', parameter.declaredType.syntax.span),
            )
          }
        }
        if (
          member.returnType._tag === 'Resolved' &&
          containsPositionRestrictedBorrow(member.returnType.type) &&
          (!Type.isSlot(member.returnType.type) ||
            containsPositionRestrictedBorrow(
              Type.typeArgumentAt(member.returnType.type, 0) ?? 'never',
            )) &&
          (!Type.isSlice(member.returnType.type) || returnedBorrow(member) === undefined)
        ) {
          diagnostics.push(
            Type.isSlice(member.returnType.type)
              ? Diagnostic.invalidReturnedBorrowSignature(member.returnType.syntax.span)
              : Diagnostic.sliceTypePosition('return', member.returnType.syntax.span),
          )
        }
        continue
      }
      if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        for (const operation of member.operations) {
          for (const parameter of operation.parameters) {
            if (
              parameter.declaredType._tag === 'Resolved' &&
              containsPositionRestrictedBorrow(parameter.declaredType.type) &&
              (!(
                Type.isSlice(parameter.declaredType.type) ||
                Type.isReference(parameter.declaredType.type) ||
                Type.isSlot(parameter.declaredType.type)
              ) ||
                containsPositionRestrictedBorrow(
                  Type.isSlice(parameter.declaredType.type)
                    ? parameter.declaredType.type.element
                    : Type.isReference(parameter.declaredType.type)
                      ? parameter.declaredType.type.target
                      : (Type.typeArgumentAt(parameter.declaredType.type, 0) ?? 'never'),
                ))
            )
              diagnostics.push(
                Diagnostic.sliceTypePosition('parameter', parameter.declaredType.syntax.span),
              )
          }
          if (
            operation.returnType._tag === 'Resolved' &&
            containsPositionRestrictedBorrow(operation.returnType.type) &&
            (!Type.isSlot(operation.returnType.type) ||
              containsPositionRestrictedBorrow(
                Type.typeArgumentAt(operation.returnType.type, 0) ?? 'never',
              ))
          )
            diagnostics.push(
              Diagnostic.sliceTypePosition('return', operation.returnType.syntax.span),
            )
        }
        continue
      }
      for (const field of member.fields) {
        if (
          field.declaredType._tag === 'Resolved' &&
          containsPositionRestrictedBorrow(field.declaredType.type)
        ) {
          diagnostics.push(Diagnostic.sliceTypePosition('field', field.declaredType.syntax.span))
        }
      }
    }
  }

  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member.visibility !== 'Public') return member
      if (member._tag === 'ConstantDeclaration') {
        return Object.freeze({
          ...member,
          declaredType: attachExposure(member.declaredType, modules, diagnostics),
        })
      }
      if (member._tag === 'FunctionDeclaration') {
        const parameters = member.parameters.map((parameter) =>
          Object.freeze({
            ...parameter,
            declaredType: attachExposure(parameter.declaredType, modules, diagnostics),
          }),
        )
        return Object.freeze({
          ...member,
          parameters: Object.freeze(parameters),
          returnType: attachExposure(member.returnType, modules, diagnostics),
          failureRow: Object.freeze({
            ...member.failureRow,
            members: Object.freeze(
              member.failureRow.members.map((failure) =>
                attachExposure(failure, modules, diagnostics),
              ),
            ),
          }),
        })
      }
      if (member._tag === 'ServiceDeclaration' || member._tag === 'InterfaceDeclaration') {
        const operations = member.operations.map((operation) =>
          Object.freeze({
            ...operation,
            parameters: Object.freeze(
              operation.parameters.map((parameter) =>
                Object.freeze({
                  ...parameter,
                  declaredType: attachExposure(parameter.declaredType, modules, diagnostics),
                }),
              ),
            ),
            returnType: attachExposure(operation.returnType, modules, diagnostics),
            failureRow: Object.freeze({
              ...operation.failureRow,
              members: Object.freeze(
                operation.failureRow.members.map((failure) =>
                  attachExposure(failure, modules, diagnostics),
                ),
              ),
            }),
          }),
        )
        const exposed = Object.freeze({ ...member, operations: Object.freeze(operations) })
        return exposed._tag === 'InterfaceDeclaration'
          ? Object.freeze({
              ...exposed,
              operationContracts: interfaceOperationContracts(exposed.typeParameters, operations),
            })
          : exposed
      }
      const fields = member.fields.map((field) =>
        field.visibility === 'Public'
          ? Object.freeze({
              ...field,
              declaredType: attachExposure(field.declaredType, modules, diagnostics),
            })
          : field,
      )
      return Object.freeze({ ...member, fields: Object.freeze(fields) })
    })
    return Object.freeze({
      ...module,
      members: Object.freeze(members),
      declarations: Object.freeze(
        members.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
      services: Object.freeze(
        members.filter((member): member is ServiceFact => member._tag === 'ServiceDeclaration'),
      ),
      interfaces: Object.freeze(
        members.filter((member): member is InterfaceFact => member._tag === 'InterfaceDeclaration'),
      ),
      constants: Object.freeze(
        members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
      ),
    })
  })

  const structs = modules.flatMap((module) => module.structs)
  // One graph, two readers: the component walk and the self-edge test below must agree about what
  // "inline" means, or a struct that reaches itself through an indirection is a component of one
  // in the first and a cycle in the second.
  const inlineParameters = inlineParametersOf(structs)
  const cycleCause = new Map<string, Diagnostic.Identity>()
  for (const component of stronglyConnected(structs, inlineParameters)) {
    const first = component.at(0)
    if (first === undefined) continue
    const keys = component.flatMap((struct) =>
      struct.canonical._tag === 'Canonical' ? [canonicalKey(struct.canonical.id)] : [],
    )
    const selfEdge =
      keys.length === 1 &&
      first.fields.some((field) =>
        inlineNeighbors(field, inlineParameters).some((neighbor) => neighbor === keys[0]),
      )
    if (keys.length < 2 && !selfEdge) continue
    const diagnostic = Diagnostic.inlineRecursiveStruct(
      Object.freeze(keys),
      first.name._tag === 'Present' ? first.name.token.span : first.syntax.span,
    )
    diagnostics.push(diagnostic)
    const cause = Diagnostic.identity(diagnostic)
    for (const key of keys) cycleCause.set(key, cause)
  }

  modules = modules.map((module): ModuleHeaders => {
    const members = module.members.map((member): MemberFact => {
      if (member._tag !== 'StructDeclaration') return member
      const dependencyMap = new Map<string, Type.Nominal>()
      for (const field of member.fields) {
        if (field.declaredType._tag === 'Resolved') {
          for (const type of Type.nominals(field.declaredType.type)) {
            dependencyMap.set(Type.key(type), type)
          }
        }
      }
      const dependencies = [...dependencyMap.values()].sort(Type.compare)
      const fieldCause = member.fields.find(
        (field) =>
          (field.declaredType._tag === 'Unresolved' && field.declaredType.cause !== undefined) ||
          (field.declaredType._tag === 'Resolved' &&
            field.declaredType.exposureCause !== undefined),
      )
      const key =
        member.canonical._tag === 'Canonical' ? canonicalKey(member.canonical.id) : undefined
      const cause =
        (key === undefined ? undefined : cycleCause.get(key)) ??
        (fieldCause?.declaredType._tag === 'Unresolved'
          ? fieldCause.declaredType.cause
          : fieldCause?.declaredType._tag === 'Resolved'
            ? fieldCause.declaredType.exposureCause
            : undefined)
      return Object.freeze({
        ...member,
        dependency: Object.freeze(
          cause === undefined
            ? { _tag: 'Available', types: Object.freeze(dependencies) }
            : { _tag: 'Unavailable', types: Object.freeze(dependencies), cause },
        ),
      })
    })
    const moduleDiagnostics = diagnostics.filter(
      (diagnostic) => diagnostic.span.sourceId === module.module,
    )
    return Object.freeze({
      ...module,
      members: Object.freeze(members),
      declarations: Object.freeze(
        members.filter(
          (member): member is DeclarationFact => member._tag === 'FunctionDeclaration',
        ),
      ),
      structs: Object.freeze(
        members.filter((member): member is StructFact => member._tag === 'StructDeclaration'),
      ),
      services: Object.freeze(
        members.filter((member): member is ServiceFact => member._tag === 'ServiceDeclaration'),
      ),
      interfaces: Object.freeze(
        members.filter((member): member is InterfaceFact => member._tag === 'InterfaceDeclaration'),
      ),
      constants: Object.freeze(
        members.filter((member): member is ConstantFact => member._tag === 'ConstantDeclaration'),
      ),
      diagnostics: Diagnostic.merge(moduleDiagnostics),
    })
  })

  return Object.freeze({
    _tag: 'DeclarationIndex',
    stage: 'Complete',
    modules: Object.freeze(modules),
    diagnostics: Diagnostic.merge(diagnostics),
  })
}

const interfaceByCapability = (self: Index, capability: Type.Nominal): InterfaceFact | undefined =>
  self.modules
    .find((module) => module.module === capability.module)
    ?.interfaces.find(
      (candidate) =>
        candidate.canonical._tag === 'Canonical' && candidate.canonical.id.name === capability.name,
    )

/**
 * Completed proofs, per index.
 *
 * Only proved goals are remembered. A failure carries the chain of goals that reached it, and that
 * chain belongs to the path rather than to the goal, so caching one would make a later diagnostic
 * describe whichever specialization happened to ask first.
 */
const proofMemos = new WeakMap<Index, Map<string, ConformanceGoal.Proof>>()

/** One conformance whose head covers a concrete goal, with the arguments matching it bound. */
interface ConformanceCandidate {
  readonly module: string
  readonly conformance: ConformanceFact
  readonly substitution: Type.Substitution
}

/**
 * Returns every conformance whose head covers one concrete goal.
 *
 * A head that was rejected for overlap or for declaring a requirement that does not descend is not
 * a candidate: admitting it would make the selected witness depend on which of two ambiguous
 * declarations was reached first, or would let proof search follow a step it cannot bound.
 */
const conformanceCandidates = (
  self: Index,
  goal: ConformanceGoal.ConformanceGoal,
  admission: 'Selectable' | 'Declared' = 'Selectable',
): ReadonlyArray<ConformanceCandidate> =>
  Object.freeze(
    self.modules.flatMap((module) =>
      module.conformances.flatMap((conformance): ReadonlyArray<ConformanceCandidate> => {
        if (
          conformance.capability._tag !== 'Resolved' ||
          !Type.isNominal(conformance.capability.type) ||
          conformance.provider._tag !== 'Resolved' ||
          (admission === 'Selectable' && conformance.validity._tag !== 'ValidConformance') ||
          conformance.coherence._tag !== 'Coherent' ||
          conformance.termination._tag !== 'Terminating'
        )
          return []
        const inferred = new Map<string, Type.GenericArgument>()
        if (!Type.infer(conformance.provider.type, goal.provider, inferred)) return []
        if (!Type.infer(conformance.capability.type, goal.capability, inferred)) return []
        return Object.freeze([
          Object.freeze({ module: module.module, conformance, substitution: inferred }),
        ])
      }),
    ),
  )

const provedGoal = (
  goal: ConformanceGoal.ConformanceGoal,
  selection: ConformanceGoal.Selection,
  typeArguments: ReadonlyArray<Type.GenericArgument>,
  requirements: ReadonlyArray<ConformanceGoal.Proof>,
): ConformanceGoal.Proof =>
  Object.freeze({
    _tag: 'Proved' as const,
    goal,
    selection,
    typeArguments: Object.freeze([...typeArguments]),
    requirements: Object.freeze([...requirements]),
  })

const unprovedGoal = (
  goal: ConformanceGoal.ConformanceGoal,
  failure: ConformanceGoal.Failure,
  trace: ReadonlyArray<ConformanceGoal.ConformanceGoal>,
): ConformanceGoal.Proof =>
  Object.freeze({
    _tag: 'Unproved' as const,
    goal,
    failure,
    trace: Object.freeze([...trace]),
  })

const proveGoal = (
  self: Index,
  goal: ConformanceGoal.ConformanceGoal,
  memo: Map<string, ConformanceGoal.Proof>,
  active: ReadonlyArray<ConformanceGoal.ConformanceGoal>,
): ConformanceGoal.Proof => {
  const goalKey = ConformanceGoal.key(goal)
  const completed = memo.get(goalKey)
  if (completed !== undefined) return completed
  // An in-progress goal cannot satisfy itself. Declaration-time descent already proves the search
  // finite, so reaching this means a fact was damaged; the answer recovers the path rather than
  // admitting a coinductive proof, and is deliberately not remembered.
  if (active.some((entry) => ConformanceGoal.key(entry) === goalKey))
    return unprovedGoal(goal, Object.freeze({ _tag: 'ActiveCycle' as const }), active)
  const proof = ((): ConformanceGoal.Proof => {
    if (Type.isNominal(goal.provider)) {
      if (Type.equals(goal.provider, goal.capability))
        return provedGoal(goal, Object.freeze({ _tag: 'IdentitySelection' as const }), [], [])
      if (Type.intrinsicallyConforms(goal.provider, goal.capability))
        return provedGoal(goal, Object.freeze({ _tag: 'IntrinsicSelection' as const }), [], [])
    }
    const matching = conformanceCandidates(self, goal)
    const selected = matching.at(0)
    if (matching.length === 0 || selected === undefined)
      return unprovedGoal(goal, Object.freeze({ _tag: 'MissingWitness' as const }), active)
    if (matching.length > 1)
      return unprovedGoal(
        goal,
        Object.freeze({ _tag: 'AmbiguousWitness' as const, candidates: matching.length }),
        active,
      )
    const nested = Object.freeze([...active, goal])
    const requirements: Array<ConformanceGoal.Proof> = []
    for (const requirement of declaredRequirements(self.modules, selected.conformance)) {
      const capability = Type.substitute(requirement.capability, selected.substitution)
      const provider = Type.substitute(requirement.provider, selected.substitution)
      if (!Type.isNominal(capability))
        return unprovedGoal(
          goal,
          Object.freeze({
            _tag: 'UnavailableWitness' as const,
            reason: 'a declared requirement did not resolve to an interface',
          }),
          active,
        )
      const proved = proveGoal(self, ConformanceGoal.make(capability, provider), memo, nested)
      // A failed requirement is reported as itself: the goal that has no witness is the useful
      // one, and the chain that asked for it travels with it.
      if (proved._tag === 'Unproved') return proved
      requirements.push(proved)
    }
    return provedGoal(
      goal,
      Object.freeze({
        _tag: 'SourceSelection' as const,
        module: selected.module,
        ordinal: selected.conformance.ordinal,
      }),
      selected.conformance.typeParameters
        .filter((parameter) => parameter.duplicateOf === undefined)
        .map((parameter) => selected.substitution.get(Type.key(parameter.type)) ?? parameter.type),
      requirements,
    )
  })()
  if (proof._tag === 'Proved') memo.set(goalKey, proof)
  return proof
}

/**
 * Proves one concrete conformance goal, following every declared requirement to a base witness.
 *
 * The search is finite without a fuel budget or a depth limit. Declaration-time validation proved
 * that each requirement names a strictly smaller provider, so the chain of goals reachable from one
 * concrete provider is bounded by that provider's own term size.
 */
export const prove = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ConformanceGoal.Proof => {
  const remembered = proofMemos.get(self)
  const memo = remembered ?? new Map<string, ConformanceGoal.Proof>()
  if (remembered === undefined) proofMemos.set(self, memo)
  return proveGoal(self, ConformanceGoal.make(capability, provider), memo, Object.freeze([]))
}

const interfaceConformance = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ConformanceFact | undefined => {
  const proof = prove(self, provider, capability)
  if (proof._tag !== 'Proved' || proof.selection._tag !== 'SourceSelection') return undefined
  return selectedConformance(self, proof.selection)
}

const selectedConformance = (
  self: Index,
  selection: Extract<ConformanceGoal.Selection, { readonly _tag: 'SourceSelection' }>,
): ConformanceFact | undefined =>
  self.modules
    .find((module) => module.module === selection.module)
    ?.conformances.find((conformance) => conformance.ordinal === selection.ordinal)

/** Tests whether one nominal provider has a compiler-shipped or source-declared witness. */
export const conforms = (self: Index, provider: Type.Type, capability: Type.Nominal): boolean =>
  interfaceByCapability(self, capability) !== undefined
    ? prove(self, provider, capability)._tag === 'Proved'
    : witness(self, provider, capability) !== undefined

/**
 * Returns, in declaration order, the operations one interface declares that the provider's selected
 * declared conformance leaves unmapped. Rejected declarations remain visible here only to preserve
 * the most specific diagnostic after selection excludes them. An empty result means the declaration
 * covers the whole contract. A capability that is not an interface, or a provider with no single
 * coherent, terminating declaration, has no partial witness to report and returns nothing.
 */
export const unmappedInterfaceOperations = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ReadonlyArray<string> => {
  const interface_ = interfaceByCapability(self, capability)
  const declared = conformanceCandidates(
    self,
    ConformanceGoal.make(capability, provider),
    'Declared',
  )
  const conformance = declared.length === 1 ? declared.at(0)?.conformance : undefined
  if (interface_ === undefined || conformance === undefined) return Object.freeze([])
  const mapped = new Set(
    conformance.operations.flatMap((mapping) =>
      mapping.name._tag === 'Present' ? [mapping.name.spelling] : [],
    ),
  )
  return Object.freeze(
    interface_.operations.flatMap((operation) =>
      operation.name._tag === 'Present' && !mapped.has(operation.name.spelling)
        ? [operation.name.spelling]
        : [],
    ),
  )
}

/**
 * Selects the compiler-known operation one provider's interface conformance maps an operation to.
 *
 * An operator on a bound-typed operand needs no witness: its lowering is width-neutral, and the
 * specialized operand type alone selects the concrete instruction. An operation no operator spells
 * has no such lowering, so its call reads the witness the specialization selected — two providers
 * of one interface may map one operation to two unrelated instructions, and only the conformance
 * says which. A provider with no single conformance, or a mapping that is not a two-segment
 * `Intrinsic` path, selects nothing.
 */
export const interfaceOperationIntrinsic = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
  operation: string,
): Intrinsic.Operation | undefined => {
  const mapping = interfaceConformance(self, provider, capability)?.operations.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === operation,
  )
  const target = mapping?.target
  if (
    target?._tag !== 'TypePath' ||
    target.segments.length !== 2 ||
    target.segments.at(0)?.spelling !== 'Intrinsic'
  )
    return undefined
  return Intrinsic.findOperation('Intrinsic', target.segments.at(1)?.spelling ?? '')
}

/**
 * Selects the provider's own function that its interface conformance maps one operation to.
 *
 * Nothing is returned when the mapping names a sealed intrinsic instead, which is the whole answer
 * for every scalar witness: specialization keeps lowering those operators to the compiler-known
 * operation and only a source-declared target redirects the call to ordinary Silk.
 */
export const interfaceWitnessImplementation = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
  operation: string,
): CanonicalId | undefined => {
  const conformance = interfaceConformance(self, provider, capability)
  return conformance === undefined
    ? undefined
    : witnessImplementation(self, provider, conformance, operation)
}

const witnessImplementation = (
  self: Index,
  provider: Type.Type,
  conformance: ConformanceFact,
  operation: string,
): CanonicalId | undefined => {
  if (!Type.isNominal(provider)) return undefined
  const mapping = conformance.operations.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === operation,
  )
  const target = mapping?.target
  if (
    target?._tag !== 'TypePath' ||
    target.segments.length !== 2 ||
    target.segments.at(0)?.spelling !== provider.name
  )
    return undefined
  const targetName = target.segments.at(1)?.spelling
  const declaration = self.modules
    .find((module) => module.module === provider.module)
    ?.declarations.find(
      (candidate) =>
        targetName !== undefined &&
        candidate.name._tag === 'Present' &&
        candidate.name.spelling === targetName,
    )
  return declaration?.canonical._tag === 'Canonical' ? declaration.canonical.id : undefined
}

/** One source witness together with the arguments its own specialization needs. */
export interface InterfaceWitnessTarget {
  readonly implementation: CanonicalId
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  /** The concrete provider whose terminating conditional proof selected this target. */
  readonly structuralProvider?: Type.Type
}

const inferredTargetArguments = (
  conformance: ConformanceFact,
  mapping: ConformanceFact['operations'][number],
  proof: Extract<ConformanceGoal.Proof, { readonly _tag: 'Proved' }>,
): ReadonlyArray<Type.GenericArgument> | undefined => {
  if (mapping.targetArguments === undefined) return undefined
  const headerParameters = conformance.typeParameters
    .filter((parameter) => parameter.duplicateOf === undefined)
    .map((parameter) => parameter.type)
  const headerSubstitution = Type.substitution(headerParameters, proof.typeArguments)
  if (headerSubstitution === undefined) return undefined
  const arguments_ = Object.freeze(
    mapping.targetArguments.map((argument) =>
      Type.substituteGenericArgument(argument, headerSubstitution),
    ),
  )
  return arguments_.every(Type.isConcreteGenericArgument) ? arguments_ : undefined
}

/**
 * Selects the provider's own function one interface operation maps to, with its type arguments.
 *
 * A conditional conformance's witness is generic in the header's binders, so naming the function is
 * not enough to reach code: the call needs the arguments this specialization bound. They come from
 * the proof rather than from the call site, because the proof is what decided which header covers
 * this provider.
 */
export const interfaceWitnessTarget = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
  operation: string,
): InterfaceWitnessTarget | undefined => {
  const proof = prove(self, provider, capability)
  if (proof._tag !== 'Proved' || proof.selection._tag !== 'SourceSelection') return undefined
  const conformance = selectedConformance(self, proof.selection)
  if (conformance === undefined) return undefined
  const mapping = conformance.operations.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === operation,
  )
  if (mapping === undefined) return undefined
  const implementation = witnessImplementation(self, provider, conformance, operation)
  const typeArguments = inferredTargetArguments(conformance, mapping, proof)
  if (implementation === undefined || typeArguments === undefined) return undefined
  return Object.freeze({
    implementation,
    typeArguments,
    ...(proof.requirements.length > 0 ? { structuralProvider: provider } : {}),
  })
}

/**
 * Selects every source witness implementation a proved conditional witness rests on.
 *
 * The result is innermost first, deduplicated by concrete implementation specialization, and does
 * not include the root witness. Discovery consumes it independently of the selected operation's
 * body: a declared requirement is part of admitting the witness even when that operation never
 * invokes the required interface itself.
 */
export const witnessDependencyTargets = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ReadonlyArray<InterfaceWitnessTarget> => {
  const proof = prove(self, provider, capability)
  if (proof._tag !== 'Proved') return Object.freeze([])
  const found = new Map<string, InterfaceWitnessTarget>()
  const visit = (dependency: ConformanceGoal.Proof): void => {
    if (dependency._tag !== 'Proved') return
    for (const requirement of dependency.requirements) visit(requirement)
    if (dependency.selection._tag !== 'SourceSelection') return
    const conformance = selectedConformance(self, dependency.selection)
    if (conformance === undefined) return
    for (const mapping of conformance.operations) {
      if (mapping.name._tag !== 'Present') continue
      const implementation = witnessImplementation(
        self,
        dependency.goal.provider,
        conformance,
        mapping.name.spelling,
      )
      const typeArguments = inferredTargetArguments(conformance, mapping, dependency)
      if (implementation === undefined || typeArguments === undefined) continue
      const identity = Specialization.key({
        declaration: implementation,
        typeArguments,
      })
      if (!found.has(identity))
        found.set(
          identity,
          Object.freeze({
            implementation,
            typeArguments,
            structuralProvider: dependency.goal.provider,
          }),
        )
    }
  }
  for (const requirement of proof.requirements) visit(requirement)
  return Object.freeze([...found.values()])
}

/** Selects the unique compiler-shipped or source-declared witness for one provider. */
export const witness = (
  self: Index,
  provider: Type.Type,
  capability: Type.Nominal,
): ConformanceWitness | undefined => {
  if (!Type.isNominal(provider)) return undefined
  if (Type.equals(provider, capability)) {
    return Object.freeze({ _tag: 'IdentityConformanceWitness', capability, provider })
  }
  if (Type.intrinsicallyConforms(provider, capability)) {
    return Object.freeze({
      _tag: 'IntrinsicConformanceWitness',
      capability,
      provider,
    })
  }
  // Proof selection is the single authority for matching both the provider and capability heads.
  // Repeating only provider inference here would lose capability binders and could select a header
  // whose requirements failed.
  const proof = prove(self, provider, capability)
  if (proof._tag !== 'Proved' || proof.selection._tag !== 'SourceSelection') return undefined
  const conformance = selectedConformance(self, proof.selection)
  if (conformance === undefined) return undefined
  const service = self.modules
    .find((module) => module.module === capability.module)
    ?.services.find(
      (candidate) =>
        candidate.canonical._tag === 'Canonical' && candidate.canonical.id.name === capability.name,
    )
  const mappedNames = new Set(
    conformance.operations.flatMap((mapping) =>
      mapping.name._tag === 'Present' ? [mapping.name.spelling] : [],
    ),
  )
  const completeService =
    service !== undefined &&
    conformance.hook === undefined &&
    mappedNames.size === conformance.operations.length &&
    service.operations.every(
      (operation) => operation.name._tag === 'Present' && mappedNames.has(operation.name.spelling),
    ) &&
    conformance.operations.length === service.operations.length
  const operations =
    service === undefined
      ? Object.freeze([])
      : Object.freeze(
          service.operations.flatMap((contract) => {
            const name = contract.name._tag === 'Present' ? contract.name.spelling : undefined
            const mapping = conformance.operations.find(
              (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === name,
            )
            const targetName =
              mapping?.target._tag === 'TypePath' && mapping.target.segments.length === 2
                ? mapping.target.segments.at(1)?.spelling
                : undefined
            const implementation = self.modules
              .find((module) => module.module === provider.module)
              ?.declarations.find(
                (declaration) =>
                  targetName !== undefined &&
                  declaration.name._tag === 'Present' &&
                  declaration.name.spelling === targetName &&
                  declaration.canonical._tag === 'Canonical',
              )
            return name === undefined || implementation?.canonical._tag !== 'Canonical'
              ? []
              : [
                  Object.freeze({
                    name,
                    implementation: implementation.canonical.id,
                  }),
                ]
          }),
        )
  const completeOperationSet =
    service === undefined || operations.length === service.operations.length
  return Type.equals(capability, Type.dropCapability) ||
    (completeService && completeOperationSet) ||
    (Type.equals(capability, Type.reportCapability) &&
      conformance.operations.length === 0 &&
      conformance.hook === undefined)
    ? Object.freeze({
        _tag: 'SourceConformanceWitness',
        module: conformance.module,
        ordinal: conformance.ordinal,
        capability,
        provider,
        operations,
        typeArguments: proof.typeArguments,
      })
    : undefined
}

/** Selects one mapped source implementation from a declaration-shaped witness. */
export const witnessOperation = (
  self: Extract<ConformanceWitness, { readonly _tag: 'SourceConformanceWitness' }>,
  name: string,
): CanonicalId | undefined =>
  self.operations.find((operation) => operation.name === name)?.implementation

const presentParameterNameEntries = (parameters: ReadonlyArray<ParameterFact>) =>
  presentParameterEntries(parameters)

export const lookupParameter = (
  parameters: ReadonlyArray<ParameterFact>,
  name: string,
): ParameterLookup => {
  const matches = presentParameterNameEntries(parameters)
    .filter((entry) => entry.spelling === name)
    .map((entry) => entry.parameter)
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, parameter: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, parameters: Object.freeze(matches) })
}

const presentMemberEntries = (members: ReadonlyArray<MemberFact>) =>
  members.flatMap((declaration) =>
    declaration.name._tag === 'Present'
      ? [
          Object.freeze({
            spelling: declaration.name.spelling,
            token: declaration.name.token,
            declaration,
          }),
        ]
      : [],
  )

export const lookupMember = (members: ReadonlyArray<MemberFact>, name: string): MemberLookup => {
  const matches = presentMemberEntries(members)
    .filter((entry) => entry.spelling === name)
    .map((entry) => entry.declaration)
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, declarations: Object.freeze(matches) })
}

export const lookupDeclaration = (
  declarations: ReadonlyArray<DeclarationFact>,
  name: string,
): DeclarationLookup => {
  const matches = declarations.filter(
    (declaration) => declaration.name._tag === 'Present' && declaration.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, declarations: Object.freeze(matches) })
}

export const lookupStruct = (structs: ReadonlyArray<StructFact>, name: string): StructLookup => {
  const matches = structs.filter(
    (struct) => struct.name._tag === 'Present' && struct.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, declarations: Object.freeze(matches) })
}

export const lookupField = (fields: ReadonlyArray<FieldFact>, name: string): FieldLookup => {
  const matches = fields.filter(
    (field) => field.name._tag === 'Present' && field.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, field: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, fields: Object.freeze(matches) })
}

export const lookup = (self: Index, module: string, name: string): DeclarationLookup =>
  lookupDeclaration(
    self.modules.find((candidate) => candidate.module === module)?.declarations ??
      Object.freeze([]),
    name,
  )

export const member = (self: Index, module: string, name: string): MemberLookup =>
  lookupMember(
    self.modules.find((candidate) => candidate.module === module)?.members ?? Object.freeze([]),
    name,
  )

export const struct = (self: Index, module: string, name: string): StructLookup =>
  lookupStruct(
    self.modules.find((candidate) => candidate.module === module)?.structs ?? Object.freeze([]),
    name,
  )

/** Looks up one completed declaration by canonical identity. */
export const byCanonical = (self: Index, id: CanonicalId): MemberFact | undefined => {
  const result = member(self, id.module, id.name)
  return result._tag === 'Resolved' && result.declaration.canonical._tag === 'Canonical'
    ? result.declaration
    : undefined
}

/** Tests whether every value of this concrete type copies freely (no affine obligation). */
export const copyType = (
  self: Index,
  type: Type.Type,
  visiting: ReadonlySet<string> = new Set(),
): boolean => {
  if (Type.isBuiltin(type) || Type.isString(type) || Type.isReference(type) || Type.isSlice(type))
    return true
  if (Type.isFixedArray(type)) return copyType(self, type.element, visiting)
  if (Type.equals(type, Type.unit)) return true
  if (!Type.isNominal(type) || Type.isIntrinsicNominal(type)) return false
  const key = Type.key(type)
  if (visiting.has(key)) return false
  const declaration = byCanonical(self, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') return false
  const substitution =
    Type.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(visiting).add(key)
  return declaration.fields.every(
    (field) =>
      field.declaredType._tag === 'Resolved' &&
      copyType(self, Type.substitute(field.declaredType.type, substitution), next),
  )
}

/** Tests whether a value of this type can retain lexical storage through its fields. */
export const containsLexicalBorrow = (
  self: Index,
  type: Type.Type,
  seen: ReadonlySet<string> = new Set(),
): boolean => {
  if (Type.isString(type) || Type.isSlice(type) || Type.isReference(type)) return true
  if (Type.isFixedArray(type)) return containsLexicalBorrow(self, type.element, seen)
  if (Type.isUnion(type))
    return type.members.some((member) => containsLexicalBorrow(self, member, seen))
  if (Type.isEffect(type))
    return (
      containsLexicalBorrow(self, type.success, seen) ||
      type.failures.some((failure) => containsLexicalBorrow(self, failure, seen))
    )
  if (!Type.isNominal(type)) return false
  const key = Type.key(type)
  if (seen.has(key)) return false
  const declaration = byCanonical(self, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration')
    return type.arguments
      .filter(Type.isTypeArgument)
      .some((argument) => containsLexicalBorrow(self, argument, seen))
  const substitution =
    Type.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(seen).add(key)
  return declaration.fields.some(
    (field) =>
      field.declaredType._tag === 'Resolved' &&
      containsLexicalBorrow(self, Type.substitute(field.declaredType.type, substitution), next),
  )
}

/** One stored bare-callable occurrence that denies an aggregate type a target layout. */
export interface StoredCallable {
  /** Field names from the aggregate down to the callable-typed position; empty for elements. */
  readonly path: ReadonlyArray<string>
  readonly callable: Type.Callable
}

/**
 * Finds the first stored position at which a concrete type keeps a bare callable value.
 *
 * The walk mirrors what nominal layout planning can actually see: struct fields after
 * substitution, fixed-array and slice elements, and union members. It deliberately does not
 * descend through references (their layout is an address regardless of the target), intrinsic
 * nominals (their layouts never depend on their type arguments), or Effect types (their hidden
 * environments plan callables from concrete identities). A position this finds is exactly one
 * `Layout.layoutType` would refuse with `callable environment layout is planned from its hidden
 * concrete identity`, so a construction of the enclosing aggregate cannot receive a layout.
 */
export const storedCallable = (
  self: Index,
  type: Type.Type,
  seen: ReadonlySet<string> = new Set(),
): StoredCallable | undefined => {
  if (Type.isCallable(type)) return Object.freeze({ path: Object.freeze([]), callable: type })
  if (Type.isFixedArray(type) || Type.isSlice(type)) return storedCallable(self, type.element, seen)
  if (Type.isUnion(type)) {
    for (const member of type.members) {
      const found = storedCallable(self, member, seen)
      if (found !== undefined) return found
    }
    return undefined
  }
  if (!Type.isNominal(type) || Type.isIntrinsicNominal(type)) return undefined
  const key = Type.key(type)
  if (seen.has(key)) return undefined
  const declaration = byCanonical(self, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration') return undefined
  const substitution =
    Type.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(seen).add(key)
  for (const field of declaration.fields) {
    if (field.declaredType._tag !== 'Resolved' || field.name._tag !== 'Present') continue
    const found = storedCallable(self, Type.substitute(field.declaredType.type, substitution), next)
    if (found !== undefined)
      return Object.freeze({
        path: Object.freeze([field.name.spelling, ...found.path]),
        callable: found.callable,
      })
  }
  return undefined
}
