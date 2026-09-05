import type * as DeclarationLifetime from './DeclarationLifetime.js'
import * as Lifetime from './Lifetime.js'
import type * as AggregateIdentity from './AggregateIdentity.js'
import * as CallableContract from './CallableContract.js'
import type * as ConformanceHead from './ConformanceHead.js'
import type * as Constraint from './Constraint.js'
import type { Index } from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import * as TypeInference from './internal/TypeInference.js'
import * as Presentation from './Presentation.js'
import type * as Operator from './Operator.js'
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import type * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StaticText from './StaticText.js'
import type * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

/** One function declaration header and its syntax-owned semantic facts. */
export interface DeclarationFact {
  readonly _tag: 'FunctionDeclaration'
  readonly lifetimeElaboration?: DeclarationLifetime.Context
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly phase: 'Runtime' | 'Static'
  readonly functionKind: 'Ordinary' | 'Effect'
  readonly unsafe: boolean
  /** Present when native code supplies the body: the ABI and the logical native symbol. */
  readonly foreign?: { readonly abi: 'C'; readonly symbol: string }
  /** Native export facts for an `export "C"` function: its ABI and the C-callable symbol. */
  readonly foreignExport?: { readonly abi: 'C'; readonly symbol: string }
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly parameterCount: number
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
  readonly opaqueResult?: OpaqueResultFact
  readonly failureRow: FailureRowFact
  readonly requirementRow: RequirementRowFact
  readonly constraints: ReadonlyArray<ConstraintFact>
  readonly constraintContracts: ReadonlyArray<Constraint.Constraint>
  readonly conformanceImplementation?: {
    readonly ordinal: number
    readonly operation: string
    readonly self: Type.Parameter
  }
  /**
   * Present on a function declared inside an inherent impl. The owner's canonical identity is
   * filled in once declaration completion resolves the head; until then the head's spelling and
   * ordinal identify it. `receiver` is true when parameter zero is spelled `self` and typed as the
   * owner, which is what makes the member callable through a value later.
   */
  readonly associatedMember?: AssociatedMemberFact
  readonly bodyTemplate?: FunctionBodyTemplate
  readonly syntax: SyntaxTree.Node
}

/** The membership facts of one function declared inside an inherent impl. */
export interface AssociatedMemberFact {
  readonly ordinal: number
  readonly ownerSpelling: string
  readonly owner?: CanonicalId
  readonly name: string
  readonly self: Type.Parameter
  readonly receiver: boolean
}

/** One inherent impl head: `impl [<Binders>] Owner { ... }` before and after owner resolution. */
export interface InherentImplFact {
  readonly _tag: 'InherentImplDeclaration'
  readonly lifetimeElaboration?: DeclarationLifetime.Context
  readonly module: string
  readonly ordinal: number
  readonly self: Type.Parameter
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly ownerSpelling: string
  readonly owner: DeclaredTypeFact
  readonly validity:
    | { readonly _tag: 'Valid' }
    | { readonly _tag: 'Invalid'; readonly cause: Diagnostic.Identity }
  readonly syntax: SyntaxTree.Node
}

/** One retained function body plus its source-independent deterministic syntax encoding. */
export interface FunctionBodyTemplate {
  readonly _tag: 'FunctionBodyTemplate'
  readonly syntax: SyntaxTree.Node
  readonly canonical: string
}

/** The semantic types recognized in declaration and executable analysis. */
export type SemanticType = Type.Type

/** Resolves one retained requirement-role fact to its semantic identity when available. */
export const requirementRoleIdentity = (
  role: RequirementRoleFact,
): Type.Requirement['role'] | undefined => {
  if (role._tag === 'DefaultRole') {
    return RequirementRow.defaultRole
  }
  if (role._tag === 'ResolvedRole') {
    return role.role
  }
  return undefined
}

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

/** A deterministic variant identity nested under its owning nominal union declaration. */
export interface UnionVariantId {
  readonly _tag: 'UnionVariantId'
  readonly union: DeclarationId
  readonly ordinal: number
}

/** The aggregate declaration scope that owns one field identity. */
export type FieldOwnerId =
  | { readonly _tag: 'StructFieldOwnerId'; readonly declaration: DeclarationId }
  | { readonly _tag: 'UnionVariantFieldOwnerId'; readonly variant: UnionVariantId }

/** A deterministic field identity nested under one struct or nominal-union variant. */
export interface FieldId {
  readonly _tag: 'FieldId'
  readonly owner: FieldOwnerId
  readonly ordinal: number
}

/** Returns the top-level declaration that transitively owns one aggregate field. */
export const fieldDeclaration = (self: FieldId): DeclarationId =>
  self.owner._tag === 'StructFieldOwnerId' ? self.owner.declaration : self.owner.variant.union

/** Tests canonical local field identity without relying on object reference equality. */
export const sameFieldId = (left: FieldId, right: FieldId): boolean => {
  if (left.ordinal !== right.ordinal || left.owner._tag !== right.owner._tag) return false
  if (left.owner._tag === 'StructFieldOwnerId' && right.owner._tag === 'StructFieldOwnerId') {
    return (
      left.owner.declaration.sourceId === right.owner.declaration.sourceId &&
      left.owner.declaration.ordinal === right.owner.declaration.ordinal
    )
  }
  if (
    left.owner._tag === 'UnionVariantFieldOwnerId' &&
    right.owner._tag === 'UnionVariantFieldOwnerId'
  ) {
    return (
      left.owner.variant.union.sourceId === right.owner.variant.union.sourceId &&
      left.owner.variant.union.ordinal === right.owner.variant.union.ordinal &&
      left.owner.variant.ordinal === right.owner.variant.ordinal
    )
  }
  return false
}

/** Encodes one field identity as a stable key for maps, diagnostics, and LLVM lowering. */
export const fieldIdKey = (self: FieldId): string => {
  if (self.owner._tag === 'StructFieldOwnerId') {
    const declaration = self.owner.declaration
    return `struct:${declaration.sourceId}:${declaration.ordinal}:${self.ordinal}`
  }
  const variant = self.owner.variant
  return `union:${variant.union.sourceId}:${variant.union.ordinal}:${variant.ordinal}:${self.ordinal}`
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
  readonly lifetimeBounds?: ReadonlyArray<Lifetime.Lifetime>
  readonly implicitLifetime?: boolean
  readonly type: Type.Parameter
  readonly name: DeclaredName
  readonly syntax: SyntaxTree.Node
  readonly duplicateOf?: Type.Parameter
  readonly bounds: ReadonlyArray<BoundFact>
  readonly staticProperties: ReadonlyArray<Type.SealedStaticProperty>
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

/** Source and resolution state of one optional nominal dependency role. */
export type RequirementRoleFact =
  | { readonly _tag: 'DefaultRole' }
  | { readonly _tag: 'UnresolvedRole'; readonly path: TypePathFact }
  | {
      readonly _tag: 'ResolvedRole'
      readonly role: Type.Requirement['role']
      readonly path: TypePathFact
      readonly declaration: CanonicalId
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
      readonly _tag: 'Lifetime'
      readonly lifetime: Lifetime.Lifetime
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
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
      readonly implicitLifetimeArguments?: ReadonlyArray<Lifetime.Lifetime>
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
      readonly lifetime: Lifetime.Lifetime
      readonly access: Type.Slice['access']
      readonly element: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Reference'
      readonly lifetime: Lifetime.Lifetime
      readonly access: Type.BorrowAccess
      readonly target: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Pointer'
      readonly mutable: boolean
      readonly pointee: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Callable'
      readonly lifetimes: Type.ExecutableLifetimes
      readonly unsafe: boolean
      readonly mode: Type.CallableMode
      readonly parameters: ReadonlyArray<DeclaredTypeFact>
      readonly result: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'ForeignFunction'
      readonly parameters: ReadonlyArray<DeclaredTypeFact>
      readonly result: DeclaredTypeFact
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Applied'
      readonly implicitLifetimeArguments?: ReadonlyArray<Lifetime.Lifetime>
      readonly target: DeclaredTypeFact
      readonly arguments: ReadonlyArray<DeclaredTypeFact>
      readonly requirementRow?: {
        readonly requirements: ReadonlyArray<{
          readonly capability: DeclaredTypeFact
          readonly role: RequirementRoleFact
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
      readonly lifetimes: Type.ExecutableLifetimes
      readonly access: Type.Effect['access']
      readonly success: DeclaredTypeFact
      readonly failures: ReadonlyArray<DeclaredTypeFact>
      readonly requirements: ReadonlyArray<{
        readonly capability: DeclaredTypeFact
        readonly role: RequirementRoleFact
        readonly access: Type.Requirement['access']
        readonly syntax: SyntaxTree.Node
      }>
      readonly requirementParameters: ReadonlyArray<Type.Parameter>
      /** The complete row expression, retained when the row subtracts (`Without<R, K>`). */
      readonly requirementExpression?: RowExpressionFact
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

/** Source-shaped row syntax retained before module resolution and symbolic normalization. */
export type RowExpressionFact =
  | { readonly _tag: 'EmptyRowExpression' }
  | {
      readonly _tag: 'RowParameterExpression'
      readonly parameter: Type.Parameter
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'FailureMemberExpression'
      readonly member: DeclaredTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'RequirementMemberExpression'
      readonly capability: DeclaredTypeFact
      readonly access: Type.Requirement['access']
      readonly role: RequirementRoleFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'UnionRowExpression'
      readonly operands: ReadonlyArray<RowExpressionFact>
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'WithoutRowExpression'
      readonly source: RowExpressionFact
      readonly selected: RowExpressionFact
      readonly syntax: SyntaxTree.Node
    }
  | { readonly _tag: 'UnavailableRowExpression'; readonly syntax: SyntaxTree.Node }

export type ConstraintFact =
  | {
      readonly _tag: 'MembershipConstraint'
      readonly domain: 'Failure' | 'Requirement' | 'Unavailable'
      readonly selected: RowExpressionFact
      readonly source: RowExpressionFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'ProviderConstraint'
      readonly mode: 'Shared' | 'Exclusive' | 'Take'
      readonly provider: DeclaredTypeFact
      readonly selected: RowExpressionFact
      readonly source: RowExpressionFact
      readonly syntax: SyntaxTree.Node
    }

/** A source-retained and canonically normalized effect failure row. */
export interface FailureRowFact {
  readonly _tag: 'FailureRow'
  readonly members: ReadonlyArray<DeclaredTypeFact>
  readonly parameters: ReadonlyArray<Type.Parameter>
  readonly failures: ReadonlyArray<Type.Type>
  readonly syntax?: SyntaxTree.Node
  readonly available: boolean
  readonly expression: RowExpressionFact
  readonly row: Type.FailureRow
}

/** A source-retained and canonically normalized Effect capability requirement row. */
export interface RequirementRowFact {
  readonly _tag: 'RequirementRow'
  readonly entries: ReadonlyArray<{
    readonly capability: DeclaredTypeFact
    readonly role: RequirementRoleFact
    readonly access: Type.Requirement['access']
    readonly syntax: SyntaxTree.Node
  }>
  readonly parameters: ReadonlyArray<Type.Parameter>
  readonly requirements: ReadonlyArray<Type.Requirement>
  readonly syntax?: SyntaxTree.Node
  readonly available: boolean
  readonly expression: RowExpressionFact
  readonly row: Type.RequirementsRow
}

/** One ordered parameter declaration with exact concrete provenance. */
export interface ParameterFact {
  readonly _tag: 'ParameterDeclaration'
  readonly id: ParameterId
  readonly name: DeclaredName
  readonly phase: 'Runtime' | 'Static'
  readonly bindingMutability: 'Immutable' | 'Mutable'
  readonly declaredType: DeclaredTypeFact
  readonly syntax: SyntaxTree.Node
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
      readonly _tag: 'DurationLiteral'
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
  readonly initializerTemplate: StaticExpressionTemplate
  readonly literal: ConstantLiteralFact
  readonly initializer: SyntaxTree.Node
  readonly syntax: SyntaxTree.Node
}

/** One immutable Silk binding backed by an imported or exported C data symbol. */
export interface ForeignStaticFact {
  readonly _tag: 'ForeignStaticDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly direction: 'Import' | 'Export'
  readonly foreign: { readonly abi: 'C'; readonly symbol: string }
  readonly declaredType: DeclaredTypeFact
  readonly initializerTemplate?: StaticExpressionTemplate
  readonly literal?: ConstantLiteralFact
  readonly initializer?: SyntaxTree.Node
  readonly syntax: SyntaxTree.Node
}

/** One transparent type alias header. Its target erases to a canonical type at resolution. */
export interface AliasFact {
  readonly _tag: 'AliasDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly target: DeclaredTypeFact
  /** Retained only so semantic analysis can reject a parameterized alias at its exact span. */
  readonly parameterList?: SyntaxTree.Node
  readonly syntax: SyntaxTree.Node
}

/** One target-neutral static initializer retained with deterministic source-independent syntax. */
export interface StaticExpressionTemplate {
  readonly _tag: 'StaticExpressionTemplate'
  readonly syntax: SyntaxTree.Node
  readonly canonical: string
}

/** One nominal compile-time dependency role declaration. */
export interface RoleFact {
  readonly _tag: 'RoleDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly syntax: SyntaxTree.Node
}

/** The unique, duplicate, or unidentified state of one field name. */
export type FieldState =
  | { readonly _tag: 'Unique'; readonly id: FieldId }
  | { readonly _tag: 'Duplicate'; readonly original: FieldId; readonly cause: Diagnostic.Identity }
  | { readonly _tag: 'Unidentified' }

/** One ordered aggregate field header shared by structs and nominal-union variants. */
export interface FieldFact {
  readonly _tag: 'AggregateField'
  readonly id: FieldId
  /** Labeled source fields and positional tuple elements remain disjoint identities. */
  readonly member: AggregateIdentity.MemberIdentity
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
  readonly lifetimeElaboration?: DeclarationLifetime.Context
  readonly _tag: 'StructDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  /** The physical-layout promise retained independently of source visibility and field shape. */
  readonly layout:
    | { readonly _tag: 'Silk' }
    | { readonly _tag: 'Foreign'; readonly abi: 'C'; readonly abiSpan: SourceSpan.SourceSpan }
    | {
        readonly _tag: 'InvalidForeign'
        readonly abi: string | undefined
        readonly abiSpan: SourceSpan.SourceSpan
      }
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  /** Canonical source or literal-occurrence identity for this nominal aggregate. */
  readonly identity?: AggregateIdentity.AggregateIdentity
  /** Source structs, tuples, and anonymous literals all use one nominal aggregate fact. */
  readonly aggregateKind: 'Named' | 'Positional' | 'AnonymousNamed' | 'AnonymousPositional'
  readonly fields: ReadonlyArray<FieldFact>
  readonly dependency: StructDependency
  readonly syntax: SyntaxTree.Node
}

/** The canonical identity of one uniquely named variant of a canonical nominal union. */
export interface CanonicalUnionVariantId {
  readonly _tag: 'CanonicalUnionVariantId'
  readonly union: CanonicalId
  readonly name: string
}

export type UnionVariantCanonicalState =
  | { readonly _tag: 'Canonical'; readonly id: CanonicalUnionVariantId }
  | {
      readonly _tag: 'Duplicate'
      readonly original: CanonicalUnionVariantId
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unidentified' }

/** One source-ordered unit or named-field variant subordinate to a nominal union. */
export interface UnionVariantFact {
  readonly _tag: 'UnionVariant'
  readonly id: UnionVariantId
  readonly canonical: UnionVariantCanonicalState
  readonly name: DeclaredName
  readonly kind: 'Unit' | 'Fields'
  readonly fields: ReadonlyArray<FieldFact>
  readonly syntax: SyntaxTree.Node
}

export type UnionValidity =
  | { readonly _tag: 'Valid' }
  | { readonly _tag: 'Invalid'; readonly causes: ReadonlyArray<Diagnostic.Identity> }

/** One nominal tagged union declaration and its subordinate ordered variants. */
export interface UnionFact {
  readonly lifetimeElaboration?: DeclarationLifetime.Context
  readonly _tag: 'UnionDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly variants: ReadonlyArray<UnionVariantFact>
  readonly dependency: StructDependency
  readonly validity: UnionValidity
  readonly syntax: SyntaxTree.Node
}

/** A deterministic member identity nested under its owning enum declaration. */
export interface EnumMemberId {
  readonly _tag: 'EnumMemberId'
  readonly enum: DeclarationId
  readonly ordinal: number
}

/** The canonical identity of one uniquely named member of a canonical enum. */
export interface CanonicalEnumMemberId {
  readonly _tag: 'CanonicalEnumMemberId'
  readonly enum: CanonicalId
  readonly name: string
}

export type EnumMemberCanonicalState =
  | { readonly _tag: 'Canonical'; readonly id: CanonicalEnumMemberId }
  | {
      readonly _tag: 'Duplicate'
      readonly original: CanonicalEnumMemberId
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unidentified' }

/** The selected fixed-width representation or its explicit recovery state. */
export type EnumRepresentationFact =
  | {
      readonly _tag: 'Available'
      readonly scalar: Scalar.EnumRepresentation
      readonly explicit: boolean
      readonly syntax: SyntaxTree.Element
    }
  | {
      readonly _tag: 'Unavailable'
      readonly explicit: boolean
      readonly syntax: SyntaxTree.Element
      readonly spelling?: string
      readonly cause?: Diagnostic.Identity
    }

/** One checked discriminant or its local recovery state. */
export type EnumDiscriminantFact =
  | {
      readonly _tag: 'Available'
      readonly value: bigint
      readonly source: 'Explicit' | 'Implicit'
      readonly syntax: SyntaxTree.Element
    }
  | {
      readonly _tag: 'Unavailable'
      readonly source: 'Explicit' | 'Implicit'
      readonly syntax: SyntaxTree.Element
      readonly attempted?: bigint
      readonly cause?: Diagnostic.Identity
    }

/** One declaration-ordered, fieldless scalar-enum member. */
export interface EnumMemberFact {
  readonly _tag: 'EnumMember'
  readonly id: EnumMemberId
  readonly canonical: EnumMemberCanonicalState
  readonly name: DeclaredName
  readonly discriminant: EnumDiscriminantFact
  readonly syntax: SyntaxTree.Node
}

/** Canonical identity of one compiler-generated operation owned by an enum declaration. */
export interface EnumAssociatedOperationId {
  readonly _tag: 'EnumAssociatedOperationId'
  readonly enum: CanonicalId
  readonly name: 'value'
}

/** The generated backing-value projection contributed by one canonical enum declaration. */
export interface EnumAssociatedOperationFact {
  readonly _tag: 'EnumAssociatedOperation'
  readonly id: EnumAssociatedOperationId
  readonly name: 'value'
  readonly enum: CanonicalId
  readonly parameter: Type.Nominal
  readonly result: Scalar.EnumRepresentation
  readonly intrinsic: {
    readonly _tag: 'IntrinsicOperationId'
    readonly actor: 'Intrinsic'
    readonly name: 'enumValue'
  }
}

/** Whether all declaration-owned enum invariants were established. */
export type EnumValidity =
  | { readonly _tag: 'Valid' }
  | { readonly _tag: 'Invalid'; readonly causes: ReadonlyArray<Diagnostic.Identity> }

/** One canonical nominal scalar enum and its checked declaration-ordered member set. */
export interface EnumFact {
  readonly _tag: 'EnumDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly representation: EnumRepresentationFact
  readonly members: ReadonlyArray<EnumMemberFact>
  readonly associatedOperations: ReadonlyArray<EnumAssociatedOperationFact>
  readonly validity: EnumValidity
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
  readonly lifetimeElaboration?: DeclarationLifetime.Context
  readonly _tag: 'ServiceOperation'
  readonly id: DeclarationId
  readonly state: ServiceOperationState
  readonly functionKind: 'Ordinary' | 'Effect'
  readonly unsafe: boolean
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly parameterCount: number
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly operator?: {
    readonly operator: Operator.Eligible
    readonly token: Token.Token
    readonly syntax: SyntaxTree.Node
  }
  readonly name: DeclaredName
  readonly returnType: ReturnTypeFact
  readonly opaqueResult?: OpaqueResultFact
  readonly failureRow: FailureRowFact
  readonly requirementRow: RequirementRowFact
  readonly constraints: ReadonlyArray<ConstraintFact>
  readonly constraintContracts: ReadonlyArray<Constraint.Constraint>
  readonly syntax: SyntaxTree.Node
}

const contractParameterMode = (type: Type.Type): CallableContract.ParameterMode => {
  if (Type.isReference(type)) return type.access
  if (Type.isEffect(type)) return type.access === 'Take' ? 'Take' : type.access
  return 'Value'
}

/** Computes the retained environment from a declaration's input contracts and written bounds. */
export const executableLifetimes = (
  declaration: DeclarationFact | ServiceOperationFact,
): Type.ExecutableLifetimes => {
  const inputs = declaration.parameters.flatMap((parameter) =>
    parameter.declaredType._tag === 'Resolved' ? [parameter.declaredType.type] : [],
  )
  const retained =
    declaration.functionKind === 'Effect'
      ? [
          ...new Map(
            inputs
              .flatMap(Type.storageLifetimes)
              .filter((region) => region._tag !== 'StaticLifetime')
              .map((region) => [Lifetime.key(region), region]),
          ).values(),
        ]
      : []
  const genericStorage =
    declaration.functionKind === 'Effect'
      ? [
          ...new Map(
            inputs
              .flatMap(Type.storageParameters)
              .filter((parameter) => !parameter.staticProperties.includes('Intrinsic.Detached'))
              .map((parameter) => [Type.key(parameter), parameter]),
          ).values(),
        ]
      : []
  const lifetimeBinders = declaration.typeParameters.flatMap((parameter) =>
    parameter.type.kind === 'Lifetime'
      ? [Lifetime.bound(parameter.type.owner, parameter.type.ordinal, parameter.type.name)]
      : [],
  )
  const owner = declaration.lifetimeElaboration?.owner ??
    lifetimeBinders.at(0)?.owner ?? {
      module: declaration.id.sourceId,
      name: `function#${declaration.id.ordinal}`,
    }
  const lifetimeBounds: Array<Lifetime.Outlives> = []
  const typeOutlives: Array<Type.TypeOutlives> = []
  for (const parameter of declaration.typeParameters)
    for (const shorter of parameter.lifetimeBounds ?? []) {
      const argument = Type.parameterArgument(parameter.type)
      if (Lifetime.isLifetime(argument)) lifetimeBounds.push({ longer: argument, shorter })
      else if (Type.isTypeArgument(argument))
        typeOutlives.push({ type: argument, lifetime: shorter })
      else if (Type.isRepresentationArgument(argument)) {
        const type = Type.representedType(argument)
        if (type !== undefined) typeOutlives.push({ type, lifetime: shorter })
      }
    }
  const explicitEnvironment =
    declaration.functionKind === 'Effect'
      ? declaration.lifetimeElaboration?.explicitEnvironment
      : undefined
  let environment: Lifetime.Lifetime = Lifetime.staticLifetime
  const sole = retained.at(0)
  let synthesized = false
  if (explicitEnvironment !== undefined) environment = explicitEnvironment
  else if (retained.length === 1 && sole !== undefined && genericStorage.length === 0)
    environment = sole
  else if (retained.length > 1 || genericStorage.length > 0) {
    const assumptions = Lifetime.assumptions(lifetimeBounds)
    const proves = (longer: Lifetime.Lifetime, shorter: Lifetime.Lifetime): boolean =>
      Lifetime.outlives(assumptions, longer, shorter)
    const candidates = [
      ...new Map(
        [...retained, ...lifetimeBinders, ...typeOutlives.map((bound) => bound.lifetime)].map(
          (candidate) => [Lifetime.key(candidate), candidate],
        ),
      ).values(),
    ].filter(
      (candidate) =>
        retained.every((source) => proves(source, candidate)) &&
        genericStorage.every((type) =>
          Type.satisfiesOutlives(type, candidate, typeOutlives, proves),
        ),
    )
    const common = candidates.filter((candidate) =>
      candidates.every((other) => proves(other, candidate)),
    )
    const selected = common.length === 1 ? common.at(0) : undefined
    if (selected !== undefined) environment = selected
    else {
      const names = new Set(
        declaration.typeParameters.map((parameter) => parameter.type.name.replace(/^'/, '')),
      )
      let name = 'env'
      let suffix = 1
      while (names.has(name)) name = `env${suffix++}`
      environment = Lifetime.bound(
        owner,
        Math.max(-1, ...declaration.typeParameters.map((parameter) => parameter.type.ordinal)) + 1,
        name,
      )
      lifetimeBinders.push(environment)
      synthesized = true
    }
  }
  if (synthesized || explicitEnvironment !== undefined)
    lifetimeBounds.push(...retained.map((longer) => ({ longer, shorter: environment })))
  typeOutlives.push(...genericStorage.map((type) => ({ type, lifetime: environment })))
  return Object.freeze({
    environment,
    lifetimeBinders: Object.freeze(lifetimeBinders),
    lifetimeBounds: Lifetime.assumptions(lifetimeBounds).bounds,
    typeOutlives: Type.normalizeTypeOutlives(typeOutlives),
  })
}

/** Row normalization has no stored values and therefore no retained environment. */
const rowLifetimes: Type.ExecutableLifetimes = Object.freeze({
  environment: Lifetime.staticLifetime,
  lifetimeBinders: Object.freeze([]),
})

/** Adapts a resolved source callable to the same contract consumed by sealed intrinsics. */
export const callableContract = (
  declaration: DeclarationFact | ServiceOperationFact,
  enclosingTypeParameters: ReadonlyArray<TypeParameterFact> = Object.freeze([]),
): CallableContract.CallableContract => {
  const success = declaration.returnType._tag === 'Resolved' ? declaration.returnType.type : 'never'
  const result =
    declaration.functionKind === 'Effect'
      ? Type.effectWithRows(
          success,
          declaration.failureRow.row,
          { ...executableLifetimes(declaration), lifetimeBinders: [] },
          'Shared',
          declaration.requirementRow.row,
        )
      : success
  return CallableContract.make({
    ...executableLifetimes(declaration),
    functionKind: declaration.functionKind === 'Effect' ? 'Effect' : 'Function',
    unsafe: declaration.unsafe,
    binders: [...enclosingTypeParameters, ...declaration.typeParameters].map(
      (parameter) => parameter.type,
    ),
    parameters: declaration.parameters.flatMap((parameter) =>
      parameter.declaredType._tag === 'Resolved'
        ? [
            Object.freeze({
              type: parameter.declaredType.type,
              mode: contractParameterMode(parameter.declaredType.type),
            }),
          ]
        : [],
    ),
    result,
    constraints: declaration.constraintContracts,
  })
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
  readonly lifetimes: Type.ExecutableLifetimes
  readonly unsafe: boolean
  readonly operands: ReadonlyArray<InterfaceOperandFact>
  readonly success: ReturnTypeFact
  readonly failureRow: FailureRowFact
  readonly requirementRow: RequirementRowFact
  readonly receiverAccess: InterfaceOperandAccess
}

/** One complete interface operation after substituting a particular interface application. */
export interface InterfaceOperationApplicationFact extends Omit<
  InterfaceOperationContractFact,
  '_tag'
> {
  readonly _tag: 'InterfaceOperationApplication'
  readonly capability: Type.Nominal
  readonly provider: Type.Type
  readonly source: InterfaceOperationContractFact
}

/**
 * One interface application retained on a bound.
 *
 * `providerMatches` records that the separate `Self` substitution is a valid ordinary type.
 * Damaged applications stay visible with `available: false` and never collapse into an
 * operation-free interface.
 */
export interface InterfaceApplicationFact {
  readonly _tag: 'InterfaceApplication'
  readonly declaration: CanonicalId
  readonly capability: Type.Nominal
  readonly provider: Type.Type
  readonly providerMatches: boolean
  readonly visibility: ContractFact['visibility']
  readonly operations: ReadonlyArray<InterfaceOperationApplicationFact>
  readonly available: boolean
}

/**
 * One nominal static contract declared with either `interface` or `service`.
 *
 * `dependencyEligible` is the complete semantic difference between the two source spellings.
 * Every operation, bound, conformance, and witness consumes this shared fact.
 */
export interface ContractFact {
  readonly _tag: 'InterfaceDeclaration' | 'ServiceDeclaration'
  readonly id: DeclarationId
  readonly canonical: CanonicalState
  readonly visibility: 'Public' | 'Private'
  readonly dependencyEligible: boolean
  /** The declaration-owned implicit provider binding available as `Self` in operation contracts. */
  readonly self: Type.Parameter
  readonly typeParameters: ReadonlyArray<TypeParameterFact>
  readonly name: DeclaredName
  readonly operations: ReadonlyArray<ServiceOperationFact>
  readonly operationContracts: ReadonlyArray<InterfaceOperationContractFact>
  readonly syntax: SyntaxTree.Node
}

export type ServiceFact = ContractFact & {
  readonly _tag: 'ServiceDeclaration'
  readonly dependencyEligible: true
}

export type InterfaceFact = ContractFact & {
  readonly _tag: 'InterfaceDeclaration'
  readonly dependencyEligible: false
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
  dependencyEligible: boolean,
  capability: Type.Nominal | undefined,
): InterfaceOperationContractFact => {
  const authored = operation.parameters.map((parameter): InterfaceOperandFact =>
    Object.freeze({
      _tag: 'InterfaceOperand',
      parameter,
      type: parameter.declaredType,
      access: interfaceOperandAccess(parameter.declaredType),
    }),
  )
  // Only ambient dependency syntax (`? &Service` on the operation itself) obtains the provider
  // operand from the Effect environment; an operation written without it keeps exactly the
  // contract the equivalent interface operation has.
  const ambient =
    capability === undefined
      ? undefined
      : operation.requirementRow.requirements.find((requirement) =>
          Type.equals(requirement.capability, capability),
        )
  const serviceAccess = ambient?.access ?? 'Shared'
  const receiver =
    !dependencyEligible ||
    ambient === undefined ||
    provider === undefined ||
    operation.name._tag !== 'Present'
      ? []
      : (() => {
          const type = Type.reference(
            serviceAccess,
            provider,
            executableLifetimes(operation).environment,
          )
          const declaredType: DeclaredTypeFact = Object.freeze({
            _tag: 'Resolved',
            type,
            spelling: Type.encode(type),
            token: operation.name.token,
            syntax: operation.syntax,
          })
          const parameter: ParameterFact = Object.freeze({
            _tag: 'ParameterDeclaration',
            id: Object.freeze({ _tag: 'ParameterId', function: operation.id, ordinal: -1 }),
            name: Object.freeze({
              _tag: 'Present',
              spelling: 'self',
              token: operation.name.token,
            }),
            phase: 'Runtime',
            bindingMutability: 'Immutable',
            declaredType,
            syntax: operation.syntax,
          })
          return [
            Object.freeze({
              _tag: 'InterfaceOperand' as const,
              parameter,
              type: declaredType,
              access: serviceAccess,
            }),
          ]
        })()
  const operands = Object.freeze([...receiver, ...authored])
  return Object.freeze({
    _tag: 'InterfaceOperationContract',
    declaration: operation,
    ...(provider === undefined ? {} : { provider }),
    functionKind: operation.functionKind,
    lifetimes: executableLifetimes(operation),
    unsafe: operation.unsafe,
    operands,
    success: operation.returnType,
    failureRow: operation.failureRow,
    requirementRow: operation.requirementRow,
    receiverAccess: interfaceReceiverAccess(operands, provider),
  })
}

export const interfaceOperationContracts = (
  contract: Pick<ContractFact, 'canonical' | 'dependencyEligible' | 'self' | 'typeParameters'>,
  operations: ReadonlyArray<ServiceOperationFact>,
): ReadonlyArray<InterfaceOperationContractFact> => {
  const capability =
    contract.canonical._tag === 'Canonical'
      ? Type.nominal(
          contract.canonical.id.module,
          contract.canonical.id.name,
          contract.typeParameters.map((parameter) => Type.parameterArgument(parameter.type)),
        )
      : undefined
  return Object.freeze(
    operations.map((operation) =>
      interfaceOperationContract(operation, contract.self, contract.dependencyEligible, capability),
    ),
  )
}

const substituteDeclaredTypeFact = (
  fact: DeclaredTypeFact,
  substitution: Type.Substitution,
  module: string,
): DeclaredTypeFact => {
  if (fact._tag === 'Reference') {
    const target = substituteDeclaredTypeFact(fact.target, substitution, module)
    const lifetime = Type.substituteLifetime(fact.lifetime, substitution)
    return target === fact.target && Lifetime.equals(lifetime, fact.lifetime)
      ? fact
      : Object.freeze({
          ...fact,
          lifetime,
          target,
          spelling: `&${Lifetime.display(lifetime)} ${fact.access === 'Exclusive' ? 'mut ' : ''}${target._tag === 'Unavailable' ? '_' : target.spelling}`,
        })
  }
  // ponytail: Callable, Applied, and Union facts keep their `Self` spelling; recurse when a
  // presentation needs `fn(Self) -> U` closed too.
  if (fact._tag !== 'Resolved') return fact
  const type = Type.substitute(fact.type, substitution)
  // The spelling stays source-like: the local owner reads `Option<T>`, not `main.Option<T>`.
  return Object.freeze({ ...fact, type, spelling: Presentation.type(type, module) })
}

const substituteRowExpressionFact = (
  fact: RowExpressionFact,
  substitution: Type.Substitution,
  module: string,
): RowExpressionFact => {
  switch (fact._tag) {
    case 'EmptyRowExpression':
    case 'RowParameterExpression':
    case 'UnavailableRowExpression':
      return fact
    case 'FailureMemberExpression':
      return Object.freeze({
        ...fact,
        member: substituteDeclaredTypeFact(fact.member, substitution, module),
      })
    case 'RequirementMemberExpression': {
      return Object.freeze({
        ...fact,
        capability: substituteDeclaredTypeFact(fact.capability, substitution, module),
      })
    }
    case 'UnionRowExpression':
      return Object.freeze({
        ...fact,
        operands: Object.freeze(
          fact.operands.map((operand) =>
            substituteRowExpressionFact(operand, substitution, module),
          ),
        ),
      })
    case 'WithoutRowExpression':
      return Object.freeze({
        ...fact,
        source: substituteRowExpressionFact(fact.source, substitution, module),
        selected: substituteRowExpressionFact(fact.selected, substitution, module),
      })
  }
}

export const closeConformanceSelf = (
  declaration: DeclarationFact,
  self: Type.Parameter,
  provider: Type.Type,
): DeclarationFact => {
  const substitution: Type.Substitution = new Map<string, Type.GenericArgument>([
    [Type.key(self), provider],
  ])
  const module =
    declaration.canonical._tag === 'Canonical'
      ? declaration.canonical.id.module
      : declaration.id.sourceId
  const rowsBefore = Type.effectWithRows(
    Type.unit,
    declaration.failureRow.row,
    rowLifetimes,
    'Shared',
    declaration.requirementRow.row,
  )
  const rowsType = Type.substitute(rowsBefore, substitution)
  // Only a fact that mentions `Self` is rewritten: substituting through a type that never names
  // it would still renormalize representation binders (`?R`) that inference needs to see intact.
  const selfKey = Type.key(self)
  const mentionsSelf = (fact: DeclaredTypeFact): boolean => {
    switch (fact._tag) {
      case 'Resolved':
        return Type.parameters(fact.type).some((parameter) => Type.key(parameter) === selfKey)
      case 'Reference':
        return mentionsSelf(fact.target)
      case 'Applied':
        return mentionsSelf(fact.target) || fact.arguments.some(mentionsSelf)
      case 'FixedArray':
      case 'Slice':
        return mentionsSelf(fact.element)
      default:
        return false
    }
  }
  const closeFact = (fact: DeclaredTypeFact): DeclaredTypeFact =>
    mentionsSelf(fact) ? substituteDeclaredTypeFact(fact, substitution, module) : fact
  const rowsMentionSelf = Type.parameters(rowsBefore).some(
    (parameter) => Type.key(parameter) === selfKey,
  )
  if (
    !rowsMentionSelf &&
    !mentionsSelf(declaration.returnType) &&
    !declaration.parameters.some((parameter) => mentionsSelf(parameter.declaredType))
  )
    return declaration
  if (!rowsMentionSelf || Type.equals(rowsBefore, rowsType)) {
    return Object.freeze({
      ...declaration,
      parameters: Object.freeze(
        declaration.parameters.map((parameter) =>
          Object.freeze({ ...parameter, declaredType: closeFact(parameter.declaredType) }),
        ),
      ),
      returnType: closeFact(declaration.returnType),
    })
  }
  const rows = Type.isEffect(rowsType)
    ? rowsType
    : Type.effect(Type.unit, [], rowLifetimes, 'Shared')
  return Object.freeze({
    ...declaration,
    parameters: Object.freeze(
      declaration.parameters.map((parameter) =>
        Object.freeze({
          ...parameter,
          declaredType: substituteDeclaredTypeFact(parameter.declaredType, substitution, module),
        }),
      ),
    ),
    returnType: substituteDeclaredTypeFact(declaration.returnType, substitution, module),
    failureRow: substituteFailureRowFact(declaration.failureRow, substitution, rows, module),
    requirementRow: substituteRequirementRowFact(
      declaration.requirementRow,
      substitution,
      rows,
      module,
    ),
  })
}

const failureRowFromEffect = (effect: Type.Effect): Type.FailureRow => effect.failureRow

const requirementRowFromEffect = (effect: Type.Effect): Type.RequirementsRow =>
  Type.requirementRowParameters(effect).reduce<Type.RequirementsRow>(
    (row, parameter) =>
      RowAlgebra.union(
        Type.requirementRowPolicy(),
        row,
        RowAlgebra.parameter<Type.Requirement, Type.Parameter, Type.RequirementMemberShape>(
          parameter,
        ),
      ),
    RowAlgebra.concrete(Type.requirementRowPolicy(), Type.requirementMembers(effect)),
  )

const substituteFailureRowFact = (
  fact: FailureRowFact,
  substitution: Type.Substitution,
  rows: Type.Effect,
  module: string,
): FailureRowFact => {
  const members = Object.freeze(
    fact.members.map((member) => substituteDeclaredTypeFact(member, substitution, module)),
  )
  return Object.freeze({
    ...fact,
    members,
    expression: substituteRowExpressionFact(fact.expression, substitution, module),
    row: failureRowFromEffect(rows),
    parameters: Object.freeze([]),
    failures: Type.failureMembers(rows),
    available: members.every(
      (member) => member._tag === 'Resolved' && Type.isTypeArgument(member.type),
    ),
  })
}

const substituteRequirementRowFact = (
  fact: RequirementRowFact,
  substitution: Type.Substitution,
  rows: Type.Effect,
  module: string,
): RequirementRowFact => {
  const entries = Object.freeze(
    fact.entries.map((entry) =>
      Object.freeze({
        ...entry,
        capability: substituteDeclaredTypeFact(entry.capability, substitution, module),
      }),
    ),
  )
  return Object.freeze({
    ...fact,
    entries,
    expression: substituteRowExpressionFact(fact.expression, substitution, module),
    row: requirementRowFromEffect(rows),
    parameters: Type.requirementRowParameters(rows),
    requirements: Type.requirementMembers(rows),
    available:
      Type.requirementRowParameters(rows).length === 0 &&
      entries.every(
        (entry) =>
          entry.capability._tag === 'Resolved' &&
          ((Type.isNominal(entry.capability.type) &&
            Type.isRuntimeConcrete(entry.capability.type)) ||
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
      lifetimes: source.lifetimes,
      unsafe: source.unsafe,
      operands: source.operands,
      success: source.success,
      failureRow: source.failureRow,
      requirementRow: source.requirementRow,
      receiverAccess: source.receiverAccess,
    })
  const operands = Object.freeze(
    source.operands.map((operand): InterfaceOperandFact => {
      const type = substituteDeclaredTypeFact(operand.type, substitution, capability.module)
      return Object.freeze({
        ...operand,
        parameter: Object.freeze({ ...operand.parameter, declaredType: type }),
        type,
        access: interfaceOperandAccess(type),
      })
    }),
  )
  const substitutedRows = Type.substitute(
    Type.effectWithRows(
      Type.unit,
      source.failureRow.row,
      source.lifetimes,
      'Shared',
      source.requirementRow.row,
    ),
    substitution,
  )
  const rows = Type.isEffect(substitutedRows)
    ? substitutedRows
    : Type.effect(Type.unit, [], rowLifetimes, 'Shared')
  return Object.freeze({
    _tag: 'InterfaceOperationApplication',
    declaration: source.declaration,
    capability,
    provider,
    source,
    functionKind: source.functionKind,
    lifetimes: {
      environment: rows.environment,
      lifetimeBinders: rows.lifetimeBinders,
      lifetimeBounds: rows.lifetimeBounds,
      typeOutlives: rows.typeOutlives,
    },
    unsafe: source.unsafe,
    operands,
    success: substituteDeclaredTypeFact(source.success, substitution, capability.module),
    failureRow: substituteFailureRowFact(source.failureRow, substitution, rows, capability.module),
    requirementRow: substituteRequirementRowFact(
      source.requirementRow,
      substitution,
      rows,
      capability.module,
    ),
    receiverAccess: interfaceReceiverAccess(operands, provider),
  })
}

/** Instantiates invocation binders after an interface application has already selected its provider. */
export const instantiateInterfaceOperation = (
  self: InterfaceOperationApplicationFact,
  substitution: Type.Substitution,
): InterfaceOperationApplicationFact =>
  Object.freeze({
    ...applyInterfaceOperation(
      {
        ...self,
        _tag: 'InterfaceOperationContract',
        lifetimes: {
          ...self.lifetimes,
          lifetimeBinders: self.lifetimes.lifetimeBinders.filter(
            (binder) => !substitution.has(Lifetime.key(binder)),
          ),
        },
      },
      self.capability,
      self.provider,
      substitution,
    ),
    source: self.source,
  })

const interfaceOperationAvailable = (operation: InterfaceOperationApplicationFact): boolean =>
  operation.operands.every((operand) => operand.type._tag === 'Resolved') &&
  operation.success._tag === 'Resolved' &&
  operation.failureRow.members.every(
    (member) => member._tag === 'Resolved' && Type.isTypeArgument(member.type),
  ) &&
  operation.requirementRow.entries.every(
    (entry) =>
      entry.capability._tag === 'Resolved' &&
      (Type.isNominal(entry.capability.type) ||
        (Type.isParameter(entry.capability.type) && entry.capability.type.kind === 'Value')),
  )

export const interfaceApplication = (
  declaration: ContractFact,
  capability: Type.Nominal,
  provider: Type.Type,
): InterfaceApplicationFact | undefined => {
  if (declaration.canonical._tag !== 'Canonical') return undefined
  const providerMatches = Type.isTypeArgument(provider)
  const substitution = TypeInference.substitution(
    [declaration.self, ...declaration.typeParameters.map((parameter) => parameter.type)],
    [provider, ...capability.arguments],
  )
  const sourceContracts =
    declaration.operationContracts.length === declaration.operations.length
      ? declaration.operationContracts
      : interfaceOperationContracts(declaration, declaration.operations)
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

/** The operation-free application carried by the compiler-sealed `Copy` property. */
export const copyApplication = (provider: Type.Type): InterfaceApplicationFact =>
  Object.freeze({
    _tag: 'InterfaceApplication',
    declaration: Object.freeze({
      _tag: 'CanonicalDeclarationId',
      module: Type.copyCapability.module,
      name: Type.copyCapability.name,
    }),
    capability: Type.copyCapability,
    provider,
    providerMatches: Type.isTypeArgument(provider),
    visibility: 'Public',
    operations: Object.freeze([]),
    available: Type.isTypeArgument(provider),
  })

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
  readonly self: Type.Parameter
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
    readonly form: 'Mapped' | 'Inline'
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
export type MemberFact =
  | DeclarationFact
  | StructFact
  | EnumFact
  | UnionFact
  | ServiceFact
  | InterfaceFact
  | ConstantFact
  | ForeignStaticFact
  | RoleFact
  | AliasFact

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

export type EnumLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: EnumFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<EnumFact>
    }

export type EnumMemberLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly member: EnumMemberFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly members: ReadonlyArray<EnumMemberFact>
    }

export type StructLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: StructFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<StructFact>
    }

export type UnionLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly declaration: UnionFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly declarations: ReadonlyArray<UnionFact>
    }

export type UnionVariantLookup =
  | { readonly _tag: 'Resolved'; readonly spelling: string; readonly variant: UnionVariantFact }
  | { readonly _tag: 'Missing'; readonly spelling: string }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly variants: ReadonlyArray<UnionVariantFact>
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
  readonly enums: ReadonlyArray<EnumFact>
  readonly unions: ReadonlyArray<UnionFact>
  readonly services: ReadonlyArray<ServiceFact>
  readonly interfaces: ReadonlyArray<InterfaceFact>
  readonly constants: ReadonlyArray<ConstantFact>
  readonly conformances: ReadonlyArray<ConformanceFact>
  readonly inherentImpls: ReadonlyArray<InherentImplFact>
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

export const presentParameterEntries = (parameters: ReadonlyArray<ParameterFact>) =>
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

export const lookupParameter = (
  parameters: ReadonlyArray<ParameterFact>,
  name: string,
): ParameterLookup => {
  const matches = presentParameterEntries(parameters)
    .filter((entry) => entry.spelling === name)
    .map((entry) => entry.parameter)
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, parameter: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, parameters: Object.freeze(matches) })
}

const memberIndexCache = new WeakMap<ReadonlyArray<MemberFact>, Map<string, Array<MemberFact>>>()

/**
 * A member is keyed by its canonical name, which for an inherent member is `Owner.member` while
 * its presented spelling stays the bare `member`. A duplicate or unidentified inherent member is
 * keyed under the same dotted spelling so it never shadows a root declaration of that name.
 */
const memberKey = (declaration: MemberFact): string | undefined => {
  if (declaration.canonical._tag === 'Canonical') return declaration.canonical.id.name
  if (declaration._tag === 'FunctionDeclaration' && declaration.associatedMember !== undefined)
    return `${declaration.associatedMember.ownerSpelling}.${declaration.associatedMember.name}`
  return declaration.name._tag === 'Present' ? declaration.name.spelling : undefined
}

const memberIndex = (members: ReadonlyArray<MemberFact>): Map<string, Array<MemberFact>> => {
  let index = memberIndexCache.get(members)
  if (index === undefined) {
    index = new Map()
    for (const declaration of members) {
      const key = memberKey(declaration)
      if (key === undefined) continue
      const bucket = index.get(key)
      if (bucket === undefined) index.set(key, [declaration])
      else bucket.push(declaration)
    }
    memberIndexCache.set(members, index)
  }
  return index
}

export const lookupMember = (members: ReadonlyArray<MemberFact>, name: string): MemberLookup => {
  const matches = memberIndex(members).get(name) ?? []
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({
        _tag: 'Ambiguous',
        spelling: name,
        declarations: Object.freeze([...matches]),
      })
}

export const lookupDeclaration = (
  declarations: ReadonlyArray<DeclarationFact>,
  name: string,
): DeclarationLookup => {
  // Keyed like members: a canonical declaration by its canonical name, so an inherent member is
  // found as `Owner.member` and never as the bare spelling a root declaration would use.
  const matches = declarations.filter((declaration) => memberKey(declaration) === name)
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, declarations: Object.freeze(matches) })
}

export const enumValueOperation = (
  declaration: Pick<EnumFact, 'canonical' | 'representation'>,
): EnumAssociatedOperationFact | undefined => {
  if (declaration.canonical._tag !== 'Canonical' || declaration.representation._tag !== 'Available')
    return undefined
  const enum_ = declaration.canonical.id
  return Object.freeze({
    _tag: 'EnumAssociatedOperation',
    id: Object.freeze({ _tag: 'EnumAssociatedOperationId', enum: enum_, name: 'value' }),
    name: 'value',
    enum: enum_,
    parameter: Type.nominal(enum_.module, enum_.name),
    result: declaration.representation.scalar,
    intrinsic: Object.freeze({
      _tag: 'IntrinsicOperationId',
      actor: 'Intrinsic',
      name: 'enumValue',
    }),
  })
}

export const lookupEnumMember = (
  members: ReadonlyArray<EnumMemberFact>,
  name: string,
): EnumMemberLookup => {
  const matches = members.filter(
    (member) => member.name._tag === 'Present' && member.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, member: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, members: Object.freeze(matches) })
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

export const lookupUnion = (unions: ReadonlyArray<UnionFact>, name: string): UnionLookup => {
  const matches = unions.filter(
    (union) => union.name._tag === 'Present' && union.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, declarations: Object.freeze(matches) })
}

export const lookupUnionVariant = (
  variants: ReadonlyArray<UnionVariantFact>,
  name: string,
): UnionVariantLookup => {
  const matches = variants.filter(
    (variant) => variant.name._tag === 'Present' && variant.name.spelling === name,
  )
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling: name })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling: name, variant: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling: name, variants: Object.freeze(matches) })
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

/** Looks up one aggregate member without conflating ordinal positions with source field names. */
export const lookupAggregateMember = (
  fields: ReadonlyArray<FieldFact>,
  member: AggregateIdentity.MemberIdentity,
): FieldLookup => {
  const matches = fields.filter((field) => {
    if (field.member._tag !== member._tag) return false
    return field.member._tag === 'LabeledAggregateMember' &&
      member._tag === 'LabeledAggregateMember'
      ? field.member.label === member.label
      : field.member._tag === 'OrdinalAggregateMember' &&
          member._tag === 'OrdinalAggregateMember' &&
          field.member.ordinal === member.ordinal
  })
  const spelling = member._tag === 'LabeledAggregateMember' ? member.label : `${member.ordinal}`
  const first = matches.at(0)
  if (first === undefined) return Object.freeze({ _tag: 'Missing', spelling })
  return matches.length === 1
    ? Object.freeze({ _tag: 'Resolved', spelling, field: first })
    : Object.freeze({ _tag: 'Ambiguous', spelling, fields: Object.freeze(matches) })
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

export const enumByName = (self: Index, module: string, name: string): EnumLookup => {
  const result = member(self, module, name)
  if (result._tag === 'Missing') return result
  if (result._tag === 'Resolved')
    return result.declaration._tag === 'EnumDeclaration'
      ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: result.declaration })
      : Object.freeze({ _tag: 'Missing', spelling: name })
  const declarations = result.declarations.filter(
    (declaration): declaration is EnumFact => declaration._tag === 'EnumDeclaration',
  )
  return declarations.length === 0
    ? Object.freeze({ _tag: 'Missing', spelling: name })
    : Object.freeze({
        _tag: 'Ambiguous',
        spelling: name,
        declarations: Object.freeze(declarations),
      })
}

export const struct = (self: Index, module: string, name: string): StructLookup =>
  lookupStruct(
    self.modules.find((candidate) => candidate.module === module)?.structs ?? Object.freeze([]),
    name,
  )

export const unionByName = (self: Index, module: string, name: string): UnionLookup => {
  const result = member(self, module, name)
  if (result._tag === 'Missing') return result
  if (result._tag === 'Resolved')
    return result.declaration._tag === 'UnionDeclaration'
      ? Object.freeze({ _tag: 'Resolved', spelling: name, declaration: result.declaration })
      : Object.freeze({ _tag: 'Missing', spelling: name })
  const declarations = result.declarations.filter(
    (declaration): declaration is UnionFact => declaration._tag === 'UnionDeclaration',
  )
  return declarations.length === 0
    ? Object.freeze({ _tag: 'Missing', spelling: name })
    : Object.freeze({
        _tag: 'Ambiguous',
        spelling: name,
        declarations: Object.freeze(declarations),
      })
}

/** Looks up one completed declaration by canonical identity. */
export const byCanonical = (self: Index, id: CanonicalId): MemberFact | undefined => {
  const generated = self.generatedAggregates.get(`${id.module}:${id.name}`)
  if (generated !== undefined) return generated
  const result = member(self, id.module, id.name)
  return result._tag === 'Resolved' && result.declaration.canonical._tag === 'Canonical'
    ? result.declaration
    : undefined
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
      Type.failureMembers(type).some((failure) => containsLexicalBorrow(self, failure, seen))
    )
  if (!Type.isNominal(type)) return false
  const key = Type.key(type)
  if (seen.has(key)) return false
  const declaration = byCanonical(self, {
    _tag: 'CanonicalDeclarationId',
    module: type.module,
    name: type.name,
  })
  if (declaration?._tag !== 'StructDeclaration' && declaration?._tag !== 'UnionDeclaration')
    return type.arguments
      .filter(Type.isTypeArgument)
      .some((argument) => containsLexicalBorrow(self, argument, seen))
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(seen).add(key)
  const fields =
    declaration._tag === 'StructDeclaration'
      ? declaration.fields
      : declaration.variants.flatMap((variant) => variant.fields)
  return fields.some(
    (field) =>
      field.declaredType._tag === 'Resolved' &&
      containsLexicalBorrow(self, Type.substitute(field.declaredType.type, substitution), next),
  )
}

/** One stored bare-callable occurrence that denies an aggregate type a target layout. */
export interface StoredExecutable {
  readonly path: ReadonlyArray<string>
  readonly contract: Type.Callable | Type.Effect
}

/**
 * Finds the first aggregate position that retains a bare callable or Effect value. A bare
 * executable contract has no hidden concrete identity, so the aggregate cannot lay it out; a
 * representation parameter (`F: Effect<i32>`) carries that identity instead.
 */
export const storedExecutable = (
  self: Index,
  type: Type.Type,
  kind: 'Callable' | 'Effect',
  seen: ReadonlySet<string> = new Set(),
): StoredExecutable | undefined => {
  if (Type.isCallable(type))
    return kind === 'Callable'
      ? Object.freeze({ path: Object.freeze([]), contract: type })
      : undefined
  if (Type.isEffect(type))
    return kind === 'Effect'
      ? Object.freeze({ path: Object.freeze([]), contract: type })
      : undefined
  if (Type.isFixedArray(type) || Type.isSlice(type))
    return storedExecutable(self, type.element, kind, seen)
  if (Type.isUnion(type)) {
    for (const member of type.members) {
      const found = storedExecutable(self, member, kind, seen)
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
  if (declaration?._tag !== 'StructDeclaration' && declaration?._tag !== 'UnionDeclaration')
    return undefined
  const substitution =
    TypeInference.substitution(
      declaration.typeParameters.map((parameter) => parameter.type),
      type.arguments,
    ) ?? new Map()
  const next = new Set(seen).add(key)
  const fields =
    declaration._tag === 'StructDeclaration'
      ? declaration.fields.map((field) => Object.freeze({ field, path: Object.freeze([]) }))
      : declaration.variants.flatMap((variant) =>
          variant.fields.map((field) =>
            Object.freeze({
              field,
              path:
                variant.name._tag === 'Present'
                  ? Object.freeze([variant.name.spelling])
                  : Object.freeze([]),
            }),
          ),
        )
  for (const { field, path } of fields) {
    if (field.declaredType._tag !== 'Resolved' || field.name._tag !== 'Present') continue
    const found = storedExecutable(
      self,
      Type.substitute(field.declaredType.type, substitution),
      kind,
      next,
    )
    if (found !== undefined)
      return Object.freeze({
        path: Object.freeze([...path, field.name.spelling, ...found.path]),
        contract: found.contract,
      })
  }
  return undefined
}
