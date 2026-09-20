import * as Location from './Location.js'
import type * as NativeAssembly from './NativeAssembly.js'
import type * as AuthoredHir from './AuthoredHir.js'
import * as AuthoredIdentity from './AuthoredIdentity.js'
import * as AuthoredLowering from './AuthoredLowering.js'
import * as SemanticContext from './SemanticContext.js'
import * as BodyQuery from './BodyQuery.js'
import * as BodyBuilder from './BodyBuilder.js'
import * as LifetimeFlow from './LifetimeFlow.js'
import { dual } from 'effect/Function'
import type * as CallableContract from './CallableContract.js'
import * as ConformanceProof from './ConformanceProof.js'
import type * as ConformanceGoal from './ConformanceGoal.js'
import type * as Constraint from './Constraint.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as NominalVariance from './NominalVariance.js'
import * as Tir from './Tir.js'
import type * as Intrinsic from './Intrinsic.js'
import type * as Match from './Match.js'
import type * as NameResolution from './NameResolution.js'
import type * as Operator from './Operator.js'
import * as Scalar from './Scalar.js'
import * as SourceSpan from './SourceSpan.js'
import type * as StaticEvaluation from './StaticEvaluation.js'
import type * as StaticText from './StaticText.js'
import type * as StaticValue from './StaticValue.js'
import * as Lifetime from './Lifetime.js'
import * as SemanticDisplay from './SemanticDisplay.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/** The only semantic type recognized by the first analysis slice. */
export type SemanticType = DeclarationFacts.SemanticType

/** A deterministic declaration identity local to one analyzed source snapshot. */
export type DeclarationId = DeclarationFacts.DeclarationId

/** A deterministic parameter identity nested under its owning function declaration. */
export type ParameterId = DeclarationFacts.ParameterId

/** A declaration name supplied by syntax or explicitly unavailable after recovery. */
export type DeclaredName = DeclarationFacts.DeclaredName

/** The resolved, unresolved, or syntax-unavailable declared return type. */
export type DeclaredTypeFact = DeclarationFacts.DeclaredTypeFact

/** The declared type fact attached to a function return. */
export type ReturnTypeFact = DeclarationFacts.ReturnTypeFact

/** One ordered parameter declaration with exact concrete provenance. */
export type ParameterFact = DeclarationFacts.ParameterFact

/** The closed result of looking up a parameter spelling within one function. */
export type ParameterLookup = DeclarationFacts.ParameterLookup

/** One `let` binding declaration with its inferred type and initializer facts. */
export interface BindingDeclarationFact {
  readonly _tag: 'BindingFact'
  readonly id: Tir.BindingId
  readonly name: DeclaredName
  readonly phase: 'Runtime' | 'Static'
  readonly mutability: 'Immutable' | 'Mutable'
  readonly declaredType?: DeclaredTypeFact
  readonly inferredType: ExpressionTypeFact
  readonly initializer: Tir.Expression
  readonly staticValue?: StaticValue.Value
  /** Exact callable value captured when this binding was initialized, before later source writes. */
  readonly exactCallable?: Extract<
    ExpressionFact,
    { readonly _tag: 'FunctionItem' | 'CallableSection' }
  >
  /** Whether the initialized value has one compile-time concrete callable representation. */
  readonly concreteCallableIdentity?: true
  readonly anchor: AuthoredHir.Anchor
}

/** One independently elaborated lexical scope produced by a selected static iteration. */
export interface StaticIterationScopeFact {
  readonly _tag: 'StaticIterationScope'
  readonly ordinal: number
  readonly binding: BindingDeclarationFact
  readonly statements: ReadonlyArray<StatementFact>
  readonly staticIterations: ReadonlyArray<StaticIterationFact>
}

/** The authored static iteration plus its target-selected semantic expansion. */
export interface StaticIterationFact {
  readonly _tag: 'StaticIteration'
  readonly iterable: Tir.Expression
  readonly state: 'Deferred' | 'Rejected' | 'Expanded'
  readonly scopes: ReadonlyArray<StaticIterationScopeFact>
  readonly anchor: AuthoredHir.Anchor
}

/** A bare identifier resolved against enclosing parameters and preceding bindings. */
export type ParameterReferenceFact =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly parameter: ParameterFact
    }
  | {
      readonly _tag: 'ResolvedBinding'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly binding: BindingDeclarationFact
    }
  | {
      readonly _tag: 'ResolvedPattern'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly binding: PatternBindingFact
    }
  | {
      readonly _tag: 'Missing'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly cause?: Diagnostic.Identity<Location.Location>
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly parameters: ReadonlyArray<ParameterFact>
    }
  | {
      readonly _tag: 'Unavailable'
      readonly anchor: AuthoredHir.Anchor
    }

/** The available, out-of-range, or syntax-unavailable integer-expression fact. */
export type IntegerExpressionFact =
  | {
      readonly _tag: 'Available'
      readonly type: SemanticType
      readonly value: bigint
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'OutOfRange'
      readonly type: SemanticType
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'Unavailable'
      readonly anchor: AuthoredHir.Anchor
    }

/** One fixed-`u64` duration expression after exact nanosecond scaling. */
export interface DurationExpressionFact {
  readonly _tag: 'Duration'
  readonly value?: bigint
  readonly spelling?: string
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

export type FloatingExpressionFact =
  | {
      readonly _tag: 'Available'
      readonly type: Scalar.FloatSpelling
      readonly bits: bigint
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
    }
  | { readonly _tag: 'Unavailable'; readonly anchor: AuthoredHir.Anchor }

export interface StaticTextExpressionFact {
  readonly _tag: 'StaticText'
  readonly data?: StaticText.Data
  readonly type: ExpressionTypeFact
  /**
   * The authored text literal this fact wraps directly, when it wraps one.
   *
   * Caller provenance for a compile error points inside the literal the caller wrote, which is a
   * narrower position than the expression the fact is anchored to once the value flows through a
   * parameter. Only a fact built straight from a literal can name it.
   */
  readonly literal?: AuthoredHir.Anchor
  readonly anchor: AuthoredHir.Anchor
}

/** One character literal carrying the single Unicode scalar value its body denotes. */
export interface CharacterExpressionFact {
  readonly _tag: 'Character'
  readonly value?: number
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One dedicated selected compile-time failure expression. */
export interface CompileErrorExpressionFact {
  readonly _tag: 'CompileError'
  readonly message: Tir.Expression
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** A call callee resolved against top-level declarations or unavailable after syntax recovery. */
export type CallReferenceFact =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly declaration: DeclarationFact
    }
  | {
      readonly _tag: 'ResolvedBuiltin'
      readonly assembly?: NativeAssembly.NativeAssembly
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly actor: string
      readonly operation: Tir.BuiltinOperation
      readonly intrinsic: Intrinsic.OperationId
      readonly parameters: ReadonlyArray<SemanticType>
      readonly result: SemanticType
      readonly unsafe: boolean
    }
  | {
      readonly _tag: 'ResolvedEnumOperation'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly operation: DeclarationFacts.EnumAssociatedOperationFact
    }
  | {
      readonly _tag: 'ResolvedEnumEquality'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly enum: DeclarationFacts.CanonicalId
      readonly operator: 'Equals' | 'NotEquals'
    }
  | {
      readonly _tag: 'ResolvedIntrinsicContract'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly intrinsic: Intrinsic.Operation
      readonly contract: CallableContract.CallableContract
    }
  | {
      readonly _tag: 'ResolvedServiceOperation'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly service: DeclarationFacts.ServiceFact
      readonly operation: DeclarationFacts.ServiceOperationFact
    }
  /**
   * One statically selected interface operation. The normalized capability and provider identify
   * the conformance question; which implementation runs is the witness's answer at specialization,
   * not one this reference records.
   */
  | {
      readonly _tag: 'ResolvedInterfaceOperation'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly capability: Type.Nominal
      readonly provider: Type.Type
      readonly operation: string
      readonly declaration: DeclarationFacts.ServiceOperationFact
      readonly interfaceContract: DeclarationFacts.InterfaceOperationApplicationFact
      readonly parameters: ReadonlyArray<SemanticType>
      readonly result: SemanticType
    }
  | {
      readonly _tag: 'Missing'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly cause?: Diagnostic.Identity<Location.Location>
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly anchor: AuthoredHir.Anchor
      readonly declarations: ReadonlyArray<DeclarationFact>
      readonly cause?: Diagnostic.Identity<Location.Location>
    }
  | {
      readonly _tag: 'Unavailable'
      readonly anchor: AuthoredHir.Anchor
    }

/** Exact qualifier and member spellings retained beside semantic reference resolution. */
export type ReferencePathFact =
  | {
      readonly _tag: 'ReferencePath'
      readonly qualifierSpelling?: string
      readonly qualifierAnchor?: AuthoredHir.Anchor
      readonly memberSpelling: string
      readonly memberAnchor: AuthoredHir.Anchor
    }
  | { readonly _tag: 'UnavailableReferencePath'; readonly anchor: AuthoredHir.Anchor }

/** Exact source tokens and catalog identity for one recognized intrinsic member path. */
export type IntrinsicReferenceFact =
  | {
      readonly _tag: 'ResolvedIntrinsicReference'
      readonly actor: Intrinsic.Actor
      readonly operation: Intrinsic.Operation
      readonly actorAnchor: AuthoredHir.Anchor
      readonly operationAnchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'ResolvedCapabilityOperationReference'
      readonly actor: DeclarationFacts.StructFact | Intrinsic.Actor
      readonly operation: Intrinsic.Operation
      readonly actorAnchor: AuthoredHir.Anchor
      readonly operationAnchor: AuthoredHir.Anchor
    }
  | { readonly _tag: 'UnavailableIntrinsicReference'; readonly anchor: AuthoredHir.Anchor }

/** The available or unavailable type of one returned expression. */
export type ExpressionTypeFact =
  | { readonly _tag: 'Available'; readonly type: SemanticType }
  | { readonly _tag: 'Unavailable' }

/** One bare identifier expression with its local reference and type facts. */
export interface IdentifierExpressionFact {
  readonly _tag: 'Identifier'
  readonly reference: ParameterReferenceFact
  /** Concrete compile-time value retained while residualizing a runtime specialization. */
  readonly staticValue?: StaticValue.Value
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One `move <place>` expression with its consuming subject fact. */
export interface MoveExpressionFact {
  readonly _tag: 'Move'
  readonly subject: Tir.Expression
  readonly exactCallable?: Extract<
    ExpressionFact,
    { readonly _tag: 'FunctionItem' | 'CallableSection' }
  >
  readonly concreteCallableIdentity?: true
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

export type BorrowSelectorFact =
  | {
      readonly _tag: 'Field'
      readonly field: DeclarationFacts.FieldId
      readonly span: SourceSpan.SourceSpan
      readonly at?: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'Index'
      readonly index: Tir.Expression
      readonly array: Type.FixedArray
      readonly bounds: Extract<BoundsFact, { readonly _tag: 'Proven' | 'Runtime' }>
      readonly span: SourceSpan.SourceSpan
      readonly at?: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'SliceIndex'
      readonly index: Tir.Expression
      readonly slice: Type.Slice
      readonly span: SourceSpan.SourceSpan
      readonly at?: AuthoredHir.Anchor
    }

export type BorrowRootFact =
  | {
      readonly _tag: 'BindingRoot'
      readonly binding: BindingDeclarationFact
      readonly path: ReadonlyArray<BorrowSelectorFact>
    }
  | {
      readonly _tag: 'ParameterRoot'
      readonly parameter: ParameterFact
      readonly path: ReadonlyArray<BorrowSelectorFact>
    }
  | {
      readonly _tag: 'PatternRoot'
      readonly binding: PatternBindingFact
      readonly path: ReadonlyArray<BorrowSelectorFact>
    }
  | {
      readonly _tag: 'TemporaryRoot'
      readonly owner: Tir.TemporaryOwnerId
      readonly value: Tir.Expression
      readonly path: ReadonlyArray<BorrowSelectorFact>
    }

export type BorrowFormationFact =
  | {
      readonly _tag: 'FixedArrayBorrow'
      readonly root: BorrowRootFact
      readonly array: Type.FixedArray
    }
  | {
      readonly _tag: 'SliceReborrow'
      readonly root: BorrowRootFact
      readonly parent: Type.Slice
      readonly suspendsParent: boolean
    }
  | {
      readonly _tag: 'ValueBorrow'
      readonly root: BorrowRootFact
      readonly source: Type.Type
    }
  | {
      readonly _tag: 'ValueReborrow'
      readonly root: BorrowRootFact
      readonly parent: Type.Reference
      readonly suspendsParent: boolean
    }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity<Location.Location> }

/** One explicit whole-root borrowed view. */
export interface BorrowExpressionFact {
  readonly _tag: 'Borrow'
  readonly access: Type.BorrowAccess
  readonly subject: Tir.Expression
  readonly formation: BorrowFormationFact
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One flattened leaf binding introduced by a nominal pattern. */
export interface PatternBindingFact {
  readonly _tag: 'PatternBinding'
  readonly id: Match.BindingId
  readonly name: DeclaredName
  /** Absent for a whole-member binding, which owns the entire matched payload. */
  readonly field?: DeclarationFacts.FieldFact
  readonly path: ReadonlyArray<DeclarationFacts.FieldId>
  readonly type: ExpressionTypeFact
  readonly access: Match.Access
  /** Owned place aliases inherit assignment permission from the matched root. */
  readonly placeMutability?: 'Mutable' | 'Immutable'
  readonly anchor: AuthoredHir.Anchor
}

export type PatternFieldState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationFacts.FieldFact }
  | { readonly _tag: 'Unknown'; readonly cause: Diagnostic.Identity<Location.Location> }
  | {
      readonly _tag: 'Duplicate'
      readonly field: DeclarationFacts.FieldFact
      readonly cause: Diagnostic.Identity<Location.Location>
    }
  | { readonly _tag: 'Unavailable' }

export interface PatternFieldFact {
  readonly _tag: 'PatternField'
  readonly name: string | undefined
  readonly state: PatternFieldState
  readonly binding?: PatternBindingFact
  readonly nested?: PatternFact
  readonly anchor: AuthoredHir.Anchor
}

export type PatternFact =
  | {
      readonly _tag: 'UnavailablePattern'
      readonly id: Match.PatternId
      readonly member?: undefined
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly complete: false
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'EnumMemberPattern'
      readonly id: Match.PatternId
      readonly enum?: DeclarationFacts.EnumFact
      readonly member?: DeclarationFacts.EnumMemberFact
      readonly coverage?: Match.CoverageIdentity
      readonly qualifierAnchor?: AuthoredHir.Anchor
      readonly memberAnchor?: AuthoredHir.Anchor
      readonly span: SourceSpan.SourceSpan
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly complete: boolean
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'IntegerPattern'
      readonly id: Match.PatternId
      readonly value?: bigint
      readonly span: SourceSpan.SourceSpan
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly complete: false
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'TypePattern'
      readonly id: Match.PatternId
      readonly member?: Type.Type
      readonly declared: DeclarationFacts.DeclaredTypeFact
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly complete: boolean
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'NominalPattern'
      readonly id: Match.PatternId
      readonly target: StructTargetFact
      readonly member?: Type.Nominal
      readonly fields: ReadonlyArray<PatternFieldFact>
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly rest: boolean
      readonly complete: boolean
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'UnionVariantPattern'
      readonly id: Match.PatternId
      readonly target: UnionVariantTargetFact
      readonly member?: Type.Nominal
      readonly coverage?: Match.CoverageIdentity
      readonly fields: ReadonlyArray<PatternFieldFact>
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly rest: boolean
      readonly complete: boolean
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'UniversalPattern'
      readonly id: Match.PatternId
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly anchor: AuthoredHir.Anchor
    }

/** The authored match body, with ordinary blocks retaining their statement region. */
export type MatchArmBodyFact =
  | {
      readonly _tag: 'Expression'
      readonly expression: Tir.Expression
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'Block'
      readonly statements: ReadonlyArray<StatementFact>
      readonly completion: { readonly fallsThrough: boolean }
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }

export interface MatchArmFact {
  readonly tests: ReadonlyArray<Match.PatternTest>
  readonly _tag: 'MatchArm'
  readonly id: Match.ArmId
  readonly pattern: PatternFact
  readonly bindings: ReadonlyArray<PatternBindingFact>
  readonly guard?: Tir.Expression
  readonly body: MatchArmBodyFact
  readonly before: ReadonlyArray<Match.CoverageIdentity>
  readonly after: ReadonlyArray<Match.CoverageIdentity>
  readonly reachable: boolean
  readonly anchor: AuthoredHir.Anchor
}

export interface MatchExpressionFact {
  readonly _tag: 'Match'
  readonly id: Match.MatchId
  readonly access: Match.Access
  readonly scrutinee: Tir.Expression
  readonly members: ReadonlyArray<Match.CoverageIdentity>
  readonly arms: ReadonlyArray<MatchArmFact>
  readonly exhaustive: boolean
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One statement-form pattern decision shared by irrefutable let and refutable if-let. */
export interface PatternSelectionFact {
  readonly tests: ReadonlyArray<Match.PatternTest>
  readonly _tag: 'PatternSelection'
  readonly id: Match.MatchId
  readonly arm: Match.ArmId
  readonly access: Match.Access
  /** Authored initializer, retaining an outer move/borrow for ownership loan analysis. */
  readonly source: Tir.Expression
  readonly subject: Tir.Expression
  readonly members: ReadonlyArray<Match.CoverageIdentity>
  readonly pattern: PatternFact
  readonly bindings: ReadonlyArray<PatternBindingFact>
  readonly irrefutable: boolean
  readonly loanEnd: SourceSpan.SourceSpan
  /** The authored node `loanEnd` presents. */
  readonly loanEndAt: AuthoredHir.Anchor
  readonly anchor: AuthoredHir.Anchor
}

export type StructTargetFact =
  | {
      readonly _tag: 'Resolved'
      readonly struct: DeclarationFacts.StructFact
      readonly type: Type.Nominal
      /** Present for source-named constructors; occurrence-generated literals have no type token. */
      readonly anchor?: AuthoredHir.Anchor
    }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity<Location.Location> }

export type UnionVariantTargetFact =
  | {
      readonly _tag: 'Resolved'
      readonly union: DeclarationFacts.UnionFact
      readonly variant: DeclarationFacts.UnionVariantFact
      readonly type: Type.Nominal
      readonly anchor: AuthoredHir.Anchor
    }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity<Location.Location> }

export type StructInitializerState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationFacts.FieldFact }
  | { readonly _tag: 'Unknown'; readonly cause: Diagnostic.Identity<Location.Location> }
  | {
      readonly _tag: 'Duplicate'
      readonly field: DeclarationFacts.FieldFact
      readonly cause: Diagnostic.Identity<Location.Location>
    }
  | {
      readonly _tag: 'TypeMismatch'
      readonly field: DeclarationFacts.FieldFact
      readonly cause: Diagnostic.Identity<Location.Location>
    }
  | {
      readonly _tag: 'Inaccessible'
      readonly field: DeclarationFacts.FieldFact
      readonly cause: Diagnostic.Identity<Location.Location>
    }
  | { readonly _tag: 'Unavailable' }

export interface StructInitializerFact {
  readonly _tag: 'StructInitializer'
  readonly name: string | undefined
  readonly expression: Tir.Expression
  readonly state: StructInitializerState
  /** The written field label, which is where an editor finds this initializer's field. */
  readonly nameAnchor?: AuthoredHir.Anchor
  readonly anchor: AuthoredHir.Anchor
}

export interface StructTypeArgumentFact {
  readonly parameter: Type.Parameter
  readonly argument?: Type.GenericArgument
  readonly source: 'Explicit' | 'Inferred' | 'Unavailable'
  readonly origins: ReadonlyArray<Location.Location>
}

export interface StructLiteralExpressionFact {
  readonly _tag: 'StructLiteral'
  readonly target: StructTargetFact
  readonly authorized: boolean
  readonly typeArguments: ReadonlyArray<StructTypeArgumentFact>
  readonly initializers: ReadonlyArray<StructInitializerFact>
  readonly fields: ReadonlyArray<{
    readonly field: DeclarationFacts.FieldFact
    readonly initializer: StructInitializerFact
  }>
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

export interface UnionVariantExpressionFact {
  readonly _tag: 'UnionVariant'
  readonly target: UnionVariantTargetFact
  readonly authorized: boolean
  readonly typeArguments: ReadonlyArray<StructTypeArgumentFact>
  readonly initializers: ReadonlyArray<StructInitializerFact>
  readonly fields: ReadonlyArray<{
    readonly field: DeclarationFacts.FieldFact
    readonly initializer: StructInitializerFact
  }>
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

export type ProjectionState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationFacts.FieldFact }
  | { readonly _tag: 'SliceLength' }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity<Location.Location> }

export interface FieldProjectionExpressionFact {
  readonly _tag: 'FieldProjection'
  readonly subject: Tir.Expression
  /** Construction-only summary retained instead of the subject's recursive decision record. */
  readonly root?: AssignmentRootFact
  readonly borrowRoot?: BorrowRootFact
  readonly placeBorrowAccess?: Type.BorrowAccess
  /** Exact compile-time projection retained for residual literal materialization. */
  readonly staticValue?: StaticValue.Value
  readonly nominal?: Type.Nominal
  readonly borrowAccess?: Type.BorrowAccess
  readonly fieldName: string | undefined
  readonly fieldAnchor?: AuthoredHir.Anchor
  readonly state: ProjectionState
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

export type ReferentProjectionState =
  | { readonly _tag: 'Resolved'; readonly reference: Type.Reference }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity<Location.Location> }

/** One explicit postfix projection from a reference value to its borrowed target place. */
export interface ReferentProjectionExpressionFact {
  readonly _tag: 'ReferentProjection'
  readonly subject: Tir.Expression
  readonly root?: AssignmentRootFact
  readonly borrowRoot?: BorrowRootFact
  readonly placeBorrowAccess?: Type.BorrowAccess
  readonly reference?: Type.Reference
  readonly borrowAccess?: Type.BorrowAccess
  readonly state: ReferentProjectionState
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One source-ordered array element with contextual compatibility retained independently. */
export interface ArrayElementFact {
  readonly _tag: 'ArrayElement'
  readonly ordinal: number
  readonly expression: Tir.Expression
  readonly expected?: SemanticType
  readonly compatibility:
    | { readonly _tag: 'Compatible' }
    | {
        readonly _tag: 'TypeMismatch'
        readonly expected: SemanticType
        readonly actual: SemanticType
      }
    | { readonly _tag: 'Unavailable' }
  readonly anchor: AuthoredHir.Anchor
}

export type ArrayLiteralState =
  | { readonly _tag: 'Complete'; readonly type: Type.FixedArray }
  | { readonly _tag: 'MissingContext' }
  | { readonly _tag: 'LengthMismatch'; readonly expected: number; readonly actual: number }
  | { readonly _tag: 'IncompatibleElements' }
  | { readonly _tag: 'Unavailable' }

/** One complete-or-unavailable fixed-array literal and every retained element fact. */
export interface ArrayLiteralExpressionFact {
  readonly _tag: 'ArrayLiteral'
  readonly elements: ReadonlyArray<ArrayElementFact>
  readonly expected?: Type.FixedArray
  readonly elementType?: SemanticType
  readonly length: number
  readonly state: ArrayLiteralState
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

export type BoundsFact =
  | { readonly _tag: 'Proven'; readonly index: number; readonly length: number }
  | {
      readonly _tag: 'Invalid'
      readonly index: number
      readonly length: number
      readonly cause: Diagnostic.Identity<Location.Location>
    }
  | { readonly _tag: 'Runtime'; readonly length: number }
  | { readonly _tag: 'RuntimeSlice' }
  | { readonly _tag: 'Unavailable' }

/** One typed checked array-place projection. */
export interface IndexProjectionExpressionFact {
  readonly _tag: 'IndexProjection'
  readonly subject: Tir.Expression
  readonly index: Tir.Expression
  readonly root?: AssignmentRootFact
  readonly borrowRoot?: BorrowRootFact
  readonly placeBorrowAccess?: Type.BorrowAccess
  readonly array?: Type.FixedArray
  readonly slice?: Type.Slice
  readonly elementType?: SemanticType
  readonly borrowAccess?: Type.BorrowAccess
  readonly access: 'CopyRead' | 'ConsumeRequested'
  readonly bounds: BoundsFact
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One `true`/`false` literal expression fact. */
export interface BooleanExpressionFact {
  readonly _tag: 'Boolean'
  readonly value: boolean
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One reference to a typed compile-time scalar declaration. */
export interface ConstantExpressionFact {
  readonly _tag: 'Constant'
  readonly declaration: DeclarationFacts.ConstantFact
  readonly anchor: AuthoredHir.Anchor
  readonly value?:
    | { readonly _tag: 'Boolean'; readonly value: boolean }
    | {
        readonly _tag: 'Integer'
        readonly value: bigint
        readonly type: SemanticType
      }
    | {
        readonly _tag: 'Floating'
        readonly bits: bigint
        readonly spelling: string
        readonly type: 'f32' | 'f64'
      }
    | { readonly _tag: 'String'; readonly data: StaticText.Data }
    | { readonly _tag: 'Character'; readonly value: number }
  readonly type: ExpressionTypeFact
}

/** One runtime load from a declaration-owned C data symbol. */
export interface ForeignStaticExpressionFact {
  readonly _tag: 'ForeignStatic'
  readonly declaration: DeclarationFacts.ForeignStaticFact
  readonly anchor: AuthoredHir.Anchor
  readonly type: ExpressionTypeFact
}

/** One compiler-known operand-to-parameter relationship. */
export interface BuiltinArgumentMappingFact {
  readonly _tag: 'BuiltinArgumentMapping'
  readonly argument: ArgumentFact
  readonly ordinal: number
  readonly expected: SemanticType
}

/**
 * `&&` or `||`. Both operands are `bool` and the result is `bool`, but the operator reaches no
 * actor operation: an actor call evaluates both operands, and the right operand here evaluates
 * only when the left one does not already decide the result.
 */
export interface ShortCircuitExpressionFact {
  readonly _tag: 'ShortCircuit'
  readonly operator: Operator.ShortCircuit
  /** The left operand first, then the conditionally evaluated right operand. */
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One prefix or infix operator and its canonical builtin resolution. */
export interface OperatorExpressionFact {
  readonly _tag: 'Operator'
  readonly selectedConformances?: ReadonlyArray<ConformanceGoal.Proof>
  readonly operator: Operator.Prefix | Operator.Infix
  readonly reference: CallReferenceFact
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly mappings: ReadonlyArray<BuiltinArgumentMappingFact>
  readonly contract: CallContractFact
  readonly interfaceOperation?: InterfaceOperationFact
  readonly witnessEffectSite?: Tir.EffectSiteId
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/**
 * The bound contract one operator resolves through inside a generic body.
 *
 * The operator still elaborates against the compiler-known operation of a stand-in actor, because
 * the operand type is not known until specialization. This records which interface operation the
 * operator spells so specialization can redirect the call to a source witness; a scalar argument
 * keeps the compiler-known operation and never consults it.
 */
export interface InterfaceOperationFact {
  readonly capability: Type.Nominal
  readonly provider: Type.Type
  readonly operation: string
  readonly contract: DeclarationFacts.InterfaceOperationApplicationFact
}

/** One declaration or builtin named as a callable value without invocation. */
export interface FunctionItemExpressionFact {
  readonly _tag: 'FunctionItem'
  readonly selectedConformances?: ReadonlyArray<ConformanceGoal.Proof>
  readonly reference: CallReferenceFact
  readonly path: ReferencePathFact
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  /** Present when contextual typing converts an exported function item to its C address. */
  readonly foreignAddress?: { readonly symbol: string }
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One trailing value retained by an automatic trailing-argument section. */
export interface CallableCaptureFact {
  readonly _tag: 'CallableCapture'
  readonly ordinal: number
  readonly parameterOrdinal: number
  readonly expression: Tir.Expression
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
}

/** One implicit lexical capture retained by an anonymous callable construction. */
export interface AnonymousCaptureFact {
  readonly _tag: 'AnonymousCapture'
  readonly reference: BindingDeclarationFact | ParameterFact | PatternBindingFact
  readonly access: CallableCaptureFact['access']
  readonly span: SourceSpan.SourceSpan
  readonly expression: Tir.Expression
}

/** One hidden concrete section construction awaiting an ordered leading parameter prefix. */
export interface CallableSectionExpressionFact {
  readonly _tag: 'CallableSection'
  readonly selectedConformances?: ReadonlyArray<ConformanceGoal.Proof>
  readonly site: Tir.CallableSiteId
  readonly reference: CallReferenceFact
  readonly path: ReferencePathFact
  readonly remainingParameters: ReadonlyArray<number>
  readonly captures: ReadonlyArray<CallableCaptureFact>
  readonly retainedDependencies: ReadonlyArray<number>
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly environmentOwner?: Type.CallableEnvironmentIdentity['owner']
  readonly substitution: Type.Substitution
  readonly mode: Type.CallableMode
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
  /** Source-only provenance for a section whose target is an anonymous callable body. */
  readonly anonymous?: {
    readonly functionKind: 'Ordinary' | 'Effect'
    readonly captures: ReadonlyArray<AnonymousCaptureFact>
  }
}

/** One complete unsafe invocation through a native C function address. */
export interface ForeignApplyExpressionFact {
  readonly _tag: 'ForeignApply'
  readonly evaluation: 'CalleeThenArguments' | 'LeftThenCallable'
  readonly callee: Tir.Expression
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly contract: Type.ForeignFunction
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One ordinary invocation through a first-class callable expression. */
export interface CallableApplyExpressionFact {
  readonly _tag: 'CallableApply'
  /** Exact source identity, when semantic application can discharge its generic obligations. */
  readonly sourceTarget?: Extract<CallReferenceFact, { readonly _tag: 'Resolved' }>
  readonly selectedConformances?: ReadonlyArray<ConformanceGoal.Proof>
  readonly callee: Tir.Expression
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly mode: Type.CallableMode
  readonly contract?: Type.Callable
  /** Generic evidence learned from the newly supplied callable arguments. */
  readonly substitution: Type.Substitution
  readonly inferredProviderSelectors: ReadonlyArray<InferredProviderSelector>
  /**
   * Present when the supplied arguments are a proper trailing suffix of a callable value's
   * remaining parameters: the application stages a new section over the value instead of
   * invoking it, splicing these captures after the value's own environment.
   */
  readonly staged?: {
    readonly site: Tir.CallableSiteId
    readonly captures: ReadonlyArray<CallableCaptureFact>
  }
  readonly provenance:
    | { readonly _tag: 'DirectCallableApplication' }
    | {
        readonly _tag: 'PipelineCallableApplication'
        readonly left: Tir.Expression
        readonly callable: Tir.Expression
        readonly evaluation: 'LeftThenCallable'
      }
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One outer lexical value captured by a lazy effect block. */
export interface EffectCaptureFact {
  readonly _tag: 'EffectCapture'
  readonly reference: BindingDeclarationFact | ParameterFact | PatternBindingFact
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
  readonly span: SourceSpan.SourceSpan
  /** The authored use that `span` presents. */
  readonly anchor: AuthoredHir.Anchor
  /** First lexical identifier occurrence retained for anonymous environment construction. */
  readonly expression?: Tir.Expression
}

/** One existing provider retained by an Effect provision wrapper. */
export interface EffectRequirementBindingFact {
  readonly _tag: 'EffectRequirementBinding'
  readonly reference: BindingDeclarationFact | ParameterFact
  readonly selected: Type.RequirementsRow
  readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
  readonly capability?: Type.Nominal | Type.Parameter
  readonly providerType: Type.Nominal | Type.Parameter
  readonly witness?: DeclarationFacts.ConformanceWitness
  readonly role?: string
  /** Fixed provider mode used by requirement selection and runtime service dispatch. */
  readonly selectionAccess: 'Shared' | 'Exclusive' | 'Take'
  /** Ordinary capture access derived from the provider argument expression. */
  readonly captureAccess: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
  readonly span: SourceSpan.SourceSpan
  /** The authored node `span` presents. */
  readonly at: AuthoredHir.Anchor
}

/** One lazy imperative effect block and its capture-derived execution contract. */
export interface EffectExpressionFact {
  readonly _tag: 'EffectBlock'
  readonly site: Tir.EffectSiteId
  readonly representationOwner?: Type.ExecutableSpecializationOwner
  readonly statements: ReadonlyArray<StatementFact>
  readonly captures: ReadonlyArray<EffectCaptureFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly regions: ReadonlyArray<Tir.RegionId>
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One qualified payload-free enum member value with canonical declaration identity. */
export interface EnumMemberExpressionFact {
  readonly _tag: 'EnumMember'
  readonly enum: DeclarationFacts.EnumFact
  readonly member?: DeclarationFacts.EnumMemberFact
  readonly cause?: Diagnostic.Identity<Location.Location>
  readonly qualifierAnchor: AuthoredHir.Anchor
  readonly memberAnchor: AuthoredHir.Anchor
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One declaration-owned projection of an enum member's exact representation value. */
export interface EnumValueExpressionFact {
  readonly _tag: 'EnumValue'
  readonly operation: DeclarationFacts.EnumAssociatedOperationFact
  readonly argument: Tir.Expression
  readonly qualifierAnchor: AuthoredHir.Anchor
  readonly operationAnchor: AuthoredHir.Anchor
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One semantic expression fact at any returned or argument position. */
export type ExpressionFact =
  | {
      readonly _tag: 'Integer'
      readonly integer: IntegerExpressionFact
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }
  | DurationExpressionFact
  | {
      readonly _tag: 'Floating'
      readonly floating: FloatingExpressionFact
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }
  | StaticTextExpressionFact
  | CharacterExpressionFact
  | CompileErrorExpressionFact
  | {
      readonly _tag: 'Unit'
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }
  | BooleanExpressionFact
  | ConstantExpressionFact
  | ForeignStaticExpressionFact
  | EnumMemberExpressionFact
  | EnumValueExpressionFact
  | IdentifierExpressionFact
  | MoveExpressionFact
  | BorrowExpressionFact
  | MatchExpressionFact
  | StructLiteralExpressionFact
  | UnionVariantExpressionFact
  | ArrayLiteralExpressionFact
  | FieldProjectionExpressionFact
  | ReferentProjectionExpressionFact
  | IndexProjectionExpressionFact
  | OperatorExpressionFact
  | ShortCircuitExpressionFact
  | FunctionItemExpressionFact
  | CallableSectionExpressionFact
  | ForeignApplyExpressionFact
  | CallableApplyExpressionFact
  | EffectExpressionFact
  | {
      readonly _tag: 'Run'
      readonly subject: Tir.Expression
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      /** `Place.replace(place, value)`: swap one writable place, yielding its old value. */
      readonly _tag: 'PlaceReplace'
      readonly reference: IntrinsicReferenceFact
      readonly destination: Tir.Expression
      readonly root?: AssignmentRootFact
      readonly value: Tir.Expression
      readonly compatible: boolean
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'EffectBindRequirement'
      readonly reference: IntrinsicReferenceFact
      readonly protected: Tir.Expression
      readonly provider?: EffectRequirementBindingFact
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      /**
       * Member-selective recovery. `protectedRow`, `selected`, `handlerRow`, and `residualRow`
       * are the four rows `bootstrap-semantic-facts` requires this operation to record; the
       * residual is carried explicitly because it has no source-level type to recover it from.
       */
      readonly _tag: 'EffectCatch'
      readonly reference: IntrinsicReferenceFact
      readonly protected: Tir.Expression
      readonly handler: Tir.Expression
      readonly selected?: Type.Type
      readonly protectedRow: Type.FailureRow
      readonly handlerRow: Type.FailureRow
      readonly residualRow: Type.FailureRow
      readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'Call'
      /** Selected source evidence retained for generic semantic obligation replay. */
      readonly selectedConformances?: ReadonlyArray<ConformanceGoal.Proof>
      readonly reference: CallReferenceFact
      readonly path: ReferencePathFact
      readonly interfaceApplication?: DeclarationFacts.DeclaredTypeFact
      readonly typeArguments: ReadonlyArray<TypeArgumentFact>
      readonly arguments: ReadonlyArray<ArgumentFact>
      readonly staticArguments?: ReadonlyArray<{
        readonly parameter: ParameterFact
        readonly value: StaticValue.Value
        /** Caller-authored provenance retained outside canonical specialization identity. */
        readonly textOrigin?: StaticEvaluation.TextOrigin
      }>
      /** Complete compile-time result when this call targets a `static fn`. */
      readonly staticValue?: StaticValue.Value
      /** Source text provenance retained separately from the canonical static result. */
      readonly staticTextSpan?: Location.Location
      /** Source-independent origin retained for cached static text results. */
      readonly staticTextOrigin?: StaticEvaluation.TextOrigin
      /** Original compile-time failure when eager static-call evaluation did not complete. */
      readonly staticFailure?: StaticEvaluation.StaticFailure
      readonly mappings: ReadonlyArray<ArgumentMappingFact>
      readonly contract: CallContractFact
      readonly witnessEffectSite?: Tir.EffectSiteId
      readonly type: ExpressionTypeFact
      readonly anchor: AuthoredHir.Anchor
    }

/** Whether a selected result retains any validity requirement carried by an input value. */
export const retainsLifetimes = (
  source: SemanticType,
  result: SemanticType,
  assumptions: Lifetime.Assumptions,
): boolean => {
  const required = Type.storageLifetimes(result).filter(
    (lifetime) => lifetime._tag !== 'StaticLifetime',
  )
  return (
    required.length > 0 &&
    Type.storageLifetimes(source).some((lifetime) =>
      required.some((output) => Lifetime.outlives(assumptions, lifetime, output)),
    )
  )
}

/** Arguments whose access capability, or owned payload, is retained by the selected result. */
export const retainedResultArguments = (
  self: ExpressionFact,
  assumptions: Lifetime.Assumptions,
): ReadonlyArray<ArgumentFact> => {
  if (
    (self._tag !== 'Call' && self._tag !== 'CallableApply' && self._tag !== 'Operator') ||
    self.type._tag !== 'Available'
  )
    return []
  const result = self.type.type
  return self.arguments.filter((argument) => {
    if (argument.type._tag !== 'Available') return false
    const source = argument.type.type
    // Extracting T from &mut Owner<T> transfers T's external dependencies, not the access
    // capability to Owner's storage. Only the outer borrow can retain that storage loan.
    if (Type.isReference(source) || Type.isSlice(source))
      return Type.storageLifetimes(result).some(
        (output) =>
          output._tag !== 'StaticLifetime' &&
          Lifetime.outlives(assumptions, source.lifetime, output),
      )
    return retainsLifetimes(source, result, assumptions)
  })
}

/** A deterministic argument identity within one caller and concrete call site. */
export interface ArgumentId {
  readonly _tag: 'ArgumentId'
  readonly function: DeclarationId
  readonly callSpan: SourceSpan.SourceSpan
  /** The authored call `callSpan` presents. */
  readonly call?: AuthoredHir.Anchor
  readonly ordinal: number
}

/** One ordered, syntax-owned call argument. */
export interface ArgumentFact {
  readonly _tag: 'Argument'
  readonly id: ArgumentId
  readonly expression: Tir.Expression
  /** Construction-only loan root needed by intrinsics that project through an argument. */
  readonly borrowRoot?: BorrowRootFact
  readonly type: ExpressionTypeFact
  readonly anchor: AuthoredHir.Anchor
}

/** One positional argument-to-parameter relationship. */
export interface ArgumentMappingFact {
  readonly _tag: 'ArgumentMapping'
  readonly argument: ArgumentFact
  readonly parameter: ParameterFact
}

/** One source-owned explicit call type argument, resolved canonically when available. */
export interface TypeArgumentFact {
  readonly _tag: 'TypeArgument'
  readonly ordinal: number
  readonly anchor: AuthoredHir.Anchor
  readonly declared: DeclaredTypeFact
  readonly type?: SemanticType | Lifetime.Lifetime | Type.RequirementRowArgument
  /** Canonical role of an access-independent requirement selector such as `T at Role`. */
  readonly requirementRole?: Type.Requirement['role']
}

/** One omitted provider-selector binder and the concrete service selected for it. */
export interface InferredProviderSelector {
  readonly parameter: Type.Parameter
  readonly selected: Type.Requirement
}

/** Why a call contract cannot be established. */
export type UnavailableCallContractReason =
  | { readonly _tag: 'UnavailableCallSyntax'; readonly anchor: AuthoredHir.Anchor }
  | { readonly _tag: 'UnavailableCallTarget'; readonly reference: CallReferenceFact }
  | { readonly _tag: 'UnavailableMappedType'; readonly mapping: ArgumentMappingFact }
  | { readonly _tag: 'UnavailableBuiltinArgument'; readonly argument: ArgumentFact }
  | {
      readonly _tag: 'ArgumentTypeMismatch'
      readonly argument: ArgumentFact
      readonly expected: SemanticType
    }

/** The complete positional contract outcome for one call. */
export type CallContractFact =
  | {
      readonly _tag: 'Compatible'
      readonly expectedCount: number
      readonly actualCount: number
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly substitution: Type.Substitution
      readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
      readonly inferredProviderSelectors: ReadonlyArray<InferredProviderSelector>
      /** Open source selections justified by exact bounds of the enclosing generic declaration. */
      readonly symbolicConformances?: ReadonlyArray<ConformanceProof.SymbolicConformanceSelection>
    }
  | {
      readonly _tag: 'ArityMismatch'
      readonly expectedCount: number
      readonly actualCount: number
    }
  | {
      readonly _tag: 'Unavailable'
      readonly reason: UnavailableCallContractReason
      readonly cause?: Diagnostic.Identity<Location.Location>
    }

/** Whether one returned expression is known to match its declared result type. */
export type ReturnCompatibility = { readonly _tag: 'Compatible' } | { readonly _tag: 'Unavailable' }

export type AssignmentRootFact = BindingDeclarationFact | ParameterFact | PatternBindingFact

export type AssignmentRootAccess =
  | 'ImmutableOwned'
  | 'MutableOwned'
  | 'SharedBorrowed'
  | 'ExclusiveBorrowed'

const assignmentPlaceBorrowAccess = (place: ExpressionFact): Type.BorrowAccess | undefined => {
  if (
    place._tag !== 'FieldProjection' &&
    place._tag !== 'ReferentProjection' &&
    place._tag !== 'IndexProjection'
  ) {
    return undefined
  }
  return place.placeBorrowAccess
}

/** Classifies writable roots without conflating owned binding mutability with pointee access. */
export const assignmentRootAccess = (
  root: AssignmentRootFact,
  place: ExpressionFact,
): AssignmentRootAccess => {
  if (root._tag === 'PatternBinding')
    return root.access === 'Place' && root.placeMutability === 'Mutable'
      ? 'MutableOwned'
      : 'ImmutableOwned'
  let type: Type.Type | undefined
  if (root._tag === 'ParameterDeclaration') {
    if (root.declaredType._tag === 'Resolved') {
      type = root.declaredType.type
    } else {
      type = undefined
    }
  } else if (root.inferredType._tag === 'Available') {
    type = root.inferredType.type
  } else {
    type = undefined
  }
  const rootBorrowAccess =
    place._tag !== 'Identifier' &&
    type !== undefined &&
    (Type.isSlice(type) || Type.isReference(type))
      ? type.access
      : undefined
  const placeBorrowAccess = assignmentPlaceBorrowAccess(place)
  let borrowAccess: Type.BorrowAccess | undefined
  if (rootBorrowAccess === 'Shared' || placeBorrowAccess === 'Shared') borrowAccess = 'Shared'
  else if (rootBorrowAccess === 'Exclusive' || placeBorrowAccess === 'Exclusive')
    borrowAccess = 'Exclusive'
  if (borrowAccess !== undefined)
    return borrowAccess === 'Exclusive' ? 'ExclusiveBorrowed' : 'SharedBorrowed'
  const mutability = root._tag === 'ParameterDeclaration' ? root.bindingMutability : root.mutability
  return mutability === 'Mutable' ? 'MutableOwned' : 'ImmutableOwned'
}

/** Resolves the mutable root a writable-place expression is anchored to, if any. */
export const assignmentRoot = (fact: ExpressionFact): AssignmentRootFact | undefined => {
  if (fact._tag === 'Identifier') {
    if (fact.reference._tag === 'ResolvedBinding') return fact.reference.binding
    if (fact.reference._tag === 'Resolved') return fact.reference.parameter
    if (fact.reference._tag === 'ResolvedPattern' && fact.reference.binding.access === 'Place')
      return fact.reference.binding
    return undefined
  }
  if (
    fact._tag === 'FieldProjection' ||
    fact._tag === 'ReferentProjection' ||
    fact._tag === 'IndexProjection'
  ) {
    return fact.root
  }
  return undefined
}

/** One public function declaration and its syntax-owned semantic facts. */
export type DeclarationFact = DeclarationFacts.DeclarationFact

/** One analyzed body statement in source order, nesting through conditionals. */
export type StatementFact =
  | {
      readonly _tag: 'UnsafeStatement'
      readonly statements: ReadonlyArray<StatementFact>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'BindStatement'
      readonly binding: BindingDeclarationFact
      readonly region: Tir.RegionId
    }
  | {
      readonly _tag: 'PatternBindStatement'
      readonly selection: PatternSelectionFact
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'ExpressionStatement'
      readonly expression: ExpressionFact
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'IfStatement'
      readonly condition: ExpressionFact
      readonly taken: ReadonlyArray<StatementFact>
      readonly otherwise: ReadonlyArray<StatementFact>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'IfLetStatement'
      readonly selection: PatternSelectionFact
      readonly taken: ReadonlyArray<StatementFact>
      readonly otherwise: ReadonlyArray<StatementFact>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'WriteStatement'
      readonly destination: ExpressionFact
      readonly root?: AssignmentRootFact
      readonly value: ExpressionFact
      readonly compatible: boolean
      /** Checked conversion bounds for this value; installation still controls region validity. */
      readonly lifetimeProof: ReadonlyArray<Lifetime.Outlives>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'WhileStatement'
      readonly loop: Tir.LoopId
      readonly parent?: Tir.LoopId
      readonly condition: ExpressionFact
      readonly body: ReadonlyArray<StatementFact>
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'BreakStatement'
      readonly target?: Tir.LoopId
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'ContinueStatement'
      readonly target?: Tir.LoopId
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'ReturnStatement'
      readonly expression: ExpressionFact
      /** A trailing block expression returns without an authored `return` statement. */
      readonly implicit?: true
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'FailStatement'
      readonly expression: ExpressionFact
      readonly failure?: Type.Type
      readonly transfer: 'Copy' | 'Move'
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }
  | {
      readonly _tag: 'DropStatement'
      readonly expression: ExpressionFact
      readonly region: Tir.RegionId
      readonly anchor: AuthoredHir.Anchor
    }

/** One function's declaration, statements, bindings, and compatibility facts. */
export interface FunctionFact {
  readonly _tag: 'FunctionFact'
  readonly comparisonWork?: Readonly<import('./TypeCompatibility.js').Work>
  readonly lifetimeFlow?: import('./LifetimeFlow.js').LifetimeFlow
  readonly declaration: DeclarationFact
  /** The authored block this body elaborated, absent for a declaration without one. */
  readonly bodyAnchor?: AuthoredHir.Anchor
  readonly statements: ReadonlyArray<StatementFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly regionOrder: ReadonlyArray<Tir.RegionId>
  readonly returnedExpression: ExpressionFact
  readonly returnCompatibility: ReturnCompatibility
  /** The finite composite Effect representation joined across distinct return sites. */
  readonly resultRepresentation?: SemanticType
  readonly generatedAggregates: ReadonlyArray<DeclarationFacts.StructFact>
  /** Static-only authored loops and their independently elaborated target-selected scopes. */
  readonly staticIterations: ReadonlyArray<StaticIterationFact>
  /** The authored names this body resolves, published for navigation and never executed. */
  readonly occurrences: ReadonlyArray<import('./SemanticOccurrence.js').LocatedOccurrence>
  /** What the body infers that its author did not write, published for editor hints. */
  readonly hints: ReadonlyArray<import('./TypeHint.js').Row>
  /** What each returned expression shows about the opaque result this body produces. */
  readonly opaqueEvidence: ReadonlyArray<import('./OpaqueRealization.js').Evidence>
}

/** Stable identity of one parent-linked lexical scope in an elaborated function. */
export interface LexicalScopeId {
  readonly _tag: 'LexicalScopeId'
  readonly function: DeclarationId
  readonly ordinal: number
}

/** Names introduced directly by one lexical scope, before parent traversal and shadowing. */
export interface LexicalScopeFact {
  readonly _tag: 'LexicalScope'
  readonly id: LexicalScopeId
  readonly parent?: LexicalScopeId
  /** The scope reaches from the start of `first` to the end of `last`: authored nodes, no offsets. */
  readonly first: AuthoredHir.Anchor
  readonly last: AuthoredHir.Anchor
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly bindings: ReadonlyArray<ScopeBinding>
  readonly patternBindings: ReadonlyArray<PatternBindingFact>
}

/** A local as its scope publishes it: what it is, never how it was initialized. */
export type ScopeBinding = Omit<BindingDeclarationFact, 'initializer' | 'exactCallable'>

/** The closed result of looking up one declaration spelling. */
export type DeclarationLookup = DeclarationFacts.DeclarationLookup

/**
 * What a checked body publishes beside its nodes.
 *
 * Every row names authored nodes and declaration ids, never a position or a header object, so a
 * reused body keeps its results unchanged and each revision presents them again.
 */
export interface BodyResults {
  /** Authoritative selected-evidence payloads, addressed densely by executable nodes. */
  readonly evidence: ReadonlyArray<ReadonlyArray<Constraint.ConstraintEvidence>>
  /** Authoritative revision-free unavailable causes, addressed densely by executable nodes. */
  readonly causes: ReadonlyArray<Diagnostic.Identity<Location.Location>>
  /** The authored names the body resolves: navigation reads these and never the body. */
  readonly occurrences: ReadonlyArray<import('./SemanticOccurrence.js').LocatedOccurrence>
  /** What the body infers that its author did not write, for editor hints. */
  readonly hints: ReadonlyArray<import('./TypeHint.js').Row>
  /** What each returned expression shows about the opaque result this body produces. */
  readonly opaqueEvidence: ReadonlyArray<import('./OpaqueRealization.js').Evidence>
  /** The finite region proof of the body, which ownership replays at cleanup. */
  readonly lifetimes?: import('./LifetimeFlow.js').LifetimeFlow
  /** The body's lexical scopes and the locals each one introduces, for completion and hover. */
  readonly scopes: ReadonlyArray<LexicalScopeFact>
  /** Aggregates the body generated, which layout and later stages declare beside source ones. */
  readonly aggregates: ReadonlyArray<DeclarationFacts.StructFact>
  /** The body's half of the module's constrained-callable escape rule. */
  readonly callables: CallableFlow
  /** Why an application of this body must be specialized, when its own statements say so. */
  readonly staticStructure?: StaticStructure
  /** The type each authored expression was given, in source order, for hover. */
  readonly expressionTypes: ReadonlyArray<ExpressionTypeRow>
  /** How each authored `static for` ended, outermost first. */
  readonly staticIterations: ReadonlyArray<StaticIterationRow>
}

/** One `static for` of a body: whether it was expanded, and the element each expansion bound. */
export interface StaticIterationRow {
  readonly at: AuthoredHir.Anchor
  readonly state: StaticIterationFact['state']
  readonly elements: ReadonlyArray<StaticValue.Value | undefined>
  readonly nested: ReadonlyArray<ReadonlyArray<StaticIterationRow>>
}

/** One typed expression as hover shows it. */
export interface ExpressionTypeRow {
  readonly at: AuthoredHir.Anchor
  readonly type: Type.Type
  readonly presentation?: SemanticDisplay.Presentation
}

/** Static work a body still holds after checking, which only a concrete application resolves. */
export type StaticStructure = 'StaticBinding' | 'UnresolvedConstant' | 'CompileError' | 'StaticCall'

/** One checked body: the declaration it belongs to, its nodes and its results. */
export interface CheckedBody {
  readonly artifact: Tir.ArtifactId
  readonly declaration: DeclarationFact
  /** A compiler-made body (an anonymous callable) that source lookup never finds. */
  readonly hidden: boolean
  /** A `static fn` keeps its static structure here and is never part of the module's runtime TIR. */
  readonly function: Tir.TirFunction
  readonly results: BodyResults
}

/** The complete deterministic elaboration result for all direct bootstrap declarations. */
export interface Result {
  readonly _tag: 'Elaboration'
  /** The authored module this elaboration consumed; reuse keys derive from it. */
  readonly authored: AuthoredLowering.Lowered
  readonly generatedAggregates: ReadonlyArray<DeclarationFacts.StructFact>
  readonly lexicalScopes: ReadonlyArray<LexicalScopeFact>
  readonly tir: Tir.Module
  /** Every checked body of the module, source declarations first and compiler-made ones after. */
  readonly bodies: ReadonlyArray<CheckedBody>
  readonly diagnostics: ReadonlyArray<Diagnostic.Located>
}

/** How many source bodies a module checked: compiler-made bodies are not counted. */
export const sourceBodyCount = (self: Result): number =>
  self.bodies.reduce((sum, body) => sum + (body.hidden ? 0 : 1), 0)

/** All executable working records, including compiler-private anonymous targets. */
export const executableFunctions = (self: Result): ReadonlyArray<FunctionFact> => {
  const all = records(self)
  return Object.freeze([...all.functions, ...all.hiddenFunctions])
}

export const compatible: ReturnCompatibility = Object.freeze({ _tag: 'Compatible' })
export const unavailableCompatibility: ReturnCompatibility = Object.freeze({ _tag: 'Unavailable' })
export const availableI32ExpressionType: ExpressionTypeFact = Object.freeze({
  _tag: 'Available',
  type: 'i32',
})
export const availableUsizeExpressionType: ExpressionTypeFact = Object.freeze({
  _tag: 'Available',
  type: 'usize',
})
export const availableBoolExpressionType: ExpressionTypeFact = Object.freeze({
  _tag: 'Available',
  type: 'bool',
})
export const availableExpressionType = (type: SemanticType): ExpressionTypeFact => {
  if (type === 'i32') {
    return availableI32ExpressionType
  }
  if (type === 'usize') {
    return availableUsizeExpressionType
  }
  if (type === 'bool') {
    return availableBoolExpressionType
  }
  return Object.freeze({ _tag: 'Available', type })
}
export const unavailableExpressionType: ExpressionTypeFact = Object.freeze({ _tag: 'Unavailable' })

export const typesCompatible = (
  source: SemanticType,
  target: SemanticType,
  context?: TypeCompatibility.Context,
): boolean => TypeCompatibility.isCompatible(TypeCompatibility.check(source, target, context))

export const declaredReturnTypesCompatible = (
  context: SemanticContext.SemanticContext,
  declaration: DeclarationFact,
  expression: ExpressionFact,
  compatibility?: TypeCompatibility.Context,
): boolean => {
  if (declaration.returnType._tag !== 'Resolved' || expression.type._tag !== 'Available')
    return false
  const source = expression.type.type
  const target = declaration.returnType.type
  if (typesCompatible(source, target, compatibility)) return true
  const representation = representationOfExpression(context, expression)
  const contract = Type.isRepresented(source) ? source.contract : source
  if (
    declaration.opaqueResult !== undefined &&
    Type.isRepresented(target) &&
    Type.isOpaqueRepresentationArgument(target.representation.argument) &&
    Type.equalsOpaqueFamily(target.representation.argument.family, declaration.opaqueResult.family)
  )
    return representation !== undefined && typesCompatible(contract, target.contract, compatibility)
  if (
    representation !== undefined &&
    (Type.isCallable(contract) || Type.isEffect(contract)) &&
    typesCompatible(Type.represented(contract, contract, representation), target, compatibility)
  )
    return true
  if (
    declaration.opaqueResult !== undefined &&
    Type.isUnion(target) &&
    target.members.some(
      (member) =>
        Type.isRepresented(member) &&
        Type.isOpaqueRepresentationArgument(member.representation.argument) &&
        Type.equalsOpaqueFamily(
          member.representation.argument.family,
          declaration.opaqueResult?.family ?? member.representation.argument.family,
        ) &&
        Type.haveSameRepresentationShape(source, member.contract),
    )
  )
    return true
  if (declaration.opaqueResult !== undefined && Type.haveSameRepresentationShape(source, target))
    return true
  if (!Type.isRepresented(target)) return false
  if (!(Type.isCallable(contract) || Type.isEffect(contract))) return false
  if (representation === undefined) return false
  const represented = Type.represented(
    contract,
    target.representation.requiredBound,
    representation,
  )
  return (
    represented.representation.admissibility._tag !== 'Unavailable' &&
    Type.equalsGenericArgument(representation, target.representation.argument)
  )
}

export const representationJoinDiagnostic = (
  expected: SemanticType,
  actual: SemanticType,
  expectedOrigin: Location.Location,
  actualOrigin: Location.Location,
  span: Location.Location,
): Diagnostic.Located | undefined => {
  const divergence = Type.firstRepresentationDivergence(expected, actual)
  return divergence === undefined
    ? undefined
    : Diagnostic.divergentRepresentationJoin(
        Type.encodeGenericArgument(divergence.left),
        Type.encodeGenericArgument(divergence.right),
        Object.freeze([expectedOrigin, actualOrigin]),
        span,
      )
}

export const contextualIntegerCompatible = (
  expression: ExpressionFact | Tir.Expression,
  target: SemanticType,
): boolean => {
  const value =
    'origin' in expression
      ? expression._tag === 'IntegerLiteral'
        ? expression.value
        : undefined
      : expression._tag === 'Integer' && expression.integer._tag === 'Available'
        ? expression.integer.value
        : undefined
  if (value === undefined) return false
  if (typeof target !== 'string' || !Scalar.isIntegerSpelling(target)) return false
  const scalar = Scalar.find(target)
  if (scalar?.category !== 'Integer') return false
  const range = Scalar.range(scalar, 64)
  return value >= range.minimum && value <= range.maximum
}

export const unionConversionDiagnostic = (
  source: SemanticType,
  target: SemanticType,
  span: Location.Location,
  context?: TypeCompatibility.Context,
): Diagnostic.Located | undefined => {
  const compatibility = TypeCompatibility.check(source, target, context)
  return compatibility._tag === 'Incompatible' &&
    (Type.isUnion(source) || Type.isNever(source) || Type.isUnion(target) || Type.isNever(target))
    ? Diagnostic.incompatibleUnionConversion(
        Type.encode(source),
        Type.encode(target),
        compatibility.missing.map(Type.encode),
        span,
      )
    : undefined
}

/** The authored callee of a call, seeing through pipelines to the applied target. */
export const callCallee = (node: AuthoredHir.Expression): AuthoredHir.Expression =>
  node._tag === 'CallExpression' ? node.callee : node

/**
 * The last name a type spells, seeing through an application so that an applied qualifier such as
 * `Holder<i32>` names `Holder` exactly as the bare qualifier `Holder` does.
 */
const qualifierName = (type: AuthoredHir.Type): AuthoredHir.Name | undefined => {
  if (type._tag === 'AppliedType') return qualifierName(type.target)
  return type._tag === 'NamedType' ? type.path.segments.at(-1) : undefined
}

/**
 * The qualifier/member name pair a callee reference spells, taken from the authored vocabulary.
 * Grouped expressions do not exist in authored HIR, so no unwrapping is needed.
 */
export const referenceNames = (node: AuthoredHir.Expression): ReadonlyArray<AuthoredHir.Name> => {
  const callee = callCallee(node)
  if (callee._tag === 'PipelineExpression') return referenceNames(callee.target)
  if (callee._tag === 'IdentifierExpression') return Object.freeze([callee.name])
  if (callee._tag === 'MemberExpression') {
    const qualifier = qualifierName(callee.selector.subject)
    return qualifier === undefined
      ? Object.freeze([])
      : Object.freeze([qualifier, callee.selector.member])
  }
  if (callee._tag !== 'FieldExpression') return Object.freeze([])
  const qualifier = referenceNames(callee.subject).at(-1)
  return qualifier === undefined ? Object.freeze([]) : Object.freeze([qualifier, callee.field])
}

export const referencePath = (
  context: SemanticContext.SemanticContext,
  node: AuthoredHir.Expression,
): ReferencePathFact => {
  const names = referenceNames(node)
  const member = names.at(-1)
  const qualifier = names.length > 1 ? names.at(0) : undefined
  if (member === undefined)
    return Object.freeze({ _tag: 'UnavailableReferencePath', anchor: node.anchor })
  return Object.freeze({
    _tag: 'ReferencePath',
    ...(qualifier === undefined
      ? {}
      : {
          qualifierSpelling: SemanticContext.nameText(context, qualifier) ?? '',
          qualifierAnchor: qualifier.anchor,
        }),
    memberSpelling: SemanticContext.nameText(context, member) ?? '',
    memberAnchor: member.anchor,
  })
}

export const lookupParameter = DeclarationFacts.lookupParameter

export const lookupDeclaration = DeclarationFacts.lookupDeclaration

export interface IntegerResult {
  readonly fact: IntegerExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Located>
}

export interface ExpressionResult {
  readonly fact: ExpressionFact
  /** The typed node already published while checking this construct. */
  readonly node?: Tir.Expression
  readonly diagnostics: ReadonlyArray<Diagnostic.Located>
  readonly type: SemanticType | undefined
}

/** Reads the typed node construction publishes before it proceeds to the parent construct. */
export const expressionNode = (self: ExpressionResult): Tir.Expression => {
  if (self.node === undefined) throw new RangeError('Expression analysis did not publish a TIR node')
  return self.node
}

/** Construction-time type view shared by a shallow decision and an already published child. */
export const constructionExpressionType = (
  self: ExpressionFact | Tir.Expression,
): ExpressionTypeFact =>
  'origin' in self
    ? self._tag === 'Unavailable'
      ? unavailableExpressionType
      : availableExpressionType(self.type)
    : self.type

/** Authored position retained by both construction decisions and typed nodes. */
export const constructionExpressionAnchor = (
  self: ExpressionFact | Tir.Expression,
): AuthoredHir.Anchor => ('origin' in self ? self.origin.anchor : self.anchor)

export interface IdentifierResult {
  readonly fact: IdentifierExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Located>
  readonly type: SemanticType | undefined
  readonly anchor: AuthoredHir.Anchor
}

export interface ArgumentsResult {
  readonly facts: ReadonlyArray<ArgumentFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Located>
}

export const argumentFact = (
  declaration: DeclarationFact,
  context: SemanticContext.SemanticContext,
  call: AuthoredHir.Anchor,
  input: ExpressionResult | ArgumentFact,
  ordinal: number,
): ArgumentFact => {
  const expression = 'fact' in input ? expressionNode(input) : input.expression
  const type = 'fact' in input ? input.fact.type : input.type
  const anchor = 'fact' in input ? input.fact.anchor : input.anchor
  const borrowRoot =
    !('fact' in input)
      ? input.borrowRoot
      : input.fact._tag === 'Borrow' && input.fact.formation._tag !== 'Unavailable'
        ? input.fact.formation.root
        : input.fact._tag === 'FieldProjection' ||
            input.fact._tag === 'ReferentProjection' ||
            input.fact._tag === 'IndexProjection'
          ? input.fact.borrowRoot
          : input.fact._tag === 'Identifier' && input.fact.reference._tag === 'ResolvedBinding'
            ? Object.freeze({
                _tag: 'BindingRoot' as const,
                binding: input.fact.reference.binding,
                path: Object.freeze([]),
              })
            : input.fact._tag === 'Identifier' && input.fact.reference._tag === 'Resolved'
              ? Object.freeze({
                  _tag: 'ParameterRoot' as const,
                  parameter: input.fact.reference.parameter,
                  path: Object.freeze([]),
                })
              : input.fact._tag === 'Identifier' &&
                  input.fact.reference._tag === 'ResolvedPattern'
                ? Object.freeze({
                    _tag: 'PatternRoot' as const,
                    binding: input.fact.reference.binding,
                    path: Object.freeze([]),
                  })
          : undefined
  return Object.freeze({
    _tag: 'Argument',
    id: Object.freeze({
      _tag: 'ArgumentId',
      function: declaration.id,
      callSpan: context.spanOf(call),
      call,
      ordinal,
    }),
    expression,
    ...(borrowRoot === undefined ? {} : { borrowRoot }),
    type,
    anchor,
  })
}

import { copyAssumptionsOf } from './CallResolution.js'
import {
  analyzeConstant,
  effectCaptureFacts,
  representationOfExpression,
} from './ExpressionAnalysis.js'
import {
  directExpressionChildren,
  directStatementExpressions,
  lowerStatements,
  staticLowering,
} from './TirLowering.js'
import { analyzeFunctionBody } from './StatementAnalysis.js'
export interface FactVisitor {
  readonly statement?: (statement: StatementFact) => void
  readonly expression?: (expression: ExpressionFact) => void
  readonly node?: (expression: Tir.Expression) => void
  readonly descendExpressions?: boolean
  readonly descendEffectBlocks?: boolean
}

/** Direct semantic children in source evaluation order. */
export const expressionChildren = (
  self: ExpressionFact | Tir.Expression,
): ReadonlyArray<ExpressionFact | Tir.Expression> =>
  'origin' in self ? Tir.expressionChildren(self) : directExpressionChildren(self)

const visitExpressionFact = (
  expression: ExpressionFact | Tir.Expression,
  visitor: FactVisitor,
): void => {
  if ('origin' in expression) {
    visitor.node?.(expression)
    for (const child of Tir.expressionChildren(expression)) visitExpressionFact(child, visitor)
    return
  }
  visitor.expression?.(expression)
  if (expression._tag === 'Match') {
    visitExpressionFact(expression.scrutinee, visitor)
    for (const arm of expression.arms) {
      if (arm.guard !== undefined) visitExpressionFact(arm.guard, visitor)
      if (arm.body._tag === 'Expression') visitExpressionFact(arm.body.expression, visitor)
      else visitStatementFacts(arm.body.statements, visitor)
    }
    return
  }
  if (expression._tag === 'EffectBlock') {
    if (visitor.descendEffectBlocks !== false) visitStatementFacts(expression.statements, visitor)
    return
  }
  for (const child of directExpressionChildren(expression)) visitExpressionFact(child, visitor)
}

/** Visits one expression tree in deterministic source order. */
export const visitExpressionFacts = (
  self: ExpressionFact | Tir.Expression,
  visitor: FactVisitor,
): void =>
  visitExpressionFact(self, visitor)

/** Visits statement trees and, by default, every nested expression in source order. */
export const visitStatementFacts = (
  self: ReadonlyArray<StatementFact>,
  visitor: FactVisitor,
): void => {
  const descendExpressions = visitor.descendExpressions !== false
  for (const statement of self) {
    visitor.statement?.(statement)
    if (descendExpressions)
      for (const expression of directStatementExpressions(statement))
        visitExpressionFact(expression, visitor)
    if (statement._tag === 'UnsafeStatement') visitStatementFacts(statement.statements, visitor)
    else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
      visitStatementFacts(statement.taken, visitor)
      visitStatementFacts(statement.otherwise, visitor)
    } else if (statement._tag === 'WhileStatement') visitStatementFacts(statement.body, visitor)
  }
}

const constrainedCallableSchema = (
  expression: ExpressionFact | Tir.Expression,
): Type.CallableSchema | undefined => {
  const type = constructionExpressionType(expression)
  if (type._tag !== 'Available' || !Type.isCallable(type.type))
    return undefined
  const schema = type.type.schema
  return schema !== undefined &&
    (schema.binders.some(
      (binder) => binder.kind !== 'Lifetime' && !schema.substitution.has(Type.key(binder)),
    ) ||
      schema.constraints.length > 0 ||
      schema.evidence.length > 0)
    ? schema
    : undefined
}

const canonicalFunctionKey = (declaration: DeclarationFact): string | undefined =>
  declaration.canonical._tag === 'Canonical'
    ? `${declaration.canonical.id.module}\u0000${declaration.canonical.id.name}`
    : undefined

/** Where a callable value in a relay body comes from, before the module's other relays are known. */
export type CallableSource =
  | { readonly _tag: 'Parameter'; readonly ordinal: number }
  | { readonly _tag: 'Binding'; readonly ordinal: number; readonly source: CallableSource }
  | {
      readonly _tag: 'Call'
      readonly target: string
      readonly arguments: ReadonlyArray<{
        readonly parameter: number
        readonly source: CallableSource
      }>
    }

/** One place a constrained callable value would leave compile time. */
export type CallableEscape =
  | { readonly _tag: 'Value'; readonly at: AuthoredHir.Anchor }
  | {
      readonly _tag: 'Call'
      /** The canonical callee, which may turn out to relay one of its arguments. */
      readonly target?: string
      readonly arguments: ReadonlyArray<{
        readonly parameter: number
        readonly at: AuthoredHir.Anchor
      }>
      /** Set when the call's own result is a constrained callable. */
      readonly result?: AuthoredHir.Anchor
    }

/**
 * How constrained callables move through one body. The escape rule is decided per module, because
 * whether a call relays its argument depends on the callee's body; each body publishes only its
 * own half, so a reused body takes part without being read again.
 */
export interface CallableFlow {
  /**
   * Present when the body is only lexical binds followed by one return. Admitting any other
   * statement would erase observable work when lowering replaces the chain by its originating
   * callable recipe.
   */
  readonly relay?: { readonly leading: ReadonlyArray<number>; readonly source: CallableSource }
  readonly escapes: ReadonlyArray<CallableEscape>
}

const callableSourceOf = (
  current: ExpressionFact | Tir.Expression,
  bindings: ReadonlySet<number> = new Set(),
  tirBindings: ReadonlyMap<number, Tir.Expression> = new Map(),
  index?: DeclarationIndex.Index,
): CallableSource | undefined => {
  if ('origin' in current) {
    if (current._tag === 'Move')
      return callableSourceOf(current.subject, bindings, tirBindings, index)
    if (current._tag === 'ParameterReference')
      return { _tag: 'Parameter', ordinal: current.parameter.ordinal }
    if (current._tag === 'BindingReference') {
      const ordinal = current.binding.ordinal
      if (bindings.has(ordinal)) return undefined
      const initializer = tirBindings.get(ordinal)
      if (initializer === undefined) return undefined
      const source = callableSourceOf(
        initializer,
        new Set(bindings).add(ordinal),
        tirBindings,
        index,
      )
      return source === undefined ? undefined : { _tag: 'Binding', ordinal, source }
    }
    if (current._tag !== 'Call') return undefined
    const declaration =
      index === undefined ? undefined : DeclarationFacts.byCanonical(index, current.target)
    const parameters =
      declaration?._tag === 'FunctionDeclaration'
        ? declaration.parameters.filter((parameter) => parameter.phase !== 'Static')
        : []
    return {
      _tag: 'Call',
      target: `${current.target.module}\u0000${current.target.name}`,
      arguments: current.arguments.flatMap((argument, position) => {
        const source = callableSourceOf(argument, bindings, tirBindings, index)
        return source === undefined
          ? []
          : [{ parameter: parameters.at(position)?.id.ordinal ?? position, source }]
      }),
    }
  }
  if (current._tag === 'Move')
    return callableSourceOf(current.subject, bindings, tirBindings, index)
  if (current._tag === 'Identifier') {
    if (current.reference._tag === 'Resolved')
      return { _tag: 'Parameter', ordinal: current.reference.parameter.id.ordinal }
    if (current.reference._tag !== 'ResolvedBinding') return undefined
    const ordinal = current.reference.binding.id.ordinal
    if (bindings.has(ordinal)) return undefined
    const source = callableSourceOf(
      current.reference.binding.initializer,
      new Set(bindings).add(ordinal),
      tirBindings,
      index,
    )
    return source === undefined ? undefined : { _tag: 'Binding', ordinal, source }
  }
  if (current._tag !== 'Call' || current.reference._tag !== 'Resolved') return undefined
  const target = canonicalFunctionKey(current.reference.declaration)
  if (target === undefined) return undefined
  return {
    _tag: 'Call',
    target,
    arguments: current.mappings.flatMap((mapping) => {
      const source = callableSourceOf(mapping.argument.expression, bindings, tirBindings, index)
      return source === undefined ? [] : [{ parameter: mapping.parameter.id.ordinal, source }]
    }),
  }
}

const callableFlowOf = (
  fn: FunctionFact,
  index: DeclarationIndex.Index,
  builder: BodyBuilder.BodyBuilder,
): CallableFlow => {
  const escapes: Array<CallableEscape> = []
  const tirBindings = new Map<number, Tir.Expression>()
  visitStatementFacts(fn.statements, {
    statement: (statement) => {
      if (statement._tag === 'BindStatement' && 'origin' in statement.binding.initializer)
        tirBindings.set(statement.binding.id.ordinal, statement.binding.initializer)
    },
    descendExpressions: false,
  })
  const value = (expression: ExpressionFact | Tir.Expression): void => {
    if (constrainedCallableSchema(expression) !== undefined)
      escapes.push({ _tag: 'Value', at: constructionExpressionAnchor(expression) })
  }
  const expressionEscape = (expression: ExpressionFact): void => {
    if (expression._tag === 'StructLiteral' || expression._tag === 'UnionVariant') {
      for (const initializer of expression.initializers) value(initializer.expression)
      return
    }
    if (expression._tag === 'ArrayLiteral') {
      for (const element of expression.elements) value(element.expression)
      return
    }
    if (expression._tag === 'CallableSection') {
      for (const capture of expression.captures) value(capture.expression)
      return
    }
    if (expression._tag === 'EffectBlock') {
      for (const capture of expression.captures)
        if (capture.reference._tag === 'BindingFact') value(capture.reference.initializer)
      return
    }
    if (expression._tag === 'Match') {
      value(expression)
      return
    }
    if (expression._tag === 'CallableApply') {
      for (const argument of expression.arguments) value(argument.expression)
      return
    }
    if (expression._tag !== 'Call') return
    const target =
      expression.reference._tag === 'Resolved'
        ? canonicalFunctionKey(expression.reference.declaration)
        : undefined
    const arguments_ = expression.mappings.flatMap((mapping) =>
      constrainedCallableSchema(mapping.argument.expression) === undefined
        ? []
        : [
            {
              parameter: mapping.parameter.id.ordinal,
              at: constructionExpressionAnchor(mapping.argument.expression),
            },
          ],
    )
    const constrained = constrainedCallableSchema(expression) !== undefined
    if (arguments_.length === 0 && !constrained) return
    escapes.push({
      _tag: 'Call',
      ...(target === undefined ? {} : { target }),
      arguments: arguments_,
      ...(constrained ? { result: expression.anchor } : {}),
    })
  }
  visitStatementFacts(fn.statements, {
    statement: (statement) => {
      if (statement._tag === 'ReturnStatement') value(statement.expression)
      else if (statement._tag === 'WriteStatement') value(statement.value)
    },
    expression: expressionEscape,
    node: (expression) => {
      const decision = BodyBuilder.expressionDecision(builder, expression)
      if (decision !== undefined) {
        expressionEscape(decision)
        return
      }
      if (expression._tag === 'Construct' || expression._tag === 'ConstructUnionVariant') {
        for (const field of expression.fields) value(field.value)
        return
      }
      if (expression._tag === 'ArrayConstruct') {
        for (const element of expression.elements) value(element)
        return
      }
      if (expression._tag === 'CallableSection') {
        for (const capture of expression.captures) value(capture.value)
        return
      }
      if (expression._tag === 'Match') {
        value(expression)
        return
      }
      if (expression._tag === 'CallableApply') {
        for (const argument of expression.arguments) value(argument)
        return
      }
      if (expression._tag !== 'Call') return
      const declaration = DeclarationFacts.byCanonical(index, expression.target)
      const parameters =
        declaration?._tag === 'FunctionDeclaration'
          ? declaration.parameters.filter((parameter) => parameter.phase !== 'Static')
          : []
      const arguments_ = expression.arguments.flatMap((argument, position) =>
        constrainedCallableSchema(argument) === undefined
          ? []
          : [
              {
                parameter: parameters.at(position)?.id.ordinal ?? position,
                at: constructionExpressionAnchor(argument),
              },
            ],
      )
      const constrained = constrainedCallableSchema(expression) !== undefined
      if (arguments_.length === 0 && !constrained) return
      escapes.push({
        _tag: 'Call',
        target: `${expression.target.module}\u0000${expression.target.name}`,
        arguments: arguments_,
        ...(constrained ? { result: constructionExpressionAnchor(expression) } : {}),
      })
    },
  })
  const leading = fn.statements.slice(0, -1)
  const terminal = fn.statements.at(-1)
  const source =
    fn.declaration.parameters.length === 1 &&
    terminal?._tag === 'ReturnStatement' &&
    leading.every((statement) => statement._tag === 'BindStatement')
      ? callableSourceOf(terminal.expression, new Set(), tirBindings, index)
      : undefined
  return Object.freeze({
    ...(source === undefined
      ? {}
      : {
          relay: {
            leading: leading.flatMap((statement) =>
              statement._tag === 'BindStatement' ? [statement.binding.id.ordinal] : [],
            ),
            source,
          },
        }),
    escapes: Object.freeze(escapes),
  })
}

/** The parameter a relay body hands back, once the relays it calls are resolved. */
const relayedParameter = (
  key_: string,
  flows: ReadonlyMap<string, CallableFlow>,
  resolving: ReadonlySet<string> = new Set(),
): number | undefined => {
  const relay = flows.get(key_)?.relay
  if (relay === undefined || resolving.has(key_)) return undefined
  const inner = new Set(resolving).add(key_)
  const visited = new Set<number>()
  const resolve = (source: CallableSource): number | undefined => {
    if (source._tag === 'Parameter') return source.ordinal
    if (source._tag === 'Binding') {
      visited.add(source.ordinal)
      return resolve(source.source)
    }
    const forwarded = relayedParameter(source.target, flows, inner)
    const argument = source.arguments.find((entry) => entry.parameter === forwarded)
    return argument === undefined ? undefined : resolve(argument.source)
  }
  const forwarded = resolve(relay.source)
  return forwarded === undefined || relay.leading.some((ordinal) => !visited.has(ordinal))
    ? undefined
    : forwarded
}

const constrainedCallableEscapeDiagnostics = (
  bodies: ReadonlyArray<CheckedBody>,
): ReadonlyArray<Diagnostic.Located> => {
  const source = bodies.filter((body) => !body.hidden)
  const flows = new Map(
    source.flatMap((body) => {
      const key_ = canonicalFunctionKey(body.declaration)
      return key_ === undefined ? [] : [[key_, body.results.callables] as const]
    }),
  )
  const diagnostics: Array<Diagnostic.Located> = []
  const seen = new Set<string>()
  const reject = (at: AuthoredHir.Anchor): void => {
    const key_ = AuthoredIdentity.anchorKey(at)
    if (seen.has(key_)) return
    seen.add(key_)
    diagnostics.push(Diagnostic.nonConcreteSpecialization('constrained callable', Location.at(at)))
  }
  for (const body of source)
    for (const escape of body.results.callables.escapes) {
      if (escape._tag === 'Value') {
        reject(escape.at)
        continue
      }
      const forwarded =
        escape.target === undefined ? undefined : relayedParameter(escape.target, flows)
      let relayed = false
      for (const argument of escape.arguments)
        if (argument.parameter === forwarded) relayed = true
        else reject(argument.at)
      if (escape.result !== undefined && !relayed) reject(escape.result)
    }
  return Object.freeze(diagnostics)
}

const lexicalScopesOf = (fn: FunctionFact): ReadonlyArray<LexicalScopeFact> => {
  const scopes: Array<LexicalScopeFact> = []
  {
    let ordinal = 0
    const add = (options: {
      readonly parent?: LexicalScopeId
      readonly first: AuthoredHir.Anchor
      readonly last?: AuthoredHir.Anchor
      readonly parameters?: ReadonlyArray<ParameterFact>
      readonly bindings?: ReadonlyArray<BindingDeclarationFact>
      readonly patternBindings?: ReadonlyArray<PatternBindingFact>
    }): LexicalScopeId => {
      const id = Object.freeze({
        _tag: 'LexicalScopeId' as const,
        function: fn.declaration.id,
        ordinal,
      })
      ordinal += 1
      scopes.push(
        Object.freeze({
          _tag: 'LexicalScope',
          id,
          ...(options.parent === undefined ? {} : { parent: options.parent }),
          first: options.first,
          last: options.last ?? options.first,
          parameters: Object.freeze(Array.from(options.parameters ?? [])),
          // A scope names its locals; their initializers are working records and stay behind.
          bindings: Object.freeze(
            (options.bindings ?? []).map(
              ({ initializer: _initializer, exactCallable: _exactCallable, ...local }) =>
                Object.freeze(local),
            ),
          ),
          patternBindings: Object.freeze(Array.from(options.patternBindings ?? [])),
        }),
      )
      return id
    }
    const anchorOf = (statement: StatementFact): AuthoredHir.Anchor =>
      statement._tag === 'BindStatement' ? statement.binding.anchor : statement.anchor
    const extentOf = (
      statements: ReadonlyArray<StatementFact>,
      fallback: AuthoredHir.Anchor,
    ): { readonly first: AuthoredHir.Anchor; readonly last: AuthoredHir.Anchor } => {
      const first = statements.at(0)
      const last = statements.at(-1)
      return first === undefined || last === undefined
        ? { first: fallback, last: fallback }
        : { first: anchorOf(first), last: anchorOf(last) }
    }
    let visitStatements: (
      statements: ReadonlyArray<StatementFact>,
      parent: LexicalScopeId | undefined,
      fallback: AuthoredHir.Anchor,
    ) => LexicalScopeId
    const visitExpression = (
      expression: ExpressionFact | Tir.Expression,
      parent: LexicalScopeId,
    ): void => {
      if ('origin' in expression) {
        for (const child of Tir.expressionChildren(expression)) visitExpression(child, parent)
        return
      }
      if (expression._tag === 'Match') {
        visitExpression(expression.scrutinee, parent)
        for (const arm of expression.arms) {
          const armScope = add({
            parent,
            first: arm.anchor,
            patternBindings: arm.bindings,
          })
          if (arm.guard !== undefined) visitExpression(arm.guard, armScope)
          if (arm.body._tag === 'Expression') visitExpression(arm.body.expression, armScope)
          else visitStatements(arm.body.statements, armScope, arm.body.anchor)
        }
        return
      }
      if (expression._tag === 'EffectBlock') {
        visitStatements(expression.statements, parent, expression.anchor)
        return
      }
      for (const child of directExpressionChildren(expression)) visitExpression(child, parent)
    }
    visitStatements = (
      statements: ReadonlyArray<StatementFact>,
      parent: LexicalScopeId | undefined,
      fallback: AuthoredHir.Anchor,
    ): LexicalScopeId => {
      const current = add({
        ...(parent === undefined ? {} : { parent }),
        ...extentOf(statements, fallback),
        ...(parent === undefined ? { parameters: fn.declaration.parameters } : {}),
        bindings: statements.flatMap((statement) =>
          statement._tag === 'BindStatement' ? [statement.binding] : [],
        ),
        patternBindings: statements.flatMap((statement) =>
          statement._tag === 'PatternBindStatement' ? statement.selection.bindings : [],
        ),
      })
      for (const statement of statements) {
        for (const expression of directStatementExpressions(statement))
          visitExpression(expression, current)
        if (statement._tag === 'UnsafeStatement')
          visitStatements(statement.statements, current, statement.anchor)
        else if (statement._tag === 'IfStatement') {
          visitStatements(statement.taken, current, statement.anchor)
          visitStatements(statement.otherwise, current, statement.anchor)
        } else if (statement._tag === 'IfLetStatement') {
          const takenScope = add({
            parent: current,
            first: statement.anchor,
            patternBindings: statement.selection.bindings,
          })
          visitStatements(statement.taken, takenScope, statement.anchor)
          visitStatements(statement.otherwise, current, statement.anchor)
        } else if (statement._tag === 'WhileStatement')
          visitStatements(statement.body, current, statement.anchor)
      }
      return current
    }
    visitStatements(fn.statements, undefined, fn.declaration.anchor)
  }
  return Object.freeze(scopes)
}

/** Elaborates every declaration body into immutable facts and the module's TIR. */
export interface Input {
  readonly bodyQuery?: BodyQuery.BodyQuery
  readonly authored: AuthoredLowering.Lowered
  readonly headers: DeclarationFacts.ModuleHeaders
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
}

/** The authored block of a callable declaration body, when it has one. */
export const authoredBody = (
  authored: AuthoredLowering.Lowered,
  declaration: DeclarationFact,
): AuthoredHir.Block | undefined => {
  const found = AuthoredLowering.declarationOf(authored, declaration.owner)
  return found?.body._tag === 'CallableBody' ? found.body.block : undefined
}

const runtimeTirFunction = (
  context: SemanticContext.SemanticContext,
  fact: FunctionFact,
  index: DeclarationIndex.Index,
  builder: BodyBuilder.BodyBuilder,
): Tir.TirFunction => {
  const lifetimeAssumptions = Lifetime.assumptions(fact.lifetimeFlow?.input.constraints ?? [])
  const lifetimeCompatibility = TypeCompatibility.context({
    assumptions: lifetimeAssumptions,
    nominalVariance: NominalVariance.derive(index).summaries,
  })
  const originalEntryRegion =
    fact.regionOrder.at(0) ??
    Object.freeze({
      _tag: 'TirRegion' as const,
      function: fact.declaration.id,
      ordinal: 0,
    })
  const baseContract = Tir.contractOf(fact.declaration)
  if (
    fact.declaration.functionKind === 'Effect' &&
    fact.declaration.returnType._tag === 'Resolved' &&
    baseContract._tag === 'Contract'
  ) {
    const referenced = effectCaptureFacts(
      context,
      fact.statements,
      0,
      index,
      copyAssumptionsOf(fact.declaration),
      Object.freeze({ builder }),
    )
    // Every runtime argument enters the deferred environment at construction, even when the body
    // never reads it. Retaining declaration order also retains reverse-argument cleanup order.
    const referencedParameterOrdinals = new Set(
      referenced.flatMap((capture): ReadonlyArray<number> =>
        capture.reference._tag === 'ParameterDeclaration' ? [capture.reference.id.ordinal] : [],
      ),
    )
    const unreferenced = fact.declaration.parameters.flatMap(
      (parameter): ReadonlyArray<EffectCaptureFact> => {
        if (parameter.phase !== 'Runtime' || referencedParameterOrdinals.has(parameter.id.ordinal))
          return []
        const declared = parameter.declaredType
        return [
          Object.freeze({
            _tag: 'EffectCapture',
            reference: parameter,
            access:
              declared._tag === 'Resolved' && Type.isSlice(declared.type)
                ? declared.type.access
                : 'Take',
            span: context.spanOf(parameter.anchor),
            anchor: parameter.anchor,
          }),
        ]
      },
    )
    const captures = [...referenced, ...unreferenced]
      .sort(
        (left, right) =>
          left.reference.id.ordinal - right.reference.id.ordinal ||
          left.span.start - right.span.start,
      )
      .map((capture) => {
        if (capture.reference._tag !== 'ParameterDeclaration') return capture
        const declared = capture.reference.declaredType
        if (declared._tag !== 'Resolved' || Type.isSlice(declared.type)) return capture
        return Object.freeze({
          ...capture,
          access: ConformanceProof.copyType(
            index,
            declared.type,
            copyAssumptionsOf(fact.declaration),
          )
            ? ('Copy' as const)
            : ('Take' as const),
        })
      })
    const semanticCaptureAccess = (capture: EffectCaptureFact): Type.Effect['access'] | 'Copy' => {
      if (capture.reference._tag !== 'ParameterDeclaration') return capture.access
      const declared = capture.reference.declaredType
      if (declared._tag !== 'Resolved') return capture.access
      if (Type.isEffect(declared.type)) return declared.type.access
      if (Type.isCallable(declared.type)) return declared.type.mode
      if (Type.isReference(declared.type) || Type.isSlice(declared.type))
        return declared.type.access
      return capture.access
    }
    const semanticAccesses = captures.map(semanticCaptureAccess)
    let access: 'Take' | 'Exclusive' | 'Shared'
    if (semanticAccesses.some((capture) => capture === 'Take')) {
      access = 'Take'
    } else if (semanticAccesses.some((capture) => capture === 'Exclusive')) {
      access = 'Exclusive'
    } else {
      access = 'Shared'
    }
    const type = Type.effectWithRows(
      fact.declaration.returnType.type,
      fact.declaration.failureRow.row,
      { ...DeclarationFacts.executableLifetimes(fact.declaration), lifetimeBinders: [] },
      access,
      fact.declaration.requirementRow.row,
    )
    const siteAnchor = fact.bodyAnchor ?? fact.declaration.anchor
    const siteSpan = context.spanOf(siteAnchor)
    const entryRegion: Tir.RegionId = Object.freeze({
      _tag: 'TirRegion',
      function: fact.declaration.id,
      ordinal: Math.max(-1, ...fact.regionOrder.map((region) => region.ordinal)) + 1,
    })
    const effectBlock: Extract<Tir.Expression, { readonly _tag: 'EffectBlock' }> = Object.freeze({
      _tag: 'EffectBlock',
      site: Object.freeze({
        _tag: 'EffectSiteId',
        function: fact.declaration.id,
        ...(fact.declaration.canonical._tag === 'Canonical'
          ? { owner: fact.declaration.canonical.id }
          : {}),
        ordinal: -1,
        span: siteSpan,
        at: siteAnchor,
      }),
      statements: lowerStatements(fact.statements, {
        context,
        builder,
        lifetimeAssumptions,
        lifetimeCompatibility,
        ...(fact.declaration.opaqueResult === undefined
          ? {}
          : { opaqueResultFamily: fact.declaration.opaqueResult.family }),
        resultType: fact.declaration.returnType.type,
        functionId: fact.declaration.id,
        eraseIntrinsicSections: true,
      }),
      captures: Object.freeze(
        captures.map((capture) =>
          Object.freeze({
            ...(capture.reference._tag === 'BindingFact'
              ? { binding: BodyBuilder.localId(builder, capture.reference.id) }
              : {}),
            ...(capture.reference._tag === 'PatternBinding'
              ? { pattern: BodyBuilder.localId(builder, capture.reference.id) }
              : {}),
            ...(capture.reference._tag === 'ParameterDeclaration'
              ? { parameter: BodyBuilder.localId(builder, capture.reference.id) }
              : {}),
            access: capture.access,
            span: capture.span,
          }),
        ),
      ),
      type,
      span: siteSpan,
      origin: Tir.synthetic(siteAnchor, 'effect-body'),
    })
    return Object.freeze({
      _tag: 'TirFunction',
      declaration: fact.declaration,
      contract: Object.freeze({
        _tag: 'Contract',
        unsafe: baseContract.unsafe,
        parameters: baseContract.parameters,
        result: type,
        constraints: baseContract.constraints,
      }),
      entryRegion,
      regionOrder: Object.freeze([entryRegion, ...fact.regionOrder]),
      statements: Object.freeze([
        Object.freeze({
          _tag: 'Return',
          expression: effectBlock,
          region: entryRegion,
          span: siteSpan,
          origin: Tir.synthetic(siteAnchor, 'effect-return'),
        }),
      ]),
    })
  }
  return Object.freeze({
    _tag: 'TirFunction',
    declaration: fact.declaration,
    contract: baseContract,
    entryRegion: originalEntryRegion,
    regionOrder: fact.regionOrder,
    statements: lowerStatements(fact.statements, {
      context,
      builder,
      lifetimeAssumptions,
      lifetimeCompatibility,
      ...(fact.declaration.opaqueResult === undefined
        ? {}
        : { opaqueResultFamily: fact.declaration.opaqueResult.family }),
      ...(fact.declaration.returnType._tag === 'Resolved'
        ? { resultType: fact.declaration.returnType.type }
        : {}),
      ...(fact.resultRepresentation === undefined
        ? {}
        : { resultRepresentation: fact.resultRepresentation }),
      functionId: fact.declaration.id,
      eraseIntrinsicSections: true,
    }),
  })
}

/**
 * Everything construction publishes for one source declaration: its own body first, then the
 * compiler-made bodies it produced, and what checking them reported. It is the unit of reuse.
 */
export interface CheckedUnit {
  readonly bodies: ReadonlyArray<CheckedBody>
  readonly diagnostics: ReadonlyArray<Diagnostic.Located>
}

const staticStructureOf = (
  fact: FunctionFact,
  index: DeclarationIndex.Index,
): StaticStructure | undefined => {
  let found: StaticStructure | undefined
  const recordNode = (expression: Tir.Expression): void => {
    if (found !== undefined) return
    if (expression._tag === 'ConstantReference') found = 'UnresolvedConstant'
    else if (expression._tag === 'CompileError') found = 'CompileError'
    else if (expression._tag === 'StaticCall') found = 'StaticCall'
    else if (expression._tag === 'Call' || expression._tag === 'EffectConstruct') {
      const declaration = DeclarationFacts.byCanonical(index, expression.target)
      if (
        declaration?._tag === 'FunctionDeclaration' &&
        (declaration.phase === 'Static' ||
          declaration.parameters.some((parameter) => parameter.phase === 'Static'))
      )
        found = 'StaticCall'
    }
  }
  visitStatementFacts(fact.statements, {
    statement: (statement) => {
      if (
        found === undefined &&
        statement._tag === 'BindStatement' &&
        statement.binding.phase === 'Static'
      )
        found = 'StaticBinding'
    },
    expression: (expression) => {
      if (found !== undefined) return
      if (expression._tag === 'Constant' && expression.value === undefined)
        found = 'UnresolvedConstant'
      else if (expression._tag === 'CompileError') found = 'CompileError'
      else if (
        expression._tag === 'Call' &&
        expression.reference._tag === 'Resolved' &&
        (expression.reference.declaration.phase === 'Static' ||
          expression.reference.declaration.parameters.some(
            (parameter) => parameter.phase === 'Static',
          ))
      )
        found = 'StaticCall'
    },
    node: recordNode,
  })
  return found
}

const staticIterationRows = (
  iterations: ReadonlyArray<StaticIterationFact>,
): ReadonlyArray<StaticIterationRow> =>
  Object.freeze(
    iterations.map((iteration) =>
      Object.freeze({
        at: iteration.anchor,
        state: iteration.state,
        elements: Object.freeze(iteration.scopes.map((scope) => scope.binding.staticValue)),
        nested: Object.freeze(
          iteration.scopes.map((scope) => staticIterationRows(scope.staticIterations)),
        ),
      }),
    ),
  )

const expressionTypesOf = (fact: FunctionFact): ReadonlyArray<ExpressionTypeRow> => {
  const rows: Array<ExpressionTypeRow> = []
  visitStatementFacts(fact.statements, {
    expression: (expression) => {
      if (expression.type._tag !== 'Available') return
      rows.push(
        Object.freeze({
          at: expression.anchor,
          type: expression.type.type,
          ...(expression._tag === 'CallableSection' && expression.anonymous !== undefined
            ? { presentation: SemanticDisplay.anonymousCallable(expression, expression.anonymous) }
            : {}),
        }),
      )
    },
  })
  return Object.freeze(rows)
}

/** A `static fn` body keeps its static structure: it runs in the evaluator and never at run time. */
const staticTirFunction = (
  context: SemanticContext.SemanticContext,
  fact: FunctionFact,
  builder: BodyBuilder.BodyBuilder,
): Tir.TirFunction =>
  Object.freeze({
    _tag: 'TirFunction',
    declaration: fact.declaration,
    contract: Tir.contractOf(fact.declaration),
    entryRegion:
      fact.regionOrder.at(0) ??
      Object.freeze({ _tag: 'TirRegion' as const, function: fact.declaration.id, ordinal: 0 }),
    regionOrder: fact.regionOrder,
    statements: staticLowering(context, builder).statements(fact.statements),
  })

/**
 * A checked body as one revision reads it. The body itself names authored nodes only; every
 * position it shows is stamped here from the node beside it, and its header is this revision's.
 */
export const presentBody = (
  self: CheckedBody,
  context: SemanticContext.SemanticContext,
  declaration: DeclarationFact = self.declaration,
): CheckedBody => {
  const stamped = Tir.stamp(self.results, context.spanOf)
  const results =
    stamped.lifetimes === undefined
      ? stamped
      : Object.freeze({
          ...stamped,
          lifetimes: LifetimeFlow.present(stamped.lifetimes, context),
        })
  return Object.freeze({
    artifact: self.artifact,
    declaration,
    hidden: self.hidden,
    function: Tir.present(self.function, context.spanOf, declaration),
    results,
  })
}

/** Publishes one analyzed body: its nodes, when it runs, and the tables later stages read. */
export const checkedBody = (
  context: SemanticContext.SemanticContext,
  index: DeclarationIndex.Index,
  fact: FunctionFact,
  /** Set for a compiler-made body: the artifact whose construction produced it. */
  parent?: Tir.ArtifactId,
  request: Tir.ArtifactId['request'] = Object.freeze({ _tag: 'Check' }),
  construction?: BodyBuilder.BodyBuilder,
): CheckedBody => {
  const hidden = parent !== undefined
  const artifact: Tir.ArtifactId = Object.freeze({
    owner: fact.declaration.owner,
    request,
    ...(parent === undefined ? {} : { parent }),
  })
  const builder = construction ?? BodyBuilder.make(artifact)
  if (Tir.artifactKey(builder.artifact) !== Tir.artifactKey(artifact))
    throw new RangeError('TIR body builder belongs to another artifact')
  const lowered = BodyBuilder.index(
    builder,
    fact.declaration.phase === 'Static'
      ? staticTirFunction(context, fact, builder)
      : runtimeTirFunction(context, fact, index, builder),
  )
  const staticStructure = staticStructureOf(fact, index)
  const results: BodyResults = Object.freeze({
    evidence: Object.freeze(Array.from(builder.evidence)),
    causes: Object.freeze(Array.from(builder.causes)),
    occurrences: fact.occurrences,
    hints: fact.hints,
    opaqueEvidence: fact.opaqueEvidence,
    ...(fact.lifetimeFlow === undefined ? {} : { lifetimes: fact.lifetimeFlow }),
    scopes: lexicalScopesOf(fact),
    aggregates: fact.generatedAggregates,
    callables: callableFlowOf(fact, index, builder),
    ...(staticStructure === undefined ? {} : { staticStructure }),
    expressionTypes: expressionTypesOf(fact),
    staticIterations: staticIterationRows(fact.staticIterations),
  })
  return Object.freeze({
    artifact,
    declaration: fact.declaration,
    hidden,
    function: lowered,
    results,
  })
}

type Records = {
  readonly functions: ReadonlyArray<FunctionFact>
  readonly hiddenFunctions: ReadonlyArray<FunctionFact>
}
const inputs = new WeakMap<Result, Omit<Input, 'bodyQuery'>>()
const inspected = new WeakMap<Result, Records>()

/**
 * The working records a module's bodies are built from, for tests of construction and for the
 * inspector, which shows construction itself. Construction keeps none of them: they are built
 * again here, from the same inputs, only when someone asks. No compiler stage reads them.
 */
export const records = (self: Result): Records => {
  const known = inspected.get(self)
  if (known !== undefined) return known
  const input = inputs.get(self)
  if (input === undefined) throw new RangeError('Only an elaborated module has working records')
  const context = SemanticContext.make(input.authored)
  const hiddenAnalyses: Array<import('./ExpressionAnalysis.js').FunctionAnalysis> = []
  const functions = input.headers.declarations
    .filter((declaration) => declaration.foreign === undefined)
    .map(
      (declaration) =>
        analyzeFunctionBody(
          context,
          declaration,
          input.headers.declarations,
          Object.freeze({
            scope: input.scope,
            index: input.index,
            hiddenFunctions: hiddenAnalyses,
          }),
        ).fact,
    )
  const result = Object.freeze({
    functions: Object.freeze(functions),
    hiddenFunctions: Object.freeze(hiddenAnalyses.map((analysis) => analysis.fact)),
  })
  inspected.set(self, result)
  return result
}

export const elaborateModule = (input: Input): Result => {
  const { authored, headers, scope, index } = input
  const context = SemanticContext.make(authored)
  const declarations = headers.declarations
  // A foreign header has a native body: it is indexed and callable but never analyzed here.
  const units = declarations
    .filter((declaration) => declaration.foreign === undefined)
    .map((declaration): CheckedUnit => {
      const build = (): BodyQuery.Built => {
        const hiddenFunctions: Array<import('./ExpressionAnalysis.js').FunctionAnalysis> = []
        const analysis = analyzeFunctionBody(
          context,
          declaration,
          declarations,
          Object.freeze({ scope, index, hiddenFunctions }),
        )
        const own = checkedBody(
          context,
          index,
          analysis.fact,
          undefined,
          undefined,
          analysis.builder,
        )
        return {
          unit: Object.freeze({
            bodies: Object.freeze([
              own,
              ...hiddenFunctions.map((hidden) =>
                checkedBody(context, index, hidden.fact, own.artifact, undefined, hidden.builder),
              ),
            ]),
            diagnostics: analysis.diagnostics,
          }),
          records: [analysis, hiddenFunctions],
        }
      }
      return input.bodyQuery === undefined
        ? build().unit
        : BodyQuery.check(input.bodyQuery, context, authored, scope, declaration, build)
    })
  const constantDiagnostics = headers.constants.flatMap((constant) =>
    constant.name._tag === 'Present'
      ? analyzeConstant(context, constant, constant.name.anchor, true).diagnostics
      : [],
  )
  const all = units.flatMap((unit) => unit.bodies)
  const bodies = Object.freeze([
    ...all.filter((body) => !body.hidden),
    ...all.filter((body) => body.hidden),
  ])
  const tir: Tir.Module = Object.freeze({
    _tag: 'TirModule',
    module: authored.module.owner.module,
    functions: Object.freeze(
      bodies.flatMap((body) => (body.declaration.phase === 'Static' ? [] : [body.function])),
    ),
  })

  const result: Result = Object.freeze({
    _tag: 'Elaboration',
    authored,
    generatedAggregates: Object.freeze(bodies.flatMap((body) => body.results.aggregates)),
    lexicalScopes: Object.freeze(bodies.flatMap((body) => body.results.scopes)),
    tir,
    bodies,
    diagnostics: Object.freeze([
      ...headers.diagnostics,
      ...constantDiagnostics,
      ...units.flatMap((unit) => unit.diagnostics),
      ...constrainedCallableEscapeDiagnostics(bodies),
    ]),
  })
  inputs.set(result, Object.freeze({ authored, headers, scope, index }))
  return result
}

/** Looks up every present declaration with the exact requested spelling. */
export const declarationByName = dual<
  (spelling: string) => (self: Result) => DeclarationLookup,
  (self: Result, spelling: string) => DeclarationLookup
>(2, (self, spelling) =>
  lookupDeclaration(
    self.bodies.flatMap((body) => (body.hidden ? [] : [body.declaration])),
    spelling,
  ),
)

/** Looks up every present parameter with the exact requested spelling in one function. */
export const parameterByName = dual<
  (spelling: string) => (self: DeclarationFact) => ParameterLookup,
  (self: DeclarationFact, spelling: string) => ParameterLookup
>(2, (self, spelling) => lookupParameter(self.parameters, spelling))
