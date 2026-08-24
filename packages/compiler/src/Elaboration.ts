import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
import type * as CallableContract from './CallableContract.js'
import * as ConformanceProof from './ConformanceProof.js'
import type * as Constraint from './Constraint.js'
import * as DeclarationFacts from './DeclarationFacts.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Hir from './Hir.js'
import type * as Intrinsic from './Intrinsic.js'
import type * as Match from './Match.js'
import type * as NameResolution from './NameResolution.js'
import type * as Operator from './Operator.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import type * as StaticText from './StaticText.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as TargetConstant from './TargetConstant.js'
import type * as Token from './Token.js'
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
  readonly id: Hir.BindingId
  readonly name: DeclaredName
  readonly mutability: 'Immutable' | 'Mutable'
  readonly inferredType: ExpressionTypeFact
  readonly initializer: ExpressionFact
  readonly syntax: SyntaxTree.Node
}

/** A bare identifier resolved against enclosing parameters and preceding bindings. */
export type ParameterReferenceFact =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly parameter: ParameterFact
    }
  | {
      readonly _tag: 'ResolvedBinding'
      readonly spelling: string
      readonly token: Token.Token
      readonly binding: BindingDeclarationFact
    }
  | {
      readonly _tag: 'ResolvedPattern'
      readonly spelling: string
      readonly token: Token.Token
      readonly binding: PatternBindingFact
    }
  | {
      readonly _tag: 'Missing'
      readonly spelling: string
      readonly token: Token.Token
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly token: Token.Token
      readonly parameters: ReadonlyArray<ParameterFact>
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** The available, out-of-range, or syntax-unavailable integer-expression fact. */
export type IntegerExpressionFact =
  | {
      readonly _tag: 'Available'
      readonly type: SemanticType
      readonly value: bigint
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'OutOfRange'
      readonly type: SemanticType
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

export type FloatingExpressionFact =
  | {
      readonly _tag: 'Available'
      readonly type: Scalar.FloatSpelling
      readonly bits: bigint
      readonly spelling: string
      readonly token: Token.Token
      readonly syntax: SyntaxTree.Node
    }
  | { readonly _tag: 'Unavailable'; readonly syntax: SyntaxTree.Element }

export interface StaticTextExpressionFact {
  readonly _tag: 'StaticText'
  readonly data?: StaticText.Data
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One character literal carrying the single Unicode scalar value its body denotes. */
export interface CharacterExpressionFact {
  readonly _tag: 'Character'
  readonly value?: number
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** A call callee resolved against top-level declarations or unavailable after syntax recovery. */
export type CallReferenceFact =
  | {
      readonly _tag: 'Resolved'
      readonly spelling: string
      readonly token: Token.Token
      readonly declaration: DeclarationFact
    }
  | {
      readonly _tag: 'ResolvedBuiltin'
      readonly spelling: string
      readonly token: Token.Token
      readonly actor: string
      readonly operation: Hir.BuiltinOperation
      readonly intrinsic: Intrinsic.OperationId
      readonly parameters: ReadonlyArray<SemanticType>
      readonly result: SemanticType
      readonly unsafe: boolean
      readonly returnedBorrowParameter?: number
    }
  | {
      readonly _tag: 'ResolvedIntrinsicContract'
      readonly spelling: string
      readonly token: Token.Token
      readonly intrinsic: Intrinsic.Operation
      readonly contract: CallableContract.CallableContract
    }
  | {
      readonly _tag: 'ResolvedServiceOperation'
      readonly spelling: string
      readonly token: Token.Token
      readonly service: DeclarationFacts.ServiceFact
      readonly operation: DeclarationFacts.ServiceOperationFact
    }
  /**
   * One operation of a type parameter's bound, reached through the bound's own name. The contract
   * is the interface's own declaration over the bounded parameter, checked once before any concrete
   * argument exists; which implementation runs is the witness's answer at specialization, not one
   * this reference records.
   */
  | {
      readonly _tag: 'ResolvedBoundOperation'
      readonly spelling: string
      readonly token: Token.Token
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
      readonly token: Token.Token
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Ambiguous'
      readonly spelling: string
      readonly token: Token.Token
      readonly declarations: ReadonlyArray<DeclarationFact>
      readonly cause?: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Unavailable'
      readonly syntax: SyntaxTree.Element
    }

/** Exact qualifier and member tokens retained beside semantic reference resolution. */
export type ReferencePathFact =
  | {
      readonly _tag: 'ReferencePath'
      readonly qualifier?: Token.Token
      readonly member: Token.Token
    }
  | { readonly _tag: 'UnavailableReferencePath'; readonly syntax: SyntaxTree.Element }

/** Exact source tokens and catalog identity for one recognized intrinsic member path. */
export type IntrinsicReferenceFact =
  | {
      readonly _tag: 'ResolvedIntrinsicReference'
      readonly actor: Intrinsic.Actor
      readonly operation: Intrinsic.Operation
      readonly actorToken: Token.Token
      readonly operationToken: Token.Token
    }
  | {
      readonly _tag: 'ResolvedCapabilityOperationReference'
      readonly actor: DeclarationFacts.StructFact | Intrinsic.Actor
      readonly operation: Intrinsic.Operation
      readonly actorToken: Token.Token
      readonly operationToken: Token.Token
    }
  | { readonly _tag: 'UnavailableIntrinsicReference'; readonly syntax: SyntaxTree.Element }

/** The available or unavailable type of one returned expression. */
export type ExpressionTypeFact =
  | { readonly _tag: 'Available'; readonly type: SemanticType }
  | { readonly _tag: 'Unavailable' }

/** One bare identifier expression with its local reference and type facts. */
export interface IdentifierExpressionFact {
  readonly _tag: 'Identifier'
  readonly reference: ParameterReferenceFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One `move <place>` expression with its consuming subject fact. */
export interface MoveExpressionFact {
  readonly _tag: 'Move'
  readonly subject: ExpressionFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

export type BorrowSelectorFact =
  | {
      readonly _tag: 'Field'
      readonly field: DeclarationFacts.FieldId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Index'
      readonly index: ExpressionFact
      readonly array: Type.FixedArray
      readonly bounds: Extract<BoundsFact, { readonly _tag: 'Proven' | 'Runtime' }>
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'SliceIndex'
      readonly index: ExpressionFact
      readonly slice: Type.Slice
      readonly span: SourceSpan.SourceSpan
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
      readonly owner: Hir.TemporaryOwnerId
      readonly value: ExpressionFact
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
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

/** One explicit whole-root slice borrow accepted only at an ordinary call boundary. */
export interface BorrowExpressionFact {
  readonly _tag: 'Borrow'
  readonly access: Type.Slice['access']
  readonly subject: ExpressionFact
  readonly formation: BorrowFormationFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
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
  readonly syntax: SyntaxTree.Node
}

export type PatternFieldState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationFacts.FieldFact }
  | { readonly _tag: 'Unknown'; readonly cause: Diagnostic.Identity }
  | {
      readonly _tag: 'Duplicate'
      readonly field: DeclarationFacts.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unavailable' }

export interface PatternFieldFact {
  readonly _tag: 'PatternField'
  readonly name: string | undefined
  readonly token?: Token.Token
  readonly state: PatternFieldState
  readonly binding?: PatternBindingFact
  readonly nested?: PatternFact
  readonly syntax: SyntaxTree.Node
}

export type PatternFact =
  | {
      readonly _tag: 'UnavailablePattern'
      readonly id: Match.PatternId
      readonly member?: undefined
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly complete: false
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'TypePattern'
      readonly id: Match.PatternId
      readonly member?: Type.Type
      readonly declared: DeclarationFacts.DeclaredTypeFact
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly complete: boolean
      readonly syntax: SyntaxTree.Node
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
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'UniversalPattern'
      readonly id: Match.PatternId
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
      readonly syntax: SyntaxTree.Node
    }

export interface MatchArmFact {
  readonly _tag: 'MatchArm'
  readonly id: Match.ArmId
  readonly pattern: PatternFact
  readonly bindings: ReadonlyArray<PatternBindingFact>
  readonly guard?: ExpressionFact
  readonly result: ExpressionFact
  readonly before: ReadonlyArray<Type.Type>
  readonly after: ReadonlyArray<Type.Type>
  readonly reachable: boolean
  readonly syntax: SyntaxTree.Node
}

export interface MatchExpressionFact {
  readonly _tag: 'Match'
  readonly id: Match.MatchId
  readonly access: Match.Access
  readonly scrutinee: ExpressionFact
  readonly members: ReadonlyArray<Type.Type>
  readonly arms: ReadonlyArray<MatchArmFact>
  readonly exhaustive: boolean
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One statement-form pattern decision shared by irrefutable let and refutable if-let. */
export interface PatternSelectionFact {
  readonly _tag: 'PatternSelection'
  readonly id: Match.MatchId
  readonly arm: Match.ArmId
  readonly access: Match.Access
  /** Authored initializer, retaining an outer move/borrow for ownership loan analysis. */
  readonly source: ExpressionFact
  readonly subject: ExpressionFact
  readonly members: ReadonlyArray<Type.Type>
  readonly pattern: PatternFact
  readonly bindings: ReadonlyArray<PatternBindingFact>
  readonly irrefutable: boolean
  readonly loanEnd: SourceSpan.SourceSpan
  readonly syntax: SyntaxTree.Node
}

export type StructTargetFact =
  | {
      readonly _tag: 'Resolved'
      readonly struct: DeclarationFacts.StructFact
      readonly type: Type.Nominal
      readonly token: Token.Token
    }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

export type StructInitializerState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationFacts.FieldFact }
  | { readonly _tag: 'Unknown'; readonly cause: Diagnostic.Identity }
  | {
      readonly _tag: 'Duplicate'
      readonly field: DeclarationFacts.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | {
      readonly _tag: 'TypeMismatch'
      readonly field: DeclarationFacts.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Inaccessible'
      readonly field: DeclarationFacts.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Unavailable' }

export interface StructInitializerFact {
  readonly _tag: 'StructInitializer'
  readonly name: string | undefined
  readonly token?: Token.Token
  readonly expression: ExpressionFact
  readonly state: StructInitializerState
  readonly syntax: SyntaxTree.Node
}

export interface StructTypeArgumentFact {
  readonly parameter: Type.Parameter
  readonly argument?: Type.GenericArgument
  readonly source: 'Explicit' | 'Inferred' | 'Unavailable'
  readonly origins: ReadonlyArray<SourceSpan.SourceSpan>
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
  readonly syntax: SyntaxTree.Node
}

export type ProjectionState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationFacts.FieldFact }
  | { readonly _tag: 'SliceLength' }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

export interface FieldProjectionExpressionFact {
  readonly _tag: 'FieldProjection'
  readonly subject: ExpressionFact
  readonly nominal?: Type.Nominal
  readonly borrowAccess?: Type.Slice['access']
  readonly fieldName: string | undefined
  readonly fieldToken?: Token.Token
  readonly state: ProjectionState
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One source-ordered array element with contextual compatibility retained independently. */
export interface ArrayElementFact {
  readonly _tag: 'ArrayElement'
  readonly ordinal: number
  readonly expression: ExpressionFact
  readonly expected?: SemanticType
  readonly compatibility:
    | { readonly _tag: 'Compatible' }
    | {
        readonly _tag: 'TypeMismatch'
        readonly expected: SemanticType
        readonly actual: SemanticType
      }
    | { readonly _tag: 'Unavailable' }
  readonly syntax: SyntaxTree.Node
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
  readonly syntax: SyntaxTree.Node
}

export type BoundsFact =
  | { readonly _tag: 'Proven'; readonly index: number; readonly length: number }
  | {
      readonly _tag: 'Invalid'
      readonly index: number
      readonly length: number
      readonly cause: Diagnostic.Identity
    }
  | { readonly _tag: 'Runtime'; readonly length: number }
  | { readonly _tag: 'RuntimeSlice' }
  | { readonly _tag: 'Unavailable' }

/** One typed checked array-place projection. */
export interface IndexProjectionExpressionFact {
  readonly _tag: 'IndexProjection'
  readonly subject: ExpressionFact
  readonly index: ExpressionFact
  readonly array?: Type.FixedArray
  readonly slice?: Type.Slice
  readonly elementType?: SemanticType
  readonly borrowAccess?: Type.Slice['access']
  readonly access: 'CopyRead' | 'ConsumeRequested'
  readonly bounds: BoundsFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One `true`/`false` literal expression fact. */
export interface BooleanExpressionFact {
  readonly _tag: 'Boolean'
  readonly value: boolean
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One reference to a typed compile-time scalar declaration. */
export interface ConstantExpressionFact {
  readonly _tag: 'Constant'
  readonly declaration: DeclarationFacts.ConstantFact
  readonly token: Token.Token
  readonly value?:
    | { readonly _tag: 'Boolean'; readonly value: boolean }
    | {
        readonly _tag: 'Integer'
        readonly value: bigint
        readonly type: SemanticType
        // Present when the declaration named a pointer-width fact instead of spelling a literal.
        // `value` then holds the widest selection; `Lower` re-selects it for the chosen target.
        readonly target?: TargetConstant.Selector
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
  readonly syntax: SyntaxTree.Node
}

/** One parenthesized expression retaining its concrete grouping. */
export interface GroupedExpressionFact {
  readonly _tag: 'Grouped'
  readonly expression: ExpressionFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
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
  readonly syntax: SyntaxTree.Node
}

/** One prefix or infix operator and its canonical builtin resolution. */
export interface OperatorExpressionFact {
  readonly _tag: 'Operator'
  readonly operator: Operator.Prefix | Operator.Infix
  readonly reference: CallReferenceFact
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly mappings: ReadonlyArray<BuiltinArgumentMappingFact>
  readonly contract: CallContractFact
  readonly interfaceOperation?: InterfaceOperationFact
  readonly witnessEffectSite?: Hir.EffectSiteId
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
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
  readonly reference: CallReferenceFact
  readonly path: ReferencePathFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One trailing value retained by an automatic trailing-argument section. */
export interface CallableCaptureFact {
  readonly _tag: 'CallableCapture'
  readonly ordinal: number
  readonly parameterOrdinal: number
  readonly expression: ExpressionFact
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
}

/** One hidden concrete section construction awaiting an ordered leading parameter prefix. */
export interface CallableSectionExpressionFact {
  readonly _tag: 'CallableSection'
  readonly site: Hir.CallableSiteId
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
  readonly syntax: SyntaxTree.Node
}

/** One ordinary invocation through a first-class callable expression. */
export interface CallableApplyExpressionFact {
  readonly _tag: 'CallableApply'
  readonly callee: ExpressionFact
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly mode: Type.CallableMode
  readonly contract?: Type.Callable
  /** Generic evidence learned from the newly supplied callable arguments. */
  readonly substitution: Type.Substitution
  readonly inferredProviderSelectors: ReadonlyArray<InferredProviderSelector>
  readonly provenance:
    | { readonly _tag: 'DirectCallableApplication' }
    | {
        readonly _tag: 'PipelineCallableApplication'
        readonly left: ExpressionFact
        readonly callable: ExpressionFact
        readonly evaluation: 'LeftThenCallable'
      }
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One outer binding or parameter captured by a lazy effect block. */
export interface EffectCaptureFact {
  readonly _tag: 'EffectCapture'
  readonly reference: BindingDeclarationFact | ParameterFact
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
  readonly span: SourceSpan.SourceSpan
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
}

/** One lazy imperative effect block and its capture-derived execution contract. */
export interface EffectExpressionFact {
  readonly _tag: 'EffectBlock'
  readonly site: Hir.EffectSiteId
  readonly representationOwner?: Type.ExecutableSpecializationOwner
  readonly statements: ReadonlyArray<StatementFact>
  readonly captures: ReadonlyArray<EffectCaptureFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly regions: ReadonlyArray<Hir.RegionId>
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One semantic expression fact at any returned or argument position. */
export type ExpressionFact =
  | {
      readonly _tag: 'Integer'
      readonly integer: IntegerExpressionFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Floating'
      readonly floating: FloatingExpressionFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | StaticTextExpressionFact
  | CharacterExpressionFact
  | {
      readonly _tag: 'Unit'
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | BooleanExpressionFact
  | ConstantExpressionFact
  | IdentifierExpressionFact
  | MoveExpressionFact
  | BorrowExpressionFact
  | MatchExpressionFact
  | StructLiteralExpressionFact
  | ArrayLiteralExpressionFact
  | FieldProjectionExpressionFact
  | IndexProjectionExpressionFact
  | GroupedExpressionFact
  | OperatorExpressionFact
  | ShortCircuitExpressionFact
  | FunctionItemExpressionFact
  | CallableSectionExpressionFact
  | CallableApplyExpressionFact
  | EffectExpressionFact
  | {
      readonly _tag: 'Run'
      readonly subject: ExpressionFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      /** Executes one typed Effect into ordinary Result data without catching traps. */
      readonly _tag: 'EffectResult'
      readonly reference: IntrinsicReferenceFact
      readonly protected: ExpressionFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      /** `Place.replace(place, value)`: swap one writable place, yielding its old value. */
      readonly _tag: 'PlaceReplace'
      readonly reference: IntrinsicReferenceFact
      readonly destination: ExpressionFact
      readonly root?: AssignmentRootFact
      readonly value: ExpressionFact
      readonly compatible: boolean
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'EffectBindRequirement'
      readonly reference: IntrinsicReferenceFact
      readonly protected: ExpressionFact
      readonly provider?: EffectRequirementBindingFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      /**
       * Member-selective recovery. `protectedRow`, `selected`, `handlerRow`, and `residualRow`
       * are the four rows `bootstrap-semantic-facts` requires this operation to record; the
       * residual is carried explicitly because it has no source-level type to recover it from.
       */
      readonly _tag: 'EffectCatch'
      readonly reference: IntrinsicReferenceFact
      readonly protected: ExpressionFact
      readonly handler: ExpressionFact
      readonly selected?: Type.Type
      readonly protectedRow: Type.FailureRow
      readonly handlerRow: Type.FailureRow
      readonly residualRow: Type.FailureRow
      readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'Call'
      readonly reference: CallReferenceFact
      readonly path: ReferencePathFact
      readonly typeArguments: ReadonlyArray<TypeArgumentFact>
      readonly arguments: ReadonlyArray<ArgumentFact>
      readonly mappings: ReadonlyArray<ArgumentMappingFact>
      readonly contract: CallContractFact
      readonly witnessEffectSite?: Hir.EffectSiteId
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }

/** The unique call argument whose lexical storage may back this call's result. */
export const returnedBorrowArgument = (self: ExpressionFact): ArgumentFact | undefined => {
  if (self._tag !== 'Call') return undefined
  if (self.reference._tag === 'ResolvedBuiltin') {
    const ordinal = self.reference.returnedBorrowParameter
    return ordinal === undefined ? undefined : self.arguments.at(ordinal)
  }
  if (self.reference._tag !== 'Resolved') return undefined
  const declared = DeclarationFacts.returnedBorrow(self.reference.declaration)
  if (declared !== undefined) {
    return self.mappings.find(
      (mapping) => mapping.parameter.id.ordinal === declared.parameter.id.ordinal,
    )?.argument
  }
  if (self.type._tag !== 'Available' || !Type.containsViewBorrow(self.type.type)) return undefined
  const candidates = self.mappings.filter(
    (mapping) =>
      mapping.argument.type._tag === 'Available' &&
      Type.containsViewBorrow(mapping.argument.type.type),
  )
  return candidates.length === 1 ? candidates.at(0)?.argument : undefined
}

/** A deterministic argument identity within one caller and concrete call site. */
export interface ArgumentId {
  readonly _tag: 'ArgumentId'
  readonly function: DeclarationId
  readonly callSpan: SourceSpan.SourceSpan
  readonly ordinal: number
}

/** One ordered, syntax-owned call argument. */
export interface ArgumentFact {
  readonly _tag: 'Argument'
  readonly id: ArgumentId
  readonly expression: ExpressionFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
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
  readonly syntax: SyntaxTree.Node
  readonly declared: DeclaredTypeFact
  readonly type?: SemanticType
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
  | { readonly _tag: 'UnavailableCallSyntax'; readonly syntax: SyntaxTree.Node }
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
    }
  | {
      readonly _tag: 'ArityMismatch'
      readonly expectedCount: number
      readonly actualCount: number
    }
  | {
      readonly _tag: 'Unavailable'
      readonly reason: UnavailableCallContractReason
      readonly cause?: Diagnostic.Identity
    }

/** Whether one returned expression is known to match its declared result type. */
export type ReturnCompatibility = { readonly _tag: 'Compatible' } | { readonly _tag: 'Unavailable' }

export type AssignmentRootFact = BindingDeclarationFact | ParameterFact

/** Resolves the mutable root a writable-place expression is anchored to, if any. */
export const assignmentRoot = (fact: ExpressionFact): AssignmentRootFact | undefined => {
  if (fact._tag === 'Identifier') {
    if (fact.reference._tag === 'ResolvedBinding') return fact.reference.binding
    if (fact.reference._tag === 'Resolved') return fact.reference.parameter
    return undefined
  }
  if (fact._tag === 'FieldProjection' || fact._tag === 'IndexProjection') {
    return assignmentRoot(fact.subject)
  }
  if (fact._tag === 'Grouped') return assignmentRoot(fact.expression)
  return undefined
}

/** One public function declaration and its syntax-owned semantic facts. */
export type DeclarationFact = DeclarationFacts.DeclarationFact

/** One analyzed body statement in source order, nesting through conditionals. */
export type StatementFact =
  | {
      readonly _tag: 'UnsafeStatement'
      readonly statements: ReadonlyArray<StatementFact>
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'BindStatement'
      readonly binding: BindingDeclarationFact
      readonly region: Hir.RegionId
    }
  | {
      readonly _tag: 'PatternBindStatement'
      readonly selection: PatternSelectionFact
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'ExpressionStatement'
      readonly expression: ExpressionFact
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'IfStatement'
      readonly condition: ExpressionFact
      readonly taken: ReadonlyArray<StatementFact>
      readonly otherwise: ReadonlyArray<StatementFact>
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'IfLetStatement'
      readonly selection: PatternSelectionFact
      readonly taken: ReadonlyArray<StatementFact>
      readonly otherwise: ReadonlyArray<StatementFact>
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'WriteStatement'
      readonly destination: ExpressionFact
      readonly root?: AssignmentRootFact
      readonly value: ExpressionFact
      readonly compatible: boolean
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'WhileStatement'
      readonly loop: Hir.LoopId
      readonly parent?: Hir.LoopId
      readonly condition: ExpressionFact
      readonly body: ReadonlyArray<StatementFact>
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'BreakStatement'
      readonly target?: Hir.LoopId
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'ContinueStatement'
      readonly target?: Hir.LoopId
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'ReturnStatement'
      readonly expression: ExpressionFact
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'FailStatement'
      readonly expression: ExpressionFact
      readonly failure?: Type.Type
      readonly transfer: 'Copy' | 'Move'
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'DropStatement'
      readonly expression: ExpressionFact
      readonly region: Hir.RegionId
      readonly syntax: SyntaxTree.Node
    }

/** One function's declaration, statements, bindings, and compatibility facts. */
export interface FunctionFact {
  readonly _tag: 'FunctionFact'
  readonly declaration: DeclarationFact
  readonly statements: ReadonlyArray<StatementFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly regionOrder: ReadonlyArray<Hir.RegionId>
  readonly returnedExpression: ExpressionFact
  readonly returnCompatibility: ReturnCompatibility
  readonly returnedBorrow?: DeclarationFacts.ReturnedBorrowFact
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
  readonly span: SourceSpan.SourceSpan
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly patternBindings: ReadonlyArray<PatternBindingFact>
}

/** The closed result of looking up one declaration spelling. */
export type DeclarationLookup = DeclarationFacts.DeclarationLookup

/** The complete deterministic elaboration result for all direct bootstrap declarations. */
export interface Result {
  readonly _tag: 'Elaboration'
  readonly syntax: SyntaxFile.SyntaxFile
  readonly functions: ReadonlyArray<FunctionFact>
  readonly lexicalScopes: ReadonlyArray<LexicalScopeFact>
  readonly hir: Hir.Module
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
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
export const availableExpressionType = (type: SemanticType): ExpressionTypeFact =>
  type === 'i32'
    ? availableI32ExpressionType
    : type === 'usize'
      ? availableUsizeExpressionType
      : type === 'bool'
        ? availableBoolExpressionType
        : Object.freeze({ _tag: 'Available', type })
export const unavailableExpressionType: ExpressionTypeFact = Object.freeze({ _tag: 'Unavailable' })

export const typesCompatible = (source: SemanticType, target: SemanticType): boolean =>
  TypeCompatibility.isCompatible(TypeCompatibility.check(source, target))

export const declaredReturnTypesCompatible = (
  declaration: DeclarationFact,
  expression: ExpressionFact,
): boolean => {
  if (declaration.returnType._tag !== 'Resolved' || expression.type._tag !== 'Available')
    return false
  const source = expression.type.type
  const target = declaration.returnType.type
  if (typesCompatible(source, target)) return true
  const representation = representationOfExpression(expression)
  const contract = Type.isRepresented(source) ? source.contract : source
  if (
    representation !== undefined &&
    (Type.isCallable(contract) || Type.isEffect(contract)) &&
    typesCompatible(Type.represented(contract, contract, representation), target)
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
  expectedOrigin: SourceSpan.SourceSpan,
  actualOrigin: SourceSpan.SourceSpan,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic | undefined => {
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
  expression: ExpressionFact,
  target: SemanticType,
): boolean => {
  if (expression._tag !== 'Integer' || expression.integer._tag !== 'Available') return false
  if (typeof target !== 'string' || !Scalar.isIntegerSpelling(target)) return false
  const scalar = Scalar.find(target)
  if (scalar?.category !== 'Integer') return false
  const range = Scalar.range(scalar, 64)
  return expression.integer.value >= range.minimum && expression.integer.value <= range.maximum
}

export const unionConversionDiagnostic = (
  source: SemanticType,
  target: SemanticType,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic | undefined => {
  const compatibility = TypeCompatibility.check(source, target)
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

export const expressionNodeKinds: ReadonlyArray<SyntaxTree.NodeKind> = Object.freeze([
  'IntegerLiteralExpression',
  'FloatingLiteralExpression',
  'StaticTextLiteralExpression',
  'CharacterLiteralExpression',
  'UnitExpression',
  'BooleanLiteralExpression',
  'IdentifierExpression',
  'MoveExpression',
  'EffectExpression',
  'BorrowExpression',
  'MatchExpression',
  'StructLiteralExpression',
  'ArrayLiteralExpression',
  'FieldProjectionExpression',
  'IndexProjectionExpression',
  'CallExpression',
  'GroupedExpression',
  'PrefixExpression',
  'InfixExpression',
  'PipelineExpression',
  'RunExpression',
  'UnsafeExpression',
])

export const isExpressionNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  SyntaxTree.isNode(element) && expressionNodeKinds.includes(element.kind)

export const isRecursiveArgumentNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  isExpressionNode(element) &&
  (element.kind === 'CallExpression' ||
    element.kind === 'MoveExpression' ||
    element.kind === 'EffectExpression' ||
    element.kind === 'BorrowExpression' ||
    element.kind === 'MatchExpression' ||
    element.kind === 'StructLiteralExpression' ||
    element.kind === 'ArrayLiteralExpression' ||
    element.kind === 'FieldProjectionExpression' ||
    element.kind === 'IndexProjectionExpression' ||
    element.kind === 'GroupedExpression' ||
    element.kind === 'PrefixExpression' ||
    element.kind === 'InfixExpression' ||
    element.kind === 'PipelineExpression' ||
    element.kind === 'RunExpression' ||
    element.kind === 'UnsafeExpression' ||
    SyntaxTree.isAvailableSyntax(element))

export const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = SyntaxTree.directNode(parent, kind)
  if (child === undefined) {
    throw new RangeError(`Semantic analysis expected ${kind} below ${parent.kind}`)
  }
  return child
}

export const directToken = SyntaxTree.directToken

export const callCallee = (node: SyntaxTree.Node): SyntaxTree.Node =>
  node.kind === 'CallExpression' ? (node.children.find(isExpressionNode) ?? node) : node

export const callReferenceTokens = (node: SyntaxTree.Node): ReadonlyArray<Token.Token> => {
  const callee = callCallee(node)
  if (callee.kind === 'GroupedExpression') {
    const expression = callee.children.find(isExpressionNode)
    return expression === undefined ? Object.freeze([]) : callReferenceTokens(expression)
  }
  if (callee.kind === 'IdentifierExpression') {
    const identifier = directToken(callee, 'Identifier')
    return identifier === undefined ? Object.freeze([]) : Object.freeze([identifier])
  }
  if (callee.kind !== 'FieldProjectionExpression') return Object.freeze([])
  const subject = callee.children.find(isExpressionNode)
  const member = directToken(callee, 'Identifier') ?? directToken(callee, 'DropKeyword')
  const qualifier = subject === undefined ? undefined : callReferenceTokens(subject).at(-1)
  return qualifier === undefined || member === undefined
    ? Object.freeze([])
    : Object.freeze([qualifier, member])
}

export const referencePath = (node: SyntaxTree.Node): ReferencePathFact => {
  const identifiers = callReferenceTokens(node)
  const member = identifiers.at(-1)
  const qualifier = identifiers.length > 1 ? identifiers.at(0) : undefined
  return member === undefined
    ? Object.freeze({ _tag: 'UnavailableReferencePath', syntax: node })
    : Object.freeze({
        _tag: 'ReferencePath',
        ...(qualifier === undefined ? {} : { qualifier }),
        member,
      })
}

export const pipelineInput = (node: SyntaxTree.Node): SyntaxTree.Node | undefined =>
  node.children.filter(isExpressionNode).at(0)

export const pipelineCallable = (node: SyntaxTree.Node): SyntaxTree.Node | undefined =>
  node.children.filter(isExpressionNode).at(1)

export const unavailableSyntax = SyntaxTree.unavailableChild

export const isAvailableSyntax = SyntaxTree.isAvailableSyntax

export const unavailableElement = SyntaxTree.unavailableElement

export const lookupParameter = DeclarationFacts.lookupParameter

export const lookupDeclaration = DeclarationFacts.lookupDeclaration

export const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Semantic token span does not belong to source ${source.id}`),
  )

export interface IntegerResult {
  readonly fact: IntegerExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export interface ExpressionResult {
  readonly fact: ExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
}

export interface IdentifierResult {
  readonly fact: IdentifierExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
  readonly syntax: SyntaxTree.Node
}

export interface ArgumentsResult {
  readonly facts: ReadonlyArray<ArgumentFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

export const argumentFact = (
  declaration: DeclarationFact,
  callSpan: SourceSpan.SourceSpan,
  expression: ExpressionFact,
  ordinal: number,
): ArgumentFact =>
  Object.freeze({
    _tag: 'Argument',
    id: Object.freeze({
      _tag: 'ArgumentId',
      function: declaration.id,
      callSpan,
      ordinal,
    }),
    expression,
    type: expression.type,
    syntax: expression.syntax,
  })

import { copyAssumptionsOf } from './CallResolution.js'
import {
  analyzeConstant,
  compareDiagnostics,
  effectCaptureFacts,
  representationOfExpression,
} from './ExpressionAnalysis.js'
import {
  directExpressionChildren,
  directStatementExpressions,
  lowerStatements,
  statementSpan,
} from './HirLowering.js'
import { analyzeFunctionBody } from './StatementAnalysis.js'
export interface FactVisitor {
  readonly statement?: (statement: StatementFact) => void
  readonly expression?: (expression: ExpressionFact) => void
  readonly descendExpressions?: boolean
}

const visitExpressionFact = (expression: ExpressionFact, visitor: FactVisitor): void => {
  visitor.expression?.(expression)
  if (expression._tag === 'Match') {
    visitExpressionFact(expression.scrutinee, visitor)
    for (const arm of expression.arms) {
      if (arm.guard !== undefined) visitExpressionFact(arm.guard, visitor)
      visitExpressionFact(arm.result, visitor)
    }
    return
  }
  if (expression._tag === 'EffectBlock') {
    visitStatementFacts(expression.statements, visitor)
    return
  }
  for (const child of directExpressionChildren(expression)) visitExpressionFact(child, visitor)
}

/** Visits one expression tree in deterministic source order. */
export const visitExpressionFacts = (self: ExpressionFact, visitor: FactVisitor): void =>
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

const constrainedCallableSchema = (expression: ExpressionFact): Type.CallableSchema | undefined => {
  if (expression.type._tag !== 'Available' || !Type.isCallable(expression.type.type))
    return undefined
  const schema = expression.type.type.schema
  return schema !== undefined &&
    (schema.binders.length > 0 || schema.constraints.length > 0 || schema.evidence.length > 0)
    ? schema
    : undefined
}

const canonicalFunctionKey = (declaration: DeclarationFact): string | undefined =>
  declaration.canonical._tag === 'Canonical'
    ? `${declaration.canonical.id.module}\u0000${declaration.canonical.id.name}`
    : undefined

/**
 * Proves that one ordinary source function is a compile-time-only whole-value relay. Only lexical
 * binds followed by one return qualify: admitting any other statement would erase observable work
 * when lowering replaces the chain by its originating callable recipe.
 */
const forwardedCallableParameter = (
  fn: FunctionFact,
  functions: ReadonlyMap<string, FunctionFact>,
  resolvingFunctions: ReadonlySet<string> = new Set(),
): number | undefined => {
  const key_ = canonicalFunctionKey(fn.declaration)
  if (key_ === undefined || resolvingFunctions.has(key_)) return undefined
  const leading = fn.statements.slice(0, -1)
  const terminal = fn.statements.at(-1)
  if (
    fn.declaration.parameters.length !== 1 ||
    terminal?._tag !== 'ReturnStatement' ||
    leading.some((statement) => statement._tag !== 'BindStatement')
  )
    return undefined
  const resolving = new Set(resolvingFunctions).add(key_)
  const forwardedBindings = new Set<number>()
  const expression = (
    current: ExpressionFact,
    bindings: ReadonlySet<number> = new Set(),
  ): number | undefined => {
    if (current._tag === 'Grouped' || current._tag === 'Move')
      return expression(current._tag === 'Grouped' ? current.expression : current.subject, bindings)
    if (current._tag === 'Identifier') {
      if (current.reference._tag === 'Resolved') return current.reference.parameter.id.ordinal
      if (current.reference._tag !== 'ResolvedBinding') return undefined
      const ordinal = current.reference.binding.id.ordinal
      if (bindings.has(ordinal)) return undefined
      forwardedBindings.add(ordinal)
      return expression(current.reference.binding.initializer, new Set(bindings).add(ordinal))
    }
    if (current._tag !== 'Call' || current.reference._tag !== 'Resolved') return undefined
    const targetKey = canonicalFunctionKey(current.reference.declaration)
    const target = targetKey === undefined ? undefined : functions.get(targetKey)
    const forwarded =
      target === undefined ? undefined : forwardedCallableParameter(target, functions, resolving)
    const argument =
      forwarded === undefined
        ? undefined
        : current.mappings.find((mapping) => mapping.parameter.id.ordinal === forwarded)?.argument
    return argument === undefined ? undefined : expression(argument.expression, bindings)
  }
  const forwarded = expression(terminal.expression)
  return forwarded === undefined ||
    leading.some(
      (statement) =>
        statement._tag !== 'BindStatement' || !forwardedBindings.has(statement.binding.id.ordinal),
    )
    ? undefined
    : forwarded
}

const constrainedCallableEscapeDiagnostics = (
  functions: ReadonlyArray<FunctionFact>,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  const byCanonical = new Map(
    functions.flatMap((fn) => {
      const key_ = canonicalFunctionKey(fn.declaration)
      return key_ === undefined ? [] : [[key_, fn] as const]
    }),
  )
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const seen = new Set<string>()
  const reject = (expression: ExpressionFact): void => {
    const span = expression.syntax.span
    const key_ = `${span.sourceId}:${span.start}:${span.end}`
    if (seen.has(key_)) return
    seen.add(key_)
    diagnostics.push(Diagnostic.nonConcreteSpecialization('constrained callable', span))
  }
  for (const fn of functions) {
    visitStatementFacts(fn.statements, {
      statement: (statement) => {
        if (
          (statement._tag === 'ReturnStatement' || statement._tag === 'WriteStatement') &&
          constrainedCallableSchema(
            statement._tag === 'ReturnStatement' ? statement.expression : statement.value,
          ) !== undefined
        )
          reject(statement._tag === 'ReturnStatement' ? statement.expression : statement.value)
      },
      expression: (expression) => {
        if (expression._tag === 'StructLiteral') {
          for (const initializer of expression.initializers)
            if (constrainedCallableSchema(initializer.expression) !== undefined)
              reject(initializer.expression)
          return
        }
        if (expression._tag === 'ArrayLiteral') {
          for (const element of expression.elements)
            if (constrainedCallableSchema(element.expression) !== undefined)
              reject(element.expression)
          return
        }
        if (expression._tag === 'CallableSection') {
          for (const capture of expression.captures)
            if (constrainedCallableSchema(capture.expression) !== undefined)
              reject(capture.expression)
          return
        }
        if (expression._tag === 'EffectBlock') {
          for (const capture of expression.captures) {
            const captured =
              capture.reference._tag === 'BindingFact' ? capture.reference.initializer : undefined
            if (captured !== undefined && constrainedCallableSchema(captured) !== undefined)
              reject(captured)
          }
          return
        }
        if (expression._tag === 'Match' && constrainedCallableSchema(expression) !== undefined) {
          reject(expression)
          return
        }
        if (expression._tag === 'CallableApply') {
          for (const argument of expression.arguments)
            if (constrainedCallableSchema(argument.expression) !== undefined)
              reject(argument.expression)
          return
        }
        if (expression._tag !== 'Call') return
        const targetKey =
          expression.reference._tag === 'Resolved'
            ? canonicalFunctionKey(expression.reference.declaration)
            : undefined
        const target = targetKey === undefined ? undefined : byCanonical.get(targetKey)
        const forwarded =
          target === undefined ? undefined : forwardedCallableParameter(target, byCanonical)
        let relayed = false
        for (const mapping of expression.mappings) {
          if (constrainedCallableSchema(mapping.argument.expression) === undefined) continue
          if (mapping.parameter.id.ordinal === forwarded) relayed = true
          else reject(mapping.argument.expression)
        }
        if (constrainedCallableSchema(expression) !== undefined && !relayed) reject(expression)
      },
    })
  }
  return Object.freeze(diagnostics)
}

const lexicalScopesOf = (
  source: SourceFile.SourceFile,
  functions: ReadonlyArray<FunctionFact>,
): ReadonlyArray<LexicalScopeFact> => {
  const scopes: Array<LexicalScopeFact> = []
  for (const fn of functions) {
    let ordinal = 0
    const add = (options: {
      readonly parent?: LexicalScopeId
      readonly span: SourceSpan.SourceSpan
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
          span: options.span,
          parameters: Object.freeze(Array.from(options.parameters ?? [])),
          bindings: Object.freeze(Array.from(options.bindings ?? [])),
          patternBindings: Object.freeze(Array.from(options.patternBindings ?? [])),
        }),
      )
      return id
    }
    const spanOf = (
      statements: ReadonlyArray<StatementFact>,
      fallback: SourceSpan.SourceSpan,
    ): SourceSpan.SourceSpan => {
      const first = statements.at(0)
      const last = statements.at(-1)
      return first === undefined || last === undefined
        ? fallback
        : Option.getOrElse(
            SourceSpan.make(source, statementSpan(first).start, statementSpan(last).end),
            () => fallback,
          )
    }
    let visitStatements: (
      statements: ReadonlyArray<StatementFact>,
      parent: LexicalScopeId | undefined,
      fallback: SourceSpan.SourceSpan,
    ) => LexicalScopeId
    const visitExpression = (expression: ExpressionFact, parent: LexicalScopeId): void => {
      if (expression._tag === 'Match') {
        visitExpression(expression.scrutinee, parent)
        for (const arm of expression.arms) {
          const armScope = add({
            parent,
            span: arm.syntax.span,
            patternBindings: arm.bindings,
          })
          if (arm.guard !== undefined) visitExpression(arm.guard, armScope)
          visitExpression(arm.result, armScope)
        }
        return
      }
      if (expression._tag === 'EffectBlock') {
        visitStatements(expression.statements, parent, expression.syntax.span)
        return
      }
      for (const child of directExpressionChildren(expression)) visitExpression(child, parent)
    }
    visitStatements = (
      statements: ReadonlyArray<StatementFact>,
      parent: LexicalScopeId | undefined,
      fallback: SourceSpan.SourceSpan,
    ): LexicalScopeId => {
      const current = add({
        ...(parent === undefined ? {} : { parent }),
        span: spanOf(statements, fallback),
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
          visitStatements(statement.statements, current, statement.syntax.span)
        else if (statement._tag === 'IfStatement') {
          visitStatements(statement.taken, current, statement.syntax.span)
          visitStatements(statement.otherwise, current, statement.syntax.span)
        } else if (statement._tag === 'IfLetStatement') {
          const takenScope = add({
            parent: current,
            span: statement.syntax.span,
            patternBindings: statement.selection.bindings,
          })
          visitStatements(statement.taken, takenScope, statement.syntax.span)
          visitStatements(statement.otherwise, current, statement.syntax.span)
        } else if (statement._tag === 'WhileStatement')
          visitStatements(statement.body, current, statement.syntax.span)
      }
      return current
    }
    visitStatements(fn.statements, undefined, fn.declaration.syntax.span)
  }
  return Object.freeze(scopes)
}

/** Elaborates every declaration body into immutable facts and the module's HIR. */
export interface Input {
  readonly syntax: SyntaxFile.SyntaxFile
  readonly headers: DeclarationFacts.ModuleHeaders
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
}

export const elaborateModule = (input: Input): Result => {
  const { syntax, headers, scope, index } = input
  const source = syntax.source
  const declarations = headers.declarations
  const analyzed = declarations.map((declaration) =>
    analyzeFunctionBody(source, declaration, declarations, Object.freeze({ scope, index })),
  )
  const constantDiagnostics = headers.constants.flatMap((constant) =>
    constant.name._tag === 'Present'
      ? analyzeConstant(constant, constant.name.token, constant.initializer, true).diagnostics
      : [],
  )
  const functions = Object.freeze(analyzed.map((result) => result.fact))
  const diagnostics = [
    ...headers.diagnostics,
    ...constantDiagnostics,
    ...analyzed.flatMap((result) => result.diagnostics),
    ...constrainedCallableEscapeDiagnostics(functions),
  ].sort(compareDiagnostics)
  const hir: Hir.Module = Object.freeze({
    _tag: 'HirModule',
    module: source.id,
    functions: Object.freeze(
      functions.map((fact) =>
        (() => {
          const originalEntryRegion =
            fact.regionOrder.at(0) ??
            Object.freeze({
              _tag: 'HirRegion' as const,
              function: fact.declaration.id,
              ordinal: 0,
            })
          const baseContract = Hir.contractOf(fact.declaration)
          if (
            fact.declaration.functionKind === 'Effect' &&
            fact.declaration.returnType._tag === 'Resolved' &&
            baseContract._tag === 'Contract'
          ) {
            const captures = effectCaptureFacts(
              fact.statements,
              0,
              index,
              copyAssumptionsOf(fact.declaration),
            ).map((capture) => {
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
            const semanticCaptureAccess = (
              capture: EffectCaptureFact,
            ): Type.Effect['access'] | 'Copy' => {
              if (capture.reference._tag !== 'ParameterDeclaration') return capture.access
              const declared = capture.reference.declaredType
              if (declared._tag !== 'Resolved') return capture.access
              if (Type.isEffect(declared.type)) return declared.type.access
              if (Type.isCallable(declared.type)) return declared.type.mode
              if (Type.isReference(declared.type)) return declared.type.access
              return capture.access
            }
            const semanticAccesses = captures.map(semanticCaptureAccess)
            const access = semanticAccesses.some((capture) => capture === 'Take')
              ? 'Take'
              : semanticAccesses.some((capture) => capture === 'Exclusive')
                ? 'Exclusive'
                : 'Shared'
            const type = Type.effectWithRows(
              fact.declaration.returnType.type,
              fact.declaration.failureRow.row,
              access,
              fact.declaration.requirementRow.row,
            )
            const body = SyntaxTree.directNode(fact.declaration.syntax, 'Block')
            const siteSpan = body?.span ?? fact.declaration.syntax.span
            const entryRegion: Hir.RegionId = Object.freeze({
              _tag: 'HirRegion',
              function: fact.declaration.id,
              ordinal: Math.max(-1, ...fact.regionOrder.map((region) => region.ordinal)) + 1,
            })
            const effectBlock: Extract<Hir.Expression, { readonly _tag: 'EffectBlock' }> =
              Object.freeze({
                _tag: 'EffectBlock',
                site: Object.freeze({
                  _tag: 'EffectSiteId',
                  function: fact.declaration.id,
                  ...(fact.declaration.canonical._tag === 'Canonical'
                    ? { owner: fact.declaration.canonical.id }
                    : {}),
                  ordinal: -1,
                  span: siteSpan,
                }),
                statements: lowerStatements(fact.statements, {
                  resultType: fact.declaration.returnType.type,
                  functionId: fact.declaration.id,
                  eraseIntrinsicSections: true,
                  borrowBindingInitializers: true,
                }),
                captures: Object.freeze(
                  captures.map((capture) =>
                    Object.freeze({
                      ...(capture.reference._tag === 'BindingFact'
                        ? { binding: capture.reference.id }
                        : { parameter: capture.reference.id }),
                      access: capture.access,
                      span: capture.span,
                    }),
                  ),
                ),
                type,
                span: siteSpan,
              })
            return Object.freeze({
              _tag: 'HirFunction' as const,
              declaration: fact.declaration,
              contract: Object.freeze({
                _tag: 'Contract' as const,
                unsafe: baseContract.unsafe,
                parameters: baseContract.parameters,
                result: type,
                constraints: baseContract.constraints,
              }),
              entryRegion,
              regionOrder: Object.freeze([entryRegion, ...fact.regionOrder]),
              statements: Object.freeze([
                Object.freeze({
                  _tag: 'Return' as const,
                  expression: effectBlock,
                  region: entryRegion,
                  span: siteSpan,
                }),
              ]),
            })
          }
          return Object.freeze({
            _tag: 'HirFunction' as const,
            declaration: fact.declaration,
            contract: baseContract,
            entryRegion: originalEntryRegion,
            regionOrder: fact.regionOrder,
            statements: lowerStatements(fact.statements, {
              ...(fact.declaration.returnType._tag === 'Resolved'
                ? { resultType: fact.declaration.returnType.type }
                : {}),
              functionId: fact.declaration.id,
              eraseIntrinsicSections: true,
              borrowBindingInitializers: true,
            }),
          })
        })(),
      ),
    ),
  })

  return Object.freeze({
    _tag: 'Elaboration',
    syntax,
    functions,
    lexicalScopes: lexicalScopesOf(syntax.source, functions),
    hir,
    diagnostics: Object.freeze(diagnostics),
  })
}

/** Looks up every present declaration with the exact requested spelling. */
export const declarationByName = dual<
  (spelling: string) => (self: Result) => DeclarationLookup,
  (self: Result, spelling: string) => DeclarationLookup
>(2, (self, spelling) =>
  lookupDeclaration(
    self.functions.map((fact) => fact.declaration),
    spelling,
  ),
)

/** Looks up every present parameter with the exact requested spelling in one function. */
export const parameterByName = dual<
  (spelling: string) => (self: DeclarationFact) => ParameterLookup,
  (self: DeclarationFact, spelling: string) => ParameterLookup
>(2, (self, spelling) => lookupParameter(self.parameters, spelling))
