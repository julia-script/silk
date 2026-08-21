import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
import * as CallableContract from './CallableContract.js'
import * as ConformanceGoal from './ConformanceGoal.js'
import * as Constraint from './Constraint.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import * as DigitSeparator from './internal/DigitSeparator.js'
import * as IntegerLiteral from './internal/IntegerLiteral.js'
import * as LiteralForm from './LiteralForm.js'
import * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import * as Operator from './Operator.js'
import * as Presentation from './Presentation.js'
import * as ProviderSelection from './ProviderSelection.js'
import * as RequirementRow from './RequirementRow.js'
import * as RowAlgebra from './RowAlgebra.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as StaticText from './StaticText.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as TargetConstant from './TargetConstant.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/** The only semantic type recognized by the first analysis slice. */
export type SemanticType = DeclarationIndex.SemanticType

/** A deterministic declaration identity local to one analyzed source snapshot. */
export type DeclarationId = DeclarationIndex.DeclarationId

/** A deterministic parameter identity nested under its owning function declaration. */
export type ParameterId = DeclarationIndex.ParameterId

/** A declaration name supplied by syntax or explicitly unavailable after recovery. */
export type DeclaredName = DeclarationIndex.DeclaredName

/** The resolved, unresolved, or syntax-unavailable declared return type. */
export type DeclaredTypeFact = DeclarationIndex.DeclaredTypeFact

/** The declared type fact attached to a function return. */
export type ReturnTypeFact = DeclarationIndex.ReturnTypeFact

/** One ordered parameter declaration with exact concrete provenance. */
export type ParameterFact = DeclarationIndex.ParameterFact

/** The closed result of looking up a parameter spelling within one function. */
export type ParameterLookup = DeclarationIndex.ParameterLookup

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
      readonly service: DeclarationIndex.ServiceFact
      readonly operation: DeclarationIndex.ServiceOperationFact
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
      readonly declaration: DeclarationIndex.ServiceOperationFact
      readonly interfaceContract: DeclarationIndex.InterfaceOperationApplicationFact
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
      readonly actor: DeclarationIndex.StructFact | Intrinsic.Actor
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
      readonly field: DeclarationIndex.FieldId
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
  readonly field?: DeclarationIndex.FieldFact
  readonly path: ReadonlyArray<DeclarationIndex.FieldId>
  readonly type: ExpressionTypeFact
  readonly access: Match.Access
  readonly syntax: SyntaxTree.Node
}

export type PatternFieldState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationIndex.FieldFact }
  | { readonly _tag: 'Unknown'; readonly cause: Diagnostic.Identity }
  | {
      readonly _tag: 'Duplicate'
      readonly field: DeclarationIndex.FieldFact
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
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>>
      readonly complete: false
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'TypePattern'
      readonly id: Match.PatternId
      readonly member?: Type.Type
      readonly declared: DeclarationIndex.DeclaredTypeFact
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>>
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
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>>
      readonly rest: boolean
      readonly complete: boolean
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'UniversalPattern'
      readonly id: Match.PatternId
      readonly bindings: ReadonlyArray<PatternBindingFact>
      readonly omitted: ReadonlyArray<ReadonlyArray<DeclarationIndex.FieldId>>
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
      readonly struct: DeclarationIndex.StructFact
      readonly type: Type.Nominal
      readonly token: Token.Token
    }
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

export type StructInitializerState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationIndex.FieldFact }
  | { readonly _tag: 'Unknown'; readonly cause: Diagnostic.Identity }
  | {
      readonly _tag: 'Duplicate'
      readonly field: DeclarationIndex.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | {
      readonly _tag: 'TypeMismatch'
      readonly field: DeclarationIndex.FieldFact
      readonly cause: Diagnostic.Identity
    }
  | {
      readonly _tag: 'Inaccessible'
      readonly field: DeclarationIndex.FieldFact
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
    readonly field: DeclarationIndex.FieldFact
    readonly initializer: StructInitializerFact
  }>
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

export type ProjectionState =
  | { readonly _tag: 'Resolved'; readonly field: DeclarationIndex.FieldFact }
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
  readonly declaration: DeclarationIndex.ConstantFact
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
  readonly contract: DeclarationIndex.InterfaceOperationApplicationFact
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
  readonly witness?: DeclarationIndex.ConformanceWitness
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
  const declared = DeclarationIndex.returnedBorrow(self.reference.declaration)
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
const assignmentRoot = (fact: ExpressionFact): AssignmentRootFact | undefined => {
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
export type DeclarationFact = DeclarationIndex.DeclarationFact

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
  readonly returnedBorrow?: DeclarationIndex.ReturnedBorrowFact
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
export type DeclarationLookup = DeclarationIndex.DeclarationLookup

/** The complete deterministic elaboration result for all direct bootstrap declarations. */
export interface Result {
  readonly _tag: 'Elaboration'
  readonly syntax: SyntaxFile.SyntaxFile
  readonly functions: ReadonlyArray<FunctionFact>
  readonly lexicalScopes: ReadonlyArray<LexicalScopeFact>
  readonly hir: Hir.Module
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const compatible: ReturnCompatibility = Object.freeze({ _tag: 'Compatible' })
const unavailableCompatibility: ReturnCompatibility = Object.freeze({ _tag: 'Unavailable' })
const availableI32ExpressionType: ExpressionTypeFact = Object.freeze({
  _tag: 'Available',
  type: 'i32',
})
const availableUsizeExpressionType: ExpressionTypeFact = Object.freeze({
  _tag: 'Available',
  type: 'usize',
})
const availableBoolExpressionType: ExpressionTypeFact = Object.freeze({
  _tag: 'Available',
  type: 'bool',
})
const availableExpressionType = (type: SemanticType): ExpressionTypeFact =>
  type === 'i32'
    ? availableI32ExpressionType
    : type === 'usize'
      ? availableUsizeExpressionType
      : type === 'bool'
        ? availableBoolExpressionType
        : Object.freeze({ _tag: 'Available', type })
const unavailableExpressionType: ExpressionTypeFact = Object.freeze({ _tag: 'Unavailable' })

const typesCompatible = (source: SemanticType, target: SemanticType): boolean =>
  TypeCompatibility.isCompatible(TypeCompatibility.check(source, target))

const declaredReturnTypesCompatible = (
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

const representationJoinDiagnostic = (
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

const contextualIntegerCompatible = (expression: ExpressionFact, target: SemanticType): boolean => {
  if (expression._tag !== 'Integer' || expression.integer._tag !== 'Available') return false
  if (typeof target !== 'string' || !Scalar.isIntegerSpelling(target)) return false
  const scalar = Scalar.find(target)
  if (scalar?.category !== 'Integer') return false
  const range = Scalar.range(scalar, 64)
  return expression.integer.value >= range.minimum && expression.integer.value <= range.maximum
}

const unionConversionDiagnostic = (
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

const expressionNodeKinds: ReadonlyArray<SyntaxTree.NodeKind> = Object.freeze([
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

const isExpressionNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
  SyntaxTree.isNode(element) && expressionNodeKinds.includes(element.kind)

const isRecursiveArgumentNode = (element: SyntaxTree.Element): element is SyntaxTree.Node =>
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

const childNode = (parent: SyntaxTree.Node, kind: SyntaxTree.NodeKind): SyntaxTree.Node => {
  const child = SyntaxTree.directNode(parent, kind)
  if (child === undefined) {
    throw new RangeError(`Semantic analysis expected ${kind} below ${parent.kind}`)
  }
  return child
}

const directToken = SyntaxTree.directToken

const callCallee = (node: SyntaxTree.Node): SyntaxTree.Node =>
  node.kind === 'CallExpression' ? (node.children.find(isExpressionNode) ?? node) : node

const callReferenceTokens = (node: SyntaxTree.Node): ReadonlyArray<Token.Token> => {
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

const referencePath = (node: SyntaxTree.Node): ReferencePathFact => {
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

const pipelineInput = (node: SyntaxTree.Node): SyntaxTree.Node | undefined =>
  node.children.filter(isExpressionNode).at(0)

const pipelineCallable = (node: SyntaxTree.Node): SyntaxTree.Node | undefined =>
  node.children.filter(isExpressionNode).at(1)

const unavailableSyntax = SyntaxTree.unavailableChild

const isAvailableSyntax = SyntaxTree.isAvailableSyntax

const unavailableElement = SyntaxTree.unavailableElement

const lookupParameter = DeclarationIndex.lookupParameter

const lookupDeclaration = DeclarationIndex.lookupDeclaration

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  Option.getOrThrowWith(
    SourceFile.spelling(source, token.span),
    () => new RangeError(`Semantic token span does not belong to source ${source.id}`),
  )

interface IntegerResult {
  readonly fact: IntegerExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface ExpressionResult {
  readonly fact: ExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
}

interface IdentifierResult {
  readonly fact: IdentifierExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
  readonly syntax: SyntaxTree.Node
}

interface ArgumentsResult {
  readonly facts: ReadonlyArray<ArgumentFact>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const argumentFact = (
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

const analyzeInteger = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  expected?: SemanticType,
): IntegerResult => {
  const token = directToken(node, 'DecimalInteger')
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unavailable',
        syntax: unavailableSyntax(node, 'DecimalInteger'),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const minusToken = directToken(node, 'Minus')
  const negative = minusToken !== undefined
  const literalSpan =
    minusToken === undefined
      ? token.span
      : Option.getOrElse(
          SourceSpan.make(source, minusToken.span.start, token.span.end),
          () => token.span,
        )
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic integer span does not belong to source ${source.id}`),
  )
  const selected =
    typeof expected === 'string' && Scalar.isIntegerSpelling(expected)
      ? Scalar.find(expected)
      : Scalar.defaultInteger
  if (selected === undefined || selected.category !== 'Integer')
    throw new RangeError('The scalar catalog lost its default integer')
  const magnitude = IntegerLiteral.magnitude(bytes)
  const value = negative ? -magnitude : magnitude
  // Target-width integers are retained against the widest admitted target here. The concrete
  // target validates its selected 32- or 64-bit range before MIR is committed.
  const range = Scalar.range(selected, 64)
  if (value >= range.minimum && value <= range.maximum) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Available',
        type: selected.spelling,
        value,
        token,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const digits = Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
  const tokenSpelling = negative ? `-${digits}` : digits
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'OutOfRange',
      type: selected.spelling,
      spelling: tokenSpelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      selected.spelling === 'usize' && negative
        ? Diagnostic.usizeNegative(tokenSpelling, literalSpan)
        : Diagnostic.integerOutOfRange(tokenSpelling, literalSpan),
    ]),
  })
}

const analyzeFloating = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  expected?: SemanticType,
): {
  readonly fact: FloatingExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
} => {
  const unavailable = (
    diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
  ): { readonly fact: FloatingExpressionFact; readonly diagnostics: typeof diagnostics } =>
    Object.freeze({ fact: Object.freeze({ _tag: 'Unavailable', syntax: node }), diagnostics })
  const token = directToken(node, 'DecimalFloat')
  if (token === undefined) return unavailable(Object.freeze([]))
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic float span does not belong to source ${source.id}`),
  )
  const unsigned = DigitSeparator.strip(bytes)
  const spelling = directToken(node, 'Minus') === undefined ? unsigned : `-${unsigned}`
  const selected = Scalar.isFloatSpelling(expected) ? expected : Scalar.defaultFloat.spelling
  const encoded = FloatingPoint.fromDecimal(spelling, selected === 'f32' ? 32 : 64)
  // A spelling the lexer accepted but no float can represent must never pass silently.
  if (encoded === undefined) {
    return unavailable(Object.freeze([Diagnostic.invalidFloatLiteral(spelling, node.span)]))
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Available',
      type: selected,
      bits: encoded.bits,
      spelling,
      token,
      syntax: node,
    }),
    diagnostics: Object.freeze([]),
  })
}

const analyzeConstant = (
  declaration: DeclarationIndex.ConstantFact,
  token: Token.Token,
  syntax: SyntaxTree.Node,
  reportDiagnostic: boolean,
): ExpressionResult => {
  const declared = declaration.declaredType
  const literal = declaration.literal
  let value: ConstantExpressionFact['value']
  let type: SemanticType | undefined
  let detail: string | undefined

  if (declared._tag !== 'Resolved' || typeof declared.type !== 'string') {
    detail = 'the declared type must be one primitive scalar or string'
  } else if (literal._tag === 'Malformed') {
    detail = literal.detail
  } else if (literal._tag === 'Unavailable') {
    detail = 'the initializer must be one literal'
  } else if (Type.isString(declared.type) && literal._tag === 'StringLiteral') {
    if (literal.data.kind !== 'Text') detail = 'a byte-string literal does not produce a string'
    else {
      type = Type.string
      value = Object.freeze({ _tag: 'String', data: literal.data })
    }
  } else if (declared.type === 'bool' && literal._tag === 'BooleanLiteral') {
    type = 'bool'
    value = Object.freeze({ _tag: 'Boolean', value: literal.value })
  } else if (declared.type === 'char' && literal._tag === 'CharacterLiteral') {
    type = 'char'
    value = Object.freeze({ _tag: 'Character', value: literal.value })
  } else if (Scalar.isIntegerSpelling(declared.type) && literal._tag === 'IntegerLiteral') {
    const scalar = Scalar.find(declared.type)
    if (scalar === undefined || scalar.category !== 'Integer') {
      detail = `unknown integer type ${declared.type}`
    } else {
      const range = Scalar.range(scalar, 64)
      if (literal.value < range.minimum || literal.value > range.maximum) {
        detail = `${literal.spelling} does not fit ${declared.type}`
      } else {
        type = declared.type
        value = Object.freeze({ _tag: 'Integer', value: literal.value, type })
      }
    }
  } else if (literal._tag === 'TargetConstant') {
    // The pointer width is not known here — elaboration precedes target selection — so the fact is
    // recorded with its widest value and its selector. `Lower` narrows it once the target is fixed.
    const expected = TargetConstant.declaredType(literal.selector)
    if (declared.type !== expected) {
      detail = `${TargetConstant.root}.${literal.selector} is ${expected}, not ${Type.encode(declared.type)}`
    } else {
      type = expected
      value = Object.freeze({
        _tag: 'Integer',
        value: TargetConstant.unselected(literal.selector),
        type,
        target: literal.selector,
      })
    }
  } else if (Scalar.isFloatSpelling(declared.type) && literal._tag === 'FloatingLiteral') {
    const selected = declared.type
    const encoded = FloatingPoint.fromDecimal(literal.spelling, selected === 'f32' ? 32 : 64)
    if (encoded === undefined) detail = `${literal.spelling} is not a valid ${selected} literal`
    else {
      type = selected
      value = Object.freeze({
        _tag: 'Floating',
        bits: encoded.bits,
        spelling: literal.spelling,
        type: selected,
      })
    }
  } else {
    detail = `the literal kind does not match ${declared._tag === 'Resolved' ? Type.encode(declared.type) : 'the declared type'}`
  }

  const diagnostic =
    detail === undefined
      ? undefined
      : Diagnostic.invalidConstant(detail, declaration.initializer.span)
  const expressionType =
    type === undefined ? unavailableExpressionType : availableExpressionType(type)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Constant',
      declaration,
      token,
      ...(value === undefined ? {} : { value }),
      type: expressionType,
      syntax,
    }),
    diagnostics: Object.freeze(reportDiagnostic && diagnostic !== undefined ? [diagnostic] : []),
    type,
  })
}

/** The value names visible at one body position: parameters plus completed bindings. */
interface Scope {
  readonly parameters: ReadonlyArray<ParameterFact>
  readonly bindings: ReadonlyArray<BindingDeclarationFact>
  readonly patternBindings: ReadonlyArray<PatternBindingFact>
}

interface ValueResolution {
  readonly reference: ParameterReferenceFact
  readonly type: ExpressionTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const resolveValueName = (
  scope: Scope,
  tokenSpelling: string,
  token: Token.Token,
): ValueResolution => {
  const binding = scope.bindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === tokenSpelling,
  )
  if (binding !== undefined) {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'ResolvedBinding' as const,
        spelling: tokenSpelling,
        token,
        binding,
      }),
      type: binding.inferredType,
      diagnostics: Object.freeze([]),
    })
  }
  const patternBinding = scope.patternBindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === tokenSpelling,
  )
  if (patternBinding !== undefined) {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'ResolvedPattern' as const,
        spelling: tokenSpelling,
        token,
        binding: patternBinding,
      }),
      type: patternBinding.type,
      diagnostics: Object.freeze([]),
    })
  }
  const lookup = lookupParameter(scope.parameters, tokenSpelling)
  if (lookup._tag === 'Resolved') {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'Resolved' as const,
        spelling: tokenSpelling,
        token,
        parameter: lookup.parameter,
      }),
      type:
        lookup.parameter.declaredType._tag === 'Resolved'
          ? availableExpressionType(lookup.parameter.declaredType.type)
          : unavailableExpressionType,
      diagnostics: Object.freeze([]),
    })
  }
  if (lookup._tag === 'Ambiguous') {
    return Object.freeze({
      reference: Object.freeze({
        _tag: 'Ambiguous' as const,
        spelling: tokenSpelling,
        token,
        parameters: lookup.parameters,
      }),
      type: unavailableExpressionType,
      diagnostics: Object.freeze([]),
    })
  }
  const missingDiagnostic = Diagnostic.unknownValueReference(tokenSpelling, token.span)
  return Object.freeze({
    reference: Object.freeze({
      _tag: 'Missing' as const,
      spelling: tokenSpelling,
      token,
      cause: Diagnostic.identity(missingDiagnostic),
    }),
    type: unavailableExpressionType,
    diagnostics: Object.freeze([missingDiagnostic]),
  })
}

const analyzeIdentifier = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  scope: Scope,
): IdentifierResult => {
  const token = directToken(node, 'Identifier')
  if (token === undefined || !node.children.every(isAvailableSyntax)) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Identifier',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax:
            token === undefined
              ? unavailableSyntax(node, 'Identifier')
              : unavailableElement(node.children, node),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
      syntax: node,
    })
  }

  const resolution = resolveValueName(scope, spelling(source, token), token)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Identifier',
      reference: resolution.reference,
      type: resolution.type,
      syntax: node,
    }),
    diagnostics: resolution.diagnostics,
    type: resolution.type._tag === 'Available' ? resolution.type.type : undefined,
    syntax: node,
  })
}

const analyzeConstantReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  resolution: ResolutionContext,
): ExpressionResult | undefined => {
  const identifiers = SyntaxTree.tokens(node).filter((token) => token.kind === 'Identifier')
  const first = identifiers.at(0)
  const second = identifiers.at(1)
  if (first === undefined || identifiers.length > 2) return undefined
  const lookup =
    second === undefined
      ? NameResolution.lookup(resolution.scope, resolution.index, spelling(source, first))
      : NameResolution.lookupQualified(
          resolution.scope,
          resolution.index,
          spelling(source, first),
          spelling(source, second),
          second,
        )
  return lookup._tag === 'Resolved' && lookup.declaration._tag === 'ConstantDeclaration'
    ? analyzeConstant(lookup.declaration, second ?? first, node, false)
    : undefined
}

interface MoveResult {
  readonly fact: MoveExpressionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly type: SemanticType | undefined
}

const analyzeMove = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): MoveResult => {
  const subjectNode = node.children.find(isExpressionNode)
  const subject =
    subjectNode === undefined
      ? undefined
      : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
  if (subject === undefined) throw new RangeError('Move expression requires a subject expression')
  const invalidMove =
    subject.fact._tag === 'Constant'
      ? Diagnostic.invalidConstant('constants are immediate values and cannot be moved', node.span)
      : undefined
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Move',
      subject: subject.fact,
      type: subject.fact.type,
      syntax: node,
    }),
    diagnostics: Object.freeze(
      invalidMove === undefined ? subject.diagnostics : [...subject.diagnostics, invalidMove],
    ),
    type: invalidMove === undefined ? subject.type : undefined,
  })
}

const borrowRoot = (subject: ExpressionFact): BorrowRootFact | undefined => {
  if (subject._tag === 'Grouped') return borrowRoot(subject.expression)
  if (subject._tag === 'FieldProjection' && subject.state._tag === 'Resolved') {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'Field' as const,
              field: subject.state.field.id,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (
    subject._tag === 'IndexProjection' &&
    subject.array !== undefined &&
    (subject.bounds._tag === 'Proven' || subject.bounds._tag === 'Runtime')
  ) {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'Index' as const,
              index: subject.index,
              array: subject.array,
              bounds: subject.bounds,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (
    subject._tag === 'IndexProjection' &&
    subject.slice !== undefined &&
    subject.bounds._tag === 'RuntimeSlice'
  ) {
    const root = borrowRoot(subject.subject)
    return root === undefined
      ? undefined
      : Object.freeze({
          ...root,
          path: Object.freeze([
            ...root.path,
            Object.freeze({
              _tag: 'SliceIndex' as const,
              index: subject.index,
              slice: subject.slice,
              span: subject.syntax.span,
            }),
          ]),
        })
  }
  if (subject._tag !== 'Identifier') return undefined
  if (subject.reference._tag === 'ResolvedBinding') {
    return Object.freeze({
      _tag: 'BindingRoot',
      binding: subject.reference.binding,
      path: Object.freeze([]),
    })
  }
  if (subject.reference._tag === 'Resolved') {
    return Object.freeze({
      _tag: 'ParameterRoot',
      parameter: subject.reference.parameter,
      path: Object.freeze([]),
    })
  }
  if (subject.reference._tag === 'ResolvedPattern') {
    return Object.freeze({
      _tag: 'PatternRoot',
      binding: subject.reference.binding,
      path: Object.freeze([]),
    })
  }
  return undefined
}

const exclusiveBorrowRoot = (root: BorrowRootFact): boolean =>
  root._tag === 'TemporaryRoot' ||
  (root._tag === 'BindingRoot' && root.binding.mutability === 'Mutable') ||
  (root._tag === 'PatternRoot' && root.binding.access === 'Exclusive') ||
  (root._tag === 'ParameterRoot' &&
    root.path.length > 0 &&
    root.parameter.declaredType._tag === 'Resolved' &&
    Type.isReference(root.parameter.declaredType.type) &&
    root.parameter.declaredType.type.access === 'Exclusive')

const unavailableBorrow = (
  node: SyntaxTree.Node,
  access: Type.Slice['access'],
  subject: ExpressionFact,
  diagnostics: ReadonlyArray<Diagnostic.Diagnostic>,
  cause?: Diagnostic.Diagnostic,
): ExpressionResult =>
  Object.freeze({
    fact: Object.freeze({
      _tag: 'Borrow',
      access,
      subject,
      formation: Object.freeze({
        _tag: 'Unavailable',
        ...(cause === undefined ? {} : { cause: Diagnostic.identity(cause) }),
      }),
      type: unavailableExpressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([...diagnostics, ...(cause === undefined ? [] : [cause])]),
    type: undefined,
  })

const analyzeBorrow = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected: SemanticType | undefined,
  borrowAllowed: boolean,
): ExpressionResult => {
  const access: Type.Slice['access'] =
    directToken(node, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
  const subjectNode = node.children.find(isExpressionNode)
  const subjectResult =
    subjectNode === undefined
      ? undefined
      : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
  const subject = subjectResult?.fact ?? unavailableExpression(node)
  const diagnostics = subjectResult?.diagnostics ?? Object.freeze([])
  if (
    !borrowAllowed ||
    (expected !== undefined && !Type.isSlice(expected) && !Type.isReference(expected))
  ) {
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidBorrowPosition(node.span),
    )
  }
  const sourceType = subjectResult?.type
  const root =
    borrowRoot(subject) ??
    (sourceType === undefined
      ? undefined
      : Object.freeze({
          _tag: 'TemporaryRoot' as const,
          owner: Object.freeze({
            _tag: 'TemporaryOwnerId' as const,
            function: declaration.id,
            span: subject.syntax.span,
            ordinal: 0,
          }),
          value: subject,
          path: Object.freeze([]),
        }))
  if (root === undefined || sourceType === undefined) {
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
    )
  }
  if (expected === undefined && !Type.isFixedArray(sourceType) && !Type.isSlice(sourceType)) {
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    const type = Type.reference(access, sourceType)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'ValueBorrow', root, source: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (expected !== undefined && Type.isReference(expected)) {
    if (!Type.infer(expected.target, sourceType, new Map())) {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
      )
    }
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    if (access !== expected.access) {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
      )
    }
    const type = Type.reference(access, sourceType)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'ValueBorrow', root, source: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (Type.isFixedArray(sourceType)) {
    if (access === 'Exclusive' && !exclusiveBorrowRoot(root)) {
      const name =
        subject._tag === 'Identifier' && 'spelling' in subject.reference
          ? subject.reference.spelling
          : '?'
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.exclusiveBorrowRequiresMutable(name, subjectNode?.span ?? node.span),
      )
    }
    const type = Type.slice(access, sourceType.element)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({ _tag: 'FixedArrayBorrow', root, array: sourceType }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  if (Type.isSlice(sourceType)) {
    if (sourceType.access === 'Shared' && access === 'Exclusive') {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidSliceReborrow(sourceType.access, access, node.span),
      )
    }
    const type = Type.slice(access, sourceType.element)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Borrow',
        access,
        subject,
        formation: Object.freeze({
          _tag: 'SliceReborrow',
          root,
          parent: sourceType,
          suspendsParent: sourceType.access === 'Exclusive',
        }),
        type: availableExpressionType(type),
        syntax: node,
      }),
      diagnostics,
      type,
    })
  }
  return unavailableBorrow(
    node,
    access,
    subject,
    diagnostics,
    Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
  )
}

interface StructTargetResult {
  readonly fact: StructTargetFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const intrinsicStruct = (
  type: Type.Nominal,
  syntax: SyntaxTree.Node,
  token: Token.Token,
): DeclarationIndex.StructFact => {
  const ordinal = Type.intrinsicNominalOrdinal(type)
  const id: DeclarationIndex.DeclarationId = Object.freeze({
    _tag: 'DeclarationId',
    sourceId: type.module,
    ordinal,
  })
  const fieldTypes: ReadonlyArray<readonly [string, Type.Type]> = Type.equals(type, Type.layout)
    ? Object.freeze([
        Object.freeze(['bytes', 'usize'] as const),
        Object.freeze(['alignment', 'usize'] as const),
      ])
    : Type.equals(type, Type.invalidAlignment)
      ? Object.freeze([Object.freeze(['alignment', 'usize'] as const)])
      : Object.freeze([])
  return Object.freeze({
    _tag: 'StructDeclaration',
    id,
    canonical: Object.freeze({
      _tag: 'Canonical',
      id: Object.freeze({
        _tag: 'CanonicalDeclarationId',
        module: type.module,
        name: type.name,
      }),
    }),
    visibility: 'Public',
    typeParameters: Object.freeze([]),
    name: Object.freeze({ _tag: 'Present', spelling: type.name, token }),
    fields: Object.freeze(
      fieldTypes.map(([name, fieldType], fieldOrdinal) =>
        Object.freeze({
          _tag: 'StructField' as const,
          id: Object.freeze({ _tag: 'FieldId' as const, struct: id, ordinal: fieldOrdinal }),
          state: Object.freeze({
            _tag: 'Unique' as const,
            id: Object.freeze({ _tag: 'FieldId' as const, struct: id, ordinal: fieldOrdinal }),
          }),
          visibility: 'Public' as const,
          name: Object.freeze({ _tag: 'Present' as const, spelling: name, token }),
          declaredType: Object.freeze({
            _tag: 'Resolved' as const,
            type: fieldType,
            spelling: Type.encode(fieldType),
            token,
            syntax,
          }),
          syntax,
        }),
      ),
    ),
    dependency: Object.freeze({ _tag: 'Available', types: Object.freeze([]) }),
    syntax,
  })
}

const resolveStructTarget = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  resolution: ResolutionContext,
  caller?: DeclarationFact,
  inferConstructionArguments = false,
): StructTargetResult => {
  const environment = new Map(
    (caller?.typeParameters ?? []).flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const analyzed = DeclarationIndex.analyzeDeclaredType(source, syntax, environment)
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  if (inferConstructionArguments) {
    const applied = analyzed.fact._tag === 'Applied' ? analyzed.fact : undefined
    const targetFact = applied?.target ?? analyzed.fact
    const path = targetFact._tag === 'Unresolved' ? targetFact.path : undefined
    const base =
      path === undefined
        ? targetFact._tag === 'Resolved' && Type.isNominal(targetFact.type)
          ? targetFact.type
          : undefined
        : (() => {
            const candidate = NameResolution.resolveType(
              nameResolution,
              resolution.index,
              source.id,
              path,
            ).fact
            return candidate._tag === 'Resolved' && Type.isNominal(candidate.type)
              ? candidate.type
              : undefined
          })()
    const candidate =
      base === undefined
        ? undefined
        : DeclarationIndex.byCanonical(resolution.index, {
            _tag: 'CanonicalDeclarationId',
            module: base.module,
            name: base.name,
          })
    if (base !== undefined && candidate?._tag === 'StructDeclaration') {
      const supplied = applied?.arguments ?? []
      const sourceParameters = candidate.typeParameters.filter(
        (parameter) =>
          parameter.type.kind !== 'CallableRepresentation' &&
          parameter.type.kind !== 'EffectRepresentation',
      )
      if (supplied.length <= sourceParameters.length) {
        const resolvedArguments = supplied.map((argument) =>
          DeclarationIndex.resolveTypeFact(
            resolution.index,
            source.id,
            argument,
            (module, argumentPath) =>
              NameResolution.resolveType(nameResolution, resolution.index, module, argumentPath),
          ),
        )
        let suppliedOrdinal = 0
        const arguments_ = candidate.typeParameters.flatMap(
          (parameter): ReadonlyArray<Type.GenericArgument> => {
            if (
              parameter.type.kind === 'CallableRepresentation' ||
              parameter.type.kind === 'EffectRepresentation'
            )
              return [Type.representationParameterArgument(parameter.type)]
            const resolved = resolvedArguments.at(suppliedOrdinal)
            suppliedOrdinal += 1
            if (resolved === undefined) return [Type.parameterArgument(parameter.type)]
            if (resolved?.fact._tag !== 'Resolved') return []
            if (parameter.type.kind === 'Value')
              return Type.isTypeArgument(resolved.fact.type) ? [resolved.fact.type] : []
            if (
              parameter.type.kind === 'RequirementRow' &&
              Type.isParameter(resolved.fact.type) &&
              resolved.fact.type.kind === 'RequirementRow'
            )
              return [Type.requirementRowArgument([], [resolved.fact.type])]
            return []
          },
        )
        if (arguments_.length === candidate.typeParameters.length) {
          const parameters = candidate.typeParameters.map((parameter) => parameter.type)
          if (Type.prefixSubstitution(parameters, arguments_) !== undefined) {
            const token = SyntaxTree.tokens(syntax).find(
              (candidateToken) => candidateToken.kind === 'Identifier',
            )
            if (token !== undefined)
              return Object.freeze({
                fact: Object.freeze({
                  _tag: 'Resolved',
                  struct: candidate,
                  type: Type.nominal(base.module, base.name, arguments_),
                  token,
                }),
                diagnostics: Diagnostic.merge(
                  analyzed.diagnostics,
                  ...resolvedArguments.map((argument) => argument.diagnostics),
                ),
              })
          }
        }
      }
    }
  }
  const resolved = DeclarationIndex.resolveTypeFact(
    resolution.index,
    source.id,
    analyzed.fact,
    (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
  )
  if (resolved.fact._tag === 'Resolved' && Type.isNominal(resolved.fact.type)) {
    if (Type.isIntrinsicNominal(resolved.fact.type)) {
      const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
      if (token !== undefined)
        return Object.freeze({
          fact: Object.freeze({
            _tag: 'Resolved',
            struct: intrinsicStruct(resolved.fact.type, syntax, token),
            type: resolved.fact.type,
            token,
          }),
          diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
        })
    }
    const declaration = DeclarationIndex.byCanonical(resolution.index, {
      _tag: 'CanonicalDeclarationId',
      module: resolved.fact.type.module,
      name: resolved.fact.type.name,
    })
    if (declaration?._tag === 'StructDeclaration') {
      const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
      if (token === undefined)
        return Object.freeze({
          fact: Object.freeze({ _tag: 'Unavailable' }),
          diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
        })
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'Resolved',
          struct: declaration,
          type: resolved.fact.type,
          token,
        }),
        diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
      })
    }
  }
  const token = SyntaxTree.tokens(syntax).find((candidate) => candidate.kind === 'Identifier')
  const diagnostic = Diagnostic.expectedType(
    resolved.fact._tag === 'Resolved' ? Type.encode(resolved.fact.type) : 'unavailable struct',
    token?.span ?? syntax.span,
  )
  return Object.freeze({
    fact: Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) }),
    diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics, [diagnostic]),
  })
}

interface PatternCounters {
  pattern: number
  binding: number
  invalid: boolean
}

interface PatternResult {
  readonly fact: PatternFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface PatternTypeResult {
  readonly type?: Type.Type
  readonly declared: DeclarationIndex.DeclaredTypeFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const resolvePatternType = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  resolution: ResolutionContext,
  declaration: DeclarationFact,
): PatternTypeResult => {
  const environment = new Map(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const analyzed = DeclarationIndex.analyzeDeclaredType(source, syntax, environment)
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  const resolved = DeclarationIndex.resolveTypeFact(
    resolution.index,
    source.id,
    analyzed.fact,
    (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
  )
  return Object.freeze({
    ...(resolved.fact._tag === 'Resolved' ? { type: resolved.fact.type } : {}),
    declared: resolved.fact,
    diagnostics: Diagnostic.merge(analyzed.diagnostics, resolved.diagnostics),
  })
}

const patternDeclaredName = (
  source: SourceFile.SourceFile,
  syntax: SyntaxTree.Node,
  token: Token.Token | undefined,
): DeclaredName =>
  token === undefined
    ? Object.freeze({ _tag: 'Unavailable', syntax })
    : Object.freeze({ _tag: 'Present', spelling: spelling(source, token), token })

const analyzePattern = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  arm: Match.ArmId,
  access: Match.Access,
  scope: Scope,
  resolution: ResolutionContext,
  declaration: DeclarationFact,
  counters: PatternCounters,
  prefix: ReadonlyArray<DeclarationIndex.FieldId> = Object.freeze([]),
  localNames = new Map<string, SourceSpan.SourceSpan>(),
): PatternResult => {
  const id: Match.PatternId = Object.freeze({
    _tag: 'PatternId',
    arm,
    ordinal: counters.pattern,
  })
  counters.pattern += 1
  if (node.kind === 'ErrorPattern') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'UnavailablePattern',
        id,
        bindings: Object.freeze([]),
        omitted: Object.freeze([]),
        complete: false,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (node.kind === 'UniversalPattern') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'UniversalPattern',
        id,
        bindings: Object.freeze([]),
        omitted: Object.freeze(access === 'Move' ? [Object.freeze([])] : []),
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
    })
  }

  if (node.kind === 'BindingPattern') {
    // `Member name` binds the entire member payload: no field destructuring, nothing omitted.
    const bindingTargetSyntax =
      SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
    const bindingTarget = resolvePatternType(source, bindingTargetSyntax, resolution, declaration)
    const bindingDiagnostics: Array<Diagnostic.Diagnostic> = [...bindingTarget.diagnostics]
    const member = bindingTarget.type
    const bindingToken = node.children.find(
      (element): element is Token.Token =>
        SyntaxTree.isToken(element) && element.kind === 'Identifier',
    )
    const declaredName: DeclaredName =
      bindingToken === undefined
        ? Object.freeze({ _tag: 'Unavailable' as const, syntax: node })
        : Object.freeze({
            _tag: 'Present' as const,
            spelling: spelling(source, bindingToken),
            token: bindingToken,
          })
    if (declaredName._tag === 'Present') {
      const original =
        scopeSpanFor(scope, declaredName.spelling) ?? localNames.get(declaredName.spelling)
      if (original === undefined) localNames.set(declaredName.spelling, declaredName.token.span)
      else {
        counters.invalid = true
        bindingDiagnostics.push(
          Diagnostic.patternBindingConflict(
            declaredName.spelling,
            original,
            declaredName.token.span,
          ),
        )
      }
    }
    const wholeBinding: PatternBindingFact = Object.freeze({
      _tag: 'PatternBinding',
      id: Object.freeze({ _tag: 'PatternBindingId' as const, arm, ordinal: counters.binding }),
      name: declaredName,
      path: prefix,
      type: member === undefined ? unavailableExpressionType : availableExpressionType(member),
      access,
      syntax: node,
    })
    counters.binding += 1
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'TypePattern',
        id,
        ...(member === undefined ? {} : { member }),
        declared: bindingTarget.declared,
        bindings: Object.freeze([wholeBinding]),
        omitted: Object.freeze([]),
        complete: member !== undefined && !counters.invalid && isAvailableSyntax(node),
        syntax: node,
      }),
      diagnostics: Object.freeze(bindingDiagnostics),
    })
  }

  const targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  const target = resolveStructTarget(source, targetSyntax, resolution, declaration)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct = target.fact._tag === 'Resolved' ? target.fact.struct : undefined
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const structSubstitution =
    struct === undefined || nominal === undefined
      ? new Map<string, SemanticType>()
      : (Type.substitution(
          struct.typeParameters.map((parameter) => parameter.type),
          nominal.arguments,
        ) ?? new Map())
  const label = nominal === undefined ? 'unknown struct' : Type.encode(nominal)
  const seen = new Map<string, PatternFieldFact>()
  const bindings: Array<PatternBindingFact> = []
  const fields = SyntaxTree.directNodes(node, 'PatternField').map((fieldNode): PatternFieldFact => {
    const identifiers = fieldNode.children.filter(
      (element): element is Token.Token =>
        SyntaxTree.isToken(element) && element.kind === 'Identifier',
    )
    const nameToken = identifiers.at(0)
    const name = nameToken === undefined ? undefined : spelling(source, nameToken)
    const lookup =
      struct === undefined || name === undefined
        ? undefined
        : DeclarationIndex.lookupField(struct.fields, name)
    let state: PatternFieldState = Object.freeze({ _tag: 'Unavailable' })
    let resolvedField: DeclarationIndex.FieldFact | undefined
    if (lookup?._tag === 'Resolved') {
      const original = seen.get(name ?? '')
      if (original === undefined) {
        resolvedField = lookup.field
        state = Object.freeze({ _tag: 'Resolved', field: lookup.field })
      } else {
        const diagnostic = Diagnostic.duplicatePatternField(
          name ?? '',
          original.syntax.span,
          nameToken?.span ?? fieldNode.span,
        )
        diagnostics.push(diagnostic)
        state = Object.freeze({
          _tag: 'Duplicate',
          field: lookup.field,
          cause: Diagnostic.identity(diagnostic),
        })
      }
    } else if (name !== undefined) {
      const diagnostic = Diagnostic.unknownStructField(
        label,
        name,
        nameToken?.span ?? fieldNode.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unknown', cause: Diagnostic.identity(diagnostic) })
    }

    const nestedNode =
      SyntaxTree.directNode(fieldNode, 'NominalPattern') ??
      SyntaxTree.directNode(fieldNode, 'BindingPattern')
    let nested: PatternFact | undefined
    let binding: PatternBindingFact | undefined
    if (nestedNode !== undefined) {
      const nestedResult = analyzePattern(
        source,
        nestedNode,
        arm,
        access,
        scope,
        resolution,
        declaration,
        counters,
        resolvedField === undefined ? prefix : Object.freeze([...prefix, resolvedField.id]),
        localNames,
      )
      diagnostics.push(...nestedResult.diagnostics)
      nested = nestedResult.fact
      const expected =
        resolvedField?.declaredType._tag === 'Resolved'
          ? Type.substitute(resolvedField.declaredType.type, structSubstitution)
          : undefined
      if (
        expected !== undefined &&
        (nested._tag === 'NominalPattern' || nested._tag === 'TypePattern') &&
        nested.member !== undefined &&
        !Type.equals(expected, nested.member)
      ) {
        counters.invalid = true
        diagnostics.push(
          Diagnostic.matchMemberNotInScrutinee(
            Type.encode(nested.member),
            Type.encode(expected),
            nestedNode.span,
          ),
        )
      }
    } else if (resolvedField !== undefined) {
      const bindingToken = identifiers.at(1) ?? nameToken
      const declaredName = patternDeclaredName(source, fieldNode, bindingToken)
      const bindingType =
        resolvedField.declaredType._tag === 'Resolved'
          ? availableExpressionType(
              Type.substitute(resolvedField.declaredType.type, structSubstitution),
            )
          : unavailableExpressionType
      binding = Object.freeze({
        _tag: 'PatternBinding',
        id: Object.freeze({
          _tag: 'PatternBindingId',
          arm,
          ordinal: counters.binding,
        }),
        name: declaredName,
        field: resolvedField,
        path: Object.freeze([...prefix, resolvedField.id]),
        type: bindingType,
        access,
        syntax: fieldNode,
      })
      counters.binding += 1
      bindings.push(binding)
      if (declaredName._tag === 'Present') {
        const original =
          scopeSpanFor(scope, declaredName.spelling) ?? localNames.get(declaredName.spelling)
        if (original === undefined) localNames.set(declaredName.spelling, declaredName.token.span)
        else {
          counters.invalid = true
          diagnostics.push(
            Diagnostic.patternBindingConflict(
              declaredName.spelling,
              original,
              declaredName.token.span,
            ),
          )
        }
      }
    }
    if (nested?.bindings !== undefined) bindings.push(...nested.bindings)
    const fact: PatternFieldFact = Object.freeze({
      _tag: 'PatternField',
      name,
      ...(nameToken === undefined ? {} : { token: nameToken }),
      state,
      ...(binding === undefined ? {} : { binding }),
      ...(nested === undefined ? {} : { nested }),
      syntax: fieldNode,
    })
    if (name !== undefined && !seen.has(name)) seen.set(name, fact)
    return fact
  })

  const rest = SyntaxTree.directNode(node, 'RestPattern') !== undefined
  const omitted: Array<ReadonlyArray<DeclarationIndex.FieldId>> = fields.flatMap(
    (field) => field.nested?.omitted ?? [],
  )
  if (struct !== undefined && !rest) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      diagnostics.push(Diagnostic.missingPatternField(label, field.name.spelling, node.span))
    }
  } else if (struct !== undefined && rest) {
    for (const field of struct.fields) {
      if (field.name._tag === 'Present' && seen.has(field.name.spelling)) continue
      omitted.push(Object.freeze([...prefix, field.id]))
    }
  }
  const complete =
    target.fact._tag === 'Resolved' &&
    !counters.invalid &&
    isAvailableSyntax(node) &&
    fields.every(
      (field) =>
        field.state._tag === 'Resolved' &&
        (field.nested === undefined ||
          ((field.nested._tag === 'NominalPattern' || field.nested._tag === 'TypePattern') &&
            field.nested.complete)),
    ) &&
    (rest ||
      struct?.fields.every(
        (field) => field.name._tag !== 'Present' || seen.has(field.name.spelling),
      ) === true)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'NominalPattern',
      id,
      target: target.fact,
      ...(nominal === undefined ? {} : { member: nominal }),
      fields: Object.freeze(fields),
      bindings: Object.freeze(bindings),
      omitted: Object.freeze(omitted),
      rest,
      complete,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
  })
}

const unavailableExpression = (syntax: SyntaxTree.Node): ExpressionFact =>
  Object.freeze({
    _tag: 'Identifier',
    reference: Object.freeze({ _tag: 'Unavailable', syntax }),
    type: unavailableExpressionType,
    syntax,
  })

const matchAccess = (node: SyntaxTree.Node): Match.Access => {
  const access = SyntaxTree.directNode(node, 'MatchAccess')
  if (access === undefined) return 'Copy'
  if (directToken(access, 'MoveKeyword') !== undefined) return 'Move'
  if (directToken(access, 'Ampersand') === undefined) return 'Copy'
  return directToken(access, 'MutKeyword') === undefined ? 'Shared' : 'Exclusive'
}

const analyzeMatch = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
): ExpressionResult => {
  const id: Match.MatchId = Object.freeze({
    _tag: 'MatchId',
    function: declaration.id,
    span: node.span,
  })
  const access = matchAccess(node)
  const expressionNodes = node.children.filter(isExpressionNode)
  const scrutineeNode = expressionNodes.at(0)
  const scrutinee =
    scrutineeNode === undefined
      ? undefined
      : analyzeExpression(source, scrutineeNode, declarations, declaration, scope, resolution)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...(scrutinee?.diagnostics ?? [])]
  const members = scrutinee?.type === undefined ? undefined : Match.membersOf(scrutinee.type)

  const preliminary = SyntaxTree.directNodes(node, 'MatchArm').map((armNode, ordinal) => {
    const armId: Match.ArmId = Object.freeze({ _tag: 'MatchArmId', match: id, ordinal })
    const patternNode =
      SyntaxTree.directNode(armNode, 'ErrorPattern') ??
      SyntaxTree.directNode(armNode, 'NominalPattern') ??
      SyntaxTree.directNode(armNode, 'BindingPattern') ??
      SyntaxTree.directNode(armNode, 'UniversalPattern')
    if (patternNode === undefined) throw new RangeError('Match arm requires a pattern')
    const pattern = analyzePattern(
      source,
      patternNode,
      armId,
      access,
      scope,
      resolution,
      declaration,
      {
        pattern: 0,
        binding: 0,
        invalid: false,
      },
    )
    diagnostics.push(...pattern.diagnostics)
    return Object.freeze({ armNode, armId, pattern: pattern.fact })
  })
  const coverage = Match.cover(
    members ?? Object.freeze([]),
    preliminary.map(({ armNode, pattern }) =>
      Object.freeze({
        ...((pattern._tag === 'NominalPattern' || pattern._tag === 'TypePattern') &&
        pattern.member !== undefined
          ? { member: pattern.member }
          : {}),
        universal: pattern._tag === 'UniversalPattern',
        guarded: directToken(armNode, 'IfKeyword') !== undefined,
      }),
    ),
  )
  const arms = preliminary.map(({ armNode, armId, pattern }, ordinal): MatchArmFact => {
    const transition = coverage.transitions.at(ordinal)
    if (transition === undefined) throw new RangeError('Match coverage lost an arm')
    if (
      (pattern._tag === 'NominalPattern' || pattern._tag === 'TypePattern') &&
      pattern.member !== undefined &&
      members !== undefined &&
      !members.some((member) => Type.equals(member, pattern.member ?? member))
    ) {
      diagnostics.push(
        Diagnostic.matchMemberNotInScrutinee(
          Type.encode(pattern.member),
          scrutinee?.type === undefined ? 'unknown' : Type.encode(scrutinee.type),
          pattern.syntax.span,
        ),
      )
    } else if (
      pattern._tag !== 'UnavailablePattern' &&
      !transition.reachable &&
      (members?.length ?? 0) > 0
    ) {
      diagnostics.push(
        Diagnostic.unreachableMatchArm(
          pattern._tag === 'UniversalPattern'
            ? '_'
            : pattern.member === undefined
              ? 'unknown'
              : Type.encode(pattern.member),
          armNode.span,
        ),
      )
    }
    const armExpressions = armNode.children.filter(isExpressionNode)
    const guarded = directToken(armNode, 'IfKeyword') !== undefined
    const guardNode = guarded ? armExpressions.at(0) : undefined
    const resultNode = armExpressions.at(guarded ? 1 : 0)
    const armScope: Scope = Object.freeze({
      parameters: scope.parameters,
      bindings: scope.bindings,
      patternBindings: Object.freeze([...scope.patternBindings, ...pattern.bindings]),
    })
    const guard =
      guardNode === undefined
        ? undefined
        : analyzeExpression(source, guardNode, declarations, declaration, armScope, resolution)
    if (guard !== undefined) {
      diagnostics.push(...guard.diagnostics)
      if (guard.type !== undefined && guard.type !== 'bool') {
        diagnostics.push(
          Diagnostic.matchGuardNotBool(Type.encode(guard.type), guardNode?.span ?? armNode.span),
        )
      }
    }
    const result =
      resultNode === undefined
        ? undefined
        : analyzeExpression(
            source,
            resultNode,
            declarations,
            declaration,
            armScope,
            resolution,
            expected,
            borrowAllowed,
          )
    if (result !== undefined) diagnostics.push(...result.diagnostics)
    return Object.freeze({
      _tag: 'MatchArm',
      id: armId,
      pattern,
      bindings: pattern.bindings,
      ...(guard === undefined ? {} : { guard: guard.fact }),
      result: result?.fact ?? unavailableExpression(resultNode ?? armNode),
      before: transition.before,
      after: transition.after,
      reachable: transition.reachable,
      syntax: armNode,
    })
  })
  if (
    members !== undefined &&
    !coverage.exhaustive &&
    !preliminary.some(({ pattern }) => pattern._tag === 'UnavailablePattern')
  ) {
    diagnostics.push(Diagnostic.incompleteMatch(coverage.missing.map(Type.encode), node.span))
  }
  const reachableTypes = arms.flatMap((arm) =>
    arm.reachable && arm.result.type._tag === 'Available' ? [arm.result.type.type] : [],
  )
  const unavailableReachableResult = arms.some(
    (arm) => arm.reachable && arm.result.type._tag !== 'Available',
  )
  const joined = Match.join(reachableTypes)
  if (joined._tag === 'Incompatible') {
    let divergentRepresentations:
      | {
          readonly divergence: Type.RepresentationDivergence
          readonly spans: readonly [SourceSpan.SourceSpan, SourceSpan.SourceSpan]
        }
      | undefined
    const reachableResults = arms.flatMap((arm) =>
      arm.reachable && arm.result.type._tag === 'Available'
        ? [Object.freeze({ type: arm.result.type.type, span: arm.result.syntax.span })]
        : [],
    )
    for (const [leftOrdinal, left] of reachableResults.entries()) {
      for (const right of reachableResults.slice(leftOrdinal + 1)) {
        const divergence = Type.firstRepresentationDivergence(left.type, right.type)
        if (divergence !== undefined) {
          divergentRepresentations = Object.freeze({
            divergence,
            spans: Object.freeze([left.span, right.span] as const),
          })
          break
        }
      }
      if (divergentRepresentations !== undefined) break
    }
    diagnostics.push(
      divergentRepresentations === undefined
        ? Diagnostic.incompatibleMatchResults(joined.types.map(Type.encode), node.span)
        : Diagnostic.divergentRepresentationJoin(
            Type.encodeGenericArgument(divergentRepresentations.divergence.left),
            Type.encodeGenericArgument(divergentRepresentations.divergence.right),
            divergentRepresentations.spans,
            node.span,
          ),
    )
  }
  const hasInvalidGuard = arms.some(
    (arm) =>
      arm.guard !== undefined &&
      arm.guard.type._tag === 'Available' &&
      arm.guard.type.type !== 'bool',
  )
  const joinedEffect =
    joined._tag === 'Joined'
      ? Type.isRepresented(joined.type)
        ? Type.isEffect(joined.type.contract)
          ? joined.type.contract
          : undefined
        : Type.isEffect(joined.type)
          ? joined.type
          : undefined
      : undefined
  const effectAlternatives =
    joinedEffect === undefined
      ? Object.freeze([])
      : Object.freeze(
          arms.flatMap((arm) => {
            if (!arm.reachable) return []
            const representation = representationOfExpression(arm.result)
            return representation !== undefined &&
              Type.isExactRepresentationArgument(representation) &&
              Type.isEffectIdentityArgument(representation.identity) &&
              Type.isEffect(representation.contract)
              ? [representation]
              : []
          }),
        )
  const reachableEffectArms =
    joinedEffect === undefined ? 0 : arms.filter((arm) => arm.reachable).length
  const unavailableEffectComposite =
    joinedEffect !== undefined && effectAlternatives.length !== reachableEffectArms
  if (unavailableEffectComposite)
    diagnostics.push(
      Diagnostic.nonFiniteEffectJoin(
        'every reachable alternative must retain one exact static Effect representation',
        node.span,
      ),
    )
  const callableSites = arms.flatMap((arm) =>
    arm.reachable && arm.result._tag === 'CallableSection'
      ? [Hir.executableSiteKey(arm.result.site)]
      : [],
  )
  const erasesCallableIdentity = new Set(callableSites).size > 1
  if (erasesCallableIdentity) diagnostics.push(Diagnostic.callableIdentityErasure(node.span))
  const type =
    members !== undefined &&
    coverage.exhaustive &&
    arms.every(
      (arm) =>
        arm.reachable &&
        (arm.pattern._tag === 'UniversalPattern' ||
          ((arm.pattern._tag === 'NominalPattern' || arm.pattern._tag === 'TypePattern') &&
            arm.pattern.complete)),
    ) &&
    !unavailableReachableResult &&
    !hasInvalidGuard &&
    !unavailableEffectComposite &&
    !erasesCallableIdentity &&
    joined._tag === 'Joined'
      ? availableExpressionType(
          joinedEffect === undefined
            ? joined.type
            : Type.represented(
                joinedEffect,
                joinedEffect,
                effectAlternatives.length === 1
                  ? (effectAlternatives.at(0) ??
                      Type.compositeEffectRepresentationArgument(joinedEffect, effectAlternatives))
                  : Type.compositeEffectRepresentationArgument(joinedEffect, effectAlternatives),
              ),
        )
      : unavailableExpressionType
  const fact: MatchExpressionFact = Object.freeze({
    _tag: 'Match',
    id,
    access,
    scrutinee: scrutinee?.fact ?? unavailableExpression(scrutineeNode ?? node),
    members: Object.freeze([...(members ?? [])]),
    arms: Object.freeze(arms),
    exhaustive: coverage.exhaustive,
    type,
    syntax: node,
  })
  return Object.freeze({
    fact,
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const callableRepresentationTarget = (
  reference: CallReferenceFact,
): Type.CallableIdentityArgument['target'] | undefined => {
  if (reference._tag === 'Resolved') {
    const canonical = reference.declaration.canonical
    return canonical._tag === 'Canonical'
      ? Object.freeze({
          _tag: 'Declaration',
          module: canonical.id.module,
          name: canonical.id.name,
        })
      : undefined
  }
  return reference._tag === 'ResolvedBuiltin'
    ? Object.freeze({
        _tag: 'Builtin',
        actor: reference.actor,
        operation: reference.operation,
        intrinsic: Object.freeze({
          actor: reference.intrinsic.actor,
          name: reference.intrinsic.name,
        }),
      })
    : undefined
}

const exactCallableRepresentation = (
  reference: CallReferenceFact,
  contract: Type.Callable,
  typeArguments: ReadonlyArray<Type.GenericArgument> = Object.freeze([]),
  environment?: Type.CallableEnvironmentIdentity,
): Type.ExactRepresentationArgument | undefined => {
  const target = callableRepresentationTarget(reference)
  if (target === undefined) return undefined
  const identity =
    target._tag === 'Declaration'
      ? `declaration:${target.module}:${target.name}`
      : `builtin:${target.actor}:${target.operation}`
  return Type.exactRepresentationArgument(
    Type.callableIdentityArgument(identity, target, typeArguments, environment),
    contract,
  )
}

const exactEffectDeclarationRepresentation = (
  declaration: DeclarationFact,
  contract: Type.Effect,
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): Type.ExactRepresentationArgument | undefined => {
  if (declaration.functionKind !== 'Effect' || declaration.canonical._tag !== 'Canonical')
    return undefined
  const owner = Object.freeze({
    declaration: Object.freeze({
      module: declaration.canonical.id.module,
      name: declaration.canonical.id.name,
    }),
    typeArguments,
  })
  const site: Hir.EffectSiteId = Object.freeze({
    _tag: 'EffectSiteId',
    function: declaration.id,
    owner: declaration.canonical.id,
    ordinal: -1,
    span: declaration.syntax.span,
  })
  return Type.exactRepresentationArgument(
    Type.effectIdentityArgument(Hir.effectRepresentationIdentity(site), owner),
    contract,
  )
}

const exactEffectIdentityOfExpression = (
  expression: ExpressionFact,
): Type.EffectIdentityArgument | undefined => {
  const representation = representationOfExpression(expression)
  return representation?._tag === 'ExactRepresentationArgument' &&
    Type.isEffectIdentityArgument(representation.identity)
    ? representation.identity
    : undefined
}

const hiddenEffectArguments = (
  declaration: DeclarationFact,
  substitution: Type.Substitution,
  argumentAt: (ordinal: number) => ExpressionFact | undefined,
): ReadonlyArray<Type.EffectIdentityArgument> =>
  Object.freeze(
    declaration.parameters.flatMap((parameter, ordinal) => {
      const declared = parameter.declaredType
      if (declared._tag !== 'Resolved') return []
      const specialized = Type.substitute(declared.type, substitution)
      const contract = Type.isRepresented(specialized) ? specialized.contract : specialized
      if (!Type.isEffect(contract)) return []
      const argument = argumentAt(ordinal)
      const identity =
        argument === undefined ? undefined : exactEffectIdentityOfExpression(argument)
      return identity === undefined ? [] : [identity]
    }),
  )

const exactEffectApplicationContract = (
  _declaration: DeclarationFact,
  _substitution: Type.Substitution,
  contract: Type.Effect,
): Type.Effect => contract

const effectCallableApplicationRepresentation = (
  expression: CallableApplyExpressionFact,
  contract: Type.Effect,
): Type.ExactRepresentationArgument | undefined => {
  const callee = expression.callee
  if (callee._tag !== 'CallableSection' || callee.reference._tag !== 'Resolved') return undefined
  const substitution = new Map(callee.substitution)
  for (const [parameter, argument] of expression.substitution) substitution.set(parameter, argument)
  const declaredArguments = Object.freeze(
    callee.reference.declaration.typeParameters.map(
      (parameter) =>
        substitution.get(Type.key(parameter.type)) ??
        Type.substituteGenericArgument(parameter.type, substitution),
    ),
  )
  const applicationArgument = (ordinal: number): ExpressionFact | undefined => {
    const captured = callee.captures.find((capture) => capture.parameterOrdinal === ordinal)
    if (captured !== undefined) return captured.expression
    const argumentOrdinal = callee.remainingParameters.indexOf(ordinal)
    return argumentOrdinal < 0 ? undefined : expression.arguments.at(argumentOrdinal)?.expression
  }
  return exactEffectDeclarationRepresentation(
    callee.reference.declaration,
    exactEffectApplicationContract(callee.reference.declaration, substitution, contract),
    Object.freeze([
      ...declaredArguments,
      ...hiddenEffectArguments(callee.reference.declaration, substitution, applicationArgument),
    ]),
  )
}

/**
 * Recovers a compile-time representation from semantic expression structure. This is deliberately
 * frontend-owned: later phases consume the retained argument and never reconstruct it from syntax.
 */
export function representationOfExpression(
  expression: ExpressionFact,
): Type.RepresentationArgument | undefined {
  if (expression.type._tag === 'Available' && Type.isRepresented(expression.type.type))
    return expression.type.type.representation.argument
  if (expression._tag === 'FunctionItem' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    return Type.isCallable(contract)
      ? exactCallableRepresentation(expression.reference, contract)
      : undefined
  }
  if (expression._tag === 'CallableSection' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    if (!Type.isCallable(contract)) return undefined
    if (expression.environmentOwner === undefined) return undefined
    const environment = Hir.callableEnvironmentIdentity(
      expression.site,
      expression.environmentOwner,
    )
    return exactCallableRepresentation(
      expression.reference,
      contract,
      expression.typeArguments,
      environment,
    )
  }
  if (expression._tag === 'EffectBlock' && expression.type._tag === 'Available') {
    const contract = expression.type.type
    if (!Type.isEffect(contract)) return undefined
    const site = expression.site
    return Type.exactRepresentationArgument(
      Type.effectIdentityArgument(
        Hir.effectRepresentationIdentity(site),
        expression.representationOwner,
      ),
      contract,
    )
  }
  if (
    expression._tag === 'Call' &&
    expression.reference._tag === 'Resolved' &&
    expression.contract._tag === 'Compatible' &&
    expression.type._tag === 'Available' &&
    Type.isEffect(expression.type.type)
  ) {
    return exactEffectDeclarationRepresentation(
      expression.reference.declaration,
      expression.type.type,
      Object.freeze([
        ...expression.contract.typeArguments,
        ...hiddenEffectArguments(
          expression.reference.declaration,
          expression.contract.substitution,
          (ordinal) => expression.arguments.at(ordinal)?.expression,
        ),
      ]),
    )
  }
  if (
    expression._tag === 'CallableApply' &&
    expression.type._tag === 'Available' &&
    Type.isEffect(expression.type.type)
  ) {
    return effectCallableApplicationRepresentation(expression, expression.type.type)
  }
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding')
    return representationOfExpression(expression.reference.binding.initializer)
  if (expression._tag === 'Move') return representationOfExpression(expression.subject)
  if (expression._tag === 'Grouped') return representationOfExpression(expression.expression)
  return undefined
}

interface InferredStructArgument {
  readonly argument: Type.GenericArgument
  readonly span: SourceSpan.SourceSpan
}

const isOwnStructArgument = (parameter: Type.Parameter, argument: Type.GenericArgument): boolean =>
  Type.equalsGenericArgument(Type.parameterArgument(parameter), argument)

const analyzeStructLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  const target = resolveStructTarget(source, targetSyntax, resolution, declaration, true)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct = target.fact._tag === 'Resolved' ? target.fact.struct : undefined
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const nominalLabel = nominal === undefined ? 'unknown struct' : Type.encode(nominal)
  const inferredArguments = new Map<string, InferredStructArgument>()
  const argumentOrigins = new Map<string, ReadonlyArray<SourceSpan.SourceSpan>>()
  const explicitArguments = new Set<string>()
  if (struct !== undefined && nominal !== undefined) {
    for (const [ordinal, parameter] of struct.typeParameters.entries()) {
      const argument = nominal.arguments.at(ordinal)
      if (argument === undefined || isOwnStructArgument(parameter.type, argument)) continue
      const parameterKey = Type.key(parameter.type)
      inferredArguments.set(parameterKey, Object.freeze({ argument, span: targetSyntax.span }))
      argumentOrigins.set(parameterKey, Object.freeze([targetSyntax.span]))
      explicitArguments.add(parameterKey)
    }
  }
  const structSubstitution = new Map<string, Type.GenericArgument>()
  for (const [parameterKey, inferred] of inferredArguments)
    structSubstitution.set(parameterKey, inferred.argument)
  const definingModule = nominal?.module
  const authorized =
    definingModule !== undefined &&
    struct?.syntax.kind === 'StructDeclaration' &&
    struct.fields.every((field) => field.visibility === 'Public' || definingModule === source.id)
  const accessDiagnostic =
    nominal !== undefined && !authorized
      ? Diagnostic.inaccessibleStructConstruction(Type.encode(nominal), node.span)
      : undefined
  if (accessDiagnostic !== undefined) diagnostics.push(accessDiagnostic)

  const seen = new Map<string, StructInitializerFact>()
  const initializers = SyntaxTree.directNodes(node, 'StructFieldInitializer').map(
    (initializer): StructInitializerFact => {
      const nameToken = directToken(initializer, 'Identifier')
      const name = nameToken === undefined ? undefined : spelling(source, nameToken)
      const fieldLookup =
        struct === undefined || name === undefined
          ? undefined
          : DeclarationIndex.lookupField(struct.fields, name)
      const expected =
        fieldLookup?._tag === 'Resolved' && fieldLookup.field.declaredType._tag === 'Resolved'
          ? Type.substitute(fieldLookup.field.declaredType.type, structSubstitution)
          : undefined
      const contextualExpected =
        expected !== undefined && Type.isRepresented(expected) ? expected.contract : expected
      const expressionNode = initializer.children.find(isExpressionNode)
      if (expressionNode === undefined) {
        throw new RangeError('Struct initializer requires an expression node')
      }
      const expression = analyzeExpression(
        source,
        expressionNode,
        declarations,
        declaration,
        scope,
        resolution,
        contextualExpected,
      )
      if (expression === undefined) {
        throw new RangeError(`Cannot analyze struct initializer ${expressionNode.kind}`)
      }
      diagnostics.push(...expression.diagnostics)
      let state: StructInitializerState = Object.freeze({ _tag: 'Unavailable' })
      if (name !== undefined && nameToken !== undefined && struct !== undefined) {
        const previous = seen.get(name)
        if (fieldLookup?._tag !== 'Resolved') {
          const diagnostic = Diagnostic.unknownStructField(nominalLabel, name, nameToken.span)
          diagnostics.push(diagnostic)
          state = Object.freeze({ _tag: 'Unknown', cause: Diagnostic.identity(diagnostic) })
        } else if (previous !== undefined) {
          const diagnostic = Diagnostic.duplicateStructInitializer(
            name,
            previous.syntax.span,
            nameToken.span,
          )
          diagnostics.push(diagnostic)
          state = Object.freeze({
            _tag: 'Duplicate',
            field: fieldLookup.field,
            cause: Diagnostic.identity(diagnostic),
          })
        } else if (
          fieldLookup.field.visibility === 'Private' &&
          nominal !== undefined &&
          nominal.module !== source.id &&
          accessDiagnostic !== undefined
        ) {
          state = Object.freeze({
            _tag: 'Inaccessible',
            field: fieldLookup.field,
            cause: Diagnostic.identity(accessDiagnostic),
          })
        } else if (
          fieldLookup.field.declaredType._tag === 'Resolved' &&
          expression.type !== undefined
        ) {
          const expectedType = Type.substitute(
            fieldLookup.field.declaredType.type,
            structSubstitution,
          )
          const expectedValue = Type.isRepresented(expectedType)
            ? expectedType.contract
            : expectedType
          const actualValue = Type.isRepresented(expression.type)
            ? expression.type.contract
            : expression.type
          let representationDiagnostic: Diagnostic.Diagnostic | undefined
          if (Type.isRepresented(expectedType)) {
            const currentSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              currentSubstitution.set(parameterKey, inferred.argument)
            const candidateSubstitution = new Map(currentSubstitution)
            if (Type.infer(expectedType.contract, actualValue, candidateSubstitution)) {
              const siteSubstitution = new Map<string, Type.GenericArgument>()
              Type.infer(expectedType.contract, actualValue, siteSubstitution)
              for (const parameter of struct.typeParameters) {
                if (
                  parameter.type.kind === 'CallableRepresentation' ||
                  parameter.type.kind === 'EffectRepresentation'
                )
                  continue
                const parameterKey = Type.key(parameter.type)
                const inferred = siteSubstitution.get(parameterKey)
                if (inferred === undefined || isOwnStructArgument(parameter.type, inferred))
                  continue
                if (inferredArguments.get(parameterKey) === undefined)
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: inferred, span: expressionNode.span }),
                  )
                argumentOrigins.set(
                  parameterKey,
                  Object.freeze([
                    ...(argumentOrigins.get(parameterKey) ?? []),
                    expressionNode.span,
                  ]),
                )
              }
            }
            const representationSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              representationSubstitution.set(parameterKey, inferred.argument)
            const specialized = Type.substitute(expectedType, representationSubstitution)
            if (!Type.isRepresented(specialized))
              throw new RangeError('represented struct field lost its representation contract')
            const specializedExpectedType = specialized
            const actualRepresentation = representationOfExpression(expression.fact)
            const requiredArgument = expectedType.representation.argument
            if (actualRepresentation === undefined) {
              representationDiagnostic = Diagnostic.structFieldTypeMismatch(
                name,
                Type.encode(specializedExpectedType),
                Type.encode(expression.type),
                expressionNode.span,
              )
            } else if (requiredArgument._tag === 'RepresentationParameterArgument') {
              const parameter = requiredArgument.parameter
              const parameterKey = Type.key(parameter)
              const previousRepresentation = inferredArguments.get(parameterKey)
              if (
                previousRepresentation !== undefined &&
                !Type.equalsGenericArgument(previousRepresentation.argument, actualRepresentation)
              ) {
                representationDiagnostic = Diagnostic.conflictingInitializerRepresentation(
                  parameter.name,
                  Type.encodeGenericArgument(previousRepresentation.argument),
                  Type.encodeGenericArgument(actualRepresentation),
                  previousRepresentation.span,
                  expressionNode.span,
                )
              } else {
                const represented = Type.represented(
                  Type.isCallable(actualValue) || Type.isEffect(actualValue)
                    ? actualValue
                    : specializedExpectedType.contract,
                  specializedExpectedType.representation.requiredBound,
                  actualRepresentation,
                )
                if (represented.representation.admissibility._tag === 'Unavailable') {
                  const requiredParameter = struct.typeParameters.find(
                    (candidate) => Type.key(candidate.type) === Type.key(parameter),
                  )
                  const actualParameter =
                    actualRepresentation._tag === 'RepresentationParameterArgument'
                      ? declaration.typeParameters.find(
                          (candidate) =>
                            Type.key(candidate.type) === Type.key(actualRepresentation.parameter),
                        )
                      : undefined
                  representationDiagnostic = Diagnostic.incompatibleRepresentationBound(
                    parameter.name,
                    Type.encode(specializedExpectedType.representation.requiredBound),
                    Type.encode(represented.contract),
                    expressionNode.span,
                    {
                      ...(requiredParameter === undefined
                        ? {}
                        : { requiredDeclarationSpan: requiredParameter.syntax.span }),
                      ...(actualParameter === undefined
                        ? {}
                        : { actualDeclarationSpan: actualParameter.syntax.span }),
                    },
                  )
                  if (previousRepresentation === undefined)
                    inferredArguments.set(
                      parameterKey,
                      Object.freeze({ argument: actualRepresentation, span: expressionNode.span }),
                    )
                } else if (previousRepresentation === undefined) {
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: actualRepresentation, span: expressionNode.span }),
                  )
                }
                if (
                  previousRepresentation === undefined ||
                  Type.equalsGenericArgument(previousRepresentation.argument, actualRepresentation)
                )
                  argumentOrigins.set(
                    parameterKey,
                    Object.freeze([
                      ...(argumentOrigins.get(parameterKey) ?? []),
                      expressionNode.span,
                    ]),
                  )
              }
            } else if (!Type.equalsGenericArgument(requiredArgument, actualRepresentation)) {
              representationDiagnostic = Diagnostic.structFieldTypeMismatch(
                name,
                Type.encodeGenericArgument(requiredArgument),
                Type.encodeGenericArgument(actualRepresentation),
                expressionNode.span,
              )
            }
          } else {
            const currentSubstitution = new Map(structSubstitution)
            for (const [parameterKey, inferred] of inferredArguments)
              currentSubstitution.set(parameterKey, inferred.argument)
            const candidateSubstitution = new Map(currentSubstitution)
            if (!Type.infer(expectedType, expression.type, candidateSubstitution)) {
              const impliedSubstitution = new Map<string, Type.GenericArgument>()
              if (Type.infer(expectedType, expression.type, impliedSubstitution)) {
                for (const parameter of struct.typeParameters) {
                  if (parameter.type.kind !== 'Value') continue
                  const parameterKey = Type.key(parameter.type)
                  const previous = inferredArguments.get(parameterKey)
                  const implied = impliedSubstitution.get(parameterKey)
                  if (
                    previous === undefined ||
                    implied === undefined ||
                    Type.equalsGenericArgument(previous.argument, implied)
                  )
                    continue
                  representationDiagnostic = Diagnostic.typeArgumentConflict(
                    nominalLabel,
                    parameter.type.name,
                    Type.encodeGenericArgument(previous.argument),
                    Type.encodeGenericArgument(implied),
                    expressionNode.span,
                    previous.span,
                  )
                  break
                }
              }
              const specializedExpected = Type.substitute(expectedType, currentSubstitution)
              const divergence = Type.firstRepresentationDivergence(
                specializedExpected,
                expression.type,
              )
              if (representationDiagnostic === undefined && divergence !== undefined) {
                const parameter = struct.typeParameters.find((candidate) => {
                  const inferred = inferredArguments.get(Type.key(candidate.type))
                  return (
                    inferred !== undefined &&
                    Type.equalsGenericArgument(inferred.argument, divergence.left)
                  )
                })
                const original =
                  parameter === undefined
                    ? undefined
                    : inferredArguments.get(Type.key(parameter.type))
                if (parameter !== undefined && original !== undefined)
                  representationDiagnostic = Diagnostic.conflictingInitializerRepresentation(
                    parameter.type.name,
                    Type.encodeGenericArgument(divergence.left),
                    Type.encodeGenericArgument(divergence.right),
                    original.span,
                    expressionNode.span,
                  )
              }
            } else {
              const siteSubstitution = new Map<string, Type.GenericArgument>()
              Type.infer(expectedType, expression.type, siteSubstitution)
              for (const parameter of struct.typeParameters) {
                const parameterKey = Type.key(parameter.type)
                const inferred = siteSubstitution.get(parameterKey)
                if (
                  inferredArguments.get(parameterKey) === undefined &&
                  inferred !== undefined &&
                  !isOwnStructArgument(parameter.type, inferred)
                ) {
                  inferredArguments.set(
                    parameterKey,
                    Object.freeze({ argument: inferred, span: expressionNode.span }),
                  )
                  argumentOrigins.set(parameterKey, Object.freeze([expressionNode.span]))
                } else if (
                  inferred !== undefined &&
                  !isOwnStructArgument(parameter.type, inferred) &&
                  Type.equalsGenericArgument(
                    inferredArguments.get(parameterKey)?.argument ?? inferred,
                    inferred,
                  )
                ) {
                  argumentOrigins.set(
                    parameterKey,
                    Object.freeze([
                      ...(argumentOrigins.get(parameterKey) ?? []),
                      expressionNode.span,
                    ]),
                  )
                }
              }
            }
          }
          const compatibilitySubstitution = new Map(structSubstitution)
          for (const [parameterKey, inferred] of inferredArguments)
            compatibilitySubstitution.set(parameterKey, inferred.argument)
          const compatibleExpected = Type.substitute(expectedValue, compatibilitySubstitution)
          const compatibleValue = typesCompatible(actualValue, compatibleExpected)
          if (representationDiagnostic !== undefined || !compatibleValue) {
            const diagnostic =
              representationDiagnostic ??
              unionConversionDiagnostic(actualValue, compatibleExpected, expressionNode.span) ??
              Diagnostic.structFieldTypeMismatch(
                name,
                Type.encode(compatibleExpected),
                Type.encode(actualValue),
                expressionNode.span,
              )
            diagnostics.push(diagnostic)
            state = Object.freeze({
              _tag: 'TypeMismatch',
              field: fieldLookup.field,
              cause: Diagnostic.identity(diagnostic),
            })
          } else {
            state = Object.freeze({ _tag: 'Resolved', field: fieldLookup.field })
          }
        }
      }
      const fact: StructInitializerFact = Object.freeze({
        _tag: 'StructInitializer',
        name,
        ...(nameToken === undefined ? {} : { token: nameToken }),
        expression: expression.fact,
        state,
        syntax: initializer,
      })
      if (name !== undefined && !seen.has(name)) seen.set(name, fact)
      return fact
    },
  )

  const completedArguments =
    struct === undefined || nominal === undefined
      ? undefined
      : nominal.arguments.map((argument, ordinal): Type.GenericArgument => {
          const parameter = struct.typeParameters.at(ordinal)?.type
          if (parameter === undefined) return argument
          return inferredArguments.get(Type.key(parameter))?.argument ?? argument
        })
  const unresolvedParameters =
    struct === undefined || completedArguments === undefined
      ? []
      : struct.typeParameters.flatMap((parameter, ordinal) => {
          const argument = completedArguments.at(ordinal)
          return argument !== undefined && isOwnStructArgument(parameter.type, argument)
            ? [parameter]
            : []
        })
  for (const parameter of unresolvedParameters) {
    diagnostics.push(
      Diagnostic.uninferredTypeParameter(nominalLabel, parameter.type.name, parameter.syntax.span),
    )
  }
  const completedNominal =
    nominal === undefined ||
    completedArguments === undefined ||
    unresolvedParameters.length > 0 ||
    (struct !== undefined &&
      Type.substitution(
        struct.typeParameters.map((parameter) => parameter.type),
        completedArguments,
      ) === undefined)
      ? undefined
      : Type.nominal(nominal.module, nominal.name, completedArguments)
  const typeArguments: ReadonlyArray<StructTypeArgumentFact> = Object.freeze(
    struct?.typeParameters.map((parameter, ordinal) => {
      const parameterKey = Type.key(parameter.type)
      const argument = completedArguments?.at(ordinal)
      const origins = argumentOrigins.get(parameterKey) ?? Object.freeze([])
      return Object.freeze({
        parameter: parameter.type,
        ...(argument === undefined || isOwnStructArgument(parameter.type, argument)
          ? {}
          : { argument }),
        source:
          argument === undefined || isOwnStructArgument(parameter.type, argument)
            ? ('Unavailable' as const)
            : explicitArguments.has(parameterKey)
              ? ('Explicit' as const)
              : ('Inferred' as const),
        origins,
      })
    }) ?? [],
  )
  const completedTarget: StructTargetFact =
    completedNominal !== undefined && target.fact._tag === 'Resolved'
      ? Object.freeze({ ...target.fact, type: completedNominal })
      : target.fact

  if (struct !== undefined && completedNominal !== undefined) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      if (field.visibility === 'Private' && completedNominal.module !== source.id) continue
      diagnostics.push(
        Diagnostic.missingStructInitializer(
          Type.encode(completedNominal),
          field.name.spelling,
          node.span,
        ),
      )
    }
  }

  const fields =
    struct === undefined
      ? []
      : struct.fields.flatMap((field) => {
          if (field.name._tag !== 'Present') return []
          const fieldName = field.name.spelling
          const initializer = initializers.find(
            (candidate) => candidate.name === fieldName && candidate.state._tag === 'Resolved',
          )
          return initializer === undefined ? [] : [{ field, initializer }]
        })
  const complete =
    struct !== undefined &&
    completedNominal !== undefined &&
    authorized &&
    SyntaxTree.isAvailableSyntax(node) &&
    fields.length === struct.fields.length &&
    initializers.length === struct.fields.length &&
    initializers.every((initializer) => initializer.state._tag === 'Resolved')
  const type =
    complete && completedNominal !== undefined
      ? availableExpressionType(completedNominal)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'StructLiteral',
      target: completedTarget,
      authorized,
      typeArguments,
      initializers: Object.freeze(initializers),
      fields: Object.freeze(fields),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: complete ? completedNominal : undefined,
  })
}

const analyzeArrayLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const expectedArray = expected !== undefined && Type.isFixedArray(expected) ? expected : undefined
  const elementNodes = node.children.filter(isExpressionNode)
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  let elementType = expectedArray?.element
  let elementOrigin = expectedArray === undefined ? undefined : node.span
  const elements = elementNodes.map((elementNode, ordinal): ArrayElementFact => {
    const element = analyzeExpression(
      source,
      elementNode,
      declarations,
      declaration,
      scope,
      resolution,
      elementType,
    )
    if (element === undefined)
      throw new RangeError(`Cannot analyze array element ${elementNode.kind}`)
    diagnostics.push(...element.diagnostics)
    if (elementType === undefined && element.type !== undefined) {
      elementType = element.type
      elementOrigin = elementNode.span
    }
    let compatibility: ArrayElementFact['compatibility']
    if (element.type === undefined || elementType === undefined) {
      compatibility = Object.freeze({ _tag: 'Unavailable' })
    } else if (!typesCompatible(element.type, elementType)) {
      const diagnostic =
        representationJoinDiagnostic(
          elementType,
          element.type,
          elementOrigin ?? node.span,
          elementNode.span,
          elementNode.span,
        ) ??
        unionConversionDiagnostic(element.type, elementType, elementNode.span) ??
        Diagnostic.arrayElementTypeMismatch(
          Type.encode(elementType),
          Type.encode(element.type),
          ordinal,
          elementNode.span,
        )
      diagnostics.push(diagnostic)
      compatibility = Object.freeze({
        _tag: 'TypeMismatch',
        expected: elementType,
        actual: element.type,
      })
    } else {
      compatibility = Object.freeze({ _tag: 'Compatible' })
    }
    return Object.freeze({
      _tag: 'ArrayElement',
      ordinal,
      expression: element.fact,
      ...(elementType === undefined ? {} : { expected: elementType }),
      compatibility,
      syntax: elementNode,
    })
  })

  const actualLength = elements.length
  let state: ArrayLiteralState
  if (elementType === undefined && actualLength === 0) {
    const diagnostic = Diagnostic.emptyArrayNeedsContext(node.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'MissingContext' })
  } else if (expectedArray !== undefined && expectedArray.length !== actualLength) {
    const diagnostic = Diagnostic.arrayLengthMismatch(expectedArray.length, actualLength, node.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({
      _tag: 'LengthMismatch',
      expected: expectedArray.length,
      actual: actualLength,
    })
  } else if (elements.some((element) => element.compatibility._tag === 'TypeMismatch')) {
    state = Object.freeze({ _tag: 'IncompatibleElements' })
  } else if (
    elementType === undefined ||
    elements.some((element) => element.compatibility._tag === 'Unavailable') ||
    !SyntaxTree.isAvailableSyntax(node)
  ) {
    state = Object.freeze({ _tag: 'Unavailable' })
  } else {
    state = Object.freeze({
      _tag: 'Complete',
      type: expectedArray ?? Type.fixedArray(elementType, actualLength),
    })
  }
  const type =
    state._tag === 'Complete' ? availableExpressionType(state.type) : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ArrayLiteral',
      elements: Object.freeze(elements),
      ...(expectedArray === undefined ? {} : { expected: expectedArray }),
      ...(elementType === undefined ? {} : { elementType }),
      length: actualLength,
      state,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const analyzeIndexProjection = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const expressions = node.children.filter(isExpressionNode)
  const subjectNode = expressions.at(0)
  const indexNode = expressions.at(1)
  if (subjectNode === undefined || indexNode === undefined) {
    throw new RangeError('Index projection requires subject and index expressions')
  }
  const subject = analyzeExpression(
    source,
    subjectNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  const index = analyzeExpression(
    source,
    indexNode,
    declarations,
    declaration,
    scope,
    resolution,
    'usize',
  )
  if (subject === undefined || index === undefined) {
    throw new RangeError('Cannot analyze index projection operands')
  }
  const diagnostics: Array<Diagnostic.Diagnostic> = [...subject.diagnostics, ...index.diagnostics]
  const array =
    subject.type !== undefined && Type.isFixedArray(subject.type) ? subject.type : undefined
  const slice = subject.type !== undefined && Type.isSlice(subject.type) ? subject.type : undefined
  if (subject.type !== undefined && array === undefined && slice === undefined) {
    diagnostics.push(Diagnostic.indexOnNonArray(Type.encode(subject.type), subjectNode.span))
  }
  if (index.type !== undefined && index.type !== 'usize') {
    diagnostics.push(Diagnostic.indexNotUsize(Type.encode(index.type), indexNode.span))
  }
  let bounds: BoundsFact = Object.freeze({ _tag: 'Unavailable' })
  if (array !== undefined && index.type === 'usize') {
    const literal =
      index.fact._tag === 'Integer' && index.fact.integer._tag === 'Available'
        ? index.fact.integer.value <= BigInt(Number.MAX_SAFE_INTEGER)
          ? Number(index.fact.integer.value)
          : Number.POSITIVE_INFINITY
        : undefined
    if (literal === undefined) bounds = Object.freeze({ _tag: 'Runtime', length: array.length })
    else if (literal < 0 || literal >= array.length) {
      const diagnostic = Diagnostic.indexOutOfBounds(literal, array.length, indexNode.span)
      diagnostics.push(diagnostic)
      bounds = Object.freeze({
        _tag: 'Invalid',
        index: literal,
        length: array.length,
        cause: Diagnostic.identity(diagnostic),
      })
    } else bounds = Object.freeze({ _tag: 'Proven', index: literal, length: array.length })
  } else if (slice !== undefined && index.type === 'usize') {
    bounds = Object.freeze({ _tag: 'RuntimeSlice' })
  }
  const available =
    (array !== undefined || slice !== undefined) &&
    index.type === 'usize' &&
    bounds._tag !== 'Invalid' &&
    bounds._tag !== 'Unavailable' &&
    SyntaxTree.isAvailableSyntax(node)
  const element = array?.element ?? slice?.element
  const type =
    available && element !== undefined
      ? availableExpressionType(element)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'IndexProjection',
      subject: subject.fact,
      index: index.fact,
      ...(array === undefined ? {} : { array, elementType: array.element }),
      ...(slice === undefined
        ? {}
        : { slice, elementType: slice.element, borrowAccess: slice.access }),
      access: 'CopyRead',
      bounds,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const analyzeProjection = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const subjectNode = node.children.find(isExpressionNode)
  if (subjectNode === undefined) throw new RangeError('Projection requires a subject expression')
  const subject = analyzeExpression(
    source,
    subjectNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  if (subject === undefined) throw new RangeError(`Cannot analyze projection ${subjectNode.kind}`)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...subject.diagnostics]
  const fieldToken = directToken(node, 'Identifier')
  const fieldName = fieldToken === undefined ? undefined : spelling(source, fieldToken)
  // A reference projects the fields of its target: the read happens through the borrow, so
  // the projected value is typed by the target while consumption stays a partial-move error.
  const reference =
    subject.type !== undefined &&
    Type.isReference(subject.type) &&
    Type.isNominal(subject.type.target)
      ? subject.type
      : undefined
  const nominal =
    subject.type !== undefined && Type.isNominal(subject.type)
      ? subject.type
      : reference !== undefined && Type.isNominal(reference.target)
        ? reference.target
        : undefined
  const slice = subject.type !== undefined && Type.isSlice(subject.type) ? subject.type : undefined
  const borrowAccess =
    subject.fact._tag === 'IndexProjection' || subject.fact._tag === 'FieldProjection'
      ? subject.fact.borrowAccess
      : undefined
  let state: ProjectionState = Object.freeze({ _tag: 'Unavailable' })
  let type: SemanticType | undefined
  if (slice !== undefined && fieldName === 'length') {
    state = Object.freeze({ _tag: 'SliceLength' })
    type = 'usize'
  } else if (slice !== undefined && fieldName !== undefined && fieldToken !== undefined) {
    const diagnostic = Diagnostic.unknownProjectedField(
      Type.encode(slice),
      fieldName,
      fieldToken.span,
    )
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
  } else if (subject.type !== undefined && nominal === undefined && fieldToken !== undefined) {
    const diagnostic = Diagnostic.projectionOnNonStruct(Type.encode(subject.type), fieldToken.span)
    diagnostics.push(diagnostic)
    state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
  } else if (nominal !== undefined && fieldName !== undefined && fieldToken !== undefined) {
    const member = DeclarationIndex.byCanonical(resolution.index, {
      _tag: 'CanonicalDeclarationId',
      module: nominal.module,
      name: nominal.name,
    })
    const struct = member?._tag === 'StructDeclaration' ? member : undefined
    const lookup =
      struct === undefined ? undefined : DeclarationIndex.lookupField(struct.fields, fieldName)
    if (lookup?._tag !== 'Resolved') {
      const diagnostic = Diagnostic.unknownProjectedField(
        Type.encode(nominal),
        fieldName,
        fieldToken.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
    } else if (lookup.field.visibility === 'Private' && nominal.module !== source.id) {
      const diagnostic = Diagnostic.inaccessibleProjectedField(
        Type.encode(nominal),
        fieldName,
        fieldToken.span,
      )
      diagnostics.push(diagnostic)
      state = Object.freeze({ _tag: 'Unavailable', cause: Diagnostic.identity(diagnostic) })
    } else if (lookup.field.declaredType._tag === 'Resolved') {
      state = Object.freeze({ _tag: 'Resolved', field: lookup.field })
      const substitution =
        struct === undefined
          ? new Map<string, SemanticType>()
          : (Type.substitution(
              struct.typeParameters.map((parameter) => parameter.type),
              nominal.arguments,
            ) ?? new Map())
      type = Type.substitute(lookup.field.declaredType.type, substitution)
    }
  }
  const typeFact = type === undefined ? unavailableExpressionType : availableExpressionType(type)
  const projectionAccess = borrowAccess ?? reference?.access
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FieldProjection',
      subject: subject.fact,
      ...(nominal === undefined ? {} : { nominal }),
      ...(projectionAccess === undefined ? {} : { borrowAccess: projectionAccess }),
      fieldName,
      ...(fieldToken === undefined ? {} : { fieldToken }),
      state,
      type: typeFact,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type,
  })
}

const analyzeArgumentNodes = (
  source: SourceFile.SourceFile,
  site: SyntaxTree.Node,
  nodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expectedTypes: ReadonlyArray<SemanticType | undefined> = Object.freeze([]),
): ArgumentsResult => {
  const analyzed = nodes.flatMap((element, ordinal): ReadonlyArray<ExpressionResult> => {
    const result = analyzeExpression(
      source,
      element,
      declarations,
      declaration,
      scope,
      resolution,
      expectedTypes.at(ordinal),
      true,
    )
    return result === undefined ? [] : [result]
  })
  const facts = analyzed.map((result, ordinal) =>
    argumentFact(declaration, site.span, result.fact, ordinal),
  )

  return Object.freeze({
    facts: Object.freeze(facts),
    diagnostics: Object.freeze(analyzed.flatMap((result) => result.diagnostics)),
  })
}

function analyzeArguments(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  callTypeArguments?: CallTypeArgumentsResult,
): ArgumentsResult {
  const argumentList = childNode(call, 'ArgumentList')
  const argumentNodes = argumentList.children.filter(isRecursiveArgumentNode)
  const identifiers = callReferenceTokens(call)
  const first = identifiers.at(0)
  const second = identifiers.at(1)
  let target: SourceCallable | undefined
  let builtinParameters: ReadonlyArray<SemanticType> = Object.freeze([])
  let builtinTypeParameters: ReadonlyArray<Type.Parameter> = Object.freeze([])
  let boundParameters: ReadonlyArray<SemanticType> = Object.freeze([])
  if (first !== undefined && second === undefined) {
    const name = spelling(source, first)
    const resolved = NameResolution.lookup(resolution.scope, resolution.index, name)
    const local = lookupDeclaration(declarations, name)
    target =
      resolved._tag === 'Resolved' && resolved.declaration._tag === 'FunctionDeclaration'
        ? resolved.declaration
        : local._tag === 'Resolved'
          ? local.declaration
          : undefined
  } else if (first !== undefined && second !== undefined) {
    const qualifierSpelling = spelling(source, first)
    const memberSpelling = spelling(source, second)
    const qualifier = NameResolution.lookup(resolution.scope, resolution.index, qualifierSpelling)
    if (qualifier._tag === 'Intrinsic') {
      const library =
        qualifierSpelling === 'Effect'
          ? DeclarationIndex.lookup(resolution.index, 'silk/effect', memberSpelling)
          : undefined
      if (
        library?._tag === 'Resolved' &&
        library.declaration._tag === 'FunctionDeclaration' &&
        library.declaration.visibility === 'Public'
      ) {
        target = library.declaration
      } else {
        const builtin = builtinSignature(qualifierSpelling, memberSpelling)
        const intrinsic = Intrinsic.findOperation(qualifierSpelling, memberSpelling)
        const contract =
          intrinsic?.rule._tag === 'ContractRule' ? intrinsic.rule.contract : undefined
        builtinParameters =
          builtin?.parameters ?? contract?.parameters.map((parameter) => parameter.type) ?? []
        builtinTypeParameters = builtin?.typeParameters ?? contract?.binders ?? []
      }
    } else if (qualifier._tag === 'Namespace') {
      const member = DeclarationIndex.lookup(resolution.index, qualifier.module, memberSpelling)
      target =
        member._tag === 'Resolved' && member.declaration._tag === 'FunctionDeclaration'
          ? member.declaration
          : undefined
    } else if (
      qualifier._tag === 'Resolved' &&
      qualifier.declaration._tag === 'ServiceDeclaration'
    ) {
      target = serviceOperation(qualifier.declaration, memberSpelling)
    } else if (
      qualifier._tag === 'Resolved' &&
      qualifier.declaration._tag === 'InterfaceDeclaration'
    ) {
      const memberToken = second
      const bound = boundOperationReference(
        declaration,
        qualifier.declaration,
        qualifierSpelling,
        memberSpelling,
        memberToken,
      )
      if (bound?._tag === 'BoundOperation') boundParameters = bound.reference.parameters
      else if (qualifier.declaration.canonical._tag === 'Canonical') {
        const member = DeclarationIndex.lookup(
          resolution.index,
          qualifier.declaration.canonical.id.module,
          memberSpelling,
        )
        target =
          member._tag === 'Resolved' &&
          member.declaration._tag === 'FunctionDeclaration' &&
          member.declaration.visibility === 'Public'
            ? member.declaration
            : undefined
      }
    } else if (
      qualifier._tag === 'Resolved' &&
      (qualifier.declaration._tag === 'StructDeclaration' ||
        qualifier.declaration._tag === 'InterfaceDeclaration') &&
      qualifier.declaration.canonical._tag === 'Canonical'
    ) {
      // A nominal type doubles as an actor: `Vector.length(...)` names a public function of the
      // module that declares `Vector`. The call itself already resolves that way, but arguments are
      // analyzed first, and without the same lookup they get no expected types — which reads to a
      // borrow argument as "no borrow is wanted here" and rejects it as an invalid borrow position.
      const member = DeclarationIndex.lookup(
        resolution.index,
        qualifier.declaration.canonical.id.module,
        memberSpelling,
      )
      target =
        member._tag === 'Resolved' &&
        member.declaration._tag === 'FunctionDeclaration' &&
        member.declaration.visibility === 'Public'
          ? member.declaration
          : undefined
    }
  }
  const declaredTypeParameters =
    target?.typeParameters.map((parameter) => parameter.type) ?? Object.freeze([])
  const explicitTypes = callTypeArguments?.types
  const builtinSubstitution =
    callTypeArguments?.explicit === true &&
    explicitTypes !== undefined &&
    explicitTypes.length <= builtinTypeParameters.length
      ? Type.prefixSubstitution(builtinTypeParameters, explicitTypes)
      : undefined
  // An explicit prefix is context for the value arguments just as a complete list is: the
  // parameters it binds become concrete expected types, and the ones it leaves open stay symbolic
  // exactly as they are when nothing was written.
  const substitution =
    callTypeArguments?.explicit === true && explicitTypes !== undefined
      ? Type.prefixSubstitution(declaredTypeParameters, explicitTypes)
      : undefined
  const expectedTypes = Object.freeze(
    boundParameters.length > 0
      ? boundParameters
      : builtinParameters.length > 0
        ? builtinParameters
            .slice(
              isSectionArity(builtinParameters.length, argumentNodes.length)
                ? builtinParameters.length - argumentNodes.length
                : 0,
            )
            .map((parameter) => Type.substitute(parameter, builtinSubstitution ?? new Map()))
        : (target?.parameters ?? [])
            .slice(
              target !== undefined && isSectionArity(target.parameters.length, argumentNodes.length)
                ? target.parameters.length - argumentNodes.length
                : 0,
            )
            .map((parameter) =>
              parameter.declaredType._tag === 'Resolved'
                ? Type.substitute(parameter.declaredType.type, substitution ?? new Map())
                : undefined,
            ),
  )
  return analyzeArgumentNodes(
    source,
    call,
    argumentNodes,
    declarations,
    declaration,
    scope,
    resolution,
    expectedTypes,
  )
}

interface CallContractResult {
  readonly mappings: ReadonlyArray<ArgumentMappingFact>
  readonly fact: CallContractFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

interface CallTypeArgumentsResult {
  readonly explicit: boolean
  readonly facts: ReadonlyArray<TypeArgumentFact>
  readonly types?: ReadonlyArray<SemanticType>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const analyzeCallTypeArguments = (
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  caller: DeclarationFact,
  resolution: ResolutionContext,
): CallTypeArgumentsResult => {
  const list = SyntaxTree.directNode(call, 'CallTypeArgumentList')
  if (list === undefined) {
    return Object.freeze({
      explicit: false,
      facts: Object.freeze([]),
      diagnostics: Object.freeze([]),
    })
  }
  const environment = new Map(
    caller.typeParameters.flatMap((parameter) =>
      parameter.name._tag === 'Present' ? [[parameter.name.spelling, parameter.type] as const] : [],
    ),
  )
  const nameResolution: NameResolution.Resolution = Object.freeze({
    _tag: 'NameResolution',
    modules: Object.freeze([resolution.scope]),
    diagnostics: Object.freeze([]),
  })
  const nodes = list.children.filter(
    (element): element is SyntaxTree.Node =>
      SyntaxTree.isNode(element) &&
      (element.kind === 'RequirementSelector' ||
        element.kind === 'TypePath' ||
        element.kind === 'AppliedType' ||
        element.kind === 'FixedArrayType' ||
        element.kind === 'SliceType' ||
        element.kind === 'ReferenceType' ||
        element.kind === 'CallableType' ||
        element.kind === 'ParenthesizedType' ||
        element.kind === 'UnionType'),
  )
  const analyzed = nodes.map((node, ordinal) => {
    const selectorNodes =
      node.kind === 'RequirementSelector'
        ? node.children.filter(SyntaxTree.isNode)
        : Object.freeze<ReadonlyArray<SyntaxTree.Node>>([])
    const argumentNode = selectorNodes.at(0) ?? node
    const roleNode = selectorNodes.at(1)
    const directToken =
      argumentNode.kind === 'TypePath'
        ? SyntaxTree.tokens(argumentNode).find((token) => token.kind === 'Identifier')
        : undefined
    const directParameter =
      directToken === undefined ? undefined : environment.get(spelling(source, directToken))
    if (
      directToken !== undefined &&
      directParameter !== undefined &&
      directParameter.kind !== 'Value'
    )
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'TypeArgument' as const,
          ordinal,
          syntax: node,
          declared: Object.freeze({
            _tag: 'Resolved' as const,
            type: directParameter,
            spelling: directParameter.name,
            token: directToken,
            syntax: node,
          }),
          type: directParameter,
        }),
        diagnostics: Object.freeze([]),
      })
    const roleSegments =
      roleNode?.kind === 'TypePath'
        ? SyntaxTree.tokens(roleNode)
            .filter((token) => token.kind === 'Identifier')
            .map((token) => Object.freeze({ spelling: spelling(source, token), token }))
        : []
    const rolePath =
      roleNode?.kind === 'TypePath' && roleSegments.length > 0
        ? Object.freeze({
            _tag: 'TypePath' as const,
            spelling: roleSegments.map((segment) => segment.spelling).join('.'),
            segments: Object.freeze(roleSegments),
            syntax: roleNode,
          })
        : undefined
    const roleResolution =
      rolePath === undefined
        ? undefined
        : NameResolution.resolveItem(nameResolution, resolution.index, source.id, rolePath)
    const roleDeclaration =
      roleResolution?._tag === 'Resolved' && roleResolution.declaration._tag === 'RoleDeclaration'
        ? roleResolution.declaration
        : undefined
    const requirementRole =
      roleDeclaration?.canonical._tag === 'Canonical'
        ? RequirementRow.declaredRole(
            roleDeclaration.canonical.id.module,
            roleDeclaration.canonical.id.name,
          )
        : undefined
    const roleDiagnostics =
      rolePath === undefined || requirementRole !== undefined
        ? Object.freeze<ReadonlyArray<Diagnostic.Diagnostic>>([])
        : Object.freeze([
            Diagnostic.invalidRequirementType(`role ${rolePath.spelling}`, rolePath.syntax.span),
          ])
    const raw = DeclarationIndex.analyzeDeclaredType(source, argumentNode, environment)
    const resolved = DeclarationIndex.resolveTypeFact(
      resolution.index,
      source.id,
      raw.fact,
      (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
    )
    const invalidBorrow =
      resolved.fact._tag === 'Resolved' &&
      (Type.isReference(resolved.fact.type)
        ? Type.containsPositionRestrictedBorrow(resolved.fact.type.target)
        : Type.containsPositionRestrictedBorrow(resolved.fact.type))
        ? Diagnostic.sliceTypePosition('type argument', node.span)
        : undefined
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'TypeArgument' as const,
        ordinal,
        syntax: node,
        declared: resolved.fact,
        ...(requirementRole === undefined ? {} : { requirementRole }),
        ...(resolved.fact._tag === 'Resolved' &&
        invalidBorrow === undefined &&
        roleDiagnostics.length === 0
          ? { type: resolved.fact.type }
          : {}),
      }),
      diagnostics: Diagnostic.merge(
        raw.diagnostics,
        resolved.diagnostics,
        roleDiagnostics,
        ...(invalidBorrow === undefined ? [] : [[invalidBorrow]]),
      ),
    })
  })
  const facts = Object.freeze(analyzed.map((entry) => entry.fact))
  const available = facts.map((fact) => fact.type)
  return Object.freeze({
    explicit: true,
    facts,
    ...(available.every((type) => type !== undefined)
      ? {
          types: Object.freeze(
            available.filter((type): type is SemanticType => type !== undefined),
          ),
        }
      : {}),
    diagnostics: Diagnostic.merge(...analyzed.map((entry) => entry.diagnostics)),
  })
}

const hasAvailableCallSyntax = (call: SyntaxTree.Node): boolean => {
  const argumentList = childNode(call, 'ArgumentList')
  const callHeadAvailable = call.children.every(
    (element) =>
      (SyntaxTree.isNode(element) && element.kind === 'ArgumentList') || isAvailableSyntax(element),
  )
  const listStructureAvailable = argumentList.children.every(
    (element) => isRecursiveArgumentNode(element) || isAvailableSyntax(element),
  )
  return callHeadAvailable && listStructureAvailable
}

const isSectionArity = (expectedCount: number, actualCount: number): boolean =>
  actualCount > 0 && actualCount < expectedCount

type SourceCallable = DeclarationFact | DeclarationIndex.ServiceOperationFact

const sourceCallable = (reference: CallReferenceFact): SourceCallable | undefined =>
  reference._tag === 'Resolved'
    ? reference.declaration
    : reference._tag === 'ResolvedServiceOperation'
      ? reference.operation
      : undefined

const resolvedCallableContract = (
  reference: CallReferenceFact,
): CallableContract.CallableContract | undefined => {
  if (reference._tag === 'ResolvedIntrinsicContract') return reference.contract
  const callable = sourceCallable(reference)
  return callable === undefined
    ? undefined
    : DeclarationIndex.callableContract(
        callable,
        reference._tag === 'ResolvedServiceOperation'
          ? reference.service.typeParameters
          : Object.freeze([]),
      )
}

const callArityDiagnostic = (
  reference: Extract<
    CallReferenceFact,
    {
      readonly _tag:
        | 'Resolved'
        | 'ResolvedBuiltin'
        | 'ResolvedIntrinsicContract'
        | 'ResolvedServiceOperation'
        | 'ResolvedBoundOperation'
    }
  >,
  expectedCount: number,
  actualCount: number,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic => {
  if (expectedCount === 1 && actualCount === 0)
    return Diagnostic.redundantUnaryEmptyCall(reference.spelling, span)
  return Diagnostic.wrongCallArity(
    reference._tag === 'ResolvedBuiltin'
      ? Object.freeze({
          _tag: 'BuiltinTarget',
          actor: reference.actor,
          operation: reference.operation,
        })
      : reference._tag === 'ResolvedIntrinsicContract'
        ? Object.freeze({
            _tag: 'BuiltinTarget',
            actor: 'Intrinsic',
            operation: reference.intrinsic.spelling,
          })
        : reference._tag === 'ResolvedBoundOperation'
          ? Object.freeze({
              _tag: 'BuiltinTarget',
              actor: reference.capability.name,
              operation: reference.operation,
            })
          : reference._tag === 'Resolved'
            ? reference.declaration.id
            : reference.operation.id,
    expectedCount,
    actualCount,
    span,
  )
}

/** One value argument paired with the parameter type it must determine. */
interface SpecializationSite {
  /** Position of the argument in the call, so a caller can keep one mistake to one report. */
  readonly ordinal: number
  readonly pattern: SemanticType
  readonly actual: SemanticType
  readonly expression: ExpressionFact
}

/** One written type argument the value arguments contradict, reported at what was written. */
interface SpecializationConflict {
  readonly diagnostic: Diagnostic.Diagnostic
  /** The argument that implied the other type, absent when no value argument is involved. */
  readonly ordinal?: number
}

interface SeededSpecialization {
  readonly substitution: Type.Substitution
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly conflicts: ReadonlyArray<SpecializationConflict>
  /**
   * A parameter no explicit argument wrote and no value argument determines. It waits for the
   * ordinary argument checks, because an argument the call got wrong is the better first report.
   */
  readonly unresolved?: Diagnostic.Diagnostic
}

/**
 * Specializes a call from an explicit prefix of its type arguments plus its value arguments. The
 * prefix seeds the substitution and the parameters past it are inferred exactly as they are when
 * nothing was written, so a call annotates only the parameters inference cannot reach.
 *
 * A prefix that names every parameter binds everything and leaves inference nothing to do, which
 * is the same substitution a complete explicit list has always produced.
 *
 * `deferred` names the parameters allowed to stay open because something other than these
 * arguments determines them, which is how a callable section keeps its captured parameter generic.
 */
const seededSpecialization = (
  target: string,
  declared: ReadonlyArray<Type.Parameter>,
  explicit: ReadonlyArray<TypeArgumentFact>,
  sites: ReadonlyArray<SpecializationSite>,
  span: SourceSpan.SourceSpan,
  deferred: ReadonlySet<string> = new Set(),
): SeededSpecialization => {
  const written = new Map<string, TypeArgumentFact>()
  const seeded = new Map<string, Type.GenericArgument>()
  const conflicts: Array<SpecializationConflict> = []
  for (const fact of explicit) {
    const parameter = declared.at(fact.ordinal)
    const writtenType = fact.type
    if (parameter === undefined || writtenType === undefined) continue
    const argument: Type.GenericArgument | undefined =
      parameter.kind === 'Value' && Type.isTypeArgument(writtenType)
        ? writtenType
        : parameter.kind === 'RequirementRow'
          ? Type.isParameter(writtenType) && writtenType.kind === 'RequirementRow'
            ? Type.requirementRowArgument([], [writtenType])
            : Type.isNominal(writtenType) ||
                (Type.isParameter(writtenType) && writtenType.kind === 'Value')
              ? Type.isParameter(writtenType)
                ? Type.requirementRowArgumentFromRow(
                    RowAlgebra.singleton(
                      Type.requirementRowPolicy(),
                      Type.requirementMemberShape(
                        writtenType,
                        'Shared',
                        fact.requirementRole ?? RequirementRow.defaultRole,
                      ),
                      fact.syntax.span,
                    ),
                  )
                : Type.requirementRowArgument([
                    Object.freeze({
                      capability: writtenType,
                      role: fact.requirementRole ?? RequirementRow.defaultRole,
                      access: 'Shared',
                    }),
                  ])
              : undefined
          : undefined
    if (argument === undefined) {
      conflicts.push(
        Object.freeze({
          diagnostic: Diagnostic.genericParameterKindMismatch(
            parameter.name,
            parameter.kind,
            Type.isNominal(writtenType) ? 'RequirementRow' : 'Value',
            fact.syntax.span,
          ),
        }),
      )
      continue
    }
    seeded.set(Type.key(parameter), argument)
    written.set(Type.key(parameter), fact)
  }
  const inferred = new Map(seeded)
  let rowFailure: Type.RowInferenceFailure | undefined
  for (const site of sites) {
    const attempt = new Map(inferred)
    if (Type.infer(site.pattern, site.actual, attempt)) {
      commitSpecialization(inferred, attempt)
      continue
    }
    // Inference under the prefix failed. When the argument still satisfies what the prefix says
    // this parameter is, the written type simply wins — that is how a widened literal keeps
    // working under `take<u8>(1)`.
    const expected = Type.substitute(site.pattern, inferred)
    if (
      typesCompatible(site.actual, expected) ||
      contextualIntegerCompatible(site.expression, expected)
    )
      continue
    rowFailure ??= Type.rowInferenceFailure(site.pattern, site.actual)
    const implied = new Map<string, Type.GenericArgument>()
    // Only what this argument alone implies can contradict the prefix; an argument that does not
    // unify at all is an ordinary argument mismatch and belongs to the argument pass.
    if (!Type.infer(site.pattern, site.actual, implied)) continue
    for (const [identity, fact] of written) {
      const suppliedArgument = implied.get(identity)
      const explicitArgument = seeded.get(identity)
      if (suppliedArgument === undefined || explicitArgument === undefined) continue
      if (Type.genericArgumentKey(suppliedArgument) === Type.genericArgumentKey(explicitArgument))
        continue
      conflicts.push(
        Object.freeze({
          ordinal: site.ordinal,
          diagnostic: Diagnostic.typeArgumentConflict(
            target,
            declared.at(fact.ordinal)?.name ?? fact.ordinal.toString(),
            Type.encodeGenericArgument(explicitArgument),
            Type.encodeGenericArgument(suppliedArgument),
            fact.syntax.span,
          ),
        }),
      )
    }
  }
  const open = declared.find(
    (parameter) => !inferred.has(Type.key(parameter)) && !deferred.has(Type.key(parameter)),
  )
  const typeArguments = Object.freeze(
    declared.flatMap((parameter) => {
      const argument = inferred.get(Type.key(parameter))
      return argument === undefined ? [] : [argument]
    }),
  )
  return Object.freeze({
    substitution: inferred,
    typeArguments,
    conflicts: Object.freeze(conflicts),
    ...(open === undefined || conflicts.length > 0
      ? {}
      : {
          unresolved:
            rowFailure === undefined
              ? Diagnostic.uninferredTypeParameter(target, open.name, span)
              : Diagnostic.contractRowInference(rowFailure, span),
        }),
  })
}

const commitSpecialization = (
  target: Map<string, Type.GenericArgument>,
  source: ReadonlyMap<string, Type.GenericArgument>,
): void => {
  target.clear()
  for (const [identity, argument] of source) target.set(identity, argument)
}

const contractSpecializationSites = (
  arguments_: ReadonlyArray<ArgumentFact>,
  contract: CallableContract.CallableContract,
): ReadonlyArray<SpecializationSite> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal): ReadonlyArray<SpecializationSite> => {
      const parameter = contract.parameters.at(ordinal)
      return argument.type._tag === 'Available' && parameter !== undefined
        ? [
            Object.freeze({
              ordinal,
              pattern: parameter.type,
              actual: argument.type.type,
              expression: argument.expression,
            }),
          ]
        : []
    }),
  )

interface ConstraintSolveResult {
  readonly substitution: Type.Substitution
  readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const constraintOrigins = (
  callable: SourceCallable | undefined,
): ReadonlyArray<SourceSpan.SourceSpan> =>
  Object.freeze(callable?.constraints.map((constraint) => constraint.syntax.span) ?? [])

/** Solves provider relations only after arguments have independently established their operands. */
const solveCallableConstraints = (
  constraints: ReadonlyArray<Constraint.Constraint>,
  origins: ReadonlyArray<SourceSpan.SourceSpan>,
  initial: Type.Substitution,
  caller: DeclarationFact | undefined,
  resolution: ResolutionContext,
  span: SourceSpan.SourceSpan,
): ConstraintSolveResult => {
  const substitution = new Map(initial)
  const evidence: Array<Constraint.ConstraintEvidence> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const givens = caller?.constraintContracts ?? Object.freeze([])
  const checked = constraints.flatMap((constraint, ordinal) =>
    constraint._tag === 'ProviderSelectionConstraint'
      ? []
      : [Object.freeze({ constraint, ordinal })],
  )
  for (const entry of checked) {
    const wanted = Constraint.substitute(entry.constraint, substitution)
    if (givens.some((given) => Constraint.key(given) === Constraint.key(wanted))) {
      evidence.push(Constraint.assumed(wanted, substitution))
      continue
    }
    if (wanted._tag === 'ProviderSelectionConstraint')
      throw new RangeError('substitution changed a checked constraint into a provider selection')
    const proof = Constraint.proveStructural(wanted)
    if (proof !== undefined) {
      evidence.push(proof)
      continue
    }
    diagnostics.push(
      wanted._tag === 'RequirementSubsetConstraint'
        ? Diagnostic.invalidEffectProvision(
            'selected requirement row is not an exact subset of the source row',
            span,
          )
        : Diagnostic.invalidEffectHandler(
            wanted._tag === 'NominalMemberConstraint'
              ? 'selected failure is absent or remains underconstrained'
              : 'selected failure type is not an exact subset of the source failure type',
            span,
          ),
    )
  }
  const providers = constraints.flatMap((constraint, ordinal) =>
    constraint._tag === 'ProviderSelectionConstraint'
      ? [Object.freeze({ constraint, ordinal })]
      : [],
  )
  const grouped = new Map<string, ReadonlyArray<(typeof providers)[number]>>()
  for (const provider of providers) {
    const selected = provider.constraint.selected.expression
    const groupKey =
      selected._tag === 'RowParameter'
        ? Type.key(selected.parameter)
        : Constraint.key(provider.constraint)
    grouped.set(groupKey, Object.freeze([...(grouped.get(groupKey) ?? []), provider]))
  }
  for (const [selectedKey, group] of grouped) {
    const wanted = group.map(({ constraint }) => Constraint.substitute(constraint, substitution))
    const assumed = wanted.every((constraint) =>
      givens.some((given) => Constraint.key(given) === Constraint.key(constraint)),
    )
    if (assumed) {
      for (const constraint of wanted) evidence.push(Constraint.assumed(constraint, substitution))
      continue
    }
    const selectedArgument = substitution.get(selectedKey)
    const selected =
      selectedArgument !== undefined && Type.isRequirementRowArgument(selectedArgument)
        ? selectedArgument.row
        : undefined
    const relations = wanted.flatMap((constraint, ordinal) =>
      constraint._tag === 'ProviderSelectionConstraint'
        ? [
            Object.freeze<ProviderSelection.Relation>({
              wanted: constraint,
              origins: [origins.at(group.at(ordinal)?.ordinal ?? 0) ?? span],
            }),
          ]
        : [],
    )
    const solved = ProviderSelection.solve({
      relations,
      ...(selected === undefined ? {} : { selected }),
      responsible: span,
      oracle: Object.freeze({
        match: (provider: Type.Type, capability: Type.Nominal) =>
          DeclarationIndex.providerMatch(resolution.index, provider, capability),
      }),
    })
    if (solved._tag === 'Rejected') {
      diagnostics.push(...solved.diagnostics.map(Diagnostic.providerSelection))
      continue
    }
    if (selectedArgument === undefined) {
      const parameter = group.at(0)?.constraint.selected.expression
      if (parameter?._tag === 'RowParameter')
        substitution.set(
          Type.key(parameter.parameter),
          Type.requirementRowArgument([solved.member]),
        )
    }
    for (const selectedEvidence of solved.evidence) {
      const solvedWanted = wanted.find(
        (candidate) => Constraint.key(candidate) === selectedEvidence.wantedKey,
      )
      const specialized =
        solvedWanted === undefined ? undefined : Constraint.substitute(solvedWanted, substitution)
      if (specialized?._tag === 'ProviderSelectionConstraint')
        evidence.push(
          Constraint.requirementSelectionEvidence(
            specialized,
            solved.member,
            selectedEvidence.providerMatch,
          ),
        )
    }
  }
  return Object.freeze({
    substitution,
    evidence: Object.freeze(evidence),
    diagnostics: Object.freeze(diagnostics),
  })
}

const analyzeCallContract = (
  call: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
  syntaxAvailable = hasAvailableCallSyntax(call),
  callTypeArguments?: CallTypeArgumentsResult,
  resolution?: ResolutionContext,
  caller?: DeclarationFact,
): CallContractResult => {
  if (!syntaxAvailable) {
    return Object.freeze({
      mappings: Object.freeze([]),
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
      }),
      diagnostics: Object.freeze([]),
    })
  }
  // A bound operation's contract is a fixed parameter and result list over the bounded parameter,
  // exactly like a compiler-known operation's, so both are checked the same way.
  if (reference._tag === 'ResolvedBuiltin' || reference._tag === 'ResolvedBoundOperation') {
    const unavailableArgument = argumentsList.find((argument) => argument.type._tag !== 'Available')
    if (unavailableArgument !== undefined) {
      return Object.freeze({
        mappings: Object.freeze([]),
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'UnavailableBuiltinArgument',
            argument: unavailableArgument,
          }),
        }),
        diagnostics: Object.freeze([]),
        type: undefined,
      })
    }
    for (const [ordinal, argument] of argumentsList.entries()) {
      const expected = reference.parameters.at(ordinal)
      if (
        expected !== undefined &&
        argument.type._tag === 'Available' &&
        !typesCompatible(argument.type.type, expected)
      ) {
        const mismatch =
          Type.isCallable(expected) && Type.isCallable(argument.type.type)
            ? Diagnostic.incompatibleCallableSignature(
                Type.encode(expected),
                Type.encode(argument.type.type),
                argument.syntax.span,
              )
            : (unionConversionDiagnostic(argument.type.type, expected, argument.syntax.span) ??
              Diagnostic.argumentTypeMismatch(
                Type.encode(expected),
                Type.encode(argument.type.type),
                argument.syntax.span,
              ))
        return Object.freeze({
          mappings: Object.freeze([]),
          fact: Object.freeze({
            _tag: 'Unavailable',
            reason: Object.freeze({ _tag: 'ArgumentTypeMismatch', argument, expected }),
            cause: Diagnostic.identity(mismatch),
          }),
          diagnostics: Object.freeze([mismatch]),
        })
      }
    }
    const expectedCount = reference.parameters.length
    const actualCount = argumentsList.length
    if (expectedCount !== actualCount) {
      return Object.freeze({
        mappings: Object.freeze([]),
        fact: Object.freeze({ _tag: 'ArityMismatch', expectedCount, actualCount }),
        diagnostics: Object.freeze([
          callArityDiagnostic(reference, expectedCount, actualCount, call.span),
        ]),
      })
    }
    return Object.freeze({
      mappings: Object.freeze([]),
      fact: Object.freeze({
        _tag: 'Compatible',
        expectedCount,
        actualCount,
        typeArguments: Object.freeze([]),
        substitution: new Map(),
        evidence: Object.freeze([]),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  if (
    reference._tag !== 'Resolved' &&
    reference._tag !== 'ResolvedServiceOperation' &&
    reference._tag !== 'ResolvedIntrinsicContract'
  ) {
    const cause =
      reference._tag === 'Missing' || reference._tag === 'Ambiguous' ? reference.cause : undefined
    return Object.freeze({
      mappings: Object.freeze([]),
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallTarget', reference }),
        ...(cause === undefined ? {} : { cause }),
      }),
      diagnostics: Object.freeze([]),
    })
  }

  const callable = sourceCallable(reference)
  const contract = resolvedCallableContract(reference)
  if (contract === undefined) throw new RangeError('resolved call lost its callable contract')
  const parameters = callable?.parameters ?? Object.freeze([])
  const mappings = Object.freeze(
    argumentsList.flatMap((argument, ordinal): ReadonlyArray<ArgumentMappingFact> => {
      const parameter = parameters.at(ordinal)
      return parameter === undefined
        ? []
        : [Object.freeze({ _tag: 'ArgumentMapping', argument, parameter })]
    }),
  )
  const unavailableArgument = argumentsList.find((argument) => argument.type._tag !== 'Available')
  const unavailableMapping = mappings.find(
    (mapping) => mapping.parameter.declaredType._tag !== 'Resolved',
  )
  if (unavailableArgument !== undefined) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'UnavailableBuiltinArgument' as const,
          argument: unavailableArgument,
        }),
      }),
      diagnostics: Object.freeze([]),
    })
  }
  if (unavailableMapping !== undefined)
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'UnavailableMappedType' as const,
          mapping: unavailableMapping,
        }),
      }),
      diagnostics: Object.freeze([]),
    })
  const sites = contractSpecializationSites(argumentsList, contract)
  const implicitDecay = sites.find(
    (site) => Type.isFixedArray(site.actual) && Type.isSlice(site.pattern),
  )
  if (implicitDecay !== undefined && Type.isSlice(implicitDecay.pattern)) {
    const expected = implicitDecay.pattern
    const argument = argumentsList.at(implicitDecay.ordinal)
    if (argument === undefined) throw new RangeError('specialization site lost its argument')
    const diagnostic = Diagnostic.implicitSliceDecay(Type.encode(expected), argument.syntax.span)
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'ArgumentTypeMismatch',
          argument,
          expected,
        }),
        cause: Diagnostic.identity(diagnostic),
      }),
      diagnostics: Object.freeze([diagnostic]),
    })
  }
  const declaredTypeParameters = contract.binders
  const constraintDeferred = new Set(
    contract.constraints.flatMap((constraint) =>
      constraint._tag === 'ProviderSelectionConstraint' &&
      constraint.selected.expression._tag === 'RowParameter'
        ? [Type.key(constraint.selected.expression.parameter)]
        : [],
    ),
  )
  let substitution: Type.Substitution
  let typeArguments: ReadonlyArray<Type.GenericArgument>
  let unresolvedSpecialization: Diagnostic.Diagnostic | undefined
  if (callTypeArguments?.explicit === true) {
    // More type arguments than the callable declares is the arity error that remains: fewer is a
    // prefix, and the parameters it leaves open are inferred from the value arguments below.
    if (callTypeArguments.facts.length > declaredTypeParameters.length) {
      const diagnostic = Diagnostic.typeArgumentArity(
        reference.spelling,
        declaredTypeParameters.length,
        callTypeArguments.facts.length,
        call.span,
      )
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostics: Object.freeze([diagnostic]),
      })
    }
    if (callTypeArguments.types === undefined) {
      const unavailable = callTypeArguments.facts.find((fact) => fact.type === undefined)
      const cause =
        unavailable !== undefined && 'cause' in unavailable.declared
          ? unavailable.declared.cause
          : undefined
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          ...(cause === undefined ? {} : { cause }),
        }),
        diagnostics: Object.freeze([]),
        type: undefined,
      })
    }
    const seeded = seededSpecialization(
      reference.spelling,
      declaredTypeParameters,
      callTypeArguments.facts,
      sites,
      call.span,
      constraintDeferred,
    )
    const conflict = seeded.conflicts.at(0)
    if (conflict !== undefined) {
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          cause: Diagnostic.identity(conflict.diagnostic),
        }),
        diagnostics: Object.freeze([conflict.diagnostic]),
      })
    }
    typeArguments = seeded.typeArguments
    substitution = seeded.substitution
    unresolvedSpecialization = seeded.unresolved
  } else if (declaredTypeParameters.length === 0) {
    typeArguments = Object.freeze([])
    substitution = new Map()
  } else {
    const inferred = new Map<string, Type.GenericArgument>()
    let compatible = true
    let rowFailure: Type.RowInferenceFailure | undefined
    let pending = [...sites]
    while (pending.length > 0) {
      const deferred: Array<SpecializationSite> = []
      let progressed = false
      for (const site of pending) {
        const pattern = site.pattern
        const supplied = site.actual
        const argument = argumentsList.at(site.ordinal)
        if (argument === undefined) {
          compatible = false
          break
        }
        const representedSupplied =
          Type.isRepresented(pattern) &&
          !Type.isRepresented(supplied) &&
          (Type.isCallable(supplied) || Type.isEffect(supplied))
            ? (() => {
                const representation = representationOfExpression(argument.expression)
                return representation === undefined
                  ? undefined
                  : Type.represented(supplied, pattern.representation.requiredBound, representation)
              })()
            : supplied
        if (representedSupplied === undefined) {
          compatible = false
          rowFailure = Type.rowInferenceFailure(pattern, supplied)
          break
        }
        const attempt = new Map(inferred)
        if (Type.infer(pattern, representedSupplied, attempt)) {
          commitSpecialization(inferred, attempt)
          progressed = true
        } else {
          deferred.push(site)
        }
      }
      if (!compatible) break
      if (deferred.length === 0) break
      if (!progressed) {
        const failed = deferred.at(0)
        rowFailure =
          failed === undefined ? undefined : Type.rowInferenceFailure(failed.pattern, failed.actual)
        compatible = false
        break
      }
      pending = deferred
    }
    typeArguments = Object.freeze(
      declaredTypeParameters.flatMap((parameter) => {
        const inferredType = inferred.get(Type.key(parameter))
        return inferredType === undefined ? [] : [inferredType]
      }),
    )
    const missingFromArguments = declaredTypeParameters.find(
      (parameter) =>
        !inferred.has(Type.key(parameter)) && !constraintDeferred.has(Type.key(parameter)),
    )
    if (!compatible || missingFromArguments !== undefined) {
      const diagnostic =
        rowFailure === undefined
          ? Diagnostic.typeArgumentInference(reference.spelling, call.span)
          : Diagnostic.contractRowInference(rowFailure, call.span)
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          cause: Diagnostic.identity(diagnostic),
        }),
        diagnostics: Object.freeze([diagnostic]),
      })
    }
    substitution = inferred
  }
  let evidence: ReadonlyArray<Constraint.ConstraintEvidence> = Object.freeze([])
  if (resolution !== undefined && contract.constraints.length > 0) {
    const solved = solveCallableConstraints(
      contract.constraints,
      constraintOrigins(callable),
      substitution,
      caller,
      resolution,
      call.span,
    )
    substitution = solved.substitution
    evidence = solved.evidence
    const firstConstraintDiagnostic = solved.diagnostics.at(0)
    if (firstConstraintDiagnostic !== undefined)
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
          cause: Diagnostic.identity(firstConstraintDiagnostic),
        }),
        diagnostics: solved.diagnostics,
      })
    typeArguments = Object.freeze(
      declaredTypeParameters.flatMap((parameter) => {
        const argument = substitution.get(Type.key(parameter))
        return argument === undefined ? [] : [argument]
      }),
    )
  }
  const remainingOpen = declaredTypeParameters.find(
    (parameter) => substitution.get(Type.key(parameter)) === undefined,
  )
  if (remainingOpen !== undefined)
    unresolvedSpecialization ??= Diagnostic.uninferredTypeParameter(
      reference.spelling,
      remainingOpen.name,
      call.span,
    )
  for (const site of sites) {
    const argument = argumentsList.at(site.ordinal)
    if (argument === undefined) continue
    const expected = Type.substitute(site.pattern, substitution)
    const expectedValue = Type.isRepresented(expected) ? expected.contract : expected
    const suppliedValue = Type.isRepresented(site.actual) ? site.actual.contract : site.actual
    if (
      !typesCompatible(suppliedValue, expectedValue) &&
      !contextualIntegerCompatible(argument.expression, expectedValue)
    ) {
      const mismatch =
        Type.isCallable(expectedValue) && Type.isCallable(suppliedValue)
          ? Diagnostic.incompatibleCallableSignature(
              Type.encode(expectedValue),
              Type.encode(suppliedValue),
              argument.syntax.span,
            )
          : Type.isSlice(expectedValue) && Type.isFixedArray(suppliedValue)
            ? Diagnostic.implicitSliceDecay(Type.encode(expectedValue), argument.syntax.span)
            : (unionConversionDiagnostic(suppliedValue, expectedValue, argument.syntax.span) ??
              Diagnostic.argumentTypeMismatch(
                Type.encode(expectedValue),
                Type.encode(suppliedValue),
                argument.syntax.span,
              ))
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'ArgumentTypeMismatch',
            argument,
            expected,
          }),
          cause: Diagnostic.identity(mismatch),
        }),
        diagnostics: Object.freeze([mismatch]),
      })
    }
  }

  const expectedCount = contract.parameters.length
  const actualCount = argumentsList.length
  if (expectedCount !== actualCount) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({ _tag: 'ArityMismatch', expectedCount, actualCount }),
      diagnostics: Object.freeze([
        callArityDiagnostic(reference, expectedCount, actualCount, call.span),
      ]),
    })
  }
  // Every argument the call did supply is sound, so what remains open is genuinely undetermined
  // rather than a consequence of an argument the author already needs to fix.
  if (unresolvedSpecialization !== undefined) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
        cause: Diagnostic.identity(unresolvedSpecialization),
      }),
      diagnostics: Object.freeze([unresolvedSpecialization]),
    })
  }

  return Object.freeze({
    mappings,
    fact: Object.freeze({
      _tag: 'Compatible',
      expectedCount,
      actualCount,
      typeArguments,
      substitution,
      evidence,
    }),
    diagnostics: Object.freeze([]),
  })
}

const interfaceConstraintDiagnostics = (
  reference: CallReferenceFact,
  contract: CallContractResult,
  index: DeclarationIndex.Index,
  caller: DeclarationFact,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  if (reference._tag !== 'Resolved' || contract.fact._tag !== 'Compatible') return Object.freeze([])
  const substitution = contract.fact.substitution
  return Object.freeze(
    reference.declaration.typeParameters.flatMap((parameter) => {
      const provider = substitution.get(Type.key(parameter.type))
      if (provider === undefined || !Type.isTypeArgument(provider)) return []
      return parameter.bounds.flatMap((bound): ReadonlyArray<Diagnostic.Diagnostic> => {
        if (bound._tag !== 'ResolvedBound')
          return [
            Diagnostic.invalidConformance(
              `unknown interface constraint ${bound.spelling}`,
              parameter.syntax.span,
            ),
          ]
        const substitutedCapability = Type.substitute(bound.application.capability, substitution)
        if (!Type.isNominal(substitutedCapability))
          return [
            Diagnostic.invalidConformance(
              `unknown interface constraint ${bound.spelling}`,
              parameter.syntax.span,
            ),
          ]
        const capability = substitutedCapability
        const callerCopyAssumptions = copyAssumptionsOf(caller)
        const assumedByCaller =
          Type.equals(capability, Type.copyCapability) &&
          DeclarationIndex.copyType(index, provider, callerCopyAssumptions)
        if (!bound.application.providerMatches)
          return [
            Diagnostic.invalidConformance(
              `${bound.spelling} cannot bind Self to ${Type.encode(provider)}`,
              parameter.syntax.span,
            ),
          ]
        // Selection excludes rejected declarations, but a partial declaration still carries the most
        // useful source error: name the exact operation it failed to map before reporting the broader
        // missing-witness result.
        const unmapped = DeclarationIndex.unmappedInterfaceOperations(index, provider, capability)
        if (unmapped.length > 0)
          return unmapped.map((operation) =>
            Diagnostic.invalidConformance(
              `${Type.encode(provider)} does not implement ${bound.spelling}.${operation}`,
              span,
            ),
          )
        if (!assumedByCaller && !DeclarationIndex.conforms(index, provider, capability)) {
          // A conditional header that covers this provider but whose own requirements failed has a
          // more useful answer than "does not implement": the chain says which requirement is
          // missing and which wrapper asked for it.
          const proof = DeclarationIndex.prove(index, provider, capability)
          if (
            proof._tag === 'Unproved' &&
            ConformanceGoal.key(proof.goal) !==
              ConformanceGoal.key(ConformanceGoal.make(capability, provider))
          )
            return [
              Diagnostic.unprovenConformance(
                ConformanceGoal.encode(ConformanceGoal.make(capability, provider)),
                ConformanceGoal.describe(proof.failure),
                ConformanceGoal.traceLines(proof),
                span,
              ),
            ]
          return [
            Diagnostic.invalidConformance(
              `${Type.encode(provider)} does not implement ${bound.spelling}`,
              span,
            ),
          ]
        }
        return []
      })
    }),
  )
}

const copyAssumptionsOf = (declaration: DeclarationFact): ReadonlySet<string> =>
  new Set(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.bounds.some(
        (candidate) =>
          candidate._tag === 'ResolvedBound' &&
          Type.equals(candidate.application.capability, Type.copyCapability),
      )
        ? [Type.key(parameter.type)]
        : [],
    ),
  )

interface BuiltinSignature {
  readonly id: Intrinsic.OperationId
  readonly operation: Hir.BuiltinOperation
  readonly typeParameters?: ReadonlyArray<Type.Parameter>
  readonly parameters: ReadonlyArray<SemanticType>
  readonly result: SemanticType
  readonly unsafe?: boolean
  readonly returnedBorrowParameter?: number
}

const builtinSignature = (
  actor: string,
  operation: string,
  parameterKind: 'Call' | 'Primitive' = 'Call',
): BuiltinSignature | undefined => {
  const catalog = Intrinsic.findOperation(actor, operation)
  if (catalog === undefined || !Intrinsic.isBuiltinOperation(catalog)) return undefined
  return Object.freeze({
    id: catalog.id,
    operation: catalog.rule.operation,
    typeParameters: catalog.rule.typeParameters,
    parameters: parameterKind === 'Call' ? catalog.callParameters : catalog.rule.parameters,
    result: catalog.rule.result,
    unsafe: catalog.unsafe,
    ...(catalog.returnedBorrowParameter === undefined
      ? {}
      : { returnedBorrowParameter: catalog.returnedBorrowParameter }),
  })
}

const callableResultType = (declaration: SourceCallable): SemanticType | undefined => {
  if (declaration.returnType._tag !== 'Resolved') return undefined
  if (declaration.functionKind === 'Ordinary') return declaration.returnType.type
  return Type.effectWithRows(
    declaration.returnType.type,
    declaration.failureRow.row,
    'Shared',
    declaration.requirementRow.row,
  )
}

const callableTypeOfReference = (reference: CallReferenceFact): Type.Callable | undefined => {
  if (reference._tag === 'ResolvedBuiltin')
    return Type.callable(
      reference.parameters,
      reference.result,
      'Shared',
      undefined,
      reference.unsafe,
    )
  const callable = sourceCallable(reference)
  if (callable === undefined) return undefined
  const parameters = callable.parameters.flatMap((parameter) =>
    parameter.declaredType._tag === 'Resolved' ? [parameter.declaredType.type] : [],
  )
  const result = callableResultType(callable)
  if (parameters.length !== callable.parameters.length || result === undefined) return undefined
  const contract = resolvedCallableContract(reference)
  return Type.callable(
    parameters,
    result,
    'Shared',
    contract === undefined || contract.constraints.length === 0
      ? undefined
      : Object.freeze({
          contract,
          binders: contract.binders,
          constraints: contract.constraints,
          evidence: Object.freeze([]),
          substitution: new Map(),
          contractKey: CallableContract.key(contract),
          constraintKeys: Object.freeze(contract.constraints.map(Constraint.key)),
          evidenceKeys: Object.freeze([]),
          origins: constraintOrigins(callable),
        }),
    callable.unsafe,
  )
}

const serviceOperation = (
  service: DeclarationIndex.ServiceFact,
  spelling_: string,
): DeclarationIndex.ServiceOperationFact | undefined =>
  service.operations.find(
    (operation) =>
      operation.state._tag === 'Unique' &&
      operation.name._tag === 'Present' &&
      operation.name.spelling === spelling_,
  )

/**
 * The contract one interface operation declares over a bounded parameter.
 *
 * The interface writes its contract over its own type parameter; a bound applies that interface to
 * one parameter of the bounded declaration, so the operation's contract over that parameter is the
 * declared one with the interface's parameter substituted. It is the same contract the conformance
 * check already holds every witness to, which is what lets the body be checked once, over the
 * canonical parameter, before any concrete argument exists.
 */
const interfaceOperationContract = (
  operation: DeclarationIndex.InterfaceOperationApplicationFact,
):
  | {
      readonly declaration: DeclarationIndex.ServiceOperationFact
      readonly contract: DeclarationIndex.InterfaceOperationApplicationFact
      readonly parameters: ReadonlyArray<SemanticType>
      readonly result: SemanticType
    }
  | undefined => {
  if (operation.declaration.typeParameters.length > 0 || operation.success._tag !== 'Resolved')
    return undefined
  const parameters = operation.operands.flatMap((operand) =>
    operand.type._tag === 'Resolved' ? [operand.type.type] : [],
  )
  if (parameters.length !== operation.operands.length) return undefined
  const result =
    operation.functionKind === 'Ordinary'
      ? operation.success.type
      : Type.effectWithRows(
          operation.success.type,
          operation.failureRow.row,
          'Shared',
          operation.requirementRow.row,
        )
  return Object.freeze({
    declaration: operation.declaration,
    contract: operation,
    parameters: Object.freeze(parameters),
    result,
  })
}

/**
 * Resolves one `Bound.operation(...)` receiver against the bounds of the declaration being
 * elaborated.
 *
 * A bound's operation is spelled through the bound's own name, so inside a body bounded by an
 * interface that name selects the bound's operation rather than a same-named public function of the
 * module declaring the interface. The preference is deliberately narrow: only a name the bound's
 * recorded contract actually declares is taken, so every other member of that module keeps
 * resolving exactly where it resolved before, and a body with no such bound is untouched.
 *
 * One declaration may bound two of its parameters by one interface. The receiver then names no
 * single parameter, and the call is reported rather than resolved to either.
 */
const boundOperationReference = (
  declaration: DeclarationFact,
  interface_: DeclarationIndex.InterfaceFact,
  qualifier: string,
  member: string,
  memberToken: Token.Token,
):
  | {
      readonly _tag: 'BoundOperation'
      readonly reference: Extract<CallReferenceFact, { readonly _tag: 'ResolvedBoundOperation' }>
    }
  | { readonly _tag: 'AmbiguousBound'; readonly parameters: ReadonlyArray<string> }
  | undefined => {
  if (interface_.canonical._tag !== 'Canonical') return undefined
  const capability = interface_.canonical.id
  const bounded = declaration.typeParameters.flatMap((parameter) =>
    parameter.bounds.flatMap((bound) =>
      bound._tag === 'ResolvedBound' &&
      bound.application.declaration.module === capability.module &&
      bound.application.declaration.name === capability.name &&
      bound.application.operations.some(
        (operation) =>
          operation.declaration.name._tag === 'Present' &&
          operation.declaration.name.spelling === member,
      )
        ? [Object.freeze({ parameter, bound })]
        : [],
    ),
  )
  if (bounded.length === 0) return undefined
  if (bounded.length > 1)
    return Object.freeze({
      _tag: 'AmbiguousBound',
      parameters: Object.freeze(
        bounded.map(({ parameter }) =>
          parameter.name._tag === 'Present' ? parameter.name.spelling : Type.encode(parameter.type),
        ),
      ),
    })
  const selected = bounded.at(0)
  if (selected === undefined) return undefined
  const { parameter, bound } = selected
  const operation = bound.application.operations.find(
    (candidate) =>
      candidate.declaration.name._tag === 'Present' &&
      candidate.declaration.name.spelling === member,
  )
  if (operation === undefined) return undefined
  const contract = interfaceOperationContract(operation)
  if (contract === undefined) return undefined
  return Object.freeze({
    _tag: 'BoundOperation',
    reference: Object.freeze({
      _tag: 'ResolvedBoundOperation' as const,
      spelling: `${qualifier}.${member}`,
      token: memberToken,
      capability: bound.application.capability,
      provider: parameter.type,
      operation: member,
      declaration: contract.declaration,
      interfaceContract: contract.contract,
      parameters: contract.parameters,
      result: contract.result,
    }),
  })
}

const resolvedFunctionReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  resolution: ResolutionContext,
): CallReferenceFact | undefined => {
  const identifiers = callReferenceTokens(node)
  const first = identifiers.at(0)
  const second = identifiers.at(1)
  if (first === undefined) return undefined
  if (second === undefined) {
    const name = spelling(source, first)
    const resolved = NameResolution.lookup(resolution.scope, resolution.index, name)
    const local = lookupDeclaration(declarations, name)
    const declaration =
      resolved._tag === 'Resolved' && resolved.declaration._tag === 'FunctionDeclaration'
        ? resolved.declaration
        : local._tag === 'Resolved'
          ? local.declaration
          : undefined
    return declaration === undefined
      ? undefined
      : Object.freeze({
          _tag: 'Resolved',
          spelling: name,
          token: first,
          declaration,
        })
  }
  const qualifier = spelling(source, first)
  const member = spelling(source, second)
  const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
  if (qualifierLookup._tag === 'Intrinsic') {
    if (qualifier === 'Effect') {
      const library = DeclarationIndex.lookup(resolution.index, 'silk/effect', member)
      if (
        library._tag === 'Resolved' &&
        library.declaration._tag === 'FunctionDeclaration' &&
        library.declaration.visibility === 'Public'
      )
        return Object.freeze({
          _tag: 'Resolved',
          spelling: `${qualifier}.${member}`,
          token: second,
          declaration: library.declaration,
        })
    }
    const signature = builtinSignature(qualifier, member)
    return signature === undefined
      ? undefined
      : Object.freeze({
          _tag: 'ResolvedBuiltin',
          spelling: `${qualifier}.${member}`,
          token: second,
          actor: qualifier,
          operation: signature.operation,
          intrinsic: signature.id,
          parameters: signature.parameters,
          result: signature.result,
          unsafe: signature.unsafe === true,
          ...(signature.returnedBorrowParameter === undefined
            ? {}
            : { returnedBorrowParameter: signature.returnedBorrowParameter }),
        })
  }
  if (qualifierLookup._tag !== 'Namespace') return undefined
  const memberLookup = DeclarationIndex.lookup(resolution.index, qualifierLookup.module, member)
  if (
    memberLookup._tag !== 'Resolved' ||
    memberLookup.declaration._tag !== 'FunctionDeclaration' ||
    memberLookup.declaration.visibility !== 'Public'
  )
    return undefined
  return Object.freeze({
    _tag: 'Resolved',
    spelling: `${qualifier}.${member}`,
    token: second,
    declaration: memberLookup.declaration,
  })
}

const analyzeFunctionItem = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  resolution: ResolutionContext,
): ExpressionResult | undefined => {
  const reference = resolvedFunctionReference(source, node, declarations, resolution)
  if (reference === undefined) {
    const identifiers = callReferenceTokens(node)
    const qualifierToken = identifiers.at(0)
    const memberToken = identifiers.at(1)
    if (qualifierToken === undefined || memberToken === undefined) return undefined
    const qualifier = spelling(source, qualifierToken)
    const member = spelling(source, memberToken)
    const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
    if (qualifierLookup._tag !== 'Namespace') return undefined
    const memberLookup = DeclarationIndex.lookup(resolution.index, qualifierLookup.module, member)
    const diagnostic =
      memberLookup._tag !== 'Resolved'
        ? Diagnostic.unknownImportedMember(qualifierLookup.module, member, memberToken.span)
        : memberLookup.declaration.visibility !== 'Public'
          ? Diagnostic.inaccessibleImportedMember(qualifierLookup.module, member, memberToken.span)
          : undefined
    if (diagnostic === undefined) return undefined
    const missing: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: `${qualifier}.${member}`,
      token: memberToken,
      cause: Diagnostic.identity(diagnostic),
    })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'FunctionItem',
        reference: missing,
        path: referencePath(node),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([diagnostic]),
      type: undefined,
    })
  }
  const callable = callableTypeOfReference(reference)
  const type =
    callable === undefined ? unavailableExpressionType : availableExpressionType(callable)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionItem',
      reference,
      path: referencePath(node),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([]),
    type: callable,
  })
}

interface SectionContractResult {
  readonly substitution: Type.Substitution
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly valid: boolean
}

/** A section binds its written arguments to the callable's trailing parameter suffix. */
const sectionSpecializationSites = (
  contract: CallableContract.CallableContract,
  arguments_: ReadonlyArray<ArgumentFact>,
): ReadonlyArray<SpecializationSite> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal): ReadonlyArray<SpecializationSite> => {
      const parameter = contract.parameters.at(
        contract.parameters.length - arguments_.length + ordinal,
      )
      return argument.type._tag === 'Available' && parameter !== undefined
        ? [
            Object.freeze({
              ordinal,
              pattern: parameter.type,
              actual: argument.type.type,
              expression: argument.expression,
            }),
          ]
        : []
    }),
  )

const analyzeSectionContract = (
  call: SyntaxTree.Node,
  reference: Extract<
    CallReferenceFact,
    { readonly _tag: 'Resolved' | 'ResolvedBuiltin' | 'ResolvedIntrinsicContract' }
  >,
  arguments_: ReadonlyArray<ArgumentFact>,
  callTypeArguments: CallTypeArgumentsResult,
): SectionContractResult => {
  if (reference._tag === 'ResolvedBuiltin') {
    const captureStart = reference.parameters.length - arguments_.length
    const diagnostics = arguments_.flatMap((argument, ordinal) => {
      if (argument.type._tag !== 'Available') return []
      const expected = reference.parameters.at(captureStart + ordinal)
      if (expected === undefined || typesCompatible(argument.type.type, expected)) return []
      return [
        Diagnostic.argumentTypeMismatch(
          Type.encode(expected),
          Type.encode(argument.type.type),
          argument.syntax.span,
        ),
      ]
    })
    if (callTypeArguments.explicit)
      diagnostics.push(
        Diagnostic.typeArgumentArity(
          reference.spelling,
          0,
          callTypeArguments.facts.length,
          call.span,
        ),
      )
    return Object.freeze({
      substitution: new Map(),
      typeArguments: Object.freeze([]),
      diagnostics: Object.freeze(diagnostics),
      valid:
        diagnostics.length === 0 &&
        arguments_.every((argument) => argument.type._tag === 'Available'),
    })
  }

  const callable = resolvedCallableContract(reference)
  if (callable === undefined) throw new RangeError('section lost its callable contract')
  const declaredParameters = callable.binders
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const contradicted = new Set<number>()
  let substitution = new Map<string, Type.GenericArgument>()
  if (callTypeArguments.explicit) {
    if (
      callTypeArguments.types === undefined ||
      callTypeArguments.facts.length > declaredParameters.length
    ) {
      diagnostics.push(
        Diagnostic.typeArgumentArity(
          reference.spelling,
          declaredParameters.length,
          callTypeArguments.facts.length,
          call.span,
        ),
      )
    } else {
      // A section binds one trailing suffix. Written and inferred type arguments specialize those
      // captures while every remaining leading parameter stays available to later application.
      const remaining = callable.parameters.slice(0, callable.parameters.length - arguments_.length)
      const constraintDeferred = callable.constraints.flatMap((constraint) =>
        constraint._tag === 'ProviderSelectionConstraint' &&
        constraint.selected.expression._tag === 'RowParameter'
          ? [Type.key(constraint.selected.expression.parameter)]
          : [],
      )
      const seeded = seededSpecialization(
        reference.spelling,
        declaredParameters,
        callTypeArguments.facts,
        sectionSpecializationSites(callable, arguments_),
        call.span,
        new Set([
          ...remaining.flatMap((parameter) => Type.parameters(parameter.type).map(Type.key)),
          ...constraintDeferred,
        ]),
      )
      substitution = new Map(seeded.substitution)
      for (const conflict of seeded.conflicts) {
        diagnostics.push(conflict.diagnostic)
        if (conflict.ordinal !== undefined) contradicted.add(conflict.ordinal)
      }
      if (seeded.unresolved !== undefined) diagnostics.push(seeded.unresolved)
    }
  } else {
    for (const [ordinal, argument] of arguments_.entries()) {
      const parameter = callable.parameters.at(
        callable.parameters.length - arguments_.length + ordinal,
      )
      if (
        argument.type._tag === 'Available' &&
        parameter !== undefined &&
        !Type.infer(parameter.type, argument.type.type, substitution)
      ) {
        const rowFailure = Type.rowInferenceFailure(parameter.type, argument.type.type)
        diagnostics.push(
          rowFailure === undefined
            ? Diagnostic.typeArgumentInference(reference.spelling, call.span)
            : Diagnostic.contractRowInference(rowFailure, call.span),
        )
        break
      }
    }
    const remaining = callable.parameters.slice(0, callable.parameters.length - arguments_.length)
    const deferred = new Set([
      ...remaining.flatMap((parameter) => Type.parameters(parameter.type).map(Type.key)),
      ...callable.constraints.flatMap((constraint) =>
        constraint._tag === 'ProviderSelectionConstraint' &&
        constraint.selected.expression._tag === 'RowParameter'
          ? [Type.key(constraint.selected.expression.parameter)]
          : [],
      ),
    ])
    if (
      declaredParameters.some(
        (parameter) => !substitution.has(Type.key(parameter)) && !deferred.has(Type.key(parameter)),
      )
    ) {
      diagnostics.push(Diagnostic.typeArgumentInference(reference.spelling, call.span))
    }
  }
  for (const [ordinal, argument] of arguments_.entries()) {
    const parameter = callable.parameters.at(
      callable.parameters.length - arguments_.length + ordinal,
    )
    if (argument.type._tag !== 'Available' || parameter === undefined) continue
    // An argument already named as contradicting a written type argument is one mistake, and it
    // was reported where the author wrote the type.
    if (contradicted.has(ordinal)) continue
    const expected = Type.substitute(parameter.type, substitution)
    if (!Type.isConcrete(expected) || typesCompatible(argument.type.type, expected)) continue
    diagnostics.push(
      Diagnostic.argumentTypeMismatch(
        Type.encode(expected),
        Type.encode(argument.type.type),
        argument.syntax.span,
      ),
    )
  }
  const typeArguments = Object.freeze(
    declaredParameters.flatMap((parameter) => {
      const inferred = substitution.get(Type.key(parameter))
      return inferred === undefined ? [] : [inferred]
    }),
  )
  return Object.freeze({
    substitution,
    typeArguments,
    diagnostics: Object.freeze(diagnostics),
    valid:
      diagnostics.length === 0 &&
      arguments_.every((argument) => argument.type._tag === 'Available'),
  })
}

const captureAccess = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): CallableCaptureFact['access'] => {
  if (expression._tag === 'Move')
    return expression.subject.type._tag === 'Available' &&
      index !== undefined &&
      DeclarationIndex.copyType(index, expression.subject.type.type, assumptions)
      ? 'Copy'
      : 'Take'
  if (expression._tag === 'Borrow')
    return expression.access === 'Exclusive' ? 'Exclusive' : 'Shared'
  if (expression._tag === 'Grouped') return captureAccess(expression.expression, index, assumptions)
  if (expression.type._tag === 'Available' && Type.isCallable(expression.type.type))
    return expression.type.type.mode === 'Shared' ? 'Copy' : expression.type.type.mode
  if (expression.type._tag === 'Available' && Type.isEffect(expression.type.type))
    return expression.type.type.access === 'Shared' ? 'Copy' : expression.type.type.access
  return 'Copy'
}

const ownedProviderCaptureAccess = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index,
  assumptions: ReadonlySet<string> = new Set(),
): CallableCaptureFact['access'] =>
  expression._tag === 'Move' &&
  expression.subject.type._tag === 'Available' &&
  DeclarationIndex.copyType(index, expression.subject.type.type, assumptions)
    ? 'Copy'
    : captureAccess(expression, index, assumptions)

const concreteCallableIdentity = (expression: ExpressionFact): boolean => {
  if (expression._tag === 'Grouped' || expression._tag === 'Move') {
    return concreteCallableIdentity(
      expression._tag === 'Grouped' ? expression.expression : expression.subject,
    )
  }
  if (expression._tag === 'FunctionItem' || expression._tag === 'CallableSection') return true
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding') {
    return concreteCallableIdentity(expression.reference.binding.initializer)
  }
  return expression._tag === 'Call' && expression.reference._tag === 'Resolved'
}

const callableMode = (captures: ReadonlyArray<CallableCaptureFact>): Type.CallableMode =>
  captures.some((capture) => capture.access === 'Take')
    ? 'Take'
    : captures.some((capture) => capture.access === 'Exclusive')
      ? 'Exclusive'
      : 'Shared'

const sectionCallableType = (
  reference: Extract<
    CallReferenceFact,
    { readonly _tag: 'Resolved' | 'ResolvedBuiltin' | 'ResolvedIntrinsicContract' }
  >,
  substitution: Type.Substitution,
  mode: Type.CallableMode,
  argumentCount: number,
): Type.Callable | undefined => {
  if (reference._tag === 'ResolvedBuiltin') {
    const remaining = reference.parameters.slice(0, reference.parameters.length - argumentCount)
    return remaining.length === 0
      ? undefined
      : Type.callable(remaining, reference.result, mode, undefined, reference.unsafe)
  }
  const contract = resolvedCallableContract(reference)
  const result = contract?.result
  if (contract === undefined || result === undefined) return undefined
  const remaining = contract.parameters.slice(0, contract.parameters.length - argumentCount)
  if (remaining.length === 0) return undefined
  return Type.callable(
    remaining.map((parameter) => Type.substitute(parameter.type, substitution)),
    Type.substitute(result, substitution),
    mode,
    contract.constraints.length === 0
      ? undefined
      : Object.freeze({
          contract,
          binders: contract.binders,
          constraints: contract.constraints,
          evidence: Object.freeze([]),
          substitution,
          contractKey: CallableContract.key(contract),
          constraintKeys: Object.freeze(contract.constraints.map(Constraint.key)),
          evidenceKeys: Object.freeze([]),
          origins: constraintOrigins(sourceCallable(reference)),
        }),
    contract.unsafe,
  )
}

const callableSectionOf = (
  expression: ExpressionFact,
): CallableSectionExpressionFact | undefined => {
  if (expression._tag === 'CallableSection') return expression
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding')
    return callableSectionOf(expression.reference.binding.initializer)
  if (expression._tag === 'Move') return callableSectionOf(expression.subject)
  if (expression._tag === 'Grouped') return callableSectionOf(expression.expression)
  return undefined
}

function executableSite(
  tag: 'CallableSiteId',
  resolution: ResolutionContext,
  node: SyntaxTree.Node,
): Hir.CallableSiteId
function executableSite(
  tag: 'EffectSiteId',
  resolution: ResolutionContext,
  node: SyntaxTree.Node,
): Hir.EffectSiteId
function executableSite(
  tag: 'CallableSiteId' | 'EffectSiteId',
  resolution: ResolutionContext,
  node: SyntaxTree.Node,
): Hir.CallableSiteId | Hir.EffectSiteId {
  const ordinal = resolution.executableSites?.get(node) ?? 0
  return Object.freeze({
    _tag: tag,
    function:
      resolution.executableFunction ??
      Object.freeze({ _tag: 'DeclarationId', sourceId: node.span.sourceId, ordinal: 0 }),
    ...(resolution.executableOwner === undefined ? {} : { owner: resolution.executableOwner }),
    ordinal,
    span: node.span,
  })
}

const executableSites = (root: SyntaxTree.Node): ReadonlyMap<SyntaxTree.Node, number> => {
  const sites = new Map<SyntaxTree.Node, number>()
  const visit = (node: SyntaxTree.Node): void => {
    if (node.kind === 'CallExpression' || node.kind === 'EffectExpression')
      sites.set(node, sites.size)
    for (const child of node.children) if (SyntaxTree.isNode(child)) visit(child)
  }
  visit(root)
  return sites
}

const executableSpecializationOwner = (
  resolution: ResolutionContext,
): Type.ExecutableSpecializationOwner | undefined => {
  const owner = resolution.executableOwner
  if (owner === undefined) return undefined
  const declaration = DeclarationIndex.byCanonical(resolution.index, owner)
  return declaration === undefined
    ? undefined
    : Object.freeze({
        declaration: Object.freeze({ module: owner.module, name: owner.name }),
        typeArguments: Object.freeze(declaration.typeParameters.map((parameter) => parameter.type)),
      })
}

const finishCallableSection = (
  node: SyntaxTree.Node,
  reference: Extract<
    CallReferenceFact,
    { readonly _tag: 'Resolved' | 'ResolvedBuiltin' | 'ResolvedIntrinsicContract' }
  >,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  caller: DeclarationFact,
): ExpressionResult => {
  const contract = analyzeSectionContract(node, reference, argumentsResult.facts, callTypeArguments)
  const parameterCount =
    reference._tag === 'ResolvedBuiltin'
      ? reference.parameters.length
      : (resolvedCallableContract(reference)?.parameters.length ?? 0)
  const captureStart = parameterCount - argumentsResult.facts.length
  const captures = Object.freeze(
    argumentsResult.facts.map((argument, ordinal) =>
      Object.freeze({
        _tag: 'CallableCapture' as const,
        ordinal,
        parameterOrdinal: captureStart + ordinal,
        expression: argument.expression,
        access:
          ordinal === 0 &&
          reference._tag === 'ResolvedIntrinsicContract' &&
          reference.intrinsic.rule._tag === 'ContractRule' &&
          reference.intrinsic.rule.post === 'BindRequirement' &&
          reference.intrinsic.rule.providerMode === 'Take'
            ? ownedProviderCaptureAccess(
                argument.expression,
                resolution.index,
                copyAssumptionsOf(caller),
              )
            : captureAccess(argument.expression, resolution.index, copyAssumptionsOf(caller)),
      }),
    ),
  )
  const mode = callableMode(captures)
  const callable = sectionCallableType(
    reference,
    contract.substitution,
    mode,
    argumentsResult.facts.length,
  )
  const type =
    contract.valid && callable !== undefined
      ? availableExpressionType(callable)
      : unavailableExpressionType
  const environmentOwner = executableSpecializationOwner(resolution)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableSection',
      site: executableSite('CallableSiteId', resolution, node),
      reference,
      path: referencePath(node),
      remainingParameters: Object.freeze(
        Array.from({ length: captureStart }, (_, ordinal) => ordinal),
      ),
      captures,
      retainedDependencies: Object.freeze(
        captures.flatMap((capture) =>
          capture.access === 'Shared' || capture.access === 'Exclusive'
            ? [capture.parameterOrdinal]
            : [],
        ),
      ),
      typeArguments: contract.typeArguments,
      ...(environmentOwner === undefined ? {} : { environmentOwner }),
      substitution: contract.substitution,
      mode,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...contract.diagnostics,
    ]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const finishCallableApplication = (
  node: SyntaxTree.Node,
  callee: ExpressionResult,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  provenance: CallableApplyExpressionFact['provenance'] | undefined = undefined,
  resolution?: ResolutionContext,
  caller?: DeclarationFact,
): ExpressionResult => {
  const callable =
    callee.type !== undefined && Type.isCallable(callee.type)
      ? callee.type
      : callee.type !== undefined &&
          Type.isRepresented(callee.type) &&
          Type.isCallable(callee.type.contract)
        ? callee.type.contract
        : undefined
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...callee.diagnostics,
    ...argumentsResult.diagnostics,
    ...callTypeArguments.diagnostics,
  ]
  const section = callableSectionOf(callee.fact)
  const directSection = callee.fact._tag === 'CallableSection' ? callee.fact : undefined
  const stagedSection =
    directSection !== undefined &&
    callable !== undefined &&
    argumentsResult.facts.length > 0 &&
    argumentsResult.facts.length < callable.parameters.length &&
    resolution !== undefined &&
    caller !== undefined
      ? directSection
      : undefined
  const schema = callable?.schema
  const inferred = new Map<string, Type.GenericArgument>(
    schema?.substitution ?? section?.substitution ?? [],
  )
  let evidence: ReadonlyArray<Constraint.ConstraintEvidence> = Object.freeze([])
  let valid =
    callable !== undefined &&
    (node.kind === 'PipelineExpression' ? isAvailableSyntax(node) : hasAvailableCallSyntax(node))
  if (callable === undefined && callee.type !== undefined) {
    diagnostics.push(
      Diagnostic.nonCallableApplication(Type.encode(callee.type), callee.fact.syntax.span),
    )
  }
  if (
    callable?.mode === 'Exclusive' &&
    callee.fact._tag === 'Identifier' &&
    callee.fact.reference._tag === 'ResolvedBinding' &&
    callee.fact.reference.binding.mutability !== 'Mutable'
  ) {
    diagnostics.push(
      Diagnostic.invalidCallableInvocationAccess('Exclusive', callee.fact.syntax.span),
    )
    valid = false
  }
  if (schema !== undefined && !concreteCallableIdentity(callee.fact)) {
    diagnostics.push(Diagnostic.nonConcreteSpecialization('constrained callable', node.span))
    valid = false
  }
  if (callTypeArguments.explicit) {
    diagnostics.push(
      Diagnostic.typeArgumentArity('callable value', 0, callTypeArguments.facts.length, node.span),
    )
    valid = false
  }
  if (
    callable !== undefined &&
    callable.parameters.length !== argumentsResult.facts.length &&
    stagedSection === undefined
  ) {
    diagnostics.push(
      Diagnostic.wrongCallArity(
        Object.freeze({ _tag: 'BuiltinTarget', actor: 'Callable', operation: 'Apply' }),
        callable.parameters.length,
        argumentsResult.facts.length,
        node.span,
      ),
    )
    valid = false
  }
  const completeUnsafeInvocation =
    callable?.unsafe === true &&
    stagedSection === undefined &&
    callable.parameters.length === argumentsResult.facts.length
  if (completeUnsafeInvocation) {
    const diagnostic = unsafeCallDiagnostic(true, Type.encode(callable), node, resolution)
    if (diagnostic !== undefined) {
      diagnostics.push(diagnostic)
      valid = false
    }
  }
  if (callable !== undefined) {
    const parameterOffset =
      stagedSection === undefined ? 0 : callable.parameters.length - argumentsResult.facts.length
    for (const [ordinal, argument] of argumentsResult.facts.entries()) {
      const expected = callable.parameters.at(parameterOffset + ordinal)
      if (expected === undefined || argument.type._tag !== 'Available') {
        valid = false
        continue
      }
      if (!Type.infer(expected, argument.type.type, inferred)) {
        const rowFailure = Type.rowInferenceFailure(expected, argument.type.type)
        diagnostics.push(
          rowFailure !== undefined
            ? Diagnostic.contractRowInference(rowFailure, argument.syntax.span)
            : Type.isCallable(expected) && Type.isCallable(argument.type.type)
              ? Diagnostic.incompatibleCallableSignature(
                  Type.encode(expected),
                  Type.encode(argument.type.type),
                  argument.syntax.span,
                )
              : Diagnostic.argumentTypeMismatch(
                  Type.encode(expected),
                  Type.encode(argument.type.type),
                  argument.syntax.span,
                ),
        )
        valid = false
        continue
      }
      const specialized = Type.substitute(expected, inferred)
      if (Type.isConcrete(specialized) && !typesCompatible(argument.type.type, specialized)) {
        diagnostics.push(
          Type.isCallable(specialized) && Type.isCallable(argument.type.type)
            ? Diagnostic.incompatibleCallableSignature(
                Type.encode(specialized),
                Type.encode(argument.type.type),
                argument.syntax.span,
              )
            : Diagnostic.argumentTypeMismatch(
                Type.encode(specialized),
                Type.encode(argument.type.type),
                argument.syntax.span,
              ),
        )
        valid = false
      }
    }
  }
  const stagedCaptures =
    stagedSection === undefined || resolution === undefined || caller === undefined
      ? undefined
      : Object.freeze([
          ...stagedSection.captures,
          ...argumentsResult.facts.map((argument, ordinal) => {
            const remainingOffset =
              stagedSection.remainingParameters.length - argumentsResult.facts.length
            const parameterOrdinal = stagedSection.remainingParameters.at(remainingOffset + ordinal)
            if (parameterOrdinal === undefined)
              throw new RangeError('staged callable section lost a remaining parameter')
            return Object.freeze({
              _tag: 'CallableCapture' as const,
              ordinal: stagedSection.captures.length + ordinal,
              parameterOrdinal,
              expression: argument.expression,
              access: captureAccess(
                argument.expression,
                resolution.index,
                copyAssumptionsOf(caller),
              ),
            })
          }),
        ])
  if (
    valid &&
    (schema !== undefined || section !== undefined) &&
    resolution !== undefined &&
    (schema?.constraints.length ??
      (section === undefined
        ? 0
        : resolvedCallableContract(section.reference)?.constraints.length) ??
      0) > 0
  ) {
    const sectionContract =
      schema === undefined && section !== undefined
        ? resolvedCallableContract(section.reference)
        : undefined
    const constraints = schema?.constraints ?? sectionContract?.constraints
    if (constraints === undefined) throw new RangeError('section lost its callable contract')
    const solved = solveCallableConstraints(
      constraints,
      schema?.origins ??
        (section === undefined
          ? Object.freeze([])
          : constraintOrigins(sourceCallable(section.reference))),
      inferred,
      caller,
      resolution,
      node.span,
    )
    inferred.clear()
    for (const [identity, argument] of solved.substitution) inferred.set(identity, argument)
    evidence = Object.freeze([...(schema?.evidence ?? []), ...solved.evidence])
    diagnostics.push(...solved.diagnostics)
    if (solved.diagnostics.length > 0) valid = false
  }
  const type = (() => {
    if (!valid || callable === undefined) return unavailableExpressionType
    if (stagedSection !== undefined && stagedCaptures !== undefined) {
      const reference = stagedSection.reference
      if (
        reference._tag !== 'Resolved' &&
        reference._tag !== 'ResolvedBuiltin' &&
        reference._tag !== 'ResolvedIntrinsicContract'
      )
        return unavailableExpressionType
      const sectionType = sectionCallableType(
        reference,
        inferred,
        callableMode(stagedCaptures),
        stagedCaptures.length,
      )
      return sectionType === undefined
        ? unavailableExpressionType
        : availableExpressionType(sectionType)
    }
    const result = Type.substitute(callable.result, inferred)
    return availableExpressionType(
      Type.isEffect(result)
        ? Type.effectWithRows(
            result.success,
            result.failureRow,
            strongestEffectAccess(
              result.access,
              callable.mode,
              effectExpressionAccess(
                callee.fact,
                resolution?.index,
                caller === undefined ? new Set() : copyAssumptionsOf(caller),
              ),
              effectCaptureAccess(
                argumentsResult.facts,
                resolution?.index,
                caller === undefined ? new Set() : copyAssumptionsOf(caller),
              ),
            ),
            result.requirementRow,
          )
        : result,
    )
  })()
  if (stagedSection !== undefined && stagedCaptures !== undefined && resolution !== undefined) {
    const remainingCount = stagedSection.remainingParameters.length - argumentsResult.facts.length
    const environmentOwner = executableSpecializationOwner(resolution)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'CallableSection',
        site: executableSite('CallableSiteId', resolution, node),
        reference: stagedSection.reference,
        path: stagedSection.path,
        remainingParameters: Object.freeze(
          stagedSection.remainingParameters.slice(0, remainingCount),
        ),
        captures: stagedCaptures,
        retainedDependencies: Object.freeze(
          stagedCaptures.flatMap((capture) =>
            capture.access === 'Shared' || capture.access === 'Exclusive'
              ? [capture.parameterOrdinal]
              : [],
          ),
        ),
        typeArguments: stagedSection.typeArguments,
        ...(environmentOwner === undefined ? {} : { environmentOwner }),
        substitution: inferred,
        mode: callableMode(stagedCaptures),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
  if (section?.reference._tag === 'ResolvedIntrinsicContract') {
    const protected_ = argumentsResult.facts.at(0)
    if (
      section.reference.intrinsic.rule._tag === 'ContractRule' &&
      section.reference.intrinsic.rule.post === 'CatchFailure'
    ) {
      const handlerCapture = section.captures.find((capture) => capture.parameterOrdinal === 1)
      const handler = handlerCapture?.expression
      const wanted = section.reference.contract.constraints
        .map((constraint) => Constraint.substitute(constraint, inferred))
        .find(
          (constraint): constraint is Constraint.FailureSubset =>
            constraint._tag === 'FailureSubsetConstraint',
        )
      const wantedKey = wanted === undefined ? undefined : Constraint.key(wanted)
      const proved =
        wantedKey !== undefined &&
        evidence.some(
          (candidate) =>
            (candidate._tag === 'Assumed' && candidate.wantedKey === wantedKey) ||
            (candidate._tag === 'FailureSubset' &&
              wanted !== undefined &&
              RowAlgebra.equals(Type.failureRowPolicy(), candidate.selected, wanted.selected) &&
              RowAlgebra.equals(Type.failureRowPolicy(), candidate.source, wanted.source)),
        )
      const handlerType = handler?.type._tag === 'Available' ? handler.type.type : undefined
      const handlerEffect =
        handlerType !== undefined &&
        Type.isCallable(handlerType) &&
        Type.isEffect(handlerType.result)
          ? handlerType.result
          : undefined
      const catchAvailable =
        type._tag === 'Available' &&
        protected_ !== undefined &&
        handler !== undefined &&
        wanted !== undefined &&
        proved
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'EffectCatch',
          reference: sectionIntrinsicReference(section),
          protected: protected_?.expression ?? unavailableExpression(node),
          handler: handler ?? unavailableExpression(node),
          ...(wanted === undefined ? {} : { selected: Type.failureType(wanted.selected) }),
          protectedRow: wanted?.source ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
          handlerRow: handlerEffect?.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
          residualRow:
            wanted === undefined
              ? RowAlgebra.concrete(Type.failureRowPolicy(), [])
              : RowAlgebra.without(Type.failureRowPolicy(), wanted.source, wanted.selected),
          evidence,
          type: catchAvailable ? type : unavailableExpressionType,
          syntax: node,
        }),
        diagnostics: Object.freeze(diagnostics),
        type: catchAvailable && type._tag === 'Available' ? type.type : undefined,
      })
    }
    const providerCapture = section.captures.find((capture) => capture.parameterOrdinal === 1)
    const provider =
      providerCapture === undefined
        ? undefined
        : effectBindingProvider(
            section.reference.intrinsic,
            inferred,
            evidence,
            providerCapture.expression,
            providerCapture.expression.syntax.span,
            resolution?.index,
          )
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EffectBindRequirement',
        reference: sectionIntrinsicReference(section),
        protected: protected_?.expression ?? unavailableExpression(node),
        ...(type._tag === 'Available' && provider !== undefined ? { provider } : {}),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableApply',
      callee: callee.fact,
      arguments: argumentsResult.facts,
      mode: callable?.mode ?? 'Shared',
      ...(callable === undefined ? {} : { contract: callable }),
      substitution: inferred,
      provenance: provenance ?? Object.freeze({ _tag: 'DirectCallableApplication' as const }),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

/**
 * `Place.replace(place, value)`: the first argument resolves as a writable place under the same
 * rules as assignment, the second as a value of the place's type, and the whole expression
 * yields the place's previous value. The place stays initialized, so affine owners can leave a
 * struct field behind a reference without a partial move.
 */
const unavailableIdentifierFact = (node: SyntaxTree.Node): ExpressionFact =>
  Object.freeze({
    _tag: 'Identifier',
    reference: Object.freeze({
      _tag: 'Unavailable' as const,
      syntax: unavailableSyntax(node, 'Identifier'),
    }),
    type: unavailableExpressionType,
    syntax: node,
  })

function analyzePlaceReplace(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult {
  const argumentList = SyntaxTree.directNode(call, 'ArgumentList')
  const nodes =
    argumentList === undefined ? [] : argumentList.children.filter(isRecursiveArgumentNode)
  const destinationNode = nodes.at(0)
  const valueNode = nodes.at(1)
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  if (destinationNode === undefined || valueNode === undefined || nodes.length !== 2) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'PlaceReplace' as const,
        reference: intrinsicReference(source, call),
        destination: unavailableIdentifierFact(call),
        value: unavailableIdentifierFact(call),
        compatible: false,
        type: unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([
        Diagnostic.wrongCallArity(
          Object.freeze({ _tag: 'BuiltinTarget', actor: 'Place', operation: 'replace' }),
          2,
          nodes.length,
          call.span,
        ),
      ]),
      type: undefined,
    })
  }
  const destination = analyzeExpression(
    source,
    destinationNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  if (destination === undefined) {
    throw new RangeError(`Semantic analysis cannot analyze ${destinationNode.kind}`)
  }
  diagnostics.push(...destination.diagnostics)
  const value = analyzeExpression(
    source,
    valueNode,
    declarations,
    declaration,
    scope,
    resolution,
    destination.type,
  )
  if (value === undefined) {
    throw new RangeError(`Semantic analysis cannot analyze ${valueNode.kind}`)
  }
  diagnostics.push(...value.diagnostics)
  const root = assignmentRoot(destination.fact)
  if (root === undefined) {
    if (SyntaxTree.isAvailableSyntax(destinationNode) && destination.diagnostics.length === 0) {
      diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
    }
  } else if (root._tag === 'BindingFact' && root.mutability === 'Immutable') {
    diagnostics.push(
      Diagnostic.immutableAssignment(
        root.name._tag === 'Present' ? root.name.spelling : '?',
        destinationNode.span,
      ),
    )
  } else if (
    root._tag === 'ParameterDeclaration' &&
    (root.declaredType._tag !== 'Resolved' ||
      !(
        (Type.isSlice(root.declaredType.type) || Type.isReference(root.declaredType.type)) &&
        root.declaredType.type.access === 'Exclusive'
      ) ||
      (destination.fact._tag !== 'IndexProjection' && destination.fact._tag !== 'FieldProjection'))
  ) {
    diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
  }
  const compatible =
    destination.type !== undefined &&
    value.type !== undefined &&
    typesCompatible(value.type, destination.type)
  if (destination.type !== undefined && value.type !== undefined && !compatible) {
    const expectedOrigin =
      root?._tag === 'BindingFact' ? root.initializer.syntax.span : destinationNode.span
    diagnostics.push(
      representationJoinDiagnostic(
        destination.type,
        value.type,
        expectedOrigin,
        valueNode.span,
        valueNode.span,
      ) ??
        unionConversionDiagnostic(value.type, destination.type, valueNode.span) ??
        Diagnostic.assignmentTypeMismatch(
          Type.encode(destination.type),
          Type.encode(value.type),
          valueNode.span,
        ),
    )
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'PlaceReplace' as const,
      reference: intrinsicReference(source, call),
      destination: destination.fact,
      ...(root === undefined ? {} : { root }),
      value: value.fact,
      compatible,
      type:
        destination.type === undefined
          ? unavailableExpressionType
          : Object.freeze({ _tag: 'Available' as const, type: destination.type }),
      syntax: call,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: destination.type,
  })
}

const directProviderReference = (
  expression: ExpressionFact,
): BindingDeclarationFact | ParameterFact | undefined => {
  if (expression._tag === 'Identifier') {
    if (expression.reference._tag === 'ResolvedBinding') return expression.reference.binding
    if (expression.reference._tag === 'Resolved') return expression.reference.parameter
    return undefined
  }
  if (expression._tag === 'Borrow' || expression._tag === 'Move')
    return directProviderReference(expression.subject)
  if (expression._tag === 'Grouped') return directProviderReference(expression.expression)
  return undefined
}

const selectedRequirementShape = (
  row: Type.RequirementsRow,
): { readonly capability: Type.Nominal | Type.Parameter; readonly role: string } | undefined => {
  const concrete = RowAlgebra.concretize(Type.requirementRowPolicy(), row)
  if (concrete._tag === 'Concrete') {
    const selected = concrete.row.members.at(0)
    return concrete.row.members.length === 1 && selected !== undefined
      ? Object.freeze({ capability: selected.capability, role: selected.role })
      : undefined
  }
  return row.expression._tag === 'Singleton'
    ? Object.freeze({
        capability: row.expression.member.capability,
        role: row.expression.member.role,
      })
    : undefined
}

const intrinsicContractReference = (
  operation: Intrinsic.Operation,
  operationToken: Token.Token,
): Extract<CallReferenceFact, { readonly _tag: 'ResolvedIntrinsicContract' }> => {
  if (operation.rule._tag !== 'ContractRule')
    throw new RangeError('intrinsic contract reference requires a contract operation')
  const contract =
    operation.rule.contract.unsafe === operation.unsafe
      ? operation.rule.contract
      : CallableContract.make({
          functionKind: operation.rule.contract.functionKind,
          unsafe: operation.unsafe,
          binders: operation.rule.contract.binders,
          parameters: operation.rule.contract.parameters,
          result: operation.rule.contract.result,
          constraints: operation.rule.contract.constraints,
          captures: operation.rule.contract.captures,
        })
  return Object.freeze({
    _tag: 'ResolvedIntrinsicContract',
    spelling: `Intrinsic.${operation.spelling}`,
    token: operationToken,
    intrinsic: operation,
    contract,
  })
}

const effectBindingProvider = (
  operation: Intrinsic.Operation,
  substitution: Type.Substitution,
  evidence: ReadonlyArray<Constraint.ConstraintEvidence>,
  provider: ExpressionFact,
  span: SourceSpan.SourceSpan,
  index?: DeclarationIndex.Index,
): EffectRequirementBindingFact | undefined => {
  if (
    operation.rule._tag !== 'ContractRule' ||
    operation.rule.post !== 'BindRequirement' ||
    operation.rule.providerMode === undefined
  )
    return undefined
  const wanted = operation.rule.contract.constraints
    .map((constraint) => Constraint.substitute(constraint, substitution))
    .find(
      (constraint): constraint is Constraint.ProviderSelection =>
        constraint._tag === 'ProviderSelectionConstraint',
    )
  if (wanted === undefined) return undefined
  const providerReference = directProviderReference(provider)
  if (
    providerReference === undefined ||
    !(Type.isNominal(wanted.provider) || Type.isParameter(wanted.provider))
  )
    return undefined
  const wantedKey = Constraint.key(wanted)
  const proof = evidence.find(
    (candidate) =>
      (candidate._tag === 'Assumed' || candidate._tag === 'RequirementSelection') &&
      candidate.wantedKey === wantedKey,
  )
  if (proof === undefined) return undefined
  const selected = selectedRequirementShape(wanted.selected)
  return Object.freeze({
    _tag: 'EffectRequirementBinding',
    reference: providerReference,
    selected: wanted.selected,
    evidence,
    ...(selected === undefined ? {} : { capability: selected.capability }),
    providerType: wanted.provider,
    ...(selected === undefined ? {} : { role: selected.role }),
    selectionAccess: operation.rule.providerMode,
    captureAccess:
      provider._tag === 'Move' &&
      provider.subject.type._tag === 'Available' &&
      index !== undefined &&
      DeclarationIndex.copyType(index, provider.subject.type.type)
        ? 'Copy'
        : captureAccess(provider, index),
    span,
  })
}

const sectionIntrinsicReference = (
  section: CallableSectionExpressionFact,
): IntrinsicReferenceFact => {
  if (section.reference._tag !== 'ResolvedIntrinsicContract')
    return Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: section.syntax })
  const actor = Intrinsic.findActor('Intrinsic')
  const actorToken = section.path._tag === 'ReferencePath' ? section.path.qualifier : undefined
  if (actor === undefined || actorToken === undefined)
    return Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: section.syntax })
  return Object.freeze({
    _tag: 'ResolvedIntrinsicReference',
    actor,
    operation: section.reference.intrinsic,
    actorToken,
    operationToken: section.reference.token,
  })
}

const finishIntrinsicContractCall = (
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  operation: Intrinsic.Operation,
  operationToken: Token.Token,
  argumentsResult: ArgumentsResult,
  typeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  caller: DeclarationFact,
): ExpressionResult => {
  if (operation.rule._tag !== 'ContractRule')
    throw new RangeError('intrinsic contract finisher received a non-contract operation')
  const reference = intrinsicContractReference(operation, operationToken)
  const analyzed = analyzeCallContract(
    call,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(call),
    typeArguments,
    resolution,
    caller,
  )
  const unsafeDiagnostic = unsafeCallDiagnostic(
    operation.unsafe,
    reference.spelling,
    call,
    resolution,
  )
  const substitution =
    analyzed.fact._tag === 'Compatible'
      ? analyzed.fact.substitution
      : new Map<string, Type.GenericArgument>()
  const substitutedResult = Type.substitute(operation.rule.contract.result, substitution)
  const type =
    analyzed.fact._tag === 'Compatible' &&
    unsafeDiagnostic === undefined &&
    Type.isEffect(substitutedResult)
      ? availableExpressionType(
          Type.effectWithRows(
            substitutedResult.success,
            substitutedResult.failureRow,
            intrinsicEffectCaptureAccess(
              operation,
              argumentsResult.facts,
              resolution.index,
              copyAssumptionsOf(caller),
            ),
            substitutedResult.requirementRow,
          ),
        )
      : unavailableExpressionType
  const protected_ = argumentsResult.facts.at(0)
  const evidence = analyzed.fact._tag === 'Compatible' ? analyzed.fact.evidence : Object.freeze([])
  if (operation.rule.post === 'CatchFailure') {
    const handler = argumentsResult.facts.at(1)
    const wanted = operation.rule.contract.constraints
      .map((constraint) => Constraint.substitute(constraint, substitution))
      .find(
        (constraint): constraint is Constraint.FailureSubset =>
          constraint._tag === 'FailureSubsetConstraint',
      )
    const wantedKey = wanted === undefined ? undefined : Constraint.key(wanted)
    const proved =
      wantedKey !== undefined &&
      evidence.some(
        (candidate) =>
          (candidate._tag === 'Assumed' && candidate.wantedKey === wantedKey) ||
          (candidate._tag === 'FailureSubset' &&
            wanted !== undefined &&
            RowAlgebra.equals(Type.failureRowPolicy(), candidate.selected, wanted.selected) &&
            RowAlgebra.equals(Type.failureRowPolicy(), candidate.source, wanted.source)),
      )
    const handlerType = handler?.type._tag === 'Available' ? handler.type.type : undefined
    const handlerEffect =
      handlerType !== undefined && Type.isCallable(handlerType) && Type.isEffect(handlerType.result)
        ? handlerType.result
        : undefined
    const catchAvailable =
      type._tag === 'Available' &&
      protected_ !== undefined &&
      handler !== undefined &&
      wanted !== undefined &&
      proved
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EffectCatch',
        reference: intrinsicReference(source, call),
        protected: protected_?.expression ?? unavailableExpression(call),
        handler: handler?.expression ?? unavailableExpression(call),
        ...(wanted === undefined ? {} : { selected: Type.failureType(wanted.selected) }),
        protectedRow: wanted?.source ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
        handlerRow: handlerEffect?.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
        residualRow:
          wanted === undefined
            ? RowAlgebra.concrete(Type.failureRowPolicy(), [])
            : RowAlgebra.without(Type.failureRowPolicy(), wanted.source, wanted.selected),
        evidence,
        type: catchAvailable ? type : unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([
        ...argumentsResult.diagnostics,
        ...typeArguments.diagnostics,
        ...analyzed.diagnostics,
        ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
      ]),
      type: catchAvailable && type._tag === 'Available' ? type.type : undefined,
    })
  }
  const provider = argumentsResult.facts.at(1)
  const binding =
    provider === undefined
      ? undefined
      : effectBindingProvider(
          operation,
          substitution,
          evidence,
          provider.expression,
          provider.syntax.span,
          resolution.index,
        )
  const bindingAvailable =
    type._tag === 'Available' && protected_ !== undefined && binding !== undefined
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectBindRequirement',
      reference: intrinsicReference(source, call),
      protected: protected_?.expression ?? unavailableExpression(call),
      ...(bindingAvailable
        ? {
            provider: binding,
          }
        : {}),
      type,
      syntax: call,
    }),
    diagnostics: Object.freeze([
      ...argumentsResult.diagnostics,
      ...typeArguments.diagnostics,
      ...analyzed.diagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

function analyzeBuiltinCall(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  argumentsResult: ArgumentsResult,
  typeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
  caller: DeclarationFact,
): ExpressionResult {
  const identifiers = callReferenceTokens(call)
  const actorToken = identifiers.at(0)
  const operationToken = identifiers.at(1)

  if (actorToken === undefined || operationToken === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(call, 'Identifier'),
        }),
        path: referencePath(call),
        typeArguments: typeArguments.facts,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: call }),
        }),
        type: unavailableExpressionType,
        syntax: call,
      }),
      diagnostics: Object.freeze([...argumentsResult.diagnostics, ...typeArguments.diagnostics]),
      type: undefined,
    })
  }

  const actorSpelling = spelling(source, actorToken)
  const operationSpelling = spelling(source, operationToken)
  const actor = Intrinsic.findActor(actorSpelling)
  const operation = Intrinsic.findOperation(actorSpelling, operationSpelling)
  if (
    operation?.rule._tag === 'ContractRule' &&
    isSectionArity(operation.rule.contract.parameters.length, argumentsResult.facts.length)
  )
    return finishCallableSection(
      call,
      intrinsicContractReference(operation, operationToken),
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  if (operation?.rule._tag === 'ContractRule')
    return finishIntrinsicContractCall(
      source,
      call,
      operation,
      operationToken,
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  const signature = builtinSignature(actorSpelling, operationSpelling)
  const declaredTypeParameters = signature?.typeParameters ?? Object.freeze([])
  const specializationDiagnostic =
    typeArguments.explicit &&
    (typeArguments.types === undefined ||
      typeArguments.types.length !== declaredTypeParameters.length)
      ? Diagnostic.typeArgumentArity(
          `${actorSpelling}.${operationSpelling}`,
          declaredTypeParameters.length,
          typeArguments.types?.length ?? 0,
          call.span,
        )
      : undefined
  const substitution = new Map<string, Type.GenericArgument>()
  if (
    typeArguments.explicit &&
    typeArguments.types !== undefined &&
    typeArguments.types.length === declaredTypeParameters.length
  ) {
    for (const [ordinal, parameter] of declaredTypeParameters.entries()) {
      const argument = typeArguments.types.at(ordinal)
      if (argument !== undefined) substitution.set(Type.key(parameter), argument)
    }
  } else if (!typeArguments.explicit && signature !== undefined) {
    for (const [ordinal, parameter] of signature.parameters.entries()) {
      const argument = argumentsResult.facts.at(ordinal)
      if (argument?.type._tag === 'Available')
        Type.infer(parameter, argument.type.type, substitution)
    }
  }
  const missingInference = declaredTypeParameters.find(
    (parameter) => substitution.get(Type.key(parameter)) === undefined,
  )
  const inferenceDiagnostic =
    specializationDiagnostic === undefined && missingInference !== undefined
      ? Diagnostic.typeArgumentArity(
          `${actorSpelling}.${operationSpelling}`,
          declaredTypeParameters.length,
          0,
          call.span,
        )
      : undefined
  const instantiatedParameters =
    signature === undefined
      ? Object.freeze([])
      : Object.freeze(
          signature.parameters.map((parameter) => Type.substitute(parameter, substitution)),
        )
  const instantiatedResult =
    signature === undefined ? undefined : Type.substitute(signature.result, substitution)
  const unsafeDiagnostic =
    signature === undefined
      ? undefined
      : unsafeCallDiagnostic(
          signature.unsafe === true,
          `${actorSpelling}.${operationSpelling}`,
          call,
          resolution,
        )
  const missingDiagnostic =
    actor === undefined
      ? Diagnostic.unknownActor(actorSpelling, actorToken.span)
      : signature === undefined
        ? Diagnostic.unknownActorOperation(actorSpelling, operationSpelling, operationToken.span)
        : undefined
  const reference: CallReferenceFact =
    signature !== undefined
      ? Object.freeze({
          _tag: 'ResolvedBuiltin',
          spelling: `${actorSpelling}.${operationSpelling}`,
          token: operationToken,
          actor: actorSpelling,
          operation: signature.operation,
          intrinsic: signature.id,
          parameters: instantiatedParameters,
          result: instantiatedResult ?? signature.result,
          unsafe: signature.unsafe === true,
          ...(signature.returnedBorrowParameter === undefined
            ? {}
            : { returnedBorrowParameter: signature.returnedBorrowParameter }),
        })
      : Object.freeze({
          _tag: 'Missing',
          spelling: `${actorSpelling}.${operationSpelling}`,
          token: actor === undefined ? actorToken : operationToken,
          ...(missingDiagnostic === undefined
            ? {}
            : { cause: Diagnostic.identity(missingDiagnostic) }),
        })
  if (
    reference._tag === 'ResolvedBuiltin' &&
    declaredTypeParameters.length === 0 &&
    isSectionArity(reference.parameters.length, argumentsResult.facts.length)
  ) {
    return finishCallableSection(
      call,
      reference,
      argumentsResult,
      typeArguments,
      resolution,
      caller,
    )
  }
  const callContract = analyzeCallContract(call, reference, argumentsResult.facts)
  const expressionType =
    hasAvailableCallSyntax(call) &&
    reference._tag === 'ResolvedBuiltin' &&
    specializationDiagnostic === undefined &&
    inferenceDiagnostic === undefined &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(reference.result)
      : unavailableExpressionType

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(call),
      typeArguments: typeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: call,
    }),
    diagnostics: Object.freeze([
      ...(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
      ...(specializationDiagnostic === undefined ? [] : [specializationDiagnostic]),
      ...(inferenceDiagnostic === undefined ? [] : [inferenceDiagnostic]),
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
      ...argumentsResult.diagnostics,
      ...typeArguments.diagnostics,
      ...callContract.diagnostics,
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const builtinArgumentMappings = (
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
): ReadonlyArray<BuiltinArgumentMappingFact> =>
  reference._tag !== 'ResolvedBuiltin'
    ? Object.freeze([])
    : Object.freeze(
        reference.parameters.flatMap(
          (expected, ordinal): ReadonlyArray<BuiltinArgumentMappingFact> => {
            const argument = argumentsList.at(ordinal)
            return argument === undefined
              ? []
              : [Object.freeze({ _tag: 'BuiltinArgumentMapping', argument, ordinal, expected })]
          },
        ),
      )

const analyzeGroupedExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
): ExpressionResult => {
  const child = node.children.find(isExpressionNode)
  const expression =
    child === undefined
      ? undefined
      : analyzeExpression(
          source,
          child,
          declarations,
          declaration,
          scope,
          resolution,
          expected,
          borrowAllowed,
        )
  if (expression === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Grouped',
        expression: Object.freeze({
          _tag: 'Integer',
          integer: Object.freeze({ _tag: 'Unavailable', syntax: node }),
          type: unavailableExpressionType,
          syntax: node,
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  }
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Grouped',
      expression: expression.fact,
      type: expression.fact.type,
      syntax: node,
    }),
    diagnostics: expression.diagnostics,
    type: expression.type,
  })
}

/**
 * Analyzes `&&` or `||`. Both operands must be `bool` and the result is `bool`. HIR retains the
 * right operand as a conditional region, so its ordinary effects, moves, loans, and cleanup stay
 * on the path that executes it.
 */
const analyzeShortCircuitExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  operator: Operator.ShortCircuit,
  operandNodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const boolean: SemanticType = Scalar.boolean.spelling
  const argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    Object.freeze(operandNodes.map(() => boolean)),
  )
  const operandDiagnostics = argumentsResult.facts.flatMap((argument) =>
    argument.type._tag === 'Available' && !Type.equals(argument.type.type, boolean)
      ? [
          Diagnostic.argumentTypeMismatch(
            Type.encode(boolean),
            Type.encode(argument.type.type),
            argument.syntax.span,
          ),
        ]
      : [],
  )
  const rejected =
    operandDiagnostics.length > 0 ||
    argumentsResult.facts.length !== 2 ||
    argumentsResult.facts.some((argument) => argument.type._tag !== 'Available')
  const type = rejected ? unavailableExpressionType : availableExpressionType(boolean)
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'ShortCircuit',
      operator,
      arguments: argumentsResult.facts,
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...operandDiagnostics]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

interface OperatorContractSelection extends InterfaceOperationFact {
  readonly declaration: DeclarationIndex.ServiceOperationFact
  readonly parameters: ReadonlyArray<SemanticType>
  readonly result: SemanticType
  readonly label: string
}

const operatorContractSelection = (
  capability: Type.Nominal,
  provider: Type.Type,
  operation: DeclarationIndex.InterfaceOperationApplicationFact,
): OperatorContractSelection | undefined => {
  const contract = interfaceOperationContract(operation)
  const name = operation.declaration.name
  if (contract === undefined || name._tag !== 'Present') return undefined
  return Object.freeze({
    capability,
    provider,
    operation: name.spelling,
    contract: operation,
    declaration: contract.declaration,
    parameters: contract.parameters,
    result: contract.result,
    label: `${Type.encode(capability)}.${name.spelling}`,
  })
}

const boundOperatorSelections = (
  declaration: DeclarationFact,
  operator: Operator.Eligible,
): ReadonlyArray<OperatorContractSelection> =>
  Object.freeze(
    declaration.typeParameters.flatMap((parameter) =>
      parameter.bounds.flatMap((bound) =>
        bound._tag !== 'ResolvedBound'
          ? []
          : bound.application.operations.flatMap((operation) => {
              if (operation.declaration.operator?.operator !== operator) return []
              const selected = operatorContractSelection(
                bound.application.capability,
                parameter.type,
                operation,
              )
              return selected === undefined ? [] : [selected]
            }),
      ),
    ),
  )

const concreteOperatorSelections = (
  index: DeclarationIndex.Index,
  module: string,
  operator: Operator.Eligible,
): ReadonlyArray<OperatorContractSelection> => {
  const interfaces = index.modules.flatMap((headers) => headers.interfaces)
  const selections = index.modules.flatMap((headers) =>
    headers.conformances.flatMap((conformance) => {
      if (
        conformance.validity._tag !== 'ValidConformance' ||
        conformance.coherence._tag !== 'Coherent' ||
        conformance.termination._tag !== 'Terminating' ||
        conformance.capability._tag !== 'Resolved' ||
        conformance.provider._tag !== 'Resolved' ||
        !Type.isNominal(conformance.capability.type)
      )
        return []
      const capability = conformance.capability.type
      const provider = conformance.provider.type
      const interface_ = interfaces.find(
        (candidate) =>
          candidate.canonical._tag === 'Canonical' &&
          candidate.canonical.id.module === capability.module &&
          candidate.canonical.id.name === capability.name &&
          (candidate.visibility === 'Public' || candidate.canonical.id.module === module),
      )
      if (interface_ === undefined) return []
      const proof = DeclarationIndex.prove(index, provider, capability)
      if (
        proof._tag !== 'Proved' ||
        proof.selection._tag !== 'SourceSelection' ||
        proof.selection.module !== conformance.module ||
        proof.selection.ordinal !== conformance.ordinal
      )
        return []
      const application = DeclarationIndex.interfaceApplication(interface_, capability, provider)
      if (application?.available !== true) return []
      return application.operations.flatMap((operation) => {
        if (operation.declaration.operator?.operator !== operator) return []
        const selected = operatorContractSelection(capability, provider, operation)
        return selected === undefined ? [] : [selected]
      })
    }),
  )
  const unique = new Map<string, OperatorContractSelection>()
  for (const selection of selections)
    unique.set(
      `${Type.key(selection.capability)}\u0000${Type.key(selection.provider)}\u0000${selection.operation}`,
      selection,
    )
  return Object.freeze([...unique.values()])
}

const operatorSelectionMatches = (
  selection: OperatorContractSelection,
  arguments_: ReadonlyArray<ArgumentFact>,
): boolean =>
  selection.parameters.length === arguments_.length &&
  arguments_.every((argument, ordinal) => {
    const expected = selection.parameters.at(ordinal)
    return (
      expected !== undefined &&
      argument.type._tag === 'Available' &&
      (typesCompatible(argument.type.type, expected) ||
        contextualIntegerCompatible(argument.expression, expected))
    )
  })

const finishInterfaceOperator = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  operator: Operator.Eligible,
  operatorToken: Token.Token,
  operandNodes: ReadonlyArray<SyntaxTree.Node>,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  selection: OperatorContractSelection,
): ExpressionResult => {
  const argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    selection.parameters,
  )
  const arguments_ = Object.freeze(
    argumentsResult.facts.map((argument) => {
      let expression = argument.expression
      while (expression._tag === 'Grouped') expression = expression.expression
      return expression === argument.expression
        ? argument
        : Object.freeze({ ...argument, expression, syntax: expression.syntax })
    }),
  )
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedBoundOperation',
    spelling: selection.label,
    token: operatorToken,
    capability: selection.capability,
    provider: selection.provider,
    operation: selection.operation,
    declaration: selection.declaration,
    interfaceContract: selection.contract,
    parameters: selection.parameters,
    result: selection.result,
  })
  const contract = analyzeCallContract(node, reference, arguments_, true)
  const type =
    contract.fact._tag === 'Compatible'
      ? availableExpressionType(selection.result)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Operator',
      operator,
      reference,
      arguments: arguments_,
      mappings: Object.freeze(
        selection.parameters.flatMap((expected, ordinal) => {
          const argument = arguments_.at(ordinal)
          return argument === undefined
            ? []
            : [
                Object.freeze({
                  _tag: 'BuiltinArgumentMapping' as const,
                  argument,
                  ordinal,
                  expected,
                }),
              ]
        }),
      ),
      contract: contract.fact,
      interfaceOperation: selection,
      ...(selection.contract.functionKind === 'Effect'
        ? { witnessEffectSite: executableSite('EffectSiteId', resolution, node) }
        : {}),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...contract.diagnostics]),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const analyzeOperatorExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
): ExpressionResult => {
  const operatorToken = node.children.find(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) &&
      (node.kind === 'PrefixExpression'
        ? Operator.prefix(element.kind) !== undefined
        : Operator.infix(element.kind) !== undefined),
  )
  const operator =
    operatorToken === undefined
      ? undefined
      : node.kind === 'PrefixExpression'
        ? Operator.prefix(operatorToken.kind)
        : Operator.infix(operatorToken.kind)?.operator
  const operandNodes = node.children.filter(isExpressionNode)
  if (operator !== undefined && Operator.isShortCircuit(operator)) {
    return analyzeShortCircuitExpression(
      source,
      node,
      operator,
      operandNodes,
      declarations,
      declaration,
      scope,
      resolution,
    )
  }
  const initialExpected =
    typeof expected === 'string' &&
    Scalar.isSpelling(expected) &&
    node.kind === 'InfixExpression' &&
    operator !== undefined &&
    !Operator.isPredicate(operator)
      ? Object.freeze(operandNodes.map(() => expected))
      : Object.freeze([])
  let argumentsResult = analyzeArgumentNodes(
    source,
    node,
    operandNodes,
    declarations,
    declaration,
    scope,
    resolution,
    initialExpected,
  )
  if (operator === undefined || operatorToken === undefined) {
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Unavailable',
      syntax: unavailableSyntax(node, node.kind === 'PrefixExpression' ? 'Minus' : 'Plus'),
    })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Operator',
        operator: node.kind === 'PrefixExpression' ? 'Negate' : 'Add',
        reference,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: node }),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: argumentsResult.diagnostics,
      type: undefined,
    })
  }

  const firstType = argumentsResult.facts.at(0)?.type
  if (
    firstType?._tag === 'Available' &&
    typeof firstType.type === 'string' &&
    Scalar.isSpelling(firstType.type) &&
    (initialExpected.length === 0 || firstType.type !== expected) &&
    operandNodes.length > 1
  ) {
    argumentsResult = analyzeArgumentNodes(
      source,
      node,
      operandNodes,
      declarations,
      declaration,
      scope,
      resolution,
      Object.freeze(operandNodes.map(() => firstType.type)),
    )
  }
  const selectedFirstType = argumentsResult.facts.at(0)?.type
  const builtinOperand =
    selectedFirstType?._tag === 'Available' &&
    (Type.isString(selectedFirstType.type) || Scalar.isSpelling(selectedFirstType.type))
  if (!builtinOperand) {
    const candidates = [
      ...boundOperatorSelections(declaration, operator),
      ...concreteOperatorSelections(resolution.index, source.id, operator),
    ].filter((candidate) => operatorSelectionMatches(candidate, argumentsResult.facts))
    if (candidates.length === 1) {
      const candidate = candidates.at(0)
      if (candidate !== undefined)
        return finishInterfaceOperator(
          source,
          node,
          operator,
          operatorToken,
          operandNodes,
          declarations,
          declaration,
          scope,
          resolution,
          candidate,
        )
    }
    const operatorSpelling = spelling(source, operatorToken)
    const operandTypes = argumentsResult.facts.flatMap((argument) =>
      argument.type._tag === 'Available' ? [Type.encode(argument.type.type)] : [],
    )
    const diagnostic =
      candidates.length > 1
        ? Diagnostic.ambiguousOperator(
            operatorSpelling,
            candidates.map((candidate) => candidate.label),
            operatorToken.span,
          )
        : Diagnostic.operatorNotApplicable(operatorSpelling, operandTypes, operatorToken.span)
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: operatorSpelling,
      token: operatorToken,
      cause: Diagnostic.identity(diagnostic),
    })
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Operator',
        operator,
        reference,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({ _tag: 'UnavailableCallSyntax', syntax: node }),
          cause: Diagnostic.identity(diagnostic),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: Object.freeze([...argumentsResult.diagnostics, diagnostic]),
      type: undefined,
    })
  }
  const selectedActor: Operator.Actor =
    selectedFirstType?._tag === 'Available' && Type.isString(selectedFirstType.type)
      ? 'string'
      : selectedFirstType?._tag === 'Available' && Scalar.isSpelling(selectedFirstType.type)
        ? selectedFirstType.type
        : Scalar.defaultInteger.spelling
  const target = Operator.target(operator, selectedActor)
  const signature = builtinSignature(target.actor, target.operation, 'Primitive')
  if (signature === undefined) throw new RangeError('Compiler operator table is inconsistent')
  const operatorParameters = signature.parameters
  const operatorResult = signature.result
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedBuiltin',
    spelling: `${target.actor}.${target.operation}`,
    token: operatorToken,
    actor: target.actor,
    operation: signature.operation,
    intrinsic: signature.id,
    parameters: operatorParameters,
    result: operatorResult,
    unsafe: signature.unsafe === true,
  })
  const contract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    isAvailableSyntax(node),
  )
  const expressionType =
    contract.fact._tag === 'Compatible'
      ? availableExpressionType(operatorResult)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Operator',
      operator,
      reference,
      arguments: argumentsResult.facts,
      mappings: builtinArgumentMappings(reference, argumentsResult.facts),
      contract: contract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([...argumentsResult.diagnostics, ...contract.diagnostics]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const analyzePipelineExpression = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const inputNode = pipelineInput(node)
  const target = pipelineCallable(node)
  const callable =
    target === undefined
      ? undefined
      : analyzeExpression(source, target, declarations, declaration, scope, resolution)
  const expectedInput =
    callable?.type !== undefined && Type.isCallable(callable.type)
      ? callable.type.parameters.at(0)
      : undefined
  const input =
    inputNode === undefined
      ? undefined
      : analyzeExpression(
          source,
          inputNode,
          declarations,
          declaration,
          scope,
          resolution,
          expectedInput,
        )
  const inputFact = input?.fact ?? unavailableExpression(inputNode ?? node)
  const callableResult =
    callable ??
    Object.freeze({
      fact: unavailableExpression(target ?? node),
      diagnostics: Object.freeze([]),
      type: undefined,
    })
  return finishCallableApplication(
    node,
    callableResult,
    Object.freeze({
      facts: Object.freeze([argumentFact(declaration, node.span, inputFact, 0)]),
      diagnostics: input?.diagnostics ?? Object.freeze([]),
    }),
    Object.freeze({
      explicit: false,
      facts: Object.freeze([]),
      diagnostics: Object.freeze([]),
    }),
    Object.freeze({
      _tag: 'PipelineCallableApplication',
      left: inputFact,
      callable: callableResult.fact,
      evaluation: 'LeftThenCallable',
    }),
    resolution,
    declaration,
  )
}

const effectExpressionAccess = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  if (expression._tag === 'Move') {
    if (expression.subject.type._tag === 'Available' && Type.isEffect(expression.subject.type.type))
      return expression.subject.type.type.access
    if (
      expression.subject.type._tag === 'Available' &&
      Type.isCallable(expression.subject.type.type)
    )
      return expression.subject.type.type.mode
    if (
      expression.subject.type._tag === 'Available' &&
      index !== undefined &&
      DeclarationIndex.copyType(index, expression.subject.type.type, assumptions)
    )
      return 'Shared'
    return 'Take'
  }
  if (expression._tag === 'Borrow')
    return expression.access === 'Exclusive' ? 'Exclusive' : 'Shared'
  if (expression._tag === 'Grouped')
    return effectExpressionAccess(expression.expression, index, assumptions)
  if (expression._tag === 'CallableSection') return expression.mode
  if (expression.type._tag === 'Available' && Type.isEffect(expression.type.type))
    return expression.type.type.access
  if (expression.type._tag === 'Available' && Type.isCallable(expression.type.type))
    return expression.type.type.mode
  return 'Shared'
}

const effectCaptureAccess = (
  arguments_: ReadonlyArray<ArgumentFact>,
  index: DeclarationIndex.Index | undefined,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  const accesses = arguments_.map((argument) =>
    effectExpressionAccess(argument.expression, index, assumptions),
  )
  return accesses.includes('Take')
    ? 'Take'
    : accesses.includes('Exclusive')
      ? 'Exclusive'
      : 'Shared'
}

const intrinsicEffectCaptureAccess = (
  operation: Intrinsic.Operation,
  arguments_: ReadonlyArray<ArgumentFact>,
  index: DeclarationIndex.Index,
  assumptions: ReadonlySet<string> = new Set(),
): Type.Effect['access'] => {
  if (
    operation.rule._tag !== 'ContractRule' ||
    operation.rule.post !== 'BindRequirement' ||
    operation.rule.providerMode !== 'Take'
  )
    return effectCaptureAccess(arguments_, index, assumptions)
  const accesses = arguments_.map((argument, ordinal) =>
    ordinal === 1
      ? ownedProviderCaptureAccess(argument.expression, index, assumptions)
      : effectExpressionAccess(argument.expression, index, assumptions),
  )
  return accesses.includes('Take')
    ? 'Take'
    : accesses.includes('Exclusive')
      ? 'Exclusive'
      : 'Shared'
}

const strongestEffectAccess = (
  ...accesses: ReadonlyArray<Type.Effect['access']>
): Type.Effect['access'] =>
  accesses.includes('Take') ? 'Take' : accesses.includes('Exclusive') ? 'Exclusive' : 'Shared'

const intrinsicOperationTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): Intrinsic.Operation | undefined => {
  const identifiers = callReferenceTokens(node)
  const qualifier = identifiers.at(0)
  const member = identifiers.at(1)
  return qualifier === undefined || member === undefined
    ? undefined
    : Intrinsic.findOperation(spelling(source, qualifier), spelling(source, member))
}

const intrinsicReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): IntrinsicReferenceFact => {
  const identifiers = callReferenceTokens(node)
  const actorToken = identifiers.at(0)
  const operationToken = identifiers.at(1)
  const actor =
    actorToken === undefined ? undefined : Intrinsic.findActor(spelling(source, actorToken))
  const operation =
    actor === undefined || operationToken === undefined
      ? undefined
      : Intrinsic.findOperation(actor.spelling, spelling(source, operationToken))
  return actorToken === undefined ||
    operationToken === undefined ||
    actor === undefined ||
    operation === undefined
    ? Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: node })
    : Object.freeze({
        _tag: 'ResolvedIntrinsicReference',
        actor,
        operation,
        actorToken,
        operationToken,
      })
}

const isEffectResultTarget = (source: SourceFile.SourceFile, node: SyntaxTree.Node): boolean => {
  const rule = intrinsicOperationTarget(source, node)?.rule
  return rule?._tag === 'EffectRule' && rule.operation === 'Result'
}

const analyzeEffectResult = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const pipelined = node.kind === 'PipelineExpression'
  const target = pipelined ? (pipelineCallable(node) ?? node) : node
  const list = SyntaxTree.directNode(target, 'ArgumentList')
  const argumentNodes =
    list?.children.filter((element): element is SyntaxTree.Node =>
      isRecursiveArgumentNode(element),
    ) ?? []
  const protectedNode = pipelined ? pipelineInput(node) : argumentNodes.at(0)
  const protectedResult =
    protectedNode === undefined
      ? undefined
      : analyzeExpression(source, protectedNode, declarations, declaration, scope, resolution)
  const protectedEffect =
    protectedResult?.type !== undefined && Type.isEffect(protectedResult.type)
      ? protectedResult.type
      : undefined
  const diagnostics: Array<Diagnostic.Diagnostic> = [...(protectedResult?.diagnostics ?? [])]
  if (argumentNodes.length !== (pipelined ? 0 : 1))
    diagnostics.push(
      Diagnostic.invalidEffectHandler('result requires exactly one Effect', node.span),
    )
  if (protectedEffect === undefined)
    diagnostics.push(
      Diagnostic.invalidEffectHandler(
        'the protected argument is not an Effect',
        protectedNode?.span ?? node.span,
      ),
    )
  const failureValue = protectedEffect === undefined ? 'never' : Type.failureType(protectedEffect)
  const type =
    protectedEffect === undefined
      ? unavailableExpressionType
      : availableExpressionType(
          Type.effectWithRows(
            Type.result(protectedEffect.success, failureValue),
            RowAlgebra.concrete(Type.failureRowPolicy(), []),
            protectedEffect.access,
            protectedEffect.requirementRow,
          ),
        )
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectResult',
      reference: intrinsicReference(source, target),
      protected: protectedResult?.fact ?? unavailableExpression(node),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

/** Finalizes ordinary lexical captures for one source Effect body. */
const effectCaptureFacts = (
  statements: ReadonlyArray<StatementFact>,
  firstLocalBinding: number,
  index?: DeclarationIndex.Index,
  assumptions: ReadonlySet<string> = new Set(),
): ReadonlyArray<EffectCaptureFact> => {
  const captures = new Map<string, EffectCaptureFact>()
  const rank = (access: EffectCaptureFact['access']): number =>
    access === 'Take' ? 3 : access === 'Exclusive' ? 2 : access === 'Shared' ? 1 : 0
  const recordReference = (
    reference: BindingDeclarationFact | ParameterFact | undefined,
    requested: EffectCaptureFact['access'],
    span: SourceSpan.SourceSpan,
    copy: boolean,
  ): void => {
    if (reference === undefined) return
    if (reference._tag === 'BindingFact' && reference.id.ordinal >= firstLocalBinding) return
    const key = `${reference._tag}:${reference.id.ordinal}`
    const access = requested === 'Shared' && copy ? 'Copy' : requested
    const prior = captures.get(key)
    if (prior === undefined || rank(access) > rank(prior.access)) {
      captures.set(key, Object.freeze({ _tag: 'EffectCapture', reference, access, span }))
    }
  }
  const record = (fact: IdentifierExpressionFact, requested: EffectCaptureFact['access']): void => {
    const reference =
      fact.reference._tag === 'ResolvedBinding'
        ? fact.reference.binding
        : fact.reference._tag === 'Resolved'
          ? fact.reference.parameter
          : undefined
    recordReference(
      reference,
      requested,
      fact.syntax.span,
      fact.type._tag === 'Available' &&
        !Type.containsViewBorrow(fact.type.type) &&
        (index === undefined
          ? typeof fact.type.type === 'string'
          : DeclarationIndex.copyType(index, fact.type.type, assumptions)),
    )
  }
  const expression = (
    fact: ExpressionFact,
    requested: EffectCaptureFact['access'] = 'Shared',
  ): void => {
    switch (fact._tag) {
      case 'Identifier':
        record(fact, requested)
        return
      case 'Move':
        expression(fact.subject, 'Take')
        return
      case 'Borrow':
        expression(fact.subject, fact.access === 'Exclusive' ? 'Exclusive' : 'Shared')
        return
      case 'Grouped':
      case 'FieldProjection':
        expression(fact._tag === 'Grouped' ? fact.expression : fact.subject, requested)
        return
      case 'IndexProjection':
        expression(fact.subject, requested)
        expression(fact.index)
        return
      case 'StructLiteral':
        for (const item of fact.initializers) expression(item.expression)
        return
      case 'ArrayLiteral':
        for (const item of fact.elements) expression(item.expression)
        return
      case 'Match':
        expression(fact.scrutinee, requested)
        for (const arm of fact.arms) {
          if (arm.guard !== undefined) expression(arm.guard)
          expression(arm.result)
        }
        return
      case 'Operator':
      case 'ShortCircuit':
      case 'Call':
        for (const argument of fact.arguments) expression(argument.expression)
        return
      case 'FunctionItem':
        return
      case 'CallableSection':
        for (const capture of fact.captures) expression(capture.expression)
        return
      case 'CallableApply':
        expression(fact.callee)
        for (const argument of fact.arguments) expression(argument.expression)
        return
      case 'Run':
        expression(fact.subject)
        return
      case 'EffectResult':
        expression(fact.protected)
        return
      case 'EffectCatch':
        expression(fact.protected)
        expression(fact.handler)
        return
      case 'EffectBindRequirement':
        expression(fact.protected)
        recordReference(
          fact.provider?.reference,
          fact.provider?.captureAccess ?? 'Shared',
          fact.syntax.span,
          false,
        )
        return
      case 'EffectBlock':
        // Constructing a nested deferred Effect still reads the environment needed to create
        // that Effect value. Bubble those dependencies into the enclosing Effect runner so a
        // parameter used only by the nested body remains available when the child is formed.
        for (const capture of fact.captures)
          recordReference(capture.reference, capture.access, capture.span, false)
        return
      case 'Integer':
      case 'Boolean':
      case 'Character':
      case 'Constant':
        return
    }
  }
  const visit = (items: ReadonlyArray<StatementFact>): void => {
    for (const statement of items) {
      switch (statement._tag) {
        case 'UnsafeStatement':
          visit(statement.statements)
          break
        case 'BindStatement':
          expression(statement.binding.initializer)
          break
        case 'PatternBindStatement':
          expression(statement.selection.source)
          break
        case 'ExpressionStatement':
          expression(statement.expression)
          break
        case 'IfStatement':
          expression(statement.condition)
          visit(statement.taken)
          visit(statement.otherwise)
          break
        case 'IfLetStatement':
          expression(statement.selection.source)
          visit(statement.taken)
          visit(statement.otherwise)
          break
        case 'WriteStatement':
          expression(statement.destination, 'Exclusive')
          expression(statement.value)
          break
        case 'WhileStatement':
          expression(statement.condition)
          visit(statement.body)
          break
        case 'ReturnStatement':
        case 'FailStatement':
        case 'DropStatement':
          expression(statement.expression)
          break
        case 'BreakStatement':
        case 'ContinueStatement':
          break
      }
    }
  }
  visit(statements)
  return Object.freeze(
    [...captures.values()].sort(
      (left, right) =>
        left.reference.id.ordinal - right.reference.id.ordinal ||
        left.span.start - right.span.start,
    ),
  )
}

function analyzeExpression(
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
  expected?: SemanticType,
  borrowAllowed = false,
): ExpressionResult | undefined {
  if (node.kind === 'UnsafeExpression') {
    const call = SyntaxTree.directNode(node, 'CallExpression')
    if (call === undefined) return undefined
    const analyzed = analyzeExpression(
      source,
      call,
      declarations,
      declaration,
      scope,
      Object.freeze({
        ...resolution,
        unsafeCallSpans: Object.freeze([...(resolution.unsafeCallSpans ?? []), call.span]),
      }),
      expected,
      borrowAllowed,
    )
    if (analyzed === undefined) return undefined
    const invokesUnsafe = (() => {
      const fact = analyzed.fact
      if (fact._tag === 'CallableApply')
        return (
          fact.callee.type._tag === 'Available' &&
          Type.isCallable(fact.callee.type.type) &&
          fact.callee.type.type.unsafe
        )
      if (fact._tag !== 'Call') return false
      switch (fact.reference._tag) {
        case 'Resolved':
          return fact.reference.declaration.unsafe
        case 'ResolvedBuiltin':
          return fact.reference.unsafe
        case 'ResolvedIntrinsicContract':
          return fact.reference.intrinsic.unsafe
        case 'ResolvedServiceOperation':
          return fact.reference.operation.unsafe
        case 'ResolvedBoundOperation':
          return fact.reference.interfaceContract.unsafe
        default:
          return false
      }
    })()
    const diagnostic = invokesUnsafe
      ? undefined
      : Diagnostic.misplacedUnsafeAcknowledgement(node.span)
    return Object.freeze({
      fact: analyzed.fact,
      diagnostics: Object.freeze([
        ...analyzed.diagnostics,
        ...(diagnostic === undefined ? [] : [diagnostic]),
      ]),
      type: diagnostic === undefined ? analyzed.type : undefined,
    })
  }
  if (node.kind === 'EffectExpression') {
    const representationOwner = executableSpecializationOwner(resolution)
    const block = SyntaxTree.directNode(node, 'Block')
    if (block === undefined)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'EffectBlock',
          site: executableSite('EffectSiteId', resolution, node),
          ...(representationOwner === undefined ? {} : { representationOwner }),
          statements: Object.freeze([]),
          captures: Object.freeze([]),
          bindings: Object.freeze([]),
          regions: Object.freeze([]),
          type: unavailableExpressionType,
          syntax: node,
        }),
        diagnostics: Object.freeze([]),
        type: undefined,
      })
    const firstLocalBinding = resolution.nextBindingOrdinal?.value ?? 0
    const nested: BodyContext = {
      source,
      declaration,
      declarations,
      bindings: [],
      diagnostics: [],
      regions: [],
      loops: [],
      resolution,
      nextBindingOrdinal: resolution.nextBindingOrdinal ?? { value: 0 },
      regionBase: 1_000_000 + node.span.start * 100,
      effectBlock: true,
    }
    const statements = analyzeStatements(nested, block, scope)
    const returned: Array<ExpressionFact> = []
    const failures: Array<Type.Nominal> = []
    const collectTerminals = (items: ReadonlyArray<StatementFact>): void => {
      for (const statement of items) {
        if (statement._tag === 'ReturnStatement') returned.push(statement.expression)
        else if (
          statement._tag === 'FailStatement' &&
          statement.failure !== undefined &&
          Type.isNominal(statement.failure)
        )
          failures.push(statement.failure)
        else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
          collectTerminals(statement.taken)
          collectTerminals(statement.otherwise)
        } else if (statement._tag === 'WhileStatement') collectTerminals(statement.body)
      }
    }
    collectTerminals(statements)
    const success = returned.at(-1)?.type
    const captures = effectCaptureFacts(
      statements,
      firstLocalBinding,
      resolution.index,
      copyAssumptionsOf(declaration),
    )
    const access = captures.some((capture) => capture.access === 'Take')
      ? 'Take'
      : captures.some((capture) => capture.access === 'Exclusive')
        ? 'Exclusive'
        : 'Shared'
    const type =
      success?._tag === 'Available'
        ? availableExpressionType(Type.effect(success.type, failures, access))
        : unavailableExpressionType
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'EffectBlock',
        site: executableSite('EffectSiteId', resolution, node),
        ...(representationOwner === undefined ? {} : { representationOwner }),
        statements,
        captures,
        bindings: Object.freeze(nested.bindings),
        regions: Object.freeze(nested.regions),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(nested.diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }
  if (node.kind === 'BooleanLiteralExpression') {
    const token = directToken(node, 'TrueKeyword') ?? directToken(node, 'FalseKeyword')
    const type = token === undefined ? unavailableExpressionType : availableBoolExpressionType
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Boolean',
        value: token?.kind === 'TrueKeyword',
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'IntegerLiteralExpression') {
    const integer = analyzeInteger(source, node, expected)
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Integer',
        integer: integer.fact,
        type:
          integer.fact._tag === 'Available'
            ? availableExpressionType(integer.fact.type)
            : unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: integer.diagnostics,
      type: integer.fact._tag === 'Available' ? integer.fact.type : undefined,
    })
  }

  if (node.kind === 'FloatingLiteralExpression') {
    const floating = analyzeFloating(source, node, expected)
    const fact = floating.fact
    const type =
      fact._tag === 'Available' ? availableExpressionType(fact.type) : unavailableExpressionType
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Floating', floating: fact, type, syntax: node }),
      diagnostics: floating.diagnostics,
      type: fact._tag === 'Available' ? fact.type : undefined,
    })
  }

  if (node.kind === 'StaticTextLiteralExpression') {
    const token = directToken(node, 'TextLiteral') ?? directToken(node, 'ByteStringLiteral')
    const bytes =
      token === undefined ? undefined : Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    const result =
      bytes === undefined || form === undefined
        ? undefined
        : StaticText.decode(Array.from(bytes), form)
    const diagnostic =
      result?._tag === 'Invalid'
        ? Diagnostic.invalidStaticLiteral(result.detail, node.span)
        : undefined
    const data = result?._tag === 'Decoded' ? result.data : undefined
    const type =
      data === undefined
        ? unavailableExpressionType
        : availableExpressionType(data.kind === 'Text' ? Type.string : Type.slice('Shared', 'u8'))
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'StaticText',
        ...(data === undefined ? {} : { data }),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostic === undefined ? [] : [diagnostic]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'CharacterLiteralExpression') {
    const token = directToken(node, 'CharLiteral')
    const bytes =
      token === undefined ? undefined : Option.getOrUndefined(SourceFile.slice(source, token.span))
    const form = bytes === undefined ? undefined : LiteralForm.recognize(bytes)
    const result =
      bytes === undefined || form === undefined
        ? undefined
        : StaticText.decodeScalar(Array.from(bytes), form)
    const diagnostic =
      result?._tag === 'Invalid'
        ? Diagnostic.invalidStaticLiteral(result.detail, node.span)
        : undefined
    const scalar = result?._tag === 'Scalar' ? result.value : undefined
    const type = scalar === undefined ? unavailableExpressionType : availableExpressionType('char')
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Character',
        ...(scalar === undefined ? {} : { value: scalar }),
        type,
        syntax: node,
      }),
      diagnostics: Object.freeze(diagnostic === undefined ? [] : [diagnostic]),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'UnitExpression') {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Unit',
        type: availableExpressionType(Type.unit),
        syntax: node,
      }),
      diagnostics: Object.freeze([]),
      type: Type.unit,
    })
  }

  if (node.kind === 'IdentifierExpression') {
    const value = analyzeIdentifier(source, node, scope)
    if (
      value.fact._tag === 'Identifier' &&
      (value.fact.reference._tag === 'Resolved' ||
        value.fact.reference._tag === 'ResolvedBinding' ||
        value.fact.reference._tag === 'ResolvedPattern')
    )
      return value
    return (
      analyzeConstantReference(source, node, resolution) ??
      analyzeFunctionItem(source, node, declarations, resolution) ??
      value
    )
  }

  if (node.kind === 'RunExpression') {
    const subjectNode = node.children.find(isExpressionNode)
    const subject =
      subjectNode === undefined
        ? undefined
        : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
    if (subject === undefined) throw new RangeError('Run expression requires one effect subject')
    const effect =
      subject.type !== undefined && Type.isEffect(subject.type)
        ? subject.type
        : subject.type !== undefined &&
            Type.isRepresented(subject.type) &&
            Type.isEffect(subject.type.contract)
          ? subject.type.contract
          : undefined
    const type =
      effect !== undefined ? availableExpressionType(effect.success) : unavailableExpressionType
    const allowed =
      declaration.functionKind === 'Effect' ? declaration.failureRow.failures : Object.freeze([])
    const unhandled =
      (effect === undefined ? [] : Type.failureMembers(effect)).filter(
        (failure) => !allowed.some((candidate) => Type.equals(candidate, failure)),
      ) ?? []
    const symbolicFailuresUnhandled =
      effect !== undefined &&
      RowAlgebra.concretize(Type.failureRowPolicy(), effect.failureRow)._tag === 'Residual' &&
      !RowAlgebra.isKnownSubset(
        Type.failureRowPolicy(),
        effect.failureRow,
        declaration.functionKind === 'Effect'
          ? declaration.failureRow.row
          : RowAlgebra.concrete(Type.failureRowPolicy(), []),
      )
    const allowedRequirements =
      declaration.functionKind === 'Effect'
        ? declaration.requirementRow.requirements
        : Object.freeze<Type.Requirement[]>([])
    const unsatisfiedRequirements =
      (effect === undefined ? [] : Type.requirementMembers(effect)).filter(
        (requirement) =>
          !allowedRequirements.some(
            (allowed) =>
              Type.equals(allowed.capability, requirement.capability) &&
              allowed.role === requirement.role &&
              (allowed.access === 'Exclusive' || allowed.access === requirement.access),
          ),
      ) ?? []
    const symbolicRequirementsUnsatisfied =
      effect !== undefined &&
      RowAlgebra.concretize(Type.requirementRowPolicy(), effect.requirementRow)._tag ===
        'Residual' &&
      !RowAlgebra.isKnownSubset(
        Type.requirementRowPolicy(),
        effect.requirementRow,
        declaration.functionKind === 'Effect'
          ? declaration.requirementRow.row
          : RowAlgebra.concrete(Type.requirementRowPolicy(), []),
      )
    const diagnostics = [...subject.diagnostics]
    if (effect === undefined && subject.type !== undefined)
      diagnostics.push(Diagnostic.runNonEffect(Type.encode(subject.type), node.span))
    if (unhandled.length > 0 || symbolicFailuresUnhandled)
      diagnostics.push(
        Diagnostic.unhandledEffectFailures(
          unhandled.length > 0
            ? unhandled.map(Type.encode)
            : [
                RowAlgebra.encode(
                  Type.failureRowPolicy(),
                  effect?.failureRow ?? RowAlgebra.concrete(Type.failureRowPolicy(), []),
                  Type.encode,
                  Type.encode,
                  (member) => member.parameter.name,
                ),
              ],
          node.span,
        ),
      )
    if (unsatisfiedRequirements.length > 0 || symbolicRequirementsUnsatisfied)
      diagnostics.push(
        Diagnostic.unhandledEffectRequirements(
          unsatisfiedRequirements.length > 0
            ? unsatisfiedRequirements.map(
                (requirement) =>
                  `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${Type.encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
              )
            : [
                RowAlgebra.encode(
                  Type.requirementRowPolicy(),
                  effect?.requirementRow ?? RowAlgebra.concrete(Type.requirementRowPolicy(), []),
                  (requirement) =>
                    `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${Type.encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
                  Type.encode,
                  (member) =>
                    `${member.access === 'Exclusive' ? '&mut ' : '&'}${member.capability.name}${member.role === 'DefaultRole' ? '' : `@${member.role}`}`,
                ),
              ],
          node.span,
        ),
      )
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Run', subject: subject.fact, type, syntax: node }),
      diagnostics: Object.freeze(diagnostics),
      type: type._tag === 'Available' ? type.type : undefined,
    })
  }

  if (node.kind === 'MoveExpression') {
    const move = analyzeMove(source, node, declarations, declaration, scope, resolution)
    return Object.freeze({
      fact: move.fact,
      diagnostics: move.diagnostics,
      type: move.type,
    })
  }

  if (node.kind === 'BorrowExpression') {
    return analyzeBorrow(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
      borrowAllowed,
    )
  }

  if (node.kind === 'MatchExpression') {
    return analyzeMatch(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
      borrowAllowed,
    )
  }

  if (node.kind === 'StructLiteralExpression') {
    return analyzeStructLiteral(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'ArrayLiteralExpression') {
    return analyzeArrayLiteral(source, node, declarations, declaration, scope, resolution, expected)
  }

  if (node.kind === 'FieldProjectionExpression') {
    return (
      analyzeConstantReference(source, node, resolution) ??
      analyzeFunctionItem(source, node, declarations, resolution) ??
      analyzeProjection(source, node, declarations, declaration, scope, resolution)
    )
  }

  if (node.kind === 'IndexProjectionExpression') {
    return analyzeIndexProjection(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'GroupedExpression') {
    return analyzeGroupedExpression(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
      borrowAllowed,
    )
  }

  if (node.kind === 'PrefixExpression' || node.kind === 'InfixExpression') {
    return analyzeOperatorExpression(
      source,
      node,
      declarations,
      declaration,
      scope,
      resolution,
      expected,
    )
  }

  if (node.kind === 'PipelineExpression' || node.kind === 'CallExpression') {
    const operationTarget = node.kind === 'PipelineExpression' ? pipelineCallable(node) : node
    if (operationTarget !== undefined && isEffectResultTarget(source, operationTarget))
      return analyzeEffectResult(source, node, declarations, declaration, scope, resolution)
    if (node.kind === 'PipelineExpression')
      return analyzePipelineExpression(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind !== 'CallExpression') return undefined

  const callTypeArguments = analyzeCallTypeArguments(source, node, declaration, resolution)
  const argumentsResult = analyzeArguments(
    source,
    node,
    declarations,
    declaration,
    scope,
    resolution,
    callTypeArguments,
  )

  const calleeNode = callCallee(node)
  const calleeResult = analyzeExpression(
    source,
    calleeNode,
    declarations,
    declaration,
    scope,
    resolution,
  )
  const resolvedValueCallee =
    calleeResult?.fact._tag === 'Identifier' &&
    (calleeResult.fact.reference._tag === 'Resolved' ||
      calleeResult.fact.reference._tag === 'ResolvedBinding' ||
      calleeResult.fact.reference._tag === 'ResolvedPattern')
  if (
    calleeResult !== undefined &&
    calleeResult.fact._tag !== 'FunctionItem' &&
    ((calleeResult.type !== undefined &&
      (Type.isCallable(calleeResult.type) ||
        (Type.isRepresented(calleeResult.type) && Type.isCallable(calleeResult.type.contract)))) ||
      (calleeResult.type !== undefined && calleeNode.kind !== 'IdentifierExpression') ||
      resolvedValueCallee ||
      calleeResult.fact._tag === 'Constant')
  ) {
    return finishCallableApplication(
      node,
      calleeResult,
      argumentsResult,
      callTypeArguments,
      undefined,
      resolution,
      declaration,
    )
  }

  const identifiers = callReferenceTokens(node)
  if (identifiers.length === 2) {
    const qualifierToken = identifiers.at(0)
    const memberToken = identifiers.at(1)
    if (qualifierToken === undefined || memberToken === undefined)
      return analyzeBuiltinCall(
        source,
        node,
        argumentsResult,
        callTypeArguments,
        resolution,
        declaration,
      )
    const qualifier = spelling(source, qualifierToken)
    const member = spelling(source, memberToken)
    if (intrinsicOperationTarget(source, node)?.rule._tag === 'PlaceRule') {
      return analyzePlaceReplace(source, node, declarations, declaration, scope, resolution)
    }
    const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
    if (qualifierLookup._tag === 'Intrinsic') {
      const libraryReference =
        qualifier === 'Effect'
          ? resolvedFunctionReference(source, node, declarations, resolution)
          : undefined
      if (libraryReference?._tag === 'Resolved')
        return finishDeclarationCall(
          node,
          libraryReference,
          argumentsResult,
          callTypeArguments,
          undefined,
          declaration,
          resolution,
        )
      return analyzeBuiltinCall(
        source,
        node,
        argumentsResult,
        callTypeArguments,
        resolution,
        declaration,
      )
    }
    if (
      qualifierLookup._tag === 'Resolved' &&
      qualifierLookup.declaration._tag === 'ServiceDeclaration'
    ) {
      const operation = serviceOperation(qualifierLookup.declaration, member)
      const diagnostic =
        operation === undefined
          ? Diagnostic.unknownActorOperation(qualifier, member, memberToken.span)
          : undefined
      const reference: CallReferenceFact =
        operation === undefined
          ? Object.freeze({
              _tag: 'Missing',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            })
          : Object.freeze({
              _tag: 'ResolvedServiceOperation',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              service: qualifierLookup.declaration,
              operation,
            })
      return finishDeclarationCall(
        node,
        reference,
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    if (
      qualifierLookup._tag === 'Resolved' &&
      qualifierLookup.declaration._tag === 'InterfaceDeclaration'
    ) {
      const bound = boundOperationReference(
        declaration,
        qualifierLookup.declaration,
        qualifier,
        member,
        memberToken,
      )
      if (bound?._tag === 'AmbiguousBound') {
        const ambiguous = Diagnostic.ambiguousBoundOperation(
          `${qualifier}.${member}`,
          bound.parameters,
          memberToken.span,
        )
        return finishDeclarationCall(
          node,
          Object.freeze({
            _tag: 'Missing',
            spelling: `${qualifier}.${member}`,
            token: memberToken,
            cause: Diagnostic.identity(ambiguous),
          }),
          argumentsResult,
          callTypeArguments,
          ambiguous,
          declaration,
          resolution,
        )
      }
      if (bound !== undefined)
        return finishBoundOperationCall(
          node,
          bound.reference,
          argumentsResult,
          callTypeArguments,
          resolution,
        )
    }
    if (
      qualifierLookup._tag === 'Resolved' &&
      (qualifierLookup.declaration._tag === 'StructDeclaration' ||
        qualifierLookup.declaration._tag === 'InterfaceDeclaration') &&
      qualifierLookup.declaration.canonical._tag === 'Canonical'
    ) {
      const actorModule = qualifierLookup.declaration.canonical.id.module
      const memberLookup = DeclarationIndex.lookup(resolution.index, actorModule, member)
      const candidate = memberLookup._tag === 'Resolved' ? memberLookup.declaration : undefined
      const diagnostic =
        candidate === undefined
          ? Diagnostic.unknownActorOperation(qualifier, member, memberToken.span)
          : candidate.visibility === 'Private'
            ? Diagnostic.inaccessibleImportedMember(actorModule, member, memberToken.span)
            : undefined
      const reference: CallReferenceFact =
        candidate !== undefined && candidate.visibility === 'Public'
          ? Object.freeze({
              _tag: 'Resolved',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              declaration: candidate,
            })
          : Object.freeze({
              _tag: 'Missing',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            })
      return finishDeclarationCall(
        node,
        reference,
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    if (qualifierLookup._tag === 'Namespace') {
      const memberLookup = DeclarationIndex.lookup(resolution.index, qualifierLookup.module, member)
      const candidate = memberLookup._tag === 'Resolved' ? memberLookup.declaration : undefined
      const diagnostic =
        candidate === undefined
          ? Diagnostic.unknownImportedMember(qualifierLookup.module, member, memberToken.span)
          : candidate.visibility === 'Private'
            ? Diagnostic.inaccessibleImportedMember(
                qualifierLookup.module,
                member,
                memberToken.span,
              )
            : undefined
      const reference: CallReferenceFact =
        candidate !== undefined && candidate.visibility === 'Public'
          ? Object.freeze({
              _tag: 'Resolved',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              declaration: candidate,
            })
          : Object.freeze({
              _tag: 'Missing',
              spelling: `${qualifier}.${member}`,
              token: memberToken,
              ...(diagnostic === undefined ? {} : { cause: Diagnostic.identity(diagnostic) }),
            })
      return finishDeclarationCall(
        node,
        reference,
        argumentsResult,
        callTypeArguments,
        diagnostic,
        declaration,
        resolution,
      )
    }
    const diagnostic =
      qualifierLookup._tag === 'Missing' || qualifierLookup._tag === 'Resolved'
        ? Diagnostic.unknownActor(qualifier, qualifierToken.span)
        : undefined
    const inheritedCause =
      qualifierLookup._tag === 'Unavailable'
        ? qualifierLookup.cause
        : qualifierLookup._tag === 'Conflict'
          ? qualifierLookup.conflict.cause
          : undefined
    const reference: CallReferenceFact = Object.freeze({
      _tag: 'Missing',
      spelling: `${qualifier}.${member}`,
      token: qualifierToken,
      ...(diagnostic !== undefined
        ? { cause: Diagnostic.identity(diagnostic) }
        : inheritedCause === undefined
          ? {}
          : { cause: inheritedCause }),
    })
    return finishDeclarationCall(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      diagnostic,
      declaration,
      resolution,
    )
  }

  const token = identifiers.at(0)
  if (token === undefined) {
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'Call',
        reference: Object.freeze({
          _tag: 'Unavailable',
          syntax: unavailableSyntax(callCallee(node), 'Identifier'),
        }),
        path: referencePath(node),
        typeArguments: callTypeArguments.facts,
        arguments: argumentsResult.facts,
        mappings: Object.freeze([]),
        contract: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'UnavailableCallSyntax',
            syntax: node,
          }),
        }),
        type: unavailableExpressionType,
        syntax: node,
      }),
      diagnostics: argumentsResult.diagnostics,
      type: undefined,
    })
  }

  const tokenSpelling = spelling(source, token)
  const resolvedLookup = NameResolution.lookup(resolution.scope, resolution.index, tokenSpelling)
  const localLookup = lookupDeclaration(declarations, tokenSpelling)
  const lookup: DeclarationIndex.DeclarationLookup =
    resolvedLookup._tag === 'Conflict'
      ? Object.freeze({
          _tag: 'Ambiguous',
          spelling: tokenSpelling,
          declarations: Object.freeze(
            resolvedLookup.conflict.bindings.flatMap((binding) => {
              if (binding._tag !== 'LocalDeclaration' && binding._tag !== 'ImportedMember')
                return []
              const declaration = DeclarationIndex.byCanonical(
                resolution.index,
                binding.declaration,
              )
              return declaration?._tag === 'FunctionDeclaration' ? [declaration] : []
            }),
          ),
        })
      : localLookup._tag === 'Ambiguous'
        ? localLookup
        : resolvedLookup._tag === 'Resolved' &&
            resolvedLookup.declaration._tag === 'FunctionDeclaration'
          ? Object.freeze({
              _tag: 'Resolved',
              spelling: tokenSpelling,
              declaration: resolvedLookup.declaration,
            })
          : resolvedLookup._tag === 'Missing'
            ? localLookup
            : Object.freeze({ _tag: 'Missing', spelling: tokenSpelling })
  const missingDiagnostic =
    lookup._tag === 'Missing' && resolvedLookup._tag !== 'Unavailable'
      ? Diagnostic.unknownFunction(tokenSpelling, token.span)
      : undefined
  const reference: CallReferenceFact =
    lookup._tag === 'Resolved'
      ? Object.freeze({
          _tag: 'Resolved',
          spelling: tokenSpelling,
          token,
          declaration: lookup.declaration,
        })
      : lookup._tag === 'Ambiguous'
        ? Object.freeze({
            _tag: 'Ambiguous',
            spelling: tokenSpelling,
            token,
            declarations: lookup.declarations,
            ...(resolvedLookup._tag === 'Conflict' ? { cause: resolvedLookup.conflict.cause } : {}),
          })
        : Object.freeze({
            _tag: 'Missing',
            spelling: tokenSpelling,
            token,
            ...(missingDiagnostic !== undefined
              ? { cause: Diagnostic.identity(missingDiagnostic) }
              : resolvedLookup._tag === 'Unavailable' && resolvedLookup.cause !== undefined
                ? { cause: resolvedLookup.cause }
                : {}),
          })
  if (
    reference._tag === 'Resolved' &&
    isSectionArity(reference.declaration.parameters.length, argumentsResult.facts.length)
  ) {
    return finishCallableSection(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      resolution,
      declaration,
    )
  }
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
    resolution,
    declaration,
  )
  const constraintDiagnostics = interfaceConstraintDiagnostics(
    reference,
    callContract,
    resolution.index,
    declaration,
    node.span,
  )
  const unsafeDiagnostic = unsafeCallDiagnostic(
    reference._tag === 'Resolved' && reference.declaration.unsafe,
    reference.spelling,
    node,
    resolution,
  )
  const syntaxAvailable = hasAvailableCallSyntax(node)
  const expressionType =
    syntaxAvailable &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved' &&
    callContract.fact._tag === 'Compatible' &&
    constraintDiagnostics.length === 0 &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, Type.GenericArgument>()
            const success = Type.substitute(reference.declaration.returnType.type, substitution)
            if (reference.declaration.functionKind !== 'Effect')
              return Type.isEffect(success)
                ? Type.effectWithRows(
                    success.success,
                    success.failureRow,
                    effectCaptureAccess(
                      argumentsResult.facts,
                      resolution.index,
                      copyAssumptionsOf(declaration),
                    ),
                    success.requirementRow,
                  )
                : success
            return Type.effectWithRows(
              success,
              Type.substituteFailureRow(reference.declaration.failureRow.row, substitution),
              effectCaptureAccess(
                argumentsResult.facts,
                resolution.index,
                copyAssumptionsOf(declaration),
              ),
              Type.substituteRequirementsRow(
                reference.declaration.requirementRow.row,
                substitution,
              ),
            )
          })(),
        )
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(node),
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(missingDiagnostic === undefined ? [] : [missingDiagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
      ...constraintDiagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const finishDeclarationCall = (
  node: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  diagnostic: Diagnostic.Diagnostic | undefined,
  caller: DeclarationFact,
  resolution: ResolutionContext,
): ExpressionResult => {
  if (
    reference._tag === 'Resolved' &&
    isSectionArity(reference.declaration.parameters.length, argumentsResult.facts.length)
  ) {
    const section = finishCallableSection(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      resolution,
      caller,
    )
    return diagnostic === undefined
      ? section
      : Object.freeze({
          ...section,
          diagnostics: Object.freeze([diagnostic, ...section.diagnostics]),
        })
  }
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
    resolution,
    caller,
  )
  const constraintDiagnostics = interfaceConstraintDiagnostics(
    reference,
    callContract,
    resolution.index,
    caller,
    node.span,
  )
  const callable = sourceCallable(reference)
  const unsafeDiagnostic = unsafeCallDiagnostic(
    callable?.unsafe === true,
    'spelling' in reference ? reference.spelling : 'callable',
    node,
    resolution,
  )
  const expressionType =
    hasAvailableCallSyntax(node) &&
    callable !== undefined &&
    callable.returnType._tag === 'Resolved' &&
    callContract.fact._tag === 'Compatible' &&
    constraintDiagnostics.length === 0 &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, Type.GenericArgument>()
            const success = Type.substitute(callable.returnType.type, substitution)
            if (callable.functionKind !== 'Effect')
              return Type.isEffect(success)
                ? Type.effectWithRows(
                    success.success,
                    success.failureRow,
                    effectCaptureAccess(
                      argumentsResult.facts,
                      resolution.index,
                      copyAssumptionsOf(caller),
                    ),
                    success.requirementRow,
                  )
                : success
            return Type.effectWithRows(
              success,
              Type.substituteFailureRow(callable.failureRow.row, substitution),
              effectCaptureAccess(
                argumentsResult.facts,
                resolution.index,
                copyAssumptionsOf(caller),
              ),
              Type.substituteRequirementsRow(callable.requirementRow.row, substitution),
            )
          })(),
        )
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(node),
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(diagnostic === undefined ? [] : [diagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
      ...constraintDiagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

/**
 * Finishes one call to an operation the enclosing declaration's bound declares.
 *
 * The contract is the interface's own, over the bounded parameter, so the call checks exactly like
 * a compiler-known operation's. It carries no type arguments of its own: the only type the call
 * varies over is the bounded parameter, and that one is supplied by the specialization of the
 * declaration this body belongs to.
 */
const finishBoundOperationCall = (
  node: SyntaxTree.Node,
  reference: Extract<CallReferenceFact, { readonly _tag: 'ResolvedBoundOperation' }>,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
): ExpressionResult => {
  const typeArgumentDiagnostic =
    callTypeArguments.explicit && callTypeArguments.facts.length > 0
      ? Diagnostic.typeArgumentArity(
          reference.spelling,
          0,
          callTypeArguments.facts.length,
          node.span,
        )
      : undefined
  const callContract = analyzeCallContract(node, reference, argumentsResult.facts)
  const unsafeDiagnostic = unsafeCallDiagnostic(
    reference.interfaceContract.unsafe,
    reference.spelling,
    node,
    resolution,
  )
  const expressionType =
    hasAvailableCallSyntax(node) &&
    typeArgumentDiagnostic === undefined &&
    callContract.fact._tag === 'Compatible' &&
    unsafeDiagnostic === undefined
      ? availableExpressionType(reference.result)
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Call',
      reference,
      path: referencePath(node),
      typeArguments: callTypeArguments.facts,
      arguments: argumentsResult.facts,
      mappings: callContract.mappings,
      contract: callContract.fact,
      ...(reference.interfaceContract.functionKind === 'Effect'
        ? { witnessEffectSite: executableSite('EffectSiteId', resolution, node) }
        : {}),
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(typeArgumentDiagnostic === undefined ? [] : [typeArgumentDiagnostic]),
      ...argumentsResult.diagnostics,
      ...callTypeArguments.diagnostics,
      ...callContract.diagnostics,
      ...(unsafeDiagnostic === undefined ? [] : [unsafeDiagnostic]),
    ]),
    type: expressionType._tag === 'Available' ? expressionType.type : undefined,
  })
}

const statementExpressionNode = (statement: SyntaxTree.Node): SyntaxTree.Node => {
  const expression = statement.children.find((element): element is SyntaxTree.Node =>
    isExpressionNode(element),
  )
  if (expression === undefined) {
    throw new RangeError('Semantic analysis expected a statement expression')
  }
  return expression
}

const compareDiagnostics = (left: Diagnostic.Diagnostic, right: Diagnostic.Diagnostic): number =>
  left.span.start - right.span.start ||
  left.span.end - right.span.end ||
  (left.code < right.code ? -1 : left.code > right.code ? 1 : 0)

interface FunctionAnalysis {
  readonly fact: FunctionFact
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
}

const bindingName = (
  source: SourceFile.SourceFile,
  statement: SyntaxTree.Node,
): DeclarationIndex.DeclaredName => {
  const token = directToken(statement, 'Identifier')
  return token === undefined
    ? Object.freeze({
        _tag: 'Unavailable' as const,
        syntax: unavailableSyntax(statement, 'Identifier'),
      })
    : Object.freeze({ _tag: 'Present' as const, spelling: spelling(source, token), token })
}

const scopeSpanFor = (scope: Scope, spellingText: string): SourceSpan.SourceSpan | undefined => {
  const binding = scope.bindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  if (binding?.name._tag === 'Present') return binding.name.token.span
  const patternBinding = scope.patternBindings.findLast(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  if (patternBinding?.name._tag === 'Present') return patternBinding.name.token.span
  const parameter = scope.parameters.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === spellingText,
  )
  return parameter?.name._tag === 'Present' ? parameter.name.token.span : undefined
}

interface BodyContext {
  readonly source: SourceFile.SourceFile
  readonly declaration: DeclarationFact
  readonly declarations: ReadonlyArray<DeclarationFact>
  readonly bindings: Array<BindingDeclarationFact>
  readonly diagnostics: Array<Diagnostic.Diagnostic>
  readonly regions: Array<Hir.RegionId>
  readonly loops: Array<Hir.LoopId>
  readonly resolution: ResolutionContext
  readonly nextBindingOrdinal: { value: number }
  readonly regionBase?: number
  readonly effectBlock?: true
}

interface ResolutionContext {
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
  readonly unsafeSpans?: ReadonlyArray<SourceSpan.SourceSpan>
  /** Exact direct-call spans acknowledged by the expression form `unsafe call(...)`. */
  readonly unsafeCallSpans?: ReadonlyArray<SourceSpan.SourceSpan>
  readonly nextBindingOrdinal?: { value: number }
  readonly executableFunction?: DeclarationId
  readonly executableOwner?: DeclarationIndex.CanonicalId
  readonly executableSites?: ReadonlyMap<SyntaxTree.Node, number>
}

const unsafeCallAuthorized = (
  resolution: ResolutionContext | undefined,
  call: SyntaxTree.Node,
): boolean =>
  resolution !== undefined &&
  ((resolution.unsafeSpans ?? []).some(
    (span) =>
      span.sourceId === call.span.sourceId &&
      span.start <= call.span.start &&
      span.end >= call.span.end,
  ) ||
    (resolution.unsafeCallSpans ?? []).some(
      (span) =>
        span.sourceId === call.span.sourceId &&
        span.start === call.span.start &&
        span.end === call.span.end,
    ))

const unsafeCallDiagnostic = (
  unsafe: boolean,
  spelling: string,
  call: SyntaxTree.Node,
  resolution: ResolutionContext | undefined,
): Diagnostic.Diagnostic | undefined =>
  unsafe && !unsafeCallAuthorized(resolution, call)
    ? Diagnostic.missingUnsafeBoundary(spelling, call.span)
    : undefined

/** Whether a borrow-shaped value is visibly backed only by program-lifetime immutable data. */
const isStaticallyDetachedFailure = (
  expression: ExpressionFact,
  index: DeclarationIndex.Index,
): boolean => {
  if (
    expression.type._tag === 'Available' &&
    !DeclarationIndex.containsLexicalBorrow(index, expression.type.type)
  )
    return true
  switch (expression._tag) {
    case 'StaticText':
      return expression.data !== undefined
    case 'Constant':
      return expression.value?._tag === 'String'
    case 'Grouped':
      return isStaticallyDetachedFailure(expression.expression, index)
    case 'Move':
      return isStaticallyDetachedFailure(expression.subject, index)
    case 'StructLiteral':
      return expression.fields.every((field) =>
        isStaticallyDetachedFailure(field.initializer.expression, index),
      )
    case 'ArrayLiteral':
      return expression.elements.every((element) =>
        isStaticallyDetachedFailure(element.expression, index),
      )
    default:
      return false
  }
}

const analyzeStatements = (
  context: BodyContext,
  blockNode: SyntaxTree.Node,
  initialScope: Scope,
  loopStack: ReadonlyArray<Hir.LoopId> = Object.freeze([]),
): ReadonlyArray<StatementFact> => {
  const facts: Array<StatementFact> = []
  let scope = initialScope
  const blockBindings = new Map<string, SourceSpan.SourceSpan>()

  const nextRegion = (): Hir.RegionId => {
    const region = Object.freeze({
      _tag: 'HirRegion' as const,
      function: context.declaration.id,
      ordinal: (context.regionBase ?? 0) + context.regions.length,
    })
    context.regions.push(region)
    return region
  }

  const analyzePatternSelection = (
    element: SyntaxTree.Node,
    selectionScope: Scope,
  ): PatternSelectionFact => {
    const initializerNode = statementExpressionNode(element)
    const initializer = analyzeExpression(
      context.source,
      initializerNode,
      context.declarations,
      context.declaration,
      selectionScope,
      context.resolution,
      undefined,
      true,
    )
    if (initializer === undefined) {
      throw new RangeError(`Semantic analysis cannot analyze ${initializerNode.kind}`)
    }
    context.diagnostics.push(...initializer.diagnostics)
    const access: Match.Access =
      initializer.fact._tag === 'Move'
        ? 'Move'
        : initializer.fact._tag === 'Borrow'
          ? initializer.fact.access
          : 'Copy'
    const subject =
      initializer.fact._tag === 'Move' || initializer.fact._tag === 'Borrow'
        ? initializer.fact.subject
        : initializer.fact
    const id: Match.MatchId = Object.freeze({
      _tag: 'MatchId',
      function: context.declaration.id,
      span: element.span,
    })
    const arm: Match.ArmId = Object.freeze({ _tag: 'MatchArmId', match: id, ordinal: 0 })
    const patternNode =
      SyntaxTree.directNode(element, 'ErrorPattern') ??
      SyntaxTree.directNode(element, 'NominalPattern') ??
      SyntaxTree.directNode(element, 'BindingPattern') ??
      SyntaxTree.directNode(element, 'UniversalPattern')
    if (patternNode === undefined) throw new RangeError('Pattern statement requires a pattern')
    const pattern = analyzePattern(
      context.source,
      patternNode,
      arm,
      access,
      selectionScope,
      context.resolution,
      context.declaration,
      { pattern: 0, binding: 0, invalid: false },
    )
    context.diagnostics.push(...pattern.diagnostics)
    const members = subject.type._tag === 'Available' ? Match.membersOf(subject.type.type) : []
    const member =
      pattern.fact._tag === 'NominalPattern' || pattern.fact._tag === 'TypePattern'
        ? pattern.fact.member
        : undefined
    if (
      member !== undefined &&
      subject.type._tag === 'Available' &&
      !members.some((candidate) => Type.equals(candidate, member))
    ) {
      context.diagnostics.push(
        Diagnostic.matchMemberNotInScrutinee(
          Type.encode(member),
          Type.encode(subject.type.type),
          pattern.fact.syntax.span,
        ),
      )
    }
    const coverage = Match.cover(
      members,
      Object.freeze([
        Object.freeze({
          ...(member === undefined ? {} : { member }),
          universal: pattern.fact._tag === 'UniversalPattern',
          guarded: false,
        }),
      ]),
    )
    const complete = pattern.fact._tag === 'UniversalPattern' ? true : pattern.fact.complete
    return Object.freeze({
      _tag: 'PatternSelection',
      id,
      arm,
      access,
      source: initializer.fact,
      subject,
      members: Object.freeze(members),
      pattern: pattern.fact,
      bindings: pattern.fact.bindings,
      irrefutable: coverage.exhaustive && complete,
      loanEnd: element.kind === 'PatternBindingStatement' ? blockNode.span : element.span,
      syntax: element,
    })
  }

  const analyzeConditional = (
    element: SyntaxTree.Node,
    armScope: Scope,
    armLoopStack: ReadonlyArray<Hir.LoopId>,
  ): StatementFact => {
    const region = nextRegion()
    const conditionNode = statementExpressionNode(element)
    const condition = analyzeExpression(
      context.source,
      conditionNode,
      context.declarations,
      context.declaration,
      armScope,
      context.resolution,
    )
    if (condition === undefined) {
      throw new RangeError(`Semantic analysis cannot analyze ${conditionNode.kind}`)
    }
    context.diagnostics.push(...condition.diagnostics)
    if (condition.fact.type._tag === 'Available' && condition.fact.type.type !== 'bool') {
      context.diagnostics.push(
        Diagnostic.conditionNotBool(
          Type.encode(condition.fact.type.type),
          condition.fact.syntax.span,
        ),
      )
    }

    const arms = SyntaxTree.directNodes(element, 'Block')
    const firstArm = arms.at(0)
    const taken =
      firstArm === undefined ? [] : analyzeStatements(context, firstArm, armScope, armLoopStack)
    const chained = SyntaxTree.directNode(element, 'ConditionalStatement')
    const otherwiseArm = arms.at(1)
    const otherwise =
      chained !== undefined
        ? [analyzeConditional(chained, armScope, armLoopStack)]
        : otherwiseArm === undefined
          ? []
          : analyzeStatements(context, otherwiseArm, armScope, armLoopStack)
    return Object.freeze({
      _tag: 'IfStatement',
      condition: condition.fact,
      taken: Object.freeze([...taken]),
      otherwise: Object.freeze([...otherwise]),
      region,
      syntax: element,
    })
  }

  const analyzePatternConditional = (
    element: SyntaxTree.Node,
    armScope: Scope,
    armLoopStack: ReadonlyArray<Hir.LoopId>,
  ): StatementFact => {
    const region = nextRegion()
    const selection = analyzePatternSelection(element, armScope)
    const takenScope: Scope = Object.freeze({
      parameters: armScope.parameters,
      bindings: armScope.bindings,
      patternBindings: Object.freeze([...armScope.patternBindings, ...selection.bindings]),
    })
    const arms = SyntaxTree.directNodes(element, 'Block')
    const firstArm = arms.at(0)
    const taken =
      firstArm === undefined ? [] : analyzeStatements(context, firstArm, takenScope, armLoopStack)
    const chained =
      SyntaxTree.directNode(element, 'ConditionalStatement') ??
      SyntaxTree.directNode(element, 'PatternConditionalStatement')
    const otherwiseArm = arms.at(1)
    const otherwise =
      chained?.kind === 'PatternConditionalStatement'
        ? [analyzePatternConditional(chained, armScope, armLoopStack)]
        : chained !== undefined
          ? [analyzeConditional(chained, armScope, armLoopStack)]
          : otherwiseArm === undefined
            ? []
            : analyzeStatements(context, otherwiseArm, armScope, armLoopStack)
    return Object.freeze({
      _tag: 'IfLetStatement',
      selection,
      taken: Object.freeze([...taken]),
      otherwise: Object.freeze([...otherwise]),
      region,
      syntax: element,
    })
  }

  for (const element of blockNode.children) {
    if (!SyntaxTree.isNode(element)) continue

    if (element.kind === 'UnsafeStatement') {
      const region = nextRegion()
      const body = SyntaxTree.directNode(element, 'Block')
      const statements =
        body === undefined
          ? Object.freeze<StatementFact[]>([])
          : analyzeStatements(context, body, scope, loopStack)
      facts.push(
        Object.freeze({
          _tag: 'UnsafeStatement',
          statements,
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'BindingStatement') {
      const region = nextRegion()
      const bindingOrdinal = context.nextBindingOrdinal.value
      context.nextBindingOrdinal.value += 1
      const initializerNode = statementExpressionNode(element)
      const initializer = analyzeExpression(
        context.source,
        initializerNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        undefined,
        true,
      )
      if (initializer === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${initializerNode.kind}`)
      }
      context.diagnostics.push(...initializer.diagnostics)

      if (
        SyntaxTree.directToken(element, 'MutKeyword') !== undefined &&
        initializer.type !== undefined &&
        Type.isEffect(initializer.type)
      )
        context.diagnostics.push(Diagnostic.mutableEffectRecipe(element.span))

      const name = bindingName(context.source, element)
      const binding: BindingDeclarationFact = Object.freeze({
        _tag: 'BindingFact',
        id: Object.freeze({
          _tag: 'HirBinding',
          function: context.declaration.id,
          ordinal: bindingOrdinal,
        }),
        name,
        mutability:
          SyntaxTree.directToken(element, 'MutKeyword') === undefined ? 'Immutable' : 'Mutable',
        inferredType: initializer.fact.type,
        initializer: initializer.fact,
        syntax: element,
      })
      context.bindings.push(binding)
      facts.push(Object.freeze({ _tag: 'BindStatement', binding, region }))

      if (name._tag === 'Present') {
        const originalSpan = blockBindings.get(name.spelling)
        if (originalSpan === undefined) {
          blockBindings.set(name.spelling, name.token.span)
          scope = Object.freeze({
            parameters: scope.parameters,
            bindings: Object.freeze([...scope.bindings, binding]),
            patternBindings: scope.patternBindings,
          })
        } else {
          context.diagnostics.push(
            Diagnostic.rebindingName(name.spelling, originalSpan, name.token.span),
          )
        }
      }
      continue
    }

    if (element.kind === 'PatternBindingStatement') {
      const region = nextRegion()
      const selection = analyzePatternSelection(element, scope)
      if (selection.pattern._tag === 'UniversalPattern') {
        if (selection.subject.type._tag === 'Available')
          context.diagnostics.push(
            Diagnostic.expressionStatementResult(
              Presentation.type(
                selection.subject.type.type,
                context.source.id,
                context.resolution.scope,
              ),
              selection.pattern.syntax.span,
            ),
          )
      } else if (selection.pattern._tag !== 'UnavailablePattern' && !selection.irrefutable) {
        const selected = selection.pattern.member
        context.diagnostics.push(
          Diagnostic.refutableLetPattern(
            selection.subject.type._tag === 'Available'
              ? Presentation.type(
                  selection.subject.type.type,
                  context.source.id,
                  context.resolution.scope,
                )
              : '<unavailable>',
            selection.members
              .filter((member) => selected === undefined || !Type.equals(member, selected))
              .map(Type.encode),
            selection.pattern.syntax.span,
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'PatternBindStatement',
          selection,
          region,
          syntax: element,
        }),
      )
      for (const binding of selection.bindings) {
        if (binding.name._tag !== 'Present') continue
        const originalSpan = blockBindings.get(binding.name.spelling)
        if (originalSpan === undefined)
          blockBindings.set(binding.name.spelling, binding.name.token.span)
      }
      scope = Object.freeze({
        parameters: scope.parameters,
        bindings: scope.bindings,
        patternBindings: Object.freeze([...scope.patternBindings, ...selection.bindings]),
      })
      continue
    }

    if (element.kind === 'ExpressionStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      if (
        expression.type !== undefined &&
        !Type.equals(expression.type, Type.unit) &&
        !Type.isNever(expression.type)
      ) {
        context.diagnostics.push(
          Diagnostic.expressionStatementResult(
            Presentation.type(expression.type, context.source.id, context.resolution.scope),
            expressionNode.span,
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'ExpressionStatement',
          expression: expression.fact,
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'ConditionalStatement') {
      facts.push(analyzeConditional(element, scope, loopStack))
      continue
    }

    if (element.kind === 'PatternConditionalStatement') {
      facts.push(analyzePatternConditional(element, scope, loopStack))
      continue
    }

    if (element.kind === 'AssignmentStatement') {
      const region = nextRegion()
      const nodes = element.children.filter(
        (child): child is SyntaxTree.Node => SyntaxTree.isNode(child) && isExpressionNode(child),
      )
      const destinationNode = nodes.at(0)
      const valueNode = nodes.at(1)
      if (destinationNode === undefined || valueNode === undefined) {
        context.diagnostics.push(Diagnostic.invalidAssignmentPlace(element.span))
        continue
      }
      const destination = analyzeExpression(
        context.source,
        destinationNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (destination === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${destinationNode.kind}`)
      }
      context.diagnostics.push(...destination.diagnostics)
      const value = analyzeExpression(
        context.source,
        valueNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        destination.type,
      )
      if (value === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${valueNode.kind}`)
      }
      context.diagnostics.push(...value.diagnostics)
      const root = assignmentRoot(destination.fact)
      if (root === undefined) {
        if (SyntaxTree.isAvailableSyntax(destinationNode) && destination.diagnostics.length === 0) {
          context.diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
        }
      } else if (root._tag === 'BindingFact' && root.mutability === 'Immutable') {
        context.diagnostics.push(
          Diagnostic.immutableAssignment(
            root.name._tag === 'Present' ? root.name.spelling : '?',
            destinationNode.span,
          ),
        )
      } else if (
        root._tag === 'ParameterDeclaration' &&
        (root.declaredType._tag !== 'Resolved' ||
          !(
            (Type.isSlice(root.declaredType.type) || Type.isReference(root.declaredType.type)) &&
            root.declaredType.type.access === 'Exclusive'
          ) ||
          (destination.fact._tag !== 'IndexProjection' &&
            destination.fact._tag !== 'FieldProjection'))
      ) {
        context.diagnostics.push(Diagnostic.invalidAssignmentPlace(destinationNode.span))
      }
      const compatible =
        destination.type !== undefined &&
        value.type !== undefined &&
        typesCompatible(value.type, destination.type)
      if (destination.type !== undefined && value.type !== undefined && !compatible) {
        const expectedOrigin =
          root?._tag === 'BindingFact' ? root.initializer.syntax.span : destinationNode.span
        context.diagnostics.push(
          representationJoinDiagnostic(
            destination.type,
            value.type,
            expectedOrigin,
            valueNode.span,
            valueNode.span,
          ) ??
            unionConversionDiagnostic(value.type, destination.type, valueNode.span) ??
            Diagnostic.assignmentTypeMismatch(
              Type.encode(destination.type),
              Type.encode(value.type),
              valueNode.span,
            ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: 'WriteStatement',
          destination: destination.fact,
          ...(root === undefined ? {} : { root }),
          value: value.fact,
          compatible,
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'WhileStatement') {
      const region = nextRegion()
      const loop = Object.freeze({
        _tag: 'HirLoop' as const,
        function: context.declaration.id,
        ordinal: context.loops.length,
      })
      context.loops.push(loop)
      const conditionNode = statementExpressionNode(element)
      const condition = analyzeExpression(
        context.source,
        conditionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (condition === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${conditionNode.kind}`)
      }
      context.diagnostics.push(...condition.diagnostics)
      if (condition.fact.type._tag === 'Available' && condition.fact.type.type !== 'bool') {
        context.diagnostics.push(
          Diagnostic.conditionNotBool(Type.encode(condition.fact.type.type), conditionNode.span),
        )
      }
      const bodyNode = SyntaxTree.directNode(element, 'Block')
      const body =
        bodyNode === undefined
          ? []
          : analyzeStatements(context, bodyNode, scope, Object.freeze([...loopStack, loop]))
      const parent = loopStack.at(-1)
      facts.push(
        Object.freeze({
          _tag: 'WhileStatement',
          loop,
          ...(parent === undefined ? {} : { parent }),
          condition: condition.fact,
          body: Object.freeze([...body]),
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'BreakStatement' || element.kind === 'ContinueStatement') {
      const region = nextRegion()
      const target = loopStack.at(-1)
      if (target === undefined) {
        context.diagnostics.push(
          Diagnostic.transferOutsideLoop(
            element.kind === 'BreakStatement' ? 'break' : 'continue',
            element.span,
          ),
        )
      }
      facts.push(
        Object.freeze({
          _tag: element.kind,
          ...(target === undefined ? {} : { target }),
          region,
          syntax: element,
        }),
      )
      continue
    }

    if (element.kind === 'ReturnStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
        !context.effectBlock && context.declaration.returnType._tag === 'Resolved'
          ? context.declaration.returnType.type
          : undefined,
        !context.effectBlock && DeclarationIndex.returnedBorrow(context.declaration) !== undefined,
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      if (
        !context.effectBlock &&
        expression.type !== undefined &&
        Type.isCallable(expression.type) &&
        expression.type.mode !== 'Shared' &&
        !concreteCallableIdentity(expression.fact)
      ) {
        context.diagnostics.push(Diagnostic.unknownOwnedCallableReturn(expressionNode.span))
      }
      facts.push(
        Object.freeze({
          _tag: 'ReturnStatement',
          expression: expression.fact,
          region,
          syntax: element,
        }),
      )
      break
    }

    if (element.kind === 'FailStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined)
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      context.diagnostics.push(...expression.diagnostics)
      const failure =
        expression.type !== undefined &&
        (Type.isRuntimeConcrete(expression.type) ||
          (Type.isParameter(expression.type) && expression.type.kind === 'Value'))
          ? expression.type
          : undefined
      if (!context.effectBlock && context.declaration.functionKind !== 'Effect')
        context.diagnostics.push(Diagnostic.failOutsideEffect(element.span))
      if (expression.type !== undefined && failure === undefined)
        context.diagnostics.push(
          Diagnostic.invalidFailureType(Type.encode(expression.type), expressionNode.span),
        )
      if (
        failure !== undefined &&
        DeclarationIndex.containsLexicalBorrow(context.resolution.index, failure) &&
        !isStaticallyDetachedFailure(expression.fact, context.resolution.index)
      )
        context.diagnostics.push(
          Diagnostic.providerBackedFailure(Type.encode(failure), expressionNode.span),
        )
      if (
        !context.effectBlock &&
        failure !== undefined &&
        !(Type.isParameter(failure)
          ? Type.failureMemberParameters(context.declaration.failureRow.row).some((parameter) =>
              Type.equals(parameter, failure),
            )
          : context.declaration.failureRow.failures.some((candidate) =>
              Type.equals(candidate, failure),
            ))
      )
        context.diagnostics.push(
          Diagnostic.undeclaredFailure(Type.encode(failure), expressionNode.span),
        )
      facts.push(
        Object.freeze({
          _tag: 'FailStatement',
          expression: expression.fact,
          ...(failure === undefined ? {} : { failure }),
          transfer: SyntaxTree.directToken(element, 'MoveKeyword') === undefined ? 'Copy' : 'Move',
          region,
          syntax: element,
        }),
      )
      break
    }

    if (element.kind === 'DropStatement') {
      const region = nextRegion()
      const expressionNode = statementExpressionNode(element)
      const expression = analyzeExpression(
        context.source,
        expressionNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
      )
      if (expression === undefined)
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      context.diagnostics.push(...expression.diagnostics)
      facts.push(
        Object.freeze({
          _tag: 'DropStatement',
          expression: expression.fact,
          region,
          syntax: element,
        }),
      )
    }
  }

  return Object.freeze(facts)
}

interface ReturnFlow {
  readonly fallsThrough: boolean
  readonly returns: ReadonlyArray<Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>>
}

const expressionNever = (expression: ExpressionFact): boolean =>
  expression.type._tag === 'Available' && Type.isNever(expression.type.type)

const implicitReturn = (
  statement: Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>,
): boolean => SyntaxTree.directToken(statement.syntax, 'ReturnKeyword') === undefined

/**
 * Computes source-level return reachability. Parser-created zero-width unit returns preserve a
 * recoverable terminal node, but remain ordinary fallthrough for contract checking.
 */
const returnFlowOf = (
  body: ReadonlyArray<StatementFact>,
  implicitReturnFallsThrough = true,
): ReturnFlow => {
  const returns: Array<Extract<StatementFact, { readonly _tag: 'ReturnStatement' }>> = []
  let fallsThrough = true
  for (const statement of body) {
    if (!fallsThrough) break
    if (statement._tag === 'ReturnStatement') {
      if (implicitReturn(statement)) {
        fallsThrough = implicitReturnFallsThrough
      } else {
        returns.push(statement)
        fallsThrough = false
      }
      continue
    }
    if (
      statement._tag === 'FailStatement' ||
      statement._tag === 'BreakStatement' ||
      statement._tag === 'ContinueStatement'
    ) {
      fallsThrough = false
      continue
    }
    if (statement._tag === 'UnsafeStatement') {
      const nested = returnFlowOf(statement.statements, implicitReturnFallsThrough)
      returns.push(...nested.returns)
      fallsThrough = nested.fallsThrough
      continue
    }
    if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
      if (statement._tag === 'IfStatement' && expressionNever(statement.condition)) {
        fallsThrough = false
        continue
      }
      const taken = returnFlowOf(statement.taken, implicitReturnFallsThrough)
      const otherwise = returnFlowOf(statement.otherwise, implicitReturnFallsThrough)
      returns.push(...taken.returns, ...otherwise.returns)
      fallsThrough = taken.fallsThrough || otherwise.fallsThrough
      continue
    }
    if (statement._tag === 'WhileStatement') {
      if (expressionNever(statement.condition)) {
        fallsThrough = false
        continue
      }
      returns.push(...returnFlowOf(statement.body, implicitReturnFallsThrough).returns)
      fallsThrough = true
      continue
    }
    if (statement._tag === 'BindStatement') {
      fallsThrough = !expressionNever(statement.binding.initializer)
      continue
    }
    if (statement._tag === 'PatternBindStatement') {
      fallsThrough = !expressionNever(statement.selection.subject)
      continue
    }
    if (statement._tag === 'ExpressionStatement' || statement._tag === 'DropStatement') {
      fallsThrough = !expressionNever(statement.expression)
      continue
    }
    if (statement._tag === 'WriteStatement')
      fallsThrough = !expressionNever(statement.destination) && !expressionNever(statement.value)
  }
  return Object.freeze({ fallsThrough, returns: Object.freeze(returns) })
}

/** Keeps only statements that can execute, treating an implicit unit completion as a real return. */
const executableStatements = (body: ReadonlyArray<StatementFact>): ReadonlyArray<StatementFact> => {
  const reachable: Array<StatementFact> = []
  for (const statement of body) {
    reachable.push(statement)
    if (!returnFlowOf([statement], false).fallsThrough) break
  }
  return Object.freeze(reachable)
}

const analyzeFunctionBody = (
  source: SourceFile.SourceFile,
  declaration: DeclarationFact,
  declarations: ReadonlyArray<DeclarationFact>,
  resolution: ResolutionContext,
): FunctionAnalysis => {
  const blockNode = childNode(declaration.syntax, 'Block')
  const unsafeSpans: Array<SourceSpan.SourceSpan> = []
  const collectUnsafeSpans = (node: SyntaxTree.Node): void => {
    if (node.kind === 'UnsafeStatement') unsafeSpans.push(node.span)
    for (const child of node.children) if (SyntaxTree.isNode(child)) collectUnsafeSpans(child)
  }
  collectUnsafeSpans(declaration.syntax)
  const nextBindingOrdinal = { value: 0 }
  const bodyResolution: ResolutionContext = Object.freeze({
    ...resolution,
    unsafeSpans: Object.freeze(unsafeSpans),
    nextBindingOrdinal,
    executableFunction: declaration.id,
    ...(declaration.canonical._tag === 'Canonical'
      ? { executableOwner: declaration.canonical.id }
      : {}),
    executableSites: executableSites(declaration.syntax),
  })
  const context: BodyContext = {
    source,
    declaration,
    declarations,
    bindings: [],
    diagnostics: [],
    regions: [],
    loops: [],
    resolution: bodyResolution,
    nextBindingOrdinal,
  }
  const statements = analyzeStatements(
    context,
    blockNode,
    Object.freeze({ parameters: declaration.parameters, bindings: [], patternBindings: [] }),
  )
  const returnedBorrow = DeclarationIndex.returnedBorrow(declaration)

  const bindingOrigins = new Map<number, DeclarationIndex.ParameterFact | undefined>()
  const originOf = (
    expression: ExpressionFact,
    patternOrigins: ReadonlyMap<string, DeclarationIndex.ParameterFact | undefined> = new Map(),
  ): DeclarationIndex.ParameterFact | undefined => {
    if (expression._tag === 'Grouped') return originOf(expression.expression, patternOrigins)
    if (expression._tag === 'Identifier') {
      if (expression.reference._tag === 'Resolved') return expression.reference.parameter
      if (expression.reference._tag === 'ResolvedBinding') {
        const ordinal = expression.reference.binding.id.ordinal
        if (!bindingOrigins.has(ordinal)) {
          bindingOrigins.set(
            ordinal,
            originOf(expression.reference.binding.initializer, patternOrigins),
          )
        }
        return bindingOrigins.get(ordinal)
      }
      if (expression.reference._tag === 'ResolvedPattern') {
        const id = expression.reference.binding.id
        return patternOrigins.get(`${id.arm.match.span.start}:${id.arm.ordinal}:${id.ordinal}`)
      }
      return undefined
    }
    if (expression._tag === 'Borrow') {
      if (expression.formation._tag === 'Unavailable') return undefined
      const root = expression.formation.root
      if (root._tag === 'ParameterRoot') return root.parameter
      if (root._tag === 'BindingRoot') return originOf(root.binding.initializer, patternOrigins)
      if (root._tag === 'TemporaryRoot') return undefined
      const id = root.binding.id
      return patternOrigins.get(`${id.arm.match.span.start}:${id.arm.ordinal}:${id.ordinal}`)
    }
    if (expression._tag === 'FieldProjection' || expression._tag === 'IndexProjection') {
      return originOf(expression.subject, patternOrigins)
    }
    if (expression._tag === 'Call' && expression.reference._tag === 'Resolved') {
      const argument = returnedBorrowArgument(expression)
      return argument === undefined ? undefined : originOf(argument.expression, patternOrigins)
    }
    if (expression._tag === 'Call' && expression.reference._tag === 'ResolvedBuiltin') {
      const ordinal = expression.reference.returnedBorrowParameter
      const argument = ordinal === undefined ? undefined : expression.arguments.at(ordinal)
      return argument === undefined ? undefined : originOf(argument.expression, patternOrigins)
    }
    if (expression._tag === 'Match') {
      const scrutinee = originOf(expression.scrutinee, patternOrigins)
      const origins = expression.arms
        .filter((arm) => arm.reachable)
        .map((arm) => {
          const armOrigins = new Map(patternOrigins)
          for (const binding of arm.bindings) {
            const id = binding.id
            armOrigins.set(`${id.arm.match.span.start}:${id.arm.ordinal}:${id.ordinal}`, scrutinee)
          }
          return originOf(arm.result, armOrigins)
        })
      if (origins.some((origin) => origin === undefined)) return undefined
      const first = origins.at(0)
      return first !== undefined &&
        origins.every((origin) => origin?.id.ordinal === first.id.ordinal)
        ? first
        : undefined
    }
    return undefined
  }

  if (returnedBorrow !== undefined) {
    const isBorrowFreeReturn = (expression: ExpressionFact): boolean => {
      if (expression._tag === 'Grouped') return isBorrowFreeReturn(expression.expression)
      if (expression._tag === 'StaticText') return expression.data?.kind === 'Text'
      if (expression._tag !== 'Call' || expression.reference._tag !== 'Resolved') return false
      const argument = returnedBorrowArgument(expression)
      return (
        argument === undefined &&
        expression.type._tag === 'Available' &&
        Type.containsViewBorrow(expression.type.type) &&
        expression.arguments.every(
          (candidate) =>
            candidate.type._tag === 'Available' && !Type.containsViewBorrow(candidate.type.type),
        )
      )
    }
    const validateReturns = (body: ReadonlyArray<StatementFact>): void => {
      for (const statement of body) {
        if (statement._tag === 'ReturnStatement') {
          const origin = originOf(statement.expression)
          if (
            origin?.id.ordinal !== returnedBorrow.parameter.id.ordinal &&
            !isBorrowFreeReturn(statement.expression)
          ) {
            context.diagnostics.push(
              Diagnostic.invalidReturnedBorrowOrigin(statement.expression.syntax.span),
            )
          }
        } else if (statement._tag === 'UnsafeStatement') {
          validateReturns(statement.statements)
        } else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
          validateReturns(statement.taken)
          validateReturns(statement.otherwise)
        } else if (statement._tag === 'WhileStatement') {
          validateReturns(statement.body)
        }
      }
    }
    validateReturns(statements)
  }

  type Terminal = Extract<StatementFact, { _tag: 'ReturnStatement' | 'FailStatement' }>
  const terminalOf = (body: ReadonlyArray<StatementFact>): Terminal | undefined => {
    for (const statement of [...body].reverse()) {
      if (statement._tag === 'ReturnStatement' || statement._tag === 'FailStatement')
        return statement
      if (statement._tag === 'UnsafeStatement') {
        const nested = terminalOf(statement.statements)
        if (nested !== undefined) return nested
      } else if (statement._tag === 'IfStatement' || statement._tag === 'IfLetStatement') {
        const nested = terminalOf(statement.otherwise) ?? terminalOf(statement.taken)
        if (nested !== undefined) return nested
      }
    }
    return undefined
  }
  const terminal = terminalOf(statements)
  if (terminal === undefined)
    throw new RangeError('Semantic analysis expected a terminal statement')
  const expression = terminal.expression
  const returnFlow = returnFlowOf(statements)
  let validReturnContract = declaration.returnType._tag === 'Resolved'
  if (declaration.returnType._tag === 'Resolved') {
    for (const returned of returnFlow.returns) {
      if (returned.expression.type._tag !== 'Available') {
        validReturnContract = false
        continue
      }
      const actual = returned.expression.type.type
      if (declaredReturnTypesCompatible(declaration, returned.expression)) continue
      validReturnContract = false
      context.diagnostics.push(
        representationJoinDiagnostic(
          declaration.returnType.type,
          actual,
          declaration.returnType.syntax.span,
          returned.expression.syntax.span,
          returned.expression.syntax.span,
        ) ??
          unionConversionDiagnostic(
            actual,
            declaration.returnType.type,
            returned.expression.syntax.span,
          ) ??
          Diagnostic.returnTypeMismatch(
            Type.encode(declaration.returnType.type),
            Type.encode(actual),
            returned.expression.syntax.span,
          ),
      )
    }
    if (returnFlow.fallsThrough && !Type.equals(declaration.returnType.type, Type.unit)) {
      validReturnContract = false
      context.diagnostics.push(
        Diagnostic.missingReturn(
          Type.encode(declaration.returnType.type),
          SyntaxTree.directToken(blockNode, 'RightBrace')?.span ?? blockNode.span,
        ),
      )
    }
  }
  const returnCompatibility = validReturnContract ? compatible : unavailableCompatibility

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionFact',
      declaration,
      statements,
      bindings: Object.freeze([...context.bindings]),
      regionOrder: Object.freeze([...context.regions]),
      returnedExpression: expression,
      returnCompatibility,
      ...(returnedBorrow === undefined ? {} : { returnedBorrow }),
    }),
    diagnostics: Object.freeze([...context.diagnostics]),
  })
}

const hirReference = (
  reference: ParameterReferenceFact,
  type: ExpressionTypeFact,
  span: SourceSpan.SourceSpan,
): Hir.Expression => {
  if (reference._tag === 'Resolved' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'ParameterReference',
      parameter: reference.parameter.id,
      type: type.type,
      span,
    })
  }
  if (reference._tag === 'ResolvedBinding' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'BindingReference',
      binding: reference.binding.id,
      type: type.type,
      span,
    })
  }
  if (reference._tag === 'ResolvedPattern' && type._tag === 'Available') {
    return Object.freeze({
      _tag: 'PatternBindingReference',
      binding: reference.binding.id,
      type: type.type,
      span,
    })
  }
  return Object.freeze({
    _tag: 'Unavailable',
    span,
    ...(reference._tag === 'Missing' && reference.cause !== undefined
      ? { cause: reference.cause }
      : {}),
  })
}

const hirPatternSelection = (selection: PatternSelectionFact): Hir.PatternSelection => {
  const member =
    selection.pattern._tag === 'NominalPattern' || selection.pattern._tag === 'TypePattern'
      ? selection.pattern.member
      : undefined
  return Object.freeze({
    id: selection.id,
    arm: selection.arm,
    access: selection.access,
    subject: hirExpression(selection.subject),
    members: selection.members,
    ...(member === undefined ? {} : { member }),
    universal: selection.pattern._tag === 'UniversalPattern',
    bindings: Object.freeze(
      selection.bindings.flatMap(
        (binding): ReadonlyArray<Hir.PatternBinding> =>
          binding.type._tag === 'Available'
            ? [
                Object.freeze({
                  id: binding.id,
                  ...(binding.name._tag === 'Present' ? { name: binding.name.spelling } : {}),
                  ...(binding.field === undefined ? {} : { field: binding.field.id }),
                  path: binding.path,
                  type: binding.type.type,
                  access: binding.access,
                  span: binding.syntax.span,
                }),
              ]
            : [],
      ),
    ),
    cleanup: selection.pattern.omitted,
    irrefutable: selection.irrefutable,
    span: selection.syntax.span,
  })
}

const hirEffectStatements = (
  facts: ReadonlyArray<StatementFact>,
  resultType?: SemanticType,
): ReadonlyArray<Hir.Statement> =>
  Object.freeze(
    facts.map((statement): Hir.Statement => {
      if (statement._tag === 'UnsafeStatement')
        return Object.freeze({
          _tag: 'Unsafe',
          statements: hirEffectStatements(statement.statements, resultType),
          region: statement.region,
          span: statement.syntax.span,
        })
      if (statement._tag === 'BindStatement')
        return Object.freeze({
          _tag: 'Bind',
          binding: statement.binding.id,
          name:
            statement.binding.name._tag === 'Present' ? statement.binding.name.spelling : undefined,
          mutability: statement.binding.mutability,
          initializer: hirExpression(statement.binding.initializer),
          region: statement.region,
          span: statement.binding.syntax.span,
        })
      if (statement._tag === 'PatternBindStatement')
        return Object.freeze({
          _tag: 'PatternBind',
          selection: hirPatternSelection(statement.selection),
          region: statement.region,
          span: statement.syntax.span,
        })
      if (statement._tag === 'ExpressionStatement')
        return Object.freeze({
          _tag: 'Evaluate',
          expression: hirExpression(statement.expression),
          region: statement.region,
          span: statement.syntax.span,
        })
      if (statement._tag === 'IfStatement')
        return Object.freeze({
          _tag: 'If',
          condition: hirExpression(statement.condition),
          taken: hirEffectStatements(statement.taken, resultType),
          otherwise: hirEffectStatements(statement.otherwise, resultType),
          region: statement.region,
          span: statement.syntax.span,
        })
      if (statement._tag === 'IfLetStatement')
        return Object.freeze({
          _tag: 'IfLet',
          selection: hirPatternSelection(statement.selection),
          taken: hirEffectStatements(statement.taken, resultType),
          otherwise: hirEffectStatements(statement.otherwise, resultType),
          region: statement.region,
          span: statement.syntax.span,
        })
      if (statement._tag === 'WriteStatement') {
        const place =
          statement.root?._tag === 'BindingFact'
            ? hirAssignmentWritePlace(statement.destination, statement.root)
            : statement.root?._tag === 'ParameterDeclaration'
              ? hirBorrowedWritePlace(statement.destination, statement.root)
              : undefined
        if (place === undefined || !statement.compatible)
          return Object.freeze({
            _tag: 'UnavailableStatement',
            region: statement.region,
            span: statement.syntax.span,
          })
        return Object.freeze({
          _tag: 'Write',
          place,
          value: hirExpectedExpression(statement.value, place.type, 'Assignment', place.span),
          region: statement.region,
          span: statement.syntax.span,
        })
      }
      if (statement._tag === 'WhileStatement')
        return Object.freeze({
          _tag: 'While',
          loop: statement.loop,
          ...(statement.parent === undefined ? {} : { parent: statement.parent }),
          condition: hirExpression(statement.condition),
          body: hirEffectStatements(statement.body, resultType),
          region: statement.region,
          span: statement.syntax.span,
        })
      if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement')
        return statement.target === undefined
          ? Object.freeze({
              _tag: 'UnavailableStatement',
              region: statement.region,
              span: statement.syntax.span,
            })
          : Object.freeze({
              _tag: statement._tag === 'BreakStatement' ? 'Break' : 'Continue',
              target: statement.target,
              region: statement.region,
              span: statement.syntax.span,
            })
      if (statement._tag === 'ReturnStatement')
        return Object.freeze({
          _tag: 'Return',
          expression:
            resultType === undefined
              ? hirExpression(statement.expression)
              : hirExpectedExpression(
                  statement.expression,
                  resultType,
                  'Return',
                  statement.syntax.span,
                ),
          region: statement.region,
          span: statement.expression.syntax.span,
        })
      if (statement._tag === 'DropStatement')
        return Object.freeze({
          _tag: 'Drop',
          expression:
            statement.expression.type._tag === 'Available'
              ? Object.freeze({
                  _tag: 'Move' as const,
                  subject: hirExpression(statement.expression),
                  type: statement.expression.type.type,
                  span: statement.expression.syntax.span,
                })
              : hirExpression(statement.expression),
          region: statement.region,
          span: statement.syntax.span,
        })
      if (statement.failure === undefined)
        return Object.freeze({
          _tag: 'UnavailableStatement',
          region: statement.region,
          span: statement.syntax.span,
        })
      return Object.freeze({
        _tag: 'Fail',
        expression:
          statement.transfer === 'Move'
            ? Object.freeze({
                _tag: 'Move',
                subject: hirExpression(statement.expression),
                type:
                  statement.expression.type._tag === 'Available'
                    ? statement.expression.type.type
                    : statement.failure,
                span: statement.expression.syntax.span,
              })
            : hirExpression(statement.expression),
        failure: statement.failure,
        transfer: statement.transfer,
        region: statement.region,
        span: statement.syntax.span,
      })
    }),
  )

const hirCallableTarget = (reference: CallReferenceFact): Hir.CallableTarget | undefined => {
  if (reference._tag === 'ResolvedBuiltin')
    return Object.freeze({
      _tag: 'BuiltinCallableTarget',
      actor: reference.actor,
      operation: reference.operation,
      intrinsic: reference.intrinsic,
    })
  if (reference._tag === 'Resolved' && reference.declaration.canonical._tag === 'Canonical')
    return Object.freeze({
      _tag: 'DeclarationCallableTarget',
      declaration: reference.declaration.canonical.id,
    })
  return undefined
}

const hirExpression = (fact: ExpressionFact, borrow?: Hir.BorrowId): Hir.Expression => {
  if (fact._tag === 'ShortCircuit') {
    const left = fact.arguments.at(0)
    const right = fact.arguments.at(1)
    if (left === undefined || right === undefined || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    const loweredLeft = hirExpression(left.expression)
    const loweredRight = hirExpression(right.expression)
    return loweredLeft._tag === 'Unavailable' || loweredRight._tag === 'Unavailable'
      ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
      : Object.freeze({
          _tag: 'ShortCircuit',
          operator: fact.operator,
          left: loweredLeft,
          right: loweredRight,
          type: fact.type.type,
          span: fact.syntax.span,
        })
  }
  if (fact._tag === 'Integer') {
    return fact.integer._tag === 'Available'
      ? Object.freeze({
          _tag: 'IntegerLiteral',
          value: fact.integer.value,
          type: fact.integer.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Floating') {
    return fact.floating._tag === 'Available'
      ? Object.freeze({
          _tag: 'FloatingLiteral',
          bits: fact.floating.bits,
          spelling: fact.floating.spelling,
          type: fact.floating.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'StaticText') {
    return fact.data === undefined
      ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
      : fact.data.kind === 'Text'
        ? Object.freeze({
            _tag: 'StaticStringLiteral',
            data: fact.data,
            type: Type.string,
            span: fact.syntax.span,
          })
        : Object.freeze({
            _tag: 'StaticByteViewLiteral',
            data: fact.data,
            type: Type.slice('Shared', 'u8'),
            span: fact.syntax.span,
          })
  }
  if (fact._tag === 'Unit') {
    return Object.freeze({
      _tag: 'UnitLiteral',
      type: Type.unit,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Boolean') {
    return fact.type._tag === 'Available'
      ? Object.freeze({
          _tag: 'BooleanLiteral',
          value: fact.value,
          type: fact.type.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Character') {
    return fact.type._tag === 'Available' && fact.value !== undefined
      ? Object.freeze({
          _tag: 'CharacterLiteral',
          value: fact.value,
          type: fact.type.type,
          span: fact.syntax.span,
        })
      : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Constant') {
    if (fact.value?._tag === 'Character')
      return Object.freeze({
        _tag: 'CharacterLiteral',
        value: fact.value.value,
        type: 'char',
        span: fact.syntax.span,
      })
    if (fact.value?._tag === 'Boolean')
      return Object.freeze({
        _tag: 'BooleanLiteral',
        value: fact.value.value,
        type: 'bool',
        span: fact.syntax.span,
      })
    if (fact.value?._tag === 'Integer')
      return Object.freeze({
        _tag: 'IntegerLiteral',
        value: fact.value.value,
        type: fact.value.type,
        ...(fact.declaration.canonical._tag === 'Canonical'
          ? { constant: fact.declaration.canonical.id }
          : {}),
        ...(fact.value.target === undefined ? {} : { targetConstant: fact.value.target }),
        span: fact.syntax.span,
      })
    if (fact.value?._tag === 'Floating')
      return Object.freeze({
        _tag: 'FloatingLiteral',
        bits: fact.value.bits,
        spelling: fact.value.spelling,
        type: fact.value.type,
        span: fact.syntax.span,
      })
    if (fact.value?._tag === 'String')
      return Object.freeze({
        _tag: 'StaticStringLiteral',
        data: fact.value.data,
        type: Type.string,
        span: fact.syntax.span,
      })
    return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  if (fact._tag === 'Identifier') {
    return hirReference(fact.reference, fact.type, fact.syntax.span)
  }
  if (fact._tag === 'Move') {
    const subject = hirExpression(fact.subject)
    if (subject._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return subject._tag === 'Unavailable'
        ? subject
        : Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'Move',
      subject:
        subject._tag === 'Project' || subject._tag === 'IndexPlace'
          ? Object.freeze({ ...subject, access: 'ConsumeRequested' as const })
          : subject,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'PlaceReplace') {
    const place =
      fact.root?._tag === 'BindingFact'
        ? hirAssignmentWritePlace(fact.destination, fact.root)
        : fact.root?._tag === 'ParameterDeclaration'
          ? hirBorrowedWritePlace(fact.destination, fact.root)
          : undefined
    if (
      place === undefined ||
      (fact.root?._tag === 'BindingFact' && fact.root.mutability !== 'Mutable') ||
      !fact.compatible ||
      fact.type._tag !== 'Available'
    ) {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'Replace',
      place,
      value: hirExpectedExpression(fact.value, place.type, 'Assignment', place.span),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectBlock') {
    if (fact.type._tag !== 'Available' || !Type.isEffect(fact.type.type))
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectBlock',
      site: fact.site,
      statements: hirEffectStatements(fact.statements, fact.type.type.success),
      captures: Object.freeze(
        fact.captures.map((capture) =>
          Object.freeze({
            ...(capture.reference._tag === 'BindingFact'
              ? { binding: capture.reference.id }
              : { parameter: capture.reference.id }),
            access: capture.access,
            span: capture.span,
          }),
        ),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Run') {
    const subject = hirExpression(fact.subject)
    if (subject._tag === 'Unavailable' || fact.type._tag !== 'Available')
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'Run',
      subject,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectResult') {
    const protected_ = hirExpression(fact.protected)
    if (
      protected_._tag === 'Unavailable' ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectResult',
      protected: protected_,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectCatch') {
    const protected_ = hirExpression(fact.protected)
    const handler = hirExpression(fact.handler)
    if (
      protected_._tag === 'Unavailable' ||
      handler._tag === 'Unavailable' ||
      fact.reference._tag !== 'ResolvedIntrinsicReference' ||
      fact.selected === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectCatch',
      intrinsic: fact.reference.operation.id,
      protected: protected_,
      handler,
      selected: fact.selected,
      protectedRow: fact.protectedRow,
      handlerRow: fact.handlerRow,
      residualRow: fact.residualRow,
      evidence: fact.evidence,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectBindRequirement') {
    const protected_ = hirExpression(fact.protected)
    if (
      protected_._tag === 'Unavailable' ||
      fact.provider === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectBindRequirement',
      protected: protected_,
      provider: Object.freeze({
        ...(fact.provider.reference._tag === 'BindingFact'
          ? { binding: fact.provider.reference.id }
          : { parameter: fact.provider.reference.id }),
        selected: fact.provider.selected,
        evidence: fact.provider.evidence,
        ...(fact.provider.capability === undefined ? {} : { capability: fact.provider.capability }),
        providerType: fact.provider.providerType,
        ...(fact.provider.witness === undefined ? {} : { witness: fact.provider.witness }),
        ...(fact.provider.role === undefined ? {} : { role: fact.provider.role }),
        selectionAccess: fact.provider.selectionAccess,
        captureAccess: fact.provider.captureAccess,
        span: fact.provider.span,
      }),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Match') {
    const scrutinee = hirExpression(fact.scrutinee)
    if (scrutinee._tag === 'Unavailable' || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    const target = fact.type.type
    return Object.freeze({
      _tag: 'Match',
      id: fact.id,
      access: fact.access,
      scrutinee,
      members: fact.members,
      arms: Object.freeze(
        fact.arms.map((arm) => {
          const member =
            arm.pattern._tag === 'NominalPattern' || arm.pattern._tag === 'TypePattern'
              ? arm.pattern.member
              : undefined
          return Object.freeze({
            id: arm.id,
            ...(member === undefined ? {} : { member }),
            universal: arm.pattern._tag === 'UniversalPattern',
            bindings: Object.freeze(
              arm.bindings.flatMap((binding) =>
                binding.type._tag === 'Available'
                  ? [
                      Object.freeze({
                        id: binding.id,
                        ...(binding.name._tag === 'Present' ? { name: binding.name.spelling } : {}),
                        ...(binding.field === undefined ? {} : { field: binding.field.id }),
                        path: binding.path,
                        type: binding.type.type,
                        access: binding.access,
                        span: binding.syntax.span,
                      }),
                    ]
                  : [],
              ),
            ),
            cleanup: arm.pattern.omitted,
            ...(arm.guard === undefined ? {} : { guard: hirExpression(arm.guard) }),
            result: Type.isUnion(target)
              ? hirExpectedExpression(arm.result, target, 'MatchArm', arm.syntax.span)
              : hirExpression(arm.result),
            before: arm.before,
            after: arm.after,
            reachable: arm.reachable,
            span: arm.syntax.span,
          })
        }),
      ),
      type: target,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'StructLiteral') {
    if (
      fact.target._tag !== 'Resolved' ||
      fact.type._tag !== 'Available' ||
      fact.fields.length !== fact.target.struct.fields.length
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.target._tag === 'Unavailable' && fact.target.cause !== undefined
          ? { cause: fact.target.cause }
          : {}),
      })
    }
    const substitution =
      Type.substitution(
        fact.target.struct.typeParameters.map((parameter) => parameter.type),
        fact.target.type.arguments,
      ) ?? new Map()
    return Object.freeze({
      _tag: 'Construct',
      nominal: fact.target.type,
      evaluationOrder: Object.freeze(
        fact.initializers.flatMap((initializer) =>
          initializer.state._tag === 'Resolved' ? [initializer.state.field.id] : [],
        ),
      ),
      fields: Object.freeze(
        fact.fields.map(({ field, initializer }) => {
          const value =
            field.declaredType._tag === 'Resolved'
              ? hirExpectedExpression(
                  initializer.expression,
                  Type.substitute(field.declaredType.type, substitution),
                  'StructField',
                  field.syntax.span,
                )
              : hirExpression(initializer.expression)
          return Object.freeze({ field: field.id, value })
        }),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'ArrayLiteral') {
    if (fact.state._tag !== 'Complete' || fact.type._tag !== 'Available') {
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    }
    return Object.freeze({
      _tag: 'ArrayConstruct',
      elements: Object.freeze(
        fact.elements.map((element) =>
          element.expected === undefined
            ? hirExpression(element.expression)
            : hirExpectedExpression(
                element.expression,
                element.expected,
                'ArrayElement',
                element.syntax.span,
              ),
        ),
      ),
      type: fact.state.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'FieldProjection') {
    if (fact.state._tag === 'SliceLength' && fact.type._tag === 'Available') {
      const slice = hirExpression(fact.subject)
      return slice._tag === 'Unavailable'
        ? slice
        : Object.freeze({
            _tag: 'SliceLength',
            slice,
            type: 'usize',
            span: fact.syntax.span,
          })
    }
    if (
      fact.nominal === undefined ||
      fact.state._tag !== 'Resolved' ||
      fact.type._tag !== 'Available'
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.state._tag === 'Unavailable' && fact.state.cause !== undefined
          ? { cause: fact.state.cause }
          : {}),
      })
    }
    return Object.freeze({
      _tag: 'Project',
      subject: hirExpression(fact.subject),
      nominal: fact.nominal,
      field: fact.state.field.id,
      access: 'CopyRead',
      ...(fact.borrowAccess === undefined ? {} : { borrowAccess: fact.borrowAccess }),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'IndexProjection') {
    if (
      fact.slice !== undefined &&
      fact.type._tag === 'Available' &&
      fact.bounds._tag === 'RuntimeSlice'
    ) {
      const slice = hirExpression(fact.subject)
      const index = hirExpression(fact.index)
      if (slice._tag === 'Unavailable' || index._tag === 'Unavailable') {
        return slice._tag === 'Unavailable' ? slice : index
      }
      return Object.freeze({
        _tag: 'SliceIndexPlace',
        slice,
        index,
        access: fact.slice.access,
        sourceType: fact.slice,
        type: fact.type.type,
        span: fact.syntax.span,
      })
    }
    if (
      fact.array === undefined ||
      fact.type._tag !== 'Available' ||
      (fact.bounds._tag !== 'Proven' && fact.bounds._tag !== 'Runtime')
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.bounds._tag === 'Invalid' ? { cause: fact.bounds.cause } : {}),
      })
    }
    const subject = hirExpression(fact.subject)
    const index = hirExpression(fact.index)
    if (subject._tag === 'Unavailable' || index._tag === 'Unavailable') {
      return subject._tag === 'Unavailable' ? subject : index
    }
    return Object.freeze({
      _tag: 'IndexPlace',
      subject,
      index,
      array: fact.array,
      access: fact.access,
      bounds: fact.bounds,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Borrow') {
    if (
      borrow === undefined ||
      fact.formation._tag === 'Unavailable' ||
      fact.type._tag !== 'Available' ||
      (!Type.isSlice(fact.type.type) && !Type.isReference(fact.type.type))
    ) {
      return Object.freeze({
        _tag: 'Unavailable',
        span: fact.syntax.span,
        ...(fact.formation._tag === 'Unavailable' && fact.formation.cause !== undefined
          ? { cause: fact.formation.cause }
          : {}),
      })
    }
    const root: Hir.SliceRoot =
      fact.formation.root._tag === 'BindingRoot'
        ? Object.freeze({
            _tag: 'BindingSliceRoot',
            binding: fact.formation.root.binding.id,
          })
        : fact.formation.root._tag === 'ParameterRoot'
          ? Object.freeze({
              _tag: 'ParameterSliceRoot',
              parameter: fact.formation.root.parameter.id,
            })
          : fact.formation.root._tag === 'PatternRoot'
            ? Object.freeze({
                _tag: 'PatternSliceRoot',
                binding: fact.formation.root.binding.id,
              })
            : Object.freeze({
                _tag: 'TemporarySliceRoot',
                owner: fact.formation.root.owner,
                value: hirExpression(fact.formation.root.value),
              })
    const selectors: Array<Hir.BorrowSelector> = []
    for (const selector of fact.formation.root.path) {
      if (selector._tag === 'Field') {
        selectors.push(
          Object.freeze({
            _tag: 'Field',
            field: selector.field,
            span: selector.span,
          }),
        )
        continue
      }
      if (selector._tag === 'SliceIndex') {
        const index = hirExpression(selector.index)
        if (index._tag === 'Unavailable') {
          return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
        }
        selectors.push(
          Object.freeze({
            _tag: 'SliceIndex',
            index,
            slice: selector.slice,
            span: selector.span,
          }),
        )
        continue
      }
      const index = hirExpression(selector.index)
      if (index._tag === 'Unavailable') {
        return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
      }
      selectors.push(
        Object.freeze({
          _tag: 'Index',
          index,
          array: selector.array,
          bounds: selector.bounds,
          span: selector.span,
        }),
      )
    }
    if (fact.formation._tag === 'ValueBorrow' && Type.isReference(fact.type.type)) {
      return Object.freeze({
        _tag: 'ValueBorrow',
        borrow,
        root,
        selectors: Object.freeze(selectors),
        source: fact.formation.source,
        access: fact.access,
        type: fact.type.type,
        span: fact.syntax.span,
      })
    }
    if (fact.formation._tag === 'ValueBorrow')
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    if (!Type.isSlice(fact.type.type))
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'SliceBorrow',
      borrow,
      root,
      selectors: Object.freeze(selectors),
      source:
        fact.formation._tag === 'FixedArrayBorrow' ? fact.formation.array : fact.formation.parent,
      access: fact.access,
      reborrow: fact.formation._tag === 'SliceReborrow',
      suspendsParent: fact.formation._tag === 'SliceReborrow' && fact.formation.suspendsParent,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'Grouped') return hirExpression(fact.expression)
  if (fact._tag === 'FunctionItem') {
    const target = hirCallableTarget(fact.reference)
    if (target === undefined || fact.type._tag !== 'Available' || !Type.isCallable(fact.type.type))
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'FunctionItem',
      target,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'CallableSection') {
    const target = hirCallableTarget(fact.reference)
    if (target === undefined || fact.type._tag !== 'Available' || !Type.isCallable(fact.type.type))
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'CallableSection',
      site: fact.site,
      target,
      remainingParameters: fact.remainingParameters,
      captures: Object.freeze(
        fact.captures.map((capture) =>
          Object.freeze({
            ordinal: capture.ordinal,
            parameterOrdinal: capture.parameterOrdinal,
            value: hirExpression(
              capture.expression,
              Object.freeze({
                _tag: 'BorrowId',
                function: fact.site.function,
                callSpan: fact.syntax.span,
                ordinal: capture.ordinal,
              }),
            ),
            access: capture.access,
          }),
        ),
      ),
      typeArguments: fact.typeArguments,
      substitution: fact.substitution,
      retainedDependencies: fact.retainedDependencies,
      mode: fact.mode,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'CallableApply') {
    if (fact.type._tag !== 'Available')
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'CallableApply',
      callee: hirExpression(fact.callee),
      arguments: Object.freeze(
        fact.arguments.map((argument) => hirExpression(argument.expression)),
      ),
      access: fact.mode,
      substitution: fact.substitution,
      evaluation:
        fact.provenance._tag === 'PipelineCallableApplication'
          ? 'LeftThenCallable'
          : 'CalleeThenArguments',
      realization: fact.callee._tag === 'CallableSection' ? 'DirectErasedSection' : 'Environment',
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (
    fact.reference._tag === 'ResolvedBoundOperation' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const borrowIds = Object.freeze(
      fact.arguments.flatMap(
        (argument, ordinal): ReadonlyArray<Hir.BorrowId> =>
          argument.expression._tag === 'Borrow' &&
          argument.expression.formation._tag !== 'Unavailable'
            ? [
                Object.freeze({
                  _tag: 'BorrowId' as const,
                  function: argument.id.function,
                  callSpan: argument.id.callSpan,
                  ordinal,
                }),
              ]
            : [],
      ),
    )
    return Object.freeze({
      _tag: 'BoundOperationCall',
      capability: fact.reference.capability,
      provider: fact.reference.provider,
      operation: fact.reference.operation,
      contract: fact.reference.interfaceContract,
      ...(fact.witnessEffectSite === undefined
        ? {}
        : { witnessEffectSite: fact.witnessEffectSite }),
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) =>
          hirExpression(
            argument.expression,
            argument.expression._tag === 'Borrow' &&
              argument.expression.formation._tag !== 'Unavailable'
              ? Object.freeze({
                  _tag: 'BorrowId',
                  function: argument.id.function,
                  callSpan: argument.id.callSpan,
                  ordinal,
                })
              : undefined,
          ),
        ),
      ),
      loanEnds: borrowIds,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (
    fact.reference._tag === 'ResolvedBuiltin' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const returnedBorrowParameter = fact.reference.returnedBorrowParameter
    const directLoanEnds = fact.arguments.flatMap(
      (argument, ordinal): ReadonlyArray<Hir.BorrowId> =>
        argument.expression._tag === 'Borrow' &&
        argument.expression.formation._tag !== 'Unavailable' &&
        returnedBorrowParameter !== ordinal
          ? [
              Object.freeze({
                _tag: 'BorrowId' as const,
                function: argument.id.function,
                callSpan: argument.id.callSpan,
                ordinal,
              }),
            ]
          : [],
    )
    const nestedSlotLoanEnds =
      fact.reference.operation === 'SlotWrite' ||
      fact.reference.operation === 'SlotTake' ||
      fact.reference.operation === 'SlotCopy' ||
      fact.reference.operation === 'SlotDrop'
        ? fact.arguments.flatMap((argument): ReadonlyArray<Hir.BorrowId> => {
            const nested = argument.expression
            if (
              nested._tag !== 'Call' ||
              nested.reference._tag !== 'ResolvedBuiltin' ||
              nested.reference.operation !== 'RawBufferSlot'
            )
              return []
            return nested.arguments.flatMap(
              (slotArgument, ordinal): ReadonlyArray<Hir.BorrowId> =>
                slotArgument.expression._tag === 'Borrow' &&
                slotArgument.expression.formation._tag !== 'Unavailable'
                  ? [
                      Object.freeze({
                        _tag: 'BorrowId' as const,
                        function: slotArgument.id.function,
                        callSpan: slotArgument.id.callSpan,
                        ordinal,
                      }),
                    ]
                  : [],
            )
          })
        : []
    const arguments_ = Object.freeze(
      fact.arguments.map((argument, ordinal) => {
        const borrowId: Hir.BorrowId | undefined =
          argument.expression._tag === 'Borrow' &&
          argument.expression.formation._tag !== 'Unavailable'
            ? Object.freeze({
                _tag: 'BorrowId',
                function: argument.id.function,
                callSpan: argument.id.callSpan,
                ordinal,
              })
            : undefined
        return hirExpression(argument.expression, borrowId)
      }),
    )
    const heldLoans = Object.freeze(
      fact.reference.operation === 'RawBufferSlot'
        ? directLoanEnds
        : fact.arguments.flatMap(
            (argument, ordinal): ReadonlyArray<Hir.BorrowId> =>
              argument.expression._tag === 'Borrow' &&
              argument.expression.formation._tag !== 'Unavailable' &&
              returnedBorrowParameter === ordinal
                ? [
                    Object.freeze({
                      _tag: 'BorrowId' as const,
                      function: argument.id.function,
                      callSpan: argument.id.callSpan,
                      ordinal,
                    }),
                  ]
                : [],
          ),
    )
    if (fact.reference.operation === 'StringFromUtf8Unchecked') {
      const source = arguments_.at(0)
      return source === undefined || source._tag === 'Unavailable'
        ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
        : Object.freeze({
            _tag: 'RuntimeStringView',
            source,
            heldLoans,
            type: Type.string,
            span: fact.syntax.span,
          })
    }
    if (fact.reference.operation === 'StringEqualsExact') {
      const left = arguments_.at(0)
      const right = arguments_.at(1)
      return left === undefined ||
        right === undefined ||
        left._tag === 'Unavailable' ||
        right._tag === 'Unavailable'
        ? Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
        : Object.freeze({
            _tag: 'StringEquality',
            left,
            right,
            negated: fact._tag === 'Operator' && fact.operator === 'NotEquals',
            intrinsic: fact.reference.intrinsic,
            type: Scalar.boolean.spelling,
            span: fact.syntax.span,
          })
    }
    return Object.freeze({
      _tag: 'BuiltinCall',
      operation: fact.reference.operation,
      intrinsic: fact.reference.intrinsic,
      ...(fact._tag === 'Operator' && fact.interfaceOperation !== undefined
        ? { interfaceOperation: fact.interfaceOperation }
        : {}),
      ...(fact._tag === 'Operator' && fact.witnessEffectSite !== undefined
        ? { witnessEffectSite: fact.witnessEffectSite }
        : {}),
      typeArguments: Object.freeze(
        fact._tag === 'Call'
          ? fact.typeArguments.flatMap((argument) =>
              argument.type === undefined ? [] : [argument.type],
            )
          : [],
      ),
      arguments: arguments_,
      loanEnds: Object.freeze(
        fact.reference.operation === 'RawBufferSlot'
          ? []
          : [...directLoanEnds, ...nestedSlotLoanEnds],
      ),
      heldLoans,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (
    fact.reference._tag === 'ResolvedServiceOperation' &&
    fact.reference.service.canonical._tag === 'Canonical' &&
    fact.reference.operation.name._tag === 'Present' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available' &&
    Type.isEffect(fact.type.type)
  ) {
    const serviceArguments = fact.contract.typeArguments
      .slice(0, fact.reference.service.typeParameters.length)
      .filter(Type.isTypeArgument)
    const service = Type.nominal(
      fact.reference.service.canonical.id.module,
      fact.reference.service.canonical.id.name,
      serviceArguments,
    )
    const requirement = Type.requirementMembers(fact.type.type).find((candidate) =>
      Type.equals(candidate.capability, service),
    )
    if (requirement === undefined)
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    const substitution = fact.contract.substitution
    const target = fact.reference.operation
    return Object.freeze({
      _tag: 'ServiceEffectConstruct',
      service,
      operation: fact.reference.operation.name.spelling,
      role: requirement.role,
      access: requirement.access,
      typeArguments: fact.contract.typeArguments,
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) => {
          const parameter = target.parameters.at(ordinal)
          const borrowId: Hir.BorrowId | undefined =
            argument.expression._tag === 'Borrow' &&
            argument.expression.formation._tag !== 'Unavailable'
              ? Object.freeze({
                  _tag: 'BorrowId',
                  function: argument.id.function,
                  callSpan: argument.id.callSpan,
                  ordinal,
                })
              : undefined
          return parameter?.declaredType._tag === 'Resolved'
            ? hirExpectedExpression(
                argument.expression,
                Type.substitute(parameter.declaredType.type, substitution),
                'Argument',
                parameter.syntax.span,
                borrowId,
              )
            : hirExpression(argument.expression, borrowId)
        }),
      ),
      loanEnds: Object.freeze(
        fact.arguments.flatMap(
          (argument, ordinal): ReadonlyArray<Hir.BorrowId> =>
            argument.expression._tag === 'Borrow' &&
            argument.expression.formation._tag !== 'Unavailable'
              ? [
                  Object.freeze({
                    _tag: 'BorrowId',
                    function: argument.id.function,
                    callSpan: argument.id.callSpan,
                    ordinal,
                  }),
                ]
              : [],
        ),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (
    fact.reference._tag === 'Resolved' &&
    fact.reference.declaration.canonical._tag === 'Canonical' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const target = fact.reference.declaration
    const substitution = fact.contract.substitution
    const returnedBorrowOrdinal = returnedBorrowArgument(fact)?.id.ordinal
    const call = {
      target: fact.reference.declaration.canonical.id,
      typeArguments: fact.contract.typeArguments,
      arguments: Object.freeze(
        fact.arguments.map((argument, ordinal) => {
          const parameter = target.parameters.at(ordinal)
          const borrowId: Hir.BorrowId | undefined =
            argument.expression._tag === 'Borrow' &&
            argument.expression.formation._tag !== 'Unavailable'
              ? Object.freeze({
                  _tag: 'BorrowId',
                  function: argument.id.function,
                  callSpan: argument.id.callSpan,
                  ordinal,
                })
              : undefined
          return parameter?.declaredType._tag === 'Resolved'
            ? hirExpectedExpression(
                argument.expression,
                Type.substitute(parameter.declaredType.type, substitution),
                'Argument',
                parameter.syntax.span,
                borrowId,
              )
            : hirExpression(argument.expression, borrowId)
        }),
      ),
      loanEnds: Object.freeze(
        fact.arguments.flatMap(
          (argument, ordinal): ReadonlyArray<Hir.BorrowId> =>
            argument.expression._tag === 'Borrow' &&
            argument.expression.formation._tag !== 'Unavailable' &&
            returnedBorrowOrdinal !== ordinal
              ? [
                  Object.freeze({
                    _tag: 'BorrowId',
                    function: argument.id.function,
                    callSpan: argument.id.callSpan,
                    ordinal,
                  }),
                ]
              : [],
        ),
      ),
      heldLoans: Object.freeze(
        fact.arguments.flatMap(
          (argument, ordinal): ReadonlyArray<Hir.BorrowId> =>
            argument.expression._tag === 'Borrow' &&
            argument.expression.formation._tag !== 'Unavailable' &&
            returnedBorrowOrdinal === ordinal
              ? [
                  Object.freeze({
                    _tag: 'BorrowId',
                    function: argument.id.function,
                    callSpan: argument.id.callSpan,
                    ordinal,
                  }),
                ]
              : [],
        ),
      ),
      type: fact.type.type,
      span: fact.syntax.span,
    }
    return Type.isEffect(fact.type.type) &&
      fact.reference._tag === 'Resolved' &&
      fact.reference.declaration.functionKind === 'Effect'
      ? Object.freeze({ ...call, _tag: 'EffectConstruct' as const, type: fact.type.type })
      : Object.freeze({ ...call, _tag: 'Call' as const })
  }
  const cause =
    fact.reference._tag === 'Missing' || fact.reference._tag === 'Ambiguous'
      ? fact.reference.cause
      : fact.contract._tag === 'Unavailable'
        ? fact.contract.cause
        : undefined
  return Object.freeze({
    _tag: 'Unavailable',
    span: fact.syntax.span,
    ...(cause === undefined ? {} : { cause }),
  })
}

const hirExpectedExpression = (
  fact: ExpressionFact,
  target: SemanticType,
  context: Extract<Hir.Expression, { readonly _tag: 'UnionConvert' }>['context'],
  expectedAt: SourceSpan.SourceSpan,
  borrow?: Hir.BorrowId,
): Hir.Expression => {
  if (
    fact._tag === 'Integer' &&
    fact.integer._tag === 'Available' &&
    contextualIntegerCompatible(fact, target) &&
    typeof target === 'string' &&
    Scalar.isIntegerSpelling(target)
  )
    return Object.freeze({
      _tag: 'IntegerLiteral',
      value: fact.integer.value,
      type: target,
      span: fact.syntax.span,
    })
  const loweredSource = hirExpression(fact, borrow)
  if (loweredSource._tag === 'Unavailable') return loweredSource
  const unionTarget = Type.isUnion(target) ? target : undefined
  const representation = unionTarget === undefined ? undefined : representationOfExpression(fact)
  const sourceContract = Type.isRepresented(loweredSource.type)
    ? loweredSource.type.contract
    : loweredSource.type
  const representedSource =
    representation !== undefined &&
    (Type.isCallable(sourceContract) || Type.isEffect(sourceContract)) &&
    unionTarget?.members.some(
      (member) =>
        Type.equals(member, sourceContract) ||
        (Type.isRepresented(member) && Type.equals(member.contract, sourceContract)),
    )
      ? Type.represented(sourceContract, sourceContract, representation)
      : undefined
  const source = loweredSource
  if (Type.isRepresented(target) && Type.haveSameRepresentationShape(source.type, target))
    return source
  const compatibility = TypeCompatibility.check(representedSource ?? source.type, target)
  if (compatibility._tag === 'Exact') return source
  if (compatibility._tag === 'CallableMode' || compatibility._tag === 'EffectAccess') return source
  if (compatibility._tag === 'Bottom') return source
  if (compatibility._tag === 'Incompatible') {
    return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
  }
  return Object.freeze({
    _tag: 'UnionConvert',
    source,
    sourceType: compatibility.source,
    target: compatibility.target,
    conversion: compatibility._tag,
    mappings: compatibility.mappings,
    access: 'Owned',
    context,
    expectedAt,
    type: compatibility.target,
    span: fact.syntax.span,
  })
}

const hirWritePlace = (
  fact: ExpressionFact,
  root: BindingDeclarationFact,
): Hir.WritePlace | undefined => {
  const selectors: Array<Hir.WriteSelector> = []
  const walk = (current: ExpressionFact): boolean => {
    if (current._tag === 'Grouped') return walk(current.expression)
    if (current._tag === 'Identifier') {
      return (
        current.reference._tag === 'ResolvedBinding' &&
        current.reference.binding.id.ordinal === root.id.ordinal
      )
    }
    if (current._tag === 'FieldProjection') {
      if (
        !walk(current.subject) ||
        current.state._tag !== 'Resolved' ||
        current.type._tag !== 'Available'
      ) {
        return false
      }
      selectors.push(
        Object.freeze({
          _tag: 'Field',
          field: current.state.field.id,
          type: current.type.type,
          span: current.syntax.span,
        }),
      )
      return true
    }
    if (current._tag === 'IndexProjection') {
      if (
        !walk(current.subject) ||
        current.array === undefined ||
        current.type._tag !== 'Available' ||
        (current.bounds._tag !== 'Proven' && current.bounds._tag !== 'Runtime')
      ) {
        return false
      }
      const index = hirExpression(current.index)
      if (index._tag === 'Unavailable') return false
      selectors.push(
        Object.freeze({
          _tag: 'Index',
          index,
          array: current.array,
          bounds: current.bounds,
          type: current.type.type,
          span: current.syntax.span,
        }),
      )
      return true
    }
    return false
  }
  if (!walk(fact) || fact.type._tag !== 'Available') return undefined
  return Object.freeze({
    _tag: 'WritePlace',
    root: root.id,
    selectors: Object.freeze(selectors),
    type: fact.type.type,
    span: fact.syntax.span,
  })
}

const assignmentRootType = (root: AssignmentRootFact): SemanticType | undefined => {
  if (root._tag === 'ParameterDeclaration') {
    return root.declaredType._tag === 'Resolved' ? root.declaredType.type : undefined
  }
  return root.inferredType._tag === 'Available' ? root.inferredType.type : undefined
}

const hirBorrowedWritePlace = (
  fact: ExpressionFact,
  root: AssignmentRootFact,
): Hir.BorrowedWritePlace | undefined => {
  const rootType = assignmentRootType(root)
  if (
    rootType === undefined ||
    !(Type.isSlice(rootType) || Type.isReference(rootType)) ||
    rootType.access !== 'Exclusive'
  ) {
    return undefined
  }
  const selectors: Array<Hir.BorrowedWriteSelector> = []
  const walk = (current: ExpressionFact): boolean => {
    if (current._tag === 'Grouped') return walk(current.expression)
    if (current._tag === 'Identifier') {
      return root._tag === 'ParameterDeclaration'
        ? current.reference._tag === 'Resolved' &&
            current.reference.parameter.id.ordinal === root.id.ordinal
        : current.reference._tag === 'ResolvedBinding' &&
            current.reference.binding.id.ordinal === root.id.ordinal
    }
    if (current._tag === 'FieldProjection') {
      if (
        !walk(current.subject) ||
        current.state._tag !== 'Resolved' ||
        current.type._tag !== 'Available' ||
        current.borrowAccess !== 'Exclusive'
      ) {
        return false
      }
      selectors.push(
        Object.freeze({
          _tag: 'Field',
          field: current.state.field.id,
          type: current.type.type,
          span: current.syntax.span,
        }),
      )
      return true
    }
    if (current._tag === 'IndexProjection') {
      if (
        !walk(current.subject) ||
        current.slice === undefined ||
        current.slice.access !== 'Exclusive' ||
        current.type._tag !== 'Available' ||
        current.bounds._tag !== 'RuntimeSlice'
      ) {
        return false
      }
      const index = hirExpression(current.index)
      if (index._tag === 'Unavailable') return false
      selectors.push(
        Object.freeze({
          _tag: 'SliceIndex',
          index,
          slice: current.slice,
          type: current.type.type,
          span: current.syntax.span,
        }),
      )
      return true
    }
    return false
  }
  if (!walk(fact) || fact.type._tag !== 'Available') return undefined
  return Object.freeze({
    _tag: 'BorrowedWritePlace',
    root:
      root._tag === 'ParameterDeclaration'
        ? Object.freeze({ _tag: 'ParameterSliceRoot' as const, parameter: root.id })
        : Object.freeze({ _tag: 'BindingSliceRoot' as const, binding: root.id }),
    slice: rootType,
    selectors: Object.freeze(selectors),
    type: fact.type.type,
    span: fact.syntax.span,
  })
}

const hirAssignmentWritePlace = (
  fact: ExpressionFact,
  root: BindingDeclarationFact,
): Hir.WritePlace | undefined => {
  const rootType = assignmentRootType(root)
  return rootType !== undefined &&
    (Type.isSlice(rootType) || Type.isReference(rootType)) &&
    rootType.access === 'Exclusive'
    ? hirBorrowedWritePlace(fact, root)
    : hirWritePlace(fact, root)
}

const statementSpan = (statement: StatementFact): SourceSpan.SourceSpan =>
  statement._tag === 'BindStatement' ? statement.binding.syntax.span : statement.syntax.span

const directStatementExpressions = (statement: StatementFact): ReadonlyArray<ExpressionFact> => {
  switch (statement._tag) {
    case 'BindStatement':
      return Object.freeze([statement.binding.initializer])
    case 'PatternBindStatement':
      return Object.freeze([statement.selection.source])
    case 'ExpressionStatement':
      return Object.freeze([statement.expression])
    case 'ReturnStatement':
    case 'FailStatement':
    case 'DropStatement':
      return Object.freeze([statement.expression])
    case 'IfStatement':
    case 'WhileStatement':
      return Object.freeze([statement.condition])
    case 'IfLetStatement':
      return Object.freeze([statement.selection.source])
    case 'WriteStatement':
      return Object.freeze([statement.destination, statement.value])
    case 'UnsafeStatement':
    case 'BreakStatement':
    case 'ContinueStatement':
      return Object.freeze([])
  }
}

const directExpressionChildren = (expression: ExpressionFact): ReadonlyArray<ExpressionFact> => {
  switch (expression._tag) {
    case 'Move':
    case 'Borrow':
    case 'FieldProjection':
    case 'Run':
      return Object.freeze([expression.subject])
    case 'PlaceReplace':
      return Object.freeze([expression.destination, expression.value])
    case 'IndexProjection':
      return Object.freeze([expression.subject, expression.index])
    case 'ArrayLiteral':
      return Object.freeze(expression.elements.map((element) => element.expression))
    case 'StructLiteral':
      return Object.freeze(expression.initializers.map((initializer) => initializer.expression))
    case 'Grouped':
      return Object.freeze([expression.expression])
    case 'EffectResult':
      return Object.freeze([expression.protected])
    case 'EffectBindRequirement':
      return Object.freeze([expression.protected])
    case 'EffectCatch':
      return Object.freeze([expression.protected, expression.handler])
    case 'CallableSection':
      return Object.freeze(expression.captures.map((capture) => capture.expression))
    case 'CallableApply':
      return Object.freeze([
        expression.callee,
        ...expression.arguments.map((argument) => argument.expression),
      ])
    case 'Operator':
    case 'ShortCircuit':
    case 'Call':
      return Object.freeze(expression.arguments.map((argument) => argument.expression))
    case 'EffectBlock':
    case 'Match':
    case 'Integer':
    case 'Floating':
    case 'StaticText':
    case 'Character':
    case 'Unit':
    case 'Boolean':
    case 'Constant':
    case 'Identifier':
    case 'FunctionItem':
      return Object.freeze([])
  }
}

/** Callbacks for one deterministic traversal of elaborated statement and expression facts. */
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
  readonly headers: DeclarationIndex.ModuleHeaders
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
  const erasedIntrinsicSection = (expression: ExpressionFact): boolean =>
    callableSectionOf(expression)?.reference._tag === 'ResolvedIntrinsicContract'
  const hirStatements = (
    facts: ReadonlyArray<StatementFact>,
    resultType?: SemanticType,
    functionId?: DeclarationId,
  ): ReadonlyArray<Hir.Statement> =>
    Object.freeze(
      executableStatements(facts)
        .filter(
          (statement) =>
            !(
              (statement._tag === 'BindStatement' &&
                erasedIntrinsicSection(statement.binding.initializer)) ||
              (statement._tag === 'DropStatement' && erasedIntrinsicSection(statement.expression))
            ),
        )
        .map((statement): Hir.Statement => {
          if (statement._tag === 'UnsafeStatement') {
            return Object.freeze({
              _tag: 'Unsafe' as const,
              statements: hirStatements(statement.statements, resultType, functionId),
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          if (statement._tag === 'BindStatement') {
            return Object.freeze({
              _tag: 'Bind' as const,
              binding: statement.binding.id,
              name:
                statement.binding.name._tag === 'Present'
                  ? statement.binding.name.spelling
                  : undefined,
              mutability: statement.binding.mutability,
              initializer:
                statement.binding.initializer._tag === 'Borrow' && functionId !== undefined
                  ? hirExpression(
                      statement.binding.initializer,
                      Object.freeze({
                        _tag: 'BorrowId',
                        function: functionId,
                        callSpan: statement.binding.initializer.syntax.span,
                        ordinal: 0,
                      }),
                    )
                  : hirExpression(statement.binding.initializer),
              region: statement.region,
              span: statement.binding.syntax.span,
            })
          }
          if (statement._tag === 'PatternBindStatement') {
            return Object.freeze({
              _tag: 'PatternBind' as const,
              selection: hirPatternSelection(statement.selection),
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          if (statement._tag === 'ExpressionStatement') {
            return Object.freeze({
              _tag: 'Evaluate' as const,
              expression: hirExpression(statement.expression),
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          if (statement._tag === 'IfStatement') {
            return Object.freeze({
              _tag: 'If' as const,
              condition: hirExpression(statement.condition),
              taken: hirStatements(statement.taken, resultType, functionId),
              otherwise: hirStatements(statement.otherwise, resultType, functionId),
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          if (statement._tag === 'IfLetStatement') {
            return Object.freeze({
              _tag: 'IfLet' as const,
              selection: hirPatternSelection(statement.selection),
              taken: hirStatements(statement.taken, resultType, functionId),
              otherwise: hirStatements(statement.otherwise, resultType, functionId),
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          if (statement._tag === 'WriteStatement') {
            const place =
              statement.root?._tag === 'BindingFact'
                ? hirAssignmentWritePlace(statement.destination, statement.root)
                : statement.root?._tag === 'ParameterDeclaration'
                  ? hirBorrowedWritePlace(statement.destination, statement.root)
                  : undefined
            if (
              place === undefined ||
              (statement.root?._tag === 'BindingFact' && statement.root.mutability !== 'Mutable') ||
              !statement.compatible
            ) {
              return Object.freeze({
                _tag: 'UnavailableStatement' as const,
                region: statement.region,
                span: statement.syntax.span,
              })
            }
            return Object.freeze({
              _tag: 'Write' as const,
              place,
              value: hirExpectedExpression(statement.value, place.type, 'Assignment', place.span),
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          if (statement._tag === 'WhileStatement') {
            return Object.freeze({
              _tag: 'While' as const,
              loop: statement.loop,
              ...(statement.parent === undefined ? {} : { parent: statement.parent }),
              condition: hirExpression(statement.condition),
              body: hirStatements(statement.body, resultType, functionId),
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          if (statement._tag === 'BreakStatement' || statement._tag === 'ContinueStatement') {
            if (statement.target === undefined) {
              return Object.freeze({
                _tag: 'UnavailableStatement' as const,
                region: statement.region,
                span: statement.syntax.span,
              })
            }
            return Object.freeze({
              _tag:
                statement._tag === 'BreakStatement' ? ('Break' as const) : ('Continue' as const),
              target: statement.target,
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          if (statement._tag === 'ReturnStatement')
            return Object.freeze({
              _tag: 'Return' as const,
              expression:
                resultType === undefined
                  ? hirExpression(statement.expression)
                  : hirExpectedExpression(
                      statement.expression,
                      resultType,
                      'Return',
                      statement.syntax.span,
                    ),
              region: statement.region,
              span: statement.expression.syntax.span,
            })
          if (statement._tag === 'DropStatement')
            return Object.freeze({
              _tag: 'Drop' as const,
              expression:
                statement.expression.type._tag === 'Available'
                  ? Object.freeze({
                      _tag: 'Move' as const,
                      subject: hirExpression(statement.expression),
                      type: statement.expression.type.type,
                      span: statement.expression.syntax.span,
                    })
                  : hirExpression(statement.expression),
              region: statement.region,
              span: statement.syntax.span,
            })
          if (statement._tag === 'FailStatement') {
            if (statement.failure === undefined)
              return Object.freeze({
                _tag: 'UnavailableStatement' as const,
                region: statement.region,
                span: statement.syntax.span,
              })
            return Object.freeze({
              _tag: 'Fail' as const,
              expression:
                statement.transfer === 'Move'
                  ? Object.freeze({
                      _tag: 'Move' as const,
                      subject: hirExpression(statement.expression),
                      type:
                        statement.expression.type._tag === 'Available'
                          ? statement.expression.type.type
                          : statement.failure,
                      span: statement.expression.syntax.span,
                    })
                  : hirExpression(statement.expression),
              failure: statement.failure,
              transfer: statement.transfer,
              region: statement.region,
              span: statement.syntax.span,
            })
          }
          throw new RangeError('Unknown statement fact')
        }),
    )
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
                access: DeclarationIndex.copyType(
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
                statements: hirStatements(
                  fact.statements,
                  fact.declaration.returnType.type,
                  fact.declaration.id,
                ),
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
            statements: hirStatements(
              fact.statements,
              fact.declaration.returnType._tag === 'Resolved'
                ? fact.declaration.returnType.type
                : undefined,
              fact.declaration.id,
            ),
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
