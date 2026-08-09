import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as FloatingPoint from './FloatingPoint.js'
import * as Hir from './Hir.js'
import * as Intrinsic from './Intrinsic.js'
import * as Match from './Match.js'
import * as NameResolution from './NameResolution.js'
import * as Operator from './Operator.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as StaticText from './StaticText.js'
import type * as SyntaxFile from './SyntaxFile.js'
import * as SyntaxTree from './SyntaxTree.js'
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

export type BorrowRootFact =
  | {
      readonly _tag: 'BindingRoot'
      readonly binding: BindingDeclarationFact
    }
  | {
      readonly _tag: 'ParameterRoot'
      readonly parameter: ParameterFact
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
  readonly before: ReadonlyArray<Type.Nominal>
  readonly after: ReadonlyArray<Type.Nominal>
  readonly reachable: boolean
  readonly syntax: SyntaxTree.Node
}

export interface MatchExpressionFact {
  readonly _tag: 'Match'
  readonly id: Match.MatchId
  readonly access: Match.Access
  readonly scrutinee: ExpressionFact
  readonly members: ReadonlyArray<Type.Nominal>
  readonly arms: ReadonlyArray<MatchArmFact>
  readonly exhaustive: boolean
  readonly type: ExpressionTypeFact
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
  | { readonly _tag: 'Unavailable' }

export interface StructInitializerFact {
  readonly _tag: 'StructInitializer'
  readonly name: string | undefined
  readonly token?: Token.Token
  readonly expression: ExpressionFact
  readonly state: StructInitializerState
  readonly syntax: SyntaxTree.Node
}

export interface StructLiteralExpressionFact {
  readonly _tag: 'StructLiteral'
  readonly target: StructTargetFact
  readonly authorized: boolean
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

/** One prefix or infix operator and its canonical builtin resolution. */
export interface OperatorExpressionFact {
  readonly _tag: 'Operator'
  readonly operator: Operator.Prefix | Operator.Infix
  readonly reference: CallReferenceFact
  readonly arguments: ReadonlyArray<ArgumentFact>
  readonly mappings: ReadonlyArray<BuiltinArgumentMappingFact>
  readonly contract: CallContractFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One declaration or builtin named as a callable value without invocation. */
export interface FunctionItemExpressionFact {
  readonly _tag: 'FunctionItem'
  readonly reference: CallReferenceFact
  readonly path: ReferencePathFact
  readonly type: ExpressionTypeFact
  readonly syntax: SyntaxTree.Node
}

/** One trailing value retained by an automatic leading-argument section. */
export interface CallableCaptureFact {
  readonly _tag: 'CallableCapture'
  readonly ordinal: number
  readonly parameterOrdinal: number
  readonly expression: ExpressionFact
  readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
}

/** One hidden concrete section construction awaiting parameter zero. */
export interface CallableSectionExpressionFact {
  readonly _tag: 'CallableSection'
  readonly site: {
    readonly _tag: 'CallableSiteId'
    readonly function: DeclarationId
    readonly span: SourceSpan.SourceSpan
  }
  readonly reference: CallReferenceFact
  readonly path: ReferencePathFact
  readonly omittedParameter: 0
  readonly captures: ReadonlyArray<CallableCaptureFact>
  readonly retainedDependencies: ReadonlyArray<number>
  readonly typeArguments: ReadonlyArray<SemanticType>
  readonly substitution: ReadonlyMap<string, SemanticType>
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
  readonly substitution: ReadonlyMap<string, SemanticType>
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
export interface EffectProviderCaptureFact {
  readonly _tag: 'EffectProviderCapture'
  readonly reference: BindingDeclarationFact | ParameterFact
  readonly capability: Type.Nominal
  readonly providerType: Type.Nominal
  readonly witness: DeclarationIndex.ConformanceWitness
  readonly role: string
  readonly access: 'Shared' | 'Exclusive' | 'Take'
  readonly span: SourceSpan.SourceSpan
}

/** One lazy imperative effect block and its capture-derived execution contract. */
export interface EffectExpressionFact {
  readonly _tag: 'EffectBlock'
  readonly site: Hir.EffectSiteId
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
  | {
      readonly _tag: 'Unit'
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | BooleanExpressionFact
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
      readonly _tag: 'EffectCatch'
      readonly reference: IntrinsicReferenceFact
      readonly typeArguments: ReadonlyArray<TypeArgumentFact>
      readonly protected: ExpressionFact
      readonly handled?: Type.Nominal
      readonly handler: ExpressionFact
      readonly handlerEffect?: Type.Effect
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'EffectRetry'
      readonly reference: IntrinsicReferenceFact
      readonly protected: ExpressionFact
      readonly retries: ExpressionFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'EffectTransform'
      readonly reference: IntrinsicReferenceFact
      readonly operation: 'Map' | 'FlatMap' | 'Tap'
      readonly protected: ExpressionFact
      readonly callback: ExpressionFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'EffectProvide'
      readonly reference: IntrinsicReferenceFact
      readonly protected: ExpressionFact
      readonly provider?: EffectProviderCaptureFact
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
    }
  | {
      readonly _tag: 'EffectProvideWith'
      readonly reference: IntrinsicReferenceFact
      readonly protected: ExpressionFact
      readonly acquisition: ExpressionFact
      readonly capability?: Type.Nominal
      readonly role?: string
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
      readonly type: ExpressionTypeFact
      readonly syntax: SyntaxTree.Node
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
      readonly typeArguments: ReadonlyArray<SemanticType>
      readonly substitution: ReadonlyMap<string, SemanticType>
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
      readonly _tag: 'IfStatement'
      readonly condition: ExpressionFact
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
      readonly failure?: Type.Nominal
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
  readonly index: DeclarationIndex.Index
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

const unsignedMagnitude = (bytes: Uint8Array): bigint => {
  let value = 0n
  for (const byte of bytes) value = value * 10n + BigInt(byte - 0x30)
  return value
}

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
  const magnitude = unsignedMagnitude(bytes)
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
): FloatingExpressionFact => {
  const token = directToken(node, 'DecimalFloat')
  if (token === undefined) return Object.freeze({ _tag: 'Unavailable', syntax: node })
  const bytes = Option.getOrThrowWith(
    SourceFile.slice(source, token.span),
    () => new RangeError(`Semantic float span does not belong to source ${source.id}`),
  )
  const unsigned = Array.from(bytes, (byte) => String.fromCharCode(byte)).join('')
  const spelling = directToken(node, 'Minus') === undefined ? unsigned : `-${unsigned}`
  const selected = Scalar.isFloatSpelling(expected) ? expected : Scalar.defaultFloat.spelling
  const encoded = FloatingPoint.fromDecimal(spelling, selected === 'f32' ? 32 : 64)
  return encoded === undefined
    ? Object.freeze({ _tag: 'Unavailable', syntax: node })
    : Object.freeze({
        _tag: 'Available',
        type: selected,
        bits: encoded.bits,
        spelling,
        token,
        syntax: node,
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
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'Move',
      subject: subject.fact,
      type: subject.fact.type,
      syntax: node,
    }),
    diagnostics: subject.diagnostics,
    type: subject.type,
  })
}

const borrowRoot = (subject: ExpressionFact): BorrowRootFact | undefined => {
  if (subject._tag !== 'Identifier') return undefined
  if (subject.reference._tag === 'ResolvedBinding') {
    return Object.freeze({ _tag: 'BindingRoot', binding: subject.reference.binding })
  }
  if (subject.reference._tag === 'Resolved') {
    return Object.freeze({ _tag: 'ParameterRoot', parameter: subject.reference.parameter })
  }
  return undefined
}

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
    expected === undefined ||
    (!Type.isSlice(expected) && !Type.isReference(expected))
  ) {
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidBorrowPosition(node.span),
    )
  }
  const root = borrowRoot(subject)
  const sourceType = subjectResult?.type
  if (root === undefined || sourceType === undefined) {
    return unavailableBorrow(
      node,
      access,
      subject,
      diagnostics,
      Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
    )
  }
  if (Type.isReference(expected)) {
    if (!Type.infer(expected.target, sourceType, new Map())) {
      return unavailableBorrow(
        node,
        access,
        subject,
        diagnostics,
        Diagnostic.invalidBorrowOperand(subjectNode?.span ?? node.span),
      )
    }
    if (
      access === 'Exclusive' &&
      (root._tag !== 'BindingRoot' || root.binding.mutability !== 'Mutable')
    ) {
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
    if (
      access === 'Exclusive' &&
      (root._tag !== 'BindingRoot' || root.binding.mutability !== 'Mutable')
    ) {
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
  if (Type.isSlice(sourceType) && root._tag === 'ParameterRoot') {
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
    const bindingTarget = resolveStructTarget(source, bindingTargetSyntax, resolution, declaration)
    const bindingDiagnostics: Array<Diagnostic.Diagnostic> = [...bindingTarget.diagnostics]
    const member = bindingTarget.fact._tag === 'Resolved' ? bindingTarget.fact.type : undefined
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
        _tag: 'NominalPattern',
        id,
        target: bindingTarget.fact,
        ...(member === undefined ? {} : { member }),
        fields: Object.freeze([]),
        bindings: Object.freeze([wholeBinding]),
        omitted: Object.freeze([]),
        rest: false,
        complete:
          bindingTarget.fact._tag === 'Resolved' && !counters.invalid && isAvailableSyntax(node),
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

    const nestedNode = SyntaxTree.directNode(fieldNode, 'NominalPattern')
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
        nested._tag === 'NominalPattern' &&
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
          (field.nested._tag === 'NominalPattern' && field.nested.complete)),
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
  if (scrutinee?.type !== undefined && members === undefined) {
    diagnostics.push(Diagnostic.matchScrutineeNotNominal(Type.encode(scrutinee.type), node.span))
  }

  const preliminary = SyntaxTree.directNodes(node, 'MatchArm').map((armNode, ordinal) => {
    const armId: Match.ArmId = Object.freeze({ _tag: 'MatchArmId', match: id, ordinal })
    const patternNode =
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
        ...(pattern._tag === 'NominalPattern' && pattern.member !== undefined
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
      pattern._tag === 'NominalPattern' &&
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
    } else if (!transition.reachable && (members?.length ?? 0) > 0) {
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
  if (members !== undefined && !coverage.exhaustive) {
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
    diagnostics.push(Diagnostic.incompatibleMatchResults(joined.types.map(Type.encode), node.span))
  }
  const hasInvalidGuard = arms.some(
    (arm) =>
      arm.guard !== undefined &&
      arm.guard.type._tag === 'Available' &&
      arm.guard.type.type !== 'bool',
  )
  const effectSites = arms.flatMap((arm) =>
    arm.reachable && arm.result._tag === 'EffectBlock'
      ? [
          `${arm.result.site.function.sourceId}:${arm.result.site.function.ordinal}:${arm.result.site.span.start}`,
        ]
      : [],
  )
  const erasesEffectIdentity = new Set(effectSites).size > 1
  if (erasesEffectIdentity) diagnostics.push(Diagnostic.effectIdentityErasure(node.span))
  const callableSites = arms.flatMap((arm) =>
    arm.reachable && arm.result._tag === 'CallableSection'
      ? [
          `${arm.result.site.function.sourceId}:${arm.result.site.function.ordinal}:${arm.result.site.span.start}`,
        ]
      : [],
  )
  const erasesCallableIdentity = new Set(callableSites).size > 1
  if (erasesCallableIdentity) diagnostics.push(Diagnostic.callableIdentityErasure(node.span))
  const type =
    members !== undefined &&
    coverage.exhaustive &&
    arms.every(
      (arm) => arm.reachable && (arm.pattern._tag !== 'NominalPattern' || arm.pattern.complete),
    ) &&
    !unavailableReachableResult &&
    !hasInvalidGuard &&
    !erasesEffectIdentity &&
    !erasesCallableIdentity &&
    joined._tag === 'Joined'
      ? availableExpressionType(joined.type)
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

const analyzeStructLiteral = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const targetSyntax = SyntaxTree.directNode(node, 'AppliedType') ?? childNode(node, 'TypePath')
  const target = resolveStructTarget(source, targetSyntax, resolution, declaration)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct = target.fact._tag === 'Resolved' ? target.fact.struct : undefined
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const nominalLabel = nominal === undefined ? 'unknown struct' : Type.encode(nominal)
  const structSubstitution =
    struct === undefined || nominal === undefined
      ? new Map<string, SemanticType>()
      : (Type.substitution(
          struct.typeParameters.map((parameter) => parameter.type),
          nominal.arguments,
        ) ?? new Map())
  const authorized =
    nominal !== undefined && (nominal.module === source.id || Type.isOutOfMemory(nominal))
  if (nominal !== undefined && !authorized) {
    diagnostics.push(Diagnostic.externalRawStructLiteral(Type.encode(nominal), node.span))
  }

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
        expected,
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
          fieldLookup.field.declaredType._tag === 'Resolved' &&
          expression.type !== undefined &&
          !typesCompatible(
            expression.type,
            Type.substitute(fieldLookup.field.declaredType.type, structSubstitution),
          )
        ) {
          const expectedType = Type.substitute(
            fieldLookup.field.declaredType.type,
            structSubstitution,
          )
          const diagnostic =
            unionConversionDiagnostic(expression.type, expectedType, expressionNode.span) ??
            Diagnostic.structFieldTypeMismatch(
              name,
              Type.encode(expectedType),
              Type.encode(expression.type),
              expressionNode.span,
            )
          diagnostics.push(diagnostic)
          state = Object.freeze({
            _tag: 'TypeMismatch',
            field: fieldLookup.field,
            cause: Diagnostic.identity(diagnostic),
          })
        } else if (
          fieldLookup.field.declaredType._tag === 'Resolved' &&
          expression.type !== undefined
        ) {
          state = Object.freeze({ _tag: 'Resolved', field: fieldLookup.field })
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

  if (struct !== undefined && nominal !== undefined) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
      diagnostics.push(
        Diagnostic.missingStructInitializer(Type.encode(nominal), field.name.spelling, node.span),
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
    nominal !== undefined &&
    authorized &&
    SyntaxTree.isAvailableSyntax(node) &&
    fields.length === struct.fields.length &&
    initializers.length === struct.fields.length &&
    initializers.every((initializer) => initializer.state._tag === 'Resolved')
  const type =
    complete && nominal !== undefined ? availableExpressionType(nominal) : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'StructLiteral',
      target: target.fact,
      authorized,
      initializers: Object.freeze(initializers),
      fields: Object.freeze(fields),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: complete ? nominal : undefined,
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
    if (elementType === undefined && element.type !== undefined) elementType = element.type
    let compatibility: ArrayElementFact['compatibility']
    if (element.type === undefined || elementType === undefined) {
      compatibility = Object.freeze({ _tag: 'Unavailable' })
    } else if (!typesCompatible(element.type, elementType)) {
      const diagnostic =
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
  let target: DeclarationFact | undefined
  let builtinParameters: ReadonlyArray<SemanticType> = Object.freeze([])
  let builtinTypeParameters: ReadonlyArray<Type.Parameter> = Object.freeze([])
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
      const builtin = builtinSignature(qualifierSpelling, memberSpelling)
      builtinParameters = builtin?.parameters ?? Object.freeze([])
      builtinTypeParameters = builtin?.typeParameters ?? Object.freeze([])
    } else if (qualifier._tag === 'Namespace') {
      const member = DeclarationIndex.lookup(resolution.index, qualifier.module, memberSpelling)
      target = member._tag === 'Resolved' ? member.declaration : undefined
    }
  }
  const declaredTypeParameters =
    target?.typeParameters.map((parameter) => parameter.type) ?? Object.freeze([])
  const explicitTypes = callTypeArguments?.types
  const builtinSubstitution =
    callTypeArguments?.explicit === true &&
    explicitTypes !== undefined &&
    explicitTypes.length === builtinTypeParameters.length
      ? Type.substitution(builtinTypeParameters, explicitTypes)
      : undefined
  const substitution =
    callTypeArguments?.explicit === true &&
    explicitTypes !== undefined &&
    explicitTypes.length === declaredTypeParameters.length
      ? Type.substitution(declaredTypeParameters, explicitTypes)
      : undefined
  const expectedTypes = Object.freeze(
    builtinParameters.length > 0
      ? builtinParameters
          .slice(
            builtinParameters.length >= 2 && argumentNodes.length === builtinParameters.length - 1
              ? 1
              : 0,
          )
          .map((parameter) => Type.substitute(parameter, builtinSubstitution ?? new Map()))
      : (target?.parameters ?? [])
          .slice(
            target !== undefined &&
              target.parameters.length >= 2 &&
              argumentNodes.length === target.parameters.length - 1
              ? 1
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
      (element.kind === 'TypePath' ||
        element.kind === 'AppliedType' ||
        element.kind === 'FixedArrayType' ||
        element.kind === 'SliceType' ||
        element.kind === 'CallableType' ||
        element.kind === 'ParenthesizedType' ||
        element.kind === 'UnionType'),
  )
  const analyzed = nodes.map((node, ordinal) => {
    const raw = DeclarationIndex.analyzeDeclaredType(source, node, environment)
    const resolved = DeclarationIndex.resolveTypeFact(
      resolution.index,
      source.id,
      raw.fact,
      (module, path) => NameResolution.resolveType(nameResolution, resolution.index, module, path),
    )
    const invalidBorrow =
      resolved.fact._tag === 'Resolved' && Type.containsBorrow(resolved.fact.type)
        ? Diagnostic.sliceTypePosition('type argument', node.span)
        : undefined
    return Object.freeze({
      fact: Object.freeze({
        _tag: 'TypeArgument' as const,
        ordinal,
        syntax: node,
        declared: resolved.fact,
        ...(resolved.fact._tag === 'Resolved' && invalidBorrow === undefined
          ? { type: resolved.fact.type }
          : {}),
      }),
      diagnostics: Diagnostic.merge(
        raw.diagnostics,
        resolved.diagnostics,
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

const callArityDiagnostic = (
  reference: Extract<CallReferenceFact, { readonly _tag: 'Resolved' | 'ResolvedBuiltin' }>,
  expectedCount: number,
  actualCount: number,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic => {
  if (expectedCount === 1 && actualCount === 0)
    return Diagnostic.redundantUnaryEmptyCall(reference.spelling, span)
  if (expectedCount >= 2 && actualCount < expectedCount - 1)
    return Diagnostic.deeperUnderApplication(reference.spelling, expectedCount, actualCount, span)
  return Diagnostic.wrongCallArity(
    reference._tag === 'ResolvedBuiltin'
      ? Object.freeze({
          _tag: 'BuiltinTarget',
          actor: reference.actor,
          operation: reference.operation,
        })
      : reference.declaration.id,
    expectedCount,
    actualCount,
    span,
  )
}

const analyzeCallContract = (
  call: SyntaxTree.Node,
  reference: CallReferenceFact,
  argumentsList: ReadonlyArray<ArgumentFact>,
  syntaxAvailable = hasAvailableCallSyntax(call),
  callTypeArguments?: CallTypeArgumentsResult,
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
  if (reference._tag === 'ResolvedBuiltin') {
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
      }),
      diagnostics: Object.freeze([]),
    })
  }

  if (reference._tag !== 'Resolved') {
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

  const parameters = reference.declaration.parameters
  const mappings = Object.freeze(
    argumentsList.flatMap((argument, ordinal): ReadonlyArray<ArgumentMappingFact> => {
      const parameter = parameters.at(ordinal)
      return parameter === undefined
        ? []
        : [Object.freeze({ _tag: 'ArgumentMapping', argument, parameter })]
    }),
  )
  const unavailableMapping = mappings.find(
    (mapping) =>
      mapping.argument.type._tag !== 'Available' ||
      mapping.parameter.declaredType._tag !== 'Resolved',
  )
  if (unavailableMapping !== undefined) {
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'UnavailableMappedType',
          mapping: unavailableMapping,
        }),
      }),
      diagnostics: Object.freeze([]),
    })
  }
  const implicitDecay = mappings.find(
    (mapping) =>
      mapping.argument.type._tag === 'Available' &&
      Type.isFixedArray(mapping.argument.type.type) &&
      mapping.parameter.declaredType._tag === 'Resolved' &&
      Type.isSlice(mapping.parameter.declaredType.type),
  )
  if (
    implicitDecay !== undefined &&
    implicitDecay.parameter.declaredType._tag === 'Resolved' &&
    Type.isSlice(implicitDecay.parameter.declaredType.type)
  ) {
    const expected = implicitDecay.parameter.declaredType.type
    const diagnostic = Diagnostic.implicitSliceDecay(
      Type.encode(expected),
      implicitDecay.argument.syntax.span,
    )
    return Object.freeze({
      mappings,
      fact: Object.freeze({
        _tag: 'Unavailable',
        reason: Object.freeze({
          _tag: 'ArgumentTypeMismatch',
          argument: implicitDecay.argument,
          expected,
        }),
        cause: Diagnostic.identity(diagnostic),
      }),
      diagnostics: Object.freeze([diagnostic]),
    })
  }
  const declaredTypeParameters = reference.declaration.typeParameters.map(
    (parameter) => parameter.type,
  )
  let substitution: ReadonlyMap<string, SemanticType>
  let typeArguments: ReadonlyArray<SemanticType>
  if (callTypeArguments?.explicit === true) {
    if (callTypeArguments.facts.length !== declaredTypeParameters.length) {
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
    typeArguments = Object.freeze(Array.from(callTypeArguments.types))
    const explicitSubstitution = Type.substitution(declaredTypeParameters, typeArguments)
    if (explicitSubstitution === undefined) {
      throw new RangeError('validated call type arguments lost their declaration arity')
    }
    substitution = explicitSubstitution
  } else if (declaredTypeParameters.length === 0) {
    typeArguments = Object.freeze([])
    substitution = new Map()
  } else {
    const inferred = new Map<string, SemanticType>()
    const compatible = mappings.every(
      (mapping) =>
        mapping.argument.type._tag === 'Available' &&
        mapping.parameter.declaredType._tag === 'Resolved' &&
        Type.infer(mapping.parameter.declaredType.type, mapping.argument.type.type, inferred),
    )
    typeArguments = Object.freeze(
      declaredTypeParameters.flatMap((parameter) => {
        const inferredType = inferred.get(Type.key(parameter))
        return inferredType === undefined ? [] : [inferredType]
      }),
    )
    if (!compatible || typeArguments.length !== declaredTypeParameters.length) {
      const diagnostic = Diagnostic.typeArgumentInference(reference.spelling, call.span)
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
  for (const mapping of mappings) {
    if (
      mapping.argument.type._tag !== 'Available' ||
      mapping.parameter.declaredType._tag !== 'Resolved'
    ) {
      continue
    }
    const expected = Type.substitute(mapping.parameter.declaredType.type, substitution)
    if (!typesCompatible(mapping.argument.type.type, expected)) {
      const mismatch =
        Type.isCallable(expected) && Type.isCallable(mapping.argument.type.type)
          ? Diagnostic.incompatibleCallableSignature(
              Type.encode(expected),
              Type.encode(mapping.argument.type.type),
              mapping.argument.syntax.span,
            )
          : Type.isSlice(expected) && Type.isFixedArray(mapping.argument.type.type)
            ? Diagnostic.implicitSliceDecay(Type.encode(expected), mapping.argument.syntax.span)
            : (unionConversionDiagnostic(
                mapping.argument.type.type,
                expected,
                mapping.argument.syntax.span,
              ) ??
              Diagnostic.argumentTypeMismatch(
                Type.encode(expected),
                Type.encode(mapping.argument.type.type),
                mapping.argument.syntax.span,
              ))
      return Object.freeze({
        mappings,
        fact: Object.freeze({
          _tag: 'Unavailable',
          reason: Object.freeze({
            _tag: 'ArgumentTypeMismatch',
            argument: mapping.argument,
            expected,
          }),
          cause: Diagnostic.identity(mismatch),
        }),
        diagnostics: Object.freeze([mismatch]),
      })
    }
  }

  const expectedCount = parameters.length
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

  return Object.freeze({
    mappings,
    fact: Object.freeze({
      _tag: 'Compatible',
      expectedCount,
      actualCount,
      typeArguments,
      substitution,
    }),
    diagnostics: Object.freeze([]),
  })
}

interface BuiltinSignature {
  readonly operation: Hir.BuiltinOperation
  readonly typeParameters?: ReadonlyArray<Type.Parameter>
  readonly parameters: ReadonlyArray<SemanticType>
  readonly result: SemanticType
  readonly unsafe?: boolean
}

const builtinSignature = (actor: string, operation: string): BuiltinSignature | undefined => {
  const catalog = Intrinsic.findOperation(actor, operation)
  if (catalog?.rule._tag !== 'BuiltinRule') return undefined
  return Object.freeze({
    operation: catalog.rule.operation,
    typeParameters: catalog.rule.typeParameters,
    parameters: catalog.rule.parameters,
    result: catalog.rule.result,
    unsafe: catalog.unsafe,
  })
}

const callableResultType = (declaration: DeclarationFact): SemanticType | undefined => {
  if (declaration.returnType._tag !== 'Resolved') return undefined
  if (declaration.functionKind === 'Ordinary') return declaration.returnType.type
  return Type.effect(
    declaration.returnType.type,
    declaration.failureRow.failures,
    'Shared',
    declaration.requirementRow.requirements,
  )
}

const callableTypeOfReference = (reference: CallReferenceFact): Type.Callable | undefined => {
  if (reference._tag === 'ResolvedBuiltin')
    return Type.callable(reference.parameters, reference.result)
  if (reference._tag !== 'Resolved') return undefined
  const parameters = reference.declaration.parameters.flatMap((parameter) =>
    parameter.declaredType._tag === 'Resolved' ? [parameter.declaredType.type] : [],
  )
  const result = callableResultType(reference.declaration)
  return parameters.length === reference.declaration.parameters.length && result !== undefined
    ? Type.callable(parameters, result)
    : undefined
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
    const signature = builtinSignature(qualifier, member)
    return signature === undefined
      ? undefined
      : Object.freeze({
          _tag: 'ResolvedBuiltin',
          spelling: `${qualifier}.${member}`,
          token: second,
          actor: qualifier,
          operation: signature.operation,
          parameters: signature.parameters,
          result: signature.result,
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
  readonly substitution: ReadonlyMap<string, SemanticType>
  readonly typeArguments: ReadonlyArray<SemanticType>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly valid: boolean
}

const analyzeSectionContract = (
  call: SyntaxTree.Node,
  reference: Extract<CallReferenceFact, { readonly _tag: 'Resolved' | 'ResolvedBuiltin' }>,
  arguments_: ReadonlyArray<ArgumentFact>,
  callTypeArguments: CallTypeArgumentsResult,
): SectionContractResult => {
  if (reference._tag === 'ResolvedBuiltin') {
    const diagnostics = arguments_.flatMap((argument, ordinal) => {
      if (argument.type._tag !== 'Available') return []
      const expected = reference.parameters.at(ordinal + 1)
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

  const declaredParameters = reference.declaration.typeParameters.map((parameter) => parameter.type)
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  let substitution = new Map<string, SemanticType>()
  if (callTypeArguments.explicit) {
    if (
      callTypeArguments.types === undefined ||
      callTypeArguments.facts.length !== declaredParameters.length
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
      substitution = new Map(
        Type.substitution(declaredParameters, callTypeArguments.types) ?? new Map(),
      )
    }
  } else {
    for (const [ordinal, argument] of arguments_.entries()) {
      const parameter = reference.declaration.parameters.at(ordinal + 1)
      if (
        argument.type._tag === 'Available' &&
        parameter?.declaredType._tag === 'Resolved' &&
        !Type.infer(parameter.declaredType.type, argument.type.type, substitution)
      ) {
        diagnostics.push(Diagnostic.typeArgumentInference(reference.spelling, call.span))
        break
      }
    }
    const leading = reference.declaration.parameters.at(0)?.declaredType
    const deferred = new Set(
      leading?._tag === 'Resolved' ? Type.parameters(leading.type).map(Type.key) : [],
    )
    if (
      declaredParameters.some(
        (parameter) => !substitution.has(Type.key(parameter)) && !deferred.has(Type.key(parameter)),
      )
    ) {
      diagnostics.push(Diagnostic.typeArgumentInference(reference.spelling, call.span))
    }
  }
  for (const [ordinal, argument] of arguments_.entries()) {
    const parameter = reference.declaration.parameters.at(ordinal + 1)
    if (argument.type._tag !== 'Available' || parameter?.declaredType._tag !== 'Resolved') continue
    const expected = Type.substitute(parameter.declaredType.type, substitution)
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

const captureAccess = (expression: ExpressionFact): CallableCaptureFact['access'] => {
  if (expression._tag === 'Move') return 'Take'
  if (expression._tag === 'Borrow')
    return expression.access === 'Exclusive' ? 'Exclusive' : 'Shared'
  if (expression._tag === 'Grouped') return captureAccess(expression.expression)
  return 'Copy'
}

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
  reference: Extract<CallReferenceFact, { readonly _tag: 'Resolved' | 'ResolvedBuiltin' }>,
  substitution: ReadonlyMap<string, SemanticType>,
  mode: Type.CallableMode,
): Type.Callable | undefined => {
  if (reference._tag === 'ResolvedBuiltin') {
    const leading = reference.parameters.at(0)
    return leading === undefined ? undefined : Type.callable([leading], reference.result, mode)
  }
  const leading = reference.declaration.parameters.at(0)?.declaredType
  const result = callableResultType(reference.declaration)
  return leading?._tag === 'Resolved' && result !== undefined
    ? Type.callable(
        [Type.substitute(leading.type, substitution)],
        Type.substitute(result, substitution),
        mode,
      )
    : undefined
}

const finishCallableSection = (
  node: SyntaxTree.Node,
  reference: Extract<CallReferenceFact, { readonly _tag: 'Resolved' | 'ResolvedBuiltin' }>,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
): ExpressionResult => {
  const contract = analyzeSectionContract(node, reference, argumentsResult.facts, callTypeArguments)
  const captures = Object.freeze(
    argumentsResult.facts.map((argument, ordinal) =>
      Object.freeze({
        _tag: 'CallableCapture' as const,
        ordinal,
        parameterOrdinal: ordinal + 1,
        expression: argument.expression,
        access: captureAccess(argument.expression),
      }),
    ),
  )
  const mode = callableMode(captures)
  const callable = sectionCallableType(reference, contract.substitution, mode)
  const type =
    contract.valid && callable !== undefined
      ? availableExpressionType(callable)
      : unavailableExpressionType
  const owner = argumentsResult.facts.at(0)?.id.function
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableSection',
      site: Object.freeze({
        _tag: 'CallableSiteId',
        function:
          owner ??
          Object.freeze({ _tag: 'DeclarationId', sourceId: node.span.sourceId, ordinal: 0 }),
        span: node.span,
      }),
      reference,
      path: referencePath(node),
      omittedParameter: 0,
      captures,
      retainedDependencies: Object.freeze(
        captures.flatMap((capture) =>
          capture.access === 'Shared' || capture.access === 'Exclusive'
            ? [capture.parameterOrdinal]
            : [],
        ),
      ),
      typeArguments: contract.typeArguments,
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
): ExpressionResult => {
  const callable =
    callee.type !== undefined && Type.isCallable(callee.type) ? callee.type : undefined
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...callee.diagnostics,
    ...argumentsResult.diagnostics,
    ...callTypeArguments.diagnostics,
  ]
  const inferred = new Map<string, SemanticType>()
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
  if (callTypeArguments.explicit) {
    diagnostics.push(
      Diagnostic.typeArgumentArity('callable value', 0, callTypeArguments.facts.length, node.span),
    )
    valid = false
  }
  if (callable !== undefined && callable.parameters.length !== argumentsResult.facts.length) {
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
  if (callable !== undefined) {
    for (const [ordinal, argument] of argumentsResult.facts.entries()) {
      const expected = callable.parameters.at(ordinal)
      if (expected === undefined || argument.type._tag !== 'Available') {
        valid = false
        continue
      }
      if (!Type.infer(expected, argument.type.type, inferred)) {
        diagnostics.push(
          Type.isCallable(expected) && Type.isCallable(argument.type.type)
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
  const type =
    valid && callable !== undefined
      ? availableExpressionType(Type.substitute(callable.result, inferred))
      : unavailableExpressionType
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
    diagnostics.push(
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

function analyzeBuiltinCall(
  source: SourceFile.SourceFile,
  call: SyntaxTree.Node,
  argumentsResult: ArgumentsResult,
  typeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
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
  const substitution = new Map<string, SemanticType>()
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
  const unsafeAuthorized =
    signature?.unsafe !== true ||
    (resolution.unsafeSpans ?? []).some(
      (span) =>
        span.sourceId === call.span.sourceId &&
        span.start <= call.span.start &&
        span.end >= call.span.end,
    )
  const unsafeDiagnostic =
    unsafeAuthorized || signature === undefined
      ? undefined
      : Diagnostic.missingUnsafeBoundary(`${actorSpelling}.${operationSpelling}`, call.span)
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
          parameters: instantiatedParameters,
          result: instantiatedResult ?? signature.result,
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
    reference.parameters.length >= 2 &&
    argumentsResult.facts.length === reference.parameters.length - 1
  ) {
    return finishCallableSection(call, reference, argumentsResult, typeArguments)
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
): ExpressionResult => {
  const child = node.children.find(isExpressionNode)
  const expression =
    child === undefined
      ? undefined
      : analyzeExpression(source, child, declarations, declaration, scope, resolution, expected)
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
  const initialExpected =
    typeof expected === 'string' && Scalar.isSpelling(expected) && node.kind === 'InfixExpression'
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
    initialExpected.length === 0 &&
    firstType?._tag === 'Available' &&
    typeof firstType.type === 'string' &&
    Scalar.isSpelling(firstType.type) &&
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
  const selectedActor: Operator.Actor =
    selectedFirstType?._tag === 'Available' && Scalar.isSpelling(selectedFirstType.type)
      ? selectedFirstType.type
      : Scalar.defaultInteger.spelling
  const target = Operator.target(operator, selectedActor)
  const signature = builtinSignature(target.actor, target.operation)
  if (signature === undefined) throw new RangeError('Compiler operator table is inconsistent')
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedBuiltin',
    spelling: `${target.actor}.${target.operation}`,
    token: operatorToken,
    actor: target.actor,
    operation: signature.operation,
    parameters: signature.parameters,
    result: signature.result,
  })
  const contract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    isAvailableSyntax(node),
  )
  const expressionType =
    contract.fact._tag === 'Compatible'
      ? availableExpressionType(signature.result)
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
  )
}

const effectCaptureAccess = (arguments_: ReadonlyArray<ArgumentFact>): Type.Effect['access'] => {
  const accessOf = (expression: ExpressionFact): Type.Effect['access'] => {
    if (expression._tag === 'Move') return 'Take'
    if (expression._tag === 'Borrow')
      return expression.access === 'Exclusive' ? 'Exclusive' : 'Shared'
    if (expression._tag === 'Grouped') return accessOf(expression.expression)
    return 'Shared'
  }
  const accesses = arguments_.map((argument) => accessOf(argument.expression))
  return accesses.includes('Take')
    ? 'Take'
    : accesses.includes('Exclusive')
      ? 'Exclusive'
      : 'Shared'
}

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

const capabilityOperationReference = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  actor: DeclarationIndex.StructFact | Intrinsic.Actor | undefined,
): IntrinsicReferenceFact => {
  const identifiers = callReferenceTokens(node)
  const actorToken = identifiers.at(0)
  const operationToken = identifiers.at(1)
  const operation =
    operationToken === undefined
      ? undefined
      : Intrinsic.findOperation('Effect', spelling(source, operationToken))
  return actor === undefined ||
    actorToken === undefined ||
    operationToken === undefined ||
    operation?.rule._tag !== 'EffectRule' ||
    (operation.rule.operation !== 'Provide' && operation.rule.operation !== 'ProvideWith')
    ? Object.freeze({ _tag: 'UnavailableIntrinsicReference', syntax: node })
    : Object.freeze({
        _tag: 'ResolvedCapabilityOperationReference',
        actor,
        operation,
        actorToken,
        operationToken,
      })
}

const isEffectCatchTarget = (source: SourceFile.SourceFile, node: SyntaxTree.Node): boolean =>
  intrinsicOperationTarget(source, node)?.rule._tag === 'EffectRule' &&
  intrinsicOperationTarget(source, node)?.rule.operation === 'Catch'

const isEffectRetryTarget = (source: SourceFile.SourceFile, node: SyntaxTree.Node): boolean =>
  intrinsicOperationTarget(source, node)?.rule._tag === 'EffectRule' &&
  intrinsicOperationTarget(source, node)?.rule.operation === 'Retry'

const effectTransformTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): 'Map' | 'FlatMap' | 'Tap' | undefined => {
  const target = intrinsicOperationTarget(source, node)
  if (target?.rule._tag !== 'EffectRule') return undefined
  return target.rule.operation === 'Map' ||
    target.rule.operation === 'FlatMap' ||
    target.rule.operation === 'Tap'
    ? target.rule.operation
    : undefined
}

const combineEffectAccess = (
  ...accesses: ReadonlyArray<Type.Effect['access'] | Type.CallableMode>
): Type.Effect['access'] =>
  accesses.includes('Take') ? 'Take' : accesses.includes('Exclusive') ? 'Exclusive' : 'Shared'

const analyzeEffectTransform = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  operation: 'Map' | 'FlatMap' | 'Tap',
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
  const callbackNode = argumentNodes.at(pipelined ? 0 : 1)
  const protectedResult =
    protectedNode === undefined
      ? undefined
      : analyzeExpression(source, protectedNode, declarations, declaration, scope, resolution)
  const callbackResult =
    callbackNode === undefined
      ? undefined
      : analyzeExpression(source, callbackNode, declarations, declaration, scope, resolution)
  const protectedEffect =
    protectedResult?.type !== undefined && Type.isEffect(protectedResult.type)
      ? protectedResult.type
      : undefined
  const callback =
    callbackResult?.type !== undefined && Type.isCallable(callbackResult.type)
      ? callbackResult.type
      : undefined
  const callbackEffect =
    callback !== undefined && Type.isEffect(callback.result) ? callback.result : undefined
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...(protectedResult?.diagnostics ?? []),
    ...(callbackResult?.diagnostics ?? []),
  ]
  let valid = true
  const reject = (detail: string, span: SourceSpan.SourceSpan = node.span): void => {
    diagnostics.push(Diagnostic.invalidEffectHandler(detail, span))
    valid = false
  }
  if (argumentNodes.length !== (pipelined ? 1 : 2))
    reject(`${operation.toLowerCase()} requires one Effect and one unary callable`)
  if (protectedEffect === undefined)
    reject('the protected argument is not an Effect', protectedNode?.span)
  if (callback === undefined || callback.parameters.length !== 1)
    reject('the callback must be one unary callable value', callbackNode?.span)
  const callbackInput = callback?.parameters.at(0)
  if (
    protectedEffect !== undefined &&
    callbackInput !== undefined &&
    !typesCompatible(protectedEffect.success, callbackInput)
  )
    reject(
      `callback input ${Type.encode(callbackInput)} does not accept ${Type.encode(protectedEffect.success)}`,
      callbackNode?.span,
    )
  if ((operation === 'FlatMap' || operation === 'Tap') && callbackEffect === undefined)
    reject(`${operation.toLowerCase()} requires a callback returning Effect`, callbackNode?.span)

  const resultType = (() => {
    if (!valid || protectedEffect === undefined || callback === undefined)
      return unavailableExpressionType
    if (operation === 'Map') {
      return availableExpressionType(
        Type.effect(
          callback.result,
          protectedEffect.failures,
          combineEffectAccess(protectedEffect.access, callback.mode),
          protectedEffect.requirements,
        ),
      )
    }
    if (callbackEffect === undefined) return unavailableExpressionType
    return availableExpressionType(
      Type.effect(
        operation === 'Tap' ? protectedEffect.success : callbackEffect.success,
        [...protectedEffect.failures, ...callbackEffect.failures],
        combineEffectAccess(protectedEffect.access, callback.mode, callbackEffect.access),
        [...protectedEffect.requirements, ...callbackEffect.requirements],
      ),
    )
  })()
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectTransform',
      reference: intrinsicReference(source, target),
      operation,
      protected: protectedResult?.fact ?? unavailableExpression(node),
      callback: callbackResult?.fact ?? unavailableExpression(node),
      type: resultType,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: resultType._tag === 'Available' ? resultType.type : undefined,
  })
}

const capabilityEffectOperationTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): Intrinsic.Operation | undefined => {
  const token = callReferenceTokens(node).at(1)
  if (token === undefined) return undefined
  const operation = Intrinsic.findOperation('Effect', spelling(source, token))
  return operation?.rule._tag === 'EffectRule' &&
    (operation.rule.operation === 'Provide' || operation.rule.operation === 'ProvideWith')
    ? operation
    : undefined
}

const isEffectProvideTarget = (source: SourceFile.SourceFile, node: SyntaxTree.Node): boolean =>
  capabilityEffectOperationTarget(source, node)?.rule._tag === 'EffectRule' &&
  capabilityEffectOperationTarget(source, node)?.rule.operation === 'Provide'

const isEffectProvideWithTarget = (source: SourceFile.SourceFile, node: SyntaxTree.Node): boolean =>
  capabilityEffectOperationTarget(source, node)?.rule._tag === 'EffectRule' &&
  capabilityEffectOperationTarget(source, node)?.rule.operation === 'ProvideWith'

const analyzeEffectProvide = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const pipelined = node.kind === 'PipelineExpression'
  const target = pipelined ? (pipelineCallable(node) ?? node) : node
  const identifiers = callReferenceTokens(target)
  const qualifier = identifiers.at(0)
  const capabilityLookup =
    qualifier === undefined
      ? undefined
      : NameResolution.lookup(resolution.scope, resolution.index, spelling(source, qualifier))
  const capabilityDeclaration =
    capabilityLookup?._tag === 'Resolved' &&
    capabilityLookup.declaration._tag === 'StructDeclaration' &&
    capabilityLookup.declaration.canonical._tag === 'Canonical'
      ? capabilityLookup.declaration
      : undefined
  const capability =
    capabilityLookup?._tag === 'Intrinsic' && capabilityLookup.actor === 'Allocator'
      ? Type.allocator
      : capabilityDeclaration === undefined
        ? undefined
        : Type.nominal(
            capabilityDeclaration.canonical._tag === 'Canonical'
              ? capabilityDeclaration.canonical.id.module
              : source.id,
            capabilityDeclaration.name._tag === 'Present'
              ? capabilityDeclaration.name.spelling
              : qualifier === undefined
                ? '?'
                : spelling(source, qualifier),
          )
  const list = SyntaxTree.directNode(target, 'ArgumentList')
  const nodes =
    list?.children.filter((element): element is SyntaxTree.Node => SyntaxTree.isNode(element)) ?? []
  const roleNode = nodes.find((candidate) => candidate.kind === 'RoleExpression')
  const argumentNodes = nodes.filter(isRecursiveArgumentNode)
  const protectedNode = pipelined ? pipelineInput(node) : argumentNodes.at(0)
  const providerNode = argumentNodes.at(pipelined ? 0 : 1)
  const protectedResult =
    protectedNode === undefined
      ? undefined
      : analyzeExpression(source, protectedNode, declarations, declaration, scope, resolution)
  const effect =
    protectedResult?.type !== undefined && Type.isEffect(protectedResult.type)
      ? protectedResult.type
      : undefined
  const roleToken = roleNode?.children.find(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
  const explicitRole = roleToken === undefined ? undefined : spelling(source, roleToken)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...(protectedResult?.diagnostics ?? [])]
  let valid = true
  const reject = (detail: string, span: SourceSpan.SourceSpan = node.span): void => {
    diagnostics.push(Diagnostic.invalidEffectProvision(detail, span))
    valid = false
  }
  if (argumentNodes.length !== (pipelined ? 1 : 2))
    reject('provide requires one Effect and one explicit provider borrow')
  if (effect === undefined) reject('the protected argument is not an Effect', protectedNode?.span)
  if (capability === undefined)
    reject('the capability qualifier must name one concrete nominal type', qualifier?.span)

  const providerSubject =
    providerNode?.kind === 'BorrowExpression' || providerNode?.kind === 'MoveExpression'
      ? providerNode.children.find(
          (element): element is SyntaxTree.Node =>
            SyntaxTree.isNode(element) && element.kind === 'IdentifierExpression',
        )
      : undefined
  const providerResult =
    providerSubject === undefined ? undefined : analyzeIdentifier(source, providerSubject, scope)
  diagnostics.push(...(providerResult?.diagnostics ?? []))
  const providerReference =
    providerResult?.fact._tag === 'Identifier'
      ? providerResult.fact.reference._tag === 'ResolvedBinding'
        ? providerResult.fact.reference.binding
        : providerResult.fact.reference._tag === 'Resolved'
          ? providerResult.fact.reference.parameter
          : undefined
      : undefined
  const providerAccess =
    providerNode?.kind === 'MoveExpression'
      ? ('Take' as const)
      : providerNode?.kind === 'BorrowExpression' &&
          SyntaxTree.directToken(providerNode, 'MutKeyword') !== undefined
        ? ('Exclusive' as const)
        : ('Shared' as const)
  const selectedWitness =
    capability === undefined || providerResult?.type === undefined
      ? undefined
      : DeclarationIndex.witness(resolution.index, providerResult.type, capability)
  if (
    (providerNode?.kind !== 'BorrowExpression' && providerNode?.kind !== 'MoveExpression') ||
    providerReference === undefined
  )
    reject('the provider must be a direct &value, &mut value, or move value', providerNode?.span)
  if (
    providerAccess === 'Exclusive' &&
    (providerReference?._tag !== 'BindingFact' || providerReference.mutability !== 'Mutable')
  )
    reject('exclusive provision requires a mutable local binding', providerNode?.span)
  if (
    capability !== undefined &&
    providerResult?.type !== undefined &&
    !Type.equals(providerResult.type, capability) &&
    selectedWitness === undefined
  )
    reject(
      `provider type ${Type.encode(providerResult.type)} does not match ${Type.encode(capability)}`,
      providerNode?.span,
    )
  const candidates =
    effect?.requirements.filter(
      (requirement) => capability !== undefined && Type.equals(requirement.capability, capability),
    ) ?? []
  const selected =
    explicitRole === undefined
      ? candidates.length === 1
        ? candidates[0]
        : undefined
      : candidates.find((candidate) => candidate.role === explicitRole)
  if (selected === undefined) {
    reject(
      candidates.length > 1 && explicitRole === undefined
        ? 'the capability has multiple roles; select one with @Role'
        : 'the Effect does not require the selected capability role',
      roleNode?.span ?? node.span,
    )
  } else if (selected.access === 'Exclusive' && providerAccess === 'Shared') {
    reject('the selected requirement needs an exclusive provider', providerNode?.span)
  }
  const requirements =
    effect === undefined || selected === undefined
      ? []
      : effect.requirements.filter((requirement) => requirement !== selected)
  const access =
    effect?.access === 'Take' || providerAccess === 'Take'
      ? ('Take' as const)
      : effect?.access === 'Exclusive' || providerAccess === 'Exclusive'
        ? ('Exclusive' as const)
        : ('Shared' as const)
  const resultType =
    valid && effect !== undefined
      ? availableExpressionType(Type.effect(effect.success, effect.failures, access, requirements))
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectProvide',
      reference: capabilityOperationReference(
        source,
        target,
        capabilityDeclaration ??
          (capabilityLookup?._tag === 'Intrinsic'
            ? Intrinsic.findActor(capabilityLookup.actor)
            : undefined),
      ),
      protected: protectedResult?.fact ?? unavailableExpression(node),
      ...(providerReference === undefined ||
      capability === undefined ||
      selected === undefined ||
      providerResult?.type === undefined ||
      !Type.isNominal(providerResult.type) ||
      selectedWitness === undefined
        ? {}
        : {
            provider: Object.freeze({
              _tag: 'EffectProviderCapture' as const,
              reference: providerReference,
              capability,
              providerType: providerResult.type,
              witness: selectedWitness,
              role: selected.role,
              access: providerAccess,
              span: providerNode?.span ?? node.span,
            }),
          }),
      type: resultType,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: resultType._tag === 'Available' ? resultType.type : undefined,
  })
}

const analyzeEffectProvideWith = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const pipelined = node.kind === 'PipelineExpression'
  const target = pipelined ? (pipelineCallable(node) ?? node) : node
  const identifiers = callReferenceTokens(target)
  const qualifier = identifiers.at(0)
  const capabilityLookup =
    qualifier === undefined
      ? undefined
      : NameResolution.lookup(resolution.scope, resolution.index, spelling(source, qualifier))
  const capabilityDeclaration =
    capabilityLookup?._tag === 'Resolved' &&
    capabilityLookup.declaration._tag === 'StructDeclaration' &&
    capabilityLookup.declaration.canonical._tag === 'Canonical'
      ? capabilityLookup.declaration
      : undefined
  const capability =
    capabilityLookup?._tag === 'Intrinsic' && capabilityLookup.actor === 'Allocator'
      ? Type.allocator
      : capabilityDeclaration?.canonical._tag === 'Canonical'
        ? Type.nominal(
            capabilityDeclaration.canonical.id.module,
            capabilityDeclaration.canonical.id.name,
          )
        : undefined
  const list = SyntaxTree.directNode(target, 'ArgumentList')
  const nodes =
    list?.children.filter((element): element is SyntaxTree.Node => SyntaxTree.isNode(element)) ?? []
  const roleNode = nodes.find((candidate) => candidate.kind === 'RoleExpression')
  const argumentNodes = nodes.filter(isRecursiveArgumentNode)
  const protectedNode = pipelined ? pipelineInput(node) : argumentNodes.at(0)
  const acquisitionNode = argumentNodes.at(pipelined ? 0 : 1)
  const protectedResult =
    protectedNode === undefined
      ? undefined
      : analyzeExpression(source, protectedNode, declarations, declaration, scope, resolution)
  const acquisitionResult =
    acquisitionNode === undefined
      ? undefined
      : analyzeExpression(source, acquisitionNode, declarations, declaration, scope, resolution)
  const protectedEffect =
    protectedResult?.type !== undefined && Type.isEffect(protectedResult.type)
      ? protectedResult.type
      : undefined
  const acquisitionEffect =
    acquisitionResult?.type !== undefined && Type.isEffect(acquisitionResult.type)
      ? acquisitionResult.type
      : undefined
  const roleToken = roleNode?.children.find(
    (element): element is Token.Token =>
      SyntaxTree.isToken(element) && element.kind === 'Identifier',
  )
  const explicitRole = roleToken === undefined ? undefined : spelling(source, roleToken)
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...(protectedResult?.diagnostics ?? []),
    ...(acquisitionResult?.diagnostics ?? []),
  ]
  let valid = true
  const reject = (detail: string, span: SourceSpan.SourceSpan = node.span): void => {
    diagnostics.push(Diagnostic.invalidEffectProvision(detail, span))
    valid = false
  }
  if (argumentNodes.length !== (pipelined ? 1 : 2))
    reject('provideWith requires one protected Effect and one acquisition Effect')
  if (protectedEffect === undefined)
    reject('the protected argument is not an Effect', protectedNode?.span)
  if (acquisitionEffect === undefined)
    reject('the acquisition argument is not an Effect', acquisitionNode?.span)
  if (capability === undefined)
    reject('the capability qualifier must name one concrete nominal type', qualifier?.span)
  if (
    capability !== undefined &&
    acquisitionEffect !== undefined &&
    !Type.equals(acquisitionEffect.success, capability) &&
    !DeclarationIndex.conforms(resolution.index, acquisitionEffect.success, capability)
  )
    reject(
      `acquisition succeeds with ${Type.encode(acquisitionEffect.success)}, not ${Type.encode(capability)}`,
      acquisitionNode?.span,
    )
  const candidates =
    protectedEffect?.requirements.filter(
      (requirement) => capability !== undefined && Type.equals(requirement.capability, capability),
    ) ?? []
  const selected =
    explicitRole === undefined
      ? candidates.length === 1
        ? candidates[0]
        : undefined
      : candidates.find((candidate) => candidate.role === explicitRole)
  if (selected === undefined)
    reject(
      candidates.length > 1 && explicitRole === undefined
        ? 'the capability has multiple roles; select one with @Role'
        : 'the Effect does not require the selected capability role',
      roleNode?.span ?? node.span,
    )
  const access =
    protectedEffect?.access === 'Take' || acquisitionEffect?.access === 'Take'
      ? ('Take' as const)
      : protectedEffect?.access === 'Exclusive' || acquisitionEffect?.access === 'Exclusive'
        ? ('Exclusive' as const)
        : ('Shared' as const)
  const resultType =
    valid &&
    protectedEffect !== undefined &&
    acquisitionEffect !== undefined &&
    capability !== undefined &&
    selected !== undefined
      ? availableExpressionType(
          Type.effect(
            protectedEffect.success,
            [...protectedEffect.failures, ...acquisitionEffect.failures],
            access,
            [
              ...protectedEffect.requirements.filter((requirement) => requirement !== selected),
              ...acquisitionEffect.requirements,
            ],
          ),
        )
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectProvideWith',
      reference: capabilityOperationReference(
        source,
        target,
        capabilityDeclaration ??
          (capabilityLookup?._tag === 'Intrinsic'
            ? Intrinsic.findActor(capabilityLookup.actor)
            : undefined),
      ),
      protected: protectedResult?.fact ?? unavailableExpression(node),
      acquisition: acquisitionResult?.fact ?? unavailableExpression(node),
      ...(capability === undefined ? {} : { capability }),
      ...(selected === undefined ? {} : { role: selected.role }),
      type: resultType,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: resultType._tag === 'Available' ? resultType.type : undefined,
  })
}

const analyzeEffectCatch = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const pipelined = node.kind === 'PipelineExpression'
  const target = pipelined ? (pipelineCallable(node) ?? node) : node
  const typeArguments = analyzeCallTypeArguments(source, target, declaration, resolution)
  const handledCandidate = typeArguments.types?.at(0)
  const handled =
    typeArguments.types?.length === 1 &&
    handledCandidate !== undefined &&
    Type.isNominal(handledCandidate) &&
    Type.isConcrete(handledCandidate)
      ? handledCandidate
      : undefined
  const list = SyntaxTree.directNode(target, 'ArgumentList')
  const arguments_ =
    list?.children.filter((element): element is SyntaxTree.Node =>
      isRecursiveArgumentNode(element),
    ) ?? []
  const protectedNode = pipelined ? pipelineInput(node) : arguments_.at(0)
  const handlerNode = arguments_.at(pipelined ? 0 : 1)
  const protectedResult =
    protectedNode === undefined
      ? undefined
      : analyzeExpression(source, protectedNode, declarations, declaration, scope, resolution)
  const handlerResult =
    handlerNode === undefined
      ? undefined
      : analyzeExpression(source, handlerNode, declarations, declaration, scope, resolution)
  const handler =
    handlerResult?.type !== undefined && Type.isCallable(handlerResult.type)
      ? handlerResult.type
      : undefined
  const handlerEffect =
    handler !== undefined && Type.isEffect(handler.result) ? handler.result : undefined
  const effect =
    protectedResult?.type !== undefined && Type.isEffect(protectedResult.type)
      ? protectedResult.type
      : undefined
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...typeArguments.diagnostics,
    ...(protectedResult?.diagnostics ?? []),
    ...(handlerResult?.diagnostics ?? []),
  ]
  let valid = true
  const reject = (detail: string, span: SourceSpan.SourceSpan = node.span): void => {
    diagnostics.push(Diagnostic.invalidEffectHandler(detail, span))
    valid = false
  }
  if (typeArguments.types?.length !== 1 || handled === undefined)
    reject('catch requires one concrete nominal type argument')
  if (arguments_.length !== (pipelined ? 1 : 2))
    reject('catch requires a protected effect and one handler function')
  if (effect === undefined) reject('the protected argument is not a effect', protectedNode?.span)
  if (handler === undefined || handlerEffect === undefined)
    reject('the handler must be a unary callable returning Effect', handlerNode?.span)
  if (handler?.parameters.length !== 1) reject('the handler must accept exactly one failure')
  const handlerParameter = handler?.parameters.at(0)
  if (
    handled !== undefined &&
    (handlerParameter === undefined || !Type.equals(handlerParameter, handled))
  )
    reject('the handler parameter must exactly match the caught failure')
  if (
    effect !== undefined &&
    handled !== undefined &&
    !effect.failures.some((failure) => Type.equals(failure, handled))
  )
    reject('the caught failure is absent from the protected row')
  if (
    effect !== undefined &&
    handlerEffect !== undefined &&
    !Type.equals(effect.success, handlerEffect.success)
  )
    reject('the handler success type must match the protected effect')
  const residual =
    effect === undefined || handled === undefined
      ? []
      : effect.failures.filter((failure) => !Type.equals(failure, handled))
  const failures = [...residual, ...(handlerEffect?.failures ?? [])]
  const requirements = [...(effect?.requirements ?? []), ...(handlerEffect?.requirements ?? [])]
  const access = combineEffectAccess(
    effect?.access ?? 'Shared',
    handler?.mode ?? 'Shared',
    handlerEffect?.access ?? 'Shared',
  )
  const resultType =
    valid && effect !== undefined
      ? availableExpressionType(Type.effect(effect.success, failures, access, requirements))
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectCatch',
      reference: intrinsicReference(source, target),
      typeArguments: typeArguments.facts,
      protected: protectedResult?.fact ?? unavailableExpression(node),
      ...(handled === undefined ? {} : { handled }),
      handler: handlerResult?.fact ?? unavailableExpression(node),
      ...(handlerEffect === undefined ? {} : { handlerEffect }),
      type: resultType,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: resultType._tag === 'Available' ? resultType.type : undefined,
  })
}

const analyzeEffectRetry = (
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
  const arguments_ =
    list?.children.filter((element): element is SyntaxTree.Node =>
      isRecursiveArgumentNode(element),
    ) ?? []
  const protectedNode = pipelined ? pipelineInput(node) : arguments_.at(0)
  const retriesNode = arguments_.at(pipelined ? 0 : 1)
  const protectedResult =
    protectedNode === undefined
      ? undefined
      : analyzeExpression(source, protectedNode, declarations, declaration, scope, resolution)
  const retriesResult =
    retriesNode === undefined
      ? undefined
      : analyzeExpression(source, retriesNode, declarations, declaration, scope, resolution, 'i32')
  const effect =
    protectedResult?.type !== undefined && Type.isEffect(protectedResult.type)
      ? protectedResult.type
      : undefined
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...(protectedResult?.diagnostics ?? []),
    ...(retriesResult?.diagnostics ?? []),
  ]
  let valid = true
  const reject = (detail: string, span: SourceSpan.SourceSpan = node.span): void => {
    diagnostics.push(Diagnostic.invalidEffectRetry(detail, span))
    valid = false
  }
  if (arguments_.length !== (pipelined ? 1 : 2))
    reject('retry requires one Effect and one i32 retry count')
  if (effect === undefined) reject('the protected argument is not an Effect', protectedNode?.span)
  if (effect?.access === 'Take') reject('a take-once Effect cannot be retried', protectedNode?.span)
  if (retriesResult?.type !== 'i32') reject('the retry count must be i32', retriesNode?.span)
  const type =
    valid && effect !== undefined ? availableExpressionType(effect) : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectRetry',
      reference: intrinsicReference(source, target),
      protected: protectedResult?.fact ?? unavailableExpression(node),
      retries: retriesResult?.fact ?? unavailableExpression(node),
      type,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: type._tag === 'Available' ? type.type : undefined,
  })
}

const effectCaptureFacts = (
  statements: ReadonlyArray<StatementFact>,
  firstLocalBinding: number,
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
      fact.type._tag === 'Available' && typeof fact.type.type === 'string',
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
      case 'EffectCatch':
        expression(fact.protected)
        expression(fact.handler)
        return
      case 'EffectRetry':
        expression(fact.protected)
        expression(fact.retries)
        return
      case 'EffectTransform':
        expression(fact.protected)
        expression(fact.callback)
        return
      case 'EffectProvide':
        expression(fact.protected)
        recordReference(
          fact.provider?.reference,
          fact.provider?.access ?? 'Shared',
          fact.syntax.span,
          false,
        )
        return
      case 'EffectBlock':
        return
      case 'Integer':
      case 'Boolean':
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
        case 'IfStatement':
          expression(statement.condition)
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
  if (node.kind === 'EffectExpression') {
    const block = SyntaxTree.directNode(node, 'Block')
    if (block === undefined)
      return Object.freeze({
        fact: Object.freeze({
          _tag: 'EffectBlock',
          site: Object.freeze({
            _tag: 'EffectSiteId',
            function: declaration.id,
            span: node.span,
          }),
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
    const firstLocalBinding =
      Math.max(-1, ...scope.bindings.map((binding) => binding.id.ordinal)) + 1
    const nested: BodyContext = {
      source,
      declaration,
      declarations,
      bindings: [],
      diagnostics: [],
      regions: [],
      loops: [],
      resolution,
      bindingBase: firstLocalBinding,
      regionBase: 1_000_000 + node.span.start * 100,
      effectBlock: true,
    }
    const statements = analyzeStatements(nested, block, scope)
    const returned: Array<ExpressionFact> = []
    const failures: Array<Type.Nominal> = []
    const collectTerminals = (items: ReadonlyArray<StatementFact>): void => {
      for (const statement of items) {
        if (statement._tag === 'ReturnStatement') returned.push(statement.expression)
        else if (statement._tag === 'FailStatement' && statement.failure !== undefined)
          failures.push(statement.failure)
        else if (statement._tag === 'IfStatement') {
          collectTerminals(statement.taken)
          collectTerminals(statement.otherwise)
        } else if (statement._tag === 'WhileStatement') collectTerminals(statement.body)
      }
    }
    collectTerminals(statements)
    const success = returned.at(-1)?.type
    const captures = effectCaptureFacts(statements, firstLocalBinding)
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
        site: Object.freeze({
          _tag: 'EffectSiteId',
          function: declaration.id,
          span: node.span,
        }),
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
    const type =
      floating._tag === 'Available'
        ? availableExpressionType(floating.type)
        : unavailableExpressionType
    return Object.freeze({
      fact: Object.freeze({ _tag: 'Floating', floating, type, syntax: node }),
      diagnostics: Object.freeze([]),
      type: floating._tag === 'Available' ? floating.type : undefined,
    })
  }

  if (node.kind === 'StaticTextLiteralExpression') {
    const token = directToken(node, 'TextLiteral') ?? directToken(node, 'ByteStringLiteral')
    const bytes =
      token === undefined ? undefined : Option.getOrUndefined(SourceFile.slice(source, token.span))
    const result =
      bytes === undefined
        ? undefined
        : StaticText.decode(Array.from(bytes), token?.kind === 'ByteStringLiteral')
    const diagnostic =
      result?._tag === 'Invalid'
        ? Diagnostic.invalidStaticLiteral(result.detail, node.span)
        : undefined
    const data = result?._tag === 'Decoded' ? result.data : undefined
    const type =
      data === undefined
        ? unavailableExpressionType
        : availableExpressionType(Type.slice('Shared', 'u8'))
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
    return analyzeFunctionItem(source, node, declarations, resolution) ?? value
  }

  if (node.kind === 'RunExpression') {
    const subjectNode = node.children.find(isExpressionNode)
    const subject =
      subjectNode === undefined
        ? undefined
        : analyzeExpression(source, subjectNode, declarations, declaration, scope, resolution)
    if (subject === undefined) throw new RangeError('Run expression requires one effect subject')
    const effect =
      subject.type !== undefined && Type.isEffect(subject.type) ? subject.type : undefined
    const type =
      effect !== undefined ? availableExpressionType(effect.success) : unavailableExpressionType
    const allowed =
      declaration.functionKind === 'Effect' ? declaration.failureRow.failures : Object.freeze([])
    const unhandled =
      effect?.failures.filter(
        (failure) => !allowed.some((candidate) => Type.equals(candidate, failure)),
      ) ?? []
    const allowedRequirements =
      declaration.functionKind === 'Effect'
        ? declaration.requirementRow.requirements
        : Object.freeze<Type.Requirement[]>([])
    const unsatisfiedRequirements =
      effect?.requirements.filter(
        (requirement) =>
          !allowedRequirements.some(
            (allowed) =>
              Type.equals(allowed.capability, requirement.capability) &&
              allowed.role === requirement.role &&
              (allowed.access === 'Exclusive' || allowed.access === requirement.access),
          ),
      ) ?? []
    const diagnostics = [...subject.diagnostics]
    if (effect === undefined && subject.type !== undefined)
      diagnostics.push(Diagnostic.runNonEffect(Type.encode(subject.type), node.span))
    if (unhandled.length > 0)
      diagnostics.push(Diagnostic.unhandledEffectFailures(unhandled.map(Type.encode), node.span))
    if (unsatisfiedRequirements.length > 0)
      diagnostics.push(
        Diagnostic.unhandledEffectRequirements(
          unsatisfiedRequirements.map(
            (requirement) =>
              `${requirement.access === 'Exclusive' ? '&mut ' : '&'}${Type.encode(requirement.capability)}${requirement.role === 'DefaultRole' ? '' : `@${requirement.role}`}`,
          ),
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
    return analyzeMatch(source, node, declarations, declaration, scope, resolution, expected)
  }

  if (node.kind === 'StructLiteralExpression') {
    return analyzeStructLiteral(source, node, declarations, declaration, scope, resolution)
  }

  if (node.kind === 'ArrayLiteralExpression') {
    return analyzeArrayLiteral(source, node, declarations, declaration, scope, resolution, expected)
  }

  if (node.kind === 'FieldProjectionExpression') {
    return (
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
    const transform =
      operationTarget === undefined ? undefined : effectTransformTarget(source, operationTarget)
    if (transform !== undefined)
      return analyzeEffectTransform(
        source,
        node,
        transform,
        declarations,
        declaration,
        scope,
        resolution,
      )
    if (operationTarget !== undefined && isEffectCatchTarget(source, operationTarget))
      return analyzeEffectCatch(source, node, declarations, declaration, scope, resolution)
    if (operationTarget !== undefined && isEffectRetryTarget(source, operationTarget))
      return analyzeEffectRetry(source, node, declarations, declaration, scope, resolution)
    if (operationTarget !== undefined && isEffectProvideWithTarget(source, operationTarget))
      return analyzeEffectProvideWith(source, node, declarations, declaration, scope, resolution)
    if (operationTarget !== undefined && isEffectProvideTarget(source, operationTarget))
      return analyzeEffectProvide(source, node, declarations, declaration, scope, resolution)
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
    ((calleeResult.type !== undefined && Type.isCallable(calleeResult.type)) ||
      (calleeResult.type !== undefined && calleeNode.kind !== 'IdentifierExpression') ||
      resolvedValueCallee)
  ) {
    return finishCallableApplication(node, calleeResult, argumentsResult, callTypeArguments)
  }

  const identifiers = callReferenceTokens(node)
  if (identifiers.length === 2) {
    const qualifierToken = identifiers.at(0)
    const memberToken = identifiers.at(1)
    if (qualifierToken === undefined || memberToken === undefined)
      return analyzeBuiltinCall(source, node, argumentsResult, callTypeArguments, resolution)
    const qualifier = spelling(source, qualifierToken)
    const member = spelling(source, memberToken)
    if (qualifier === 'Place' && member === 'replace') {
      return analyzePlaceReplace(source, node, declarations, declaration, scope, resolution)
    }
    const qualifierLookup = NameResolution.lookup(resolution.scope, resolution.index, qualifier)
    if (qualifierLookup._tag === 'Intrinsic')
      return analyzeBuiltinCall(source, node, argumentsResult, callTypeArguments, resolution)
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
      return finishDeclarationCall(node, reference, argumentsResult, callTypeArguments, diagnostic)
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
    return finishDeclarationCall(node, reference, argumentsResult, callTypeArguments, diagnostic)
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
    reference.declaration.parameters.length >= 2 &&
    argumentsResult.facts.length === reference.declaration.parameters.length - 1
  ) {
    return finishCallableSection(node, reference, argumentsResult, callTypeArguments)
  }
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
  )
  const syntaxAvailable = hasAvailableCallSyntax(node)
  const expressionType =
    syntaxAvailable &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved'
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, SemanticType>()
            const success = Type.substitute(reference.declaration.returnType.type, substitution)
            if (reference.declaration.functionKind !== 'Effect') return success
            return Type.effect(
              success,
              reference.declaration.failureRow.failures.flatMap((failure) => {
                const specialized = Type.substitute(failure, substitution)
                return Type.isNominal(specialized) ? [specialized] : []
              }),
              effectCaptureAccess(argumentsResult.facts),
              reference.declaration.requirementRow.requirements.flatMap((requirement) => {
                const capability = Type.substitute(requirement.capability, substitution)
                return Type.isNominal(capability)
                  ? [Object.freeze({ ...requirement, capability })]
                  : []
              }),
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
): ExpressionResult => {
  if (
    reference._tag === 'Resolved' &&
    reference.declaration.parameters.length >= 2 &&
    argumentsResult.facts.length === reference.declaration.parameters.length - 1
  ) {
    const section = finishCallableSection(node, reference, argumentsResult, callTypeArguments)
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
  )
  const expressionType =
    hasAvailableCallSyntax(node) &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved'
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, SemanticType>()
            const success = Type.substitute(reference.declaration.returnType.type, substitution)
            if (reference.declaration.functionKind !== 'Effect') return success
            return Type.effect(
              success,
              reference.declaration.failureRow.failures.flatMap((failure) => {
                const specialized = Type.substitute(failure, substitution)
                return Type.isNominal(specialized) ? [specialized] : []
              }),
              effectCaptureAccess(argumentsResult.facts),
              reference.declaration.requirementRow.requirements.flatMap((requirement) => {
                const capability = Type.substitute(requirement.capability, substitution)
                return Type.isNominal(capability)
                  ? [Object.freeze({ ...requirement, capability })]
                  : []
              }),
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
  readonly bindingBase?: number
  readonly regionBase?: number
  readonly effectBlock?: true
}

interface ResolutionContext {
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
  readonly unsafeSpans?: ReadonlyArray<SourceSpan.SourceSpan>
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
      const initializerNode = statementExpressionNode(element)
      const initializer = analyzeExpression(
        context.source,
        initializerNode,
        context.declarations,
        context.declaration,
        scope,
        context.resolution,
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
          ordinal: (context.bindingBase ?? 0) + context.bindings.length,
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

    if (element.kind === 'ConditionalStatement') {
      const region = nextRegion()
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
          Diagnostic.conditionNotBool(
            Type.encode(condition.fact.type.type),
            condition.fact.syntax.span,
          ),
        )
      }

      const arms = SyntaxTree.directNodes(element, 'Block')
      const taken =
        arms.at(0) === undefined
          ? []
          : analyzeStatements(context, arms[0] as SyntaxTree.Node, scope, loopStack)
      const otherwiseArm = arms.at(1)
      const otherwise =
        otherwiseArm === undefined ? [] : analyzeStatements(context, otherwiseArm, scope, loopStack)
      facts.push(
        Object.freeze({
          _tag: 'IfStatement',
          condition: condition.fact,
          taken: Object.freeze([...taken]),
          otherwise: Object.freeze([...otherwise]),
          region,
          syntax: element,
        }),
      )
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
        context.diagnostics.push(
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
      )
      if (expression === undefined) {
        throw new RangeError(`Semantic analysis cannot analyze ${expressionNode.kind}`)
      }
      context.diagnostics.push(...expression.diagnostics)
      if (
        !context.effectBlock &&
        context.declaration.returnType._tag === 'Resolved' &&
        expression.type !== undefined &&
        !typesCompatible(expression.type, context.declaration.returnType.type)
      ) {
        const diagnostic = unionConversionDiagnostic(
          expression.type,
          context.declaration.returnType.type,
          expressionNode.span,
        )
        if (diagnostic !== undefined) context.diagnostics.push(diagnostic)
      }
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
        expression.type !== undefined && Type.isNominal(expression.type)
          ? expression.type
          : undefined
      if (!context.effectBlock && context.declaration.functionKind !== 'Effect')
        context.diagnostics.push(Diagnostic.failOutsideEffect(element.span))
      if (expression.type !== undefined && failure === undefined)
        context.diagnostics.push(
          Diagnostic.invalidFailureType(Type.encode(expression.type), expressionNode.span),
        )
      if (
        !context.effectBlock &&
        failure !== undefined &&
        !context.declaration.failureRow.failures.some((candidate) =>
          Type.equals(candidate, failure),
        )
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
  const bodyResolution: ResolutionContext = Object.freeze({
    ...resolution,
    unsafeSpans: Object.freeze(unsafeSpans),
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
  }
  declaration.failureRow.failures.forEach((failure, ordinal) => {
    if (!DeclarationIndex.containsLexicalBorrow(bodyResolution.index, failure)) return
    context.diagnostics.push(
      Diagnostic.providerBackedFailure(
        Type.encode(failure),
        declaration.failureRow.members.at(ordinal)?.syntax.span ??
          declaration.failureRow.syntax?.span ??
          declaration.syntax.span,
      ),
    )
  })
  const statements = analyzeStatements(
    context,
    blockNode,
    Object.freeze({ parameters: declaration.parameters, bindings: [], patternBindings: [] }),
  )

  const trailing = [...statements]
    .reverse()
    .find(
      (statement): statement is Extract<StatementFact, { _tag: 'ReturnStatement' }> =>
        statement._tag === 'ReturnStatement',
    )
  const terminal =
    trailing ??
    [...statements]
      .reverse()
      .find(
        (statement): statement is Extract<StatementFact, { _tag: 'FailStatement' }> =>
          statement._tag === 'FailStatement',
      )
  if (terminal === undefined)
    throw new RangeError('Semantic analysis expected a terminal statement')
  const expression = terminal.expression
  const expressionType = expression.type._tag === 'Available' ? expression.type.type : undefined

  const returnCompatibility =
    terminal._tag === 'ReturnStatement' &&
    declaration.returnType._tag === 'Resolved' &&
    expressionType !== undefined &&
    typesCompatible(expressionType, declaration.returnType.type)
      ? compatible
      : unavailableCompatibility

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'FunctionFact',
      declaration,
      statements,
      bindings: Object.freeze([...context.bindings]),
      regionOrder: Object.freeze([...context.regions]),
      returnedExpression: expression,
      returnCompatibility,
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
      if (statement._tag === 'IfStatement')
        return Object.freeze({
          _tag: 'If',
          condition: hirExpression(statement.condition),
          taken: hirEffectStatements(statement.taken, resultType),
          otherwise: hirEffectStatements(statement.otherwise, resultType),
          region: statement.region,
          span: statement.syntax.span,
        })
      if (statement._tag === 'WriteStatement') {
        const place =
          statement.root?._tag === 'BindingFact'
            ? hirWritePlace(statement.destination, statement.root)
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
    })
  if (reference._tag === 'Resolved' && reference.declaration.canonical._tag === 'Canonical')
    return Object.freeze({
      _tag: 'DeclarationCallableTarget',
      declaration: reference.declaration.canonical.id,
    })
  return undefined
}

const hirExpression = (fact: ExpressionFact, borrow?: Hir.BorrowId): Hir.Expression => {
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
      : Object.freeze({
          _tag: 'StaticTextLiteral',
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
        ? hirWritePlace(fact.destination, fact.root)
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
  if (fact._tag === 'EffectCatch') {
    const protected_ = hirExpression(fact.protected)
    const handler = hirExpression(fact.handler)
    if (
      protected_._tag === 'Unavailable' ||
      handler._tag === 'Unavailable' ||
      fact.handled === undefined ||
      fact.handlerEffect === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectCatch',
      protected: protected_,
      handled: fact.handled,
      handler,
      handlerEffect: fact.handlerEffect,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectRetry') {
    const protected_ = hirExpression(fact.protected)
    const retries = hirExpression(fact.retries)
    if (
      protected_._tag === 'Unavailable' ||
      retries._tag === 'Unavailable' ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectRetry',
      protected: protected_,
      retries,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectTransform') {
    const protected_ = hirExpression(fact.protected)
    const callback = hirExpression(fact.callback)
    if (
      protected_._tag === 'Unavailable' ||
      callback._tag === 'Unavailable' ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectTransform',
      operation: fact.operation,
      protected: protected_,
      callback,
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectProvide') {
    const protected_ = hirExpression(fact.protected)
    if (
      protected_._tag === 'Unavailable' ||
      fact.provider === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectProvide',
      protected: protected_,
      provider: Object.freeze({
        ...(fact.provider.reference._tag === 'BindingFact'
          ? { binding: fact.provider.reference.id }
          : { parameter: fact.provider.reference.id }),
        capability: fact.provider.capability,
        providerType: fact.provider.providerType,
        witness: fact.provider.witness,
        role: fact.provider.role,
        access: fact.provider.access,
        span: fact.provider.span,
      }),
      type: fact.type.type,
      span: fact.syntax.span,
    })
  }
  if (fact._tag === 'EffectProvideWith') {
    const protected_ = hirExpression(fact.protected)
    const acquisition = hirExpression(fact.acquisition)
    if (
      protected_._tag === 'Unavailable' ||
      acquisition._tag === 'Unavailable' ||
      fact.capability === undefined ||
      fact.role === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectProvideWith',
      protected: protected_,
      acquisition,
      capability: fact.capability,
      role: fact.role,
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
          const member = arm.pattern._tag === 'NominalPattern' ? arm.pattern.member : undefined
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
        : Object.freeze({
            _tag: 'ParameterSliceRoot',
            parameter: fact.formation.root.parameter.id,
          })
    if (fact.formation._tag === 'ValueBorrow' && Type.isReference(fact.type.type)) {
      return Object.freeze({
        _tag: 'ValueBorrow',
        borrow,
        root,
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
      omittedParameter: 0,
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
    fact.reference._tag === 'ResolvedBuiltin' &&
    fact.contract._tag === 'Compatible' &&
    fact.type._tag === 'Available'
  ) {
    const directLoanEnds = fact.arguments.flatMap(
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
    return Object.freeze({
      _tag: 'BuiltinCall',
      operation: fact.reference.operation,
      typeArguments: Object.freeze(
        fact._tag === 'Call'
          ? fact.typeArguments.flatMap((argument) =>
              argument.type === undefined ? [] : [argument.type],
            )
          : [],
      ),
      arguments: Object.freeze(
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
      ),
      loanEnds: Object.freeze(
        fact.reference.operation === 'RawBufferSlot'
          ? []
          : [...directLoanEnds, ...nestedSlotLoanEnds],
      ),
      heldLoans: Object.freeze(fact.reference.operation === 'RawBufferSlot' ? directLoanEnds : []),
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
    }
    return Type.isEffect(fact.type.type)
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
  const source = hirExpression(fact, borrow)
  if (source._tag === 'Unavailable') return source
  const compatibility = TypeCompatibility.check(source.type, target)
  if (compatibility._tag === 'Exact') return source
  if (compatibility._tag === 'CallableMode') return source
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

const hirBorrowedWritePlace = (
  fact: ExpressionFact,
  root: ParameterFact,
): Hir.BorrowedWritePlace | undefined => {
  if (
    root.declaredType._tag !== 'Resolved' ||
    !(Type.isSlice(root.declaredType.type) || Type.isReference(root.declaredType.type)) ||
    root.declaredType.type.access !== 'Exclusive'
  ) {
    return undefined
  }
  const selectors: Array<Hir.BorrowedWriteSelector> = []
  const walk = (current: ExpressionFact): boolean => {
    if (current._tag === 'Grouped') return walk(current.expression)
    if (current._tag === 'Identifier') {
      return (
        current.reference._tag === 'Resolved' &&
        current.reference.parameter.id.ordinal === root.id.ordinal
      )
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
    root: root.id,
    slice: root.declaredType.type,
    selectors: Object.freeze(selectors),
    type: fact.type.type,
    span: fact.syntax.span,
  })
}

const statementSpan = (statement: StatementFact): SourceSpan.SourceSpan =>
  statement._tag === 'BindStatement' ? statement.binding.syntax.span : statement.syntax.span

const directStatementExpressions = (statement: StatementFact): ReadonlyArray<ExpressionFact> => {
  switch (statement._tag) {
    case 'BindStatement':
      return Object.freeze([statement.binding.initializer])
    case 'ReturnStatement':
    case 'FailStatement':
    case 'DropStatement':
      return Object.freeze([statement.expression])
    case 'IfStatement':
    case 'WhileStatement':
      return Object.freeze([statement.condition])
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
    case 'EffectCatch':
      return Object.freeze([expression.protected, expression.handler])
    case 'EffectRetry':
      return Object.freeze([expression.protected, expression.retries])
    case 'EffectProvide':
      return Object.freeze([expression.protected])
    case 'EffectProvideWith':
      return Object.freeze([expression.protected, expression.acquisition])
    case 'EffectTransform':
      return Object.freeze([expression.protected, expression.callback])
    case 'CallableSection':
      return Object.freeze(expression.captures.map((capture) => capture.expression))
    case 'CallableApply':
      return Object.freeze([
        expression.callee,
        ...expression.arguments.map((argument) => argument.expression),
      ])
    case 'Operator':
    case 'Call':
      return Object.freeze(expression.arguments.map((argument) => argument.expression))
    case 'EffectBlock':
    case 'Match':
    case 'Integer':
    case 'Floating':
    case 'StaticText':
    case 'Unit':
    case 'Boolean':
    case 'Identifier':
    case 'FunctionItem':
      return Object.freeze([])
  }
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
      })
      for (const statement of statements) {
        for (const expression of directStatementExpressions(statement))
          visitExpression(expression, current)
        if (statement._tag === 'UnsafeStatement')
          visitStatements(statement.statements, current, statement.syntax.span)
        else if (statement._tag === 'IfStatement') {
          visitStatements(statement.taken, current, statement.syntax.span)
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
  const functions = Object.freeze(analyzed.map((result) => result.fact))
  const diagnostics = [
    ...headers.diagnostics,
    ...analyzed.flatMap((result) => result.diagnostics),
  ].sort(compareDiagnostics)
  const hirStatements = (
    facts: ReadonlyArray<StatementFact>,
    resultType?: SemanticType,
  ): ReadonlyArray<Hir.Statement> =>
    Object.freeze(
      facts.map((statement): Hir.Statement => {
        if (statement._tag === 'UnsafeStatement') {
          return Object.freeze({
            _tag: 'Unsafe' as const,
            statements: hirStatements(statement.statements, resultType),
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
            initializer: hirExpression(statement.binding.initializer),
            region: statement.region,
            span: statement.binding.syntax.span,
          })
        }
        if (statement._tag === 'IfStatement') {
          return Object.freeze({
            _tag: 'If' as const,
            condition: hirExpression(statement.condition),
            taken: hirStatements(statement.taken, resultType),
            otherwise: hirStatements(statement.otherwise, resultType),
            region: statement.region,
            span: statement.syntax.span,
          })
        }
        if (statement._tag === 'WriteStatement') {
          const place =
            statement.root?._tag === 'BindingFact'
              ? hirWritePlace(statement.destination, statement.root)
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
            body: hirStatements(statement.body, resultType),
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
            _tag: statement._tag === 'BreakStatement' ? ('Break' as const) : ('Continue' as const),
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
            const copyCapture = (type: Type.Type): boolean =>
              Type.isBuiltin(type) || (Type.isFixedArray(type) && copyCapture(type.element))
            const captures = effectCaptureFacts(fact.statements, 0).map((capture) => {
              if (capture.reference._tag !== 'ParameterDeclaration') return capture
              const declared = capture.reference.declaredType
              if (declared._tag !== 'Resolved' || Type.isSlice(declared.type)) return capture
              return Object.freeze({
                ...capture,
                access: copyCapture(declared.type) ? ('Copy' as const) : ('Take' as const),
              })
            })
            const access = captures.some((capture) => capture.access === 'Take')
              ? 'Take'
              : captures.some((capture) => capture.access === 'Exclusive')
                ? 'Exclusive'
                : 'Shared'
            const type = Type.effect(
              fact.declaration.returnType.type,
              fact.declaration.failureRow.failures,
              access,
              fact.declaration.requirementRow.requirements,
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
                  span: siteSpan,
                }),
                statements: hirStatements(fact.statements, fact.declaration.returnType.type),
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
                parameters: baseContract.parameters,
                result: type,
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
            ),
          })
        })(),
      ),
    ),
  })

  return Object.freeze({
    _tag: 'Elaboration',
    syntax,
    index,
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
