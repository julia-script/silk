import { dual } from 'effect/Function'
import * as Option from 'effect/Option'
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
      readonly returnedBorrowParameter?: number
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
      readonly provider: Type.Parameter
      readonly operation: string
      readonly declaration: DeclarationIndex.ServiceOperationFact
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
      readonly path: ReadonlyArray<DeclarationIndex.FieldId>
    }
  | {
      readonly _tag: 'ParameterRoot'
      readonly parameter: ParameterFact
      readonly path: ReadonlyArray<DeclarationIndex.FieldId>
    }
  | {
      readonly _tag: 'PatternRoot'
      readonly binding: PatternBindingFact
      readonly path: ReadonlyArray<DeclarationIndex.FieldId>
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
  readonly provider: Type.Parameter
  readonly operation: string
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
  readonly site: Hir.CallableSiteId
  readonly reference: CallReferenceFact
  readonly path: ReferencePathFact
  readonly omittedParameter: 0
  readonly captures: ReadonlyArray<CallableCaptureFact>
  readonly retainedDependencies: ReadonlyArray<number>
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
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
  readonly capability: Type.Nominal | Type.Parameter
  readonly providerType: Type.Nominal | Type.Parameter
  readonly witness?: DeclarationIndex.ConformanceWitness
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
      readonly selected?: Type.Nominal
      readonly protectedRow: ReadonlyArray<Type.Nominal>
      readonly handlerRow: ReadonlyArray<Type.Nominal>
      readonly residualRow: ReadonlyArray<Type.Nominal>
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
      readonly failure?: Type.Nominal | Type.FailureProjection
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
  source: SemanticType,
): boolean =>
  declaration.returnType._tag === 'Resolved' &&
  (typesCompatible(source, declaration.returnType.type) ||
    (declaration.opaqueResult !== undefined &&
      Type.haveSameRepresentationShape(source, declaration.returnType.type)))

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
      : Object.freeze({ ...root, path: Object.freeze([...root.path, subject.state.field.id]) })
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
  inferRepresentations = false,
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
  if (inferRepresentations) {
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
      const explicitParameters = candidate.typeParameters.filter(
        (parameter) =>
          parameter.type.kind !== 'CallableRepresentation' &&
          parameter.type.kind !== 'EffectRepresentation',
      )
      const inferredParameterCount = candidate.typeParameters.length - explicitParameters.length
      if (inferredParameterCount > 0 && supplied.length === explicitParameters.length) {
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
            if (resolved?.fact._tag !== 'Resolved') return []
            if (parameter.type.kind === 'Value')
              return Type.isTypeArgument(resolved.fact.type) ? [resolved.fact.type] : []
            if (
              parameter.type.kind === 'FailureRow' &&
              Type.isParameter(resolved.fact.type) &&
              resolved.fact.type.kind === 'FailureRow'
            )
              return [Type.failureRowArgument([], [resolved.fact.type])]
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
  const effectSites = arms.flatMap((arm) =>
    arm.reachable && arm.result._tag === 'EffectBlock'
      ? [Hir.executableSiteKey(arm.result.site)]
      : [],
  )
  const erasesEffectIdentity = new Set(effectSites).size > 1
  if (erasesEffectIdentity) diagnostics.push(Diagnostic.effectIdentityErasure(node.span))
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
  environment?: string,
): Type.ExactRepresentationArgument | undefined => {
  const target = callableRepresentationTarget(reference)
  if (target === undefined) return undefined
  const identity =
    environment ??
    (target._tag === 'Declaration'
      ? `declaration:${target.module}:${target.name}`
      : `builtin:${target.actor}:${target.operation}`)
  return Type.exactRepresentationArgument(
    Type.callableIdentityArgument(identity, target, typeArguments, environment),
    contract,
  )
}

/**
 * Recovers a compile-time representation from semantic expression structure. This is deliberately
 * frontend-owned: later phases consume the retained argument and never reconstruct it from syntax.
 */
export const representationOfExpression = (
  expression: ExpressionFact,
): Type.RepresentationArgument | undefined => {
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
    const site = expression.site
    const environment = `callable:${Hir.executableSiteKey(site)}`
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
      Type.effectIdentityArgument(`effect:${Hir.executableSiteKey(site)}`),
      contract,
    )
  }
  if (expression._tag === 'Identifier' && expression.reference._tag === 'ResolvedBinding')
    return representationOfExpression(expression.reference.binding.initializer)
  if (expression._tag === 'Move') return representationOfExpression(expression.subject)
  if (expression._tag === 'Grouped') return representationOfExpression(expression.expression)
  return undefined
}

interface InferredRepresentation {
  readonly argument: Type.RepresentationArgument
  readonly span: SourceSpan.SourceSpan
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
  const target = resolveStructTarget(source, targetSyntax, resolution, declaration, true)
  const diagnostics: Array<Diagnostic.Diagnostic> = [...target.diagnostics]
  const struct = target.fact._tag === 'Resolved' ? target.fact.struct : undefined
  const nominal = target.fact._tag === 'Resolved' ? target.fact.type : undefined
  const nominalLabel = nominal === undefined ? 'unknown struct' : Type.encode(nominal)
  const inferredRepresentations = new Map<string, InferredRepresentation>()
  if (struct !== undefined && nominal !== undefined) {
    for (const [ordinal, parameter] of struct.typeParameters.entries()) {
      if (
        parameter.type.kind !== 'CallableRepresentation' &&
        parameter.type.kind !== 'EffectRepresentation'
      )
        continue
      const argument = nominal.arguments.at(ordinal)
      if (argument === undefined || !Type.isRepresentationArgument(argument)) continue
      const isOwnPlaceholder =
        Type.isRepresentationParameterArgument(argument) &&
        Type.key(argument.parameter) === Type.key(parameter.type)
      if (!isOwnPlaceholder)
        inferredRepresentations.set(
          Type.key(parameter.type),
          Object.freeze({ argument, span: targetSyntax.span }),
        )
    }
  }
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
            const actualRepresentation = representationOfExpression(expression.fact)
            const requiredArgument = expectedType.representation.argument
            if (actualRepresentation === undefined) {
              representationDiagnostic = Diagnostic.structFieldTypeMismatch(
                name,
                Type.encode(expectedType),
                Type.encode(expression.type),
                expressionNode.span,
              )
            } else if (requiredArgument._tag === 'RepresentationParameterArgument') {
              const parameter = requiredArgument.parameter
              const parameterKey = Type.key(parameter)
              const previousRepresentation = inferredRepresentations.get(parameterKey)
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
                    : expectedType.contract,
                  expectedType.representation.requiredBound,
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
                    Type.encode(expectedType.representation.requiredBound),
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
                    inferredRepresentations.set(
                      parameterKey,
                      Object.freeze({ argument: actualRepresentation, span: expressionNode.span }),
                    )
                } else if (previousRepresentation === undefined) {
                  inferredRepresentations.set(
                    parameterKey,
                    Object.freeze({ argument: actualRepresentation, span: expressionNode.span }),
                  )
                }
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
            for (const [parameterKey, inferred] of inferredRepresentations)
              currentSubstitution.set(parameterKey, inferred.argument)
            const candidateSubstitution = new Map(currentSubstitution)
            if (!Type.infer(expectedType, expression.type, candidateSubstitution)) {
              const specializedExpected = Type.substitute(expectedType, currentSubstitution)
              const divergence = Type.firstRepresentationDivergence(
                specializedExpected,
                expression.type,
              )
              if (divergence !== undefined) {
                const parameter = struct.typeParameters.find((candidate) => {
                  const inferred = inferredRepresentations.get(Type.key(candidate.type))
                  return (
                    inferred !== undefined &&
                    Type.equalsGenericArgument(inferred.argument, divergence.left)
                  )
                })
                const original =
                  parameter === undefined
                    ? undefined
                    : inferredRepresentations.get(Type.key(parameter.type))
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
              for (const parameter of struct.typeParameters) {
                if (
                  parameter.type.kind !== 'CallableRepresentation' &&
                  parameter.type.kind !== 'EffectRepresentation'
                )
                  continue
                const parameterKey = Type.key(parameter.type)
                const inferred = candidateSubstitution.get(parameterKey)
                if (
                  inferredRepresentations.get(parameterKey) === undefined &&
                  inferred !== undefined &&
                  Type.isRepresentationArgument(inferred) &&
                  !(
                    Type.isRepresentationParameterArgument(inferred) &&
                    Type.key(inferred.parameter) === parameterKey
                  )
                )
                  inferredRepresentations.set(
                    parameterKey,
                    Object.freeze({ argument: inferred, span: expressionNode.span }),
                  )
              }
            }
          }
          const compatibilitySubstitution = new Map(structSubstitution)
          for (const [parameterKey, inferred] of inferredRepresentations)
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
          if (
            parameter === undefined ||
            (parameter.kind !== 'CallableRepresentation' &&
              parameter.kind !== 'EffectRepresentation')
          )
            return argument
          return inferredRepresentations.get(Type.key(parameter))?.argument ?? argument
        })
  const unresolvedRepresentations =
    struct === undefined || completedArguments === undefined
      ? []
      : struct.typeParameters.flatMap((parameter, ordinal) => {
          if (
            parameter.type.kind !== 'CallableRepresentation' &&
            parameter.type.kind !== 'EffectRepresentation'
          )
            return []
          const argument = completedArguments.at(ordinal)
          return argument !== undefined &&
            Type.isRepresentationParameterArgument(argument) &&
            Type.key(argument.parameter) === Type.key(parameter.type)
            ? [parameter]
            : []
        })
  for (const parameter of unresolvedRepresentations) {
    diagnostics.push(
      Diagnostic.uninferredTypeParameter(nominalLabel, parameter.type.name, parameter.syntax.span),
    )
  }
  const completedNominal =
    nominal === undefined ||
    completedArguments === undefined ||
    unresolvedRepresentations.length > 0 ||
    (struct !== undefined &&
      Type.substitution(
        struct.typeParameters.map((parameter) => parameter.type),
        completedArguments,
      ) === undefined)
      ? undefined
      : Type.nominal(nominal.module, nominal.name, completedArguments)
  const completedTarget: StructTargetFact =
    completedNominal !== undefined && target.fact._tag === 'Resolved'
      ? Object.freeze({ ...target.fact, type: completedNominal })
      : target.fact

  if (struct !== undefined && completedNominal !== undefined) {
    for (const field of struct.fields) {
      if (field.name._tag !== 'Present' || seen.has(field.name.spelling)) continue
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
          ? DeclarationIndex.lookup(resolution.index, 'silk/effects', memberSpelling)
          : undefined
      if (
        library?._tag === 'Resolved' &&
        library.declaration._tag === 'FunctionDeclaration' &&
        library.declaration.visibility === 'Public'
      ) {
        target = library.declaration
      } else {
        const builtin = builtinSignature(qualifierSpelling, memberSpelling)
        builtinParameters = builtin?.parameters ?? Object.freeze([])
        builtinTypeParameters = builtin?.typeParameters ?? Object.freeze([])
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
    explicitTypes.length === builtinTypeParameters.length
      ? Type.substitution(builtinTypeParameters, explicitTypes)
      : undefined
  // An explicit prefix is context for the value arguments just as a complete list is: the
  // parameters it binds become concrete expected types, and the ones it leaves open stay symbolic
  // exactly as they are when nothing was written.
  const substitution =
    callTypeArguments?.explicit === true && explicitTypes !== undefined
      ? Type.prefixSubstitution(declaredTypeParameters, explicitTypes)
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
      resolved.fact._tag === 'Resolved' && Type.containsPositionRestrictedBorrow(resolved.fact.type)
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

type SourceCallable = DeclarationFact | DeclarationIndex.ServiceOperationFact

const sourceCallable = (reference: CallReferenceFact): SourceCallable | undefined =>
  reference._tag === 'Resolved'
    ? reference.declaration
    : reference._tag === 'ResolvedServiceOperation'
      ? reference.operation
      : undefined

const sourceCallableTypeParameters = (
  reference: Extract<CallReferenceFact, { readonly _tag: 'Resolved' | 'ResolvedServiceOperation' }>,
): ReadonlyArray<DeclarationIndex.TypeParameterFact> =>
  reference._tag === 'Resolved'
    ? reference.declaration.typeParameters
    : [...reference.service.typeParameters, ...reference.operation.typeParameters]

const callArityDiagnostic = (
  reference: Extract<
    CallReferenceFact,
    {
      readonly _tag:
        | 'Resolved'
        | 'ResolvedBuiltin'
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
  if (expectedCount >= 2 && actualCount < expectedCount - 1)
    return Diagnostic.deeperUnderApplication(reference.spelling, expectedCount, actualCount, span)
  return Diagnostic.wrongCallArity(
    reference._tag === 'ResolvedBuiltin'
      ? Object.freeze({
          _tag: 'BuiltinTarget',
          actor: reference.actor,
          operation: reference.operation,
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
    const argument = fact.type
    if (parameter === undefined || argument === undefined) continue
    if (parameter.kind !== 'Value' || !Type.isTypeArgument(argument)) {
      conflicts.push(
        Object.freeze({
          diagnostic: Diagnostic.genericParameterKindMismatch(
            parameter.name,
            parameter.kind,
            'Value',
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

const specializationSites = (
  mappings: ReadonlyArray<ArgumentMappingFact>,
): ReadonlyArray<SpecializationSite> =>
  Object.freeze(
    mappings.flatMap(
      (mapping, ordinal): ReadonlyArray<SpecializationSite> =>
        mapping.argument.type._tag === 'Available' &&
        mapping.parameter.declaredType._tag === 'Resolved'
          ? [
              Object.freeze({
                ordinal,
                pattern: mapping.parameter.declaredType.type,
                actual: mapping.argument.type.type,
                expression: mapping.argument.expression,
              }),
            ]
          : [],
    ),
  )

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
      }),
      diagnostics: Object.freeze([]),
    })
  }

  if (reference._tag !== 'Resolved' && reference._tag !== 'ResolvedServiceOperation') {
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
  if (callable === undefined) throw new RangeError('resolved source call lost its callable')
  const parameters = callable.parameters
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
  const declaredTypeParameters = sourceCallableTypeParameters(reference).map(
    (parameter) => parameter.type,
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
      specializationSites(mappings),
      call.span,
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
    for (const mapping of mappings) {
      if (
        mapping.argument.type._tag !== 'Available' ||
        mapping.parameter.declaredType._tag !== 'Resolved'
      ) {
        compatible = false
        break
      }
      const pattern = mapping.parameter.declaredType.type
      const supplied = mapping.argument.type.type
      const representedSupplied =
        Type.isRepresented(pattern) &&
        !Type.isRepresented(supplied) &&
        (Type.isCallable(supplied) || Type.isEffect(supplied))
          ? (() => {
              const representation = representationOfExpression(mapping.argument.expression)
              return representation === undefined
                ? undefined
                : Type.represented(supplied, pattern.representation.requiredBound, representation)
            })()
          : supplied
      if (
        representedSupplied === undefined ||
        !Type.infer(pattern, representedSupplied, inferred)
      ) {
        rowFailure = Type.rowInferenceFailure(pattern, supplied)
        compatible = false
        break
      }
    }
    typeArguments = Object.freeze(
      declaredTypeParameters.flatMap((parameter) => {
        const inferredType = inferred.get(Type.key(parameter))
        return inferredType === undefined ? [] : [inferredType]
      }),
    )
    if (!compatible || typeArguments.length !== declaredTypeParameters.length) {
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
  for (const mapping of mappings) {
    if (
      mapping.argument.type._tag !== 'Available' ||
      mapping.parameter.declaredType._tag !== 'Resolved'
    ) {
      continue
    }
    const expected = Type.substitute(mapping.parameter.declaredType.type, substitution)
    const expectedValue = Type.isRepresented(expected) ? expected.contract : expected
    const suppliedValue = Type.isRepresented(mapping.argument.type.type)
      ? mapping.argument.type.type.contract
      : mapping.argument.type.type
    if (
      !typesCompatible(suppliedValue, expectedValue) &&
      !contextualIntegerCompatible(mapping.argument.expression, expectedValue)
    ) {
      const mismatch =
        Type.isCallable(expectedValue) && Type.isCallable(suppliedValue)
          ? Diagnostic.incompatibleCallableSignature(
              Type.encode(expectedValue),
              Type.encode(suppliedValue),
              mapping.argument.syntax.span,
            )
          : Type.isSlice(expectedValue) && Type.isFixedArray(suppliedValue)
            ? Diagnostic.implicitSliceDecay(
                Type.encode(expectedValue),
                mapping.argument.syntax.span,
              )
            : (unionConversionDiagnostic(
                suppliedValue,
                expectedValue,
                mapping.argument.syntax.span,
              ) ??
              Diagnostic.argumentTypeMismatch(
                Type.encode(expectedValue),
                Type.encode(suppliedValue),
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
    }),
    diagnostics: Object.freeze([]),
  })
}

const interfaceConstraintDiagnostics = (
  reference: CallReferenceFact,
  contract: CallContractResult,
  index: DeclarationIndex.Index,
  span: SourceSpan.SourceSpan,
): ReadonlyArray<Diagnostic.Diagnostic> => {
  if (reference._tag !== 'Resolved' || contract.fact._tag !== 'Compatible') return Object.freeze([])
  const substitution = contract.fact.substitution
  return Object.freeze(
    reference.declaration.typeParameters.flatMap((parameter) => {
      const bound = parameter.bound
      if (bound === undefined) return []
      const provider = substitution.get(Type.key(parameter.type))
      if (provider === undefined || !Type.isTypeArgument(provider)) return []
      if (bound._tag !== 'ResolvedBound')
        return [
          Diagnostic.invalidConformance(
            `unknown interface constraint ${bound.spelling}`,
            parameter.syntax.span,
          ),
        ]
      const capability = Type.nominal(bound.capability.module, bound.capability.name, [provider])
      if (!DeclarationIndex.conforms(index, provider, capability))
        return [
          Diagnostic.invalidConformance(
            `${Type.encode(provider)} does not implement ${bound.spelling}`,
            span,
          ),
        ]
      // A witness that exists is not yet a witness that is complete: specialization admits the type
      // argument only when the conformance maps every operation the bound declares, so a bound with
      // more than one operation cannot be half-satisfied.
      return DeclarationIndex.unmappedInterfaceOperations(index, provider, capability).map(
        (operation) =>
          Diagnostic.invalidConformance(
            `${Type.encode(provider)} does not implement ${bound.spelling}.${operation}`,
            span,
          ),
      )
    }),
  )
}

interface BuiltinSignature {
  readonly id: Intrinsic.OperationId
  readonly operation: Hir.BuiltinOperation
  readonly typeParameters?: ReadonlyArray<Type.Parameter>
  readonly parameters: ReadonlyArray<SemanticType>
  readonly result: SemanticType
  readonly unsafe?: boolean
  readonly returnedBorrowParameter?: number
}

const builtinSignature = (actor: string, operation: string): BuiltinSignature | undefined => {
  const catalog = Intrinsic.findOperation(actor, operation)
  if (catalog?.rule._tag !== 'BuiltinRule') return undefined
  return Object.freeze({
    id: catalog.id,
    operation: catalog.rule.operation,
    typeParameters: catalog.rule.typeParameters,
    parameters: catalog.rule.parameters,
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
  return Type.effect(
    declaration.returnType.type,
    declaration.failureRow.failures,
    'Shared',
    declaration.requirementRow.requirements,
    declaration.failureRow.parameters,
    declaration.requirementRow.parameters,
  )
}

const callableTypeOfReference = (reference: CallReferenceFact): Type.Callable | undefined => {
  if (reference._tag === 'ResolvedBuiltin')
    return Type.callable(reference.parameters, reference.result)
  const callable = sourceCallable(reference)
  if (callable === undefined) return undefined
  const parameters = callable.parameters.flatMap((parameter) =>
    parameter.declaredType._tag === 'Resolved' ? [parameter.declaredType.type] : [],
  )
  const result = callableResultType(callable)
  return parameters.length === callable.parameters.length && result !== undefined
    ? Type.callable(parameters, result)
    : undefined
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
  interface_: DeclarationIndex.InterfaceFact,
  member: string,
  parameter: Type.Parameter,
):
  | {
      readonly declaration: DeclarationIndex.ServiceOperationFact
      readonly parameters: ReadonlyArray<SemanticType>
      readonly result: SemanticType
    }
  | undefined => {
  const operation = interface_.operations.find(
    (candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === member,
  )
  if (
    operation === undefined ||
    operation.functionKind !== 'Ordinary' ||
    operation.typeParameters.length > 0 ||
    operation.returnType._tag !== 'Resolved'
  )
    return undefined
  const substitution = Type.substitution(
    interface_.typeParameters.map((declared) => declared.type),
    Object.freeze([parameter]),
  )
  if (substitution === undefined) return undefined
  const parameters = operation.parameters.flatMap((declared) =>
    declared.declaredType._tag === 'Resolved'
      ? [Type.substitute(declared.declaredType.type, substitution)]
      : [],
  )
  if (parameters.length !== operation.parameters.length) return undefined
  return Object.freeze({
    declaration: operation,
    parameters: Object.freeze(parameters),
    result: Type.substitute(operation.returnType.type, substitution),
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
  const bounded = declaration.typeParameters.filter((parameter) => {
    const bound = parameter.bound
    return (
      bound?._tag === 'ResolvedBound' &&
      bound.capability.module === capability.module &&
      bound.capability.name === capability.name &&
      bound.operations.includes(member)
    )
  })
  if (bounded.length === 0) return undefined
  if (bounded.length > 1)
    return Object.freeze({
      _tag: 'AmbiguousBound',
      parameters: Object.freeze(
        bounded.map((parameter) =>
          parameter.name._tag === 'Present' ? parameter.name.spelling : Type.encode(parameter.type),
        ),
      ),
    })
  const parameter = bounded.at(0)
  if (parameter === undefined) return undefined
  const contract = interfaceOperationContract(interface_, member, parameter.type)
  if (contract === undefined) return undefined
  return Object.freeze({
    _tag: 'BoundOperation',
    reference: Object.freeze({
      _tag: 'ResolvedBoundOperation' as const,
      spelling: `${qualifier}.${member}`,
      token: memberToken,
      capability: Type.nominal(capability.module, capability.name, [parameter.type]),
      provider: parameter.type,
      operation: member,
      declaration: contract.declaration,
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
      const library = DeclarationIndex.lookup(resolution.index, 'silk/effects', member)
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

/** A section applies its arguments from the second parameter on: the first one is what it holds. */
const sectionSpecializationSites = (
  declaration: DeclarationFact,
  arguments_: ReadonlyArray<ArgumentFact>,
): ReadonlyArray<SpecializationSite> =>
  Object.freeze(
    arguments_.flatMap((argument, ordinal): ReadonlyArray<SpecializationSite> => {
      const parameter = declaration.parameters.at(ordinal + 1)
      return argument.type._tag === 'Available' && parameter?.declaredType._tag === 'Resolved'
        ? [
            Object.freeze({
              ordinal,
              pattern: parameter.declaredType.type,
              actual: argument.type.type,
              expression: argument.expression,
            }),
          ]
        : []
    }),
  )

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
      // A section takes a prefix the same way an ordinary call does: what the list writes is
      // bound, what the supplied arguments determine is inferred, and the parameter the section
      // still holds open belongs to the captured leading parameter.
      const leading = reference.declaration.parameters.at(0)?.declaredType
      const seeded = seededSpecialization(
        reference.spelling,
        declaredParameters,
        callTypeArguments.facts,
        sectionSpecializationSites(reference.declaration, arguments_),
        call.span,
        new Set(leading?._tag === 'Resolved' ? Type.parameters(leading.type).map(Type.key) : []),
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
      const parameter = reference.declaration.parameters.at(ordinal + 1)
      if (
        argument.type._tag === 'Available' &&
        parameter?.declaredType._tag === 'Resolved' &&
        !Type.infer(parameter.declaredType.type, argument.type.type, substitution)
      ) {
        const rowFailure = Type.rowInferenceFailure(parameter.declaredType.type, argument.type.type)
        diagnostics.push(
          rowFailure === undefined
            ? Diagnostic.typeArgumentInference(reference.spelling, call.span)
            : Diagnostic.contractRowInference(rowFailure, call.span),
        )
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
    // An argument already named as contradicting a written type argument is one mistake, and it
    // was reported where the author wrote the type.
    if (contradicted.has(ordinal)) continue
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
  if (expression.type._tag === 'Available' && Type.isCallable(expression.type.type))
    return expression.type.type.mode === 'Shared' ? 'Copy' : expression.type.type.mode
  if (expression.type._tag === 'Available' && Type.isEffect(expression.type.type))
    return expression.type.type.access === 'Shared' ? 'Copy' : expression.type.type.access
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
  substitution: Type.Substitution,
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

const finishCallableSection = (
  node: SyntaxTree.Node,
  reference: Extract<CallReferenceFact, { readonly _tag: 'Resolved' | 'ResolvedBuiltin' }>,
  argumentsResult: ArgumentsResult,
  callTypeArguments: CallTypeArgumentsResult,
  resolution: ResolutionContext,
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
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'CallableSection',
      site: executableSite('CallableSiteId', resolution, node),
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
  const inferred = new Map<string, Type.GenericArgument>()
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
  const type = (() => {
    if (!valid || callable === undefined) return unavailableExpressionType
    const result = Type.substitute(callable.result, inferred)
    return availableExpressionType(
      Type.isEffect(result)
        ? Type.effect(
            result.success,
            result.failures,
            strongestEffectAccess(
              result.access,
              callable.mode,
              effectExpressionAccess(callee.fact),
              effectCaptureAccess(argumentsResult.facts),
            ),
            result.requirements,
            result.failureParameters,
            result.requirementParameters,
          )
        : result,
    )
  })()
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
          intrinsic: signature.id,
          parameters: instantiatedParameters,
          result: instantiatedResult ?? signature.result,
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
    reference.parameters.length >= 2 &&
    argumentsResult.facts.length === reference.parameters.length - 1
  ) {
    return finishCallableSection(call, reference, argumentsResult, typeArguments, resolution)
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

/**
 * Names the impurity `detail` reports, or `undefined` when the expression tree performs nothing
 * and consumes nothing. `&&` and `||` skip their right operand, so an effect performed or a
 * value consumed there would depend on the left operand's value.
 */
const impurityOf = (expression: ExpressionFact): string | undefined => {
  if (expression._tag === 'Run') return 'an effect site'
  if (expression._tag === 'EffectResult') return 'an effect site'
  if (expression._tag === 'Move') return 'a move'
  for (const child of directExpressionChildren(expression)) {
    const found = impurityOf(child)
    if (found !== undefined) return found
  }
  return undefined
}

/**
 * Analyzes `&&` or `||`. Both operands must be `bool` and the result is `bool`. The right operand
 * must additionally be pure, because it evaluates only when the left operand does not already
 * decide the result.
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
  const spelling = operator === 'And' ? '`&&`' : '`||`'
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
  const right = argumentsResult.facts.at(1)
  const impurity = right === undefined ? undefined : impurityOf(right.expression)
  const purityDiagnostics =
    right === undefined || impurity === undefined
      ? Object.freeze([])
      : Object.freeze([Diagnostic.impureShortCircuitOperand(spelling, impurity, right.syntax.span)])
  const rejected =
    operandDiagnostics.length > 0 ||
    purityDiagnostics.length > 0 ||
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
    diagnostics: Object.freeze([
      ...argumentsResult.diagnostics,
      ...operandDiagnostics,
      ...purityDiagnostics,
    ]),
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
  const boundOperand =
    selectedFirstType?._tag === 'Available' && Type.isParameter(selectedFirstType.type)
      ? selectedFirstType.type
      : undefined
  const interfaceOperation =
    boundOperand === undefined
      ? undefined
      : declaration.typeParameters.flatMap((parameter): ReadonlyArray<InterfaceOperationFact> => {
          // Every operation the bound declares is callable on a bound-typed operand, so an operator
          // stays generic exactly when the bound's contract names the operation it spells.
          const bound = parameter.bound
          if (!Type.equals(parameter.type, boundOperand) || bound?._tag !== 'ResolvedBound')
            return []
          const operationName = `${operator.slice(0, 1).toLowerCase()}${operator.slice(1)}`
          return bound.operations.includes(operationName)
            ? [
                Object.freeze({
                  capability: Type.nominal(bound.capability.module, bound.capability.name, [
                    boundOperand,
                  ]),
                  provider: boundOperand,
                  operation: operationName,
                }),
              ]
            : []
        })[0]
  const genericInterface = interfaceOperation !== undefined
  const selectedActor: Operator.Actor =
    selectedFirstType?._tag === 'Available' && Type.isString(selectedFirstType.type)
      ? 'string'
      : selectedFirstType?._tag === 'Available' && Scalar.isSpelling(selectedFirstType.type)
        ? selectedFirstType.type
        : Scalar.defaultInteger.spelling
  const target = Operator.target(operator, selectedActor)
  const signature = builtinSignature(target.actor, target.operation)
  if (signature === undefined) throw new RangeError('Compiler operator table is inconsistent')
  const genericType = selectedFirstType?._tag === 'Available' ? selectedFirstType.type : undefined
  // A bound operator's contract is the compiler-known operation with the stand-in actor replaced by
  // the bound parameter. Only the positions that carry the actor's own type become generic, so a
  // comparison the bound declares keeps its `bool` result instead of widening to the parameter.
  const actorType: SemanticType | undefined = Scalar.isSpelling(target.actor)
    ? target.actor
    : undefined
  const overActor = (type: SemanticType): SemanticType =>
    genericType !== undefined && actorType !== undefined && Type.equals(type, actorType)
      ? genericType
      : type
  const operatorParameters = genericInterface
    ? Object.freeze(
        signature.parameters.length === operandNodes.length
          ? signature.parameters.map(overActor)
          : operandNodes.map(() => genericType ?? signature.result),
      )
    : signature.parameters
  const operatorResult = genericInterface ? overActor(signature.result) : signature.result
  const reference: CallReferenceFact = Object.freeze({
    _tag: 'ResolvedBuiltin',
    spelling: `${target.actor}.${target.operation}`,
    token: operatorToken,
    actor: target.actor,
    operation: signature.operation,
    intrinsic: signature.id,
    parameters: operatorParameters,
    result: operatorResult,
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
      ...(interfaceOperation === undefined ? {} : { interfaceOperation }),
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

const effectExpressionAccess = (expression: ExpressionFact): Type.Effect['access'] => {
  if (expression._tag === 'Move') {
    if (expression.subject.type._tag === 'Available' && Type.isEffect(expression.subject.type.type))
      return expression.subject.type.type.access
    if (
      expression.subject.type._tag === 'Available' &&
      Type.isCallable(expression.subject.type.type)
    )
      return expression.subject.type.type.mode
    return 'Take'
  }
  if (expression._tag === 'Borrow')
    return expression.access === 'Exclusive' ? 'Exclusive' : 'Shared'
  if (expression._tag === 'Grouped') return effectExpressionAccess(expression.expression)
  if (expression._tag === 'CallableSection') return expression.mode
  if (expression.type._tag === 'Available' && Type.isEffect(expression.type.type))
    return expression.type.type.access
  if (expression.type._tag === 'Available' && Type.isCallable(expression.type.type))
    return expression.type.type.mode
  return 'Shared'
}

const effectCaptureAccess = (arguments_: ReadonlyArray<ArgumentFact>): Type.Effect['access'] => {
  const accesses = arguments_.map((argument) => effectExpressionAccess(argument.expression))
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

const isEffectResultTarget = (source: SourceFile.SourceFile, node: SyntaxTree.Node): boolean =>
  intrinsicOperationTarget(source, node)?.rule._tag === 'EffectRule' &&
  intrinsicOperationTarget(source, node)?.rule.operation === 'Result'

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
  const onlyFailureParameter = protectedEffect?.failureParameters.at(0)
  const failureValue =
    protectedEffect !== undefined &&
    protectedEffect.failures.length === 0 &&
    protectedEffect.failureParameters.length === 1 &&
    onlyFailureParameter !== undefined
      ? Type.failureProjection(onlyFailureParameter)
      : Type.failureValue(protectedEffect?.failures ?? [])
  const type =
    protectedEffect === undefined
      ? unavailableExpressionType
      : availableExpressionType(
          Type.effect(
            Type.result(protectedEffect.success, failureValue),
            [],
            protectedEffect.access,
            protectedEffect.requirements,
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

/**
 * Recognizes the member-selective recovery seam.
 *
 * `Effect.catch` is the only operation at this seam that users spell directly rather than through
 * an `Intrinsic.` wrapper, because its result row — the protected row minus the selected member —
 * has no source-level spelling for a Silk wrapper signature to declare. The predicate therefore
 * matches the qualified source spelling under the intrinsic `Effect` namespace instead of going
 * through `intrinsicOperationTarget`, which only ever sees `Intrinsic.` call heads.
 */
const isEffectCatchTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  resolution: ResolutionContext,
): boolean => {
  const identifiers = callReferenceTokens(node)
  const qualifier = identifiers.at(0)
  const member = identifiers.at(1)
  if (qualifier === undefined || member === undefined || identifiers.length !== 2) return false
  if (spelling(source, qualifier) !== 'Effect' || spelling(source, member) !== 'catch') return false
  // Only the selector form routes here. `Effect.catch(effect, handler)` without a type argument
  // stays the whole-row recovery the stdlib declaration provides, so every existing call site
  // keeps its meaning and its lowering.
  if (SyntaxTree.directNode(node, 'CallTypeArgumentList') === undefined) return false
  const qualified = NameResolution.lookup(resolution.scope, resolution.index, 'Effect')
  return (
    qualified._tag === 'Intrinsic' ||
    (qualified._tag === 'Namespace' && qualified.module === 'silk/effects')
  )
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
  const list = SyntaxTree.directNode(target, 'ArgumentList')
  const argumentNodes =
    list?.children.filter((element): element is SyntaxTree.Node =>
      isRecursiveArgumentNode(element),
    ) ?? []
  const protectedNode = pipelined ? pipelineInput(node) : argumentNodes.at(0)
  const handlerNode = argumentNodes.at(pipelined ? 0 : 1)
  const protectedResult =
    protectedNode === undefined
      ? undefined
      : analyzeExpression(source, protectedNode, declarations, declaration, scope, resolution)
  const handlerResult =
    handlerNode === undefined
      ? undefined
      : analyzeExpression(source, handlerNode, declarations, declaration, scope, resolution)
  const selectorArguments = analyzeCallTypeArguments(source, target, declaration, resolution)
  const diagnostics: Array<Diagnostic.Diagnostic> = [
    ...(protectedResult?.diagnostics ?? []),
    ...(handlerResult?.diagnostics ?? []),
    ...selectorArguments.diagnostics,
  ]
  let valid = true
  const reject = (detail: string, span: SourceSpan.SourceSpan = node.span): void => {
    diagnostics.push(Diagnostic.invalidEffectHandler(detail, span))
    valid = false
  }
  const protectedEffect =
    protectedResult?.type !== undefined && Type.isEffect(protectedResult.type)
      ? protectedResult.type
      : undefined
  if (argumentNodes.length !== (pipelined ? 1 : 2))
    reject('catch requires one Effect and one recovery handler')
  if (protectedEffect === undefined)
    reject('the protected argument is not an Effect', protectedNode?.span)

  // The selector is an ordinary nominal, exactly as `Effect.catch<NotFound>(handler)` spells it.
  // It never becomes a declared row parameter, so it needs neither a failure-row argument form
  // nor partial positional type arguments; this seam reads it before any generic instantiation.
  const selectorTypes = selectorArguments.types ?? Object.freeze([])
  const selectorNode = selectorArguments.facts.at(0)?.syntax
  if (selectorArguments.facts.length !== 1)
    reject(
      `catch selects exactly one failure member, received ${selectorArguments.facts.length}`,
      selectorNode?.span,
    )
  const selectorType = selectorTypes.at(0)
  const selected =
    selectorType !== undefined && Type.isNominal(selectorType) ? selectorType : undefined
  if (selectorType !== undefined && selected === undefined)
    reject(
      `the selected failure ${Type.encode(selectorType)} must be one concrete nominal type`,
      selectorNode?.span,
    )

  const protectedFailures = protectedEffect?.failures ?? Object.freeze([])
  const covered =
    selected === undefined
      ? undefined
      : protectedFailures.find((member) => Type.equals(member, selected))
  if (selected !== undefined && protectedEffect !== undefined && covered === undefined)
    reject(`the protected Effect does not fail with ${Type.encode(selected)}`, selectorNode?.span)
  if (
    selected !== undefined &&
    protectedEffect !== undefined &&
    protectedEffect.failureParameters.length > 0
  )
    reject('catch cannot select a member from an open failure row', selectorNode?.span)

  const handlerType = handlerResult?.type
  const handlerCallable =
    handlerType !== undefined && Type.isCallable(handlerType) ? handlerType : undefined
  if (handlerType !== undefined && handlerCallable === undefined)
    reject('the recovery handler is not a callable', handlerNode?.span)
  const handlerParameter = handlerCallable?.parameters.at(0)
  if (
    handlerCallable !== undefined &&
    (handlerCallable.parameters.length !== 1 || handlerParameter === undefined)
  )
    reject('the recovery handler takes exactly the selected failure', handlerNode?.span)
  if (
    selected !== undefined &&
    handlerParameter !== undefined &&
    !Type.equals(handlerParameter, selected)
  )
    reject(
      `the recovery handler must accept ${Type.encode(selected)} but accepts ${Type.encode(handlerParameter)}`,
      handlerNode?.span,
    )
  const handlerEffect =
    handlerCallable !== undefined && Type.isEffect(handlerCallable.result)
      ? handlerCallable.result
      : undefined
  if (handlerCallable !== undefined && handlerEffect === undefined)
    reject('the recovery handler must return an Effect', handlerNode?.span)
  if (
    handlerEffect !== undefined &&
    protectedEffect !== undefined &&
    !Type.equals(handlerEffect.success, protectedEffect.success)
  )
    reject(
      `the recovery handler must produce ${Type.encode(protectedEffect.success)} but produces ${Type.encode(handlerEffect.success)}`,
      handlerNode?.span,
    )

  // The residual is the protected row minus the selected member, unioned with the handler's own
  // failures. Computing it here is the whole reason this operation is a compiler primitive: the
  // residual has no source-level spelling a Silk signature could declare.
  const residual = protectedFailures.filter((member) => !Type.equals(member, selected ?? 'never'))
  const failures = Type.union([...residual, ...(handlerEffect?.failures ?? [])])
  const resultFailures =
    failures._tag === 'Normalized'
      ? Type.isUnion(failures.type)
        ? failures.type.members
        : Type.isNominal(failures.type)
          ? Object.freeze([failures.type])
          : Object.freeze([])
      : Object.freeze([])
  const access =
    protectedEffect?.access === 'Take' || handlerEffect?.access === 'Take'
      ? ('Take' as const)
      : protectedEffect?.access === 'Exclusive' || handlerEffect?.access === 'Exclusive'
        ? ('Exclusive' as const)
        : ('Shared' as const)
  const requirements = [
    ...(protectedEffect?.requirements ?? []),
    ...(handlerEffect?.requirements ?? []),
  ]
  const resultType =
    valid && protectedEffect !== undefined && selected !== undefined
      ? availableExpressionType(
          Type.effect(
            protectedEffect.success,
            resultFailures,
            access,
            requirements,
            protectedEffect.failureParameters,
            [
              ...protectedEffect.requirementParameters,
              ...(handlerEffect?.requirementParameters ?? []),
            ],
          ),
        )
      : unavailableExpressionType

  // Analysis of this operation is complete, but no engine lowers the residual dispatch yet, so a
  // program that contains it cannot be built. Reporting that here — after the seam has typed the
  // expression, and without touching `valid` — keeps the whole analysis intact for tooling while
  // still telling whoever wrote the line, at their own call site, that it will not execute. The
  // alternative is what this operation did before: type cleanly, then die in the backend as an
  // `InvalidEffectOperation` MIR violation inside a stdlib function, with no user span at all.
  // The node span opens on the call's leading trivia, so the reported span starts at the `Effect`
  // token instead: an editor underlines the operation, not the whitespace before it.
  const head = callReferenceTokens(target).at(0)
  const operationSpan =
    head === undefined
      ? target.span
      : Option.getOrElse(
          SourceSpan.make(source, head.span.start, target.span.end),
          () => target.span,
        )
  diagnostics.push(Diagnostic.analysisOnlyConstruct('Member-selective Effect.catch', operationSpan))

  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectCatch',
      reference: intrinsicReference(source, target),
      protected: protectedResult?.fact ?? unavailableExpression(node),
      handler: handlerResult?.fact ?? unavailableExpression(node),
      ...(selected === undefined ? {} : { selected }),
      protectedRow: protectedFailures,
      handlerRow: handlerEffect?.failures ?? Object.freeze([]),
      residualRow: Object.freeze(residual),
      type: resultType,
      syntax: node,
    }),
    diagnostics: Object.freeze(diagnostics),
    type: resultType._tag === 'Available' ? resultType.type : undefined,
  })
}

const isEffectBindRequirementTarget = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
): boolean =>
  intrinsicOperationTarget(source, node)?.rule._tag === 'EffectRule' &&
  intrinsicOperationTarget(source, node)?.rule.operation === 'BindRequirement'

const analyzeEffectBindRequirement = (
  source: SourceFile.SourceFile,
  node: SyntaxTree.Node,
  declarations: ReadonlyArray<DeclarationFact>,
  declaration: DeclarationFact,
  scope: Scope,
  resolution: ResolutionContext,
): ExpressionResult => {
  const pipelined = node.kind === 'PipelineExpression'
  const target = pipelined ? (pipelineCallable(node) ?? node) : node
  let capability: Type.Nominal | Type.Parameter | undefined
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

  const providerSubject =
    providerNode?.kind === 'BorrowExpression' || providerNode?.kind === 'MoveExpression'
      ? providerNode.children.find(
          (element): element is SyntaxTree.Node =>
            SyntaxTree.isNode(element) && element.kind === 'IdentifierExpression',
        )
      : providerNode?.kind === 'IdentifierExpression'
        ? providerNode
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
  const providerValueType =
    providerResult?.type !== undefined && Type.isReference(providerResult.type)
      ? providerResult.type.target
      : providerResult?.type
  const providerAccess =
    providerNode?.kind === 'MoveExpression'
      ? ('Take' as const)
      : providerNode?.kind === 'BorrowExpression' &&
          SyntaxTree.directToken(providerNode, 'MutKeyword') !== undefined
        ? ('Exclusive' as const)
        : providerResult?.type !== undefined && Type.isReference(providerResult.type)
          ? providerResult.type.access
          : ('Shared' as const)
  if (
    (providerNode?.kind !== 'BorrowExpression' &&
      providerNode?.kind !== 'MoveExpression' &&
      !(
        providerNode?.kind === 'IdentifierExpression' &&
        providerResult?.type !== undefined &&
        Type.isReference(providerResult.type)
      )) ||
    providerReference === undefined
  )
    reject(
      'the provider must be a direct reference, &value, &mut value, or move value',
      providerNode?.span,
    )
  if (
    providerAccess === 'Exclusive' &&
    !(
      (providerReference?._tag === 'BindingFact' && providerReference.mutability === 'Mutable') ||
      (providerReference?._tag === 'ParameterDeclaration' &&
        providerResult?.type !== undefined &&
        Type.isReference(providerResult.type) &&
        providerResult.type.access === 'Exclusive')
    )
  )
    reject(
      'exclusive provision requires a mutable local binding or exclusive reference parameter',
      providerNode?.span,
    )
  const candidates =
    effect?.requirements.filter((requirement) => {
      if (explicitRole !== undefined && requirement.role !== explicitRole) return false
      if (providerValueType === undefined) return false
      if (requirement.access === 'Exclusive' && providerAccess === 'Shared') return false
      return (
        Type.equals(providerValueType, requirement.capability) ||
        Type.isParameter(providerValueType) ||
        Type.isParameter(requirement.capability) ||
        (Type.isNominal(providerValueType) &&
          Type.isNominal(requirement.capability) &&
          DeclarationIndex.witness(resolution.index, providerValueType, requirement.capability) !==
            undefined)
      )
    }) ?? []
  const selected =
    explicitRole === undefined
      ? candidates.length === 1
        ? candidates[0]
        : undefined
      : candidates.find((candidate) => candidate.role === explicitRole)
  if (selected !== undefined) capability = selected.capability
  const selectedWitness =
    capability === undefined ||
    providerValueType === undefined ||
    !Type.isNominal(capability) ||
    !Type.isNominal(providerValueType)
      ? undefined
      : DeclarationIndex.witness(resolution.index, providerValueType, capability)
  if (
    capability !== undefined &&
    providerValueType !== undefined &&
    Type.isNominal(capability) &&
    Type.isNominal(providerValueType) &&
    !Type.equals(providerValueType, capability) &&
    selectedWitness === undefined
  )
    reject(
      `provider type ${Type.encode(providerValueType)} does not match ${Type.encode(capability)}`,
      providerNode?.span,
    )
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
      ? availableExpressionType(
          Type.effect(
            effect.success,
            effect.failures,
            access,
            requirements,
            effect.failureParameters,
            effect.requirementParameters,
          ),
        )
      : unavailableExpressionType
  return Object.freeze({
    fact: Object.freeze({
      _tag: 'EffectBindRequirement',
      reference: intrinsicReference(source, target),
      protected: protectedResult?.fact ?? unavailableExpression(node),
      ...(providerReference === undefined ||
      capability === undefined ||
      selected === undefined ||
      providerValueType === undefined ||
      !(Type.isNominal(providerValueType) || Type.isParameter(providerValueType))
        ? {}
        : {
            provider: Object.freeze({
              _tag: 'EffectRequirementBinding' as const,
              reference: providerReference,
              capability,
              providerType: providerValueType,
              ...(selectedWitness === undefined ? {} : { witness: selectedWitness }),
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
          fact.provider?.access ?? 'Shared',
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
        case 'ExpressionStatement':
          expression(statement.expression)
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
          site: executableSite('EffectSiteId', resolution, node),
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
        site: executableSite('EffectSiteId', resolution, node),
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
    if (operationTarget !== undefined && isEffectCatchTarget(source, operationTarget, resolution))
      return analyzeEffectCatch(source, node, declarations, declaration, scope, resolution)
    if (operationTarget !== undefined && isEffectBindRequirementTarget(source, operationTarget))
      return analyzeEffectBindRequirement(
        source,
        node,
        declarations,
        declaration,
        scope,
        resolution,
      )
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
          resolution,
        )
      return analyzeBuiltinCall(source, node, argumentsResult, callTypeArguments, resolution)
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
          resolution,
        )
      }
      if (bound !== undefined)
        return finishBoundOperationCall(node, bound.reference, argumentsResult, callTypeArguments)
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
    reference.declaration.parameters.length >= 2 &&
    argumentsResult.facts.length === reference.declaration.parameters.length - 1
  ) {
    return finishCallableSection(node, reference, argumentsResult, callTypeArguments, resolution)
  }
  const callContract = analyzeCallContract(
    node,
    reference,
    argumentsResult.facts,
    hasAvailableCallSyntax(node),
    callTypeArguments,
  )
  const constraintDiagnostics = interfaceConstraintDiagnostics(
    reference,
    callContract,
    resolution.index,
    node.span,
  )
  const syntaxAvailable = hasAvailableCallSyntax(node)
  const expressionType =
    syntaxAvailable &&
    reference._tag === 'Resolved' &&
    reference.declaration.returnType._tag === 'Resolved' &&
    constraintDiagnostics.length === 0
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, Type.GenericArgument>()
            const success = Type.substitute(reference.declaration.returnType.type, substitution)
            if (reference.declaration.functionKind !== 'Effect')
              return Type.isEffect(success)
                ? Type.effect(
                    success.success,
                    success.failures,
                    effectCaptureAccess(argumentsResult.facts),
                    success.requirements,
                    success.failureParameters,
                    success.requirementParameters,
                  )
                : success
            return Type.substitute(
              Type.effect(
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
                reference.declaration.failureRow.parameters,
                reference.declaration.requirementRow.parameters,
              ),
              substitution,
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
  resolution: ResolutionContext,
): ExpressionResult => {
  if (
    reference._tag === 'Resolved' &&
    reference.declaration.parameters.length >= 2 &&
    argumentsResult.facts.length === reference.declaration.parameters.length - 1
  ) {
    const section = finishCallableSection(
      node,
      reference,
      argumentsResult,
      callTypeArguments,
      resolution,
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
  )
  const constraintDiagnostics = interfaceConstraintDiagnostics(
    reference,
    callContract,
    resolution.index,
    node.span,
  )
  const callable = sourceCallable(reference)
  const expressionType =
    hasAvailableCallSyntax(node) &&
    callable !== undefined &&
    callable.returnType._tag === 'Resolved' &&
    constraintDiagnostics.length === 0
      ? availableExpressionType(
          (() => {
            const substitution =
              callContract.fact._tag === 'Compatible'
                ? callContract.fact.substitution
                : new Map<string, Type.GenericArgument>()
            const success = Type.substitute(callable.returnType.type, substitution)
            if (callable.functionKind !== 'Effect')
              return Type.isEffect(success)
                ? Type.effect(
                    success.success,
                    success.failures,
                    effectCaptureAccess(argumentsResult.facts),
                    success.requirements,
                    success.failureParameters,
                    success.requirementParameters,
                  )
                : success
            return Type.substitute(
              Type.effect(
                success,
                callable.failureRow.failures.flatMap((failure) => {
                  const specialized = Type.substitute(failure, substitution)
                  return Type.isNominal(specialized) ? [specialized] : []
                }),
                effectCaptureAccess(argumentsResult.facts),
                callable.requirementRow.requirements.flatMap((requirement) => {
                  const capability = Type.substitute(requirement.capability, substitution)
                  return Type.isNominal(capability)
                    ? [Object.freeze({ ...requirement, capability })]
                    : []
                }),
                callable.failureRow.parameters,
                callable.requirementRow.parameters,
              ),
              substitution,
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
  const expressionType =
    hasAvailableCallSyntax(node) &&
    typeArgumentDiagnostic === undefined &&
    callContract.fact._tag === 'Compatible'
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
      type: expressionType,
      syntax: node,
    }),
    diagnostics: Object.freeze([
      ...(typeArgumentDiagnostic === undefined ? [] : [typeArgumentDiagnostic]),
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
  readonly nextBindingOrdinal: { value: number }
  readonly regionBase?: number
  readonly effectBlock?: true
}

interface ResolutionContext {
  readonly scope: NameResolution.ModuleScope
  readonly index: DeclarationIndex.Index
  readonly unsafeSpans?: ReadonlyArray<SourceSpan.SourceSpan>
  readonly nextBindingOrdinal?: { value: number }
  readonly executableFunction?: DeclarationId
  readonly executableOwner?: DeclarationIndex.CanonicalId
  readonly executableSites?: ReadonlyMap<SyntaxTree.Node, number>
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
    const taken =
      arms.at(0) === undefined
        ? []
        : analyzeStatements(context, arms[0] as SyntaxTree.Node, armScope, armLoopStack)
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
        context.declaration.returnType._tag === 'Resolved' &&
        expression.type !== undefined &&
        !declaredReturnTypesCompatible(context.declaration, expression.type)
      ) {
        const diagnostic =
          representationJoinDiagnostic(
            context.declaration.returnType.type,
            expression.type,
            context.declaration.returnType.syntax.span,
            expressionNode.span,
            expressionNode.span,
          ) ??
          unionConversionDiagnostic(
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
        expression.type !== undefined &&
        (Type.isNominal(expression.type) || Type.isFailureProjection(expression.type))
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
        !(Type.isNominal(failure)
          ? context.declaration.failureRow.failures.some((candidate) =>
              Type.equals(candidate, failure),
            )
          : context.declaration.failureRow.parameters.some((parameter) =>
              Type.equals(parameter, failure.parameter),
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
        } else if (statement._tag === 'IfStatement') {
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
      }
    }
    return undefined
  }
  const terminal = terminalOf(statements)
  if (terminal === undefined)
    throw new RangeError('Semantic analysis expected a terminal statement')
  const expression = terminal.expression
  const expressionType = expression.type._tag === 'Available' ? expression.type.type : undefined

  const returnCompatibility =
    terminal._tag === 'ReturnStatement' &&
    declaration.returnType._tag === 'Resolved' &&
    expressionType !== undefined &&
    declaredReturnTypesCompatible(declaration, expressionType)
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
      fact.selected === undefined ||
      fact.type._tag !== 'Available' ||
      !Type.isEffect(fact.type.type)
    )
      return Object.freeze({ _tag: 'Unavailable', span: fact.syntax.span })
    return Object.freeze({
      _tag: 'EffectCatch',
      protected: protected_,
      handler,
      selected: fact.selected,
      protectedRow: fact.protectedRow,
      handlerRow: fact.handlerRow,
      residualRow: fact.residualRow,
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
        capability: fact.provider.capability,
        providerType: fact.provider.providerType,
        ...(fact.provider.witness === undefined ? {} : { witness: fact.provider.witness }),
        role: fact.provider.role,
        access: fact.provider.access,
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
        : fact.formation.root._tag === 'ParameterRoot'
          ? Object.freeze({
              _tag: 'ParameterSliceRoot',
              parameter: fact.formation.root.parameter.id,
            })
          : Object.freeze({
              _tag: 'PatternSliceRoot',
              binding: fact.formation.root.binding.id,
            })
    if (fact.formation._tag === 'ValueBorrow' && Type.isReference(fact.type.type)) {
      return Object.freeze({
        _tag: 'ValueBorrow',
        borrow,
        root,
        path: fact.formation.root.path,
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
    const requirement = fact.type.type.requirements.find((candidate) =>
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
  const source = hirExpression(fact, borrow)
  if (source._tag === 'Unavailable') return source
  if (Type.isRepresented(target) && Type.haveSameRepresentationShape(source.type, target))
    return source
  const compatibility = TypeCompatibility.check(source.type, target)
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
    case 'ExpressionStatement':
      return Object.freeze([statement.expression])
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
            taken: hirStatements(statement.taken, resultType),
            otherwise: hirStatements(statement.otherwise, resultType),
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
            const type = Type.effect(
              fact.declaration.returnType.type,
              fact.declaration.failureRow.failures,
              access,
              fact.declaration.requirementRow.requirements,
              fact.declaration.failureRow.parameters,
              fact.declaration.requirementRow.parameters,
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
