import * as Constraint from './Constraint.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as Diagnostic from './Diagnostic.js'
import * as Intrinsic from './Intrinsic.js'
import * as Match from './Match.js'
import type * as Operator from './Operator.js'
import * as RowAlgebra from './RowAlgebra.js'
import type * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StaticEvaluation from './StaticEvaluation.js'
import type * as StaticText from './StaticText.js'
import * as StaticValue from './StaticValue.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/**
 * HIR: the resolved, typed semantic representation of elaborated bodies. Core operations carry
 * their resolved type and exact source provenance; unknown facts stay explicit unavailable
 * states and never masquerade as typed operations.
 */

/** A normalized function contract: ordered parameter types and the result type. */
export interface Contract {
  readonly _tag: 'Contract'
  readonly functionKind?: 'Ordinary' | 'Effect'
  readonly unsafe: boolean
  readonly parameters: ReadonlyArray<DeclarationFacts.SemanticType>
  readonly result: DeclarationFacts.SemanticType
  readonly failureRow?: Type.FailureRow
  readonly requirementRow?: Type.RequirementsRow
  readonly constraints: ReadonlyArray<Constraint.Constraint>
}

/** Resolves one symbolic binding selector at a complete application frontier. */
export const selectedRequirement = (
  provider: Extract<Expression, { readonly _tag: 'EffectBindRequirement' }>['provider'],
  substitution: Type.Substitution,
): Type.Requirement | undefined => {
  const selected = Type.substituteRequirementsRow(provider.selected, substitution)
  const concrete = RowAlgebra.concretize(Type.requirementRowPolicy(), selected)
  return concrete._tag === 'Concrete' && concrete.row.members.length === 1
    ? concrete.row.members.at(0)
    : undefined
}

/** The normalized or explicitly unavailable contract of one declaration. */
export type ContractFact =
  | Contract
  | { readonly _tag: 'Unavailable'; readonly cause?: Diagnostic.Identity }

/** A deterministic binding identity local to its declaring function's statement order. */
export interface BindingId {
  readonly _tag: 'HirBinding'
  readonly function: DeclarationFacts.DeclarationId
  readonly ordinal: number
}

/** A canonical source-ordered region identity local to one function. */
export interface RegionId {
  readonly _tag: 'HirRegion'
  readonly function: DeclarationFacts.DeclarationId
  readonly ordinal: number
}

/** Compiler-only identity for one explicit call argument borrow. */
export interface BorrowId {
  readonly _tag: 'BorrowId'
  readonly function: DeclarationFacts.DeclarationId
  readonly callSpan: SourceSpan.SourceSpan
  readonly ordinal: number
}

/** Stable compiler-owned identity for one materialized borrowable temporary. */
export interface TemporaryOwnerId {
  readonly _tag: 'TemporaryOwnerId'
  readonly function: DeclarationFacts.DeclarationId
  readonly span: SourceSpan.SourceSpan
  readonly ordinal: number
}

const borrowText = (borrow: BorrowId): string =>
  `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`

interface ExecutableSiteId {
  readonly function: DeclarationFacts.DeclarationId
  readonly owner?: DeclarationFacts.CanonicalId
  readonly ordinal: number
  readonly span: SourceSpan.SourceSpan
}

/** Hidden nominal identity for one source `effect {}` construction site. */
export interface EffectSiteId extends ExecutableSiteId {
  readonly _tag: 'EffectSiteId'
}

/** Hidden nominal identity for one automatic callable-section construction site. */
export interface CallableSiteId extends ExecutableSiteId {
  readonly _tag: 'CallableSiteId'
}

/** Returns the path- and span-independent structural identity of one executable site. */
export const executableSiteKey = (self: EffectSiteId | CallableSiteId): string =>
  `${self._tag === 'EffectSiteId' ? 'effect' : 'callable'}\u0000${
    self.owner === undefined
      ? `recovered:${self.function.ordinal}`
      : `declaration:${self.owner.module}:${self.owner.name}`
  }\u0000site:${self.ordinal}`

/** Tests structural executable-site identity without consulting diagnostic provenance. */
export const sameExecutableSite = (
  left: EffectSiteId | CallableSiteId,
  right: EffectSiteId | CallableSiteId,
): boolean => executableSiteKey(left) === executableSiteKey(right)

/** Stable hidden Effect site for a compiler-backed selective-catch expression. */
export const effectCatchSite = (
  function_: DeclarationFacts.DeclarationId,
  owner: DeclarationFacts.CanonicalId,
  span: SourceSpan.SourceSpan,
): EffectSiteId =>
  Object.freeze({
    _tag: 'EffectSiteId',
    function: function_,
    owner,
    // Authored/recovered effect-block ordinals occupy the small signed range. Catch sites derive
    // from byte offsets in a disjoint positive range so their identity is stable across targets.
    ordinal: 0x40000000 + span.start,
    span,
  })

/** Stable hidden Effect site for one compiler-backed Effect-valued builtin call. */
export const builtinEffectSite = (
  function_: DeclarationFacts.DeclarationId,
  owner: DeclarationFacts.CanonicalId,
  span: SourceSpan.SourceSpan,
): EffectSiteId =>
  Object.freeze({
    _tag: 'EffectSiteId',
    function: function_,
    owner,
    // Effect blocks and selective catches already occupy the authored and 0x40000000 ranges.
    // Builtin recipes use a third disjoint range while retaining target-independent source order.
    ordinal: 0x60000000 + span.start,
    span,
  })

/** Orders executable sites by their stable structural identities. */
export const compareExecutableSites = (
  left: EffectSiteId | CallableSiteId,
  right: EffectSiteId | CallableSiteId,
): number => executableSiteKey(left).localeCompare(executableSiteKey(right))

/** Presents one structural executable-site identity without embedding control delimiters. */
export const executableSiteLabel = (self: EffectSiteId | CallableSiteId): string =>
  executableSiteKey(self).replaceAll('\u0000', ':')

/** Projects one Effect site into the semantic origin retained by exact representations. */
export const effectRepresentationIdentity = (self: EffectSiteId): string =>
  `effect:${executableSiteKey(self)}`

/** Projects a HIR callable site into the semantic identity retained across specialization. */
export const callableEnvironmentSite = (self: CallableSiteId): Type.CallableEnvironmentSite =>
  Type.callableEnvironmentSite(
    self.owner === undefined
      ? undefined
      : Object.freeze({ module: self.owner.module, name: self.owner.name }),
    self.function.ordinal,
    self.ordinal,
  )

/** Retains one callable site's complete enclosing executable specialization. */
export const callableEnvironmentIdentity = (
  self: CallableSiteId,
  owner: Type.CallableEnvironmentIdentity['owner'],
): Type.CallableEnvironmentIdentity =>
  Type.callableEnvironmentIdentity(callableEnvironmentSite(self), owner)

/** Derives the canonical hidden runner declaration owned by one effect construction site. */
export const effectRunnerId = (
  owner: DeclarationFacts.CanonicalId,
  site: EffectSiteId,
): DeclarationFacts.CanonicalId =>
  Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: owner.module,
    name: `${owner.name}$effect$${site.ordinal}`,
  })

/** Derives the private executable declaration owned by one anonymous callable site. */
export const anonymousCallableId = (
  owner: DeclarationFacts.CanonicalId,
  site: CallableSiteId,
): DeclarationFacts.CanonicalId =>
  Object.freeze({
    _tag: 'CanonicalDeclarationId',
    module: owner.module,
    name: `${owner.name}$callable$${site.ordinal}`,
  })

/** Tests whether a canonical declaration is owned by an anonymous callable expression site. */
export const isAnonymousCallableId = (self: DeclarationFacts.CanonicalId): boolean =>
  /\$callable\$\d+$/.test(self.name)

export type CallableTarget =
  | {
      readonly _tag: 'DeclarationCallableTarget'
      readonly declaration: DeclarationFacts.CanonicalId
    }
  | {
      readonly _tag: 'BuiltinCallableTarget'
      readonly actor: string
      readonly operation: BuiltinOperation
      readonly intrinsic: Intrinsic.OperationId
    }

/** Converts the semantic callable identity retained by specialization into its HIR target. */
export const callableTargetFromIdentity = (
  target: Type.CallableIdentityArgument['target'],
): CallableTarget =>
  target._tag === 'Declaration'
    ? Object.freeze({
        _tag: 'DeclarationCallableTarget',
        declaration: Object.freeze({
          _tag: 'CanonicalDeclarationId',
          module: target.module,
          name: target.name,
        }),
      })
    : Object.freeze({
        _tag: 'BuiltinCallableTarget',
        actor: target.actor,
        operation: target.operation,
        intrinsic: Object.freeze({
          _tag: 'IntrinsicOperationId',
          actor: target.intrinsic.actor,
          name: target.intrinsic.name,
        }),
      })

/** Projects a HIR callable target into the semantic identity retained by specialization. */
export const callableTargetIdentity = (
  self: CallableTarget,
): Type.CallableIdentityArgument['target'] =>
  self._tag === 'DeclarationCallableTarget'
    ? Object.freeze({
        _tag: 'Declaration',
        module: self.declaration.module,
        name: self.declaration.name,
      })
    : Object.freeze({
        _tag: 'Builtin',
        actor: self.actor,
        operation: self.operation,
        intrinsic: Object.freeze({
          actor: self.intrinsic.actor,
          name: self.intrinsic.name,
        }),
      })

/** Tests complete structural identity for two HIR callable targets. */
export const sameCallableTarget = (left: CallableTarget, right: CallableTarget): boolean => {
  if (left._tag !== right._tag) return false
  if (left._tag === 'DeclarationCallableTarget' && right._tag === 'DeclarationCallableTarget') {
    return (
      left.declaration.module === right.declaration.module &&
      left.declaration.name === right.declaration.name
    )
  }
  if (left._tag === 'BuiltinCallableTarget' && right._tag === 'BuiltinCallableTarget') {
    return (
      left.actor === right.actor &&
      left.operation === right.operation &&
      left.intrinsic.actor === right.intrinsic.actor &&
      left.intrinsic.name === right.intrinsic.name
    )
  }
  return false
}

/** Tests whether one HIR target is the target retained by a semantic callable identity. */
export const matchesCallableTargetIdentity = (
  self: CallableTarget,
  identity: Type.CallableIdentityArgument['target'],
): boolean => sameCallableTarget(self, callableTargetFromIdentity(identity))

/** A canonical lexical loop identity local to one function. */
export interface LoopId {
  readonly _tag: 'HirLoop'
  readonly function: DeclarationFacts.DeclarationId
  readonly ordinal: number
}

/** The closed built-in operation vocabulary of the compiler-known actors. */
export type BuiltinOperation = Type.BuiltinOperation

export type BoundsMode =
  | { readonly _tag: 'Proven'; readonly index: number; readonly length: number }
  | { readonly _tag: 'Runtime'; readonly length: number }

export type SliceRoot =
  | { readonly _tag: 'BindingSliceRoot'; readonly binding: BindingId }
  | {
      readonly _tag: 'ParameterSliceRoot'
      readonly parameter: DeclarationFacts.ParameterId
    }
  | { readonly _tag: 'PatternSliceRoot'; readonly binding: Match.BindingId }
  | {
      readonly _tag: 'TemporarySliceRoot'
      readonly owner: TemporaryOwnerId
      readonly value: Expression
    }

export type BorrowSelector =
  | {
      readonly _tag: 'Field'
      readonly field: DeclarationFacts.FieldId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Index'
      readonly index: Expression
      readonly array: Type.FixedArray
      readonly bounds: BoundsMode
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'SliceIndex'
      readonly index: Expression
      readonly slice: Type.Slice
      readonly span: SourceSpan.SourceSpan
    }

/** One selector in a writable place, retained in source evaluation order. */
export type WriteSelector =
  | {
      readonly _tag: 'Field'
      readonly field: DeclarationFacts.FieldId
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Index'
      readonly index: Expression
      readonly array: Type.FixedArray
      readonly bounds: BoundsMode
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }

export type OwnedWriteRoot =
  | { readonly _tag: 'BindingWriteRoot'; readonly binding: BindingId }
  | {
      readonly _tag: 'ParameterWriteRoot'
      readonly parameter: DeclarationFacts.ParameterId
    }

/** One complete typed replacement rooted in mutable owned storage. */
export interface OwnedWritePlace {
  readonly _tag: 'WritePlace'
  readonly root: OwnedWriteRoot
  readonly selectors: ReadonlyArray<WriteSelector>
  readonly type: DeclarationFacts.SemanticType
  readonly span: SourceSpan.SourceSpan
}

export type BorrowedWriteSelector =
  | {
      readonly _tag: 'SliceIndex'
      readonly index: Expression
      readonly slice: Type.Slice
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | Extract<WriteSelector, { readonly _tag: 'Field' | 'Index' }>

export interface BorrowedWritePlace {
  readonly _tag: 'BorrowedWritePlace'
  readonly root: Extract<SliceRoot, { readonly _tag: 'BindingSliceRoot' | 'ParameterSliceRoot' }>
  /** The borrowed root: an exclusive slice, or an exclusive reference written through. */
  readonly slice: Type.Slice | Type.Reference
  readonly selectors: ReadonlyArray<BorrowedWriteSelector>
  readonly type: DeclarationFacts.SemanticType
  readonly span: SourceSpan.SourceSpan
}

export type WritePlace = OwnedWritePlace | BorrowedWritePlace

export interface PatternBinding {
  readonly id: Match.BindingId
  readonly name?: string
  /** Absent for a whole-member binding, which observes or owns the entire selected payload. */
  readonly field?: DeclarationFacts.FieldId
  readonly path: ReadonlyArray<DeclarationFacts.FieldId>
  readonly type: DeclarationFacts.SemanticType
  readonly access: Match.Access
  readonly span: SourceSpan.SourceSpan
}

export interface PatternSelection {
  readonly id: Match.MatchId
  readonly arm: Match.ArmId
  readonly access: Match.Access
  readonly subject: Expression
  readonly members: ReadonlyArray<Match.CoverageIdentity>
  readonly member?: Match.CoverageIdentity
  readonly universal: boolean
  readonly bindings: ReadonlyArray<PatternBinding>
  readonly cleanup: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
  readonly irrefutable: boolean
  readonly span: SourceSpan.SourceSpan
}

/** One typed core semantic operation with exact source provenance. */
export type Expression =
  | {
      readonly _tag: 'IntegerLiteral'
      readonly value: bigint
      readonly type: DeclarationFacts.SemanticType
      readonly constant?: DeclarationFacts.CanonicalId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'FloatingLiteral'
      readonly bits: bigint
      readonly spelling: string
      readonly type: Scalar.FloatSpelling
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'StaticStringLiteral'
      readonly data: StaticText.Data
      readonly type: Type.String
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'StaticByteViewLiteral'
      readonly data: StaticText.Data
      readonly type: Type.Slice
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'UnitLiteral'
      readonly type: typeof Type.unit
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'BooleanLiteral'
      readonly value: boolean
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'CharacterLiteral'
      readonly value: number
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EnumMember'
      readonly enum: DeclarationFacts.CanonicalId
      readonly member: DeclarationFacts.CanonicalEnumMemberId
      readonly discriminant: bigint
      readonly type: Type.Nominal
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EnumValue'
      readonly enum: DeclarationFacts.CanonicalId
      readonly value: Expression
      readonly intrinsic: Intrinsic.OperationId
      readonly type: Scalar.EnumRepresentationSpelling
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EnumEquality'
      readonly enum: DeclarationFacts.CanonicalId
      readonly left: Expression
      readonly right: Expression
      readonly negated: boolean
      readonly type: 'bool'
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'ParameterReference'
      readonly parameter: DeclarationFacts.ParameterId
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'BindingReference'
      readonly binding: BindingId
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'PatternBindingReference'
      readonly binding: Match.BindingId
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Move'
      readonly subject: Expression
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      /** Atomically reads one writable place and stores a replacement, yielding the old value. */
      readonly _tag: 'Replace'
      readonly place: WritePlace
      readonly value: Expression
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'UnionConvert'
      readonly source: Expression
      readonly sourceType: Type.Type
      readonly target: Type.StructuralUnion | Type.Effect | Type.Represented
      readonly conversion: 'Inject' | 'Widen' | 'EffectAccess' | 'EffectJoin'
      readonly mappings: ReadonlyArray<TypeCompatibility.MemberMapping>
      readonly access: 'Copy' | 'Owned'
      readonly context:
        | 'Return'
        | 'Argument'
        | 'StructField'
        | 'ArrayElement'
        | 'Assignment'
        | 'MatchArm'
      readonly expectedAt: SourceSpan.SourceSpan
      readonly type: Type.StructuralUnion | Type.Effect | Type.Represented
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Match'
      readonly id: Match.MatchId
      readonly access: Match.Access
      readonly scrutinee: Expression
      readonly members: ReadonlyArray<Match.CoverageIdentity>
      readonly arms: ReadonlyArray<{
        readonly id: Match.ArmId
        readonly member?: Match.CoverageIdentity
        readonly universal: boolean
        readonly bindings: ReadonlyArray<PatternBinding>
        readonly cleanup: ReadonlyArray<ReadonlyArray<DeclarationFacts.FieldId>>
        readonly guard?: Expression
        readonly result: Expression
        readonly before: ReadonlyArray<Match.CoverageIdentity>
        readonly after: ReadonlyArray<Match.CoverageIdentity>
        readonly reachable: boolean
        readonly span: SourceSpan.SourceSpan
      }>
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      /**
       * `&&` and `||`. The left operand always evaluates; the right operand evaluates only when
       * the left one does not already decide the result. The right operand is a pure expression
       * — elaboration rejects an effect site or a move inside it — so skipping it releases
       * nothing and performs nothing.
       */
      readonly _tag: 'ShortCircuit'
      readonly operator: Operator.ShortCircuit
      readonly left: Expression
      readonly right: Expression
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Construct'
      readonly nominal: Type.Nominal
      /** Field identities in language evaluation order; `fields` remains canonical storage order. */
      readonly evaluationOrder: ReadonlyArray<DeclarationFacts.FieldId>
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationFacts.FieldId
        readonly value: Expression
      }>
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'ConstructUnionVariant'
      readonly nominal: Type.Nominal
      readonly variant: DeclarationFacts.CanonicalUnionVariantId
      readonly variantOrdinal: number
      /** Field identities in language evaluation order; `fields` remains variant storage order. */
      readonly evaluationOrder: ReadonlyArray<DeclarationFacts.FieldId>
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationFacts.FieldId
        readonly value: Expression
      }>
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'ArrayConstruct'
      readonly elements: ReadonlyArray<Expression>
      readonly type: Type.FixedArray
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Project'
      readonly subject: Expression
      readonly nominal: Type.Nominal
      readonly field: DeclarationFacts.FieldId
      readonly access: 'CopyRead' | 'ConsumeRequested'
      readonly borrowAccess?: Type.BorrowAccess
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      /** One explicit place rooted at the target of a reference value. */
      readonly _tag: 'ReferentPlace'
      readonly subject: Expression
      readonly reference: Type.Reference
      readonly access: 'CopyRead'
      readonly borrowAccess: Type.BorrowAccess
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'IndexPlace'
      readonly subject: Expression
      readonly index: Expression
      readonly array: Type.FixedArray
      readonly access: 'CopyRead' | 'ConsumeRequested'
      readonly bounds: BoundsMode
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'SliceBorrow'
      readonly borrow: BorrowId
      readonly root: SliceRoot
      readonly selectors: ReadonlyArray<BorrowSelector>
      readonly source: Type.FixedArray | Type.Slice
      readonly access: Type.BorrowAccess
      readonly reborrow: boolean
      readonly suspendsParent: boolean
      readonly type: Type.Slice
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'ValueBorrow'
      readonly borrow: BorrowId
      readonly root: SliceRoot
      readonly selectors: ReadonlyArray<BorrowSelector>
      readonly source: Type.Type
      readonly access: Type.BorrowAccess
      readonly reborrow: boolean
      readonly suspendsParent: boolean
      readonly type: Type.Reference
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'RuntimeStringView'
      readonly source: Expression
      readonly heldLoans: ReadonlyArray<BorrowId>
      readonly type: Type.String
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'StringEquality'
      readonly left: Expression
      readonly right: Expression
      readonly negated: boolean
      readonly intrinsic: Intrinsic.OperationId
      readonly type: 'bool'
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'SliceLength'
      readonly slice: Expression
      readonly type: 'usize'
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'SliceIndexPlace'
      readonly slice: Expression
      readonly index: Expression
      readonly access: Type.Slice['access']
      readonly sourceType: Type.Slice
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Call'
      readonly target: DeclarationFacts.CanonicalId
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
      readonly staticArguments: ReadonlyArray<StaticValue.Value>
      /** Caller-authored origins aligned with static arguments, excluded from instance identity. */
      readonly staticArgumentOrigins?: ReadonlyArray<StaticEvaluation.TextOrigin | undefined>
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      /** Direct argument loans deliberately retained by a returned lexical view. */
      readonly heldLoans: ReadonlyArray<BorrowId>
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'FunctionItem'
      readonly target: CallableTarget
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly type: Type.Callable
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'CallableSection'
      readonly site: CallableSiteId
      readonly target: CallableTarget
      readonly remainingParameters: ReadonlyArray<number>
      readonly captures: ReadonlyArray<{
        readonly ordinal: number
        readonly parameterOrdinal: number
        readonly value: Expression
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }>
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly substitution: Type.Substitution
      readonly retainedDependencies: ReadonlyArray<number>
      readonly mode: Type.CallableMode
      readonly type: Type.Callable
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'CallableApply'
      readonly callee: Expression
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      readonly heldLoans: ReadonlyArray<BorrowId>
      readonly access: Type.CallableMode
      readonly substitution: Type.Substitution
      readonly evaluation: 'CalleeThenArguments' | 'LeftThenCallable'
      readonly realization: 'Environment' | 'DirectErasedSection'
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectConstruct'
      readonly target: DeclarationFacts.CanonicalId
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
      readonly staticArguments: ReadonlyArray<StaticValue.Value>
      /** Caller-authored origins aligned with static arguments, excluded from instance identity. */
      readonly staticArgumentOrigins?: ReadonlyArray<StaticEvaluation.TextOrigin | undefined>
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'ServiceEffectConstruct'
      readonly service: Type.Nominal
      readonly operation: string
      readonly role: string
      readonly access: 'Shared' | 'Exclusive'
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectBlock'
      readonly site: EffectSiteId
      readonly statements: ReadonlyArray<Statement>
      readonly captures: ReadonlyArray<{
        readonly binding?: BindingId
        readonly parameter?: DeclarationFacts.ParameterId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
        readonly span: SourceSpan.SourceSpan
      }>
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Run'
      readonly subject: Expression
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      /**
       * Member-selective recovery, carrying the four rows the semantic-fact surface records.
       * `residualRow` is the protected row minus `selected`; it is stored rather than derived
       * because no source-level type can spell it.
       */
      readonly _tag: 'EffectCatch'
      readonly intrinsic: Intrinsic.OperationId
      readonly protected: Expression
      readonly handler: Expression
      readonly selected: Type.Type
      readonly protectedRow: Type.FailureRow
      readonly handlerRow: Type.FailureRow
      readonly residualRow: Type.FailureRow
      readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EffectBindRequirement'
      readonly protected: Expression
      readonly provider: {
        readonly binding?: BindingId
        readonly parameter?: DeclarationFacts.ParameterId
        readonly selected: Type.RequirementsRow
        readonly evidence: ReadonlyArray<Constraint.ConstraintEvidence>
        readonly capability?: Type.Nominal | Type.Parameter
        readonly providerType: Type.Nominal | Type.Parameter
        readonly witness?: DeclarationFacts.ConformanceWitness
        readonly role?: string
        readonly selectionAccess: 'Shared' | 'Exclusive' | 'Take'
        readonly captureAccess: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
        readonly span: SourceSpan.SourceSpan
      }
      readonly type: Type.Effect
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'BuiltinCall'
      readonly operation: BuiltinOperation
      readonly intrinsic: Intrinsic.OperationId
      /**
       * The bound interface operation an operator inside a generic body spells. Specialization
       * redirects the call to the provider's own function when the conformance maps one, and
       * otherwise keeps the compiler-known operation this node already names.
       */
      readonly interfaceOperation?: {
        readonly capability: Type.Nominal
        readonly provider: Type.Type
        readonly operation: string
        readonly contract: DeclarationFacts.InterfaceOperationApplicationFact
      }
      readonly witnessEffectSite?: EffectSiteId
      readonly typeArguments: ReadonlyArray<Type.GenericArgument>
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      readonly heldLoans: ReadonlyArray<BorrowId>
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  /**
   * One statically selected interface-operation call. It records the conformance question rather
   * than an answer: which operation of which normalized interface application, over which provider.
   * The concrete instruction is the one the witness selected for the specialized provider, and only
   * lowering — where the substitution exists — knows it.
   */
  | {
      readonly _tag: 'InterfaceOperationCall'
      readonly capability: Type.Nominal
      readonly provider: Type.Type
      readonly operation: string
      readonly contract: DeclarationFacts.InterfaceOperationApplicationFact
      readonly witnessEffectSite?: EffectSiteId
      readonly arguments: ReadonlyArray<Expression>
      readonly loanEnds: ReadonlyArray<BorrowId>
      readonly type: DeclarationFacts.SemanticType
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Unavailable'
      readonly span: SourceSpan.SourceSpan
      readonly cause?: Diagnostic.Identity
    }

/** One elaborated body statement in source order. */
export type Statement =
  | {
      readonly _tag: 'UnavailableStatement'
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Unsafe'
      readonly statements: ReadonlyArray<Statement>
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Bind'
      readonly binding: BindingId
      readonly name: string | undefined
      readonly mutability: 'Immutable' | 'Mutable'
      readonly initializer: Expression
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'PatternBind'
      readonly selection: PatternSelection
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Evaluate'
      readonly expression: Expression
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'If'
      readonly condition: Expression
      readonly taken: ReadonlyArray<Statement>
      readonly otherwise: ReadonlyArray<Statement>
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'IfLet'
      readonly selection: PatternSelection
      readonly taken: ReadonlyArray<Statement>
      readonly otherwise: ReadonlyArray<Statement>
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Write'
      readonly place: WritePlace
      readonly value: Expression
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'While'
      readonly loop: LoopId
      readonly parent?: LoopId
      readonly condition: Expression
      readonly body: ReadonlyArray<Statement>
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Break'
      readonly target: LoopId
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Continue'
      readonly target: LoopId
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Return'
      readonly expression: Expression
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Fail'
      readonly expression: Expression
      readonly failure: Type.Type
      readonly transfer: 'Copy' | 'Move'
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'Drop'
      readonly expression: Expression
      readonly region: RegionId
      readonly span: SourceSpan.SourceSpan
    }

/** One elaborated function: its header, normalized contract, and desugared body statements. */
export interface HirFunction {
  readonly _tag: 'HirFunction'
  readonly declaration: DeclarationFacts.DeclarationFact
  readonly contract: ContractFact
  readonly entryRegion: RegionId
  readonly regionOrder: ReadonlyArray<RegionId>
  readonly statements: ReadonlyArray<Statement>
}

/** The return statement's expression — every body ends in exactly one. */
export const returned = (self: HirFunction): Expression => {
  let statements = self.statements
  let last = statements.at(-1)
  while (last?._tag === 'Unsafe') {
    statements = last.statements
    last = statements.at(-1)
  }
  if (last === undefined || last._tag !== 'Return') {
    throw new RangeError('HIR body must end in a return statement')
  }
  return last.expression
}

/** Every expression directly carried by one statement, nesting through conditionals. */
export const statementExpressions = (statement: Statement): ReadonlyArray<Expression> => {
  switch (statement._tag) {
    case 'UnavailableStatement':
      return []
    case 'Unsafe':
      return statement.statements.flatMap(statementExpressions)
    case 'Bind':
      return [statement.initializer]
    case 'PatternBind':
      return [statement.selection.subject]
    case 'Evaluate':
      return [statement.expression]
    case 'Write':
      return [
        ...statement.place.selectors.flatMap((selector) =>
          selector._tag === 'Index' || selector._tag === 'SliceIndex' ? [selector.index] : [],
        ),
        statement.value,
      ]
    case 'Return':
      return [statement.expression]
    case 'Fail':
    case 'Drop':
      return [statement.expression]
    case 'Break':
    case 'Continue':
      return []
    case 'While':
      return [statement.condition, ...statement.body.flatMap(statementExpressions)]
    case 'If':
      return [
        statement.condition,
        ...statement.taken.flatMap(statementExpressions),
        ...statement.otherwise.flatMap(statementExpressions),
      ]
    case 'IfLet':
      return [
        statement.selection.subject,
        ...statement.taken.flatMap(statementExpressions),
        ...statement.otherwise.flatMap(statementExpressions),
      ]
  }
}

/** The semantic children of one expression, including a lazy block's statement expressions. */
export const expressionChildren = (expression: Expression): ReadonlyArray<Expression> =>
  (() => {
    switch (expression._tag) {
      case 'Move':
      case 'Project':
      case 'ReferentPlace':
      case 'UnionConvert':
        return [expression._tag === 'UnionConvert' ? expression.source : expression.subject]
      case 'RuntimeStringView':
        return [expression.source]
      case 'StringEquality':
      case 'EnumEquality':
      case 'ShortCircuit':
        return [expression.left, expression.right]
      case 'EnumValue':
        return [expression.value]
      case 'Replace':
        return [expression.value]
      case 'IndexPlace':
        return [expression.subject, expression.index]
      case 'SliceLength':
        return [expression.slice]
      case 'SliceIndexPlace':
        return [expression.slice, expression.index]
      case 'SliceBorrow':
      case 'ValueBorrow':
        return [
          ...(expression.root._tag === 'TemporarySliceRoot' ? [expression.root.value] : []),
          ...expression.selectors.flatMap((selector) =>
            selector._tag === 'Index' || selector._tag === 'SliceIndex' ? [selector.index] : [],
          ),
        ]
      case 'Construct':
      case 'ConstructUnionVariant':
        return expression.fields.map((field) => field.value)
      case 'ArrayConstruct':
        return expression.elements
      case 'Call':
      case 'EffectConstruct':
      case 'ServiceEffectConstruct':
      case 'BuiltinCall':
      case 'InterfaceOperationCall':
        return expression.arguments
      case 'CallableSection':
        return expression.captures.map((capture) => capture.value)
      case 'CallableApply':
        return expression.evaluation === 'LeftThenCallable'
          ? [...expression.arguments, expression.callee]
          : [expression.callee, ...expression.arguments]
      case 'EffectBlock':
        return expression.statements.flatMap(statementExpressions)
      case 'Run':
        return [expression.subject]
      case 'EffectBindRequirement':
        return [expression.protected]
      case 'EffectCatch':
        return [expression.protected, expression.handler]
      case 'Match':
        return [
          expression.scrutinee,
          ...expression.arms.flatMap((arm) => [
            ...(arm.guard === undefined ? [] : [arm.guard]),
            arm.result,
          ]),
        ]
      default:
        return []
    }
  })()

/** One expression and all of its semantic children in deterministic preorder. */
export const expressionTree = (expression: Expression): ReadonlyArray<Expression> => {
  const children = expressionChildren(expression)
  return Object.freeze([expression, ...children.flatMap(expressionTree)])
}

/** The first unavailable expression's cause and span, if the body has one. */
export const firstUnavailable = (
  self: HirFunction,
): { readonly span: SourceSpan.SourceSpan; readonly cause?: Diagnostic.Identity } | undefined => {
  const walk = (
    expression: Expression,
  ): { readonly span: SourceSpan.SourceSpan; readonly cause?: Diagnostic.Identity } | undefined => {
    switch (expression._tag) {
      case 'Unavailable':
        return expression
      case 'Move':
      case 'Project':
      case 'ReferentPlace':
        return walk(expression.subject)
      case 'RuntimeStringView':
        return walk(expression.source)
      case 'StringEquality':
      case 'EnumEquality':
      case 'ShortCircuit':
        return walk(expression.left) ?? walk(expression.right)
      case 'EnumValue':
        return walk(expression.value)
      case 'Replace':
        return walk(expression.value)
      case 'UnionConvert':
        return walk(expression.source)
      case 'IndexPlace':
        return walk(expression.subject) ?? walk(expression.index)
      case 'SliceLength':
        return walk(expression.slice)
      case 'SliceIndexPlace':
        return walk(expression.slice) ?? walk(expression.index)
      case 'Construct':
      case 'ConstructUnionVariant': {
        for (const field of expression.fields) {
          const found = walk(field.value)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'ArrayConstruct': {
        for (const element of expression.elements) {
          const found = walk(element)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'Call':
      case 'EffectConstruct':
      case 'ServiceEffectConstruct':
      case 'BuiltinCall':
      case 'InterfaceOperationCall': {
        for (const argument of expression.arguments) {
          const found = walk(argument)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'CallableSection': {
        for (const capture of expression.captures) {
          const found = walk(capture.value)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'CallableApply': {
        const callee = walk(expression.callee)
        if (callee !== undefined) return callee
        for (const argument of expression.arguments) {
          const found = walk(argument)
          if (found !== undefined) return found
        }
        return undefined
      }
      case 'Run':
        return walk(expression.subject)
      case 'EffectBindRequirement':
        return walk(expression.protected)
      case 'EffectCatch':
        return walk(expression.protected) ?? walk(expression.handler)
      case 'Match': {
        const scrutinee = walk(expression.scrutinee)
        if (scrutinee !== undefined) return scrutinee
        for (const arm of expression.arms) {
          const guard = arm.guard === undefined ? undefined : walk(arm.guard)
          if (guard !== undefined) return guard
          const result = walk(arm.result)
          if (result !== undefined) return result
        }
        return undefined
      }
      default:
        return undefined
    }
  }
  for (const expression of self.statements.flatMap(statementExpressions)) {
    const found = walk(expression)
    if (found !== undefined) return found
  }
  return undefined
}

export type VerificationIssue =
  | { readonly _tag: 'CyclicExpression'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidMatchArmOrder'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidMatchCoverage'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidMatchGuard'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidMatchResult'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidPatternBinding'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidSliceBorrow'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidValueBorrow'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidStaticText'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidSliceOperation'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidStringView'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidStringEquality'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidEnumValue'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidEnumEquality'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidLoanEnd'; readonly span: SourceSpan.SourceSpan }
  | { readonly _tag: 'InvalidBorrowedWrite'; readonly span: SourceSpan.SourceSpan }

const sameMembers = (
  left: ReadonlyArray<Match.CoverageIdentity>,
  right: ReadonlyArray<Match.CoverageIdentity>,
): boolean =>
  left.length === right.length &&
  left.every((member, index) => {
    const other = right.at(index)
    return other !== undefined && Match.identityEquals(member, other)
  })

/** Verifies acyclic expression ownership and the canonical facts carried by structured matches. */
export const verify = (self: Module): ReadonlyArray<VerificationIssue> => {
  const issues: Array<VerificationIssue> = []
  const active = new Set<Expression>()
  const walk = (expression: Expression): void => {
    if (active.has(expression)) {
      issues.push(Object.freeze({ _tag: 'CyclicExpression', span: expression.span }))
      return
    }
    active.add(expression)
    if (
      (expression._tag === 'StaticStringLiteral' &&
        (expression.data.kind !== 'Text' || !Type.equals(expression.type, Type.string))) ||
      (expression._tag === 'StaticByteViewLiteral' &&
        (expression.data.kind !== 'Bytes' ||
          !Type.equals(expression.type, Type.slice('Shared', 'u8'))))
    ) {
      issues.push(Object.freeze({ _tag: 'InvalidStaticText', span: expression.span }))
    }
    if (expression._tag === 'SliceBorrow') {
      const element = Type.isFixedArray(expression.source)
        ? expression.source.element
        : expression.source.element
      if (
        !Type.equals(expression.type, Type.slice(expression.access, element)) ||
        expression.reborrow !== Type.isSlice(expression.source) ||
        (Type.isSlice(expression.source) &&
          expression.source.access === 'Shared' &&
          expression.access === 'Exclusive')
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceBorrow', span: expression.span }))
      }
    }
    if (expression._tag === 'ValueBorrow') {
      const sourceTarget =
        expression.reborrow && Type.isReference(expression.source)
          ? expression.source.target
          : expression.source
      if (
        !Type.equals(expression.type, Type.reference(expression.access, sourceTarget)) ||
        expression.type.access !== expression.access ||
        expression.reborrow !== Type.isReference(expression.source) ||
        expression.suspendsParent !==
          (Type.isReference(expression.source) && expression.source.access === 'Exclusive') ||
        (Type.isReference(expression.source) &&
          expression.source.access === 'Shared' &&
          expression.access === 'Exclusive')
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidValueBorrow', span: expression.span }))
      }
    }
    if (expression._tag === 'RuntimeStringView') {
      const begins = expressionTree(expression.source).flatMap((candidate) =>
        candidate._tag === 'SliceBorrow' || candidate._tag === 'ValueBorrow'
          ? [borrowText(candidate.borrow)]
          : [],
      )
      const held = expression.heldLoans.map(borrowText)
      if (
        expression.source._tag === 'Unavailable' ||
        !Type.equals(expression.type, Type.string) ||
        !Type.equals(expression.source.type, Type.slice('Shared', 'u8')) ||
        begins.length !== held.length ||
        begins.some((begin, ordinal) => begin !== held.at(ordinal)) ||
        new Set(held).size !== held.length
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidStringView', span: expression.span }))
      }
    }
    if (
      expression._tag === 'StringEquality' &&
      (expression.left._tag === 'Unavailable' ||
        expression.right._tag === 'Unavailable' ||
        !Type.equals(expression.left.type, Type.string) ||
        !Type.equals(expression.right.type, Type.string) ||
        !Type.equals(expression.type, 'bool') ||
        expression.intrinsic.name !== 'stringEqualsExact')
    ) {
      issues.push(Object.freeze({ _tag: 'InvalidStringEquality', span: expression.span }))
    }
    if (expression._tag === 'EnumMember') {
      if (
        expression.enum.module !== expression.member.enum.module ||
        expression.enum.name !== expression.member.enum.name ||
        expression.type.module !== expression.enum.module ||
        expression.type.name !== expression.enum.name ||
        expression.type.arguments.length !== 0
      )
        issues.push(Object.freeze({ _tag: 'InvalidEnumValue', span: expression.span }))
    }
    if (
      expression._tag === 'EnumValue' &&
      (expression.value._tag === 'Unavailable' ||
        !Type.isNominal(expression.value.type) ||
        expression.value.type.module !== expression.enum.module ||
        expression.value.type.name !== expression.enum.name ||
        expression.intrinsic.actor !== 'Intrinsic' ||
        expression.intrinsic.name !== 'enumValue')
    )
      issues.push(Object.freeze({ _tag: 'InvalidEnumValue', span: expression.span }))
    if (
      expression._tag === 'EnumEquality' &&
      (expression.left._tag === 'Unavailable' ||
        expression.right._tag === 'Unavailable' ||
        !Type.isNominal(expression.left.type) ||
        !Type.isNominal(expression.right.type) ||
        expression.left.type.module !== expression.enum.module ||
        expression.left.type.name !== expression.enum.name ||
        expression.right.type.module !== expression.enum.module ||
        expression.right.type.name !== expression.enum.name ||
        !Type.equals(expression.type, 'bool'))
    )
      issues.push(Object.freeze({ _tag: 'InvalidEnumEquality', span: expression.span }))
    if (expression._tag === 'SliceLength') {
      if (
        expression.slice._tag === 'Unavailable' ||
        !Type.isSlice(expression.slice.type) ||
        !Type.equals(expression.type, 'usize')
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceOperation', span: expression.span }))
      }
    }
    if (expression._tag === 'SliceIndexPlace') {
      if (
        expression.slice._tag === 'Unavailable' ||
        !Type.equals(expression.slice.type, expression.sourceType) ||
        expression.index._tag === 'Unavailable' ||
        !Type.equals(expression.index.type, 'usize') ||
        expression.access !== expression.sourceType.access ||
        !Type.equals(expression.type, expression.sourceType.element)
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceOperation', span: expression.span }))
      }
    }
    if (expression._tag === 'ReferentPlace') {
      const subjectType =
        'type' in expression.subject && typeof expression.subject.type === 'object'
          ? expression.subject.type
          : undefined
      if (
        subjectType === undefined ||
        !Type.isReference(subjectType) ||
        !Type.equals(subjectType, expression.reference) ||
        expression.borrowAccess !== expression.reference.access ||
        !Type.equals(expression.type, expression.reference.target)
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceOperation', span: expression.span }))
      }
    }
    if (expression._tag === 'Project' && expression.borrowAccess !== undefined) {
      const subjectType =
        'type' in expression.subject && typeof expression.subject.type === 'object'
          ? expression.subject.type
          : undefined
      let inherited: Type.BorrowAccess | undefined
      if (expression.subject._tag === 'SliceIndexPlace') {
        inherited = expression.subject.access
      } else if (expression.subject._tag === 'Project') {
        inherited = expression.subject.borrowAccess
      } else if (subjectType !== undefined && Type.isReference(subjectType)) {
        inherited = subjectType.access
      }
      if (inherited !== expression.borrowAccess) {
        issues.push(Object.freeze({ _tag: 'InvalidSliceOperation', span: expression.span }))
      }
    }
    if (
      expression._tag === 'Call' ||
      expression._tag === 'BuiltinCall' ||
      expression._tag === 'InterfaceOperationCall' ||
      expression._tag === 'CallableApply'
    ) {
      const begins = expression.arguments
        .flatMap(expressionTree)
        .filter(
          (
            candidate,
          ): candidate is Extract<Expression, { readonly _tag: 'SliceBorrow' | 'ValueBorrow' }> =>
            (candidate._tag === 'SliceBorrow' || candidate._tag === 'ValueBorrow') &&
            candidate.borrow.callSpan.start === expression.span.start &&
            candidate.borrow.callSpan.end === expression.span.end,
        )
        .map((candidate) => borrowText(candidate.borrow))
      const authoredEnds = [
        ...expression.loanEnds,
        ...(expression._tag === 'InterfaceOperationCall' ? [] : expression.heldLoans),
      ]
      const ends = authoredEnds
        .filter(
          (borrow) =>
            borrow.callSpan.start === expression.span.start &&
            borrow.callSpan.end === expression.span.end,
        )
        .map(borrowText)
      if (
        begins.length !== ends.length ||
        begins.some((begin, ordinal) => begin !== ends.at(ordinal)) ||
        new Set(authoredEnds.map(borrowText)).size !== authoredEnds.length
      ) {
        issues.push(Object.freeze({ _tag: 'InvalidLoanEnd', span: expression.span }))
      }
    }
    if (expression._tag === 'Match') {
      const coverage = Match.cover(
        expression.members,
        expression.arms.map((arm) =>
          Object.freeze({
            ...(arm.member === undefined ? {} : { member: arm.member }),
            universal: arm.universal,
            guarded: arm.guard !== undefined,
          }),
        ),
      )
      for (const [index, arm] of expression.arms.entries()) {
        const transition = coverage.transitions.at(index)
        if (arm.id.ordinal !== index) {
          issues.push(Object.freeze({ _tag: 'InvalidMatchArmOrder', span: arm.span }))
        }
        if (
          transition === undefined ||
          transition.reachable !== arm.reachable ||
          !sameMembers(transition.before, arm.before) ||
          !sameMembers(transition.after, arm.after)
        ) {
          issues.push(Object.freeze({ _tag: 'InvalidMatchCoverage', span: arm.span }))
        }
        if (
          arm.guard !== undefined &&
          (arm.guard._tag === 'Unavailable' || !Type.equals(arm.guard.type, 'bool'))
        ) {
          issues.push(Object.freeze({ _tag: 'InvalidMatchGuard', span: arm.guard.span }))
        }
        if (
          arm.result._tag === 'Unavailable' ||
          !TypeCompatibility.isCompatible(TypeCompatibility.check(arm.result.type, expression.type))
        ) {
          issues.push(Object.freeze({ _tag: 'InvalidMatchResult', span: arm.result.span }))
        }
        for (const binding of arm.bindings) {
          if (
            binding.id.arm.ordinal !== index ||
            binding.path.length === 0 ||
            binding.access !== expression.access
          ) {
            issues.push(Object.freeze({ _tag: 'InvalidPatternBinding', span: binding.span }))
          }
        }
      }
    }
    const children = expressionChildren(expression)
    for (const child of children) walk(child)
    active.delete(expression)
  }
  for (const fn of self.functions) {
    const statements = (body: ReadonlyArray<Statement>): void => {
      for (const statement of body) {
        if (statement._tag === 'Write' && statement.place._tag === 'BorrowedWritePlace') {
          const [first, ...rest] = statement.place.selectors
          const wellFormed = Type.isReference(statement.place.slice)
            ? statement.place.slice.access === 'Exclusive' &&
              statement.place.selectors.every(
                (selector) => selector._tag === 'Field' || selector._tag === 'Index',
              )
            : statement.place.slice.access === 'Exclusive' &&
              first?._tag === 'SliceIndex' &&
              Type.equals(first.slice, statement.place.slice) &&
              !rest.some((selector) => selector._tag !== 'Field')
          if (!wellFormed) {
            issues.push(Object.freeze({ _tag: 'InvalidBorrowedWrite', span: statement.place.span }))
          }
        }
        if (statement._tag === 'Unsafe') statements(statement.statements)
        else if (statement._tag === 'If') {
          statements(statement.taken)
          statements(statement.otherwise)
        } else if (statement._tag === 'While') statements(statement.body)
      }
    }
    statements(fn.statements)
    for (const expression of fn.statements.flatMap(statementExpressions)) walk(expression)
  }
  return Object.freeze(issues)
}

/** One module's elaborated HIR. */
export interface Module {
  readonly _tag: 'HirModule'
  readonly module: string
  readonly functions: ReadonlyArray<HirFunction>
}

/** Normalizes one header's contract, or keeps it explicitly unavailable with its cause. */
export const contractOf = (declaration: DeclarationFacts.DeclarationFact): ContractFact => {
  const parameters: Array<DeclarationFacts.SemanticType> = []
  for (const parameter of declaration.parameters) {
    if (parameter.phase === 'Static') continue
    if (parameter.declaredType._tag !== 'Resolved') {
      return Object.freeze({
        _tag: 'Unavailable',
        ...(parameter.declaredType._tag === 'Unresolved' &&
        parameter.declaredType.cause !== undefined
          ? { cause: parameter.declaredType.cause }
          : {}),
      })
    }
    parameters.push(parameter.declaredType.type)
  }
  if (declaration.returnType._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'Unavailable',
      ...(declaration.returnType._tag === 'Unresolved' && declaration.returnType.cause !== undefined
        ? { cause: declaration.returnType.cause }
        : {}),
    })
  }
  return Object.freeze({
    _tag: 'Contract',
    unsafe: declaration.unsafe,
    ...(declaration.functionKind === 'Effect'
      ? {
          functionKind: 'Effect' as const,
          failureRow: declaration.failureRow.row,
          requirementRow: declaration.requirementRow.row,
        }
      : {}),
    parameters: Object.freeze(parameters),
    result: declaration.returnType.type,
    constraints: declaration.constraintContracts,
  })
}

const spanText = (span: SourceSpan.SourceSpan): string => `[${span.start}, ${span.end})`

const identityLabel = (declaration: DeclarationFacts.DeclarationFact): string => {
  switch (declaration.canonical._tag) {
    case 'Canonical':
      return `${declaration.canonical.id.module}.${declaration.canonical.id.name}`
    case 'Duplicate':
      return `duplicate:${declaration.canonical.original.module}.${declaration.canonical.original.name}#${declaration.id.ordinal}`
    case 'Unidentified':
      return `unidentified#${declaration.id.ordinal}`
  }
}

const contractText = (contract: ContractFact): string =>
  contract._tag === 'Contract'
    ? `(${contract.parameters.map(Type.encode).join(', ')}) -> ${Type.encode(contract.result)}`
    : 'contract-unavailable'

const sliceRootText = (root: SliceRoot): string => {
  switch (root._tag) {
    case 'BindingSliceRoot':
      return `b${root.binding.ordinal}`
    case 'ParameterSliceRoot':
      return `p${root.parameter.ordinal}`
    case 'PatternSliceRoot':
      return `a${root.binding.arm.ordinal}.b${root.binding.ordinal}`
    case 'TemporarySliceRoot':
      return `t${root.owner.span.start}.${root.owner.ordinal}`
  }
}

const borrowSelectorsText = (selectors: ReadonlyArray<BorrowSelector>): string =>
  selectors
    .map((selector) => {
      if (selector._tag === 'Field') return `.#${selector.field.ordinal}`
      if (selector._tag === 'SliceIndex') return '[runtime/slice]'
      const index = selector.bounds._tag === 'Proven' ? selector.bounds.index : 'runtime'
      return `[${index}/${selector.array.length}]`
    })
    .join('')

const encodeExpression = (expression: Expression, depth: number): string => {
  const indent = '  '.repeat(depth)
  switch (expression._tag) {
    case 'IntegerLiteral':
      return `${indent}literal ${expression.value} : ${Type.encode(expression.type)}${expression.constant === undefined ? '' : ` constant=${expression.constant.module}::${expression.constant.name}`} ${spanText(expression.span)}`
    case 'FloatingLiteral':
      return `${indent}literal ${expression.spelling} bits=0x${expression.bits.toString(16)} : ${expression.type} ${spanText(expression.span)}`
    case 'StaticStringLiteral':
      return `${indent}static-string ${expression.data.id} bytes=${expression.data.bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('')} length=${expression.data.bytes.length} provenance=program : string ${spanText(expression.span)}`
    case 'StaticByteViewLiteral':
      return `${indent}static-bytes ${expression.data.id} bytes=${expression.data.bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('')} length=${expression.data.bytes.length} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'UnitLiteral':
      return `${indent}unit : () ${spanText(expression.span)}`
    case 'BooleanLiteral':
      return `${indent}literal ${expression.value} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'CharacterLiteral':
      return `${indent}literal U+${expression.value.toString(16).toUpperCase().padStart(4, '0')} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'ParameterReference':
      return `${indent}param fn${expression.parameter.function.ordinal}.p${expression.parameter.ordinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'BindingReference':
      return `${indent}binding fn${expression.binding.function.ordinal}.b${expression.binding.ordinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'PatternBindingReference':
      return `${indent}pattern-binding a${expression.binding.arm.ordinal}.b${expression.binding.ordinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'Move':
      return [
        `${indent}move : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'RuntimeStringView':
      return [
        `${indent}runtime-string-view loans=${expression.heldLoans.map(borrowText).join(',') || 'none'} : string ${spanText(expression.span)}`,
        encodeExpression(expression.source, depth + 1),
      ].join('\n')
    case 'StringEquality':
      return [
        `${indent}string-${expression.negated ? 'not-equals' : 'equals'} intrinsic=${Intrinsic.operationText(expression.intrinsic)} : bool ${spanText(expression.span)}`,
        encodeExpression(expression.left, depth + 1),
        encodeExpression(expression.right, depth + 1),
      ].join('\n')
    case 'ShortCircuit':
      return [
        `${indent}short-circuit ${expression.operator === 'And' ? '&&' : '||'} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.left, depth + 1),
        encodeExpression(expression.right, depth + 1),
      ].join('\n')
    case 'Replace':
      return [
        `${indent}replace : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.value, depth + 1),
      ].join('\n')
    case 'Run':
      return [
        `${indent}run : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'EffectCatch':
      return [
        `${indent}effect-catch intrinsic=${Intrinsic.operationText(expression.intrinsic)} ${Type.encode(expression.selected)} protected=${RowAlgebra.encode(
          Type.failureRowPolicy(),
          expression.protectedRow,
          Type.encode,
          (parameter) => parameter.name,
          (member) => member.parameter.name,
        )} handler=${RowAlgebra.encode(
          Type.failureRowPolicy(),
          expression.handlerRow,
          Type.encode,
          (parameter) => parameter.name,
          (member) => member.parameter.name,
        )} residual=${RowAlgebra.encode(
          Type.failureRowPolicy(),
          expression.residualRow,
          Type.encode,
          (parameter) => parameter.name,
          (member) => member.parameter.name,
        )} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.protected, depth + 1),
        encodeExpression(expression.handler, depth + 1),
      ].join('\n')
    case 'EffectBindRequirement':
      return [
        `${indent}effect-provide ${
          expression.provider.capability === undefined
            ? RowAlgebra.encode(
                Type.requirementRowPolicy(),
                expression.provider.selected,
                (member) => Type.encode(member.capability),
                (parameter) => parameter.name,
                (member) => member.capability.name,
              )
            : Type.encode(expression.provider.capability)
        }@${expression.provider.role ?? 'DefaultRole'} selection=${expression.provider.selectionAccess.toLowerCase()} capture=${expression.provider.captureAccess.toLowerCase()} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.protected, depth + 1),
      ].join('\n')
    case 'EffectBlock':
      return [
        `${indent}effect-block site=${executableSiteLabel(expression.site)} access=${expression.type.access.toLowerCase()} captures=${expression.captures.map((capture) => `${capture.binding === undefined ? `p${capture.parameter?.ordinal ?? '?'}` : `b${capture.binding.ordinal}`}:${capture.access.toLowerCase()}`).join(',') || 'none'} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.statements.map((statement) => encodeStatement(statement, depth + 1)),
      ].join('\n')
    case 'UnionConvert':
      return [
        `${indent}union-${expression.conversion.toLowerCase()} ${Type.encode(expression.sourceType)} -> ${Type.encode(expression.target)} access=${expression.access} context=${expression.context} expected=${spanText(expression.expectedAt)} ${spanText(expression.span)}`,
        `${indent}  mapping ${expression.mappings.map((mapping) => `${Type.encode(mapping.source)}#${mapping.sourceOrdinal}->${Type.encode(mapping.target)}#${mapping.targetOrdinal}`).join(', ') || 'empty'}`,
        encodeExpression(expression.source, depth + 1),
      ].join('\n')
    case 'Match':
      return [
        `${indent}match ${expression.access.toLowerCase()} members=${expression.members.map(Match.encodeIdentity).join(',') || 'none'} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        `${indent}  scrutinee`,
        encodeExpression(expression.scrutinee, depth + 2),
        ...expression.arms.flatMap((arm) => {
          let pattern = 'unknown'
          if (arm.universal) pattern = '_'
          else if (arm.member !== undefined) pattern = Match.encodeIdentity(arm.member)
          return [
            `${indent}  arm #${arm.id.ordinal} ${pattern} reachable=${arm.reachable} before=${arm.before.map(Match.encodeIdentity).join(',') || 'empty'} after=${arm.after.map(Match.encodeIdentity).join(',') || 'empty'} ${spanText(arm.span)}`,
            ...arm.bindings.map(
              (binding) =>
                `${indent}    binding #${binding.id.ordinal} ${binding.name ?? '?'} path=${binding.path.map((field) => `#${field.ordinal}`).join('.') || 'root'} access=${binding.access} : ${Type.encode(binding.type)} ${spanText(binding.span)}`,
            ),
            `${indent}    cleanup ${arm.cleanup.map((path) => path.map((field) => `#${field.ordinal}`).join('.') || 'payload').join(',') || 'none'}`,
            ...(arm.guard === undefined
              ? []
              : [`${indent}    guard`, encodeExpression(arm.guard, depth + 3)]),
            `${indent}    result`,
            encodeExpression(arm.result, depth + 3),
          ]
        }),
      ].join('\n')
    case 'Construct':
      return [
        `${indent}construct ${Type.encode(expression.nominal)} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        `${indent}  evaluation-order ${expression.evaluationOrder.map((field) => `#${field.ordinal}`).join(', ') || 'empty'}`,
        ...expression.fields.map(
          ({ field, value }) =>
            `${indent}  field #${field.ordinal}\n${encodeExpression(value, depth + 2)}`,
        ),
      ].join('\n')
    case 'ConstructUnionVariant':
      return [
        `${indent}construct-variant ${Type.encode(expression.nominal)}.${expression.variant.name}#${expression.variantOrdinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        `${indent}  evaluation-order ${expression.evaluationOrder.map((field) => `#${field.ordinal}`).join(', ') || 'empty'}`,
        ...expression.fields.map(
          ({ field, value }) =>
            `${indent}  field #${field.ordinal}\n${encodeExpression(value, depth + 2)}`,
        ),
      ].join('\n')
    case 'ArrayConstruct':
      return [
        `${indent}construct-array ${Type.encode(expression.type)} elements=${expression.elements.length} ${spanText(expression.span)}`,
        ...expression.elements.map(
          (element, index) =>
            `${indent}  element #${index}\n${encodeExpression(element, depth + 2)}`,
        ),
      ].join('\n')
    case 'Project':
      return [
        `${indent}project ${expression.access} ${Type.encode(expression.nominal)}.#${expression.field.ordinal} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'ReferentPlace':
      return [
        `${indent}referent ${expression.access} ${expression.borrowAccess.toLowerCase()} source=${Type.encode(expression.reference)} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
      ].join('\n')
    case 'IndexPlace':
      return [
        `${indent}index ${expression.access} ${Type.encode(expression.array)} bounds=${
          expression.bounds._tag === 'Runtime'
            ? `runtime:${expression.bounds.length}`
            : `proven:${expression.bounds.index}/${expression.bounds.length}`
        } : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.subject, depth + 1),
        encodeExpression(expression.index, depth + 1),
      ].join('\n')
    case 'SliceBorrow':
      return `${indent}${expression.reborrow ? 'reborrow-slice' : 'borrow-slice'} l${expression.borrow.ordinal} ${expression.access.toLowerCase()} ${sliceRootText(expression.root)}${borrowSelectorsText(expression.selectors)} source=${Type.encode(expression.source)} : ${Type.encode(expression.type)} suspended=${expression.suspendsParent} ${spanText(expression.span)}`
    case 'ValueBorrow':
      return `${indent}${expression.reborrow ? 'reborrow-value' : 'borrow-value'} l${expression.borrow.ordinal} ${expression.access.toLowerCase()} ${sliceRootText(expression.root)}${borrowSelectorsText(expression.selectors)} source=${Type.encode(expression.source)} : ${Type.encode(expression.type)} suspended=${expression.suspendsParent} ${spanText(expression.span)}`
    case 'SliceLength':
      return [
        `${indent}slice-length : i32 ${spanText(expression.span)}`,
        encodeExpression(expression.slice, depth + 1),
      ].join('\n')
    case 'SliceIndexPlace':
      return [
        `${indent}slice-index ${expression.access.toLowerCase()} bounds=runtime : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        encodeExpression(expression.slice, depth + 1),
        encodeExpression(expression.index, depth + 1),
      ].join('\n')
    case 'EnumMember':
      return `${indent}enum-member ${expression.member.enum.module}.${expression.member.enum.name}.${expression.member.name} discriminant=${expression.discriminant} : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'EnumValue':
      return [
        `${indent}enum-value ${expression.enum.module}.${expression.enum.name} via ${Intrinsic.operationText(expression.intrinsic)} : ${expression.type} ${spanText(expression.span)}`,
        encodeExpression(expression.value, depth + 1),
      ].join('\n')
    case 'EnumEquality':
      return [
        `${indent}enum-${expression.negated ? 'not-equals' : 'equals'} ${expression.enum.module}.${expression.enum.name} : bool ${spanText(expression.span)}`,
        encodeExpression(expression.left, depth + 1),
        encodeExpression(expression.right, depth + 1),
      ].join('\n')
    case 'FunctionItem':
      return `${indent}function-item ${
        expression.target._tag === 'DeclarationCallableTarget'
          ? `${expression.target.declaration.module}.${expression.target.declaration.name}`
          : `${expression.target.actor}.${expression.target.operation}`
      }<${expression.typeArguments.map(Type.genericArgumentKey).join(',')}> : ${Type.encode(expression.type)} ${spanText(expression.span)}`
    case 'CallableSection':
      return [
        `${indent}callable-section site=${executableSiteLabel(expression.site)} mode=${expression.mode.toLowerCase()} remaining=${expression.remainingParameters.map((ordinal) => `p${ordinal}`).join(',')} target=${
          expression.target._tag === 'DeclarationCallableTarget'
            ? `${expression.target.declaration.module}.${expression.target.declaration.name}`
            : `${expression.target.actor}.${expression.target.operation}`
        } : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.captures.map(
          (capture) =>
            `${indent}  capture #${capture.ordinal}->p${capture.parameterOrdinal} ${capture.access.toLowerCase()}\n${encodeExpression(capture.value, depth + 2)}`,
        ),
      ].join('\n')
    case 'CallableApply':
      return [
        `${indent}callable-apply access=${expression.access.toLowerCase()} evaluation=${expression.evaluation} realization=${expression.realization} substitution=${[...expression.substitution.entries()].map(([parameter, argument]) => `${parameter}=${Type.encodeGenericArgument(argument)}`).join(',') || 'none'} ends=${expression.loanEnds.map(borrowText).join(',') || 'none'} held=${expression.heldLoans.map(borrowText).join(',') || 'none'} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...(expression.evaluation === 'LeftThenCallable'
          ? [
              ...expression.arguments.map(
                (argument, ordinal) =>
                  `${indent}  argument #${ordinal}\n${encodeExpression(argument, depth + 2)}`,
              ),
              `${indent}  callee`,
              encodeExpression(expression.callee, depth + 2),
            ]
          : [
              `${indent}  callee`,
              encodeExpression(expression.callee, depth + 2),
              ...expression.arguments.map(
                (argument, ordinal) =>
                  `${indent}  argument #${ordinal}\n${encodeExpression(argument, depth + 2)}`,
              ),
            ]),
      ].join('\n')
    case 'Call':
    case 'EffectConstruct':
      return [
        `${indent}${expression._tag === 'EffectConstruct' ? 'effect-' : ''}call ${expression.target.module}.${expression.target.name}${
          expression.typeArguments.length === 0
            ? ''
            : `<${expression.typeArguments.map(Type.encodeGenericArgument).join(', ')}>`
        }${expression.evidence.length === 0 ? '' : ` evidence=${expression.evidence.map(Constraint.evidenceKey).join(',')}`}${expression.staticArguments.length === 0 ? '' : ` static=${expression.staticArguments.map(StaticValue.presentation).join(',')}`} : ${Type.encode(expression.type)} loan-ends=${expression.loanEnds.map((loan) => `l${loan.ordinal}`).join(',') || 'none'} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'ServiceEffectConstruct':
      return [
        `${indent}service-call ${Type.encode(expression.service)}.${expression.operation}@${expression.role}:${expression.access.toLowerCase()} : ${Type.encode(expression.type)} loan-ends=${expression.loanEnds.map((loan) => `l${loan.ordinal}`).join(',') || 'none'} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'BuiltinCall': {
      const first = expression.arguments.at(0)
      const actor =
        first === undefined || first._tag === 'Unavailable' ? '?' : Type.encode(first.type)
      return [
        `${indent}builtin ${actor}.${expression.operation} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    }
    case 'InterfaceOperationCall':
      return [
        `${indent}interface ${Type.encode(expression.capability)}.${expression.operation} over ${Type.encode(expression.provider)} : ${Type.encode(expression.type)} ${spanText(expression.span)}`,
        ...expression.arguments.map((argument) => encodeExpression(argument, depth + 1)),
      ].join('\n')
    case 'Unavailable':
      return `${indent}unavailable ${spanText(expression.span)}`
  }
}

const encodeStatement = (statement: Statement, depth: number): string => {
  const indent = '  '.repeat(depth)
  switch (statement._tag) {
    case 'UnavailableStatement':
      return `${indent}unavailable-statement r${statement.region.ordinal} ${spanText(statement.span)}`
    case 'Unsafe':
      return [
        `${indent}unsafe r${statement.region.ordinal} ${spanText(statement.span)}`,
        ...statement.statements.map((inner) => encodeStatement(inner, depth + 1)),
      ].join('\n')
    case 'Bind':
      return [
        `${indent}bind ${statement.mutability.toLowerCase()} b${statement.binding.ordinal} ${statement.name ?? '?'} r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.initializer, depth + 1),
      ].join('\n')
    case 'PatternBind':
      return `${indent}pattern-bind ${statement.selection.access.toLowerCase()} members=${statement.selection.members.map(Match.encodeIdentity).join(',')} r${statement.region.ordinal} ${spanText(statement.span)}`
    case 'Evaluate':
      return [
        `${indent}evaluate r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.expression, depth + 1),
      ].join('\n')
    case 'Write': {
      let root: string
      if (statement.place._tag === 'WritePlace') {
        root =
          statement.place.root._tag === 'BindingWriteRoot'
            ? `b${statement.place.root.binding.ordinal}`
            : `p${statement.place.root.parameter.ordinal}`
      } else {
        root =
          statement.place.root._tag === 'BindingSliceRoot'
            ? `slice-b${statement.place.root.binding.ordinal}`
            : `slice-p${statement.place.root.parameter.ordinal}`
      }
      const selectors = statement.place.selectors
        .map((selector) => {
          if (selector._tag === 'Field') return `.#${selector.field.ordinal}`
          if (selector._tag === 'SliceIndex') return '[runtime/slice]'
          const index = selector.bounds._tag === 'Proven' ? selector.bounds.index : 'runtime'
          return `[${index}/${selector.array.length}]`
        })
        .join('')
      return [
        `${indent}write ${root}${selectors} : ${Type.encode(statement.place.type)} r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.value, depth + 1),
      ].join('\n')
    }
    case 'If':
      return [
        `${indent}if r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.condition, depth + 1),
        `${indent}then`,
        ...statement.taken.map((inner) => encodeStatement(inner, depth + 1)),
        ...(statement.otherwise.length === 0
          ? []
          : [
              `${indent}else`,
              ...statement.otherwise.map((inner) => encodeStatement(inner, depth + 1)),
            ]),
      ].join('\n')
    case 'IfLet':
      return [
        `${indent}if-let ${statement.selection.access.toLowerCase()} members=${statement.selection.members.map(Match.encodeIdentity).join(',')} r${statement.region.ordinal} ${spanText(statement.span)}`,
        ...statement.taken.map((inner) => encodeStatement(inner, depth + 1)),
        ...statement.otherwise.map((inner) => encodeStatement(inner, depth + 1)),
      ].join('\n')
    case 'While':
      return [
        `${indent}while loop${statement.loop.ordinal} r${statement.region.ordinal}${statement.parent === undefined ? '' : ` parent=loop${statement.parent.ordinal}`} ${spanText(statement.span)}`,
        encodeExpression(statement.condition, depth + 1),
        ...statement.body.map((inner) => encodeStatement(inner, depth + 1)),
      ].join('\n')
    case 'Break':
    case 'Continue':
      return `${indent}${statement._tag.toLowerCase()} loop${statement.target.ordinal} r${statement.region.ordinal} ${spanText(statement.span)}`
    case 'Return':
      return [
        `${indent}return r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.expression, depth + 1),
      ].join('\n')
    case 'Fail':
      return [
        `${indent}fail ${Type.encode(statement.failure)} r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.expression, depth + 1),
      ].join('\n')
    case 'Drop':
      return [
        `${indent}drop r${statement.region.ordinal} ${spanText(statement.span)}`,
        encodeExpression(statement.expression, depth + 1),
      ].join('\n')
  }
}

/**
 * Deterministic textual encoding of one module's completed HIR for debugging, inspection, and
 * golden tests. No compatibility promise attaches to this format.
 */
export const encode = (self: Module): string =>
  [
    `hir-module ${self.module}`,
    ...self.functions.flatMap((fn) => [
      `fn ${identityLabel(fn.declaration)}${
        fn.declaration.typeParameters.length === 0
          ? ''
          : `<${fn.declaration.typeParameters.map((parameter) => Type.key(parameter.type)).join(',')}>`
      } ${contractText(fn.contract)} entry=r${fn.entryRegion.ordinal} regions=${fn.regionOrder.map((region) => `r${region.ordinal}`).join(',')}`,
      ...fn.statements.map((statement) => encodeStatement(statement, 1)),
    ]),
    '',
  ].join('\n')
