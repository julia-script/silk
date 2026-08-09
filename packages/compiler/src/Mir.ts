import * as Option from 'effect/Option'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import * as Match from './Match.js'
import type * as Ownership from './Ownership.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as Target from './Target.js'
import * as SilkType from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/**
 * MIR is the monomorphic, target-aware, backend-neutral structured control DAG. Structural child
 * and continuation references are acyclic. Loop repetition and exit are lexical outcomes rather
 * than graph edges; only a backend-private lowering may introduce a cyclic CFG.
 */

export type ScalarType = {
  readonly [Spelling in SilkType.Builtin]: { readonly _tag: Spelling }
}[SilkType.Builtin]

export type Type =
  | ScalarType
  | { readonly _tag: 'Bottom'; readonly type: SilkType.Bottom }
  | { readonly _tag: 'Nominal'; readonly type: SilkType.Nominal }
  | { readonly _tag: 'FixedArray'; readonly type: SilkType.FixedArray }
  | { readonly _tag: 'Slice'; readonly type: SilkType.Slice }
  | { readonly _tag: 'Reference'; readonly type: SilkType.Reference }
  | { readonly _tag: 'Union'; readonly type: SilkType.StructuralUnion }
  | {
      readonly _tag: 'EffectBorrow'
      readonly type: DeclarationIndex.SemanticType
      readonly access: 'Shared' | 'Exclusive'
    }
  | {
      readonly _tag: 'EffectValue'
      readonly type: SilkType.Effect
      readonly site: Hir.EffectSiteId
      readonly environment: Extract<
        Layout.EffectEnvironment,
        { readonly _tag: 'EffectEnvironment' }
      >
    }
  | {
      readonly _tag: 'CallableValue'
      readonly type: SilkType.Callable
      readonly target: Hir.CallableTarget
      readonly site?: Hir.CallableSiteId
      readonly environment?: Extract<
        Layout.CallableEnvironment,
        { readonly _tag: 'CallableEnvironment' }
      >
    }
  | { readonly _tag: 'EffectOutcome'; readonly type: SilkType.Effect }

export const semanticType = (self: Type): DeclarationIndex.SemanticType =>
  self._tag === 'Nominal' ||
  self._tag === 'Bottom' ||
  self._tag === 'FixedArray' ||
  self._tag === 'Slice' ||
  self._tag === 'Reference' ||
  self._tag === 'Union' ||
  self._tag === 'EffectBorrow' ||
  self._tag === 'EffectValue' ||
  self._tag === 'CallableValue' ||
  self._tag === 'EffectOutcome'
    ? self.type
    : self._tag

const typeText = (self: Type): string => SilkType.encode(semanticType(self))
const isCopyType = (type: DeclarationIndex.SemanticType): boolean =>
  SilkType.isBuiltin(type) || (SilkType.isFixedArray(type) && isCopyType(type.element))

const isStructurallyCopyType = (
  layout: Layout.Plan,
  type: DeclarationIndex.SemanticType,
  visiting = new Set<string>(),
): boolean => {
  if (SilkType.isBuiltin(type) || SilkType.isReference(type) || SilkType.isSlice(type)) return true
  if (SilkType.isFixedArray(type)) return isStructurallyCopyType(layout, type.element, visiting)
  if (SilkType.equals(type, SilkType.unit)) return true
  if (!SilkType.isNominal(type) || SilkType.isIntrinsicNominal(type)) return false
  const key = SilkType.key(type)
  if (visiting.has(key)) return false
  const entry = Layout.entry(layout, type)
  if (entry?.representation._tag !== 'Aggregate') return false
  const next = new Set(visiting).add(key)
  return entry.representation.fields.every((field) =>
    isStructurallyCopyType(layout, field.type, next),
  )
}

const callingScalarEquals = (left: Layout.CallingScalar, right: Layout.CallingScalar): boolean =>
  typeof left === 'string'
    ? left === right
    : typeof right !== 'string' &&
      SilkType.equals(left.element, right.element) &&
      left.bits === right.bits

const callingShapeEquals = (left: Layout.CallingShape, right: Layout.CallingShape): boolean =>
  left.laneCount === right.laneCount &&
  left.lanes.length === right.lanes.length &&
  left.lanes.every((lane, ordinal) => {
    const candidate = right.lanes.at(ordinal)
    return (
      candidate !== undefined &&
      callingScalarEquals(lane.type, candidate.type) &&
      lane.path.length === candidate.path.length &&
      lane.path.every((selector, index) => {
        const other = candidate.path.at(index)
        return other !== undefined && Layout.selectorEquals(selector, other)
      })
    )
  })

export interface LocalId {
  readonly _tag: 'Local'
  readonly ordinal: number
}

export interface RegionId {
  readonly _tag: 'Region'
  readonly ordinal: number
}

export interface LoopId {
  readonly _tag: 'Loop'
  readonly ordinal: number
}

export interface Provenance {
  readonly span: SourceSpan.SourceSpan
  readonly generated: boolean
}

export type BinaryOperator =
  | 'Add'
  | 'Subtract'
  | 'Multiply'
  | 'Divide'
  | 'Remainder'
  | 'Equals'
  | 'NotEquals'
  | 'LessThan'
  | 'LessOrEqual'
  | 'GreaterThan'
  | 'GreaterOrEqual'
  | 'BitAnd'
  | 'BitOr'
  | 'BitXor'
  | 'ShiftLeft'
  | 'ShiftRight'
  | 'RotateLeft'
  | 'RotateRight'
  | 'WrappingAdd'
  | 'WrappingSubtract'
  | 'WrappingMultiply'
  | 'SaturatingAdd'
  | 'SaturatingSubtract'
  | 'SaturatingMultiply'

export const isBinaryOperator = (operation: Hir.BuiltinOperation): operation is BinaryOperator =>
  operation === 'Add' ||
  operation === 'Subtract' ||
  operation === 'Multiply' ||
  operation === 'Divide' ||
  operation === 'Remainder' ||
  operation === 'Equals' ||
  operation === 'NotEquals' ||
  operation === 'LessThan' ||
  operation === 'LessOrEqual' ||
  operation === 'GreaterThan' ||
  operation === 'GreaterOrEqual' ||
  operation === 'BitAnd' ||
  operation === 'BitOr' ||
  operation === 'BitXor' ||
  operation === 'ShiftLeft' ||
  operation === 'ShiftRight' ||
  operation === 'RotateLeft' ||
  operation === 'RotateRight' ||
  operation === 'WrappingAdd' ||
  operation === 'WrappingSubtract' ||
  operation === 'WrappingMultiply' ||
  operation === 'SaturatingAdd' ||
  operation === 'SaturatingSubtract' ||
  operation === 'SaturatingMultiply'

export type PlaceSelector =
  | {
      readonly _tag: 'FieldSelector'
      readonly field: DeclarationIndex.FieldId
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ElementSelector'
      readonly length: number
      readonly index:
        | { readonly _tag: 'Proven'; readonly value: number }
        | { readonly _tag: 'Runtime'; readonly local: LocalId }
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'SliceElementSelector'
      readonly index: LocalId
      readonly access: SilkType.Slice['access']
      readonly provenance: Provenance
    }

export type Operation =
  | {
      readonly _tag: 'Literal'
      readonly destination: LocalId
      readonly type: Type
      readonly value: number | bigint
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Binary'
      readonly operator: BinaryOperator
      readonly destination: LocalId
      readonly left: LocalId
      readonly right: LocalId
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ConvertInteger'
      readonly destination: LocalId
      readonly source: LocalId
      readonly sourceType: ScalarType
      readonly type: ScalarType
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'CheckedInteger'
      readonly operation: Scalar.OperationCode
      readonly destination: LocalId
      readonly operands: ReadonlyArray<LocalId>
      readonly sourceType: ScalarType
      readonly valueType: ScalarType
      readonly type: Extract<Type, { readonly _tag: 'Union' }>
      readonly success: SilkType.Nominal
      readonly failure: SilkType.Nominal
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ValidateLayout'
      readonly destination: LocalId
      readonly bytes: LocalId
      readonly alignment: LocalId
      readonly type: Extract<Type, { readonly _tag: 'Union' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RepeatLayout'
      readonly destination: LocalId
      readonly layout: LocalId
      readonly count: LocalId
      readonly type: Extract<Type, { readonly _tag: 'Union' }>
      readonly provenance: Provenance
    }
  | {
      /** Creates one logical heap block and its self-contained reclaim ticket. */
      readonly _tag: 'Allocate'
      readonly destination: LocalId
      readonly layout: LocalId
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly failure: SilkType.Nominal
      readonly propagationType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly failureTag: number
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RawBufferFrom'
      readonly destination: LocalId
      readonly allocation: LocalId
      readonly count: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly stride: number
      readonly elementAlignment: number
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RawBufferCount'
      readonly destination: LocalId
      readonly buffer: LocalId
      readonly type: Extract<Type, { readonly _tag: 'usize' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RawBufferSlot'
      readonly destination: LocalId
      readonly buffer: LocalId
      readonly index: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'SlotWrite'
      readonly destination: LocalId
      readonly slot: LocalId
      readonly value: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'SlotTake'
      readonly destination: LocalId
      readonly slot: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      /** Non-consuming read of an initialized slot; verified for Copy element types only. */
      readonly _tag: 'SlotCopy'
      readonly destination: LocalId
      readonly slot: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'SlotDrop'
      readonly destination: LocalId
      readonly slot: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly cleanup: Ownership.CleanupPlan
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Move'
      readonly destination: LocalId
      readonly source: LocalId
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'BeginLoan'
      readonly borrow: Hir.BorrowId
      readonly destination: LocalId
      readonly root: LocalId
      readonly sourceType: Extract<Type, { readonly _tag: 'Nominal' | 'FixedArray' | 'Slice' }>
      readonly type: Extract<Type, { readonly _tag: 'Slice' | 'Reference' }>
      readonly access: SilkType.Slice['access']
      readonly reborrow: boolean
      readonly suspendsParent: boolean
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'EndLoan'
      readonly borrow: Hir.BorrowId
      readonly slice: LocalId
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'SliceLength'
      readonly destination: LocalId
      readonly slice: LocalId
      readonly type: Extract<Type, { readonly _tag: 'usize' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ConvertUnion'
      readonly destination: LocalId
      readonly source: LocalId
      readonly sourceType:
        | Extract<Type, { readonly _tag: 'Nominal' }>
        | Extract<Type, { readonly _tag: 'Union' }>
      readonly targetType: Extract<Type, { readonly _tag: 'Union' }>
      readonly conversion: 'Inject' | 'Widen'
      readonly mappings: ReadonlyArray<TypeCompatibility.MemberMapping>
      readonly sourceShape: Layout.CallingShape
      readonly targetShape: Layout.CallingShape
      readonly access: 'Copy' | 'Owned'
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Call'
      readonly destination: LocalId
      readonly target: DeclarationIndex.CanonicalId
      readonly typeArguments: ReadonlyArray<SilkType.Type>
      readonly arguments: ReadonlyArray<LocalId>
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'MakeEffect'
      readonly destination: LocalId
      readonly runner: DeclarationIndex.CanonicalId
      readonly runnerTypeArguments: ReadonlyArray<SilkType.Type>
      readonly captures: ReadonlyArray<{
        readonly source: LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }>
      readonly type: Extract<Type, { readonly _tag: 'EffectValue' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'MakeCallable'
      readonly destination: LocalId
      readonly target: Hir.CallableTarget
      readonly typeArguments: ReadonlyArray<SilkType.Type>
      readonly captures: ReadonlyArray<{
        readonly ordinal: number
        readonly parameterOrdinal: number
        readonly source: LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }>
      readonly type: Extract<Type, { readonly _tag: 'CallableValue' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ApplyCallable'
      readonly destination: LocalId
      readonly callable?: LocalId
      readonly target?: Hir.CallableTarget
      readonly typeArguments: ReadonlyArray<SilkType.Type>
      readonly captures: ReadonlyArray<{
        readonly ordinal: number
        readonly parameterOrdinal: number
        readonly source: LocalId
        readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
      }>
      readonly arguments: ReadonlyArray<LocalId>
      readonly callableType: SilkType.Callable
      readonly access: SilkType.CallableMode
      readonly evaluation: 'CalleeThenArguments' | 'LeftThenCallable'
      readonly realization: 'Environment' | 'DirectErasedSection'
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'PackEffectOutcome'
      readonly destination: LocalId
      readonly source: LocalId
      readonly tag: number
      readonly type: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'UnpackEffectSuccess'
      readonly destination: LocalId
      readonly source: LocalId
      readonly type: Exclude<Type, { readonly _tag: 'EffectOutcome' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'CatchEffect'
      readonly destination: LocalId
      readonly protectedValue: LocalId
      readonly protectedOutcome: LocalId
      readonly handlerValue: LocalId
      readonly handlerOutcome: LocalId
      readonly protectedTarget?: DeclarationIndex.CanonicalId
      readonly protectedTypeArguments: ReadonlyArray<SilkType.Type>
      readonly protectedArguments: ReadonlyArray<LocalId>
      readonly protectedValueType: Extract<Type, { readonly _tag: 'EffectValue' }>
      readonly protectedRunner: DeclarationIndex.CanonicalId
      readonly protectedRunnerArguments: ReadonlyArray<LocalId>
      readonly protectedType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly handledTag: number
      readonly handledLaneCount: number
      readonly handlerCallable: LocalId
      readonly handlerCallableType: Extract<Type, { readonly _tag: 'CallableValue' }>
      readonly handlerTarget: DeclarationIndex.CanonicalId
      readonly handlerTypeArguments: ReadonlyArray<SilkType.Type>
      readonly handlerValueType: Extract<Type, { readonly _tag: 'EffectValue' }>
      readonly handlerRunner: DeclarationIndex.CanonicalId
      readonly handlerType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly type: Exclude<Type, { readonly _tag: 'EffectOutcome' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RetryEffect'
      readonly destination: LocalId
      readonly protectedValue: LocalId
      readonly protectedOutcome: LocalId
      readonly protectedTarget?: DeclarationIndex.CanonicalId
      readonly protectedTypeArguments: ReadonlyArray<SilkType.Type>
      readonly protectedArguments: ReadonlyArray<LocalId>
      readonly protectedValueType: Extract<Type, { readonly _tag: 'EffectValue' }>
      readonly protectedRunner: DeclarationIndex.CanonicalId
      readonly protectedType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly retries: LocalId
      readonly propagationType?: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly tagMappings: ReadonlyArray<{ readonly source: number; readonly target: number }>
      readonly propagationLaneCount: number
      /** Owners still live at this site, released before a failure outcome propagates. */
      readonly releases?: ReadonlyArray<DropOperation>
      readonly type: Exclude<
        Type,
        { readonly _tag: 'EffectOutcome' | 'EffectValue' | 'EffectBorrow' }
      >
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RunEffect'
      readonly destination: LocalId
      readonly outcome: LocalId
      readonly target: DeclarationIndex.CanonicalId
      readonly typeArguments: ReadonlyArray<SilkType.Type>
      readonly arguments: ReadonlyArray<LocalId>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly propagationType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly tagMappings: ReadonlyArray<{
        readonly source: number
        readonly target: number
      }>
      /** Owners still live at this site, released before a failure outcome propagates. */
      readonly releases?: ReadonlyArray<DropOperation>
      readonly propagationLaneCount: number
      readonly type: Exclude<Type, { readonly _tag: 'EffectOutcome' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RunEffectValue'
      readonly destination: LocalId
      readonly outcome: LocalId
      readonly effect: LocalId
      readonly runner: DeclarationIndex.CanonicalId
      readonly runnerTypeArguments: ReadonlyArray<SilkType.Type>
      /** Statically selected service-provider references appended after the Effect captures. */
      readonly arguments: ReadonlyArray<LocalId>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly propagationType?: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly tagMappings: ReadonlyArray<{
        readonly source: number
        readonly target: number
      }>
      readonly propagationLaneCount: number
      /** Owners still live at this site, released before a failure outcome propagates. */
      readonly releases?: ReadonlyArray<DropOperation>
      readonly type: Exclude<Type, { readonly _tag: 'EffectOutcome' | 'EffectValue' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Construct'
      readonly destination: LocalId
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationIndex.FieldId
        readonly value: LocalId
      }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ConstructArray'
      readonly destination: LocalId
      readonly type: Extract<Type, { readonly _tag: 'FixedArray' }>
      readonly elements: ReadonlyArray<LocalId>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Project'
      readonly destination: LocalId
      readonly source: LocalId
      readonly field: DeclarationIndex.FieldId
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ReadPlace'
      readonly destination: LocalId
      readonly root: LocalId
      readonly selectors: ReadonlyArray<PlaceSelector>
      readonly type: Type
      /** Set when a paired same-place write licenses reading a non-Copy value out. */
      readonly consume?: boolean
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'CheckPlace'
      readonly root: LocalId
      readonly selectors: ReadonlyArray<PlaceSelector>
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'WritePlace'
      readonly root: LocalId
      readonly selectors: ReadonlyArray<PlaceSelector>
      readonly source: LocalId
      readonly rootType: Type
      readonly type: Type
      readonly mutable: true
      readonly replacement: 'Copy' | 'Owned'
      readonly commit: 'AfterCleanup'
      readonly provenance: Provenance
    }
  | DropOperation
  | MatchOperation

/** Releases one owned local through its cleanup plan. */
export interface DropOperation {
  readonly _tag: 'Drop'
  readonly local: LocalId
  readonly cleanup: Ownership.CleanupPlan
  readonly provenance: Provenance
}

export interface MatchBinding {
  readonly id: Match.BindingId
  readonly destination: LocalId
  readonly path: ReadonlyArray<DeclarationIndex.FieldId>
  readonly type: Type
  readonly access: Match.Access
  readonly provenance: Provenance
}

export interface MatchArm {
  readonly id: Match.ArmId
  readonly member?: SilkType.Nominal
  readonly universal: boolean
  readonly before: ReadonlyArray<SilkType.Nominal>
  readonly after: ReadonlyArray<SilkType.Nominal>
  readonly bindings: ReadonlyArray<MatchBinding>
  readonly guard?: {
    readonly operations: ReadonlyArray<Operation>
    readonly result: LocalId
  }
  readonly selected: {
    readonly access: Match.Access
    readonly operations: ReadonlyArray<Operation>
    readonly result: LocalId
    readonly cleanup: ReadonlyArray<{
      readonly path: ReadonlyArray<DeclarationIndex.FieldId>
      readonly cleanup: Ownership.CleanupPlan
    }>
    readonly endBorrow: boolean
  }
  readonly provenance: Provenance
}

/** One compiler-owned structured selection. Child operations form an acyclic expression DAG. */
export interface MatchOperation {
  readonly _tag: 'Match'
  readonly id: Match.MatchId
  readonly destination: LocalId
  readonly scrutinee: LocalId
  readonly scrutineeType:
    | Extract<Type, { readonly _tag: 'Nominal' }>
    | Extract<Type, { readonly _tag: 'Union' }>
  readonly scrutineeShape: Layout.CallingShape
  readonly access: Match.Access
  readonly members: ReadonlyArray<SilkType.Nominal>
  readonly decisions: ReadonlyArray<{
    readonly member: SilkType.Nominal
    readonly candidates: ReadonlyArray<Match.ArmId>
  }>
  readonly arms: ReadonlyArray<MatchArm>
  readonly type: Type
  readonly resultShape: Layout.CallingShape
  readonly provenance: Provenance
}

export type Outcome =
  | { readonly _tag: 'Forward'; readonly target: RegionId; readonly provenance: Provenance }
  | { readonly _tag: 'Return'; readonly value: LocalId; readonly provenance: Provenance }
  | { readonly _tag: 'Trap'; readonly reason: string; readonly provenance: Provenance }
  | { readonly _tag: 'Repeat'; readonly loop: LoopId; readonly provenance: Provenance }
  | { readonly _tag: 'Exit'; readonly loop: LoopId; readonly provenance: Provenance }
  | { readonly _tag: 'Yield'; readonly provenance: Provenance }

interface RegionBase {
  readonly id: RegionId
  readonly ownerLoop?: LoopId
}

export interface OperationRegion extends RegionBase {
  readonly _tag: 'OperationRegion'
  readonly operations: ReadonlyArray<Operation>
  readonly outcome: Outcome
}

export interface CleanupRegion extends RegionBase {
  readonly _tag: 'CleanupRegion'
  readonly releases: ReadonlyArray<Extract<Operation, { readonly _tag: 'Drop' | 'EndLoan' }>>
  readonly outcome: Outcome
}

export interface ConditionalRegion extends RegionBase {
  readonly _tag: 'ConditionalRegion'
  readonly condition: LocalId
  readonly taken: RegionId
  readonly otherwise: RegionId
  readonly following?: RegionId
  readonly provenance: Provenance
}

export interface LoopRegion extends RegionBase {
  readonly _tag: 'LoopRegion'
  readonly loop: LoopId
  readonly parent?: LoopId
  readonly condition: RegionId
  readonly conditionValue: LocalId
  readonly body: RegionId
  readonly following: RegionId
  readonly provenance: Provenance
}

export type Region = OperationRegion | CleanupRegion | ConditionalRegion | LoopRegion

export interface MirFunction {
  readonly _tag: 'MirFunction'
  readonly id: DeclarationIndex.CanonicalId
  readonly instance: Instances.InstanceKey
  readonly parameterCount: number
  readonly localTypes: ReadonlyArray<Type>
  readonly result: Type
  readonly entry: RegionId
  readonly regions: ReadonlyArray<Region>
}

export interface Module {
  readonly _tag: 'MirModule'
  readonly module: string
  readonly layout: Layout.Plan
  readonly functions: ReadonlyArray<MirFunction>
}

/** Tests whether a MIR function realizes one concrete call target. */
export const matchesInstance = (
  fn: MirFunction,
  declaration: DeclarationIndex.CanonicalId,
  typeArguments: ReadonlyArray<SilkType.Type>,
): boolean =>
  fn.id.module === declaration.module &&
  fn.id.name === declaration.name &&
  fn.instance.typeArguments.length === typeArguments.length &&
  fn.instance.typeArguments.every((argument, index) => {
    const expected = typeArguments.at(index)
    return expected !== undefined && SilkType.equals(argument, expected)
  })

export interface ControlEdge {
  readonly _tag: 'ControlEdge'
  readonly from: RegionId
  readonly to: RegionId
  readonly kind: 'Forward' | 'Taken' | 'Otherwise' | 'Following' | 'Condition' | 'Body'
}

const outcomeTarget = (
  outcome: Outcome,
): ReadonlyArray<readonly [RegionId, ControlEdge['kind']]> =>
  outcome._tag === 'Forward' ? [[outcome.target, 'Forward']] : []

const regionTargets = (region: Region): ReadonlyArray<readonly [RegionId, ControlEdge['kind']]> => {
  switch (region._tag) {
    case 'OperationRegion':
    case 'CleanupRegion':
      return outcomeTarget(region.outcome)
    case 'ConditionalRegion':
      return [
        [region.taken, 'Taken'],
        [region.otherwise, 'Otherwise'],
        ...(region.following === undefined ? [] : ([[region.following, 'Following']] as const)),
      ]
    case 'LoopRegion':
      return [
        [region.condition, 'Condition'],
        [region.body, 'Body'],
        [region.following, 'Following'],
      ]
  }
}

export const controlEdges = (self: MirFunction): ReadonlyArray<ControlEdge> =>
  Object.freeze(
    self.regions.flatMap((region) =>
      regionTargets(region).map(([to, kind]) =>
        Object.freeze({ _tag: 'ControlEdge' as const, from: region.id, to, kind }),
      ),
    ),
  )

/** Canonical parent-before-child traversal over structural edges only. */
export const topologicalRegions = (self: MirFunction): ReadonlyArray<Region> => {
  const byId = new Map(self.regions.map((region) => [region.id.ordinal, region] as const))
  const visited = new Set<number>()
  const ordered: Array<Region> = []
  const visit = (id: RegionId): void => {
    if (visited.has(id.ordinal)) return
    visited.add(id.ordinal)
    const region = byId.get(id.ordinal)
    if (region === undefined) return
    ordered.push(region)
    for (const [target] of regionTargets(region)) visit(target)
  }
  visit(self.entry)
  for (const region of [...self.regions].sort(
    (left, right) => left.id.ordinal - right.id.ordinal,
  )) {
    visit(region.id)
  }
  return Object.freeze(ordered)
}

export interface Violation {
  readonly _tag: 'Violation'
  readonly rule:
    | 'InvalidLayout'
    | 'InvalidInstance'
    | 'MissingTypeLayout'
    | 'MissingEntryRegion'
    | 'DuplicateRegionIdentity'
    | 'UnknownRegionTarget'
    | 'StructuralCycle'
    | 'InvalidLexicalOwner'
    | 'InvalidLoopTarget'
    | 'UndeclaredLocal'
    | 'InvalidAggregateOperation'
    | 'InvalidIntegerOperation'
    | 'InvalidLayoutOperation'
    | 'InvalidAllocationOperation'
    | 'InvalidRawStorageOperation'
    | 'InvalidCallShape'
    | 'InvalidCallableOperation'
    | 'InvalidEffectOperation'
    | 'InvalidWrite'
    | 'InvalidLoan'
    | 'InvalidSliceOperation'
    | 'InvalidMatchLayout'
    | 'InvalidMatchDecision'
    | 'InvalidMatchBinding'
    | 'InvalidMatchGuard'
    | 'InvalidMatchOwnership'
    | 'InvalidMatchJoin'
    | 'CyclicMatchOperation'
  readonly function?: DeclarationIndex.CanonicalId
  readonly region?: RegionId
  readonly detail: string
}

const operationsOf = (region: Region): ReadonlyArray<Operation> =>
  region._tag === 'OperationRegion'
    ? region.operations
    : region._tag === 'CleanupRegion'
      ? region.releases
      : []

const operationChildren = (operation: Operation): ReadonlyArray<Operation> =>
  operation._tag === 'Match'
    ? operation.arms.flatMap((arm) => [
        ...(arm.guard?.operations ?? []),
        ...arm.selected.operations,
      ])
    : operation._tag === 'RunEffect' ||
        operation._tag === 'RunEffectValue' ||
        operation._tag === 'RetryEffect'
      ? (operation.releases ?? [])
      : []

/** One operation and all structurally nested operations in deterministic source order. */
export const operationTree = (operation: Operation): ReadonlyArray<Operation> => {
  const seen = new Set<Operation>()
  const walk = (current: Operation): ReadonlyArray<Operation> => {
    if (seen.has(current)) return []
    seen.add(current)
    return [current, ...operationChildren(current).flatMap(walk)]
  }
  return Object.freeze(walk(operation))
}

const cyclicOperation = (operation: Operation): boolean => {
  const active = new Set<Operation>()
  const completed = new Set<Operation>()
  const walk = (current: Operation): boolean => {
    if (active.has(current)) return true
    if (completed.has(current)) return false
    active.add(current)
    const cyclic = operationChildren(current).some(walk)
    active.delete(current)
    completed.add(current)
    return cyclic
  }
  return walk(operation)
}

/** Source-stable operations across canonical topological region order. */
export const operations = (self: MirFunction): ReadonlyArray<Operation> =>
  Object.freeze(
    topologicalRegions(self).flatMap((region) => operationsOf(region).flatMap(operationTree)),
  )

export const outcomes = (self: MirFunction): ReadonlyArray<Outcome> =>
  Object.freeze(topologicalRegions(self).flatMap((region) => outcomeOf(region) ?? []))

const outcomeOf = (region: Region): Outcome | undefined =>
  region._tag === 'OperationRegion' || region._tag === 'CleanupRegion' ? region.outcome : undefined

const operationLocals = (operation: Operation): ReadonlyArray<LocalId> => {
  switch (operation._tag) {
    case 'Literal':
      return [operation.destination]
    case 'Binary':
      return [operation.destination, operation.left, operation.right]
    case 'ConvertInteger':
      return [operation.destination, operation.source]
    case 'CheckedInteger':
      return [operation.destination, ...operation.operands]
    case 'ValidateLayout':
      return [operation.destination, operation.bytes, operation.alignment]
    case 'RepeatLayout':
      return [operation.destination, operation.layout, operation.count]
    case 'Allocate':
      return [operation.destination, operation.layout]
    case 'RawBufferFrom':
      return [operation.destination, operation.allocation, operation.count]
    case 'RawBufferCount':
      return [operation.destination, operation.buffer]
    case 'RawBufferSlot':
      return [operation.destination, operation.buffer, operation.index]
    case 'SlotWrite':
      return [operation.destination, operation.slot, operation.value]
    case 'SlotTake':
    case 'SlotCopy':
      return [operation.destination, operation.slot]
    case 'SlotDrop':
      return [operation.destination, operation.slot]
    case 'Move':
      return [operation.destination, operation.source]
    case 'BeginLoan':
      return [operation.destination, operation.root]
    case 'EndLoan':
      return [operation.slice]
    case 'SliceLength':
      return [operation.destination, operation.slice]
    case 'ConvertUnion':
      return [operation.destination, operation.source]
    case 'Call':
      return [operation.destination, ...operation.arguments]
    case 'MakeEffect':
      return [operation.destination, ...operation.captures.map((capture) => capture.source)]
    case 'MakeCallable':
      return [operation.destination, ...operation.captures.map((capture) => capture.source)]
    case 'ApplyCallable':
      return [
        operation.destination,
        ...(operation.callable === undefined ? [] : [operation.callable]),
        ...operation.captures.map((capture) => capture.source),
        ...operation.arguments,
      ]
    case 'PackEffectOutcome':
    case 'UnpackEffectSuccess':
      return [operation.destination, operation.source]
    case 'CatchEffect':
      return [
        operation.destination,
        operation.protectedValue,
        operation.protectedOutcome,
        operation.handlerValue,
        operation.handlerOutcome,
        ...operation.protectedArguments,
        ...operation.protectedRunnerArguments,
      ]
    case 'RetryEffect':
      return [
        operation.destination,
        operation.protectedValue,
        operation.protectedOutcome,
        operation.retries,
        ...operation.protectedArguments,
      ]
    case 'RunEffect':
      return [operation.destination, operation.outcome, ...operation.arguments]
    case 'RunEffectValue':
      return [operation.destination, operation.outcome, operation.effect, ...operation.arguments]
    case 'Construct':
      return [operation.destination, ...operation.fields.map((field) => field.value)]
    case 'ConstructArray':
      return [operation.destination, ...operation.elements]
    case 'Project':
      return [operation.destination, operation.source]
    case 'ReadPlace':
      return [operation.destination, operation.root, ...selectorLocals(operation.selectors)]
    case 'CheckPlace':
      return [operation.root, ...selectorLocals(operation.selectors)]
    case 'WritePlace':
      return [operation.root, operation.source, ...selectorLocals(operation.selectors)]
    case 'Drop':
      return [operation.local]
    case 'Match':
      return [
        operation.destination,
        operation.scrutinee,
        ...operation.arms.flatMap((arm) => [
          ...arm.bindings.map((binding) => binding.destination),
          ...(arm.guard === undefined ? [] : [arm.guard.result]),
          arm.selected.result,
        ]),
      ]
  }
}

const localUses = (region: Region): ReadonlyArray<LocalId> => [
  ...operationsOf(region).flatMap(operationLocals),
  ...(region._tag === 'ConditionalRegion' ? [region.condition] : []),
  ...(region._tag === 'LoopRegion' ? [region.conditionValue] : []),
  ...(outcomeOf(region)?._tag === 'Return'
    ? [(outcomeOf(region) as Extract<Outcome, { readonly _tag: 'Return' }>).value]
    : []),
]

const selectorLocals = (selectors: ReadonlyArray<PlaceSelector>): ReadonlyArray<LocalId> =>
  selectors.flatMap((selector) =>
    selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'
      ? [selector.index.local]
      : selector._tag === 'SliceElementSelector'
        ? [selector.index]
        : [],
  )

const placeType = (
  fn: MirFunction,
  layout: Layout.Plan,
  root: LocalId,
  selectors: ReadonlyArray<PlaceSelector>,
): DeclarationIndex.SemanticType | undefined => {
  const rootType = fn.localTypes.at(root.ordinal)
  let current = rootType === undefined ? undefined : semanticType(rootType)
  // A reference root reads and writes through the borrow, so the place is on its target.
  if (current !== undefined && SilkType.isReference(current) && selectors.length > 0) {
    current = current.target
  }
  for (const selector of selectors) {
    if (selector._tag === 'FieldSelector') {
      const entry =
        current !== undefined && SilkType.isNominal(current)
          ? Layout.entry(layout, current)
          : undefined
      const field =
        entry?.representation._tag === 'Aggregate'
          ? entry.representation.fields.find(
              (candidate) =>
                candidate.id.ordinal === selector.field.ordinal &&
                candidate.id.struct.sourceId === selector.field.struct.sourceId &&
                candidate.id.struct.ordinal === selector.field.struct.ordinal,
            )
          : undefined
      current = field?.type
      continue
    }
    if (selector._tag === 'SliceElementSelector') {
      if (
        current === undefined ||
        !SilkType.isSlice(current) ||
        current.access !== selector.access ||
        fn.localTypes.at(selector.index.ordinal)?._tag !== 'usize'
      ) {
        return undefined
      }
      current = current.element
      continue
    }
    if (
      current === undefined ||
      !SilkType.isFixedArray(current) ||
      current.length !== selector.length
    ) {
      return undefined
    }
    if (selector.index._tag === 'Proven') {
      if (selector.index.value < 0 || selector.index.value >= selector.length) return undefined
    } else if (fn.localTypes.at(selector.index.local.ordinal)?._tag !== 'usize') return undefined
    current = current.element
  }
  return current
}

const fieldPathType = (
  layout: Layout.Plan,
  root: DeclarationIndex.SemanticType,
  path: ReadonlyArray<DeclarationIndex.FieldId>,
): DeclarationIndex.SemanticType | undefined => {
  let current: DeclarationIndex.SemanticType | undefined = root
  for (const selector of path) {
    const entry: Layout.Entry | undefined = SilkType.isNominal(current)
      ? Layout.entry(layout, current)
      : undefined
    const field: Layout.Field | undefined =
      entry?.representation._tag === 'Aggregate'
        ? entry.representation.fields.find(
            (candidate) =>
              candidate.id.ordinal === selector.ordinal &&
              candidate.id.struct.sourceId === selector.struct.sourceId &&
              candidate.id.struct.ordinal === selector.struct.ordinal,
          )
        : undefined
    current = field?.type
    if (current === undefined) return undefined
  }
  return current
}

const sameMembers = (
  left: ReadonlyArray<SilkType.Nominal>,
  right: ReadonlyArray<SilkType.Nominal>,
): boolean =>
  left.length === right.length &&
  left.every((member, ordinal) => {
    const candidate = right.at(ordinal)
    return candidate !== undefined && SilkType.equals(member, candidate)
  })

const targetText = (target: DeclarationIndex.CanonicalId): string =>
  `${target.module}.${target.name}`

const callableTargetText = (target: Hir.CallableTarget): string =>
  target._tag === 'DeclarationCallableTarget'
    ? targetText(target.declaration)
    : `${target.actor}.${target.operation}`

const sameCallableTarget = (left: Hir.CallableTarget, right: Hir.CallableTarget): boolean =>
  left._tag === right._tag &&
  (left._tag === 'DeclarationCallableTarget' && right._tag === 'DeclarationCallableTarget'
    ? left.declaration.module === right.declaration.module &&
      left.declaration.name === right.declaration.name
    : left._tag === 'BuiltinCallableTarget' && right._tag === 'BuiltinCallableTarget'
      ? left.actor === right.actor && left.operation === right.operation
      : false)

const borrowKey = (borrow: Hir.BorrowId): string =>
  `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`

const instanceText = (self: Instances.InstanceKey): string =>
  `${self.declaration.module}\u0000${self.declaration.name}\u0000${self.typeArguments
    .map(SilkType.key)
    .join('\u0000')}\u0000${self.contractRow.join('\u0000')}`

const cleanupTypes = (cleanup: Ownership.CleanupPlan): ReadonlyArray<SilkType.Type> => {
  switch (cleanup._tag) {
    case 'NoCleanup':
    case 'ParameterCleanup':
    case 'AllocationCleanup':
      return [cleanup.type]
    case 'RawBufferCleanup':
      return [cleanup.type, ...cleanupTypes(cleanup.allocation)]
    case 'HookCleanup':
      return [cleanup.type, ...cleanupTypes(cleanup.inner)]
    case 'StructCleanup':
      return [cleanup.type, ...cleanup.fields.flatMap((field) => cleanupTypes(field.cleanup))]
    case 'ArrayCleanup':
      return [cleanup.type, ...cleanupTypes(cleanup.element)]
    case 'UnionCleanup':
      return [cleanup.type, ...cleanup.cases.flatMap((entry) => cleanupTypes(entry.cleanup))]
    case 'CallableCleanup':
      return [cleanup.type, ...cleanup.slots.flatMap((slot) => cleanupTypes(slot.cleanup))]
  }
}

const operationTypes = (operation: Operation): ReadonlyArray<DeclarationIndex.SemanticType> => {
  switch (operation._tag) {
    case 'Literal':
    case 'Binary':
    case 'ValidateLayout':
    case 'RepeatLayout':
    case 'Allocate':
    case 'Project':
    case 'ReadPlace':
    case 'CheckPlace':
      return [semanticType(operation.type)]
    case 'ConvertInteger':
      return [semanticType(operation.sourceType), semanticType(operation.type)]
    case 'CheckedInteger':
      return [
        semanticType(operation.sourceType),
        semanticType(operation.valueType),
        semanticType(operation.type),
        operation.success,
        operation.failure,
      ]
    case 'RawBufferFrom':
      return [semanticType(operation.type), operation.element]
    case 'RawBufferCount':
      return [semanticType(operation.type)]
    case 'RawBufferSlot':
    case 'SlotWrite':
    case 'SlotTake':
    case 'SlotCopy':
      return [semanticType(operation.type), operation.element]
    case 'SlotDrop':
      return [semanticType(operation.type), operation.element, ...cleanupTypes(operation.cleanup)]
    case 'BeginLoan':
      return [semanticType(operation.sourceType), semanticType(operation.type)]
    case 'SliceLength':
      return [semanticType(operation.type)]
    case 'EndLoan':
      return []
    case 'Call':
      return [semanticType(operation.type), ...operation.typeArguments]
    case 'MakeEffect':
      return [semanticType(operation.type), ...operation.runnerTypeArguments]
    case 'MakeCallable':
      return [semanticType(operation.type), ...operation.typeArguments]
    case 'ApplyCallable':
      return [operation.callableType, semanticType(operation.type), ...operation.typeArguments]
    case 'PackEffectOutcome':
    case 'UnpackEffectSuccess':
      return [semanticType(operation.type)]
    case 'CatchEffect':
      return [
        semanticType(operation.protectedValueType),
        semanticType(operation.protectedType),
        semanticType(operation.handlerValueType),
        semanticType(operation.handlerType),
        semanticType(operation.type),
        ...operation.protectedTypeArguments,
      ]
    case 'RetryEffect':
      return [
        semanticType(operation.protectedValueType),
        semanticType(operation.protectedType),
        ...(operation.propagationType === undefined
          ? []
          : [semanticType(operation.propagationType)]),
        semanticType(operation.type),
        ...operation.protectedTypeArguments,
      ]
    case 'RunEffect':
      return [
        semanticType(operation.outcomeType),
        semanticType(operation.propagationType),
        semanticType(operation.type),
        ...operation.typeArguments,
      ]
    case 'RunEffectValue':
      return [
        semanticType(operation.outcomeType),
        ...(operation.propagationType === undefined
          ? []
          : [semanticType(operation.propagationType)]),
        semanticType(operation.type),
        ...operation.runnerTypeArguments,
      ]
    case 'Construct':
    case 'ConstructArray':
      return [semanticType(operation.type)]
    case 'ConvertUnion':
      return [semanticType(operation.sourceType), semanticType(operation.targetType)]
    case 'WritePlace':
      return [semanticType(operation.rootType), semanticType(operation.type)]
    case 'Match':
      return [
        semanticType(operation.scrutineeType),
        semanticType(operation.type),
        ...operation.members,
        ...operation.arms.flatMap((arm) => [
          ...(arm.member === undefined ? [] : [arm.member]),
          ...arm.before,
          ...arm.after,
          ...arm.bindings.map((binding) => semanticType(binding.type)),
          ...(arm.guard?.operations.flatMap(operationTypes) ?? []),
          ...arm.selected.operations.flatMap(operationTypes),
          ...arm.selected.cleanup.flatMap((entry) => cleanupTypes(entry.cleanup)),
        ]),
      ]
    case 'Move':
      return []
    case 'Drop':
      return cleanupTypes(operation.cleanup)
  }
}

interface ActiveLoan {
  readonly operation: Extract<Operation, { readonly _tag: 'BeginLoan' }>
  readonly root: LocalId
  readonly parent?: string
}

const accessedOwnerLocals = (operation: Operation): ReadonlyArray<LocalId> => {
  switch (operation._tag) {
    case 'Binary':
      return [operation.left, operation.right]
    case 'ConvertInteger':
      return [operation.source]
    case 'CheckedInteger':
      return operation.operands
    case 'ValidateLayout':
      return [operation.bytes, operation.alignment]
    case 'RepeatLayout':
      return [operation.layout, operation.count]
    case 'Allocate':
      return [operation.layout]
    case 'RawBufferFrom':
      return [operation.allocation, operation.count]
    case 'RawBufferCount':
      return [operation.buffer]
    case 'RawBufferSlot':
      return [operation.buffer, operation.index]
    case 'SlotWrite':
      return [operation.slot, operation.value]
    case 'SlotTake':
    case 'SlotCopy':
    case 'SlotDrop':
      return [operation.slot]
    case 'Move':
      return [operation.source]
    case 'ConvertUnion':
      return [operation.source]
    case 'Call':
      return operation.arguments
    case 'MakeEffect':
      return operation.captures.map((capture) => capture.source)
    case 'MakeCallable':
      return operation.captures.map((capture) => capture.source)
    case 'ApplyCallable':
      return [
        ...(operation.callable === undefined ? [] : [operation.callable]),
        ...operation.captures.map((capture) => capture.source),
        ...operation.arguments,
      ]
    case 'PackEffectOutcome':
    case 'UnpackEffectSuccess':
      return [operation.source]
    case 'CatchEffect':
      return operation.protectedArguments
    case 'RetryEffect':
      return [operation.retries, ...operation.protectedArguments]
    case 'RunEffect':
      return operation.arguments
    case 'RunEffectValue':
      return [operation.effect, ...operation.arguments]
    case 'Construct':
      return operation.fields.map((field) => field.value)
    case 'ConstructArray':
      return operation.elements
    case 'Project':
      return [operation.source]
    case 'ReadPlace':
    case 'CheckPlace':
    case 'WritePlace':
      return [operation.root]
    case 'Drop':
      return [operation.local]
    case 'Match':
      return [operation.scrutinee]
    case 'Literal':
    case 'BeginLoan':
    case 'EndLoan':
    case 'SliceLength':
      return []
  }
}

const loanViolations = (
  fn: MirFunction,
  region: Region,
  roots: ReadonlyArray<Operation>,
  globalBeginnings: ReadonlyMap<string, Extract<Operation, { readonly _tag: 'BeginLoan' }>>,
  globalEndings: ReadonlySet<string>,
): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = []
  const invalid = (detail: string): void => {
    violations.push(
      Object.freeze({
        _tag: 'Violation',
        rule: 'InvalidLoan',
        function: fn.id,
        region: region.id,
        detail,
      }),
    )
  }
  const process = (
    sequence: ReadonlyArray<Operation>,
    inherited: ReadonlyMap<string, ActiveLoan>,
  ): void => {
    const active = new Map(inherited)
    const inheritedKeys = new Set(inherited.keys())
    const completed = new Set<string>()
    const calls = new Set<string>()
    for (const operation of sequence) {
      if (operation._tag === 'BeginLoan') {
        const key = borrowKey(operation.borrow)
        const source = fn.localTypes.at(operation.root.ordinal)
        const destination = fn.localTypes.at(operation.destination.ordinal)
        const sourceSemantic = semanticType(operation.sourceType)
        const borrowed = operation.type.type
        const sourceElement =
          operation.sourceType._tag === 'FixedArray' || operation.sourceType._tag === 'Slice'
            ? operation.sourceType.type.element
            : undefined
        const parent = [...active.entries()].find(
          ([, loan]) => loan.operation.destination.ordinal === operation.root.ordinal,
        )
        const reborrowValid =
          operation.sourceType._tag === 'Slice'
            ? operation.reborrow &&
              operation.suspendsParent === (operation.sourceType.type.access === 'Exclusive')
            : !operation.reborrow && !operation.suspendsParent
        const parentValid =
          parent === undefined ||
          (operation.sourceType._tag === 'Slice' &&
            parent[1].operation.access === operation.sourceType.type.access &&
            operation.suspendsParent === (parent[1].operation.access === 'Exclusive'))
        if (
          active.has(key) ||
          completed.has(key) ||
          source === undefined ||
          destination === undefined ||
          !SilkType.equals(semanticType(source), sourceSemantic) ||
          (destination._tag !== 'Slice' && destination._tag !== 'Reference') ||
          !SilkType.equals(destination.type, borrowed) ||
          borrowed.access !== operation.access ||
          (SilkType.isSlice(borrowed)
            ? sourceElement === undefined || !SilkType.equals(borrowed.element, sourceElement)
            : !SilkType.equals(borrowed.target, sourceSemantic)) ||
          (operation.sourceType._tag === 'Slice' &&
            operation.sourceType.type.access === 'Shared' &&
            operation.access === 'Exclusive') ||
          !reborrowValid ||
          !parentValid
        ) {
          invalid(`loan ${key} has inconsistent root, slice type, access, or reborrow facts`)
        }
        const root = parent?.[1].root ?? operation.root
        const conflicts = [...active.entries()].some(([candidateKey, candidate]) => {
          if (candidate.root.ordinal !== root.ordinal) return false
          if (parent?.[0] === candidateKey && operation.suspendsParent) return false
          return candidate.operation.access === 'Exclusive' || operation.access === 'Exclusive'
        })
        if (conflicts) invalid(`loan ${key} conflicts with an active loan of %${root.ordinal}`)
        active.set(
          key,
          Object.freeze({
            operation,
            root,
            ...(parent === undefined ? {} : { parent: parent[0] }),
          }),
        )
        calls.add(`${operation.borrow.callSpan.start}:${operation.borrow.callSpan.end}`)
        continue
      }
      if (operation._tag === 'Call') {
        calls.add(`${operation.provenance.span.start}:${operation.provenance.span.end}`)
      }
      if (operation._tag === 'EndLoan') {
        const key = borrowKey(operation.borrow)
        const loan = active.get(key)
        const beginning = loan?.operation ?? globalBeginnings.get(key)
        const call = `${operation.borrow.callSpan.start}:${operation.borrow.callSpan.end}`
        const liveChild = [...active.values()].some((candidate) => candidate.parent === key)
        if (
          beginning === undefined ||
          completed.has(key) ||
          beginning.destination.ordinal !== operation.slice.ordinal ||
          (loan !== undefined && !calls.has(call)) ||
          liveChild
        ) {
          invalid(`loan ${key} has a missing, duplicate, premature, or mismatched ending`)
        } else {
          active.delete(key)
          completed.add(key)
        }
        continue
      }

      for (const local of accessedOwnerLocals(operation)) {
        const loan = [...active.values()].find(
          (candidate) => candidate.root.ordinal === local.ordinal,
        )
        if (loan !== undefined) {
          invalid(
            `${operation._tag} accesses owner %${local.ordinal} while loan ${borrowKey(loan.operation.borrow)} is live`,
          )
        }
        const suspended = [...active.values()].find(
          (candidate) =>
            candidate.parent !== undefined &&
            active.get(candidate.parent)?.operation.destination.ordinal === local.ordinal,
        )
        if (suspended !== undefined) {
          invalid(`${operation._tag} accesses a suspended parent slice %${local.ordinal}`)
        }
      }
      if (operation._tag === 'SliceLength') {
        const suspended = [...active.values()].some(
          (candidate) =>
            candidate.parent !== undefined &&
            active.get(candidate.parent)?.operation.destination.ordinal === operation.slice.ordinal,
        )
        if (suspended)
          invalid(`SliceLength accesses suspended parent slice %${operation.slice.ordinal}`)
      }
      if (operation._tag === 'Match') {
        for (const arm of operation.arms) {
          if (arm.guard !== undefined) process(arm.guard.operations, active)
          process(arm.selected.operations, active)
        }
      }
    }
    for (const [key] of active) {
      if (!inheritedKeys.has(key) && !globalEndings.has(key)) {
        invalid(`loan ${key} has no ending in its operation sequence`)
      }
    }
  }
  process(roots, new Map())
  return Object.freeze(violations)
}

export const verify = (self: Module): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = Layout.verify(self.layout).map((violation) =>
    Object.freeze({
      _tag: 'Violation' as const,
      rule: 'InvalidLayout' as const,
      detail: `${violation.rule}: ${violation.detail}`,
    }),
  )
  const instanceKeys = new Set<string>()
  for (const fn of self.functions) {
    const currentInstance = instanceText(fn.instance)
    const concreteTypes = [
      ...fn.instance.typeArguments,
      ...fn.localTypes.map(semanticType),
      semanticType(fn.result),
      ...fn.regions.flatMap(operationsOf).flatMap(operationTree).flatMap(operationTypes),
    ]
    if (
      fn.instance.declaration.module !== fn.id.module ||
      fn.instance.declaration.name !== fn.id.name ||
      concreteTypes.some((type) => !SilkType.isConcrete(type)) ||
      instanceKeys.has(currentInstance)
    ) {
      violations.push(
        Object.freeze({
          _tag: 'Violation',
          rule: 'InvalidInstance',
          function: fn.id,
          detail: instanceKeys.has(currentInstance)
            ? 'function repeats an existing concrete instance key'
            : 'function instance identity is inconsistent or retains an open type parameter',
        }),
      )
    }
    instanceKeys.add(currentInstance)
    const missingTypes = new Set(
      [...fn.localTypes, fn.result]
        .filter((type) => type._tag !== 'CallableValue')
        .map(semanticType)
        .filter(
          (type) =>
            Layout.entry(self.layout, type) === undefined &&
            Layout.callingShape(self.layout, type) === undefined,
        )
        .map(SilkType.key),
    )
    for (const type of [...missingTypes].sort()) {
      violations.push(
        Object.freeze({
          _tag: 'Violation',
          rule: 'MissingTypeLayout',
          function: fn.id,
          detail: `function references ${type} without a layout entry`,
        }),
      )
    }

    const byId = new Map<number, Region>()
    for (const region of fn.regions) {
      if (byId.has(region.id.ordinal)) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'DuplicateRegionIdentity',
            function: fn.id,
            region: region.id,
            detail: `region r${region.id.ordinal} is declared more than once`,
          }),
        )
      } else byId.set(region.id.ordinal, region)
    }
    if (!byId.has(fn.entry.ordinal)) {
      violations.push(
        Object.freeze({
          _tag: 'Violation',
          rule: 'MissingEntryRegion',
          function: fn.id,
          detail: `entry region r${fn.entry.ordinal} is missing`,
        }),
      )
    }
    for (const region of fn.regions) {
      for (const [target] of regionTargets(region)) {
        if (!byId.has(target.ordinal)) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'UnknownRegionTarget',
              function: fn.id,
              region: region.id,
              detail: `region references missing r${target.ordinal}`,
            }),
          )
        }
      }
    }

    const color = new Map<number, 0 | 1 | 2>()
    const visit = (region: Region): void => {
      color.set(region.id.ordinal, 1)
      for (const [target] of regionTargets(region)) {
        const targetRegion = byId.get(target.ordinal)
        if (targetRegion === undefined) continue
        if (color.get(target.ordinal) === 1) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'StructuralCycle',
              function: fn.id,
              region: region.id,
              detail: `structural edge r${region.id.ordinal} -> r${target.ordinal} forms a cycle`,
            }),
          )
        } else if (color.get(target.ordinal) !== 2) visit(targetRegion)
      }
      color.set(region.id.ordinal, 2)
    }
    for (const region of [...fn.regions].sort((a, b) => a.id.ordinal - b.id.ordinal)) {
      if (color.get(region.id.ordinal) === undefined) visit(region)
    }

    const loops = new Map<number, LoopRegion>()
    for (const region of fn.regions) {
      if (region._tag === 'LoopRegion') loops.set(region.loop.ordinal, region)
    }
    const allOperations = fn.regions.flatMap(operationsOf).flatMap(operationTree)
    const loanBeginnings = new Map<string, number>()
    const loanEndings = new Map<string, number>()
    const globalBeginnings = new Map<string, Extract<Operation, { readonly _tag: 'BeginLoan' }>>()
    for (const operation of allOperations) {
      if (operation._tag === 'BeginLoan') {
        const key = borrowKey(operation.borrow)
        loanBeginnings.set(key, (loanBeginnings.get(key) ?? 0) + 1)
        globalBeginnings.set(key, operation)
      } else if (operation._tag === 'EndLoan') {
        const key = borrowKey(operation.borrow)
        loanEndings.set(key, (loanEndings.get(key) ?? 0) + 1)
      }
    }
    const globalEndings = new Set(loanEndings.keys())
    for (const key of new Set([...loanBeginnings.keys(), ...loanEndings.keys()])) {
      if (loanBeginnings.get(key) !== 1 || loanEndings.get(key) !== 1) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLoan',
            function: fn.id,
            detail: `loan ${key} must have exactly one beginning and one ending`,
          }),
        )
      }
    }
    const isAncestor = (owner: LoopId | undefined, target: LoopId): boolean => {
      let current = owner
      const seen = new Set<number>()
      while (current !== undefined && !seen.has(current.ordinal)) {
        if (current.ordinal === target.ordinal) return true
        seen.add(current.ordinal)
        current = loops.get(current.ordinal)?.parent
      }
      return false
    }
    for (const region of fn.regions) {
      violations.push(
        ...loanViolations(fn, region, operationsOf(region), globalBeginnings, globalEndings),
      )
      if (region.ownerLoop !== undefined && !loops.has(region.ownerLoop.ordinal)) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLexicalOwner',
            function: fn.id,
            region: region.id,
            detail: `owner loop loop${region.ownerLoop.ordinal} is missing`,
          }),
        )
      }
      const outcome = outcomeOf(region)
      if (
        (outcome?._tag === 'Repeat' || outcome?._tag === 'Exit') &&
        !isAncestor(region.ownerLoop, outcome.loop)
      ) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLoopTarget',
            function: fn.id,
            region: region.id,
            detail: `${outcome._tag.toLowerCase()} targets non-ancestor loop${outcome.loop.ordinal}`,
          }),
        )
      }
      for (const used of localUses(region)) {
        if (used.ordinal < 0 || used.ordinal >= fn.localTypes.length) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'UndeclaredLocal',
              function: fn.id,
              region: region.id,
              detail: `references undeclared local %${used.ordinal}`,
            }),
          )
        }
      }
      for (const rootOperation of operationsOf(region)) {
        if (cyclicOperation(rootOperation)) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'CyclicMatchOperation',
              function: fn.id,
              region: region.id,
              detail: 'nested match operations contain a structural cycle',
            }),
          )
        }
      }
      const operations = operationsOf(region).flatMap(operationTree)
      for (const [index, operation] of operations.entries()) {
        if (operation._tag === 'Literal') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const semantic = semanticType(operation.type)
          const value = BigInt(operation.value)
          const scalar = typeof semantic === 'string' ? Scalar.find(semantic) : undefined
          const pointerBits = self.layout.target.pointerSize === 4 ? 32 : 64
          const validValue =
            scalar?.category === 'Integer'
              ? (() => {
                  const range = Scalar.range(scalar, pointerBits)
                  return value >= range.minimum && value <= range.maximum
                })()
              : scalar?.category === 'Boolean'
                ? value === 0n || value === 1n
                : false
          if (
            destination === undefined ||
            !SilkType.equals(semanticType(destination), semantic) ||
            !validValue
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidIntegerOperation',
                function: fn.id,
                region: region.id,
                detail: `literal ${operation.value.toString()} disagrees with its destination or target range`,
              }),
            )
          }
        }
        if (operation._tag === 'Binary') {
          const left = fn.localTypes.at(operation.left.ordinal)
          const right = fn.localTypes.at(operation.right.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const operand = left === undefined ? undefined : semanticType(left)
          const comparison =
            operation.operator === 'Equals' ||
            operation.operator === 'NotEquals' ||
            operation.operator === 'LessThan' ||
            operation.operator === 'LessOrEqual' ||
            operation.operator === 'GreaterThan' ||
            operation.operator === 'GreaterOrEqual'
          const scalar = typeof operand === 'string' ? Scalar.find(operand) : undefined
          const supportsOperation =
            scalar?.category === 'Integer' ||
            (scalar?.category === 'Boolean' &&
              (operation.operator === 'Equals' || operation.operator === 'NotEquals'))
          const expectedResult = comparison ? 'bool' : operand
          if (
            operand === undefined ||
            right === undefined ||
            destination === undefined ||
            !SilkType.equals(semanticType(right), operand) ||
            !supportsOperation ||
            expectedResult === undefined ||
            !SilkType.equals(semanticType(operation.type), expectedResult) ||
            !SilkType.equals(semanticType(destination), expectedResult)
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidIntegerOperation',
                function: fn.id,
                region: region.id,
                detail: `${operation.operator} has inconsistent operand or result types`,
              }),
            )
          }
        }
        if (operation._tag === 'ConvertInteger') {
          const source = fn.localTypes.at(operation.source.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const sourceScalar = Scalar.find(operation.sourceType._tag)
          const targetScalar = Scalar.find(operation.type._tag)
          if (
            sourceScalar?.category !== 'Integer' ||
            targetScalar?.category !== 'Integer' ||
            source === undefined ||
            destination === undefined ||
            !SilkType.equals(semanticType(source), operation.sourceType._tag) ||
            !SilkType.equals(semanticType(destination), operation.type._tag)
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidIntegerOperation',
                function: fn.id,
                region: region.id,
                detail: 'integer conversion has inconsistent source or destination types',
              }),
            )
        }
        if (operation._tag === 'CheckedInteger') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const operands = operation.operands.map((operand) => fn.localTypes.at(operand.ordinal))
          const sourceScalar = Scalar.find(operation.sourceType._tag)
          const valueScalar = Scalar.find(operation.valueType._tag)
          if (
            sourceScalar?.category !== 'Integer' ||
            valueScalar?.category !== 'Integer' ||
            destination === undefined ||
            operands.length < 1 ||
            operands.some(
              (operand) =>
                operand === undefined ||
                !SilkType.equals(semanticType(operand), operation.sourceType._tag),
            ) ||
            !SilkType.equals(semanticType(destination), operation.type.type) ||
            !operation.type.type.members.some((member) =>
              SilkType.equals(member, operation.success),
            ) ||
            !operation.type.type.members.some((member) =>
              SilkType.equals(member, operation.failure),
            )
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidIntegerOperation',
                function: fn.id,
                region: region.id,
                detail: 'checked integer operation has inconsistent operands or Option result',
              }),
            )
        }
        if (operation._tag === 'ValidateLayout' || operation._tag === 'RepeatLayout') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const left = fn.localTypes.at(
            operation._tag === 'ValidateLayout'
              ? operation.bytes.ordinal
              : operation.layout.ordinal,
          )
          const right = fn.localTypes.at(
            operation._tag === 'ValidateLayout'
              ? operation.alignment.ordinal
              : operation.count.ordinal,
          )
          const expectedMembers = [
            SilkType.layout,
            operation._tag === 'ValidateLayout'
              ? SilkType.invalidAlignment
              : SilkType.layoutOverflow,
          ].sort(SilkType.compare)
          const validLeft =
            operation._tag === 'ValidateLayout'
              ? left?._tag === 'usize'
              : left?._tag === 'Nominal' && SilkType.equals(left.type, SilkType.layout)
          if (
            !validLeft ||
            right?._tag !== 'usize' ||
            destination?._tag !== 'Union' ||
            !SilkType.equals(destination.type, operation.type.type) ||
            !sameMembers(operation.type.type.members, expectedMembers)
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidLayoutOperation',
                function: fn.id,
                region: region.id,
                detail: `${operation._tag} has inconsistent operands or validation result`,
              }),
            )
          }
        }
        if (operation._tag === 'Allocate') {
          const layout = fn.localTypes.at(operation.layout.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const expectedFailure = operation.propagationType.type.failures.at(
            operation.failureTag - 1,
          )
          if (
            layout?._tag !== 'Nominal' ||
            !SilkType.equals(layout.type, SilkType.layout) ||
            destination?._tag !== 'Nominal' ||
            !SilkType.equals(destination.type, SilkType.allocation) ||
            !SilkType.equals(operation.type.type, SilkType.allocation) ||
            !SilkType.equals(operation.failure, SilkType.outOfMemory) ||
            expectedFailure === undefined ||
            !SilkType.equals(expectedFailure, operation.failure) ||
            !SilkType.equals(semanticType(fn.result), operation.propagationType.type)
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAllocationOperation',
                function: fn.id,
                region: region.id,
                detail: 'allocation does not preserve Layout, Allocation, or OutOfMemory contracts',
              }),
            )
        }
        if (operation._tag === 'RawBufferFrom') {
          const allocation = fn.localTypes.at(operation.allocation.ordinal)
          const count = fn.localTypes.at(operation.count.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const elementLayout = Layout.entry(self.layout, operation.element)
          const expectedStride =
            elementLayout === undefined
              ? undefined
              : Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment
          if (
            allocation?._tag !== 'Nominal' ||
            !SilkType.equals(allocation.type, SilkType.allocation) ||
            count?._tag !== 'usize' ||
            destination?._tag !== 'Nominal' ||
            !SilkType.isRawBuffer(destination.type) ||
            !SilkType.equals(destination.type, operation.type.type) ||
            !SilkType.equals(destination.type.arguments[0], operation.element) ||
            expectedStride === undefined ||
            operation.stride !== expectedStride ||
            operation.elementAlignment !== elementLayout?.alignment
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidRawStorageOperation',
                function: fn.id,
                region: region.id,
                detail:
                  'RawBuffer construction lost allocation, count, element, or layout provenance',
              }),
            )
          }
        }
        if (operation._tag === 'RawBufferCount') {
          const buffer = fn.localTypes.at(operation.buffer.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          if (
            buffer?._tag !== 'Reference' ||
            !SilkType.isRawBuffer(buffer.type.target) ||
            destination?._tag !== 'usize'
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidRawStorageOperation',
                function: fn.id,
                region: region.id,
                detail: 'RawBuffer.count lost its borrowed buffer or usize result',
              }),
            )
        }
        if (operation._tag === 'RawBufferSlot') {
          const buffer = fn.localTypes.at(operation.buffer.ordinal)
          const index = fn.localTypes.at(operation.index.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const bufferElement =
            buffer?._tag === 'Reference' && SilkType.isRawBuffer(buffer.type.target)
              ? buffer.type.target.arguments[0]
              : undefined
          if (
            buffer?._tag !== 'Reference' ||
            buffer.type.access !== 'Exclusive' ||
            index?._tag !== 'usize' ||
            destination?._tag !== 'Nominal' ||
            !SilkType.isSlot(destination.type) ||
            bufferElement === undefined ||
            !SilkType.equals(bufferElement, operation.element) ||
            !SilkType.equals(destination.type.arguments[0], operation.element)
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidRawStorageOperation',
                function: fn.id,
                region: region.id,
                detail:
                  'Slot projection lost its exclusive buffer, bounds operand, or element provenance',
              }),
            )
        }
        if (
          operation._tag === 'SlotWrite' ||
          operation._tag === 'SlotTake' ||
          operation._tag === 'SlotCopy' ||
          operation._tag === 'SlotDrop'
        ) {
          const slot = fn.localTypes.at(operation.slot.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const slotElement =
            slot?._tag === 'Nominal' && SilkType.isSlot(slot.type)
              ? slot.type.arguments[0]
              : undefined
          const unitResult =
            operation._tag === 'SlotTake' || operation._tag === 'SlotCopy'
              ? true
              : destination?._tag === 'Nominal' && SilkType.equals(destination.type, SilkType.unit)
          const takeResult =
            !(operation._tag === 'SlotTake' || operation._tag === 'SlotCopy') ||
            (destination !== undefined &&
              SilkType.equals(semanticType(destination), operation.element) &&
              (operation._tag !== 'SlotCopy' ||
                isStructurallyCopyType(self.layout, operation.element)))
          const writeValue =
            operation._tag !== 'SlotWrite' ||
            (() => {
              const value = fn.localTypes.at(operation.value.ordinal)
              return value !== undefined && SilkType.equals(semanticType(value), operation.element)
            })()
          if (
            slotElement === undefined ||
            !SilkType.equals(slotElement, operation.element) ||
            !unitResult ||
            !takeResult ||
            !writeValue
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidRawStorageOperation',
                function: fn.id,
                region: region.id,
                detail: `${operation._tag} lost its slot, element, value, or result provenance`,
              }),
            )
        }
        if (operation._tag === 'SliceLength') {
          const slice = fn.localTypes.at(operation.slice.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          if (
            slice === undefined ||
            !SilkType.isSlice(semanticType(slice)) ||
            destination?._tag !== 'usize'
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidSliceOperation',
                function: fn.id,
                region: region.id,
                detail: 'slice length requires one logical slice local and one usize destination',
              }),
            )
          }
        }
        if (operation._tag === 'Match') {
          const source = fn.localTypes.at(operation.scrutinee.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const plannedScrutinee = Layout.callingShape(
            self.layout,
            semanticType(operation.scrutineeType),
          )
          const plannedResult = Layout.callingShape(self.layout, semanticType(operation.type))
          if (
            source === undefined ||
            destination === undefined ||
            !SilkType.equals(semanticType(source), semanticType(operation.scrutineeType)) ||
            !SilkType.equals(semanticType(destination), semanticType(operation.type)) ||
            !SilkType.equals(
              operation.scrutineeShape.type,
              semanticType(operation.scrutineeType),
            ) ||
            !SilkType.equals(operation.resultShape.type, semanticType(operation.type)) ||
            plannedScrutinee === undefined ||
            plannedResult === undefined ||
            !callingShapeEquals(plannedScrutinee, operation.scrutineeShape) ||
            !callingShapeEquals(plannedResult, operation.resultShape)
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidMatchLayout',
                function: fn.id,
                region: region.id,
                detail: 'match scrutinee or join disagrees with its locals or compiler layout',
              }),
            )
          }

          const coverage = Match.cover(
            operation.members,
            operation.arms.map((arm) => ({
              ...(arm.member === undefined ? {} : { member: arm.member }),
              universal: arm.universal,
              guarded: arm.guard !== undefined,
            })),
          )
          const decisionsValid =
            coverage.exhaustive &&
            operation.decisions.length === operation.members.length &&
            operation.decisions.every((decision, ordinal) => {
              const member = operation.members.at(ordinal)
              const expected = operation.arms.filter(
                (arm) =>
                  arm.universal ||
                  (arm.member !== undefined &&
                    member !== undefined &&
                    SilkType.equals(arm.member, member)),
              )
              return (
                member !== undefined &&
                SilkType.equals(decision.member, member) &&
                decision.candidates.length === expected.length &&
                decision.candidates.every(
                  (candidate, candidateOrdinal) =>
                    candidate.ordinal === expected.at(candidateOrdinal)?.id.ordinal,
                )
              )
            }) &&
            operation.arms.every((arm, ordinal) => {
              const transition = coverage.transitions.at(ordinal)
              return (
                transition?.reachable === true &&
                sameMembers(arm.before, transition.before) &&
                sameMembers(arm.after, transition.after)
              )
            })
          if (!decisionsValid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidMatchDecision',
                function: fn.id,
                region: region.id,
                detail: 'match decisions disagree with canonical members or source coverage order',
              }),
            )
          }

          for (const arm of operation.arms) {
            for (const binding of arm.bindings) {
              const localType = fn.localTypes.at(binding.destination.ordinal)
              const selected =
                arm.member === undefined
                  ? undefined
                  : fieldPathType(self.layout, arm.member, binding.path)
              if (
                localType === undefined ||
                selected === undefined ||
                !SilkType.equals(semanticType(localType), semanticType(binding.type)) ||
                !SilkType.equals(selected, semanticType(binding.type)) ||
                binding.access !== operation.access
              ) {
                violations.push(
                  Object.freeze({
                    _tag: 'Violation',
                    rule: 'InvalidMatchBinding',
                    function: fn.id,
                    region: region.id,
                    detail: `arm #${arm.id.ordinal} has an invalid pattern path, type, or access`,
                  }),
                )
              }
            }
            if (
              arm.guard !== undefined &&
              fn.localTypes.at(arm.guard.result.ordinal)?._tag !== 'bool'
            ) {
              violations.push(
                Object.freeze({
                  _tag: 'Violation',
                  rule: 'InvalidMatchGuard',
                  function: fn.id,
                  region: region.id,
                  detail: `arm #${arm.id.ordinal} guard does not produce bool`,
                }),
              )
            }
            const resultType = fn.localTypes.at(arm.selected.result.ordinal)
            if (
              resultType === undefined ||
              !SilkType.equals(semanticType(resultType), semanticType(operation.type))
            ) {
              violations.push(
                Object.freeze({
                  _tag: 'Violation',
                  rule: 'InvalidMatchJoin',
                  function: fn.id,
                  region: region.id,
                  detail: `arm #${arm.id.ordinal} result does not match the join destination`,
                }),
              )
            }
            const cleanupValid =
              arm.selected.access === operation.access &&
              arm.selected.endBorrow ===
                (operation.access === 'Shared' || operation.access === 'Exclusive') &&
              (operation.access === 'Move'
                ? arm.selected.cleanup.every((entry) => {
                    const selected =
                      arm.member === undefined
                        ? undefined
                        : fieldPathType(self.layout, arm.member, entry.path)
                    return selected !== undefined && SilkType.equals(selected, entry.cleanup.type)
                  })
                : arm.selected.cleanup.length === 0)
            if (!cleanupValid) {
              violations.push(
                Object.freeze({
                  _tag: 'Violation',
                  rule: 'InvalidMatchOwnership',
                  function: fn.id,
                  region: region.id,
                  detail: `arm #${arm.id.ordinal} has invalid selection ownership or cleanup`,
                }),
              )
            }
          }
        }
        if (operation._tag === 'ConvertUnion') {
          const source = fn.localTypes.at(operation.source.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const compatibility = TypeCompatibility.check(
            semanticType(operation.sourceType),
            operation.targetType.type,
          )
          const mappingsValid =
            compatibility._tag === operation.conversion &&
            compatibility.mappings.length === operation.mappings.length &&
            compatibility.mappings.every((mapping, ordinal) => {
              const actual = operation.mappings.at(ordinal)
              return (
                actual !== undefined &&
                mapping.sourceOrdinal === actual.sourceOrdinal &&
                mapping.targetOrdinal === actual.targetOrdinal &&
                SilkType.equals(mapping.source, actual.source) &&
                SilkType.equals(mapping.target, actual.target)
              )
            })
          const valid =
            source !== undefined &&
            destination !== undefined &&
            SilkType.equals(semanticType(source), semanticType(operation.sourceType)) &&
            SilkType.equals(semanticType(destination), operation.targetType.type) &&
            mappingsValid &&
            SilkType.equals(operation.sourceShape.type, semanticType(operation.sourceType)) &&
            SilkType.equals(operation.targetShape.type, operation.targetType.type) &&
            (() => {
              const sourceShape = Layout.callingShape(self.layout, operation.sourceShape.type)
              const targetShape = Layout.callingShape(self.layout, operation.targetShape.type)
              return (
                sourceShape !== undefined &&
                targetShape !== undefined &&
                callingShapeEquals(sourceShape, operation.sourceShape) &&
                callingShapeEquals(targetShape, operation.targetShape)
              )
            })()
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                region: region.id,
                detail: 'union conversion disagrees with its locals, mapping, or layout shapes',
              }),
            )
          }
        }
        if (operation._tag === 'Drop') {
          const dropped = fn.localTypes.at(operation.local.ordinal)
          const cleanup = operation.cleanup
          const droppedSemantic = dropped === undefined ? undefined : semanticType(dropped)
          const cleanupTypeMatches =
            droppedSemantic !== undefined &&
            (SilkType.equals(droppedSemantic, cleanup.type) ||
              (SilkType.isEffect(droppedSemantic) &&
                SilkType.isEffect(cleanup.type) &&
                (() => {
                  const cleanupEffect = cleanup.type
                  return (
                    SilkType.equals(droppedSemantic.success, cleanupEffect.success) &&
                    droppedSemantic.failures.length === cleanupEffect.failures.length &&
                    droppedSemantic.failures.every((failure, ordinal) => {
                      const expected = cleanupEffect.failures.at(ordinal)
                      return expected !== undefined && SilkType.equals(failure, expected)
                    })
                  )
                })()))
          const unionCasesValid =
            cleanup._tag !== 'UnionCleanup' ||
            (cleanup.cases.length === cleanup.type.members.length &&
              cleanup.cases.every((member, ordinal) => {
                const expected = cleanup.type.members.at(ordinal)
                return (
                  expected !== undefined &&
                  member.ordinal === ordinal &&
                  SilkType.equals(member.member, expected)
                )
              }))
          const callableCleanupValid =
            cleanup._tag !== 'CallableCleanup' ||
            (dropped?._tag === 'CallableValue' &&
              dropped.environment !== undefined &&
              dropped.site !== undefined &&
              dropped.site.function.sourceId === cleanup.site.function.sourceId &&
              dropped.site.function.ordinal === cleanup.site.function.ordinal &&
              dropped.site.span.start === cleanup.site.span.start &&
              dropped.site.span.end === cleanup.site.span.end &&
              (() => {
                const expected = dropped.environment.fields
                  .filter((field) => field.access === 'Take' && !isCopyType(field.type))
                  .map((field) => field.ordinal)
                  .reverse()
                const endedLoans = operations
                  .slice(0, index)
                  .filter((candidate) => candidate._tag === 'EndLoan').length
                const borrowed = dropped.environment.fields.filter(
                  (field) => field.access === 'Shared' || field.access === 'Exclusive',
                ).length
                return (
                  expected.length === cleanup.slots.length &&
                  expected.every((ordinal, slot) => cleanup.slots.at(slot)?.ordinal === ordinal) &&
                  endedLoans >= borrowed
                )
              })())
          if (
            dropped === undefined ||
            !cleanupTypeMatches ||
            !unionCasesValid ||
            !callableCleanupValid
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                region: region.id,
                detail: 'drop cleanup disagrees with its local type or canonical union cases',
              }),
            )
          }
        }
        if (operation._tag === 'Construct') {
          const layout = Layout.entry(self.layout, operation.type.type)
          const expected =
            layout?.representation._tag === 'Aggregate' ? layout.representation.fields : []
          const valid =
            expected.length === operation.fields.length &&
            operation.fields.every((field, ordinal) => {
              const declared = expected.at(ordinal)
              const valueType = fn.localTypes.at(field.value.ordinal)
              return (
                declared !== undefined &&
                declared.id.ordinal === field.field.ordinal &&
                valueType !== undefined &&
                SilkType.equals(semanticType(valueType), declared.type)
              )
            })
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                region: region.id,
                detail: `construction of ${typeText(operation.type)} does not match its canonical fields`,
              }),
            )
          }
        }
        if (operation._tag === 'ConstructArray') {
          const semantic = operation.type.type
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const valid =
            operation.elements.length === semantic.length &&
            destination !== undefined &&
            SilkType.equals(semanticType(destination), semantic) &&
            operation.elements.every((element) => {
              const elementType = fn.localTypes.at(element.ordinal)
              return (
                elementType !== undefined &&
                SilkType.equals(semanticType(elementType), semantic.element)
              )
            })
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                region: region.id,
                detail: `construction of ${typeText(operation.type)} does not match its canonical element count or type`,
              }),
            )
          }
        }
        if (operation._tag === 'Project') {
          const sourceType = fn.localTypes.at(operation.source.ordinal)
          const sourceLayout =
            sourceType?._tag === 'Nominal' ? Layout.entry(self.layout, sourceType.type) : undefined
          const field =
            sourceLayout?.representation._tag === 'Aggregate'
              ? sourceLayout.representation.fields.find(
                  (candidate) => candidate.id.ordinal === operation.field.ordinal,
                )
              : undefined
          if (field === undefined || !SilkType.equals(field.type, semanticType(operation.type))) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                region: region.id,
                detail: `projection field #${operation.field.ordinal} does not match its source type`,
              }),
            )
          }
        }
        if (operation._tag === 'ReadPlace' || operation._tag === 'CheckPlace') {
          const selected = placeType(fn, self.layout, operation.root, operation.selectors)
          const sliceSelector = operation.selectors.find(
            (selector) => selector._tag === 'SliceElementSelector',
          )
          if (
            selected === undefined ||
            !SilkType.equals(selected, semanticType(operation.type)) ||
            (operation._tag === 'ReadPlace' &&
              !isStructurallyCopyType(self.layout, selected) &&
              operation.consume !== true)
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule:
                  sliceSelector === undefined
                    ? 'InvalidAggregateOperation'
                    : 'InvalidSliceOperation',
                function: fn.id,
                region: region.id,
                detail: `${operation._tag} does not match its root, selectors, or type`,
              }),
            )
          }
        }
        if (operation._tag === 'WritePlace') {
          const selected = placeType(fn, self.layout, operation.root, operation.selectors)
          const source = fn.localTypes.at(operation.source.ordinal)
          const root = fn.localTypes.at(operation.root.ordinal)
          const sliceSelector = operation.selectors.find(
            (selector) => selector._tag === 'SliceElementSelector',
          )
          const checked = operations
            .slice(0, index)
            .some(
              (candidate) =>
                candidate._tag === 'CheckPlace' &&
                candidate.root.ordinal === operation.root.ordinal &&
                candidate.selectors === operation.selectors,
            )
          if (
            selected === undefined ||
            source === undefined ||
            root === undefined ||
            !checked ||
            !SilkType.equals(selected, semanticType(operation.type)) ||
            !SilkType.equals(semanticType(source), selected) ||
            !SilkType.equals(semanticType(root), semanticType(operation.rootType)) ||
            (sliceSelector !== undefined && sliceSelector.access !== 'Exclusive')
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidWrite',
                function: fn.id,
                region: region.id,
                detail:
                  'write lacks a matching precheck or has inconsistent root/source/place types',
              }),
            )
          }
        }
        if (operation._tag === 'Call') {
          const target = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.target, operation.typeArguments),
          )
          const valid =
            target !== undefined &&
            target.parameterCount === operation.arguments.length &&
            operation.arguments.every((argument, ordinal) => {
              const actual = fn.localTypes.at(argument.ordinal)
              const expected = target?.localTypes.at(ordinal)
              return (
                actual !== undefined &&
                expected !== undefined &&
                SilkType.equals(semanticType(actual), semanticType(expected))
              )
            }) &&
            SilkType.equals(semanticType(operation.type), semanticType(target.result))
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidCallShape',
                function: fn.id,
                region: region.id,
                detail: `call ${targetText(operation.target)} does not match its logical contract`,
              }),
            )
          }
        }
        if (operation._tag === 'MakeCallable') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const environment = operation.type.environment
          const fields = environment?.fields ?? []
          const capturesValid =
            operation.captures.length === fields.length &&
            operation.captures.every((capture, ordinal) => {
              const field = fields.at(ordinal)
              const source = fn.localTypes.at(capture.source.ordinal)
              return (
                field !== undefined &&
                source !== undefined &&
                capture.ordinal === field.ordinal &&
                capture.parameterOrdinal === field.parameterOrdinal &&
                capture.access === field.access &&
                SilkType.equals(semanticType(source), field.type)
              )
            })
          const valid =
            destination?._tag === 'CallableValue' &&
            SilkType.equals(destination.type, operation.type.type) &&
            sameCallableTarget(destination.target, operation.target) &&
            sameCallableTarget(operation.type.target, operation.target) &&
            operation.typeArguments.every(SilkType.isConcrete) &&
            (environment === undefined
              ? operation.captures.length === 0
              : capturesValid &&
                sameCallableTarget(environment.callable.target, operation.target) &&
                environment.callable.mode === operation.type.type.mode)
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidCallableOperation',
                function: fn.id,
                region: region.id,
                detail: 'callable construction disagrees with its identity, slots, or layout',
              }),
            )
          }
        }
        if (operation._tag === 'ApplyCallable') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const source =
            operation.callable === undefined
              ? undefined
              : fn.localTypes.at(operation.callable.ordinal)
          const argumentsValid =
            operation.arguments.length === operation.callableType.parameters.length &&
            operation.arguments.every((argument, ordinal) => {
              const actual = fn.localTypes.at(argument.ordinal)
              const expected = operation.callableType.parameters.at(ordinal)
              return (
                actual !== undefined &&
                expected !== undefined &&
                SilkType.equals(semanticType(actual), expected)
              )
            })
          const environmentForm =
            operation.realization === 'Environment' &&
            operation.callable !== undefined &&
            operation.target === undefined &&
            operation.captures.length === 0 &&
            source?._tag === 'CallableValue' &&
            SilkType.equals(source.type, operation.callableType)
          const directEnvironment = self.layout.callableEnvironments.find(
            (candidate) =>
              candidate._tag === 'CallableEnvironment' &&
              operation.target !== undefined &&
              sameCallableTarget(candidate.callable.target, operation.target) &&
              SilkType.equals(candidate.callable.type, operation.callableType) &&
              candidate.callable.typeArguments.length === operation.typeArguments.length &&
              candidate.callable.typeArguments.every((argument, ordinal) => {
                const actual = operation.typeArguments.at(ordinal)
                return actual !== undefined && SilkType.equals(argument, actual)
              }),
          )
          const directCapturesValid =
            operation.captures.length === 0 ||
            (directEnvironment?._tag === 'CallableEnvironment' &&
              directEnvironment.fields.length === operation.captures.length &&
              directEnvironment.fields.every((field, ordinal) => {
                const capture = operation.captures.at(ordinal)
                const sourceType =
                  capture === undefined ? undefined : fn.localTypes.at(capture.source.ordinal)
                return (
                  capture !== undefined &&
                  sourceType !== undefined &&
                  capture.ordinal === field.ordinal &&
                  capture.parameterOrdinal === field.parameterOrdinal &&
                  capture.access === field.access &&
                  SilkType.equals(semanticType(sourceType), field.type)
                )
              }))
          const directForm =
            operation.realization === 'DirectErasedSection' &&
            operation.callable === undefined &&
            operation.target !== undefined &&
            directCapturesValid
          const valid =
            destination !== undefined &&
            SilkType.equals(semanticType(destination), semanticType(operation.type)) &&
            operation.access === operation.callableType.mode &&
            operation.typeArguments.every(SilkType.isConcrete) &&
            argumentsValid &&
            (environmentForm || directForm)
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidCallableOperation',
                function: fn.id,
                region: region.id,
                detail:
                  'callable application disagrees with its mode, arguments, realization, or result',
              }),
            )
          }
        }
        if (operation._tag === 'PackEffectOutcome') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const source = fn.localTypes.at(operation.source.ordinal)
          const payload =
            operation.tag === 0
              ? operation.type.type.success
              : operation.type.type.failures.at(operation.tag - 1)
          if (
            destination?._tag !== 'EffectOutcome' ||
            source === undefined ||
            payload === undefined ||
            !SilkType.equals(destination.type, operation.type.type) ||
            !SilkType.equals(semanticType(source), payload)
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'effect outcome tag, payload, or destination type is inconsistent',
              }),
            )
        }
        if (operation._tag === 'UnpackEffectSuccess') {
          const source = fn.localTypes.at(operation.source.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          if (
            source?._tag !== 'EffectOutcome' ||
            destination === undefined ||
            !SilkType.equals(source.type.success, semanticType(destination)) ||
            !SilkType.equals(semanticType(operation.type), semanticType(destination))
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'effect success projection does not match its outcome contract',
              }),
            )
        }
        if (operation._tag === 'RunEffect') {
          const target = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.target, operation.typeArguments),
          )
          const mappingsValid =
            operation.tagMappings.length === operation.outcomeType.type.failures.length &&
            operation.tagMappings.every((mapping) => {
              const source = operation.outcomeType.type.failures.at(mapping.source - 1)
              const targetFailure = operation.propagationType.type.failures.at(mapping.target - 1)
              return (
                source !== undefined &&
                targetFailure !== undefined &&
                SilkType.equals(source, targetFailure)
              )
            })
          if (
            target === undefined ||
            target.result._tag !== 'EffectOutcome' ||
            !SilkType.equals(target.result.type, operation.outcomeType.type) ||
            !SilkType.equals(semanticType(fn.result), operation.propagationType.type) ||
            !mappingsValid
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'run propagation does not preserve canonical outcome contracts',
              }),
            )
        }
        if (operation._tag === 'CatchEffect') {
          const protectedTargetId = operation.protectedTarget
          const protectedTarget =
            protectedTargetId === undefined
              ? undefined
              : self.functions.find((candidate) =>
                  matchesInstance(candidate, protectedTargetId, operation.protectedTypeArguments),
                )
          const protectedRunner = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.protectedRunner, operation.protectedTypeArguments),
          )
          const handlerTarget = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.handlerTarget, operation.handlerTypeArguments),
          )
          const handlerRunner = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.handlerRunner, operation.handlerTypeArguments),
          )
          const handlerCallable = fn.localTypes.at(operation.handlerCallable.ordinal)
          if (
            (protectedTargetId !== undefined && protectedTarget === undefined) ||
            protectedRunner?.result._tag !== 'EffectOutcome' ||
            handlerTarget?.result._tag !== 'EffectValue' ||
            handlerRunner?.result._tag !== 'EffectOutcome' ||
            (protectedTarget !== undefined &&
              (protectedTarget.result._tag !== 'EffectValue' ||
                !SilkType.equals(
                  protectedTarget.result.type,
                  operation.protectedValueType.type,
                ))) ||
            !SilkType.equals(protectedRunner.result.type, operation.protectedType.type) ||
            !SilkType.equals(handlerTarget.result.type, operation.handlerValueType.type) ||
            !SilkType.equals(handlerRunner.result.type, operation.handlerType.type) ||
            handlerCallable?._tag !== 'CallableValue' ||
            !SilkType.equals(handlerCallable.type, operation.handlerCallableType.type) ||
            operation.handledTag < 1 ||
            operation.handledTag > operation.protectedType.type.failures.length ||
            operation.protectedType.type.failures.at(operation.handledTag - 1) === undefined
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'effect catch target, handler, or handled tag is inconsistent',
              }),
            )
        }
        if (operation._tag === 'RetryEffect') {
          const protectedTargetId = operation.protectedTarget
          const protectedTarget =
            protectedTargetId === undefined
              ? undefined
              : self.functions.find((candidate) =>
                  matchesInstance(candidate, protectedTargetId, operation.protectedTypeArguments),
                )
          const protectedRunner = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.protectedRunner, operation.protectedTypeArguments),
          )
          const retriesType = fn.localTypes.at(operation.retries.ordinal)
          const mappingsValid = operation.tagMappings.every((mapping) => {
            const source = operation.protectedType.type.failures.at(mapping.source - 1)
            const target = operation.propagationType?.type.failures.at(mapping.target - 1)
            return source !== undefined && target !== undefined && SilkType.equals(source, target)
          })
          if (
            (protectedTargetId !== undefined && protectedTarget?.result._tag !== 'EffectValue') ||
            protectedRunner?.result._tag !== 'EffectOutcome' ||
            (protectedTargetId !== undefined &&
              protectedTarget?.result._tag === 'EffectValue' &&
              !SilkType.equals(protectedTarget.result.type, operation.protectedValueType.type)) ||
            !SilkType.equals(protectedRunner.result.type, operation.protectedType.type) ||
            retriesType?._tag !== 'i32' ||
            (operation.protectedType.type.failures.length > 0 &&
              (operation.propagationType === undefined || !mappingsValid))
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'effect retry target, runner, retry count, or propagation is inconsistent',
              }),
            )
        }
      }
    }
  }
  return Object.freeze(violations)
}

const spanText = (span: SourceSpan.SourceSpan): string => `[${span.start}, ${span.end})`
const provenanceText = (provenance: Provenance): string =>
  `${spanText(provenance.span)}${provenance.generated ? ' generated' : ''}`
const localText = (local: LocalId): string => `%${local.ordinal}`
const regionText = (region: RegionId): string => `r${region.ordinal}`
const loopText = (loop: LoopId): string => `loop${loop.ordinal}`
const selectorText = (selectors: ReadonlyArray<PlaceSelector>): string =>
  selectors
    .map((selector) =>
      selector._tag === 'FieldSelector'
        ? `.#${selector.field.ordinal}`
        : selector._tag === 'SliceElementSelector'
          ? `[${localText(selector.index)}/slice:${selector.access.toLowerCase()}]`
          : `[${selector.index._tag === 'Proven' ? selector.index.value : localText(selector.index.local)}/${selector.length}]`,
    )
    .join('')

const operationText = (operation: Operation): string => {
  switch (operation._tag) {
    case 'Literal':
      return `${localText(operation.destination)} = literal ${operation.value} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'Binary':
      return `${localText(operation.destination)} = ${operation.operator.toLowerCase()} ${localText(operation.left)}, ${localText(operation.right)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ConvertInteger':
      return `${localText(operation.destination)} = convert ${localText(operation.source)} ${typeText(operation.sourceType)} -> ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'CheckedInteger':
      return `${localText(operation.destination)} = ${operation.operation.toLowerCase()} ${operation.operands.map(localText).join(', ')} ${typeText(operation.sourceType)} -> ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ValidateLayout':
      return `${localText(operation.destination)} = layout-make bytes=${localText(operation.bytes)} alignment=${localText(operation.alignment)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RepeatLayout':
      return `${localText(operation.destination)} = layout-repeat ${localText(operation.layout)} count=${localText(operation.count)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'Allocate':
      return `${localText(operation.destination)} = allocate ${localText(operation.layout)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RawBufferFrom':
      return `${localText(operation.destination)} = raw-buffer-from ${localText(operation.allocation)} count=${localText(operation.count)} element=${SilkType.encode(operation.element)} stride=${operation.stride} align=${operation.elementAlignment} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RawBufferCount':
      return `${localText(operation.destination)} = raw-buffer-count ${localText(operation.buffer)} : usize ${provenanceText(operation.provenance)}`
    case 'RawBufferSlot':
      return `${localText(operation.destination)} = raw-buffer-slot ${localText(operation.buffer)}[${localText(operation.index)}] element=${SilkType.encode(operation.element)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'SlotWrite':
      return `${localText(operation.destination)} = slot-write ${localText(operation.slot)}, ${localText(operation.value)} element=${SilkType.encode(operation.element)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'SlotTake':
      return `${localText(operation.destination)} = slot-take ${localText(operation.slot)} element=${SilkType.encode(operation.element)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'SlotCopy':
      return `${localText(operation.destination)} = slot-copy ${localText(operation.slot)} element=${SilkType.encode(operation.element)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'SlotDrop':
      return `${localText(operation.destination)} = slot-drop ${localText(operation.slot)} element=${SilkType.encode(operation.element)} cleanup=${operation.cleanup._tag} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'Move':
      return `${localText(operation.destination)} = move ${localText(operation.source)} ${provenanceText(operation.provenance)}`
    case 'BeginLoan':
      return `${localText(operation.destination)} = begin-loan l${operation.borrow.ordinal} ${operation.access.toLowerCase()} ${localText(operation.root)} source=${typeText(operation.sourceType)} : ${typeText(operation.type)} reborrow=${operation.reborrow} suspended=${operation.suspendsParent} ${provenanceText(operation.provenance)}`
    case 'EndLoan':
      return `end-loan l${operation.borrow.ordinal} ${localText(operation.slice)} ${provenanceText(operation.provenance)}`
    case 'SliceLength':
      return `${localText(operation.destination)} = slice-length ${localText(operation.slice)} : i32 ${provenanceText(operation.provenance)}`
    case 'ConvertUnion':
      return `${localText(operation.destination)} = union-${operation.conversion.toLowerCase()} ${localText(operation.source)} ${typeText(operation.sourceType)} -> ${typeText(operation.targetType)} access=${operation.access} mapping=${operation.mappings.map((mapping) => `${SilkType.encode(mapping.source)}#${mapping.sourceOrdinal}->${SilkType.encode(mapping.target)}#${mapping.targetOrdinal}`).join(',')} ${provenanceText(operation.provenance)}`
    case 'Call':
      return `${localText(operation.destination)} = call ${targetText(operation.target)}${
        operation.typeArguments.length === 0
          ? ''
          : `<${operation.typeArguments.map(SilkType.encode).join(', ')}>`
      }(${operation.arguments.map(localText).join(', ')}) : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'MakeEffect':
      return `${localText(operation.destination)} = make-effect ${targetText(operation.runner)} captures=${operation.captures.map((capture) => `${localText(capture.source)}:${capture.access.toLowerCase()}`).join(',') || 'none'} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'MakeCallable':
      return `${localText(operation.destination)} = make-callable ${callableTargetText(operation.target)} captures=${operation.captures.map((capture) => `#${capture.ordinal}->p${capture.parameterOrdinal}:${localText(capture.source)}:${capture.access.toLowerCase()}`).join(',') || 'none'} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ApplyCallable':
      return `${localText(operation.destination)} = apply-callable ${operation.callable === undefined ? (operation.target === undefined ? '?' : callableTargetText(operation.target)) : localText(operation.callable)}(${operation.arguments.map(localText).join(', ')}) captures=${operation.captures.map((capture) => `#${capture.ordinal}:${localText(capture.source)}`).join(',') || 'none'} access=${operation.access.toLowerCase()} evaluation=${operation.evaluation} realization=${operation.realization} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'PackEffectOutcome':
      return `${localText(operation.destination)} = effect-outcome tag=${operation.tag} ${localText(operation.source)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'UnpackEffectSuccess':
      return `${localText(operation.destination)} = effect-success ${localText(operation.source)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'CatchEffect':
      return `${localText(operation.destination)} = effect-catch tag=${operation.handledTag} ${operation.protectedTarget === undefined ? localText(operation.protectedValue) : targetText(operation.protectedTarget)} runner=${targetText(operation.protectedRunner)} arguments=${operation.protectedRunnerArguments.map(localText).join(',') || 'none'} -> ${targetText(operation.handlerTarget)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RetryEffect':
      return `${localText(operation.destination)} = effect-retry ${operation.protectedTarget === undefined ? localText(operation.protectedValue) : targetText(operation.protectedTarget)} retries=${localText(operation.retries)} propagate=${operation.tagMappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.type)} ${operation.releases === undefined || operation.releases.length === 0 ? '' : `releases=${operation.releases.map((release) => localText(release.local)).join(',')} `}${provenanceText(operation.provenance)}`
    case 'RunEffect':
      return `${localText(operation.destination)} = run-effect ${targetText(operation.target)} propagate=${operation.tagMappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.type)} ${operation.releases === undefined || operation.releases.length === 0 ? '' : `releases=${operation.releases.map((release) => localText(release.local)).join(',')} `}${provenanceText(operation.provenance)}`
    case 'RunEffectValue':
      return `${localText(operation.destination)} = run-effect-value ${localText(operation.effect)} runner=${targetText(operation.runner)} arguments=${operation.arguments.map(localText).join(',') || 'none'} propagate=${operation.tagMappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.type)} ${operation.releases === undefined || operation.releases.length === 0 ? '' : `releases=${operation.releases.map((release) => localText(release.local)).join(',')} `}${provenanceText(operation.provenance)}`
    case 'Construct':
      return `${localText(operation.destination)} = construct ${typeText(operation.type)} { ${operation.fields.map(({ field, value }) => `#${field.ordinal}: ${localText(value)}`).join(', ')} } ${provenanceText(operation.provenance)}`
    case 'ConstructArray':
      return `${localText(operation.destination)} = construct-array ${typeText(operation.type)} [${operation.elements.map(localText).join(', ')}] ${provenanceText(operation.provenance)}`
    case 'Project':
      return `${localText(operation.destination)} = project ${localText(operation.source)}.#${operation.field.ordinal} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ReadPlace':
      return `${localText(operation.destination)} = read-place${operation.consume === true ? ' consume' : ''} ${localText(operation.root)}${selectorText(operation.selectors)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'CheckPlace':
      return `check-place ${localText(operation.root)}${selectorText(operation.selectors)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'WritePlace':
      return `write-place ${localText(operation.root)}${selectorText(operation.selectors)} <- ${localText(operation.source)} : ${typeText(operation.type)} replacement=${operation.replacement} commit=${operation.commit} ${provenanceText(operation.provenance)}`
    case 'Drop':
      return `drop ${localText(operation.local)}${operation.cleanup._tag === 'NoCleanup' ? '' : ` cleanup=${operation.cleanup._tag}`} ${provenanceText(operation.provenance)}`
    case 'Match':
      return `${localText(operation.destination)} = match#${operation.id.span.start} ${operation.access.toLowerCase()} ${localText(operation.scrutinee)} : ${typeText(operation.scrutineeType)} -> ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
  }
}

const fieldPathText = (path: ReadonlyArray<DeclarationIndex.FieldId>): string =>
  path.length === 0 ? 'payload' : path.map((field) => `#${field.ordinal}`).join('.')

const operationLines = (operation: Operation, indent: string): ReadonlyArray<string> => {
  if (operation._tag !== 'Match') return [`${indent}${operationText(operation)}`]
  return [
    `${indent}${operationText(operation)}`,
    `${indent}  members ${operation.members.map(SilkType.encode).join(', ')}`,
    ...operation.decisions.map(
      (decision) =>
        `${indent}  decision ${SilkType.encode(decision.member)} candidates=${decision.candidates.map((candidate) => `#${candidate.ordinal}`).join(',')}`,
    ),
    ...operation.arms.flatMap((arm) => [
      `${indent}  arm #${arm.id.ordinal} ${arm.universal ? '_' : arm.member === undefined ? 'unknown' : SilkType.encode(arm.member)} before=${arm.before.map(SilkType.encode).join(',') || 'empty'} after=${arm.after.map(SilkType.encode).join(',') || 'empty'} ${provenanceText(arm.provenance)}`,
      ...arm.bindings.map(
        (binding) =>
          `${indent}    bind #${binding.id.ordinal} ${localText(binding.destination)} <- ${fieldPathText(binding.path)} : ${typeText(binding.type)} access=${binding.access} ${provenanceText(binding.provenance)}`,
      ),
      ...(arm.guard === undefined
        ? []
        : [
            `${indent}    guard -> ${localText(arm.guard.result)}`,
            ...arm.guard.operations.flatMap((child) => operationLines(child, `${indent}      `)),
          ]),
      `${indent}    selected access=${arm.selected.access} result=${localText(arm.selected.result)} end-borrow=${arm.selected.endBorrow}`,
      ...arm.selected.operations.flatMap((child) => operationLines(child, `${indent}      `)),
      ...arm.selected.cleanup.map(
        (entry) => `${indent}      cleanup ${fieldPathText(entry.path)} ${entry.cleanup._tag}`,
      ),
    ]),
  ]
}

const outcomeText = (outcome: Outcome): string => {
  switch (outcome._tag) {
    case 'Forward':
      return `forward ${regionText(outcome.target)} ${provenanceText(outcome.provenance)}`
    case 'Return':
      return `return ${localText(outcome.value)} ${provenanceText(outcome.provenance)}`
    case 'Trap':
      return `trap "${outcome.reason}" ${provenanceText(outcome.provenance)}`
    case 'Repeat':
      return `repeat ${loopText(outcome.loop)} ${provenanceText(outcome.provenance)}`
    case 'Exit':
      return `exit ${loopText(outcome.loop)} ${provenanceText(outcome.provenance)}`
    case 'Yield':
      return `yield ${provenanceText(outcome.provenance)}`
  }
}

const regionLines = (region: Region): ReadonlyArray<string> => {
  const owner = region.ownerLoop === undefined ? '' : ` owner=${loopText(region.ownerLoop)}`
  switch (region._tag) {
    case 'OperationRegion':
      return [
        `  ${regionText(region.id)} operation${owner}:`,
        ...region.operations.flatMap((operation) => operationLines(operation, '    ')),
        `    ${outcomeText(region.outcome)}`,
      ]
    case 'CleanupRegion':
      return [
        `  ${regionText(region.id)} cleanup${owner}:`,
        ...region.releases.flatMap((release) => operationLines(release, '    ')),
        `    ${outcomeText(region.outcome)}`,
      ]
    case 'ConditionalRegion':
      return [
        `  ${regionText(region.id)} conditional${owner} condition=${localText(region.condition)} taken=${regionText(region.taken)} otherwise=${regionText(region.otherwise)}${region.following === undefined ? '' : ` following=${regionText(region.following)}`} ${provenanceText(region.provenance)}`,
      ]
    case 'LoopRegion':
      return [
        `  ${regionText(region.id)} loop ${loopText(region.loop)}${region.parent === undefined ? '' : ` parent=${loopText(region.parent)}`} condition=${regionText(region.condition)} value=${localText(region.conditionValue)} body=${regionText(region.body)} following=${regionText(region.following)} ${provenanceText(region.provenance)}`,
      ]
  }
}

export const encode = (self: Module): string =>
  [
    `mir-module ${self.module}`,
    ...Layout.encode(self.layout).trimEnd().split('\n'),
    ...self.functions.flatMap((fn) => [
      `fn ${targetText(fn.id)}${
        fn.instance.typeArguments.length === 0
          ? ''
          : `<${fn.instance.typeArguments.map(SilkType.encode).join(', ')}>`
      } params=${fn.parameterCount} locals=${fn.localTypes.length} -> ${typeText(fn.result)} entry=${regionText(fn.entry)}`,
      ...topologicalRegions(fn).flatMap(regionLines),
    ]),
    '',
  ].join('\n')

const sampleSpan = (
  source: SourceFile.SourceFile,
  start: number,
  end: number,
): SourceSpan.SourceSpan =>
  Option.getOrThrowWith(
    SourceSpan.make(source, start, end),
    () => new RangeError('MIR sample produced an invalid span'),
  )
const local = (ordinal: number): LocalId => Object.freeze({ _tag: 'Local', ordinal })
const region = (ordinal: number): RegionId => Object.freeze({ _tag: 'Region', ordinal })
const i32: Type = Object.freeze({ _tag: 'i32' })
const bool: Type = Object.freeze({ _tag: 'bool' })
const canonical = (module: string, name: string): DeclarationIndex.CanonicalId =>
  Object.freeze({ _tag: 'CanonicalDeclarationId', module, name })
const instance = (declaration: DeclarationIndex.CanonicalId): Instances.InstanceKey =>
  Object.freeze({
    _tag: 'InstanceKey',
    declaration,
    typeArguments: Object.freeze([]),
    contractRow: Object.freeze([]),
  })

export const samples = (): ReadonlyArray<Module> => {
  const source = SourceFile.make(
    'sample://regions.silk',
    Uint8Array.from('pub fn answer() -> i32 { return 42 }', (char) => char.charCodeAt(0)),
  )
  const provenance = (start: number, end: number, generated = false): Provenance =>
    Object.freeze({ span: sampleSpan(source, start, end), generated })
  const straight: Module = Object.freeze({
    _tag: 'MirModule',
    module: source.id,
    layout: Layout.make(Target.aarch64AppleDarwin, ['i32']),
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical(source.id, 'answer'),
        instance: instance(canonical(source.id, 'answer')),
        parameterCount: 0,
        localTypes: Object.freeze([i32]),
        result: i32,
        entry: region(0),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(0),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Literal' as const,
                destination: local(0),
                type: i32,
                value: 42,
                provenance: provenance(32, 34),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: local(0),
              provenance: provenance(25, 34),
            }),
          }),
        ]),
      }),
    ]),
  })
  const conditional: Module = Object.freeze({
    _tag: 'MirModule',
    module: source.id,
    layout: Layout.make(Target.aarch64AppleDarwin, ['i32', 'bool']),
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical(source.id, 'choose'),
        instance: instance(canonical(source.id, 'choose')),
        parameterCount: 1,
        localTypes: Object.freeze([bool, i32]),
        result: i32,
        entry: region(0),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'ConditionalRegion' as const,
            id: region(0),
            condition: local(0),
            taken: region(1),
            otherwise: region(2),
            provenance: provenance(25, 34),
          }),
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(1),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Literal' as const,
                destination: local(1),
                type: i32,
                value: 1,
                provenance: provenance(32, 33),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: local(1),
              provenance: provenance(25, 34),
            }),
          }),
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(2),
            operations: Object.freeze([]),
            outcome: Object.freeze({
              _tag: 'Trap' as const,
              reason: 'otherwise',
              provenance: provenance(25, 34, true),
            }),
          }),
        ]),
      }),
    ]),
  })
  return Object.freeze([straight, conditional])
}
