import * as Option from 'effect/Option'
import * as CallableFieldRealization from './CallableFieldRealization.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Hir from './Hir.js'
import * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'
import * as Layout from './Layout.js'
import * as Match from './Match.js'
import type * as Ownership from './Ownership.js'
import * as Scalar from './Scalar.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import type * as StaticText from './StaticText.js'
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
  | { readonly _tag: 'String'; readonly type: SilkType.String }
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
      readonly storage?: {
        readonly _tag: 'StoredEffectField'
        readonly type: SilkType.Represented
        readonly realization: Extract<
          Layout.Representation,
          { readonly _tag: 'StoredEffectEnvironment' }
        >['realization']
      }
    }
  | {
      readonly _tag: 'EffectComposite'
      readonly type: SilkType.Represented
      readonly contract: SilkType.Effect
      readonly alternatives: ReadonlyArray<Extract<Type, { readonly _tag: 'EffectValue' }>>
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
      readonly storage?: {
        readonly _tag: 'StoredCallableField'
        readonly type: SilkType.Represented
        readonly realization: Extract<
          Layout.Representation,
          { readonly _tag: 'CallableEnvironment' }
        >['realization']
      }
    }
  | { readonly _tag: 'EffectOutcome'; readonly type: SilkType.Effect }

export const semanticType = (self: Type): DeclarationIndex.SemanticType => {
  if (self._tag === 'CallableValue' || self._tag === 'EffectValue')
    return self.storage?.type ?? self.type
  if (self._tag === 'EffectComposite') return self.type
  return self._tag === 'Nominal' ||
    self._tag === 'Bottom' ||
    self._tag === 'FixedArray' ||
    self._tag === 'String' ||
    self._tag === 'Slice' ||
    self._tag === 'Reference' ||
    self._tag === 'Union' ||
    self._tag === 'EffectBorrow' ||
    self._tag === 'EffectOutcome'
    ? self.type
    : self._tag
}

const typeText = (self: Type): string => SilkType.encode(semanticType(self))
/** Reads the concrete sealed Copy verdict published by target layout. */
export const isCopy = (layout: Layout.Plan, type: DeclarationIndex.SemanticType): boolean =>
  Layout.entry(layout, type)?.copy === true

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

export type NormalizationRejection =
  | 'ComplexConstructor'
  | 'DynamicTarget'
  | 'EffectEscapes'
  | 'EffectReused'
  | 'AffineCapture'
  | 'CrossRegionUse'
  | 'SuspendableRunner'
  | 'SuspensionUnknown'

export type NormalizationVerdict =
  | {
      readonly _tag: 'Normalized'
      readonly kind: 'FoldedConstructor' | 'DirectStaticRun'
      readonly function: DeclarationIndex.CanonicalId
      readonly region: RegionId
      readonly local: LocalId
      readonly guards: ReadonlyArray<
        'DirectTarget' | 'SingleRegion' | 'SingleUse' | 'Synchronous' | 'CopyOrShared'
      >
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Rejected'
      readonly reason: NormalizationRejection
      readonly function: DeclarationIndex.CanonicalId
      readonly region: RegionId
      readonly local: LocalId
      readonly provenance: Provenance
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
  | 'TotalOrder'

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
  operation === 'SaturatingMultiply' ||
  operation === 'TotalOrder'

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
      readonly _tag: 'StaticView'
      readonly destination: LocalId
      readonly data: string
      readonly length: number
      readonly type: Extract<Type, { readonly _tag: 'Slice' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'StaticString'
      readonly destination: LocalId
      readonly data: string
      readonly byteLength: number
      readonly type: Extract<Type, { readonly _tag: 'String' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'StringFromUtf8Unchecked'
      readonly destination: LocalId
      readonly bytes: LocalId
      readonly heldLoans: ReadonlyArray<Hir.BorrowId>
      readonly authorization: 'Unsafe'
      readonly type: Extract<Type, { readonly _tag: 'String' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'StringUtf8Bytes'
      readonly destination: LocalId
      readonly string: LocalId
      readonly heldLoans: ReadonlyArray<Hir.BorrowId>
      readonly type: Extract<Type, { readonly _tag: 'Slice' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'StringByteLength'
      readonly destination: LocalId
      readonly string: LocalId
      readonly type: Extract<Type, { readonly _tag: 'usize' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'StringEqualsExact'
      readonly destination: LocalId
      readonly left: LocalId
      readonly right: LocalId
      readonly negated: boolean
      readonly type: Extract<Type, { readonly _tag: 'bool' }>
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
      readonly _tag: 'ConvertScalar'
      readonly destination: LocalId
      readonly source: LocalId
      readonly sourceType: ScalarType
      readonly type: ScalarType
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ReinterpretScalar'
      readonly destination: LocalId
      readonly source: LocalId
      readonly sourceType: ScalarType
      readonly type: ScalarType
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'FloatUnary'
      readonly operation:
        | 'Negate'
        | 'Sqrt'
        | 'IsNaN'
        | 'IsInfinite'
        | 'IsFinite'
        | 'IsNormal'
        | 'IsSubnormal'
        | 'IsSignNegative'
      readonly destination: LocalId
      readonly source: LocalId
      readonly sourceType: ScalarType
      readonly type: ScalarType
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'FloatTranscendental'
      readonly operation: 'Sin' | 'Cos'
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
      /** Commits one immutable byte view to an explicit host-provided process destination. */
      readonly _tag: 'HostWrite'
      readonly destination: LocalId
      readonly stream: LocalId
      readonly bytes: LocalId
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly failure: SilkType.Nominal
      readonly propagationType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly failureTag: number
      readonly provenance: Provenance
    }
  | {
      /** Executes one validated native-only opaque-handle protocol operation. */
      readonly _tag: 'OsCall'
      readonly operation: Intrinsic.OperationId
      readonly destination: LocalId
      readonly arguments: ReadonlyArray<LocalId>
      readonly type: Type
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
      /** Bounds-checked non-consuming read through a shared RawBuffer borrow. */
      readonly _tag: 'RawBufferRead'
      readonly destination: LocalId
      readonly buffer: LocalId
      readonly index: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      /** Allocation-free view over a caller-proven initialized RawBuffer range. */
      readonly _tag: 'RawBufferView'
      readonly destination: LocalId
      readonly buffer: LocalId
      readonly offset: LocalId
      readonly length: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly stride: number
      readonly access: SilkType.Slice['access']
      readonly type: Extract<Type, { readonly _tag: 'Slice' }>
      readonly provenance: Provenance
    }
  | {
      /**
       * Bulk ownership transfer of a caller-proven range into raw storage. The result is as if
       * the range travelled through an intermediate buffer, so an overlapping source and
       * destination is a correct move rather than undefined behavior.
       */
      readonly _tag: 'RawBufferCopy'
      readonly destination: LocalId
      readonly buffer: LocalId
      readonly offset: LocalId
      readonly source: LocalId
      readonly length: LocalId
      readonly element: DeclarationIndex.SemanticType
      readonly stride: number
      /**
       * True when the element type is structurally Copy. A move of a Copy element leaves the
       * source range readable, which is what the byte-level backends do for every element type.
       */
      readonly retainsSource: boolean
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Sets a caller-proven byte range of raw storage to one repeated byte value. */
      readonly _tag: 'RawBufferFill'
      readonly destination: LocalId
      readonly buffer: LocalId
      readonly offset: LocalId
      readonly length: LocalId
      readonly value: LocalId
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
      readonly selectors: ReadonlyArray<PlaceSelector>
      readonly sourceType: Type
      readonly type: Extract<Type, { readonly _tag: 'Slice' | 'Reference' }>
      readonly access: SilkType.Slice['access']
      readonly reborrow: boolean
      readonly suspendsParent: boolean
      readonly provenance: Provenance
    }
  | EndLoanOperation
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
      readonly sourceType: Exclude<Type, { readonly _tag: 'EffectOutcome' }>
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
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly arguments: ReadonlyArray<LocalId>
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'MakeEffect'
      readonly destination: LocalId
      readonly runner: DeclarationIndex.CanonicalId
      readonly runnerTypeArguments: ReadonlyArray<SilkType.GenericArgument>
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
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
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
      readonly _tag: 'PackEffectComposite'
      readonly destination: LocalId
      readonly source: LocalId
      readonly alternative: number
      readonly type: Extract<Type, { readonly _tag: 'EffectComposite' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ApplyCallable'
      readonly destination: LocalId
      readonly callable?: LocalId
      readonly target?: Hir.CallableTarget
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
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
      /** Dynamically packs one member of an ordinary failure union into the Effect E channel. */
      readonly _tag: 'PackEffectFailureUnion'
      readonly destination: LocalId
      readonly source: LocalId
      readonly sourceType: Extract<Type, { readonly _tag: 'Union' }>
      readonly mappings: ReadonlyArray<{ readonly source: number; readonly target: number }>
      readonly type: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly provenance: Provenance
    }
  | {
      /** Propagates one already-materialized failure through the enclosing Effect runner. */
      readonly _tag: 'PropagateEffectFailure'
      readonly source: LocalId
      readonly sourceType: Type
      readonly propagationType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly tagMappings: ReadonlyArray<{
        readonly source: number
        readonly target: number
      }>
      readonly propagationLaneCount: number
      /** Owners still live at this site, released before the failure outcome propagates. */
      readonly releases?: ReadonlyArray<DropOperation>
      readonly type: Extract<Type, { readonly _tag: 'Bottom' }>
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
      readonly _tag: 'RunEffect'
      readonly destination: LocalId
      readonly outcome: LocalId
      readonly target: DeclarationIndex.CanonicalId
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly arguments: ReadonlyArray<LocalId>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly propagationType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly tagMappings: ReadonlyArray<{
        readonly source: number
        readonly target: number
      }>
      /** Caller-owned loans ended only when this run propagates a typed failure. */
      readonly failureLoanEnds?: ReadonlyArray<EndLoanOperation>
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
      readonly runnerTypeArguments: ReadonlyArray<SilkType.GenericArgument>
      /** Exact unsupplied runner retained when `runner` is a statically provided specialization. */
      readonly runnerBase?: {
        readonly declaration: DeclarationIndex.CanonicalId
        readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
      }
      /** Ordered compile-time provider selections proving the exact requirement row. */
      readonly providers: ReadonlyArray<{
        readonly capability: SilkType.Nominal
        readonly providerType: SilkType.Nominal
        readonly witness: DeclarationIndex.ConformanceWitness
        readonly role: string
        readonly requirementAccess: SilkType.Requirement['access']
        readonly access: 'Shared' | 'Exclusive' | 'Take'
        readonly argument?: LocalId
      }>
      /** Statically selected service-provider references appended after the Effect captures. */
      readonly arguments: ReadonlyArray<LocalId>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly propagationType?: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly tagMappings: ReadonlyArray<{
        readonly source: number
        readonly target: number
      }>
      readonly propagationLaneCount: number
      /** Caller-owned loans ended only when this run propagates or traps on a typed failure. */
      readonly failureLoanEnds?: ReadonlyArray<EndLoanOperation>
      /** Owners still live at this site, released before a failure outcome propagates. */
      readonly releases?: ReadonlyArray<DropOperation>
      readonly type: Exclude<Type, { readonly _tag: 'EffectOutcome' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RunEffectComposite'
      readonly destination: LocalId
      readonly outcome: LocalId
      readonly effect: LocalId
      readonly alternatives: ReadonlyArray<{
        readonly type: Extract<Type, { readonly _tag: 'EffectValue' }>
        readonly runner: DeclarationIndex.CanonicalId
        readonly runnerTypeArguments: ReadonlyArray<SilkType.GenericArgument>
        readonly tagMappings: ReadonlyArray<{ readonly source: number; readonly target: number }>
        readonly arguments: ReadonlyArray<LocalId>
      }>
      readonly arguments: ReadonlyArray<LocalId>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly propagationType?: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly tagMappings: ReadonlyArray<{ readonly source: number; readonly target: number }>
      readonly propagationLaneCount: number
      readonly failureLoanEnds?: ReadonlyArray<EndLoanOperation>
      readonly releases?: ReadonlyArray<DropOperation>
      readonly type: Exclude<Type, { readonly _tag: 'EffectOutcome' }>
      readonly provenance: Provenance
    }
  | {
      /** Runs a statically selected Effect directly from its proven local capture values. */
      readonly _tag: 'RunStaticEffect'
      readonly destination: LocalId
      readonly outcome: LocalId
      readonly runner: DeclarationIndex.CanonicalId
      readonly runnerTypeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly captures: ReadonlyArray<{
        readonly source: LocalId
        readonly access: 'Copy' | 'Shared'
      }>
      readonly arguments: ReadonlyArray<LocalId>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly propagationType?: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly tagMappings: ReadonlyArray<{ readonly source: number; readonly target: number }>
      readonly propagationLaneCount: number
      /** Caller-owned loans ended only when this run propagates or traps on a typed failure. */
      readonly failureLoanEnds?: ReadonlyArray<EndLoanOperation>
      readonly releases?: ReadonlyArray<DropOperation>
      readonly type: Exclude<Type, { readonly _tag: 'EffectOutcome' }>
      readonly provenance: Provenance
    }
  | {
      /** Runs one Effect and materializes only its completed typed channel as silk/result data. */
      readonly _tag: 'ReifyEffect'
      readonly destination: LocalId
      readonly outcome: LocalId
      readonly effect: LocalId
      readonly runner: DeclarationIndex.CanonicalId
      readonly runnerTypeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly arguments: ReadonlyArray<LocalId>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly resultType: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly resultField: DeclarationIndex.FieldId
      readonly resultUnion: SilkType.StructuralUnion
      readonly successType: SilkType.Nominal
      readonly successField: DeclarationIndex.FieldId
      readonly successTag: number
      readonly failureType: SilkType.Nominal
      readonly failureField: DeclarationIndex.FieldId
      readonly failureTag: number
      readonly failureValueType: SilkType.Type
      readonly resultShape: Layout.CallingShape
      readonly outcomeShape: Layout.CallingShape
      readonly failureValueShape: Layout.CallingShape
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Runs a closed application effect and converts its owned outcome to a machine status. */
      readonly _tag: 'CloseEffectEntry'
      readonly destination: LocalId
      readonly effect: LocalId
      readonly outcome: LocalId
      readonly target: DeclarationIndex.CanonicalId
      readonly runner: DeclarationIndex.CanonicalId
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly effectType: Extract<Type, { readonly _tag: 'EffectValue' }>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly failures: ReadonlyArray<{
        readonly tag: number
        readonly type: SilkType.Type
        readonly identity: string
        readonly payload: LocalId
        readonly cleanup: Ownership.CleanupPlan
      }>
      readonly type: Extract<Type, { readonly _tag: 'i32' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Construct'
      readonly destination: LocalId
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationIndex.FieldId
        readonly value: LocalId
        readonly stored?:
          | Extract<Type, { readonly _tag: 'CallableValue' }>['storage']
          | Extract<Type, { readonly _tag: 'EffectValue' }>['storage']
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
  | ShortCircuitOperation

/**
 * One compiler-owned conditional evaluation: `&&` and `||`. `left` is already evaluated when the
 * operation runs; `right` holds the operations that evaluate the right operand and the local
 * carrying its value. `&&` evaluates `right` only when `left` is true and otherwise yields false;
 * `||` evaluates `right` only when `left` is false and otherwise yields true. The right operand
 * is pure by elaboration, so skipping it performs and releases nothing.
 */
export interface ShortCircuitOperation {
  readonly _tag: 'ShortCircuit'
  readonly operator: 'And' | 'Or'
  readonly destination: LocalId
  readonly left: LocalId
  readonly right: {
    readonly operations: ReadonlyArray<Operation>
    readonly result: LocalId
  }
  readonly type: Extract<Type, { readonly _tag: 'bool' }>
  readonly provenance: Provenance
}

/** Releases one owned local through its cleanup plan. */
export interface DropOperation {
  readonly _tag: 'Drop'
  readonly local: LocalId
  readonly cleanup: Ownership.CleanupPlan
  readonly provenance: Provenance
}

/** Ends one caller-owned loan on the dynamic path that contains this operation. */
export interface EndLoanOperation {
  readonly _tag: 'EndLoan'
  readonly borrow: Hir.BorrowId
  readonly slice: LocalId
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
  readonly member?: SilkType.Type
  readonly universal: boolean
  readonly before: ReadonlyArray<SilkType.Type>
  readonly after: ReadonlyArray<SilkType.Type>
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
  readonly members: ReadonlyArray<SilkType.Type>
  readonly decisions: ReadonlyArray<{
    readonly member: SilkType.Type
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

export type SuspensionClassification = 'Synchronous' | 'Suspendable' | 'Unknown'

export interface SuspensionPointId {
  readonly _tag: 'SuspensionPointId'
  readonly owner: Instances.InstanceKey
  readonly sourceId: string
  readonly spanStart: number
  readonly spanEnd: number
  readonly ordinal: number
}

export interface ResumePointId {
  readonly _tag: 'ResumePointId'
  readonly point: SuspensionPointId
  readonly path: 'Success' | 'Failure'
}

export type SuspensionBorrowIdentity =
  | { readonly _tag: 'MirLoan'; readonly borrow: Hir.BorrowId }
  | { readonly _tag: 'BorrowedParameter'; readonly parameterOrdinal: number }
  | { readonly _tag: 'BorrowedLocal'; readonly local: LocalId }

export type CoroutineFrameAccess =
  | { readonly _tag: 'Copy' }
  | {
      readonly _tag: 'BorrowedDependency'
      readonly access: 'Shared' | 'Exclusive'
      readonly root: LocalId
      readonly loan: SuspensionBorrowIdentity
    }
  | { readonly _tag: 'AffineTransfer'; readonly cleanup: Ownership.CleanupPlan }

export interface CoroutineFrameSlot {
  readonly ordinal: number
  readonly local: LocalId
  readonly type: Type
  readonly access: CoroutineFrameAccess
}

export interface CoroutineFrameRelease {
  readonly local: LocalId
  readonly cleanup: Ownership.CleanupPlan
}

export interface CoroutineFramePathPlan {
  readonly restores: ReadonlyArray<number>
  readonly loanEnds: ReadonlyArray<SuspensionBorrowIdentity>
  readonly releases: ReadonlyArray<CoroutineFrameRelease>
}

export interface SuspensionProviderArgument {
  readonly capability: SilkType.Nominal
  readonly providerType: SilkType.Nominal
  readonly role: string
  readonly requirementAccess: SilkType.Requirement['access']
  readonly access: 'Shared' | 'Exclusive' | 'Take'
  readonly argument?: LocalId
  /** ABI lane containing the provider when `argument` is a captured environment value. */
  readonly argumentLane?: number
  readonly witness?: DeclarationIndex.ConformanceWitness
  readonly purposes: readonly ['ChildRequirement']
}

export interface SuspensionRunner {
  readonly classification: SuspensionClassification
  readonly declaration?: DeclarationIndex.CanonicalId
  readonly instance?: Instances.InstanceKey
  readonly effectIdentity?: string
  readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
  readonly outcome: SilkType.Effect
  readonly captures: ReadonlyArray<{
    readonly ordinal: number
    readonly source: 'Binding' | 'Parameter'
    readonly sourceOrdinal: number
    readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
    readonly type: DeclarationIndex.SemanticType
  }>
  readonly providers: ReadonlyArray<SuspensionProviderArgument>
}

export type SuspensionCompletion =
  | {
      readonly _tag: 'Propagate'
      readonly outcome: SilkType.Effect
      readonly failureMappings: ReadonlyArray<{ readonly source: number; readonly target: number }>
    }
  | {
      readonly _tag: 'Reify'
      readonly outcome: SilkType.Effect
      readonly resultType: SilkType.Nominal
      readonly resultField: DeclarationIndex.FieldId
      readonly resultUnion: SilkType.StructuralUnion
      readonly successType: SilkType.Nominal
      readonly successField: DeclarationIndex.FieldId
      readonly successTag: number
      readonly failureType: SilkType.Nominal
      readonly failureField: DeclarationIndex.FieldId
      readonly failureTag: number
      readonly failureValueType: SilkType.Type
      readonly resultShape: Layout.CallingShape
      readonly outcomeShape: Layout.CallingShape
      readonly failureValueShape: Layout.CallingShape
    }

export interface CoroutineFrameState {
  readonly _tag: 'CoroutineFrameState'
  readonly point: SuspensionPointId
  readonly runner: SuspensionRunner
  readonly outcome: SilkType.Effect
  readonly slots: ReadonlyArray<CoroutineFrameSlot>
  readonly success: CoroutineFramePathPlan & { readonly resume: ResumePointId }
  readonly failure: CoroutineFramePathPlan & { readonly resume: ResumePointId }
}

/** One reusable logical frame owned by one specialized suspendable invocation. */
export interface CoroutineFrameDescriptor {
  readonly _tag: 'CoroutineFrameDescriptor'
  readonly function: Instances.InstanceKey
  readonly states: ReadonlyArray<CoroutineFrameState>
}

export type CoroutineFrameHeaderRole = 'Parent' | 'State'

export interface CoroutineFrameHeaderField {
  readonly _tag: 'CoroutineFrameHeaderField'
  readonly role: CoroutineFrameHeaderRole
  readonly offset: number
  readonly size: number
  readonly alignment: number
}

export interface CoroutineFramePayloadField {
  readonly _tag: 'CoroutineFramePayloadField'
  readonly slot: number
  readonly local: LocalId
  readonly type: Type
  readonly access: CoroutineFrameAccess
  readonly offset: number
  readonly size: number
  readonly alignment: number
  readonly padding: number
}

export interface CoroutineFrameTargetStateLayout {
  readonly _tag: 'CoroutineFrameTargetStateLayout'
  readonly point: SuspensionPointId
  readonly size: number
  readonly alignment: number
  readonly payload: ReadonlyArray<CoroutineFramePayloadField>
  readonly tailPadding: number
}

/** One maximum physical frame plan shared by every mutually-exclusive state of an invocation. */
export interface CoroutineFrameTargetLayout {
  readonly _tag: 'CoroutineFrameTargetLayout'
  readonly function: Instances.InstanceKey
  readonly size: number
  readonly alignment: number
  readonly header: ReadonlyArray<CoroutineFrameHeaderField>
  readonly states: ReadonlyArray<CoroutineFrameTargetStateLayout>
}

export interface CoroutineFramePlan {
  readonly _tag: 'CoroutineFramePlan'
  readonly target: Layout.Plan['target']
  readonly entries: ReadonlyArray<CoroutineFrameTargetLayout>
}

export type SuspensionRegion =
  | {
      readonly _tag: 'SuspendEffectRegion'
      readonly point: SuspensionPointId
      readonly ownerRegion: RegionId
      readonly operation: Extract<
        Operation,
        { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
      >
      readonly deferred: SuspensionRunner
      readonly transfer: { readonly _tag: 'OriginateTransfer' }
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'RunSuspendableEffectRegion'
      readonly point: SuspensionPointId
      readonly ownerRegion: RegionId
      readonly operation: Extract<
        Operation,
        { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
      >
      readonly runner: SuspensionRunner
      readonly completion: SuspensionCompletion
      /** Independently retained post-normalization liveness fact verified against the descriptor. */
      readonly liveLocals: ReadonlyArray<LocalId>
      readonly complete: { readonly _tag: 'CompleteInCurrentActivation' }
      readonly relay: {
        readonly _tag: 'RelayExistingTransfer'
        readonly preserves: readonly ['Child', 'Origin', 'TypedOutcome']
        readonly frame: 'StatefulRelay' | 'MissingOwnershipPlan'
        readonly state?: CoroutineFrameState
      }
      readonly provenance: Provenance
    }

export type SuspendEffectRegion = Extract<
  SuspensionRegion,
  { readonly _tag: 'SuspendEffectRegion' }
>

export type RunSuspendableEffectRegion = Extract<
  SuspensionRegion,
  { readonly _tag: 'RunSuspendableEffectRegion' }
>

export interface SuspensionControlEdge {
  readonly _tag: 'SuspensionControlEdge'
  readonly from: SuspensionPointId
  readonly to: ResumePointId | { readonly _tag: 'RelayExit' }
  readonly kind: 'ResumeSuccess' | 'ResumeFailure' | 'RelayTransfer'
}

export interface MirFunction {
  readonly _tag: 'MirFunction'
  readonly id: DeclarationIndex.CanonicalId
  readonly instance: Instances.InstanceKey
  readonly parameterCount: number
  readonly localTypes: ReadonlyArray<Type>
  readonly result: Type
  readonly entry: RegionId
  readonly regions: ReadonlyArray<Region>
  /** Static origin of a generated Effect runner, including every selected provider witness. */
  readonly effectRunner?: {
    readonly base: {
      readonly declaration: DeclarationIndex.CanonicalId
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
    }
    readonly providers: ReadonlyArray<{
      readonly capability: SilkType.Nominal
      readonly providerType: SilkType.Nominal
      readonly witness: DeclarationIndex.ConformanceWitness
      readonly role: string
      readonly requirementAccess: SilkType.Requirement['access']
      readonly access: 'Shared' | 'Exclusive' | 'Take'
    }>
  }
  readonly suspension?: {
    readonly classification: SuspensionClassification
    readonly regions: ReadonlyArray<SuspensionRegion>
    readonly frame?: CoroutineFrameDescriptor
  }
}

export type Entry =
  | {
      readonly _tag: 'UnavailableEntry'
      readonly reason: Extract<Instances.Entry, { readonly _tag: 'Unavailable' }>['reason']
    }
  | {
      readonly _tag: 'OrdinaryEntry'
      readonly target: Instances.InstanceKey
      readonly machine: Instances.InstanceKey
    }
  | {
      readonly _tag: 'EffectEntry'
      readonly target: Instances.InstanceKey
      readonly machine: Instances.InstanceKey
      readonly requirements: ReadonlyArray<SilkType.Requirement>
      readonly failures: ReadonlyArray<{
        readonly tag: number
        readonly type: SilkType.Type
        readonly identity: string
      }>
    }

export interface Module {
  readonly _tag: 'MirModule'
  readonly module: string
  readonly entry: Entry
  readonly intrinsics: ReadonlyArray<Instances.IntrinsicCall>
  readonly layout: Layout.Plan
  readonly staticData?: ReadonlyArray<StaticText.Data>
  readonly functions: ReadonlyArray<MirFunction>
  readonly normalization?: ReadonlyArray<NormalizationVerdict>
  readonly coroutineFrames?: CoroutineFramePlan
}

/** The concrete zero-parameter `i32` function exported as the machine entry. */
export const machineEntry = (self: Module): Instances.InstanceKey => {
  if (self.entry._tag === 'UnavailableEntry') {
    throw new RangeError(`MIR has no machine entry: ${self.entry.reason}`)
  }
  return self.entry.machine
}

/** Tests whether a MIR function realizes one concrete call target. */
export const matchesInstance = (
  fn: MirFunction,
  declaration: DeclarationIndex.CanonicalId,
  typeArguments: ReadonlyArray<SilkType.GenericArgument>,
): boolean =>
  fn.id.module === declaration.module &&
  fn.id.name === declaration.name &&
  fn.instance.typeArguments.length === typeArguments.length &&
  fn.instance.typeArguments.every((argument, index) => {
    const expected = typeArguments.at(index)
    return (
      expected !== undefined &&
      SilkType.genericArgumentKey(argument) === SilkType.genericArgumentKey(expected)
    )
  })

/** Tests exact concrete instance identity, including the resolved contract row. */
export const matchesInstanceKey = (fn: MirFunction, key: Instances.InstanceKey): boolean =>
  instanceText(fn.instance) === instanceText(key)

const conformanceWitnessMatches = (
  left: DeclarationIndex.ConformanceWitness,
  right: DeclarationIndex.ConformanceWitness,
): boolean => {
  if (
    left._tag !== right._tag ||
    !SilkType.equals(left.capability, right.capability) ||
    !SilkType.equals(left.provider, right.provider)
  )
    return false
  if (left._tag !== 'SourceConformanceWitness') return true
  if (right._tag !== 'SourceConformanceWitness') return false
  return (
    left.module === right.module &&
    left.ordinal === right.ordinal &&
    left.typeArguments.length === right.typeArguments.length &&
    left.typeArguments.every((argument, ordinal) => {
      const expected = right.typeArguments.at(ordinal)
      return expected !== undefined && SilkType.equalsGenericArgument(argument, expected)
    }) &&
    left.operations.length === right.operations.length &&
    left.operations.every((operation, ordinal) => {
      const expected = right.operations.at(ordinal)
      return (
        expected !== undefined &&
        operation.name === expected.name &&
        operation.implementation.module === expected.implementation.module &&
        operation.implementation.name === expected.implementation.name
      )
    })
  )
}

export interface ControlEdge {
  readonly _tag: 'ControlEdge'
  readonly from: RegionId
  readonly to: RegionId
  readonly kind: 'Forward' | 'Taken' | 'Otherwise' | 'Following' | 'Condition' | 'Body'
}

/** Target-neutral completion, relay, and resume edges owned by finalized suspension control. */
export const suspensionControlEdges = (self: MirFunction): ReadonlyArray<SuspensionControlEdge> =>
  Object.freeze(
    (self.suspension?.regions ?? []).flatMap((region) =>
      region._tag === 'SuspendEffectRegion'
        ? [
            Object.freeze({
              _tag: 'SuspensionControlEdge' as const,
              from: region.point,
              to: Object.freeze({ _tag: 'RelayExit' as const }),
              kind: 'RelayTransfer' as const,
            }),
          ]
        : [
            Object.freeze({
              _tag: 'SuspensionControlEdge' as const,
              from: region.point,
              to: Object.freeze({ _tag: 'RelayExit' as const }),
              kind: 'RelayTransfer' as const,
            }),
            ...(region.relay.state === undefined
              ? []
              : [
                  Object.freeze({
                    _tag: 'SuspensionControlEdge' as const,
                    from: region.point,
                    to: region.relay.state.success.resume,
                    kind: 'ResumeSuccess' as const,
                  }),
                  Object.freeze({
                    _tag: 'SuspensionControlEdge' as const,
                    from: region.point,
                    to: region.relay.state.failure.resume,
                    kind: 'ResumeFailure' as const,
                  }),
                ]),
          ],
    ),
  )

/** Every local retained or referenced by finalized suspension control. */
export const suspensionLocals = (self: MirFunction): ReadonlyArray<LocalId> =>
  Object.freeze(
    (self.suspension?.regions ?? []).flatMap((region) => {
      if (region._tag === 'SuspendEffectRegion')
        return [
          ...operationLocals(region.operation),
          ...region.deferred.providers.flatMap((provider) =>
            provider.argument === undefined ? [] : [provider.argument],
          ),
        ]
      const descriptor = region.relay.state
      return [
        ...operationLocals(region.operation),
        ...region.liveLocals,
        ...(descriptor?.slots.flatMap((slot) => [
          slot.local,
          ...(slot.access._tag === 'BorrowedDependency' ? [slot.access.root] : []),
        ]) ?? []),
        ...(descriptor?.failure.releases.map((release) => release.local) ?? []),
        ...region.runner.providers.flatMap((provider) =>
          provider.argument === undefined ? [] : [provider.argument],
        ),
      ]
    }),
  )

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
    | 'InvalidReturn'
    | 'InvalidAggregateOperation'
    | 'InvalidIntegerOperation'
    | 'InvalidLayoutOperation'
    | 'InvalidAllocationOperation'
    | 'InvalidStandardStreamOperation'
    | 'InvalidOsOperation'
    | 'InvalidRawStorageOperation'
    | 'InvalidCallShape'
    | 'InvalidCallableOperation'
    | 'InvalidEffectOperation'
    | 'InvalidNormalization'
    | 'InvalidEntry'
    | 'InvalidWrite'
    | 'InvalidLoan'
    | 'InvalidSliceOperation'
    | 'InvalidStringOperation'
    | 'InvalidMatchLayout'
    | 'InvalidMatchDecision'
    | 'InvalidMatchBinding'
    | 'InvalidMatchGuard'
    | 'InvalidMatchOwnership'
    | 'InvalidMatchJoin'
    | 'CyclicMatchOperation'
    | 'InvalidSuspension'
    | 'InvalidCoroutineFrame'
    | 'OrphanSuspensionMachinery'
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
  operation._tag === 'ShortCircuit'
    ? operation.right.operations
    : operation._tag === 'Match'
      ? operation.arms.flatMap((arm) => [
          ...(arm.guard?.operations ?? []),
          ...arm.selected.operations,
        ])
      : operation._tag === 'RunEffect' ||
          operation._tag === 'RunEffectValue' ||
          operation._tag === 'RunStaticEffect'
        ? [...(operation.failureLoanEnds ?? []), ...(operation.releases ?? [])]
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

type LoanPathState = 'Dormant' | 'Live' | 'Ended'

interface StructuredCfgPathSemantics<State> {
  readonly initial: ReadonlySet<State>
  readonly transfer: (operation: Operation, incoming: ReadonlySet<State>) => ReadonlySet<State>
  readonly terminal: (states: ReadonlySet<State>) => void
  readonly repeat: (states: ReadonlySet<State>) => ReadonlySet<State>
  readonly merge: (...groups: ReadonlyArray<ReadonlySet<State>>) => ReadonlySet<State>
}

/**
 * Evaluates one finite-state path analysis over every structured MIR control-flow edge. The state
 * semantics remain analysis-owned; branching, guarded matches, loop routing, and terminal failure
 * paths are defined once so validators cannot disagree about reachable paths.
 */
const structuredCfgPathsValid = <State>(
  fn: MirFunction,
  byId: ReadonlyMap<number, Region>,
  loops: ReadonlyMap<number, LoopRegion>,
  semantics: StructuredCfgPathSemantics<State>,
): boolean => {
  let valid = true
  const sequence = (
    operations: ReadonlyArray<Operation>,
    incoming: ReadonlySet<State>,
  ): ReadonlySet<State> => {
    let states = incoming
    for (const operation of operations) states = transfer(operation, states)
    return states
  }
  const matchCandidates = (
    operation: MatchOperation,
    candidates: ReadonlyArray<Match.ArmId>,
    ordinal: number,
    incoming: ReadonlySet<State>,
  ): ReadonlySet<State> => {
    const candidate = candidates.at(ordinal)
    if (candidate === undefined) {
      semantics.terminal(incoming)
      return new Set()
    }
    const arm = operation.arms.find((entry) => entry.id.ordinal === candidate.ordinal)
    if (arm === undefined) {
      valid = false
      return new Set()
    }
    const guarded = arm.guard === undefined ? incoming : sequence(arm.guard.operations, incoming)
    const selected = sequence(arm.selected.operations, guarded)
    return arm.guard === undefined
      ? selected
      : semantics.merge(selected, matchCandidates(operation, candidates, ordinal + 1, guarded))
  }
  const transfer = (operation: Operation, incoming: ReadonlySet<State>): ReadonlySet<State> => {
    if (operation._tag === 'PropagateEffectFailure') {
      semantics.terminal(incoming)
      return new Set()
    }
    if (
      operation._tag === 'RunEffect' ||
      operation._tag === 'RunEffectValue' ||
      operation._tag === 'RunStaticEffect'
    ) {
      if (SilkType.failureMembers(operation.outcomeType.type).length > 0) {
        semantics.terminal(sequence(operation.failureLoanEnds ?? [], incoming))
      }
      return incoming
    }
    if (operation._tag === 'ShortCircuit')
      return semantics.merge(incoming, sequence(operation.right.operations, incoming))
    if (operation._tag === 'Match') {
      if (operation.decisions.length === 0) {
        semantics.terminal(incoming)
        return new Set()
      }
      return semantics.merge(
        ...operation.decisions.map((decision) =>
          matchCandidates(operation, decision.candidates, 0, incoming),
        ),
      )
    }
    return semantics.transfer(operation, incoming)
  }

  const incoming = new Map<number, Set<State>>()
  const pending: Array<number> = []
  const enqueue = (target: RegionId, states: ReadonlySet<State>): void => {
    if (states.size === 0 || !byId.has(target.ordinal)) return
    const known = incoming.get(target.ordinal) ?? new Set<State>()
    const previous = known.size
    for (const state of states) known.add(state)
    incoming.set(target.ordinal, known)
    if (known.size !== previous) pending.push(target.ordinal)
  }
  const route = (region: OperationRegion | CleanupRegion, states: ReadonlySet<State>): void => {
    const outcome = region.outcome
    if (outcome._tag === 'Forward') enqueue(outcome.target, states)
    else if (outcome._tag === 'Return' || outcome._tag === 'Trap') semantics.terminal(states)
    else if (outcome._tag === 'Repeat') {
      const loop = loops.get(outcome.loop.ordinal)
      if (loop !== undefined) enqueue(loop.condition, semantics.repeat(states))
    } else if (outcome._tag === 'Exit') {
      const loop = loops.get(outcome.loop.ordinal)
      if (loop !== undefined) enqueue(loop.following, states)
    } else {
      const loop =
        (region.ownerLoop === undefined ? undefined : loops.get(region.ownerLoop.ordinal)) ??
        [...loops.values()].find((candidate) => candidate.condition.ordinal === region.id.ordinal)
      if (loop !== undefined) {
        enqueue(loop.body, states)
        enqueue(loop.following, states)
      }
    }
  }

  enqueue(fn.entry, semantics.initial)
  while (pending.length > 0) {
    const ordinal = pending.shift()
    if (ordinal === undefined) continue
    const region = byId.get(ordinal)
    const states = incoming.get(ordinal)
    if (region === undefined || states === undefined) continue
    if (region._tag === 'ConditionalRegion') {
      enqueue(region.taken, states)
      enqueue(region.otherwise, states)
    } else if (region._tag === 'LoopRegion') enqueue(region.condition, states)
    else route(region, sequence(operationsOf(region), states))
  }
  return valid
}

const mergePathStates = <State>(...groups: ReadonlyArray<ReadonlySet<State>>): ReadonlySet<State> =>
  new Set(groups.flatMap((group) => [...group]))

/**
 * Proves the dynamic lifetime of one statically unique loan over structured operation branches and
 * lexical loop backedges. A path may avoid the loan entirely, but every path that begins it must
 * end it exactly once before terminating, and no loop may execute either endpoint twice.
 */
const loanPathsValid = (
  fn: MirFunction,
  key: string,
  byId: ReadonlyMap<number, Region>,
  loops: ReadonlyMap<number, LoopRegion>,
): boolean => {
  let valid = true
  const terminal = (states: ReadonlySet<LoanPathState>): void => {
    if (states.has('Live')) valid = false
  }
  const endpoint = (
    states: ReadonlySet<LoanPathState>,
    operation: Extract<Operation, { readonly _tag: 'BeginLoan' | 'EndLoan' }>,
  ): ReadonlySet<LoanPathState> => {
    if (borrowKey(operation.borrow) !== key) return states
    const next = new Set<LoanPathState>()
    for (const state of states) {
      if (operation._tag === 'BeginLoan' && state === 'Dormant') next.add('Live')
      else if (operation._tag === 'EndLoan' && state === 'Live') next.add('Ended')
      else valid = false
    }
    return next
  }
  const repeat = (states: ReadonlySet<LoanPathState>): ReadonlySet<LoanPathState> =>
    new Set([...states].map((state) => (state === 'Ended' ? 'Dormant' : state)))
  const transfer = (operation: Operation, incoming: ReadonlySet<LoanPathState>) => {
    if (operation._tag === 'BeginLoan' || operation._tag === 'EndLoan')
      return endpoint(incoming, operation)
    return incoming
  }
  const pathsValid = structuredCfgPathsValid(fn, byId, loops, {
    initial: new Set<LoanPathState>(['Dormant']),
    transfer,
    terminal,
    repeat,
    merge: mergePathStates,
  })
  return valid && pathsValid
}

/**
 * Tracks one direct reborrow relationship as correlated state over the whole control-flow graph.
 * The per-loan lifetime pass above proves each endpoint count; this pass proves that a parent is
 * live whenever its child begins and cannot end while that child remains live, including when the
 * endpoints occur in different regions.
 */
const loanAncestryPathsValid = (
  fn: MirFunction,
  parentKey: string,
  childKey: string,
  byId: ReadonlyMap<number, Region>,
  loops: ReadonlyMap<number, LoopRegion>,
): boolean => {
  const parentBit = 1
  const childBit = 2
  let valid = true
  const endpoint = (
    states: ReadonlySet<number>,
    operation: Extract<Operation, { readonly _tag: 'BeginLoan' | 'EndLoan' }>,
  ): ReadonlySet<number> => {
    const key = borrowKey(operation.borrow)
    if (key !== parentKey && key !== childKey) return states
    const next = new Set<number>()
    for (const state of states) {
      if (key === parentKey) {
        if (operation._tag === 'BeginLoan') next.add(state | parentBit)
        else {
          if ((state & childBit) !== 0) valid = false
          next.add(state & ~parentBit)
        }
      } else if (operation._tag === 'BeginLoan') {
        if ((state & parentBit) === 0) valid = false
        next.add(state | childBit)
      } else next.add(state & ~childBit)
    }
    return next
  }
  const transfer = (operation: Operation, incoming: ReadonlySet<number>): ReadonlySet<number> => {
    if (operation._tag === 'BeginLoan' || operation._tag === 'EndLoan')
      return endpoint(incoming, operation)
    return incoming
  }
  const pathsValid = structuredCfgPathsValid(fn, byId, loops, {
    initial: new Set([0]),
    transfer,
    terminal: () => undefined,
    repeat: (states) => states,
    merge: mergePathStates,
  })
  return valid && pathsValid
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

const sameSuspensionPoint = (left: SuspensionPointId, right: SuspensionPointId): boolean =>
  instanceText(left.owner) === instanceText(right.owner) &&
  left.sourceId === right.sourceId &&
  left.spanStart === right.spanStart &&
  left.spanEnd === right.spanEnd &&
  left.ordinal === right.ordinal

const sameLocalSequence = (left: ReadonlyArray<LocalId>, right: ReadonlyArray<LocalId>): boolean =>
  left.length === right.length &&
  left.every((local, ordinal) => local.ordinal === right.at(ordinal)?.ordinal)

const sameEffectContract = (left: SilkType.Effect, right: SilkType.Effect): boolean =>
  SilkType.equals(
    Object.freeze({ ...left, access: 'Shared' }),
    Object.freeze({ ...right, access: 'Shared' }),
  )

const sameEffectChannels = (left: SilkType.Effect, right: SilkType.Effect): boolean =>
  SilkType.equals(left.success, right.success) &&
  SilkType.failureMembers(left).length === SilkType.failureMembers(right).length &&
  SilkType.failureMembers(left).every((failure, ordinal) => {
    const candidate = SilkType.failureMembers(right).at(ordinal)
    return candidate !== undefined && SilkType.equals(failure, candidate)
  })

const suspensionBorrowText = (borrow: SuspensionBorrowIdentity): string =>
  borrow._tag === 'MirLoan'
    ? `loan:${borrowKey(borrow.borrow)}`
    : borrow._tag === 'BorrowedParameter'
      ? `parameter:${borrow.parameterOrdinal}`
      : `local:${borrow.local.ordinal}`

const coroutineFrameReleaseText = (release: CoroutineFrameRelease): string =>
  `${release.local.ordinal}:${release.cleanup._tag}:${SilkType.key(release.cleanup.type)}`

const providerText = (provider: SuspensionProviderArgument): string =>
  `${SilkType.key(provider.capability)}@${provider.role}:${provider.requirementAccess}:${provider.access}:${SilkType.key(provider.providerType)}:${provider.argument?.ordinal ?? 'none'}:${provider.argumentLane ?? 0}:${provider.witness?._tag ?? 'none'}:${provider.purposes.join('+')}`

const runnerText = (runner: SuspensionRunner): string =>
  [
    runner.classification,
    runner.declaration === undefined ? 'unknown' : targetText(runner.declaration),
    runner.instance === undefined ? 'unknown' : instanceText(runner.instance),
    runner.effectIdentity ?? 'none',
    runner.typeArguments.map(SilkType.genericArgumentKey).join(','),
    SilkType.key(Object.freeze({ ...runner.outcome, access: 'Shared' })),
    runner.captures
      .map(
        (capture) =>
          `${capture.ordinal}:${capture.source}:${capture.sourceOrdinal}:${capture.access}:${SilkType.key(capture.type)}`,
      )
      .join(','),
    runner.providers.map(providerText).join(','),
  ].join('|')

const suspensionViolations = (fn: MirFunction, layout: Layout.Plan): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = []
  const projectedProviderValid = (
    provider: SuspensionProviderArgument & { readonly argument: LocalId },
  ): boolean => {
    const runtimeAccess = provider.access === 'Take' ? 'Exclusive' : provider.access
    const argumentType = fn.localTypes.at(provider.argument.ordinal)
    const laneOrdinal = provider.argumentLane ?? 0
    if (!Number.isInteger(laneOrdinal) || laneOrdinal < 0) return false
    if (argumentType?._tag === 'EffectValue') {
      const lane = Layout.effectEnvironmentLanes(layout, argumentType.environment).at(laneOrdinal)
      return (
        lane !== undefined &&
        typeof lane.type !== 'string' &&
        lane.type._tag === 'Address' &&
        SilkType.equals(lane.type.element, provider.providerType)
      )
    }
    if (laneOrdinal !== 0) return false
    return (
      (argumentType?._tag === 'Reference' &&
        (argumentType.type.access === runtimeAccess ||
          (runtimeAccess === 'Shared' && argumentType.type.access === 'Exclusive')) &&
        SilkType.equals(argumentType.type.target, provider.providerType)) ||
      (argumentType?._tag === 'EffectBorrow' &&
        (argumentType.access === runtimeAccess ||
          (runtimeAccess === 'Shared' && argumentType.access === 'Exclusive')) &&
        SilkType.equals(argumentType.type, provider.providerType))
    )
  }
  const invalid = (
    rule: Extract<
      Violation['rule'],
      'InvalidSuspension' | 'InvalidCoroutineFrame' | 'OrphanSuspensionMachinery'
    >,
    detail: string,
    region?: RegionId,
  ): void => {
    violations.push(
      Object.freeze({
        _tag: 'Violation',
        rule,
        function: fn.id,
        ...(region === undefined ? {} : { region }),
        detail,
      }),
    )
  }
  const suspension = fn.suspension
  if (suspension === undefined) return Object.freeze([])
  for (const local of suspensionLocals(fn))
    if (local.ordinal < 0 || local.ordinal >= fn.localTypes.length)
      invalid(
        'InvalidCoroutineFrame',
        `suspension control references undeclared local %${local.ordinal}`,
      )
  if (suspension.classification === 'Synchronous' && suspension.regions.length > 0)
    invalid('InvalidSuspension', 'synchronous execution contains suspension control')
  const points = new Set<string>()
  for (const region of suspension.regions) {
    const pointKey = `${instanceText(region.point.owner)}:${region.point.sourceId}:${region.point.spanStart}:${region.point.spanEnd}:${region.point.ordinal}`
    if (
      points.has(pointKey) ||
      instanceText(region.point.owner) !== instanceText(fn.instance) ||
      region.point.spanStart < 0 ||
      region.point.spanStart > region.point.spanEnd
    )
      invalid(
        'InvalidSuspension',
        'suspension point identity is duplicate or disagrees with its owner',
      )
    points.add(pointKey)
    if (region._tag === 'SuspendEffectRegion') {
      if (region.transfer._tag !== 'OriginateTransfer')
        invalid('InvalidSuspension', 'explicit suspension must originate one transfer')
      const owning = fn.regions.find(
        (candidate) => candidate.id.ordinal === region.ownerRegion.ordinal,
      )
      const operationPresent =
        owning !== undefined &&
        operationsOf(owning).flatMap(operationTree).includes(region.operation)
      if (!operationPresent)
        invalid(
          'InvalidSuspension',
          'explicit suspension references no operation in its owner region',
        )
      if (!sameEffectContract(region.operation.outcomeType.type, region.deferred.outcome))
        invalid(
          'InvalidSuspension',
          'explicit suspension child outcome disagrees with its run carrier',
        )
      continue
    }
    if (
      region.complete._tag !== 'CompleteInCurrentActivation' ||
      region.relay._tag !== 'RelayExistingTransfer' ||
      region.relay.preserves.join(',') !== 'Child,Origin,TypedOutcome' ||
      region.runner.classification === 'Synchronous'
    )
      invalid(
        'InvalidSuspension',
        'suspendable run must complete locally or relay the unchanged transfer identity',
        region.ownerRegion,
      )
    const owning = fn.regions.find(
      (candidate) => candidate.id.ordinal === region.ownerRegion.ordinal,
    )
    const operationPresent =
      owning !== undefined && operationsOf(owning).flatMap(operationTree).includes(region.operation)
    if (!operationPresent)
      invalid('InvalidSuspension', 'suspendable run references no operation in its owner region')
    const operationOutcome = region.operation.outcomeType.type
    if (
      !sameEffectContract(operationOutcome, region.runner.outcome) ||
      !sameEffectChannels(region.completion.outcome, operationOutcome)
    )
      invalid(
        'InvalidSuspension',
        `runner, operation, and completion outcome contracts disagree: operation=${SilkType.encode(operationOutcome)} runner=${SilkType.encode(region.runner.outcome)} completion=${SilkType.encode(region.completion.outcome)}`,
      )
    const operationRunner =
      region.operation._tag === 'RunEffect' ? region.operation.target : region.operation.runner
    const operationTypeArguments =
      region.operation._tag === 'RunEffect'
        ? region.operation.typeArguments
        : region.operation.runnerTypeArguments
    if (
      region.runner.declaration === undefined ||
      region.runner.declaration.module !== operationRunner.module ||
      region.runner.declaration.name !== operationRunner.name ||
      region.runner.typeArguments.map(SilkType.genericArgumentKey).join(',') !==
        operationTypeArguments.map(SilkType.genericArgumentKey).join(',')
    )
      invalid('InvalidSuspension', 'suspension runner identity disagrees with its exact MIR call')
    if (
      (region.completion._tag === 'Propagate' &&
        (region.operation._tag === 'ReifyEffect' ||
          region.completion.failureMappings.length !==
            SilkType.failureMembers(region.operation.outcomeType.type).length ||
          region.completion.failureMappings.some((mapping, ordinal) => {
            const source = SilkType.failureMembers(region.operation.outcomeType.type).at(ordinal)
            const selectedSource = SilkType.failureCarrierMember(
              region.operation.outcomeType.type,
              mapping.source,
              'OneBased',
            )
            const target = SilkType.failureCarrierMember(
              region.completion.outcome,
              mapping.target,
              'OneBased',
            )
            return (
              mapping.source !== ordinal + 1 ||
              source === undefined ||
              selectedSource === undefined ||
              target === undefined ||
              !SilkType.equals(source, selectedSource) ||
              !SilkType.equals(source, target)
            )
          }))) ||
      (region.completion._tag === 'Reify' &&
        (region.operation._tag !== 'ReifyEffect' ||
          !SilkType.equals(region.completion.resultType, region.operation.resultType.type) ||
          region.completion.resultField.ordinal !== region.operation.resultField.ordinal ||
          region.completion.successTag !== region.operation.successTag ||
          region.completion.failureTag !== region.operation.failureTag))
    )
      invalid('InvalidSuspension', 'typed completion mapping disagrees with its MIR operation')
    for (const provider of region.runner.providers) {
      const argumentValid =
        provider.argument === undefined ||
        projectedProviderValid(Object.freeze({ ...provider, argument: provider.argument }))
      const purposeValid = provider.purposes.join(',') === 'ChildRequirement'
      if (!argumentValid || !purposeValid)
        invalid(
          'InvalidCoroutineFrame',
          'provider argument has incompatible local, type, or purpose',
        )
    }
    const descriptor = region.relay.state
    if (region.relay.frame === 'MissingOwnershipPlan')
      invalid(
        'InvalidCoroutineFrame',
        'suspendable run has no exact post-normalization ownership plan',
      )
    if (descriptor === undefined) {
      invalid('InvalidCoroutineFrame', 'suspendable invocation omits its coroutine-frame state')
      continue
    }
    if (
      !sameSuspensionPoint(descriptor.point, region.point) ||
      !sameSuspensionPoint(descriptor.success.resume.point, region.point) ||
      !sameSuspensionPoint(descriptor.failure.resume.point, region.point) ||
      descriptor.success.resume.path !== 'Success' ||
      descriptor.failure.resume.path !== 'Failure'
    )
      invalid(
        'InvalidCoroutineFrame',
        'continuation has missing or ambiguous stable resume identities',
      )
    if (
      runnerText(descriptor.runner) !== runnerText(region.runner) ||
      !sameEffectContract(descriptor.outcome, region.runner.outcome)
    )
      invalid('InvalidCoroutineFrame', 'continuation runner or typed outcome is stale')
    const slots = descriptor.slots
    const slotOrdinals = slots.map((slot) => slot.ordinal)
    const localOrdinals = slots.map((slot) => slot.local.ordinal)
    const expectedOrdinals = slots.map((_slot, ordinal) => ordinal)
    if (
      new Set(localOrdinals).size !== localOrdinals.length ||
      slotOrdinals.join(',') !== expectedOrdinals.join(',') ||
      !sameLocalSequence(
        [...region.liveLocals].sort((left, right) => left.ordinal - right.ordinal),
        slots.map((slot) => slot.local),
      )
    )
      invalid(
        'InvalidCoroutineFrame',
        'logical layout omits, duplicates, or reorders a post-normalization live local',
      )
    for (const slot of slots) {
      const declared = fn.localTypes.at(slot.local.ordinal)
      const accessValid =
        slot.access._tag === 'Copy'
          ? isCopy(layout, semanticType(slot.type))
          : slot.access._tag === 'BorrowedDependency'
            ? slot.type._tag === 'Reference' ||
              slot.type._tag === 'Slice' ||
              slot.type._tag === 'EffectBorrow'
            : !isCopy(layout, semanticType(slot.type))
      if (
        declared === undefined ||
        !SilkType.equals(semanticType(declared), semanticType(slot.type)) ||
        !accessValid
      )
        invalid(
          'InvalidCoroutineFrame',
          `continuation slot %${slot.local.ordinal} has incompatible type or access`,
        )
    }
    if (
      descriptor.success.restores.join(',') !== expectedOrdinals.join(',') ||
      descriptor.failure.restores.length !== 0
    )
      invalid('InvalidCoroutineFrame', 'resume path plan is incomplete')
    if (descriptor.success.loanEnds.length !== 0 || descriptor.success.releases.length !== 0)
      invalid('InvalidCoroutineFrame', 'success or failure cleanup plan diverges')
  }
  return Object.freeze(violations)
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

/** Every local named by one operation, including definitions and structured child results. */
export const operationLocals = (operation: Operation): ReadonlyArray<LocalId> => {
  switch (operation._tag) {
    case 'Literal':
    case 'StaticView':
    case 'StaticString':
      return [operation.destination]
    case 'StringFromUtf8Unchecked':
      return [operation.destination, operation.bytes]
    case 'StringUtf8Bytes':
    case 'StringByteLength':
      return [operation.destination, operation.string]
    case 'StringEqualsExact':
      return [operation.destination, operation.left, operation.right]
    case 'PackEffectComposite':
      return [operation.destination, operation.source]
    case 'Binary':
      return [operation.destination, operation.left, operation.right]
    case 'ConvertInteger':
    case 'ConvertScalar':
    case 'ReinterpretScalar':
      return [operation.destination, operation.source]
    case 'FloatUnary':
    case 'FloatTranscendental':
      return [operation.destination, operation.source]
    case 'CheckedInteger':
      return [operation.destination, ...operation.operands]
    case 'ValidateLayout':
      return [operation.destination, operation.bytes, operation.alignment]
    case 'RepeatLayout':
      return [operation.destination, operation.layout, operation.count]
    case 'Allocate':
      return [operation.destination, operation.layout]
    case 'HostWrite':
      return [operation.destination, operation.stream, operation.bytes]
    case 'OsCall':
      return [operation.destination, ...operation.arguments]
    case 'RawBufferFrom':
      return [operation.destination, operation.allocation, operation.count]
    case 'RawBufferCount':
      return [operation.destination, operation.buffer]
    case 'RawBufferSlot':
      return [operation.destination, operation.buffer, operation.index]
    case 'RawBufferRead':
      return [operation.destination, operation.buffer, operation.index]
    case 'RawBufferView':
      return [operation.destination, operation.buffer, operation.offset, operation.length]
    case 'RawBufferCopy':
      return [
        operation.destination,
        operation.buffer,
        operation.offset,
        operation.source,
        operation.length,
      ]
    case 'RawBufferFill':
      return [
        operation.destination,
        operation.buffer,
        operation.offset,
        operation.length,
        operation.value,
      ]
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
    case 'PackEffectFailureUnion':
    case 'UnpackEffectSuccess':
      return [operation.destination, operation.source]
    case 'PropagateEffectFailure':
      return [operation.source, ...(operation.releases ?? []).map((release) => release.local)]
    case 'RunEffect':
      return [operation.destination, operation.outcome, ...operation.arguments]
    case 'RunEffectValue':
      return [operation.destination, operation.outcome, operation.effect, ...operation.arguments]
    case 'RunEffectComposite':
      return [
        operation.destination,
        operation.outcome,
        operation.effect,
        ...operation.alternatives.flatMap((alternative) => alternative.arguments),
      ]
    case 'RunStaticEffect':
      return [
        operation.destination,
        operation.outcome,
        ...operation.captures.map((capture) => capture.source),
        ...operation.arguments,
      ]
    case 'ReifyEffect':
      return [operation.destination, operation.outcome, operation.effect, ...operation.arguments]
    case 'CloseEffectEntry':
      return [
        operation.destination,
        operation.effect,
        operation.outcome,
        ...operation.failures.map((failure) => failure.payload),
      ]
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
    case 'ShortCircuit':
      return [operation.destination, operation.left, operation.right.result]
  }
}

const localUses = (region: Region): ReadonlyArray<LocalId> => [
  ...operationsOf(region).flatMap(operationTree).flatMap(operationLocals),
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
  dereferenceReference = false,
): DeclarationIndex.SemanticType | undefined => {
  const rootType = fn.localTypes.at(root.ordinal)
  let current = rootType === undefined ? undefined : semanticType(rootType)
  // A reference root reads and writes through the borrow, so the place is on its target.
  if (
    current !== undefined &&
    SilkType.isReference(current) &&
    (selectors.length > 0 || dereferenceReference)
  ) {
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
  left: ReadonlyArray<SilkType.Type>,
  right: ReadonlyArray<SilkType.Type>,
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

const storedCallableTargetText = (target: SilkType.CallableIdentityArgument['target']): string =>
  callableTargetText(Hir.callableTargetFromIdentity(target))

const storedExecutableText = (
  stored: NonNullable<
    Extract<Operation, { readonly _tag: 'Construct' }>['fields'][number]['stored']
  >,
): string =>
  stored._tag === 'StoredCallableField'
    ? storedCallableTargetText(stored.realization.target)
    : targetText(stored.realization.runner)

const borrowKey = (borrow: Hir.BorrowId): string =>
  `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`

const instanceText = Instances.keyText

const callArgumentCompatible = (actual: Type, expected: Type): boolean => {
  if (
    TypeCompatibility.isCompatible(
      TypeCompatibility.check(semanticType(actual), semanticType(expected)),
    )
  )
    return true
  if (
    actual._tag !== 'EffectValue' ||
    expected._tag !== 'EffectValue' ||
    actual.storage !== undefined ||
    expected.storage?._tag !== 'StoredEffectField'
  )
    return false
  const realization = expected.storage.realization
  return (
    SilkType.equals(actual.type, realization.contract) &&
    Hir.sameExecutableSite(actual.site, realization.site) &&
    instanceText(actual.environment.instance) === instanceText(realization.runnerInstance)
  )
}

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
    case 'EffectCleanup':
      return [cleanup.type, ...cleanup.slots.flatMap((slot) => cleanupTypes(slot.cleanup))]
    case 'EffectCompositeCleanup':
      return [
        cleanup.type,
        ...cleanup.alternatives.flatMap((alternative) => cleanupTypes(alternative)),
      ]
    case 'RepresentedCallableCleanup':
    case 'RepresentedEffectCleanup':
      return [cleanup.type, cleanup.contract]
  }
}

type EffectEnvironment = Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }>

type CallableEnvironment = Extract<
  Layout.CallableEnvironment,
  { readonly _tag: 'CallableEnvironment' }
>

const effectEnvironmentByIdentity = (
  layout: Layout.Plan,
  identity: string,
): EffectEnvironment | undefined =>
  layout.effectEnvironments.find(
    (candidate): candidate is EffectEnvironment =>
      candidate._tag === 'EffectEnvironment' &&
      (Instances.effectIdentity(candidate.instance, candidate.site) === identity ||
        candidate.successEffectIdentity === identity),
  )

const callableEnvironmentByIdentity = (
  layout: Layout.Plan,
  identity: SilkType.CallableIdentityArgument,
): CallableEnvironment | undefined =>
  layout.callableEnvironments.find(
    (candidate): candidate is CallableEnvironment =>
      candidate._tag === 'CallableEnvironment' &&
      CallableFieldRealization.matchesIdentity(identity, candidate.callable),
  )

const effectFieldLaneCount = (
  layout: Layout.Plan,
  field: Layout.EffectEnvironmentField,
): number | undefined => {
  if (field.representation === 'Borrow') return 1
  if (field.effectIdentity !== undefined) {
    const environment = effectEnvironmentByIdentity(layout, field.effectIdentity)
    return environment === undefined
      ? undefined
      : Layout.effectEnvironmentLanes(layout, environment).length
  }
  if (field.callableIdentity !== undefined) {
    const environment = callableEnvironmentByIdentity(layout, field.callableIdentity)
    return environment === undefined
      ? undefined
      : Layout.callableEnvironmentLanes(layout, environment).length
  }
  return Layout.callingShape(layout, field.type)?.laneCount
}

const callableEnvironmentCleanupValid = (
  layout: Layout.Plan,
  identity: SilkType.CallableIdentityArgument,
  expectedType: SilkType.Callable,
  cleanup: Ownership.CleanupPlan,
  active: ReadonlySet<string>,
): boolean => {
  const environment = callableEnvironmentByIdentity(layout, identity)
  if (environment === undefined || cleanup._tag !== 'CallableCleanup') return false
  const environmentIdentity = Instances.callableEnvironmentIdentity(environment.callable)
  const key = `callable:${SilkType.callableEnvironmentKey(environmentIdentity)}`
  if (active.has(key)) return false
  const expected = [...environment.fields]
    .reverse()
    .filter((field) => field.access === 'Take' && !isCopy(layout, field.type))
  const next = new Set(active).add(key)
  return (
    SilkType.equals(cleanup.type, expectedType) &&
    cleanup.environment._tag === 'CallableEnvironmentIdentity' &&
    SilkType.equalsCallableEnvironmentIdentity(cleanup.environment.identity, environmentIdentity) &&
    cleanup.slots.length === expected.length &&
    cleanup.slots.every((slot, ordinal) => {
      const field = expected.at(ordinal)
      return (
        field !== undefined &&
        slot.ordinal === field.ordinal &&
        cleanupMatchesSemanticType(layout, slot.cleanup, field.type, next)
      )
    })
  )
}

const executableFieldCleanupValid = (
  layout: Layout.Plan,
  field: Layout.EffectEnvironmentField,
  cleanup: Ownership.CleanupPlan,
  active: ReadonlySet<string>,
): boolean => {
  if (field.effectIdentity !== undefined) {
    const environment = effectEnvironmentByIdentity(layout, field.effectIdentity)
    return (
      environment !== undefined &&
      effectEnvironmentCleanupValid(layout, field.effectIdentity, environment, cleanup, active)
    )
  }
  if (field.callableIdentity !== undefined && SilkType.isCallable(field.type))
    return callableEnvironmentCleanupValid(
      layout,
      field.callableIdentity,
      field.type,
      cleanup,
      active,
    )
  return cleanupMatchesSemanticType(layout, cleanup, field.type, active)
}

const effectEnvironmentCleanupValid = (
  layout: Layout.Plan,
  identity: string,
  environment: EffectEnvironment,
  cleanup: Ownership.CleanupPlan,
  active: ReadonlySet<string>,
): boolean => {
  const key = `effect:${identity}`
  if (active.has(key))
    return cleanup._tag === 'NoCleanup' && SilkType.equals(cleanup.type, environment.effect)
  const next = new Set(active).add(key)
  let laneOffset = 0
  const expected = environment.fields.flatMap((field, ordinal) => {
    const laneCount = effectFieldLaneCount(layout, field)
    const currentOffset = laneOffset
    if (laneCount !== undefined) laneOffset += laneCount
    const noCleanup: Ownership.CleanupPlan = Object.freeze({
      _tag: 'NoCleanup',
      type: field.type,
    })
    return field.representation === 'Borrow' ||
      executableFieldCleanupValid(layout, field, noCleanup, next)
      ? []
      : [Object.freeze({ field, ordinal, laneOffset: currentOffset, laneCount })]
  })
  if (expected.length === 0)
    return cleanup._tag === 'NoCleanup' && SilkType.equals(cleanup.type, environment.effect)
  return (
    cleanup._tag === 'EffectCleanup' &&
    SilkType.equals(cleanup.type, environment.effect) &&
    Hir.sameExecutableSite(cleanup.site, environment.site) &&
    cleanup.slots.length === expected.length &&
    cleanup.slots.every((slot, ordinal) => {
      const candidate = [...expected].reverse().at(ordinal)
      return (
        candidate !== undefined &&
        candidate.laneCount !== undefined &&
        slot.ordinal === candidate.ordinal &&
        slot.laneOffset === candidate.laneOffset &&
        slot.laneCount === candidate.laneCount &&
        executableFieldCleanupValid(layout, candidate.field, slot.cleanup, next)
      )
    })
  )
}

const cleanupMatchesSemanticType = (
  layout: Layout.Plan,
  cleanup: Ownership.CleanupPlan,
  type: DeclarationIndex.SemanticType,
  seen: ReadonlySet<string> = new Set(),
): boolean => {
  if (isCopy(layout, type))
    return cleanup._tag === 'NoCleanup' && SilkType.equals(cleanup.type, type)
  if (SilkType.isRepresented(type)) {
    const composite = type.representation.argument
    if (SilkType.isCompositeEffectRepresentationArgument(composite)) {
      if (
        cleanup._tag !== 'EffectCompositeCleanup' ||
        !SilkType.equals(cleanup.type, type) ||
        cleanup.alternatives.length !== composite.alternatives.length
      )
        return false
      return composite.alternatives.every((alternative, ordinal) => {
        if (!SilkType.isEffectIdentityArgument(alternative.identity)) return false
        const identity = alternative.identity
        const environment = layout.effectEnvironments.find(
          (candidate): candidate is EffectEnvironment =>
            candidate._tag === 'EffectEnvironment' &&
            Hir.effectRepresentationIdentity(candidate.site) === identity.identity &&
            identity.owner !== undefined &&
            candidate.instance.declaration.module === identity.owner.declaration.module &&
            candidate.instance.declaration.name === identity.owner.declaration.name &&
            candidate.instance.typeArguments.length === identity.owner.typeArguments.length &&
            candidate.instance.typeArguments.every((argument, argumentOrdinal) => {
              const expected = identity.owner?.typeArguments.at(argumentOrdinal)
              return expected !== undefined && SilkType.equalsGenericArgument(argument, expected)
            }),
        )
        const selected = cleanup.alternatives.at(ordinal)
        return (
          environment !== undefined &&
          selected !== undefined &&
          effectEnvironmentCleanupValid(
            layout,
            Instances.effectIdentity(environment.instance, environment.site),
            environment,
            selected,
            seen,
          )
        )
      })
    }
    const representation = Layout.entry(layout, type)?.representation
    const contractValid = TypeCompatibility.isCompatible(
      TypeCompatibility.check(type.contract, cleanup.type),
    )
    if (representation?._tag === 'CallableEnvironment')
      return (cleanup._tag === 'CallableCleanup' || cleanup._tag === 'NoCleanup') && contractValid
    if (representation?._tag === 'StoredEffectEnvironment')
      return (
        contractValid &&
        (cleanup._tag === 'EffectCleanup'
          ? storedEffectCleanupPlanValid(layout, type, representation, cleanup)
          : cleanup._tag === 'NoCleanup' &&
            representation.realization.cleanup.unrunLanes.length === 0)
      )
    return false
  }
  if (!SilkType.equals(cleanup.type, type)) return false
  if (
    SilkType.isBuiltin(type) ||
    SilkType.isString(type) ||
    SilkType.isNever(type) ||
    SilkType.isSlice(type) ||
    SilkType.isReference(type) ||
    SilkType.isEffect(type) ||
    SilkType.isCallable(type)
  )
    return cleanup._tag === 'NoCleanup'
  if (SilkType.equals(type, SilkType.allocation)) return cleanup._tag === 'AllocationCleanup'
  if (SilkType.isRawBuffer(type)) return cleanup._tag === 'RawBufferCleanup'
  if (SilkType.isFixedArray(type))
    return (
      cleanup._tag === 'ArrayCleanup' &&
      cleanup.length === type.length &&
      cleanupMatchesSemanticType(layout, cleanup.element, type.element, seen)
    )
  if (SilkType.isUnion(type))
    return (
      cleanup._tag === 'UnionCleanup' &&
      cleanup.cases.length === type.members.length &&
      cleanup.cases.every((entry, ordinal) => {
        const member = type.members.at(ordinal)
        return (
          member !== undefined &&
          entry.ordinal === ordinal &&
          SilkType.equals(entry.member, member) &&
          cleanupMatchesSemanticType(layout, entry.cleanup, member, seen)
        )
      })
    )
  if (!SilkType.isNominal(type)) return cleanup._tag === 'NoCleanup'
  const key = SilkType.key(type)
  if (seen.has(key)) return cleanup._tag === 'NoCleanup'
  const representation = Layout.entry(layout, type)?.representation
  if (representation?._tag !== 'Aggregate') return cleanup._tag === 'NoCleanup'
  const requiredHook = representation.cleanupHook
  if (requiredHook !== undefined) {
    if (
      cleanup._tag !== 'HookCleanup' ||
      cleanup.hook.module !== requiredHook.hook.module ||
      cleanup.hook.name !== requiredHook.hook.name ||
      cleanup.typeArguments.length !== requiredHook.typeArguments.length ||
      !cleanup.typeArguments.every((argument, ordinal) => {
        const expected = requiredHook.typeArguments.at(ordinal)
        return expected !== undefined && SilkType.equalsGenericArgument(argument, expected)
      })
    )
      return false
  } else if (cleanup._tag === 'HookCleanup') return false
  const concrete = cleanup._tag === 'HookCleanup' ? cleanup.inner : cleanup
  if (concrete._tag !== 'StructCleanup' || concrete.fields.length !== representation.fields.length)
    return false
  const next = new Set(seen).add(key)
  return concrete.fields.every((field, ordinal) => {
    const expected = representation.fields.at(ordinal)
    return (
      expected !== undefined &&
      field.field.ordinal === expected.id.ordinal &&
      field.field.struct.sourceId === expected.id.struct.sourceId &&
      field.field.struct.ordinal === expected.id.struct.ordinal &&
      cleanupMatchesSemanticType(layout, field.cleanup, expected.type, next)
    )
  })
}

const storedEffectCleanupPlanValid = (
  layout: Layout.Plan,
  type: SilkType.Represented,
  representation: Extract<Layout.Representation, { readonly _tag: 'StoredEffectEnvironment' }>,
  cleanup: Extract<Ownership.CleanupPlan, { readonly _tag: 'EffectCleanup' }>,
): boolean => {
  if (!Hir.sameExecutableSite(cleanup.site, representation.realization.site)) return false
  const shape = Layout.callingShape(layout, type)?.tree
  if (shape?._tag !== 'EffectEnvironmentShape') return false
  const ranges = representation.fields.map((field, ordinal) => {
    const fieldShape = shape.fields.at(ordinal)
    const laneOffset = shape.fields
      .slice(0, ordinal)
      .reduce((total, candidate) => total + candidate.shape.laneCount, 0)
    return Object.freeze({ field, fieldShape, laneOffset })
  })
  const expected = [...representation.realization.cleanup.unrunLanes].reverse().flatMap((owned) => {
    const range = ranges.find((candidate) => candidate.field.capture === owned)
    return range === undefined ? [] : [Object.freeze({ owned, ...range })]
  })
  const active = new Set([`effect:${representation.realization.runnerIdentity}`])
  return (
    expected.length === representation.realization.cleanup.unrunLanes.length &&
    cleanup.slots.length === expected.length &&
    cleanup.slots.every((slot, ordinal) => {
      const candidate = expected.at(ordinal)
      return (
        candidate !== undefined &&
        candidate.fieldShape !== undefined &&
        slot.ordinal === candidate.owned &&
        slot.laneOffset === candidate.laneOffset &&
        slot.laneCount === candidate.fieldShape.shape.laneCount &&
        executableFieldCleanupValid(layout, candidate.field, slot.cleanup, active)
      )
    })
  )
}

const storedEffectCleanupValid = (
  layout: Layout.Plan,
  dropped: Extract<Type, { readonly _tag: 'EffectValue' }> | undefined,
  cleanup: Extract<Ownership.CleanupPlan, { readonly _tag: 'EffectCleanup' }>,
): boolean => {
  const storage = dropped?.storage
  const representation =
    storage === undefined ? undefined : Layout.entry(layout, storage.type)?.representation
  return (
    storage !== undefined &&
    representation?._tag === 'StoredEffectEnvironment' &&
    storedEffectCleanupPlanValid(layout, storage.type, representation, cleanup)
  )
}

const operationTypes = (operation: Operation): ReadonlyArray<DeclarationIndex.SemanticType> => {
  switch (operation._tag) {
    case 'Literal':
    case 'StaticView':
    case 'Binary':
    case 'ValidateLayout':
    case 'RepeatLayout':
    case 'Allocate':
    case 'HostWrite':
    case 'OsCall':
    case 'Project':
    case 'ReadPlace':
    case 'CheckPlace':
      return [semanticType(operation.type)]
    case 'StaticString':
      return [SilkType.string]
    case 'PackEffectComposite':
      return [semanticType(operation.type)]
    case 'StringFromUtf8Unchecked':
      return [SilkType.slice('Shared', 'u8'), SilkType.string]
    case 'StringUtf8Bytes':
      return [SilkType.string, semanticType(operation.type)]
    case 'StringByteLength':
    case 'StringEqualsExact':
      return [SilkType.string, semanticType(operation.type)]
    case 'ConvertInteger':
    case 'ConvertScalar':
    case 'ReinterpretScalar':
    case 'FloatUnary':
    case 'FloatTranscendental':
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
    case 'RawBufferRead':
    case 'RawBufferView':
    case 'RawBufferCopy':
    case 'SlotWrite':
    case 'SlotTake':
    case 'SlotCopy':
      return [semanticType(operation.type), operation.element]
    case 'RawBufferFill':
      return [semanticType(operation.type)]
    case 'SlotDrop':
      return [semanticType(operation.type), operation.element, ...cleanupTypes(operation.cleanup)]
    case 'BeginLoan':
      return [semanticType(operation.sourceType), semanticType(operation.type)]
    case 'SliceLength':
      return [semanticType(operation.type)]
    case 'EndLoan':
      return []
    case 'Call':
      return [
        semanticType(operation.type),
        ...operation.typeArguments.filter(SilkType.isTypeArgument),
      ]
    case 'MakeEffect':
      return [
        semanticType(operation.type),
        ...operation.runnerTypeArguments.filter(SilkType.isTypeArgument),
      ]
    case 'MakeCallable':
      return [
        semanticType(operation.type),
        ...operation.typeArguments.filter(SilkType.isTypeArgument),
      ]
    case 'ApplyCallable':
      return [
        operation.callableType,
        semanticType(operation.type),
        ...operation.typeArguments.filter(SilkType.isTypeArgument),
      ]
    case 'PackEffectOutcome':
    case 'PackEffectFailureUnion':
    case 'UnpackEffectSuccess':
      return [semanticType(operation.type)]
    case 'PropagateEffectFailure':
      return [semanticType(operation.sourceType), semanticType(operation.propagationType)]
    case 'RunEffect':
      return [
        semanticType(operation.outcomeType),
        ...(operation.propagationType === undefined
          ? []
          : [semanticType(operation.propagationType)]),
        semanticType(operation.type),
        ...operation.typeArguments.filter(SilkType.isTypeArgument),
      ]
    case 'RunEffectValue':
      return [
        semanticType(operation.outcomeType),
        ...(operation.propagationType === undefined
          ? []
          : [semanticType(operation.propagationType)]),
        semanticType(operation.type),
        ...operation.runnerTypeArguments.filter(SilkType.isTypeArgument),
        ...(operation.runnerBase?.typeArguments.filter(SilkType.isTypeArgument) ?? []),
        ...operation.providers.flatMap((provider) => [provider.capability, provider.providerType]),
      ]
    case 'RunEffectComposite':
      return [
        semanticType(operation.outcomeType),
        ...(operation.propagationType === undefined
          ? []
          : [semanticType(operation.propagationType)]),
        semanticType(operation.type),
        ...operation.alternatives.flatMap((alternative) => [
          semanticType(alternative.type),
          ...alternative.runnerTypeArguments.filter(SilkType.isTypeArgument),
        ]),
      ]
    case 'RunStaticEffect':
      return [
        semanticType(operation.outcomeType),
        ...(operation.propagationType === undefined
          ? []
          : [semanticType(operation.propagationType)]),
        semanticType(operation.type),
        ...operation.runnerTypeArguments.filter(SilkType.isTypeArgument),
      ]
    case 'ReifyEffect':
      return [
        semanticType(operation.outcomeType),
        operation.resultType.type,
        operation.resultUnion,
        operation.successType,
        operation.failureType,
        operation.failureValueType,
        ...operation.runnerTypeArguments.filter(SilkType.isTypeArgument),
      ]
    case 'CloseEffectEntry':
      return [
        semanticType(operation.effectType),
        semanticType(operation.outcomeType),
        semanticType(operation.type),
        ...operation.typeArguments.filter(SilkType.isTypeArgument),
        ...operation.failures.flatMap((failure) => [
          failure.type,
          ...cleanupTypes(failure.cleanup),
        ]),
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
    case 'ShortCircuit':
      return [semanticType(operation.type), ...operation.right.operations.flatMap(operationTypes)]
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
    case 'StringFromUtf8Unchecked':
      return [operation.bytes]
    case 'StringUtf8Bytes':
    case 'StringByteLength':
      return [operation.string]
    case 'StringEqualsExact':
      return [operation.left, operation.right]
    case 'PackEffectComposite':
      return [operation.source]
    case 'Binary':
      return [operation.left, operation.right]
    case 'ConvertInteger':
    case 'ConvertScalar':
    case 'ReinterpretScalar':
    case 'FloatUnary':
    case 'FloatTranscendental':
      return [operation.source]
    case 'CheckedInteger':
      return operation.operands
    case 'ValidateLayout':
      return [operation.bytes, operation.alignment]
    case 'RepeatLayout':
      return [operation.layout, operation.count]
    case 'Allocate':
      return [operation.layout]
    case 'HostWrite':
      return [operation.stream, operation.bytes]
    case 'OsCall':
      return operation.arguments
    case 'RawBufferFrom':
      return [operation.allocation, operation.count]
    case 'RawBufferCount':
      return [operation.buffer]
    case 'RawBufferSlot':
      return [operation.buffer, operation.index]
    case 'RawBufferRead':
      return [operation.buffer, operation.index]
    case 'RawBufferView':
      return [operation.buffer, operation.offset, operation.length]
    case 'RawBufferCopy':
      return [operation.buffer, operation.offset, operation.source, operation.length]
    case 'RawBufferFill':
      return [operation.buffer, operation.offset, operation.length, operation.value]
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
    case 'PackEffectFailureUnion':
    case 'UnpackEffectSuccess':
      return [operation.source]
    case 'PropagateEffectFailure':
      return [operation.source, ...(operation.releases ?? []).map((release) => release.local)]
    case 'RunEffect':
      return operation.arguments
    case 'RunEffectValue':
      return [operation.effect, ...operation.arguments]
    case 'RunEffectComposite':
      return [
        operation.effect,
        ...operation.alternatives.flatMap((alternative) => alternative.arguments),
      ]
    case 'RunStaticEffect':
      return [...operation.captures.map((capture) => capture.source), ...operation.arguments]
    case 'ReifyEffect':
      return [operation.effect, ...operation.arguments]
    case 'CloseEffectEntry':
      return []
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
    case 'ShortCircuit':
      return [operation.left]
    case 'Literal':
    case 'StaticView':
    case 'StaticString':
    case 'BeginLoan':
    case 'EndLoan':
    case 'SliceLength':
      return []
  }
}

const loanViolations = (
  fn: MirFunction,
  layout: Layout.Plan,
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
    const endLoan = (
      operation: EndLoanOperation,
      currentActive: Map<string, ActiveLoan>,
      currentCompleted: Set<string>,
      currentCalls: ReadonlySet<string>,
    ): void => {
      const key = borrowKey(operation.borrow)
      const loan = currentActive.get(key)
      const beginning = loan?.operation ?? globalBeginnings.get(key)
      const call = `${operation.borrow.callSpan.start}:${operation.borrow.callSpan.end}`
      const liveChild = [...currentActive.values()].some((candidate) => candidate.parent === key)
      if (
        beginning === undefined ||
        currentCompleted.has(key) ||
        beginning.destination.ordinal !== operation.slice.ordinal ||
        (loan !== undefined && !currentCalls.has(call)) ||
        liveChild
      ) {
        invalid(`loan ${key} has a missing, duplicate, premature, or mismatched ending`)
      } else {
        currentActive.delete(key)
        currentCompleted.add(key)
      }
    }
    for (const operation of sequence) {
      if (operation._tag === 'BeginLoan') {
        const key = borrowKey(operation.borrow)
        const source = fn.localTypes.at(operation.root.ordinal)
        const destination = fn.localTypes.at(operation.destination.ordinal)
        const sourceSemantic = semanticType(operation.sourceType)
        const selectedSource = placeType(fn, layout, operation.root, operation.selectors)
        const rootMatchesSource =
          selectedSource !== undefined && SilkType.equals(selectedSource, sourceSemantic)
        const borrowed = operation.type.type
        const sourceElement =
          operation.sourceType._tag === 'FixedArray' || operation.sourceType._tag === 'Slice'
            ? operation.sourceType.type.element
            : undefined
        const sourceReferenceTarget =
          operation.sourceType._tag === 'Reference' ? operation.sourceType.type.target : undefined
        const parent = [...active.entries()].find(
          ([, loan]) => loan.operation.destination.ordinal === operation.root.ordinal,
        )
        const reborrowSource =
          operation.sourceType._tag === 'Slice' || operation.sourceType._tag === 'Reference'
        const reborrowValid = reborrowSource
          ? operation.reborrow &&
            operation.suspendsParent === (operation.sourceType.type.access === 'Exclusive')
          : !operation.reborrow && !operation.suspendsParent
        const parentValid =
          parent === undefined ||
          (reborrowSource &&
            parent[1].operation.access === operation.sourceType.type.access &&
            operation.suspendsParent === (parent[1].operation.access === 'Exclusive'))
        if (
          active.has(key) ||
          completed.has(key) ||
          source === undefined ||
          destination === undefined ||
          !rootMatchesSource ||
          (destination._tag !== 'Slice' && destination._tag !== 'Reference') ||
          !SilkType.equals(destination.type, borrowed) ||
          borrowed.access !== operation.access ||
          (SilkType.isSlice(borrowed)
            ? sourceElement === undefined || !SilkType.equals(borrowed.element, sourceElement)
            : sourceReferenceTarget === undefined
              ? !SilkType.equals(borrowed.target, sourceSemantic)
              : !SilkType.equals(borrowed.target, sourceReferenceTarget)) ||
          (reborrowSource &&
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
        endLoan(operation, active, completed, calls)
        continue
      }

      if (
        operation._tag === 'RunEffect' ||
        operation._tag === 'RunEffectValue' ||
        operation._tag === 'RunStaticEffect'
      ) {
        const failureActive = new Map(active)
        const failureCompleted = new Set(completed)
        for (const ending of operation.failureLoanEnds ?? [])
          endLoan(ending, failureActive, failureCompleted, calls)
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

interface SuspensionCallTarget {
  readonly declaration: DeclarationIndex.CanonicalId
  readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
}

const suspensionCallTargets = (operation: Operation): ReadonlyArray<SuspensionCallTarget> => {
  switch (operation._tag) {
    case 'Call':
    case 'RunEffect':
      return [
        Object.freeze({ declaration: operation.target, typeArguments: operation.typeArguments }),
      ]
    case 'RunEffectValue':
    case 'RunStaticEffect':
    case 'ReifyEffect':
      return [
        Object.freeze({
          declaration: operation.runner,
          typeArguments: operation.runnerTypeArguments,
        }),
      ]
    case 'RunEffectComposite':
      return operation.alternatives.map((alternative) =>
        Object.freeze({
          declaration: alternative.runner,
          typeArguments: alternative.runnerTypeArguments,
        }),
      )
    case 'CloseEffectEntry':
      return [
        Object.freeze({ declaration: operation.target, typeArguments: operation.typeArguments }),
        Object.freeze({ declaration: operation.runner, typeArguments: operation.typeArguments }),
      ]
    case 'ApplyCallable':
      return operation.target?._tag === 'DeclarationCallableTarget'
        ? [
            Object.freeze({
              declaration: operation.target.declaration,
              typeArguments: operation.typeArguments,
            }),
          ]
        : []
    default:
      return []
  }
}

const originReachableSuspensionFunctions = (self: Module): ReadonlySet<string> => {
  const reachable = new Set(
    self.functions
      .filter((fn) =>
        fn.suspension?.regions.some((region) => region._tag === 'SuspendEffectRegion'),
      )
      .map((fn) => instanceText(fn.instance)),
  )
  let changed = true
  while (changed) {
    changed = false
    for (const fn of self.functions) {
      const key = instanceText(fn.instance)
      if (reachable.has(key)) continue
      const finalSuspensionTargets = (fn.suspension?.regions ?? []).flatMap((region) =>
        region._tag === 'RunSuspendableEffectRegion' && region.runner.declaration !== undefined
          ? [
              Object.freeze({
                declaration: region.runner.declaration,
                typeArguments: region.runner.typeArguments,
              }),
            ]
          : [],
      )
      const reachesOrigin = [
        ...operations(fn).flatMap(suspensionCallTargets),
        ...finalSuspensionTargets,
      ].some((target) =>
        self.functions.some(
          (candidate) =>
            reachable.has(instanceText(candidate.instance)) &&
            matchesInstance(candidate, target.declaration, target.typeArguments),
        ),
      )
      if (reachesOrigin) {
        reachable.add(key)
        changed = true
      }
    }
  }
  return reachable
}

const suspensionTypes = (fn: MirFunction): ReadonlyArray<SilkType.Type> =>
  (fn.suspension?.regions ?? []).flatMap((region) => {
    const runner = region._tag === 'SuspendEffectRegion' ? region.deferred : region.runner
    const runnerTypes = [
      ...runner.typeArguments.filter(SilkType.isTypeArgument),
      runner.outcome,
      ...runner.captures.map((capture) => capture.type),
      ...runner.providers.flatMap((provider) => [provider.capability, provider.providerType]),
    ]
    if (region._tag === 'SuspendEffectRegion') return runnerTypes
    const completionTypes =
      region.completion._tag === 'Propagate'
        ? [region.completion.outcome]
        : [
            region.completion.outcome,
            region.completion.resultType,
            region.completion.resultUnion,
            region.completion.successType,
            region.completion.failureType,
            region.completion.failureValueType,
          ]
    const descriptor = region.relay.state
    if (descriptor === undefined) return [...runnerTypes, ...completionTypes]
    const releases = [...descriptor.success.releases, ...descriptor.failure.releases]
    return [
      ...runnerTypes,
      ...completionTypes,
      descriptor.outcome,
      ...descriptor.slots.flatMap((slot) => [
        semanticType(slot.type),
        ...(slot.access._tag === 'AffineTransfer' ? cleanupTypes(slot.access.cleanup) : []),
      ]),
      ...releases.flatMap((release) => cleanupTypes(release.cleanup)),
    ]
  })

const coroutineFrameLayoutViolations = (self: Module): ReadonlyArray<Violation> => {
  const invalid = (detail: string, fn?: MirFunction): Violation =>
    Object.freeze(
      fn === undefined
        ? { _tag: 'Violation', rule: 'InvalidCoroutineFrame', detail }
        : { _tag: 'Violation', rule: 'InvalidCoroutineFrame', function: fn.id, detail },
    )
  const descriptors = self.functions.flatMap((fn) =>
    fn.suspension?.frame === undefined
      ? []
      : [Object.freeze({ fn, descriptor: fn.suspension.frame })],
  )
  if (descriptors.length === 0)
    return self.coroutineFrames === undefined
      ? Object.freeze([])
      : Object.freeze([invalid('MIR without frames retains a coroutine-frame layout plan')])
  if (self.coroutineFrames === undefined)
    return Object.freeze([invalid('frame-producing suspension has no target-layout plan')])
  const violations: Array<Violation> = []
  if (self.coroutineFrames.target.id !== self.layout.target.id)
    violations.push(invalid('coroutine-frame layout disagrees with the MIR target'))
  const matched = new Set<number>()
  for (const { fn, descriptor } of descriptors) {
    const candidates = self.coroutineFrames.entries
      .map((entry, ordinal) => Object.freeze({ entry, ordinal }))
      .filter(({ entry }) => instanceText(entry.function) === instanceText(descriptor.function))
    const selected = candidates.at(0)
    if (selected === undefined || candidates.length !== 1) {
      violations.push(invalid('coroutine-frame descriptor must own exactly one maximum layout', fn))
      continue
    }
    matched.add(selected.ordinal)
    const entry = selected.entry
    const wordSize = self.layout.target.pointerSize
    const wordAlignment = self.layout.target.pointerAlignment
    const roles: ReadonlyArray<CoroutineFrameHeaderRole> = ['Parent', 'State']
    const headerValid =
      entry.header.length === roles.length &&
      entry.header.every(
        (field, ordinal) =>
          field.role === roles.at(ordinal) &&
          field.offset === ordinal * wordSize &&
          field.size === wordSize &&
          field.alignment === wordAlignment,
      )
    const stateValid = descriptor.states.every((state) => {
      const candidates = entry.states.filter((layout) =>
        sameSuspensionPoint(layout.point, state.point),
      )
      const layout = candidates.at(0)
      if (layout === undefined || candidates.length !== 1) return false
      let cursor = roles.length * wordSize
      let alignment: number = wordAlignment
      const payloadValid =
        layout.payload.length === state.slots.length &&
        layout.payload.every((field, ordinal) => {
          const slot = state.slots.at(ordinal)
          if (slot === undefined) return false
          const physical =
            slot.access._tag === 'BorrowedDependency' || slot.type._tag === 'EffectBorrow'
              ? Object.freeze({ size: wordSize, alignment: wordAlignment })
              : slot.type._tag === 'EffectValue'
                ? slot.type.environment
                : slot.type._tag === 'CallableValue'
                  ? (slot.type.environment?.view ??
                    Object.freeze({ size: wordSize * 2, alignment: wordAlignment }))
                  : Layout.entry(self.layout, semanticType(slot.type))
          if (physical === undefined) return false
          const offset = Math.ceil(cursor / physical.alignment) * physical.alignment
          const valid =
            field.slot === slot.ordinal &&
            field.local.ordinal === slot.local.ordinal &&
            SilkType.equals(semanticType(field.type), semanticType(slot.type)) &&
            field.access._tag === slot.access._tag &&
            field.offset === offset &&
            field.size === physical.size &&
            field.alignment === physical.alignment &&
            field.padding === offset - cursor
          cursor = offset + physical.size
          alignment = Math.max(alignment, physical.alignment)
          return valid
        })
      const size = Math.ceil(cursor / alignment) * alignment
      return (
        payloadValid &&
        layout.size === size &&
        layout.alignment === alignment &&
        layout.tailPadding === size - cursor
      )
    })
    const maximumAlignment = Math.max(
      wordAlignment,
      ...entry.states.map((state) => state.alignment),
    )
    const maximumSize =
      Math.ceil(
        Math.max(roles.length * wordSize, ...entry.states.map((state) => state.size)) /
          maximumAlignment,
      ) * maximumAlignment
    if (
      !headerValid ||
      !stateValid ||
      entry.states.length !== descriptor.states.length ||
      entry.alignment !== maximumAlignment ||
      entry.size !== maximumSize
    )
      violations.push(
        invalid('coroutine-frame maximum layout or one of its states is not canonical', fn),
      )
  }
  if (matched.size !== self.coroutineFrames.entries.length)
    violations.push(invalid('coroutine-frame layout plan contains a stale or duplicate entry'))
  return Object.freeze(violations)
}
type PropagatingRun = Extract<
  Operation,
  {
    readonly _tag: 'RunEffect' | 'RunEffectValue' | 'RunEffectComposite' | 'RunStaticEffect'
  }
>

/** Validates the one canonical success/failure outcome boundary shared by every run form. */
const runPropagationValid = (
  layout: Layout.Plan,
  fn: MirFunction,
  operation: PropagatingRun,
): boolean => {
  const failures = SilkType.failureMembers(operation.outcomeType.type)
  const propagation = operation.propagationType
  if (failures.length === 0)
    return (
      propagation === undefined &&
      operation.tagMappings.length === 0 &&
      operation.propagationLaneCount === 0 &&
      (operation.failureLoanEnds?.length ?? 0) === 0
    )
  if (propagation === undefined) return false
  const shape = Layout.callingShape(layout, propagation.type)
  return (
    SilkType.equals(semanticType(fn.result), propagation.type) &&
    shape?.laneCount === operation.propagationLaneCount &&
    operation.tagMappings.length === failures.length &&
    operation.tagMappings.every((mapping, sourceOrdinal) => {
      const expectedSource = failures.at(sourceOrdinal)
      const source = SilkType.failureCarrierMember(
        operation.outcomeType.type,
        mapping.source,
        'OneBased',
      )
      const target = SilkType.failureCarrierMember(propagation.type, mapping.target, 'OneBased')
      return (
        mapping.source === sourceOrdinal + 1 &&
        expectedSource !== undefined &&
        source !== undefined &&
        target !== undefined &&
        SilkType.equals(source, expectedSource) &&
        SilkType.equals(source, target)
      )
    })
  )
}

export const verify = (self: Module): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = Layout.verify(self.layout).map((violation) =>
    Object.freeze({
      _tag: 'Violation' as const,
      rule: 'InvalidLayout' as const,
      detail: `${violation.rule}: ${violation.detail}`,
    }),
  )
  violations.push(...coroutineFrameLayoutViolations(self))
  const staticData = self.staticData ?? []
  const staticTableValid = staticData.every((data, ordinal) => {
    const previous = ordinal === 0 ? undefined : staticData.at(ordinal - 1)
    const expectedId = `${data.kind === 'Text' ? 'text' : 'bytes'}:${data.bytes
      .map((byte) => byte.toString(16).padStart(2, '0'))
      .join('')}`
    return (
      (previous === undefined || previous.id < data.id) &&
      data.id === expectedId &&
      data.utf8 === (data.kind === 'Text') &&
      data.bytes.every((byte) => Number.isInteger(byte) && byte >= 0 && byte <= 255)
    )
  })
  const placements = self.layout.staticData ?? []
  const placementMatches =
    placements.length === staticData.length &&
    placements.every((placement, ordinal) => placement.data.id === staticData.at(ordinal)?.id)
  if (!staticTableValid || !placementMatches) {
    violations.push(
      Object.freeze({
        _tag: 'Violation',
        rule: 'InvalidSliceOperation',
        detail: 'static-data table is non-canonical or disagrees with target placement',
      }),
    )
  }
  const originReachable = originReachableSuspensionFunctions(self)
  const orphanRelay = self.functions
    .flatMap((fn) =>
      (fn.suspension?.regions ?? []).flatMap((region) => {
        if (
          region._tag !== 'RunSuspendableEffectRegion' ||
          region.runner.classification === 'Unknown'
        )
          return []
        const declaration = region.runner.declaration
        return declaration === undefined ||
          !self.functions.some(
            (candidate) =>
              originReachable.has(instanceText(candidate.instance)) &&
              matchesInstance(candidate, declaration, region.runner.typeArguments),
          )
          ? [Object.freeze({ fn, region })]
          : []
      }),
    )
    .at(0)
  if (orphanRelay !== undefined)
    violations.push(
      Object.freeze({
        _tag: 'Violation',
        rule: 'OrphanSuspensionMachinery',
        function: orphanRelay.fn.id,
        detail: `suspendable runner ${orphanRelay.region.runner.declaration === undefined ? 'unknown' : targetText(orphanRelay.region.runner.declaration)} has no reachable explicit transfer origin (origin-reachable: ${
          self.functions
            .filter((fn) => originReachable.has(instanceText(fn.instance)))
            .map((fn) => targetText(fn.id))
            .join(', ') || 'none'
        })`,
      }),
    )
  const availableEntry = self.entry._tag === 'UnavailableEntry' ? undefined : self.entry
  const target = self.functions.find(
    (fn) =>
      availableEntry !== undefined &&
      instanceText(fn.instance) === instanceText(availableEntry.target),
  )
  const machine = self.functions.find(
    (fn) =>
      availableEntry !== undefined &&
      instanceText(fn.instance) === instanceText(availableEntry.machine),
  )
  const machineClosures =
    machine?.regions
      .flatMap(operationsOf)
      .flatMap(operationTree)
      .filter((operation) => operation._tag === 'CloseEffectEntry') ?? []
  const machineCalls =
    machine?.regions
      .flatMap(operationsOf)
      .flatMap(operationTree)
      .filter((operation) => operation._tag === 'Call') ?? []
  const entryValid =
    availableEntry !== undefined &&
    target !== undefined &&
    machine !== undefined &&
    machine.parameterCount === 0 &&
    machine.result._tag === 'i32' &&
    (availableEntry._tag === 'OrdinaryEntry'
      ? (instanceText(availableEntry.target) === instanceText(availableEntry.machine) &&
          target.result._tag === 'i32' &&
          machineClosures.length === 0) ||
        (availableEntry.machine.declaration.name === '$unit-entry' &&
          SilkType.equals(semanticType(target.result), SilkType.unit) &&
          machineClosures.length === 0 &&
          machineCalls.length === 1 &&
          machineCalls.some(
            (call) =>
              call.target.module === availableEntry.target.declaration.module &&
              call.target.name === availableEntry.target.declaration.name,
          ))
      : target.result._tag === 'EffectValue' &&
        target.parameterCount === 0 &&
        machineClosures.length === 1 &&
        availableEntry.requirements.length ===
          SilkType.requirementMembers(target.result.type).length &&
        availableEntry.requirements.every((requirement, ordinal) => {
          const expected =
            target.result._tag === 'EffectValue'
              ? SilkType.requirementMembers(target.result.type).at(ordinal)
              : undefined
          return (
            expected !== undefined &&
            requirement.access === expected.access &&
            requirement.role === expected.role &&
            SilkType.equals(requirement.capability, expected.capability)
          )
        }) &&
        availableEntry.failures.length === SilkType.failureMembers(target.result.type).length &&
        availableEntry.failures.every((failure, ordinal) => {
          const expected =
            target.result._tag === 'EffectValue'
              ? SilkType.failureCarrierMember(target.result.type, failure.tag, 'OneBased')
              : undefined
          return (
            expected !== undefined &&
            failure.tag === ordinal + 1 &&
            SilkType.equals(failure.type, expected) &&
            failure.identity === SilkType.encode(expected)
          )
        }))
  if (!entryValid) {
    violations.push(
      Object.freeze({
        _tag: 'Violation',
        rule: 'InvalidEntry',
        detail:
          'machine entry must resolve to one zero-parameter i32 function and preserve its ordinary or effect-closing contract',
      }),
    )
  }
  const instanceKeys = new Set<string>()
  for (const fn of self.functions) {
    violations.push(...suspensionViolations(fn, self.layout))
    const currentInstance = instanceText(fn.instance)
    const concreteTypes = [
      ...fn.instance.typeArguments.filter(SilkType.isTypeArgument),
      ...fn.localTypes.map(semanticType),
      semanticType(fn.result),
      ...fn.regions.flatMap(operationsOf).flatMap(operationTree).flatMap(operationTypes),
      ...suspensionTypes(fn),
    ]
    if (
      fn.instance.declaration.module !== fn.id.module ||
      fn.instance.declaration.name !== fn.id.name ||
      fn.instance.typeArguments.some(
        (argument) => !SilkType.isRuntimeConcreteGenericArgument(argument),
      ) ||
      concreteTypes.some((type) => !SilkType.isRuntimeConcrete(type)) ||
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

    const loopRegions = fn.regions.filter(
      (region): region is LoopRegion => region._tag === 'LoopRegion',
    )
    const loops = new Map<number, LoopRegion>()
    for (const region of loopRegions) loops.set(region.loop.ordinal, region)
    const loopIdCounts = new Map<number, number>()
    for (const loop of loopRegions)
      loopIdCounts.set(loop.loop.ordinal, (loopIdCounts.get(loop.loop.ordinal) ?? 0) + 1)
    const conditionOwners = new Map<number, Array<LoopRegion>>()
    for (const loop of loopRegions) {
      const owners = conditionOwners.get(loop.condition.ordinal) ?? []
      owners.push(loop)
      conditionOwners.set(loop.condition.ordinal, owners)
    }
    for (const loop of loopRegions) {
      const condition = byId.get(loop.condition.ordinal)
      const owners = conditionOwners.get(loop.condition.ordinal) ?? []
      if (
        loopIdCounts.get(loop.loop.ordinal) !== 1 ||
        owners.length !== 1 ||
        condition?._tag !== 'OperationRegion' ||
        condition.outcome._tag !== 'Yield' ||
        condition.ownerLoop?.ordinal !== loop.loop.ordinal
      ) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLoopTarget',
            function: fn.id,
            region: loop.id,
            detail: `loop${loop.loop.ordinal} must own one unique yielding operation condition`,
          }),
        )
      }
    }
    for (const region of fn.regions) {
      const outcome = outcomeOf(region)
      if (outcome?._tag !== 'Yield') continue
      const owners = conditionOwners.get(region.id.ordinal) ?? []
      if (
        region._tag !== 'OperationRegion' ||
        owners.length !== 1 ||
        region.ownerLoop?.ordinal !== owners.at(0)?.loop.ordinal
      ) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLoopTarget',
            function: fn.id,
            region: region.id,
            detail: 'yield must be the uniquely owned operation condition of one lexical loop',
          }),
        )
      }
    }
    const loanBeginnings = new Map<string, number>()
    const loanEndings = new Map<string, number>()
    const globalBeginnings = new Map<string, Extract<Operation, { readonly _tag: 'BeginLoan' }>>()
    for (const region of fn.regions) {
      for (const operation of operationsOf(region).flatMap(operationTree)) {
        if (operation._tag === 'BeginLoan') {
          const key = borrowKey(operation.borrow)
          loanBeginnings.set(key, (loanBeginnings.get(key) ?? 0) + 1)
          globalBeginnings.set(key, operation)
        } else if (operation._tag === 'EndLoan') {
          const key = borrowKey(operation.borrow)
          loanEndings.set(key, (loanEndings.get(key) ?? 0) + 1)
        }
      }
    }
    const globalEndings = new Set(loanEndings.keys())
    for (const key of new Set([...loanBeginnings.keys(), ...loanEndings.keys()])) {
      const endings = loanEndings.get(key) ?? 0
      if (loanBeginnings.get(key) !== 1 || endings < 1 || !loanPathsValid(fn, key, byId, loops)) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLoan',
            function: fn.id,
            detail: `loan ${key} must begin once and end exactly once on every terminating path`,
          }),
        )
      }
    }
    const beginningsByDestination = new Map<
      number,
      readonly [string, Extract<Operation, { readonly _tag: 'BeginLoan' }>]
    >()
    for (const [key, beginning] of globalBeginnings)
      beginningsByDestination.set(beginning.destination.ordinal, [key, beginning])
    for (const [childKey, child] of globalBeginnings) {
      const parent = beginningsByDestination.get(child.root.ordinal)
      if (
        parent !== undefined &&
        parent[0] !== childKey &&
        !loanAncestryPathsValid(fn, parent[0], childKey, byId, loops)
      ) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLoan',
            function: fn.id,
            detail: `reborrow ${childKey} must remain within parent loan ${parent[0]} on every path`,
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
        ...loanViolations(
          fn,
          self.layout,
          region,
          operationsOf(region),
          globalBeginnings,
          globalEndings,
        ),
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
      if (outcome?._tag === 'Return') {
        const returned = fn.localTypes.at(outcome.value.ordinal)
        if (
          returned !== undefined &&
          returned._tag !== 'Bottom' &&
          !SilkType.equals(semanticType(returned), semanticType(fn.result))
        ) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'InvalidReturn',
              function: fn.id,
              region: region.id,
              detail: `return local ${localText(outcome.value)} has ${SilkType.encode(semanticType(returned))}, expected ${SilkType.encode(semanticType(fn.result))}`,
            }),
          )
        }
      }
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
        const invalidString = (detail: string): void => {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'InvalidStringOperation',
              function: fn.id,
              region: region.id,
              detail,
            }),
          )
        }
        const heldStringLoansValid = (
          heldLoans: ReadonlyArray<Hir.BorrowId>,
          source?: LocalId,
        ): boolean => {
          const keys = heldLoans.map(borrowKey)
          return (
            new Set(keys).size === keys.length &&
            heldLoans.every((borrow) => {
              const key = borrowKey(borrow)
              const beginning = globalBeginnings.get(key)
              return (
                beginning !== undefined &&
                globalEndings.has(key) &&
                (source === undefined || beginning.destination.ordinal === source.ordinal)
              )
            })
          )
        }
        if (operation._tag === 'StaticString') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const data = self.staticData?.find((candidate) => candidate.id === operation.data)
          if (
            destination?._tag !== 'String' ||
            operation.type._tag !== 'String' ||
            data?.kind !== 'Text' ||
            !data.utf8 ||
            data.bytes.length !== operation.byteLength
          ) {
            invalidString(
              'static string disagrees with its UTF-8 data, byte length, or string destination',
            )
          }
        }
        if (operation._tag === 'StringFromUtf8Unchecked') {
          const bytes = fn.localTypes.at(operation.bytes.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          if (
            bytes?._tag !== 'Slice' ||
            !SilkType.equals(bytes.type, SilkType.slice('Shared', 'u8')) ||
            destination?._tag !== 'String' ||
            operation.type._tag !== 'String' ||
            operation.authorization !== 'Unsafe' ||
            !heldStringLoansValid(operation.heldLoans, operation.bytes)
          ) {
            invalidString(
              'unchecked formation requires unsafe authorization, one shared byte view, retained backing loans, and a string destination',
            )
          }
        }
        if (operation._tag === 'StringUtf8Bytes') {
          const string = fn.localTypes.at(operation.string.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          if (
            string?._tag !== 'String' ||
            destination?._tag !== 'Slice' ||
            !SilkType.equals(destination.type, SilkType.slice('Shared', 'u8')) ||
            !SilkType.equals(operation.type.type, SilkType.slice('Shared', 'u8')) ||
            !heldStringLoansValid(operation.heldLoans)
          ) {
            invalidString(
              'UTF-8 byte viewing requires a string source, an immutable byte-view destination, and retained backing loans',
            )
          }
        }
        if (operation._tag === 'StringByteLength') {
          const string = fn.localTypes.at(operation.string.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          if (string?._tag !== 'String' || destination?._tag !== 'usize') {
            invalidString('string byte length requires a string source and usize destination')
          }
        }
        if (operation._tag === 'StringEqualsExact') {
          const left = fn.localTypes.at(operation.left.ordinal)
          const right = fn.localTypes.at(operation.right.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          if (left?._tag !== 'String' || right?._tag !== 'String' || destination?._tag !== 'bool') {
            invalidString(
              'exact string equality requires two string operands and a bool destination',
            )
          }
        }
        if (operation._tag === 'StaticView') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const data = self.staticData?.find((candidate) => candidate.id === operation.data)
          if (
            destination === undefined ||
            destination._tag !== 'Slice' ||
            !SilkType.equals(destination.type, operation.type.type) ||
            operation.type.type.access !== 'Shared' ||
            operation.type.type.element !== 'u8' ||
            data === undefined ||
            data.bytes.length !== operation.length
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidSliceOperation',
                function: fn.id,
                region: region.id,
                detail: 'static view disagrees with its immutable bytes, length, or destination',
              }),
            )
          }
        }
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
                : scalar?.category === 'Character'
                  ? // A Unicode scalar value: inside the range and outside the surrogate hole.
                    value >= 0n && value <= 0x10ffffn && !(value >= 0xd800n && value <= 0xdfffn)
                  : scalar?.category === 'Floating'
                    ? value >= 0n && value < 1n << BigInt(Scalar.bits(scalar, pointerBits))
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
            operation.operator === 'GreaterOrEqual' ||
            operation.operator === 'TotalOrder'
          const scalar = typeof operand === 'string' ? Scalar.find(operand) : undefined
          const supportsOperation =
            scalar?.category === 'Integer' ||
            (scalar?.category === 'Floating' &&
              (comparison ||
                operation.operator === 'Add' ||
                operation.operator === 'Subtract' ||
                operation.operator === 'Multiply' ||
                operation.operator === 'Divide' ||
                operation.operator === 'Remainder')) ||
            (scalar?.category === 'Boolean' &&
              (operation.operator === 'Equals' || operation.operator === 'NotEquals')) ||
            (scalar?.category === 'Character' && comparison && operation.operator !== 'TotalOrder')
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
        if (
          operation._tag === 'ConvertScalar' ||
          operation._tag === 'ReinterpretScalar' ||
          operation._tag === 'FloatUnary' ||
          operation._tag === 'FloatTranscendental'
        ) {
          const source = fn.localTypes.at(operation.source.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const sourceScalar = Scalar.find(operation.sourceType._tag)
          const targetScalar = Scalar.find(operation.type._tag)
          const pointerBits = self.layout.target.pointerSize === 4 ? 32 : 64
          const reinterpretable =
            operation._tag !== 'ReinterpretScalar' ||
            (sourceScalar !== undefined &&
              targetScalar !== undefined &&
              Scalar.bits(sourceScalar, pointerBits) === Scalar.bits(targetScalar, pointerBits) &&
              sourceScalar.category !== targetScalar.category)
          const unary =
            operation._tag !== 'FloatUnary' ||
            (sourceScalar?.category === 'Floating' &&
              (operation.operation === 'Negate' || operation.operation === 'Sqrt'
                ? targetScalar?.spelling === sourceScalar.spelling
                : targetScalar?.category === 'Boolean'))
          const transcendental =
            operation._tag !== 'FloatTranscendental' ||
            (sourceScalar?.category === 'Floating' &&
              targetScalar?.spelling === sourceScalar.spelling &&
              (operation.operation === 'Sin' || operation.operation === 'Cos'))
          if (
            sourceScalar === undefined ||
            targetScalar === undefined ||
            sourceScalar.category === 'Boolean' ||
            (operation._tag !== 'FloatUnary' && targetScalar.category === 'Boolean') ||
            source === undefined ||
            destination === undefined ||
            !SilkType.equals(semanticType(source), operation.sourceType._tag) ||
            !SilkType.equals(semanticType(destination), operation.type._tag) ||
            !reinterpretable ||
            !unary ||
            !transcendental
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidIntegerOperation',
                function: fn.id,
                region: region.id,
                detail: `${operation._tag} has inconsistent source or destination types`,
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
          const expectedFailure = SilkType.failureCarrierMember(
            operation.propagationType.type,
            operation.failureTag,
            'OneBased',
          )
          if (
            layout?._tag !== 'Nominal' ||
            !SilkType.equals(layout.type, SilkType.layout) ||
            destination?._tag !== 'Nominal' ||
            !SilkType.equals(destination.type, SilkType.allocation) ||
            !SilkType.equals(operation.type.type, SilkType.allocation) ||
            !SilkType.equals(operation.failure, SilkType.outOfMemoryError) ||
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
                detail:
                  'allocation does not preserve Layout, Allocation, or OutOfMemoryError contracts',
              }),
            )
        }
        if (operation._tag === 'HostWrite') {
          const stream = fn.localTypes.at(operation.stream.ordinal)
          const bytes = fn.localTypes.at(operation.bytes.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const expectedFailure = SilkType.failureCarrierMember(
            operation.propagationType.type,
            operation.failureTag,
            'OneBased',
          )
          const byteType = bytes === undefined ? undefined : semanticType(bytes)
          const byteView =
            byteType !== undefined &&
            SilkType.isSlice(byteType) &&
            byteType.access === 'Shared' &&
            byteType.element === 'u8'
          if (
            stream?._tag !== 'bool' ||
            !byteView ||
            destination?._tag !== 'Nominal' ||
            !SilkType.equals(destination.type, SilkType.unit) ||
            !SilkType.equals(operation.type.type, SilkType.unit) ||
            !SilkType.equals(operation.failure, SilkType.streamWriteFailure) ||
            expectedFailure === undefined ||
            !SilkType.equals(expectedFailure, operation.failure) ||
            !SilkType.equals(semanticType(fn.result), operation.propagationType.type)
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidStandardStreamOperation',
                function: fn.id,
                region: region.id,
                detail:
                  'standard-stream write does not preserve destination, byte-view, unit, or typed-failure contracts',
              }),
            )
          }
        }
        if (operation._tag === 'OsCall') {
          const catalog = Intrinsic.findOperationById(operation.operation)
          const rule = catalog?.rule._tag === 'BuiltinRule' ? catalog.rule : undefined
          const expectedResult =
            rule !== undefined && SilkType.isEffect(rule.result) ? rule.result.success : undefined
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const argumentsValid =
            rule?.operation.startsWith('Os') &&
            rule.parameters.length === operation.arguments.length &&
            rule.parameters.every((expected, ordinal) => {
              const argument = operation.arguments.at(ordinal)
              const actual = argument === undefined ? undefined : fn.localTypes.at(argument.ordinal)
              return actual !== undefined && SilkType.equals(semanticType(actual), expected)
            })
          if (
            catalog?.unsafe !== true ||
            catalog.targets.includes('Wasm') ||
            expectedResult === undefined ||
            destination === undefined ||
            !SilkType.equals(semanticType(destination), expectedResult) ||
            !SilkType.equals(semanticType(operation.type), expectedResult) ||
            !argumentsValid
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidOsOperation',
                function: fn.id,
                region: region.id,
                detail: 'OS operation does not match its sealed unsafe native-only signature',
              }),
            )
          }
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
        if (operation._tag === 'RawBufferRead') {
          const buffer = fn.localTypes.at(operation.buffer.ordinal)
          const index = fn.localTypes.at(operation.index.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const bufferElement =
            buffer?._tag === 'Reference' && SilkType.isRawBuffer(buffer.type.target)
              ? buffer.type.target.arguments[0]
              : undefined
          if (
            buffer?._tag !== 'Reference' ||
            buffer.type.access !== 'Shared' ||
            index?._tag !== 'usize' ||
            destination === undefined ||
            bufferElement === undefined ||
            !SilkType.equals(bufferElement, operation.element) ||
            !SilkType.equals(semanticType(destination), operation.element) ||
            !isCopy(self.layout, operation.element)
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidRawStorageOperation',
                function: fn.id,
                region: region.id,
                detail:
                  'RawBuffer.read lost its shared buffer, bounds, Copy element, or result provenance',
              }),
            )
        }
        if (operation._tag === 'RawBufferView') {
          const buffer = fn.localTypes.at(operation.buffer.ordinal)
          const offset = fn.localTypes.at(operation.offset.ordinal)
          const length = fn.localTypes.at(operation.length.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const bufferElement =
            buffer?._tag === 'Reference' && SilkType.isRawBuffer(buffer.type.target)
              ? buffer.type.target.arguments[0]
              : undefined
          const elementLayout = Layout.entry(self.layout, operation.element)
          const expectedStride =
            elementLayout === undefined
              ? undefined
              : Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment
          if (
            buffer?._tag !== 'Reference' ||
            buffer.type.access !== operation.access ||
            offset?._tag !== 'usize' ||
            length?._tag !== 'usize' ||
            destination?._tag !== 'Slice' ||
            destination.type.access !== operation.access ||
            bufferElement === undefined ||
            !SilkType.equals(bufferElement, operation.element) ||
            !SilkType.equals(destination.type.element, operation.element) ||
            operation.stride !== expectedStride
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidRawStorageOperation',
                function: fn.id,
                region: region.id,
                detail:
                  'RawBuffer view lost its borrowed buffer, initialized range, element, access, or layout provenance',
              }),
            )
          }
        }
        if (operation._tag === 'RawBufferCopy') {
          const buffer = fn.localTypes.at(operation.buffer.ordinal)
          const offset = fn.localTypes.at(operation.offset.ordinal)
          const source = fn.localTypes.at(operation.source.ordinal)
          const length = fn.localTypes.at(operation.length.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const bufferElement =
            buffer?._tag === 'Reference' && SilkType.isRawBuffer(buffer.type.target)
              ? buffer.type.target.arguments[0]
              : undefined
          const elementLayout = Layout.entry(self.layout, operation.element)
          const expectedStride =
            elementLayout === undefined
              ? undefined
              : Math.ceil(elementLayout.size / elementLayout.alignment) * elementLayout.alignment
          if (
            buffer?._tag !== 'Reference' ||
            buffer.type.access !== 'Exclusive' ||
            offset?._tag !== 'usize' ||
            length?._tag !== 'usize' ||
            source?._tag !== 'Slice' ||
            source.type.access !== 'Shared' ||
            destination?._tag !== 'Nominal' ||
            !SilkType.equals(destination.type, SilkType.unit) ||
            bufferElement === undefined ||
            !SilkType.equals(bufferElement, operation.element) ||
            !SilkType.equals(source.type.element, operation.element) ||
            operation.stride !== expectedStride ||
            operation.retainsSource !== isCopy(self.layout, operation.element)
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidRawStorageOperation',
                function: fn.id,
                region: region.id,
                detail:
                  'RawBuffer copy lost its exclusive destination, shared source range, element, or layout provenance',
              }),
            )
          }
        }
        if (operation._tag === 'RawBufferFill') {
          const buffer = fn.localTypes.at(operation.buffer.ordinal)
          const offset = fn.localTypes.at(operation.offset.ordinal)
          const length = fn.localTypes.at(operation.length.ordinal)
          const value = fn.localTypes.at(operation.value.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const bufferElement =
            buffer?._tag === 'Reference' && SilkType.isRawBuffer(buffer.type.target)
              ? buffer.type.target.arguments[0]
              : undefined
          if (
            buffer?._tag !== 'Reference' ||
            buffer.type.access !== 'Exclusive' ||
            offset?._tag !== 'usize' ||
            length?._tag !== 'usize' ||
            value?._tag !== 'u8' ||
            destination?._tag !== 'Nominal' ||
            !SilkType.equals(destination.type, SilkType.unit) ||
            bufferElement === undefined ||
            !SilkType.equals(bufferElement, 'u8')
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidRawStorageOperation',
                function: fn.id,
                region: region.id,
                detail: 'RawBuffer fill lost its exclusive byte buffer, range, or byte value',
              }),
            )
          }
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
              (operation._tag !== 'SlotCopy' || isCopy(self.layout, operation.element)))
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
              (resultType._tag !== 'Bottom' &&
                !SilkType.equals(semanticType(resultType), semanticType(operation.type)))
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
              (dropped?._tag === 'CallableValue' &&
                dropped.storage !== undefined &&
                SilkType.equals(dropped.storage.realization.contract, cleanup.type)) ||
              (dropped?._tag === 'EffectValue' &&
                dropped.storage !== undefined &&
                SilkType.equals(dropped.storage.realization.contract, cleanup.type)) ||
              (SilkType.isEffect(droppedSemantic) &&
                SilkType.isEffect(cleanup.type) &&
                (() => {
                  const cleanupEffect = cleanup.type
                  return (
                    SilkType.equals(droppedSemantic.success, cleanupEffect.success) &&
                    SilkType.failureMembers(droppedSemantic).length ===
                      SilkType.failureMembers(cleanupEffect).length &&
                    SilkType.failureMembers(droppedSemantic).every((failure, ordinal) => {
                      const expected = SilkType.failureMembers(cleanupEffect).at(ordinal)
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
              cleanup.environment._tag === 'CallableEnvironmentIdentity' &&
              SilkType.equalsCallableEnvironmentIdentity(
                cleanup.environment.identity,
                Instances.callableEnvironmentIdentity(dropped.environment.callable),
              ) &&
              (() => {
                const expected = dropped.environment.fields
                  .filter((field) => field.access === 'Take' && !isCopy(self.layout, field.type))
                  .map((field) => field.ordinal)
                  .reverse()
                return (
                  expected.length === cleanup.slots.length &&
                  expected.every((ordinal, slot) => cleanup.slots.at(slot)?.ordinal === ordinal)
                )
              })())
          const effectCleanupValid =
            cleanup._tag !== 'EffectCleanup' ||
            (dropped?._tag === 'EffectValue' &&
              (dropped.storage === undefined
                ? Hir.sameExecutableSite(cleanup.site, dropped.site)
                : storedEffectCleanupValid(self.layout, dropped, cleanup)))
          const compositeCleanupValid =
            cleanup._tag !== 'EffectCompositeCleanup' ||
            (dropped?._tag === 'EffectComposite' &&
              cleanup.alternatives.length === dropped.alternatives.length)
          const storedAggregateCleanupValid =
            droppedSemantic !== undefined &&
            (!SilkType.containsEffectRepresentation(droppedSemantic) ||
              cleanupMatchesSemanticType(self.layout, cleanup, droppedSemantic))
          if (
            dropped === undefined ||
            !cleanupTypeMatches ||
            !unionCasesValid ||
            !callableCleanupValid ||
            !effectCleanupValid ||
            !compositeCleanupValid ||
            !storedAggregateCleanupValid
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
              const storedCallableValid =
                field.stored?._tag === 'StoredCallableField' &&
                declared !== undefined &&
                valueType?._tag === 'CallableValue' &&
                SilkType.equals(field.stored.type, declared.type) &&
                TypeCompatibility.isCompatible(
                  TypeCompatibility.check(valueType.type, field.stored.realization.contract),
                ) &&
                Hir.matchesCallableTargetIdentity(
                  valueType.target,
                  field.stored.realization.target,
                ) &&
                field.stored.realization.field.ordinal === field.field.ordinal &&
                field.stored.realization.instance.module === operation.type.type.module &&
                field.stored.realization.instance.name === operation.type.type.name
              const storedEffectValid =
                field.stored?._tag === 'StoredEffectField' &&
                declared !== undefined &&
                valueType?._tag === 'EffectValue' &&
                SilkType.equals(field.stored.type, declared.type) &&
                TypeCompatibility.isCompatible(
                  TypeCompatibility.check(valueType.type, field.stored.realization.contract),
                ) &&
                Hir.sameExecutableSite(valueType.site, field.stored.realization.site) &&
                Hir.effectRunnerId(valueType.environment.instance.declaration, valueType.site)
                  .module === field.stored.realization.runner.module &&
                Hir.effectRunnerId(valueType.environment.instance.declaration, valueType.site)
                  .name === field.stored.realization.runner.name &&
                field.stored.realization.field.ordinal === field.field.ordinal &&
                field.stored.realization.instance.module === operation.type.type.module &&
                field.stored.realization.instance.name === operation.type.type.name
              return (
                declared !== undefined &&
                declared.id.ordinal === field.field.ordinal &&
                valueType !== undefined &&
                ((field.stored === undefined &&
                  SilkType.equals(semanticType(valueType), declared.type)) ||
                  storedCallableValid ||
                  storedEffectValid)
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
          const selected = placeType(
            fn,
            self.layout,
            operation.root,
            operation.selectors,
            operation._tag === 'ReadPlace',
          )
          const sliceSelector = operation.selectors.find(
            (selector) => selector._tag === 'SliceElementSelector',
          )
          const sharedMatchProjection =
            operation._tag === 'ReadPlace' &&
            operation.consume !== true &&
            operations.filter((candidate) =>
              accessedOwnerLocals(candidate).some(
                (local) => local.ordinal === operation.destination.ordinal,
              ),
            ).length === 1 &&
            operations.some(
              (candidate) =>
                candidate._tag === 'Match' &&
                candidate.scrutinee.ordinal === operation.destination.ordinal &&
                (candidate.access === 'Shared' || candidate.access === 'Exclusive'),
            )
          // A read whose value is never accessed as an owner and is only borrowed shared observes
          // the place without claiming it: it cannot be moved, dropped, or written through, so a
          // non-Copy element reaches an interface witness without being duplicated in any sense a
          // later release could notice. This is the same license the shared match projection has.
          const sharedBorrowProjection =
            operation._tag === 'ReadPlace' &&
            operation.consume !== true &&
            operations.every(
              (candidate) =>
                !accessedOwnerLocals(candidate).some(
                  (local) => local.ordinal === operation.destination.ordinal,
                ),
            ) &&
            operations.some(
              (candidate) =>
                candidate._tag === 'BeginLoan' &&
                candidate.access === 'Shared' &&
                candidate.root.ordinal === operation.destination.ordinal,
            )
          const callableViewProjection =
            operation._tag === 'ReadPlace' &&
            operation.consume !== true &&
            operation.type._tag === 'CallableValue' &&
            operation.type.storage !== undefined &&
            operations.filter((candidate) =>
              accessedOwnerLocals(candidate).some(
                (local) => local.ordinal === operation.destination.ordinal,
              ),
            ).length === 1 &&
            operations.some(
              (candidate) =>
                candidate._tag === 'ApplyCallable' &&
                candidate.callable?.ordinal === operation.destination.ordinal &&
                (candidate.access === 'Shared' || candidate.access === 'Exclusive'),
            )
          const effectViewProjection =
            operation._tag === 'ReadPlace' &&
            operation.consume !== true &&
            operation.type._tag === 'EffectValue' &&
            operation.type.storage !== undefined &&
            (operation.type.type.access === 'Shared' ||
              operation.type.type.access === 'Exclusive') &&
            operations.filter((candidate) =>
              accessedOwnerLocals(candidate).some(
                (local) => local.ordinal === operation.destination.ordinal,
              ),
            ).length === 1 &&
            operations.some(
              (candidate) =>
                candidate._tag === 'RunEffectValue' &&
                candidate.effect.ordinal === operation.destination.ordinal,
            )
          if (
            selected === undefined ||
            !SilkType.equals(selected, semanticType(operation.type)) ||
            (operation._tag === 'ReadPlace' &&
              !isCopy(self.layout, selected) &&
              operation.consume !== true &&
              !sharedMatchProjection &&
              !sharedBorrowProjection &&
              !callableViewProjection &&
              !effectViewProjection)
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
                callArgumentCompatible(actual, expected)
              )
            }) &&
            SilkType.equals(semanticType(operation.type), semanticType(target.result))
          if (!valid) {
            const argumentsDetail = operation.arguments
              .map((argument, ordinal) => {
                const actual = fn.localTypes.at(argument.ordinal)
                const expected = target?.localTypes.at(ordinal)
                return `${ordinal}:${actual === undefined ? 'missing' : SilkType.encode(semanticType(actual))}->${expected === undefined ? 'missing' : SilkType.encode(semanticType(expected))}`
              })
              .join(', ')
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidCallShape',
                function: fn.id,
                region: region.id,
                detail: `call ${targetText(operation.target)} does not match its logical contract (${argumentsDetail || 'no arguments'}; result=${SilkType.encode(semanticType(operation.type))}->${target === undefined ? 'missing' : SilkType.encode(semanticType(target.result))})`,
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
            Hir.sameCallableTarget(destination.target, operation.target) &&
            Hir.sameCallableTarget(operation.type.target, operation.target) &&
            operation.typeArguments.every(SilkType.isRuntimeConcreteGenericArgument) &&
            (environment === undefined
              ? operation.captures.length === 0
              : capturesValid &&
                Hir.sameCallableTarget(environment.callable.target, operation.target) &&
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
                TypeCompatibility.isCompatible(
                  TypeCompatibility.check(semanticType(actual), expected),
                )
              )
            })
          const environmentForm =
            operation.realization === 'Environment' &&
            operation.callable !== undefined &&
            operation.target === undefined &&
            operation.captures.length === 0 &&
            source?._tag === 'CallableValue' &&
            TypeCompatibility.isCompatible(
              TypeCompatibility.check(source.type, operation.callableType),
            )
          const directDeclaration =
            operation.target?._tag === 'DeclarationCallableTarget'
              ? operation.target.declaration
              : undefined
          const directTarget =
            directDeclaration === undefined
              ? undefined
              : self.functions.find((candidate) =>
                  matchesInstance(candidate, directDeclaration, operation.typeArguments),
                )
          const directCapturesValid =
            operation.captures.length === 0 ||
            (operation.target?._tag === 'BuiltinCallableTarget'
              ? operation.captures.every(
                  (capture, ordinal, captures) =>
                    fn.localTypes.at(capture.source.ordinal) !== undefined &&
                    captures.findIndex(
                      (candidate) => candidate.parameterOrdinal === capture.parameterOrdinal,
                    ) === ordinal,
                )
              : directTarget !== undefined &&
                operation.captures.every((capture) => {
                  const sourceType = fn.localTypes.at(capture.source.ordinal)
                  const parameterType = directTarget.localTypes.at(capture.parameterOrdinal)
                  return (
                    sourceType !== undefined &&
                    parameterType !== undefined &&
                    SilkType.equals(semanticType(sourceType), semanticType(parameterType))
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
            operation.typeArguments.every(SilkType.isRuntimeConcreteGenericArgument) &&
            argumentsValid &&
            (environmentForm || directForm)
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidCallableOperation',
                function: fn.id,
                region: region.id,
                detail: `callable application disagrees with its mode, arguments, realization, or result (destination=${destination !== undefined && SilkType.equals(semanticType(destination), semanticType(operation.type))}, mode=${operation.access}/${operation.callableType.mode}:${operation.access === operation.callableType.mode}, source=${source?._tag === 'CallableValue' ? source.type.mode : 'none'}, types=${operation.typeArguments.every(SilkType.isRuntimeConcreteGenericArgument)}, arguments=${argumentsValid}, environment=${environmentForm}, direct=${directForm}, captures=${directCapturesValid})`,
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
              : SilkType.failureCarrierMember(operation.type.type, operation.tag, 'OneBased')
          if (
            destination?._tag !== 'EffectOutcome' ||
            source === undefined ||
            payload === undefined ||
            !SilkType.equals(destination.type, operation.type.type) ||
            (source._tag !== 'Bottom' && !SilkType.equals(semanticType(source), payload))
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
        if (operation._tag === 'PackEffectFailureUnion') {
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const source = fn.localTypes.at(operation.source.ordinal)
          const mappingsValid =
            operation.mappings.length === operation.sourceType.type.members.length &&
            operation.mappings.every((mapping, sourceOrdinal) => {
              const sourceMember = SilkType.failureCarrierMember(
                operation.sourceType.type,
                mapping.source,
                'ZeroBased',
              )
              const targetFailure = SilkType.failureCarrierMember(
                operation.type.type,
                mapping.target,
                'OneBased',
              )
              return (
                mapping.source === sourceOrdinal &&
                sourceMember !== undefined &&
                targetFailure !== undefined &&
                SilkType.equals(sourceMember, targetFailure)
              )
            })
          if (
            destination?._tag !== 'EffectOutcome' ||
            source?._tag !== 'Union' ||
            !SilkType.equals(destination.type, operation.type.type) ||
            !SilkType.equals(source.type, operation.sourceType.type) ||
            !mappingsValid
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'effect failure-union mappings do not preserve E members',
              }),
            )
        }
        if (operation._tag === 'PropagateEffectFailure') {
          const source = fn.localTypes.at(operation.source.ordinal)
          const semanticSource = semanticType(operation.sourceType)
          const sourceMembers = SilkType.isUnion(semanticSource)
            ? semanticSource.members
            : Object.freeze([semanticSource])
          const propagationShape = Layout.callingShape(self.layout, operation.propagationType.type)
          const mappingsValid =
            operation.tagMappings.length === sourceMembers.length &&
            operation.tagMappings.every((mapping, sourceOrdinal) => {
              const expectedSource = sourceMembers.at(sourceOrdinal)
              const sourceMember = SilkType.failureCarrierMember(
                semanticType(operation.sourceType),
                mapping.source,
                'ZeroBased',
              )
              const targetFailure = SilkType.failureCarrierMember(
                operation.propagationType.type,
                mapping.target,
                'OneBased',
              )
              return (
                mapping.source === sourceOrdinal &&
                expectedSource !== undefined &&
                sourceMember !== undefined &&
                targetFailure !== undefined &&
                SilkType.equals(sourceMember, expectedSource) &&
                SilkType.equals(sourceMember, targetFailure)
              )
            })
          if (
            source === undefined ||
            !SilkType.equals(semanticType(source), semanticType(operation.sourceType)) ||
            !SilkType.equals(semanticType(fn.result), operation.propagationType.type) ||
            propagationShape?.laneCount !== operation.propagationLaneCount ||
            !SilkType.isNever(operation.type.type) ||
            !mappingsValid
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'failure propagation does not preserve canonical outcome contracts',
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
        if (operation._tag === 'PackEffectComposite') {
          const source = fn.localTypes.at(operation.source.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const selected = operation.type.alternatives.at(operation.alternative)
          if (
            source?._tag !== 'EffectValue' ||
            destination?._tag !== 'EffectComposite' ||
            selected === undefined ||
            !SilkType.equals(destination.type, operation.type.type) ||
            !SilkType.equals(source.type, selected.type) ||
            !Hir.sameExecutableSite(source.site, selected.site)
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'Effect composite packing does not preserve its selected exact alternative',
              }),
            )
        }
        if (operation._tag === 'RunEffect') {
          const target = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.target, operation.typeArguments),
          )
          if (
            target === undefined ||
            target.result._tag !== 'EffectOutcome' ||
            !SilkType.equals(target.result.type, operation.outcomeType.type) ||
            !runPropagationValid(self.layout, fn, operation)
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
        if (operation._tag === 'RunEffectValue') {
          const effect = fn.localTypes.at(operation.effect.ordinal)
          const outcome = fn.localTypes.at(operation.outcome.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const runner = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.runner, operation.runnerTypeArguments),
          )
          const suspensionRegion = fn.suspension?.regions.find(
            (candidate) =>
              candidate.operation._tag === 'RunEffectValue' &&
              (candidate._tag === 'RunSuspendableEffectRegion'
                ? candidate.runner.declaration?.module === operation.runner.module &&
                  candidate.runner.declaration.name === operation.runner.name
                : candidate.deferred.declaration?.module === operation.runner.module &&
                  candidate.deferred.declaration.name === operation.runner.name),
          )
          const suspensionRunner =
            suspensionRegion?._tag === 'RunSuspendableEffectRegion'
              ? suspensionRegion.runner
              : suspensionRegion?.deferred
          const propagationValid = runPropagationValid(self.layout, fn, operation)
          const effectValue = effect?._tag === 'EffectValue' ? effect : undefined
          const stored = effectValue?.storage
          const storedContractValid =
            stored === undefined ||
            (effectValue !== undefined &&
              SilkType.equals(effectValue.type, stored.realization.contract) &&
              effectValue.type.access === stored.realization.access &&
              SilkType.failureMembers(effectValue.type).length ===
                stored.realization.rows.failures.length &&
              SilkType.failureMembers(effectValue.type).every((failure, ordinal) => {
                const expected = stored.realization.rows.failures.at(ordinal)
                return expected !== undefined && SilkType.equals(failure, expected)
              }) &&
              SilkType.requirementMembers(effectValue.type).length ===
                stored.realization.rows.requirements.length &&
              SilkType.requirementMembers(effectValue.type).every((requirement, ordinal) => {
                const expected = stored.realization.rows.requirements.at(ordinal)
                return (
                  expected !== undefined &&
                  requirement.access === expected.access &&
                  requirement.role === expected.role &&
                  SilkType.equals(requirement.capability, expected.capability)
                )
              }))
          const staticRunnerValid =
            stored === undefined && operation.runnerBase === undefined
              ? true
              : (() => {
                  if (effectValue === undefined) return false
                  const base = operation.runnerBase
                  const selectedBase = base?.declaration ?? operation.runner
                  const selectedArguments = base?.typeArguments ?? operation.runnerTypeArguments
                  const expectedBase =
                    stored?.realization.runner ??
                    Hir.effectRunnerId(
                      effectValue.environment.instance.declaration,
                      effectValue.site,
                    )
                  const expectedBaseArguments =
                    stored?.realization.runnerArguments ??
                    effectValue.environment.instance.typeArguments
                  const baseMatches =
                    selectedBase.module === expectedBase.module &&
                    selectedBase.name === expectedBase.name &&
                    selectedArguments.length === expectedBaseArguments.length &&
                    selectedArguments.every((argument, ordinal) => {
                      const expected = expectedBaseArguments.at(ordinal)
                      return (
                        expected !== undefined && SilkType.equalsGenericArgument(argument, expected)
                      )
                    })
                  const expectedRequirements =
                    stored?.realization.rows.requirements ??
                    SilkType.requirementMembers(effectValue.type)
                  const requirementsMatch =
                    operation.providers.length === expectedRequirements.length &&
                    operation.providers.every((provider, ordinal) => {
                      const requirement = expectedRequirements.at(ordinal)
                      const argumentType =
                        provider.argument === undefined
                          ? undefined
                          : fn.localTypes.at(provider.argument.ordinal)
                      const semanticArgument =
                        argumentType === undefined ? undefined : semanticType(argumentType)
                      return (
                        requirement !== undefined &&
                        provider.role === requirement.role &&
                        provider.requirementAccess === requirement.access &&
                        SilkType.equals(provider.capability, requirement.capability) &&
                        SilkType.equals(provider.witness.capability, provider.capability) &&
                        SilkType.equals(provider.witness.provider, provider.providerType) &&
                        (requirement.access === 'Shared' ||
                          provider.access === 'Exclusive' ||
                          provider.access === 'Take') &&
                        (provider.argument === undefined ||
                          (semanticArgument !== undefined &&
                            SilkType.isReference(semanticArgument) &&
                            semanticArgument.access ===
                              (provider.access === 'Take' ? 'Exclusive' : provider.access) &&
                            SilkType.equals(semanticArgument.target, provider.providerType)))
                      )
                    })
                  const runtimeArguments = operation.providers.flatMap((provider) =>
                    provider.argument === undefined ? [] : [provider.argument],
                  )
                  const argumentsMatch =
                    runtimeArguments.length === operation.arguments.length &&
                    runtimeArguments.every(
                      (argument, ordinal) =>
                        argument.ordinal === operation.arguments.at(ordinal)?.ordinal,
                    )
                  const wrapperShapeMatches =
                    runner !== undefined &&
                    runner.parameterCount ===
                      effectValue.environment.fields.length + operation.arguments.length
                  const runnerBinding = runner?.effectRunner
                  const wrapperBindingMatches =
                    runnerBinding !== undefined &&
                    runnerBinding.base.declaration.module === selectedBase.module &&
                    runnerBinding.base.declaration.name === selectedBase.name &&
                    runnerBinding.base.typeArguments.length === selectedArguments.length &&
                    runnerBinding.base.typeArguments.every((argument, ordinal) => {
                      const expected = selectedArguments.at(ordinal)
                      return (
                        expected !== undefined && SilkType.equalsGenericArgument(argument, expected)
                      )
                    }) &&
                    runnerBinding.providers.length === operation.providers.length &&
                    runnerBinding.providers.every((bound, ordinal) => {
                      const claimed = operation.providers.at(ordinal)
                      return (
                        claimed !== undefined &&
                        bound.role === claimed.role &&
                        bound.access === claimed.access &&
                        SilkType.equals(bound.capability, claimed.capability) &&
                        SilkType.equals(bound.providerType, claimed.providerType) &&
                        conformanceWitnessMatches(bound.witness, claimed.witness)
                      )
                    })
                  return (
                    baseMatches &&
                    requirementsMatch &&
                    argumentsMatch &&
                    wrapperShapeMatches &&
                    wrapperBindingMatches &&
                    (operation.providers.length === 0
                      ? operation.runnerBase === undefined
                      : operation.runnerBase !== undefined)
                  )
                })()
          const valid =
            effectValue !== undefined &&
            outcome?._tag === 'EffectOutcome' &&
            destination !== undefined &&
            SilkType.equals(effectValue.type, operation.outcomeType.type) &&
            SilkType.equals(outcome.type, operation.outcomeType.type) &&
            SilkType.equals(semanticType(destination), semanticType(operation.type)) &&
            ((runner?.result._tag === 'EffectOutcome' &&
              SilkType.representationAdmissibility(runner.result.type, operation.outcomeType.type)
                ._tag === 'Admitted') ||
              (suspensionRunner !== undefined &&
                SilkType.equals(suspensionRunner.outcome, operation.outcomeType.type))) &&
            storedContractValid &&
            staticRunnerValid &&
            propagationValid
          if (!valid)
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: `Effect value run disagrees with its static runner, exact rows, access, outcome, or propagation contract (target=${targetText(operation.runner)}, effect=${effectValue !== undefined}, runner=${runner !== undefined}, suspension-runner=${suspensionRunner !== undefined}, stored-contract=${storedContractValid}, static-runner=${staticRunnerValid}, propagation=${propagationValid})`,
              }),
            )
        }
        if (operation._tag === 'RunEffectComposite') {
          const effect = fn.localTypes.at(operation.effect.ordinal)
          const outcome = fn.localTypes.at(operation.outcome.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const alternativesValid =
            effect?._tag === 'EffectComposite' &&
            operation.alternatives.length === effect.alternatives.length &&
            operation.alternatives.every((alternative, ordinal) => {
              const expected = effect.alternatives.at(ordinal)
              const runner = self.functions.find((candidate) =>
                matchesInstance(candidate, alternative.runner, alternative.runnerTypeArguments),
              )
              const sourceFailures = SilkType.failureMembers(alternative.type.type)
              const mappingsValid =
                alternative.tagMappings.length === sourceFailures.length &&
                alternative.tagMappings.every((mapping, sourceOrdinal) => {
                  const source = sourceFailures.at(sourceOrdinal)
                  const target = SilkType.failureCarrierMember(
                    operation.outcomeType.type,
                    mapping.target,
                    'OneBased',
                  )
                  return (
                    mapping.source === sourceOrdinal + 1 &&
                    source !== undefined &&
                    target !== undefined &&
                    SilkType.equals(source, target)
                  )
                })
              const inputs = [
                ...alternative.type.environment.fields.map((_, inputOrdinal) => ({
                  _tag: 'Capture' as const,
                  ordinal: inputOrdinal,
                })),
                ...alternative.arguments.map((argument) => ({
                  _tag: 'Local' as const,
                  local: argument,
                })),
              ]
              const parametersValid =
                runner !== undefined &&
                runner.parameterCount === inputs.length &&
                inputs.every((input, inputOrdinal) => {
                  const actual =
                    input._tag === 'Capture'
                      ? alternative.type.environment.fields.at(input.ordinal)?.type
                      : (() => {
                          const localType = fn.localTypes.at(input.local.ordinal)
                          return localType === undefined ? undefined : semanticType(localType)
                        })()
                  const expectedType = runner.localTypes.at(inputOrdinal)
                  return (
                    actual !== undefined &&
                    expectedType !== undefined &&
                    TypeCompatibility.isCompatible(
                      TypeCompatibility.check(actual, semanticType(expectedType)),
                    )
                  )
                })
              return (
                expected !== undefined &&
                SilkType.equals(expected.type, alternative.type.type) &&
                Hir.sameExecutableSite(expected.site, alternative.type.site) &&
                runner?.result._tag === 'EffectOutcome' &&
                SilkType.equals(runner.result.type, alternative.type.type) &&
                parametersValid &&
                mappingsValid
              )
            })
          if (
            effect?._tag !== 'EffectComposite' ||
            outcome?._tag !== 'EffectOutcome' ||
            destination === undefined ||
            !SilkType.equals(effect.contract, operation.outcomeType.type) ||
            !SilkType.equals(outcome.type, operation.outcomeType.type) ||
            !SilkType.equals(semanticType(destination), semanticType(operation.type)) ||
            !alternativesValid ||
            !runPropagationValid(self.layout, fn, operation)
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail:
                  'Effect composite run disagrees with its alternatives, joined outcome, or propagation contract',
              }),
            )
        }
        if (operation._tag === 'RunStaticEffect') {
          const outcome = fn.localTypes.at(operation.outcome.ordinal)
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const inputs = [
            ...operation.captures.map((capture) => capture.source),
            ...operation.arguments,
          ]
          const runner = self.functions.find(
            (candidate) =>
              matchesInstance(candidate, operation.runner, operation.runnerTypeArguments) &&
              candidate.result._tag === 'EffectOutcome' &&
              SilkType.equals(candidate.result.type, operation.outcomeType.type) &&
              candidate.parameterCount === inputs.length &&
              inputs.every((input, ordinal) => {
                const actual = fn.localTypes.at(input.ordinal)
                const expected = candidate.localTypes.at(ordinal)
                return (
                  actual !== undefined &&
                  expected !== undefined &&
                  TypeCompatibility.isCompatible(
                    TypeCompatibility.check(semanticType(actual), semanticType(expected)),
                  )
                )
              }),
          )
          const parametersValid =
            runner !== undefined &&
            runner.parameterCount === inputs.length &&
            inputs.every((input, ordinal) => {
              const actual = fn.localTypes.at(input.ordinal)
              const expected = runner.localTypes.at(ordinal)
              return (
                actual !== undefined &&
                expected !== undefined &&
                TypeCompatibility.isCompatible(
                  TypeCompatibility.check(semanticType(actual), semanticType(expected)),
                )
              )
            })
          const propagationValid = runPropagationValid(self.layout, fn, operation)
          const runnerResultValid =
            runner?.result._tag === 'EffectOutcome' &&
            SilkType.equals(runner.result.type, operation.outcomeType.type)
          const outcomeValid =
            outcome?._tag === 'EffectOutcome' &&
            SilkType.equals(outcome.type, operation.outcomeType.type)
          const destinationValid =
            destination !== undefined &&
            SilkType.equals(semanticType(destination), semanticType(operation.type))
          if (
            !runnerResultValid ||
            !outcomeValid ||
            !destinationValid ||
            !parametersValid ||
            !propagationValid
          )
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidNormalization',
                function: fn.id,
                region: region.id,
                detail:
                  'direct static Effect run disagrees with its runner, captures, outcome, or propagation contract',
              }),
            )
        }
        if (operation._tag === 'ReifyEffect') {
          const runner = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.runner, operation.runnerTypeArguments),
          )
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const effect = fn.localTypes.at(operation.effect.ordinal)
          const outcome = fn.localTypes.at(operation.outcome.ordinal)
          const expectedFailureValue = SilkType.failureValue(
            SilkType.failureMembers(operation.outcomeType.type),
          )
          const expectedResult = SilkType.result(
            operation.outcomeType.type.success,
            expectedFailureValue,
          )
          const expectedSuccess = SilkType.resultSuccess(operation.outcomeType.type.success)
          const expectedFailure = SilkType.resultFailure(expectedFailureValue)
          const selectedSuccess = SilkType.failureCarrierMember(
            operation.resultUnion,
            operation.successTag,
            'ZeroBased',
          )
          const selectedFailure = SilkType.failureCarrierMember(
            operation.resultUnion,
            operation.failureTag,
            'ZeroBased',
          )
          const tagsValid =
            selectedSuccess !== undefined &&
            selectedFailure !== undefined &&
            SilkType.equals(selectedSuccess, expectedSuccess) &&
            SilkType.equals(selectedFailure, expectedFailure)
          if (
            runner?.result._tag !== 'EffectOutcome' ||
            effect?._tag !== 'EffectValue' ||
            outcome?._tag !== 'EffectOutcome' ||
            destination?._tag !== 'Nominal' ||
            !SilkType.equals(runner.result.type, operation.outcomeType.type) ||
            !SilkType.equals(effect.type, operation.outcomeType.type) ||
            !SilkType.equals(outcome.type, operation.outcomeType.type) ||
            !SilkType.equals(destination.type, expectedResult) ||
            !SilkType.equals(operation.resultType.type, expectedResult) ||
            !SilkType.equals(operation.failureValueType, expectedFailureValue) ||
            !SilkType.equals(operation.successType, expectedSuccess) ||
            !SilkType.equals(operation.failureType, expectedFailure) ||
            !SilkType.equals(operation.resultShape.type, expectedResult) ||
            !SilkType.equals(operation.outcomeShape.type, operation.outcomeType.type) ||
            !SilkType.equals(operation.failureValueShape.type, expectedFailureValue) ||
            !tagsValid
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEffectOperation',
                function: fn.id,
                region: region.id,
                detail: 'effect result runner, channel data, tags, or calling shapes disagree',
              }),
            )
          }
        }
        if (operation._tag === 'CloseEffectEntry') {
          const target = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.target, operation.typeArguments),
          )
          const runner = self.functions.find((candidate) =>
            matchesInstance(candidate, operation.runner, operation.typeArguments),
          )
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const effect = fn.localTypes.at(operation.effect.ordinal)
          const outcome = fn.localTypes.at(operation.outcome.ordinal)
          const entryFailures =
            self.entry._tag === 'EffectEntry' &&
            instanceText(self.entry.machine) === instanceText(fn.instance)
              ? self.entry.failures
              : undefined
          const failuresValid =
            runner?.result._tag === 'EffectOutcome' &&
            entryFailures !== undefined &&
            operation.failures.length === SilkType.failureMembers(runner.result.type).length &&
            operation.failures.every((failure, ordinal) => {
              const expected =
                runner.result._tag === 'EffectOutcome'
                  ? SilkType.failureCarrierMember(runner.result.type, failure.tag, 'OneBased')
                  : undefined
              const entryFailure = entryFailures.at(ordinal)
              const payload = fn.localTypes.at(failure.payload.ordinal)
              return (
                expected !== undefined &&
                entryFailure !== undefined &&
                failure.tag === ordinal + 1 &&
                entryFailure.tag === failure.tag &&
                SilkType.equals(failure.type, expected) &&
                SilkType.equals(entryFailure.type, expected) &&
                failure.identity === entryFailure.identity &&
                payload !== undefined &&
                SilkType.equals(semanticType(payload), expected) &&
                SilkType.equals(failure.cleanup.type, expected)
              )
            })
          if (
            target === undefined ||
            target.parameterCount !== 0 ||
            target.result._tag !== 'EffectValue' ||
            runner?.result._tag !== 'EffectOutcome' ||
            destination?._tag !== 'i32' ||
            effect?._tag !== 'EffectValue' ||
            !SilkType.equals(effect.type, operation.effectType.type) ||
            !SilkType.equals(target.result.type, operation.effectType.type) ||
            outcome?._tag !== 'EffectOutcome' ||
            !SilkType.equals(outcome.type, operation.outcomeType.type) ||
            !SilkType.equals(runner.result.type, operation.outcomeType.type) ||
            !failuresValid
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidEntry',
                function: fn.id,
                region: region.id,
                detail:
                  'effect entry closure disagrees with its target, normalized failures, typed payloads, or cleanup plans',
              }),
            )
          }
        }
      }
    }
  }
  for (const verdict of self.normalization ?? []) {
    const fn = self.functions.find(
      (candidate) =>
        candidate.id.module === verdict.function.module &&
        candidate.id.name === verdict.function.name,
    )
    const region = fn?.regions.find((candidate) => candidate.id.ordinal === verdict.region.ordinal)
    const local = fn?.localTypes.at(verdict.local.ordinal)
    const synchronous = verdict._tag === 'Rejected' || verdict.guards.includes('Synchronous')
    if (fn === undefined || region === undefined || local === undefined || !synchronous) {
      violations.push(
        Object.freeze({
          _tag: 'Violation',
          rule: 'InvalidNormalization',
          ...(fn === undefined ? {} : { function: fn.id }),
          ...(region === undefined ? {} : { region: region.id }),
          detail: 'normalization verdict has dangling identities or lacks its synchronous proof',
        }),
      )
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
    case 'StaticView':
      return `${localText(operation.destination)} = static-view ${operation.data} length=${operation.length} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'StaticString':
      return `${localText(operation.destination)} = static-string ${operation.data} byte-length=${operation.byteLength} : string ${provenanceText(operation.provenance)}`
    case 'StringFromUtf8Unchecked':
      return `${localText(operation.destination)} = string-from-utf8-unchecked ${localText(operation.bytes)} loans=${operation.heldLoans.map((borrow) => `l${borrow.ordinal}`).join(',') || 'none'} authorization=${operation.authorization.toLowerCase()} : string ${provenanceText(operation.provenance)}`
    case 'StringUtf8Bytes':
      return `${localText(operation.destination)} = string-utf8-bytes ${localText(operation.string)} loans=${operation.heldLoans.map((borrow) => `l${borrow.ordinal}`).join(',') || 'none'} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'StringByteLength':
      return `${localText(operation.destination)} = string-byte-length ${localText(operation.string)} : usize ${provenanceText(operation.provenance)}`
    case 'StringEqualsExact':
      return `${localText(operation.destination)} = string-${operation.negated ? 'not-equals-exact' : 'equals-exact'} ${localText(operation.left)}, ${localText(operation.right)} : bool ${provenanceText(operation.provenance)}`
    case 'PackEffectComposite':
      return `${localText(operation.destination)} = effect-composite alternative=${operation.alternative} ${localText(operation.source)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'Binary':
      return `${localText(operation.destination)} = ${operation.operator.toLowerCase()} ${localText(operation.left)}, ${localText(operation.right)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ConvertInteger':
      return `${localText(operation.destination)} = convert ${localText(operation.source)} ${typeText(operation.sourceType)} -> ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ConvertScalar':
      return `${localText(operation.destination)} = convert-scalar ${localText(operation.source)} ${typeText(operation.sourceType)} -> ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ReinterpretScalar':
      return `${localText(operation.destination)} = reinterpret ${localText(operation.source)} ${typeText(operation.sourceType)} -> ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'FloatUnary':
      return `${localText(operation.destination)} = ${operation.operation.toLowerCase()} ${localText(operation.source)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'FloatTranscendental':
      return `${localText(operation.destination)} = float-${operation.operation.toLowerCase()} ${localText(operation.source)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'CheckedInteger':
      return `${localText(operation.destination)} = ${operation.operation.toLowerCase()} ${operation.operands.map(localText).join(', ')} ${typeText(operation.sourceType)} -> ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ValidateLayout':
      return `${localText(operation.destination)} = layout-make bytes=${localText(operation.bytes)} alignment=${localText(operation.alignment)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RepeatLayout':
      return `${localText(operation.destination)} = layout-repeat ${localText(operation.layout)} count=${localText(operation.count)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'Allocate':
      return `${localText(operation.destination)} = allocate ${localText(operation.layout)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'HostWrite':
      return `${localText(operation.destination)} = standard-stream-write destination=${localText(operation.stream)} bytes=${localText(operation.bytes)} failure=${SilkType.encode(operation.failure)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'OsCall':
      return `${localText(operation.destination)} = os-call ${operation.operation.actor}.${operation.operation.name}(${operation.arguments.map(localText).join(', ')}) : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RawBufferFrom':
      return `${localText(operation.destination)} = raw-buffer-from ${localText(operation.allocation)} count=${localText(operation.count)} element=${SilkType.encode(operation.element)} stride=${operation.stride} align=${operation.elementAlignment} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RawBufferCount':
      return `${localText(operation.destination)} = raw-buffer-count ${localText(operation.buffer)} : usize ${provenanceText(operation.provenance)}`
    case 'RawBufferSlot':
      return `${localText(operation.destination)} = raw-buffer-slot ${localText(operation.buffer)}[${localText(operation.index)}] element=${SilkType.encode(operation.element)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RawBufferRead':
      return `${localText(operation.destination)} = raw-buffer-read ${localText(operation.buffer)}[${localText(operation.index)}] element=${SilkType.encode(operation.element)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RawBufferView':
      return `${localText(operation.destination)} = raw-buffer-view ${localText(operation.buffer)} offset=${localText(operation.offset)} length=${localText(operation.length)} element=${SilkType.encode(operation.element)} access=${operation.access.toLowerCase()} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RawBufferCopy':
      return `${localText(operation.destination)} = raw-buffer-copy ${localText(operation.buffer)} offset=${localText(operation.offset)} source=${localText(operation.source)} length=${localText(operation.length)} element=${SilkType.encode(operation.element)} stride=${operation.stride} retains-source=${operation.retainsSource} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RawBufferFill':
      return `${localText(operation.destination)} = raw-buffer-fill ${localText(operation.buffer)} offset=${localText(operation.offset)} length=${localText(operation.length)} value=${localText(operation.value)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
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
      return `${localText(operation.destination)} = begin-loan l${operation.borrow.ordinal} ${operation.access.toLowerCase()} ${localText(operation.root)}${selectorText(operation.selectors)} source=${typeText(operation.sourceType)} : ${typeText(operation.type)} reborrow=${operation.reborrow} suspended=${operation.suspendsParent} ${provenanceText(operation.provenance)}`
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
          : `<${operation.typeArguments.map(SilkType.encodeGenericArgument).join(', ')}>`
      }(${operation.arguments.map(localText).join(', ')}) : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'MakeEffect':
      return `${localText(operation.destination)} = make-effect ${targetText(operation.runner)} captures=${operation.captures.map((capture) => `${localText(capture.source)}:${capture.access.toLowerCase()}`).join(',') || 'none'} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'MakeCallable':
      return `${localText(operation.destination)} = make-callable ${callableTargetText(operation.target)} captures=${operation.captures.map((capture) => `#${capture.ordinal}->p${capture.parameterOrdinal}:${localText(capture.source)}:${capture.access.toLowerCase()}`).join(',') || 'none'} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ApplyCallable':
      return `${localText(operation.destination)} = apply-callable ${operation.callable === undefined ? (operation.target === undefined ? '?' : callableTargetText(operation.target)) : localText(operation.callable)}(${operation.arguments.map(localText).join(', ')}) captures=${operation.captures.map((capture) => `#${capture.ordinal}:${localText(capture.source)}`).join(',') || 'none'} access=${operation.access.toLowerCase()} evaluation=${operation.evaluation} realization=${operation.realization} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'PackEffectOutcome':
      return `${localText(operation.destination)} = effect-outcome tag=${operation.tag} ${localText(operation.source)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'PackEffectFailureUnion':
      return `${localText(operation.destination)} = effect-failure-union ${localText(operation.source)} mappings=${operation.mappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'PropagateEffectFailure':
      return `propagate-effect-failure ${localText(operation.source)} mappings=${operation.tagMappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.propagationType)} ${operation.releases === undefined || operation.releases.length === 0 ? '' : `releases=${operation.releases.map((release) => localText(release.local)).join(',')} `}${provenanceText(operation.provenance)}`
    case 'UnpackEffectSuccess':
      return `${localText(operation.destination)} = effect-success ${localText(operation.source)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RunEffect':
      return `${localText(operation.destination)} = run-effect ${targetText(operation.target)} propagate=${operation.tagMappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.type)} ${operation.failureLoanEnds === undefined || operation.failureLoanEnds.length === 0 ? '' : `failure-loans=${operation.failureLoanEnds.map((ending) => `l${ending.borrow.ordinal}:${localText(ending.slice)}`).join(',')} `}${operation.releases === undefined || operation.releases.length === 0 ? '' : `releases=${operation.releases.map((release) => localText(release.local)).join(',')} `}${provenanceText(operation.provenance)}`
    case 'RunEffectValue':
      return `${localText(operation.destination)} = run-effect-value ${localText(operation.effect)} runner=${targetText(operation.runner)}${operation.runnerBase === undefined ? '' : ` base=${targetText(operation.runnerBase.declaration)}`} providers=${operation.providers.map((provider) => `${SilkType.encode(provider.capability)}@${provider.role}:${provider.requirementAccess.toLowerCase()}:${provider.access.toLowerCase()}`).join(',') || 'none'} arguments=${operation.arguments.map(localText).join(',') || 'none'} propagate=${operation.tagMappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.type)} ${operation.failureLoanEnds === undefined || operation.failureLoanEnds.length === 0 ? '' : `failure-loans=${operation.failureLoanEnds.map((ending) => `l${ending.borrow.ordinal}:${localText(ending.slice)}`).join(',')} `}${operation.releases === undefined || operation.releases.length === 0 ? '' : `releases=${operation.releases.map((release) => localText(release.local)).join(',')} `}${provenanceText(operation.provenance)}`
    case 'RunEffectComposite':
      return `${localText(operation.destination)} = run-effect-composite ${localText(operation.effect)} alternatives=${operation.alternatives.map((alternative) => targetText(alternative.runner)).join(',')} arguments=${operation.arguments.map(localText).join(',') || 'none'} propagate=${operation.tagMappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'RunStaticEffect':
      return `${localText(operation.destination)} = run-static-effect runner=${targetText(operation.runner)} captures=${operation.captures.map((capture) => `${localText(capture.source)}:${capture.access.toLowerCase()}`).join(',') || 'none'} arguments=${operation.arguments.map(localText).join(',') || 'none'} propagate=${operation.tagMappings.map((mapping) => `${mapping.source}->${mapping.target}`).join(',')} : ${typeText(operation.type)} ${operation.failureLoanEnds === undefined || operation.failureLoanEnds.length === 0 ? '' : `failure-loans=${operation.failureLoanEnds.map((ending) => `l${ending.borrow.ordinal}:${localText(ending.slice)}`).join(',')} `}${operation.releases === undefined || operation.releases.length === 0 ? '' : `releases=${operation.releases.map((release) => localText(release.local)).join(',')} `}${provenanceText(operation.provenance)}`
    case 'ReifyEffect':
      return `${localText(operation.destination)} = effect-result ${localText(operation.effect)} runner=${targetText(operation.runner)} arguments=${operation.arguments.map(localText).join(',') || 'none'} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'CloseEffectEntry':
      return `${localText(operation.destination)} = close-effect-entry ${targetText(operation.target)} effect=${localText(operation.effect)} runner=${targetText(operation.runner)} outcome=${localText(operation.outcome)} failures=${operation.failures.map((failure) => `${failure.tag}:${SilkType.encode(failure.type)}->${localText(failure.payload)}:${failure.cleanup._tag}`).join(',') || 'none'} : i32 ${provenanceText(operation.provenance)}`
    case 'Construct':
      return `${localText(operation.destination)} = construct ${typeText(operation.type)} { ${operation.fields.map(({ field, value, stored }) => `#${field.ordinal}: ${localText(value)}${stored === undefined ? '' : ` stored=${storedExecutableText(stored)}`}`).join(', ')} } ${provenanceText(operation.provenance)}`
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
    case 'ShortCircuit':
      return `${localText(operation.destination)} = short-circuit ${operation.operator === 'And' ? '&&' : '||'} ${localText(operation.left)} : bool ${provenanceText(operation.provenance)}`
  }
}

const fieldPathText = (path: ReadonlyArray<DeclarationIndex.FieldId>): string =>
  path.length === 0 ? 'payload' : path.map((field) => `#${field.ordinal}`).join('.')

const operationLines = (operation: Operation, indent: string): ReadonlyArray<string> => {
  if (operation._tag === 'ShortCircuit') {
    return [
      `${indent}${operationText(operation)}`,
      `${indent}  right -> ${localText(operation.right.result)}`,
      ...operation.right.operations.flatMap((child) => operationLines(child, `${indent}    `)),
    ]
  }
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

const suspensionPointText = (point: SuspensionPointId): string =>
  `${point.sourceId}:${point.spanStart}:${point.spanEnd}#${point.ordinal}`

const continuationPathText = (name: string, path: CoroutineFramePathPlan): string =>
  `    ${name} restores=${path.restores.join(',') || 'none'} loans=${path.loanEnds.map(suspensionBorrowText).join(',') || 'none'} releases=${path.releases.map(coroutineFrameReleaseText).join(',') || 'none'}`

const suspensionRunnerLines = (
  runner: SuspensionRunner,
  indent = '    ',
): ReadonlyArray<string> => [
  `${indent}runner classification=${runner.classification.toLowerCase()} declaration=${runner.declaration === undefined ? 'unknown' : targetText(runner.declaration)} instance=${runner.instance === undefined ? 'unknown' : instanceText(runner.instance)} effect=${runner.effectIdentity ?? 'none'} type-arguments=${runner.typeArguments.map(SilkType.encodeGenericArgument).join(',') || 'none'} outcome=${SilkType.encode(runner.outcome)}`,
  ...runner.captures.map(
    (capture) =>
      `${indent}capture ${capture.ordinal} ${capture.source.toLowerCase()}:${capture.sourceOrdinal} access=${capture.access.toLowerCase()} type=${SilkType.encode(capture.type)}`,
  ),
  ...runner.providers.map(
    (provider) =>
      `${indent}provider ${SilkType.encode(provider.capability)}@${provider.role} requirement=${provider.requirementAccess.toLowerCase()} access=${provider.access.toLowerCase()} type=${SilkType.encode(provider.providerType)} argument=${provider.argument === undefined ? 'none' : localText(provider.argument)} witness=${provider.witness?._tag ?? 'none'} purposes=${provider.purposes.join('+')}`,
  ),
]

const suspensionLines = (fn: MirFunction): ReadonlyArray<string> => {
  const suspension = fn.suspension
  if (suspension === undefined) return []
  return [
    `  suspension-classification ${suspension.classification.toLowerCase()}`,
    ...suspension.regions.flatMap((region) => {
      if (region._tag === 'SuspendEffectRegion')
        return [
          `  suspend-origin ${suspensionPointText(region.point)} owner=${regionText(region.ownerRegion)} operation=${region.operation._tag} transfer=private-frame-stack`,
          ...suspensionRunnerLines(region.deferred),
        ]
      const descriptor = region.relay.state
      return [
        `  suspend-run ${suspensionPointText(region.point)} owner=${regionText(region.ownerRegion)} operation=${region.operation._tag} runner=${region.runner.declaration === undefined ? 'unknown' : targetText(region.runner.declaration)} complete=current relay=preserve-child-origin-outcome frame=${region.relay.frame.toLowerCase()}`,
        ...suspensionRunnerLines(region.runner),
        region.completion._tag === 'Propagate'
          ? `    completion propagate outcome=${SilkType.encode(region.completion.outcome)} mappings=${region.completion.failureMappings.map((mapping) => `${mapping.source}:${mapping.target}`).join(',') || 'none'}`
          : `    completion reify outcome=${SilkType.encode(region.completion.outcome)} result=${SilkType.encode(region.completion.resultType)} success-tag=${region.completion.successTag} failure-tag=${region.completion.failureTag}`,
        `    live ${region.liveLocals.map(localText).join(',') || 'none'}`,
        ...(descriptor === undefined
          ? []
          : [
              `    descriptor outcome=${SilkType.encode(descriptor.outcome)} resume-success=${suspensionPointText(descriptor.success.resume.point)}:${descriptor.success.resume.path.toLowerCase()} resume-failure=${suspensionPointText(descriptor.failure.resume.point)}:${descriptor.failure.resume.path.toLowerCase()}`,
              ...descriptor.slots.map((slot) =>
                slot.access._tag === 'Copy'
                  ? `    slot ${slot.ordinal} ${localText(slot.local)} copy ${typeText(slot.type)}`
                  : slot.access._tag === 'BorrowedDependency'
                    ? `    slot ${slot.ordinal} ${localText(slot.local)} borrow:${slot.access.access.toLowerCase()} root=${localText(slot.access.root)} ${typeText(slot.type)}`
                    : `    slot ${slot.ordinal} ${localText(slot.local)} move:${slot.access.cleanup._tag} ${typeText(slot.type)}`,
              ),
              continuationPathText('success', descriptor.success),
              continuationPathText('failure', descriptor.failure),
            ]),
      ]
    }),
  ]
}

const coroutineFrameTargetLines = (self: Module): ReadonlyArray<string> =>
  (self.coroutineFrames?.entries ?? []).flatMap((entry) => [
    `coroutine-frame ${instanceText(entry.function)} size=${entry.size} alignment=${entry.alignment} storage=private-execution-stack`,
    ...entry.header.map(
      (field) =>
        `  header ${field.role.toLowerCase()} offset=${field.offset} size=${field.size} alignment=${field.alignment}`,
    ),
    ...entry.states.flatMap((state) => [
      `  state ${suspensionPointText(state.point)} size=${state.size} alignment=${state.alignment} tail-padding=${state.tailPadding}`,
      ...state.payload.map(
        (field) =>
          `    payload slot=${field.slot} local=${localText(field.local)} offset=${field.offset} size=${field.size} alignment=${field.alignment} padding=${field.padding}`,
      ),
    ]),
  ])

export const encode = (self: Module): string =>
  [
    `mir-module ${self.module}`,
    self.entry._tag === 'UnavailableEntry'
      ? `entry unavailable reason=${self.entry.reason}`
      : self.entry._tag === 'OrdinaryEntry'
        ? `entry ordinary target=${targetText(self.entry.target.declaration)} machine=${targetText(self.entry.machine.declaration)}`
        : `entry effect target=${targetText(self.entry.target.declaration)} machine=${targetText(self.entry.machine.declaration)} failures=${self.entry.failures.map((failure) => `${failure.tag}:${failure.identity}`).join(',') || 'none'} requirements=${self.entry.requirements.map((requirement) => `${requirement.access}:${SilkType.encode(requirement.capability)}@${requirement.role}`).join(',') || 'none'}`,
    ...(self.staticData ?? []).map(
      (data) =>
        `static ${data.id} kind=${data.kind.toLowerCase()} utf8=${data.utf8} bytes=${data.bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('')}`,
    ),
    ...(self.normalization ?? []).map((verdict) =>
      verdict._tag === 'Normalized'
        ? `normalization accepted kind=${verdict.kind} function=${targetText(verdict.function)} region=${regionText(verdict.region)} local=${localText(verdict.local)} guards=${verdict.guards.join(',')} ${provenanceText(verdict.provenance)}`
        : `normalization rejected reason=${verdict.reason} function=${targetText(verdict.function)} region=${regionText(verdict.region)} local=${localText(verdict.local)} ${provenanceText(verdict.provenance)}`,
    ),
    ...coroutineFrameTargetLines(self),
    ...Layout.encode(self.layout).trimEnd().split('\n'),
    ...self.functions.flatMap((fn) => [
      `fn ${targetText(fn.id)}${
        fn.instance.typeArguments.length === 0
          ? ''
          : `<${fn.instance.typeArguments.map(SilkType.encodeGenericArgument).join(', ')}>`
      } params=${fn.parameterCount} locals=${fn.localTypes.length} -> ${typeText(fn.result)} entry=${regionText(fn.entry)}`,
      ...suspensionLines(fn),
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
    intrinsics: Object.freeze([]),
    entry: Object.freeze({
      _tag: 'OrdinaryEntry',
      target: instance(canonical(source.id, 'answer')),
      machine: instance(canonical(source.id, 'answer')),
    }),
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
    intrinsics: Object.freeze([]),
    entry: Object.freeze({
      _tag: 'OrdinaryEntry',
      target: instance(canonical(source.id, 'choose')),
      machine: instance(canonical(source.id, 'choose')),
    }),
    layout: Layout.make(Target.aarch64AppleDarwin, ['i32', 'bool']),
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical(source.id, 'choose'),
        instance: instance(canonical(source.id, 'choose')),
        parameterCount: 0,
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
