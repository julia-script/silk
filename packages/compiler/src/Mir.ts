import type * as CleanupPlan from './CleanupPlan.js'
import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as ExecutionPackage from './ExecutionPackage.js'
import type * as ExecutionTransition from './ExecutionTransition.js'
import type * as Hir from './Hir.js'
import type * as Instances from './Instances.js'
import type * as Intrinsic from './Intrinsic.js'
import * as Layout from './Layout.js'
import * as LayoutVerify from './LayoutVerify.js'
import type * as LocalSharedControlBlock from './LocalSharedControlBlock.js'
import type * as Match from './Match.js'
import { instanceText, operationLocals } from './MirVerification.js'
import type * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StaticText from './StaticText.js'
import type {
  SuspensionBorrowIdentity,
  SuspensionClassification,
  SuspensionCompletion,
  SuspensionControlEdge,
  SuspensionPointId,
  SuspensionProviderArgument,
  SuspensionRegion,
  SuspensionRunner,
} from './Suspension.js'
import * as SilkType from './Type.js'
import type * as TypeCompatibility from './TypeCompatibility.js'

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
  | {
      readonly _tag: 'Enum'
      readonly type: SilkType.Nominal
      readonly representation: Extract<Layout.Representation, { readonly _tag: 'ScalarEnum' }>
    }
  | { readonly _tag: 'FixedArray'; readonly type: SilkType.FixedArray }
  | { readonly _tag: 'String'; readonly type: SilkType.String }
  | { readonly _tag: 'Slice'; readonly type: SilkType.Slice }
  | { readonly _tag: 'Reference'; readonly type: SilkType.Reference }
  | { readonly _tag: 'Union'; readonly type: SilkType.StructuralUnion }
  | {
      readonly _tag: 'EffectBorrow'
      readonly type: DeclarationFacts.SemanticType
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

export const semanticType = (self: Type): DeclarationFacts.SemanticType => {
  if (self._tag === 'CallableValue' || self._tag === 'EffectValue')
    return self.storage?.type ?? self.type
  if (self._tag === 'EffectComposite') return self.type
  return self._tag === 'Nominal' ||
    self._tag === 'Enum' ||
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

export const typeText = (self: Type): string => SilkType.encode(semanticType(self))
/** Reads the concrete sealed Copy verdict published by target layout. */
export const isCopy = (layout: Layout.Plan, type: DeclarationFacts.SemanticType): boolean =>
  Layout.entry(layout, type)?.copy === true

const callingScalarEquals = (left: Layout.CallingScalar, right: Layout.CallingScalar): boolean =>
  typeof left === 'string'
    ? left === right
    : typeof right !== 'string' &&
      SilkType.equals(left.element, right.element) &&
      left.bits === right.bits

export const callingShapeEquals = (
  left: Layout.CallingShape,
  right: Layout.CallingShape,
): boolean =>
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
        return other !== undefined && LayoutVerify.selectorEquals(selector, other)
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
      readonly function: DeclarationFacts.CanonicalId
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
      readonly function: DeclarationFacts.CanonicalId
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
      readonly field: DeclarationFacts.FieldId
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
      readonly _tag: 'EnumConstant'
      readonly destination: LocalId
      readonly enum: DeclarationFacts.CanonicalId
      readonly member: DeclarationFacts.CanonicalEnumMemberId
      readonly discriminant: bigint
      readonly representation: Extract<Layout.Representation, { readonly _tag: 'ScalarEnum' }>
      readonly type: Extract<Type, { readonly _tag: 'Enum' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'EnumValue'
      readonly destination: LocalId
      readonly source: LocalId
      readonly enum: DeclarationFacts.CanonicalId
      readonly representation: Extract<Layout.Representation, { readonly _tag: 'ScalarEnum' }>
      readonly type: ScalarType
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'EnumEquality'
      readonly destination: LocalId
      readonly left: LocalId
      readonly right: LocalId
      readonly enum: DeclarationFacts.CanonicalId
      readonly negated: boolean
      readonly representation: Extract<Layout.Representation, { readonly _tag: 'ScalarEnum' }>
      readonly type: Extract<Type, { readonly _tag: 'bool' }>
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
      readonly _tag: 'CheckedScalar'
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
      readonly element: DeclarationFacts.SemanticType
      readonly stride: number
      readonly elementAlignment: number
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Consumes one exact allocation and T into one initialized local-shared control block. */
      readonly _tag: 'SharedFromAllocation'
      readonly destination: LocalId
      readonly allocation: LocalId
      readonly value: LocalId
      readonly element: DeclarationFacts.SemanticType
      readonly block: LocalSharedControlBlock.Plan
      readonly allocationBlock: LocalSharedControlBlock.Plan
      /** Stable index into the canonical target-layout allocation-provenance plan. */
      readonly allocationFact: number
      /** Canonical allocation-layout origin retained from target planning. */
      readonly allocationProvenance: SourceSpan.SourceSpan
      readonly allocationAccess: 'Take'
      readonly valueAccess: 'Take'
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Consumes every input into one exact Initial execution package without running source. */
      readonly _tag: 'ExecutionFromAllocation'
      readonly destination: LocalId
      readonly allocation: LocalId
      readonly body: LocalId
      readonly endpoint: LocalId
      readonly callback: LocalId
      readonly plan: ExecutionPackage.Plan
      readonly bodyCleanup: CleanupPlan.CleanupPlan
      readonly endpointCleanup: CleanupPlan.CleanupPlan
      readonly callbackCleanup: CleanupPlan.CleanupPlan
      /** Stable index into the canonical target-layout allocation-provenance plan. */
      readonly allocationFact: number
      /** Canonical execution-layout origin retained from target planning. */
      readonly allocationProvenance: SourceSpan.SourceSpan
      readonly allocationAccess: 'Take'
      readonly bodyAccess: 'Take'
      readonly endpointAccess: 'Take'
      readonly callbackAccess: 'Take'
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Enters one verified execution activation and invokes exactly one take-once outcome. */
      readonly _tag: 'ExecutionDrive'
      readonly destination: LocalId
      /** Private result slot populated before the completion callback consumes it. */
      readonly result: LocalId
      readonly execution: LocalId
      readonly branch: LocalId
      readonly onComplete: LocalId
      readonly onSuspend: LocalId
      readonly executionAccess: 'Take'
      readonly branchAccess: 'Take'
      readonly completionAccess: 'Take'
      readonly suspensionAccess: 'Take'
      readonly completionCleanup: CleanupPlan.CleanupPlan
      readonly suspensionCleanup: CleanupPlan.CleanupPlan
      readonly completionTypeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly suspensionTypeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Consumes the generation's sole affine Wake readiness authority. */
      readonly _tag: 'ExecutionWake'
      readonly destination: LocalId
      readonly wake: LocalId
      readonly wakeAccess: 'Take'
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Registers one Wake and retains the returned guard before relinquishing this execution. */
      readonly _tag: 'ExecutionPark'
      readonly destination: LocalId
      /** Private guard slot retained by the execution-owned suspension state. */
      readonly guard: LocalId
      readonly register: LocalId
      readonly registerAccess: 'Take'
      readonly guardCleanup: CleanupPlan.CleanupPlan
      readonly registerCleanup: CleanupPlan.CleanupPlan
      readonly registrationTypeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Allocation-free compare-before-increment of one local strong count. */
      readonly _tag: 'SharedClone'
      readonly destination: LocalId
      readonly self: LocalId
      readonly element: DeclarationFacts.SemanticType
      readonly block: LocalSharedControlBlock.Plan
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** One closed access transition which invokes exactly one take-once callback. */
      readonly _tag: 'SharedWithMut'
      readonly destination: LocalId
      readonly payload: LocalId
      readonly self: LocalId
      readonly use: LocalId
      readonly onConflict: LocalId
      readonly element: DeclarationFacts.SemanticType
      readonly block: LocalSharedControlBlock.Plan
      readonly useType: SilkType.Callable
      readonly conflictType: SilkType.Callable
      readonly useCleanup: CleanupPlan.CleanupPlan
      readonly conflictCleanup: CleanupPlan.CleanupPlan
      /** Compiler-owned identity of the callback-scoped exclusive payload loan. */
      readonly loan: Hir.BorrowId
      /** Must remain empty: no result or executable state may retain `loan`. */
      readonly retainedLoans: ReadonlyArray<Hir.BorrowId>
      readonly type: Type
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
      readonly element: DeclarationFacts.SemanticType
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      /** Bounds-checked non-consuming read through a shared RawBuffer borrow. */
      readonly _tag: 'RawBufferRead'
      readonly destination: LocalId
      readonly buffer: LocalId
      readonly index: LocalId
      readonly element: DeclarationFacts.SemanticType
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
      readonly element: DeclarationFacts.SemanticType
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
      readonly element: DeclarationFacts.SemanticType
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
      readonly element: DeclarationFacts.SemanticType
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'SlotTake'
      readonly destination: LocalId
      readonly slot: LocalId
      readonly element: DeclarationFacts.SemanticType
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      /** Non-consuming read of an initialized slot; verified for Copy element types only. */
      readonly _tag: 'SlotCopy'
      readonly destination: LocalId
      readonly slot: LocalId
      readonly element: DeclarationFacts.SemanticType
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'SlotDrop'
      readonly destination: LocalId
      readonly slot: LocalId
      readonly element: DeclarationFacts.SemanticType
      readonly cleanup: CleanupPlan.CleanupPlan
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
      readonly access: SilkType.BorrowAccess
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
      readonly target: DeclarationFacts.CanonicalId
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly arguments: ReadonlyArray<LocalId>
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'MakeEffect'
      readonly destination: LocalId
      readonly runner: DeclarationFacts.CanonicalId
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
      readonly target: DeclarationFacts.CanonicalId
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
      readonly runner: DeclarationFacts.CanonicalId
      readonly runnerTypeArguments: ReadonlyArray<SilkType.GenericArgument>
      /** Exact unsupplied runner retained when `runner` is a statically provided specialization. */
      readonly runnerBase?: {
        readonly declaration: DeclarationFacts.CanonicalId
        readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
      }
      /** Ordered compile-time provider selections proving the exact requirement row. */
      readonly providers: ReadonlyArray<{
        readonly capability: SilkType.Nominal
        readonly providerType: SilkType.Nominal
        readonly witness: DeclarationFacts.ConformanceWitness
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
        readonly runner: DeclarationFacts.CanonicalId
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
      readonly runner: DeclarationFacts.CanonicalId
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
      readonly runner: DeclarationFacts.CanonicalId
      readonly runnerTypeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly arguments: ReadonlyArray<LocalId>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly resultType: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly resultField: DeclarationFacts.FieldId
      readonly resultUnion: SilkType.StructuralUnion
      readonly successType: SilkType.Nominal
      readonly successField: DeclarationFacts.FieldId
      readonly successTag: number
      readonly failureType: SilkType.Nominal
      readonly failureField: DeclarationFacts.FieldId
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
      readonly target: DeclarationFacts.CanonicalId
      readonly runner: DeclarationFacts.CanonicalId
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
      readonly effectType: Extract<Type, { readonly _tag: 'EffectValue' }>
      readonly outcomeType: Extract<Type, { readonly _tag: 'EffectOutcome' }>
      readonly failures: ReadonlyArray<{
        readonly tag: number
        readonly type: SilkType.Type
        readonly identity: string
        readonly payload: LocalId
        readonly cleanup: CleanupPlan.CleanupPlan
      }>
      readonly type: Extract<Type, { readonly _tag: 'i32' }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Construct'
      readonly destination: LocalId
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationFacts.FieldId
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
      readonly field: DeclarationFacts.FieldId
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
  readonly cleanup: CleanupPlan.CleanupPlan
  /** Exact target plan for an opaque local-shared core drop. Absent for every other cleanup. */
  readonly localShared?: {
    readonly element: SilkType.Type
    readonly block: LocalSharedControlBlock.Plan
  }
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
  readonly path: ReadonlyArray<DeclarationFacts.FieldId>
  readonly type: Type
  readonly access: Match.Access
  readonly provenance: Provenance
}

export interface MatchArm {
  readonly id: Match.ArmId
  readonly member?: Match.CoverageIdentity
  readonly universal: boolean
  readonly before: ReadonlyArray<Match.CoverageIdentity>
  readonly after: ReadonlyArray<Match.CoverageIdentity>
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
      readonly path: ReadonlyArray<DeclarationFacts.FieldId>
      readonly cleanup: CleanupPlan.CleanupPlan
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
  readonly scrutineeType: Type
  readonly scrutineeShape: Layout.CallingShape
  readonly access: Match.Access
  /** Statement patterns retain selected locals beyond this operation; expression matches do not. */
  readonly retainsBindings: boolean
  readonly members: ReadonlyArray<Match.CoverageIdentity>
  readonly decisions: ReadonlyArray<{
    readonly member: Match.CoverageIdentity
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

export type {
  SuspensionBorrowIdentity,
  SuspensionClassification,
  SuspensionCompletion,
  SuspensionControlEdge,
  SuspensionPointId,
  SuspensionProviderArgument,
  SuspensionRegion,
  SuspensionRunner,
}

export interface ResumePointId {
  readonly _tag: 'ResumePointId'
  readonly point: SuspensionPointId
  readonly path: 'Success' | 'Failure'
}

export type CoroutineFrameAccess =
  | { readonly _tag: 'Copy' }
  | {
      readonly _tag: 'BorrowedDependency'
      readonly access: 'Shared' | 'Exclusive'
      readonly root: LocalId
      readonly loan: SuspensionBorrowIdentity
    }
  | { readonly _tag: 'AffineTransfer'; readonly cleanup: CleanupPlan.CleanupPlan }

export interface CoroutineFrameSlot {
  readonly ordinal: number
  readonly local: LocalId
  readonly type: Type
  readonly access: CoroutineFrameAccess
}

export interface CoroutineFrameRelease {
  readonly local: LocalId
  readonly cleanup: CleanupPlan.CleanupPlan
}

export interface CoroutineFramePathPlan {
  readonly restores: ReadonlyArray<number>
  readonly loanEnds: ReadonlyArray<SuspensionBorrowIdentity>
  readonly releases: ReadonlyArray<CoroutineFrameRelease>
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

export type SuspendEffectRegion = Extract<
  SuspensionRegion,
  { readonly _tag: 'SuspendEffectRegion' }
>

export type RunSuspendableEffectRegion = Extract<
  SuspensionRegion,
  { readonly _tag: 'RunSuspendableEffectRegion' }
>

export interface MirFunction {
  readonly _tag: 'MirFunction'
  readonly id: DeclarationFacts.CanonicalId
  readonly instance: Instances.InstanceKey
  readonly parameterCount: number
  readonly localTypes: ReadonlyArray<Type>
  readonly result: Type
  readonly entry: RegionId
  readonly regions: ReadonlyArray<Region>
  /** Static origin of a generated Effect runner, including every selected provider witness. */
  readonly effectRunner?: {
    readonly base: {
      readonly declaration: DeclarationFacts.CanonicalId
      readonly typeArguments: ReadonlyArray<SilkType.GenericArgument>
    }
    readonly providers: ReadonlyArray<{
      readonly capability: SilkType.Nominal
      readonly providerType: SilkType.Nominal
      readonly witness: DeclarationFacts.ConformanceWitness
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
  /** Complete logical lifecycle authority verified before evaluator or backend lowering. */
  readonly executionTransitions: ReadonlyArray<ExecutionTransition.Authority>
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
  declaration: DeclarationFacts.CanonicalId,
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

export const conformanceWitnessMatches = (
  left: DeclarationFacts.ConformanceWitness,
  right: DeclarationFacts.ConformanceWitness,
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

export const regionTargets = (
  region: Region,
): ReadonlyArray<readonly [RegionId, ControlEdge['kind']]> => {
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
    | 'InvalidLocalSharedOperation'
    | 'InvalidExecutionOperation'
    | 'InvalidCallShape'
    | 'InvalidCallableOperation'
    | 'InvalidEffectOperation'
    | 'InvalidNormalization'
    | 'InvalidEntry'
    | 'InvalidWrite'
    | 'InvalidLoan'
    | 'InvalidSliceOperation'
    | 'InvalidStringOperation'
    | 'InvalidEnumOperation'
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
  readonly function?: DeclarationFacts.CanonicalId
  readonly region?: RegionId
  /** The exact authored/generated operation that caused a local-shared rejection. */
  readonly provenance?: Provenance
  readonly localSharedReason?:
    | 'CleanupContract'
    | 'InitializationContract'
    | 'CloneContract'
    | 'AccessContract'
  readonly detail: string
}

export const operationsOf = (region: Region): ReadonlyArray<Operation> =>
  region._tag === 'OperationRegion'
    ? region.operations
    : region._tag === 'CleanupRegion'
      ? region.releases
      : []

export const operationChildren = (operation: Operation): ReadonlyArray<Operation> =>
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
