import type * as ChildProcess from './ChildProcess.js'
import * as ContinuationTransaction from './ContinuationTransaction.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import * as FloatingPoint from './FloatingPoint.js'
import type * as Hir from './Hir.js'
import type * as HostInput from './HostInput.js'
import * as Instances from './Instances.js'
import * as IntrinsicAvailability from './IntrinsicAvailability.js'
import type * as Layout from './Layout.js'
import type * as Match from './Match.js'
import * as Mir from './Mir.js'
import * as OsFileSystemHost from './OsFileSystemHost.js'
import type * as Ownership from './Ownership.js'
import * as Scalar from './Scalar.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StandardInput from './StandardInput.js'
import type * as StandardStreams from './StandardStreams.js'
import * as Transcendental from './Transcendental.js'
import * as Type from './Type.js'

/**
 * The closed bootstrap interpreter, executing the lowered MIR program from the entry instance
 * discovery resolved. It is the semantics oracle for the coming native differential checks and
 * the second consumer keeping MIR's meaning in MIR. A severable leaf: nothing in the pipeline
 * depends on it.
 */

/** The exact value produced by the closed bootstrap interpreter. */
export interface I32Value {
  readonly _tag: 'I32Value'
  readonly value: number
}

/** An exact target-sized unsigned integer, independent of host number precision. */
export interface UsizeValue {
  readonly _tag: 'UsizeValue'
  readonly value: bigint
}

/** An exact fixed- or target-width integer outside the legacy i32/usize value views. */
export interface ScalarIntegerValue {
  readonly _tag: 'ScalarIntegerValue'
  readonly type: Scalar.IntegerSpelling
  readonly value: bigint
}

/**
 * One Unicode scalar value.
 *
 * `char` is its own scalar category rather than an integer, so it is its own value rather than an
 * integer view: nothing that reads an integer accepts it, and the only operations that read it are
 * the equality and ordering lanes the catalog declares.
 */
export interface CharacterValue {
  readonly _tag: 'CharacterValue'
  readonly value: number
}

export interface FloatValue {
  readonly _tag: 'FloatValue'
  readonly type: Scalar.FloatSpelling
  readonly bits: bigint
}

export interface AggregateValue {
  readonly _tag: 'AggregateValue'
  readonly type: Type.Nominal
  readonly fields: ReadonlyArray<{
    readonly field: DeclarationIndex.FieldId
    readonly value: Value
  }>
}

export interface ArrayValue {
  readonly _tag: 'ArrayValue'
  readonly type: Type.FixedArray
  readonly elements: ReadonlyArray<Value>
}

/** A logical borrowed view. Permission and loan identity remain compiler facts, not values. */
export interface SliceValue {
  readonly _tag: 'SliceValue'
  readonly frame: number
  readonly cell: number
  readonly base: number
  readonly length: number
  /** Present only for a zero-copy RawBuffer-backed slice. */
  readonly ticket?: number
}

/** Allocation-free immutable view of one compiler-owned static-data entry. */
export interface StaticViewValue {
  readonly _tag: 'StaticViewValue'
  readonly data: string
  readonly bytes: ReadonlyArray<number>
  readonly length: number
}

export type StringStorage =
  | {
      readonly _tag: 'StaticTextStorage'
      readonly data: string
      readonly bytes: ReadonlyArray<number>
    }
  | {
      readonly _tag: 'StaticByteStorage'
      readonly data: string
      readonly bytes: ReadonlyArray<number>
    }
  | {
      readonly _tag: 'RuntimeSliceStorage'
      readonly view: SliceValue
    }

/** A logical valid UTF-8 view retaining storage identity and lexical backing facts. */
export interface StringValue {
  readonly _tag: 'StringValue'
  readonly storage: StringStorage
  readonly bytes: ReadonlyArray<number>
  readonly byteLength: number
  readonly heldLoans: ReadonlyArray<string>
}

export interface ReferenceValue {
  readonly _tag: 'ReferenceValue'
  readonly frame: number
  readonly cell: number
  readonly selectors: ReadonlyArray<Extract<Mir.PlaceSelector, { readonly _tag: 'FieldSelector' }>>
}

export interface UnionValue {
  readonly _tag: 'UnionValue'
  readonly type: Type.StructuralUnion
  readonly member: Type.Nominal
  readonly payload: AggregateValue
}

export interface EffectOutcomeValue {
  readonly _tag: 'EffectOutcomeValue'
  readonly type: Type.Effect
  readonly tag: number
  readonly payload: Value
}

export interface EffectBorrowValue {
  readonly _tag: 'EffectBorrowValue'
  readonly frame: number
  readonly cell: number
  readonly access: 'Shared' | 'Exclusive'
}

export interface CallableBorrowValue {
  readonly _tag: 'CallableBorrowValue'
  readonly frame: number
  readonly cell: number
  readonly access: 'Shared' | 'Exclusive'
}

export interface CallableValue {
  readonly _tag: 'CallableValue'
  readonly ticket: number
  readonly type: Type.Callable
  readonly target: Hir.CallableTarget
  readonly typeArguments: ReadonlyArray<Type.GenericArgument>
  readonly captures: ReadonlyArray<{
    readonly ordinal: number
    readonly parameterOrdinal: number
    readonly access: 'Copy' | 'Shared' | 'Exclusive' | 'Take'
    readonly value: Value
  }>
}

export interface EffectValue {
  readonly _tag: 'EffectValue'
  readonly type: Type.Effect
  readonly site: Hir.EffectSiteId
  readonly runner: DeclarationIndex.CanonicalId
  readonly runnerTypeArguments: ReadonlyArray<Type.GenericArgument>
  readonly captures: ReadonlyArray<Value>
}

/** One logical heap block; identity and liveness live in evaluator state, not JS identity. */
export interface AllocationValue {
  readonly _tag: 'AllocationValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly bytes: bigint
  readonly alignment: bigint
}

export interface RawBufferValue {
  readonly _tag: 'RawBufferValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly count: bigint
  readonly element: Type.Type
  readonly stride: number
}

export interface SlotValue {
  readonly _tag: 'SlotValue'
  readonly type: Type.Nominal
  readonly ticket: number
  readonly index: bigint
  readonly element: Type.Type
}

/** One immutable logical evaluator value, independent of backend lane realization. */
export type Value =
  | I32Value
  | UsizeValue
  | ScalarIntegerValue
  | CharacterValue
  | FloatValue
  | AggregateValue
  | ArrayValue
  | SliceValue
  | StaticViewValue
  | StringValue
  | ReferenceValue
  | UnionValue
  | EffectBorrowValue
  | CallableBorrowValue
  | EffectValue
  | CallableValue
  | EffectOutcomeValue
  | AllocationValue
  | RawBufferValue
  | SlotValue

/** Entered the resolved entry instance. */
export interface EntryTraceEvent {
  readonly _tag: 'Entry'
  readonly frame: number
  readonly depth: number
  readonly function: DeclarationIndex.CanonicalId
  readonly instance: Instances.InstanceKey
  readonly span: SourceSpan.SourceSpan
}

/** Executed one call operation after its argument locals were computed. */
export interface CallTraceEvent {
  readonly _tag: 'Call'
  readonly frame: number
  readonly depth: number
  readonly caller: DeclarationIndex.CanonicalId
  readonly target: DeclarationIndex.CanonicalId
  readonly callerInstance: Instances.InstanceKey
  readonly targetInstance: Instances.InstanceKey
  readonly span: SourceSpan.SourceSpan
}

/** Bound one computed argument value to its positional parameter local. */
export interface BindingTraceEvent {
  readonly _tag: 'Binding'
  readonly frame: number
  readonly depth: number
  readonly target: DeclarationIndex.CanonicalId
  readonly targetInstance: Instances.InstanceKey
  readonly callSpan: SourceSpan.SourceSpan
  readonly argumentOrdinal: number
  readonly parameterOrdinal: number
  readonly value: Value
  readonly fromCall: boolean
  readonly span: SourceSpan.SourceSpan
}

/** Returned one evaluated value from a lowered function. */
export interface ReturnTraceEvent {
  readonly _tag: 'Return'
  readonly frame: number
  readonly depth: number
  readonly function: DeclarationIndex.CanonicalId
  readonly instance: Instances.InstanceKey
  readonly value: Value
  readonly span: SourceSpan.SourceSpan
}

/** One deterministic event emitted while replaying lowered operations. */
export interface ConstructTraceEvent {
  readonly _tag: 'Construct'
  readonly function: DeclarationIndex.CanonicalId
  readonly type: Type.Nominal
  readonly fieldCount: number
  readonly span: SourceSpan.SourceSpan
}

export interface ProjectTraceEvent {
  readonly _tag: 'Project'
  readonly function: DeclarationIndex.CanonicalId
  readonly type: Type.Nominal
  readonly field: DeclarationIndex.FieldId
  readonly span: SourceSpan.SourceSpan
}

export interface ArrayConstructTraceEvent {
  readonly _tag: 'ArrayConstruct'
  readonly function: DeclarationIndex.CanonicalId
  readonly type: Type.FixedArray
  readonly elementCount: number
  readonly span: SourceSpan.SourceSpan
}

export interface UnionConversionTraceEvent {
  readonly _tag: 'UnionConversion'
  readonly function: DeclarationIndex.CanonicalId
  readonly conversion: 'Inject' | 'Widen'
  readonly source: Type.Nominal | Type.StructuralUnion
  readonly target: Type.StructuralUnion
  readonly member: Type.Nominal
  readonly span: SourceSpan.SourceSpan
}

export interface PlaceReadTraceEvent {
  readonly _tag: 'PlaceRead'
  readonly function: DeclarationIndex.CanonicalId
  readonly selectors: ReadonlyArray<
    | { readonly _tag: 'Field'; readonly field: DeclarationIndex.FieldId }
    | {
        readonly _tag: 'Element'
        readonly array: Type.FixedArray
        readonly index: number
        readonly bounds: 'Proven' | 'Checked'
        readonly span: SourceSpan.SourceSpan
      }
    | {
        readonly _tag: 'StaticElement'
        readonly data: string
        readonly index: number
        readonly bounds: 'Checked'
        readonly span: SourceSpan.SourceSpan
      }
    | {
        readonly _tag: 'RawBufferElement'
        readonly ticket: number
        readonly index: number
        readonly bounds: 'Checked'
        readonly span: SourceSpan.SourceSpan
      }
  >
  readonly value: Value
  readonly span: SourceSpan.SourceSpan
}

export interface CleanupTraceEvent {
  readonly _tag: 'Cleanup'
  readonly frame: number
  readonly depth: number
  readonly function: DeclarationIndex.CanonicalId
  readonly local: number
  readonly members?: ReadonlyArray<Type.Nominal>
  readonly span: SourceSpan.SourceSpan
}

export interface MatchTraceEvent {
  readonly _tag:
    | 'MatchDispatch'
    | 'MatchCandidate'
    | 'MatchSelected'
    | 'MatchCleanup'
    | 'MatchBorrowEnd'
  readonly function: DeclarationIndex.CanonicalId
  readonly match: number
  readonly arm?: number
  readonly member: Type.Nominal
  readonly access: Match.Access
  readonly binding?: number
  readonly path?: ReadonlyArray<DeclarationIndex.FieldId>
  readonly value?: Value
  readonly members?: ReadonlyArray<Type.Nominal>
  readonly span: SourceSpan.SourceSpan
}

export interface ControlTraceEvent {
  readonly _tag:
    | 'RegionEntry'
    | 'Condition'
    | 'Iteration'
    | 'WriteCheck'
    | 'ReplacementCleanup'
    | 'Replacement'
    | 'Repeat'
    | 'Exit'
    | 'Transfer'
  readonly function: DeclarationIndex.CanonicalId
  readonly region: number
  readonly loop?: number
  readonly members?: ReadonlyArray<Type.Nominal>
  readonly span: SourceSpan.SourceSpan
}

export interface EffectTraceEvent {
  readonly _tag: 'EffectSuccess' | 'EffectFailure'
  readonly function: DeclarationIndex.CanonicalId
  readonly tag: number
  readonly span: SourceSpan.SourceSpan
}

export interface CallableTraceEvent {
  readonly _tag: 'CallableConstruct' | 'CallableApply' | 'CallableCleanup' | 'CallableRejected'
  readonly function: DeclarationIndex.CanonicalId
  readonly ticket: number
  readonly mode: Type.CallableMode
  readonly span: SourceSpan.SourceSpan
}

export interface AllocationTraceEvent {
  readonly _tag:
    | 'AllocationAcquire'
    | 'RawBufferForm'
    | 'SlotProject'
    | 'SlotWrite'
    | 'SlotTake'
    | 'SlotCopy'
    | 'RawBufferRead'
    | 'RawBufferCopy'
    | 'RawBufferFill'
    | 'SlotDrop'
    | 'AllocationRelease'
  readonly function: DeclarationIndex.CanonicalId
  readonly ticket: number
  readonly index?: bigint
  readonly count?: bigint
  readonly element?: Type.Type
  readonly span: SourceSpan.SourceSpan
}

export interface ContinuationTraceEvent {
  readonly _tag:
    | 'SuspensionOrigin'
    | 'ContinuationRequest'
    | 'ContinuationReject'
    | 'ContinuationAcquire'
    | 'ContinuationLoanEnd'
    | 'ContinuationInitialize'
    | 'ContinuationPublish'
    | 'ContinuationResume'
    | 'ContinuationRelease'
    | 'SuspensionChildStart'
    | 'SuspensionChildComplete'
  readonly function: DeclarationIndex.CanonicalId
  readonly point: Mir.SuspensionPointId
  readonly ordinal?: number
  readonly ticket?: number
  readonly bytes?: number
  readonly alignment?: number
  readonly outcome?: 'Success' | 'Failure'
  readonly span: SourceSpan.SourceSpan
}

/** One complete attempted host write, including its deterministic typed outcome. */
export interface StandardStreamTraceEvent {
  readonly _tag: 'HostWrite'
  readonly function: DeclarationIndex.CanonicalId
  readonly destination: StandardStreams.Destination
  readonly bytes: ReadonlyArray<number>
  readonly outcome: 'Written' | 'WriteFailure'
  readonly span: SourceSpan.SourceSpan
}

export interface StringTraceEvent {
  readonly _tag:
    | 'StringStatic'
    | 'StringRuntime'
    | 'StringBytes'
    | 'StringByteLength'
    | 'StringEqualsExact'
    | 'StringLoanEnd'
  readonly function: DeclarationIndex.CanonicalId
  readonly storage?: StringStorage['_tag']
  readonly byteLength?: number
  readonly result?: boolean
  readonly loan?: string
  readonly span: SourceSpan.SourceSpan
}

export type TraceEvent =
  | EntryTraceEvent
  | CallTraceEvent
  | BindingTraceEvent
  | ReturnTraceEvent
  | ConstructTraceEvent
  | ProjectTraceEvent
  | ArrayConstructTraceEvent
  | UnionConversionTraceEvent
  | PlaceReadTraceEvent
  | CleanupTraceEvent
  | MatchTraceEvent
  | ControlTraceEvent
  | EffectTraceEvent
  | CallableTraceEvent
  | AllocationTraceEvent
  | ContinuationTraceEvent
  | StandardStreamTraceEvent
  | StringTraceEvent

/** Every expected reason the closed bootstrap interpreter can stop. */
export type BlockedReason =
  | {
      readonly _tag: 'InvalidMir'
      readonly violations: ReadonlyArray<Mir.Violation>
    }
  | {
      readonly _tag: 'UnavailableEntry'
      readonly reason: Extract<Instances.Entry, { readonly _tag: 'Unavailable' }>['reason']
    }
  | {
      readonly _tag: 'Trap'
      readonly function: DeclarationIndex.CanonicalId
      readonly reason: string
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'MissingFunction'
      readonly target: DeclarationIndex.CanonicalId
      readonly span: SourceSpan.SourceSpan
    }
  | {
      readonly _tag: 'EvaluationLimit'
      readonly kind: 'Steps' | 'CallDepth'
      readonly limit: number
      readonly count: number
      readonly function: DeclarationIndex.CanonicalId
      readonly span: SourceSpan.SourceSpan
      readonly activeFrames: ReadonlyArray<ActiveFrame>
    }
  | {
      readonly _tag: 'InvalidCallableReuse'
      readonly function: DeclarationIndex.CanonicalId
      readonly ticket: number
      readonly state: 'Running' | 'Consumed' | 'Released'
      readonly span: SourceSpan.SourceSpan
    }
  | { readonly _tag: 'MissingStandardStreams' }
  | { readonly _tag: 'MissingStandardInput' }
  | { readonly _tag: 'MissingChildProcess' }
  | { readonly _tag: 'MissingHostInput' }
  | { readonly _tag: 'MissingOsFileSystemHost' }
  | {
      readonly _tag: 'IntrinsicTargetUnavailable'
      readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
    }

/** A completed exact bootstrap result. */
export interface Completed {
  readonly _tag: 'Completed'
  readonly entry: DeclarationIndex.CanonicalId
  readonly result: I32Value
  readonly trace: ReadonlyArray<TraceEvent>
}

/** A reportable application failure closed by the generated effect-entry adapter. */
export interface UnhandledFailure {
  readonly _tag: 'UnhandledFailure'
  readonly entry: DeclarationIndex.CanonicalId
  readonly tag: number
  readonly report: string
  readonly trace: ReadonlyArray<TraceEvent>
}

/** A normal, inspectable reason bootstrap evaluation could not complete. */
export interface Blocked {
  readonly _tag: 'Blocked'
  readonly entry: DeclarationIndex.CanonicalId | undefined
  readonly reason: BlockedReason
  readonly trace: ReadonlyArray<TraceEvent>
}

/** The closed outcome of executing one lowered program. */
export type Outcome = Completed | UnhandledFailure | Blocked

type Step =
  | { readonly _tag: 'Value'; readonly value: Value }
  | { readonly _tag: 'Blocked'; readonly reason: BlockedReason }
  | TransferStep

interface TransferStep {
  readonly _tag: 'Transfer'
  readonly origin: Mir.SuspendEffectRegion
  readonly child: CallRequest
}

const value = (input: number): I32Value => Object.freeze({ _tag: 'I32Value', value: input })
const usizeValue = (input: bigint): UsizeValue =>
  Object.freeze({ _tag: 'UsizeValue', value: input })
const scalarIntegerValue = (
  type: Scalar.IntegerSpelling,
  input: bigint,
): I32Value | UsizeValue | ScalarIntegerValue =>
  type === 'i32'
    ? value(Number(input))
    : type === 'usize'
      ? usizeValue(input)
      : Object.freeze({ _tag: 'ScalarIntegerValue', type, value: input })

const characterValue = (input: number): CharacterValue =>
  Object.freeze({ _tag: 'CharacterValue', value: input })

const floatValue = (type: Scalar.FloatSpelling, bits: bigint): FloatValue =>
  Object.freeze({ _tag: 'FloatValue', type, bits: BigInt.asUintN(type === 'f32' ? 32 : 64, bits) })

const borrowKey = (borrow: Hir.BorrowId): string =>
  `${borrow.function.sourceId}:${borrow.function.ordinal}:${borrow.callSpan.start}:${borrow.callSpan.end}:${borrow.ordinal}`

const floatingBits = (self: FloatValue): FloatingPoint.Bits =>
  Object.freeze({ width: self.type === 'f32' ? 32 : 64, bits: self.bits })

const floatingUnary = (
  operation: Extract<Mir.Operation, { readonly _tag: 'FloatUnary' }>['operation'],
  self: FloatValue,
): Value => {
  const bits = floatingBits(self)
  if (operation === 'Negate')
    return floatValue(self.type, self.bits ^ (1n << BigInt(bits.width - 1)))
  if (operation === 'Sqrt') return floatValue(self.type, FloatingPoint.squareRoot(bits).bits)
  const result =
    operation === 'IsNaN'
      ? FloatingPoint.isNotANumber(bits)
      : operation === 'IsInfinite'
        ? FloatingPoint.isInfinite(bits)
        : operation === 'IsFinite'
          ? FloatingPoint.isFiniteNumber(bits)
          : operation === 'IsNormal'
            ? FloatingPoint.isNormal(bits)
            : operation === 'IsSubnormal'
              ? FloatingPoint.isSubnormal(bits)
              : FloatingPoint.isSignNegative(bits)
  return value(result ? 1 : 0)
}

const floatingBinary = (
  operation: Mir.BinaryOperator,
  left: FloatValue,
  right: FloatValue,
): Value => {
  const leftBits = floatingBits(left)
  const rightBits = floatingBits(right)
  const leftNumber = FloatingPoint.toNumber(leftBits)
  const rightNumber = FloatingPoint.toNumber(rightBits)
  if (
    operation === 'Equals' ||
    operation === 'NotEquals' ||
    operation === 'LessThan' ||
    operation === 'LessOrEqual' ||
    operation === 'GreaterThan' ||
    operation === 'GreaterOrEqual'
  ) {
    const result =
      operation === 'Equals'
        ? leftNumber === rightNumber
        : operation === 'NotEquals'
          ? leftNumber !== rightNumber
          : operation === 'LessThan'
            ? leftNumber < rightNumber
            : operation === 'LessOrEqual'
              ? leftNumber <= rightNumber
              : operation === 'GreaterThan'
                ? leftNumber > rightNumber
                : leftNumber >= rightNumber
    return value(result ? 1 : 0)
  }
  if (operation === 'TotalOrder')
    return value(
      FloatingPoint.totalOrderKey(leftBits) <= FloatingPoint.totalOrderKey(rightBits) ? 1 : 0,
    )
  const result =
    operation === 'Add'
      ? leftNumber + rightNumber
      : operation === 'Subtract'
        ? leftNumber - rightNumber
        : operation === 'Multiply'
          ? leftNumber * rightNumber
          : operation === 'Divide'
            ? leftNumber / rightNumber
            : leftNumber % rightNumber
  const encoded = FloatingPoint.fromNumber(result, leftBits.width)
  return floatValue(left.type, encoded.bits)
}

const blockedStep = (reason: BlockedReason): Step =>
  Object.freeze({ _tag: 'Blocked', reason: Object.freeze(reason) })

const functionFor = (
  program: Mir.Module,
  id: DeclarationIndex.CanonicalId,
  typeArguments: ReadonlyArray<Type.GenericArgument>,
): Mir.MirFunction | undefined =>
  program.functions.find((fn) => Mir.matchesInstance(fn, id, typeArguments))

interface LocalState {
  readonly value: Value
  readonly fromCall: boolean
}

interface EvaluationState {
  nextFrame: number
  nextAllocation: number
  nextCallable: number
  nextContinuation: number
  steps: number
  readonly maxSteps: number
  readonly maxCallDepth: number
  activeFrames: ReadonlyArray<ActiveFrame>
  readonly cells: Map<string, LocalState>
  readonly allocations: Map<number, { active: boolean; readonly values: Map<string, Value> }>
  readonly callables: Map<number, { state: 'Available' | 'Running' | 'Consumed' | 'Released' }>
  readonly activeLoans: Set<string>
  readonly stringLoans: Set<string>
  readonly standardStreams?: StandardStreams.Provider
  readonly standardInput?: StandardInput.Provider
  readonly childProcess?: ChildProcess.Provider
  /** The captured streams of the most recent completed execution, indexed by stream selector. */
  readonly processCaptures: Array<ReadonlyArray<number>>
  readonly hostInput?: HostInput.Provider
  readonly osFileSystem?: OsFileSystemHost.Provider
}

export interface ActiveFrame {
  readonly frame: number
  readonly depth: number
  readonly function: DeclarationIndex.CanonicalId
  readonly instance: Instances.InstanceKey
}

interface CallRequest {
  readonly _tag: 'CallRequest'
  readonly target: Mir.MirFunction
  readonly arguments: ReadonlyArray<Value>
  readonly span: SourceSpan.SourceSpan
}

interface OriginTransferRequest {
  readonly _tag: 'OriginTransferRequest'
  readonly origin: Mir.SuspendEffectRegion
  readonly child: CallRequest
}

interface RelayTransferRequest {
  readonly _tag: 'RelayTransferRequest'
  readonly transfer: TransferStep
  readonly region: Mir.RunSuspendableEffectRegion
  readonly relay?: ContinuationTransaction.Relay
}

interface TransferContext {
  readonly step: TransferStep
  readonly relays: Array<ContinuationTransaction.Relay>
  readonly pending: Array<{
    readonly activation: ActivationRecord
    readonly relay?: ContinuationTransaction.Relay
  }>
  readonly allocations: Array<{
    readonly relay: ContinuationTransaction.Relay
    readonly allocation: AllocationValue
  }>
  preparation?: {
    index: number
    phase: 'Constructor' | 'Runner'
    readonly relay: ContinuationTransaction.Relay
    readonly pending: ActivationRecord
    activation: ActivationRecord
  }
  readonly parent?: TransferContext
}

type MachineRequest = CallRequest | OriginTransferRequest | RelayTransferRequest

interface ActivationRecord extends ActiveFrame {
  readonly locals: Map<number, LocalState>
  readonly cells: EvaluationState['cells']
  continuation?: FunctionExecution
  pendingCall?: CallRequest
  cleanupState?: Ownership.CleanupPlan['_tag']
  continuationTicket?: number
  continuationPoint?: Mir.SuspensionPointId
  continuationAllocation?: AllocationValue
}

type FunctionExecution = Generator<MachineRequest, Step, Step>
type CleanupExecution = Generator<MachineRequest, Step | undefined, Step>
type OperationsExecution = Generator<MachineRequest, Step | undefined, Step>

const cellKey = (frame: number, cell: number): string => `${frame}:${cell}`

function* executeFunction(
  program: Mir.Module,
  fn: Mir.MirFunction,
  activation: ActivationRecord,
  trace: Array<TraceEvent>,
  state: EvaluationState,
): FunctionExecution {
  const frame = activation.frame
  const locals = activation.locals

  const callFunction = function* (
    target: Mir.MirFunction,
    arguments_: ReadonlyArray<Value>,
    span: SourceSpan.SourceSpan,
  ): FunctionExecution {
    const request: CallRequest = Object.freeze({
      _tag: 'CallRequest',
      target,
      arguments: arguments_,
      span,
    })
    activation.pendingCall = request
    const result: Step = yield request
    delete activation.pendingCall
    return result
  }

  const suspensionFor = (
    operation: Mir.Operation,
  ): Mir.SuspendEffectRegion | Mir.RunSuspendableEffectRegion | undefined =>
    fn.suspension?.regions.find((region) => region.operation === operation)

  const callEffectRunner = function* (
    target: Mir.MirFunction,
    arguments_: ReadonlyArray<Value>,
    operation: Extract<
      Mir.Operation,
      { readonly _tag: 'RunEffect' | 'RunEffectValue' | 'ReifyEffect' }
    >,
  ): FunctionExecution {
    const control = suspensionFor(operation)
    if (control?._tag === 'SuspendEffectRegion') {
      const child: CallRequest = Object.freeze({
        _tag: 'CallRequest',
        target,
        arguments: arguments_,
        span: operation.provenance.span,
      })
      const request: OriginTransferRequest = Object.freeze({
        _tag: 'OriginTransferRequest',
        origin: control,
        child,
      })
      return yield request
    }
    const result = yield* callFunction(target, arguments_, operation.provenance.span)
    if (result._tag !== 'Transfer') return result
    if (control?._tag !== 'RunSuspendableEffectRegion') return result
    const descriptor = control.relay.continuation
    const relay =
      descriptor === undefined ? undefined : ContinuationTransaction.relay(program, fn, descriptor)
    const request: RelayTransferRequest = Object.freeze({
      _tag: 'RelayTransferRequest',
      transfer: result,
      region: control,
      ...(relay === undefined ? {} : { relay }),
    })
    return yield request
  }

  const evaluationLimit = (
    kind: 'Steps' | 'CallDepth',
    limit: number,
    count: number,
    span: SourceSpan.SourceSpan,
  ): Step =>
    blockedStep({
      _tag: 'EvaluationLimit',
      kind,
      limit,
      count,
      function: fn.id,
      span,
      activeFrames: Object.freeze([...state.activeFrames]),
    })

  const read = (local: Mir.LocalId): LocalState => {
    const direct = state.cells.get(cellKey(frame, local.ordinal)) ??
      locals.get(local.ordinal) ?? { value: value(0), fromCall: false }
    if (direct.value._tag !== 'EffectBorrowValue' && direct.value._tag !== 'CallableBorrowValue')
      return direct
    return (
      state.cells.get(cellKey(direct.value.frame, direct.value.cell)) ?? {
        value: value(0),
        fromCall: false,
      }
    )
  }

  const write = (local: Mir.LocalId, next: LocalState): void => {
    const alias = locals.get(local.ordinal)?.value
    if (alias?._tag === 'EffectBorrowValue') {
      state.cells.set(cellKey(alias.frame, alias.cell), next)
      return
    }
    locals.set(local.ordinal, next)
    const key = cellKey(frame, local.ordinal)
    if (state.cells.has(key)) state.cells.set(key, next)
  }

  const cell = (slice: SliceValue): LocalState => {
    const found = state.cells.get(cellKey(slice.frame, slice.cell))
    if (found === undefined) throw new RangeError('MIR slice references a missing evaluator cell')
    return found
  }

  const referenced = (local: Mir.LocalId): LocalState => {
    const reference = read(local).value
    if (reference._tag !== 'ReferenceValue') {
      throw new RangeError('MIR raw storage operation lost its whole-value reference')
    }
    const found = state.cells.get(cellKey(reference.frame, reference.cell))
    if (found === undefined)
      throw new RangeError('MIR reference points at a missing evaluator cell')
    let selected = found.value
    for (const selector of reference.selectors) {
      if (selected._tag !== 'AggregateValue')
        throw new RangeError('MIR reference selector points at a non-struct value')
      const field = selected.fields.find(
        (candidate) =>
          candidate.field.ordinal === selector.field.ordinal &&
          candidate.field.struct.sourceId === selector.field.struct.sourceId &&
          candidate.field.struct.ordinal === selector.field.struct.ordinal,
      )
      if (field === undefined) throw new RangeError('MIR reference selector lost its field')
      selected = field.value
    }
    return Object.freeze({ value: selected, fromCall: found.fromCall })
  }

  const readI32 = (local: Mir.LocalId): I32Value => {
    const found = read(local).value
    if (found._tag !== 'I32Value') {
      throw new RangeError(`MIR verifier allowed aggregate local %${local.ordinal} as a scalar`)
    }
    return found
  }

  const readUsize = (local: Mir.LocalId): UsizeValue => {
    const found = read(local).value
    if (found._tag !== 'UsizeValue') {
      throw new RangeError(`MIR verifier allowed non-usize local %${local.ordinal} as an index`)
    }
    return found
  }

  const readInteger = (local: Mir.LocalId): I32Value | UsizeValue | ScalarIntegerValue => {
    const found = read(local).value
    if (
      found._tag !== 'I32Value' &&
      found._tag !== 'UsizeValue' &&
      found._tag !== 'ScalarIntegerValue'
    ) {
      throw new RangeError(`MIR verifier allowed aggregate local %${local.ordinal} as an integer`)
    }
    return found
  }

  const readCharacter = (local: Mir.LocalId): CharacterValue => {
    const found = read(local).value
    if (found._tag !== 'CharacterValue')
      throw new RangeError(`MIR verifier allowed non-char local %${local.ordinal} as a char`)
    return found
  }

  const readFloat = (local: Mir.LocalId): FloatValue => {
    const found = read(local).value
    if (found._tag !== 'FloatValue')
      throw new RangeError(`MIR verifier allowed non-float local %${local.ordinal} as a float`)
    return found
  }

  /**
   * Releases one owned value through its complete cleanup plan: hooks run before their field
   * cleanup, struct fields release in declaration order, and every reclaim ticket is consumed
   * exactly once. Returns a blocked step when a hook call blocks or unsafe code already
   * consumed a ticket this plan still owns.
   */
  const releaseThroughPlan = function* (
    cleanup: Ownership.CleanupPlan,
    owner: Value,
    provenance: Mir.Provenance,
    localOrdinal: number,
  ): CleanupExecution {
    activation.cleanupState = cleanup._tag
    switch (cleanup._tag) {
      case 'NoCleanup':
      case 'ParameterCleanup':
        return undefined
      case 'AllocationCleanup': {
        if (owner._tag !== 'AllocationValue')
          throw new RangeError('Allocation cleanup lost its private reclaim ticket')
        const ticket = state.allocations.get(owner.ticket)
        if (ticket === undefined || !ticket.active) {
          // A caller obligation, not a compiler invariant: unsafe code can reach a second
          // release, and the run must stop as a trap rather than take down the host.
          return blockedStep({
            _tag: 'Trap',
            function: fn.id,
            reason: 'Allocation reclaim ticket was consumed more than once',
            span: provenance.span,
          })
        }
        ticket.active = false
        trace.push(
          Object.freeze({
            _tag: 'AllocationRelease',
            function: fn.id,
            ticket: owner.ticket,
            span: provenance.span,
          }),
        )
        return undefined
      }
      case 'RawBufferCleanup': {
        if (owner._tag !== 'RawBufferValue')
          throw new RangeError('RawBuffer cleanup lost its private reclaim ticket')
        const ticket = state.allocations.get(owner.ticket)
        if (ticket === undefined || !ticket.active) {
          return blockedStep({
            _tag: 'Trap',
            function: fn.id,
            reason: 'RawBuffer reclaim ticket was consumed more than once',
            span: provenance.span,
          })
        }
        ticket.active = false
        ticket.values.clear()
        trace.push(
          Object.freeze({
            _tag: 'AllocationRelease',
            function: fn.id,
            ticket: owner.ticket,
            span: provenance.span,
          }),
        )
        return undefined
      }
      case 'CallableCleanup': {
        if (owner._tag !== 'CallableValue')
          throw new RangeError('Callable cleanup lost its evaluator identity')
        const callable = state.callables.get(owner.ticket)
        if (callable === undefined)
          throw new RangeError('Callable cleanup referenced a missing evaluator identity')
        const wasAvailable = callable.state === 'Available'
        if (callable.state !== 'Consumed') callable.state = 'Released'
        trace.push(
          Object.freeze({
            _tag: 'CallableCleanup',
            function: fn.id,
            ticket: owner.ticket,
            mode: owner.type.mode,
            span: provenance.span,
          }),
        )
        if (!wasAvailable) return undefined
        for (const slot of cleanup.slots) {
          const capture = owner.captures.find((candidate) => candidate.ordinal === slot.ordinal)
          if (capture === undefined) continue
          const blocked = yield* releaseThroughPlan(
            slot.cleanup,
            capture.value,
            provenance,
            localOrdinal,
          )
          if (blocked !== undefined) return blocked
        }
        return undefined
      }
      case 'EffectCleanup': {
        if (owner._tag !== 'EffectValue')
          throw new RangeError('Effect cleanup lost its evaluator identity')
        for (const slot of cleanup.slots) {
          const capture = owner.captures.at(slot.ordinal)
          if (capture === undefined) continue
          const blocked = yield* releaseThroughPlan(slot.cleanup, capture, provenance, localOrdinal)
          if (blocked !== undefined) return blocked
        }
        return undefined
      }
      case 'HookCleanup': {
        const target = functionFor(program, cleanup.hook, cleanup.typeArguments)
        if (target === undefined) {
          return blockedStep({
            _tag: 'MissingFunction',
            target: cleanup.hook,
            span: provenance.span,
          })
        }
        const key = cellKey(frame, localOrdinal)
        state.cells.set(key, { value: owner, fromCall: false })
        trace.push(
          Object.freeze({
            _tag: 'Call',
            frame: activation.frame,
            depth: activation.depth,
            caller: fn.id,
            target: cleanup.hook,
            callerInstance: fn.instance,
            targetInstance: target.instance,
            span: provenance.span,
          }),
        )
        const reference: Value = Object.freeze({
          _tag: 'ReferenceValue' as const,
          frame,
          cell: localOrdinal,
          selectors: Object.freeze([]),
        })
        const result = yield* callFunction(target, [reference], provenance.span)
        if (result._tag === 'Blocked') return result
        if (result._tag === 'Transfer') return result
        const updated = state.cells.get(key)?.value ?? owner
        return yield* releaseThroughPlan(cleanup.inner, updated, provenance, localOrdinal)
      }
      case 'StructCleanup': {
        if (owner._tag !== 'AggregateValue') return undefined
        for (const field of cleanup.fields) {
          const entry = owner.fields.find(
            (candidate) => candidate.field.ordinal === field.field.ordinal,
          )
          if (entry === undefined) continue
          const blocked = yield* releaseThroughPlan(
            field.cleanup,
            entry.value,
            provenance,
            localOrdinal,
          )
          if (blocked !== undefined) return blocked
        }
        return undefined
      }
      case 'ArrayCleanup': {
        if (owner._tag !== 'ArrayValue') return undefined
        for (const element of owner.elements) {
          const blocked = yield* releaseThroughPlan(
            cleanup.element,
            element,
            provenance,
            localOrdinal,
          )
          if (blocked !== undefined) return blocked
        }
        return undefined
      }
      case 'UnionCleanup': {
        if (owner._tag !== 'UnionValue') return undefined
        const activeCase = cleanup.cases.find((candidate) =>
          Type.equals(candidate.member, owner.member),
        )
        return activeCase === undefined
          ? undefined
          : yield* releaseThroughPlan(activeCase.cleanup, owner.payload, provenance, localOrdinal)
      }
      case 'RepresentedCallableCleanup':
        // Lowering resolves this obligation against the shared realization before MIR exists, so
        // reaching it here would mean an aggregate is about to skip the owned captures of the
        // callable it stores. Fail loudly rather than release nothing.
        throw new RangeError('Stored callable cleanup reached evaluation unresolved')
    }
  }

  const regions = new Map(fn.regions.map((region) => [region.id.ordinal, region] as const))
  const loops = new Map(
    fn.regions.flatMap((region) =>
      region._tag === 'LoopRegion' ? [[region.loop.ordinal, region] as const] : [],
    ),
  )
  const conditionOwners = new Map(
    fn.regions.flatMap((region) =>
      region._tag === 'LoopRegion' ? [[region.condition.ordinal, region] as const] : [],
    ),
  )
  const checkedPlaces = new Map<ReadonlyArray<Mir.PlaceSelector>, ReadonlyArray<number>>()

  const invokeCallableTarget = function* (
    target: Hir.CallableTarget,
    typeArguments: ReadonlyArray<Type.GenericArgument>,
    arguments_: ReadonlyArray<Value>,
    span: SourceSpan.SourceSpan,
  ): FunctionExecution {
    if (target._tag === 'DeclarationCallableTarget') {
      const callee = functionFor(program, target.declaration, typeArguments)
      if (callee === undefined) {
        return blockedStep({ _tag: 'MissingFunction', target: target.declaration, span })
      }
      trace.push(
        Object.freeze({
          _tag: 'Call',
          frame: activation.frame,
          depth: activation.depth,
          caller: fn.id,
          target: target.declaration,
          callerInstance: fn.instance,
          targetInstance: callee.instance,
          span,
        }),
      )
      arguments_.forEach((argument, ordinal) => {
        trace.push(
          Object.freeze({
            _tag: 'Binding',
            frame: state.nextFrame,
            depth: activation.depth + 1,
            target: target.declaration,
            targetInstance: callee.instance,
            callSpan: span,
            argumentOrdinal: ordinal,
            parameterOrdinal: ordinal,
            value: argument,
            fromCall: true,
            span,
          }),
        )
      })
      return yield* callFunction(callee, arguments_, span)
    }

    const operation = target.operation
    const conversionTarget = Scalar.conversionTarget(operation)
    const actorScalar = Scalar.find(target.actor)
    if (actorScalar?.category === 'Floating') {
      const first = arguments_.at(0)
      if (operation === 'FromBits') {
        if (
          first === undefined ||
          (first._tag !== 'I32Value' &&
            first._tag !== 'UsizeValue' &&
            first._tag !== 'ScalarIntegerValue')
        )
          throw new RangeError('MIR verifier allowed invalid float bits')
        return Object.freeze({
          _tag: 'Value',
          value: floatValue(actorScalar.spelling, BigInt(first.value)),
        })
      }
      if (first?._tag !== 'FloatValue')
        throw new RangeError('MIR verifier allowed invalid float callable')
      if (operation === 'ToBits') {
        const targetType: Scalar.IntegerSpelling = actorScalar.spelling === 'f32' ? 'u32' : 'u64'
        return Object.freeze({ _tag: 'Value', value: scalarIntegerValue(targetType, first.bits) })
      }
      const floatTarget = Scalar.floatConversionTarget(operation)
      if (floatTarget !== undefined) {
        const converted = FloatingPoint.fromNumber(
          FloatingPoint.toNumber(floatingBits(first)),
          floatTarget.spelling === 'f32' ? 32 : 64,
        )
        return Object.freeze({
          _tag: 'Value',
          value: floatValue(floatTarget.spelling, converted.bits),
        })
      }
      if (conversionTarget !== undefined) {
        const number = FloatingPoint.toNumber(floatingBits(first))
        const exact = Number.isFinite(number) ? BigInt(Math.trunc(number)) : undefined
        const range = Scalar.range(
          conversionTarget,
          program.layout.target.pointerSize === 4 ? 32 : 64,
        )
        if (exact === undefined || exact < range.minimum || exact > range.maximum)
          return blockedStep({
            _tag: 'Trap',
            function: fn.id,
            reason: 'float conversion out of range',
            span,
          })
        return Object.freeze({
          _tag: 'Value',
          value: scalarIntegerValue(conversionTarget.spelling, exact),
        })
      }
      if (
        operation === 'Negate' ||
        operation === 'IsNaN' ||
        operation === 'IsInfinite' ||
        operation === 'IsFinite' ||
        operation === 'IsNormal' ||
        operation === 'IsSubnormal' ||
        operation === 'IsSignNegative'
      )
        return Object.freeze({ _tag: 'Value', value: floatingUnary(operation, first) })
      const second = arguments_.at(1)
      if (second?._tag === 'FloatValue' && Mir.isBinaryOperator(operation))
        return Object.freeze({ _tag: 'Value', value: floatingBinary(operation, first, second) })
    }
    const floatTarget = Scalar.floatConversionTarget(operation)
    if (actorScalar?.category === 'Integer' && floatTarget !== undefined) {
      const first = arguments_.at(0)
      if (
        first === undefined ||
        (first._tag !== 'I32Value' &&
          first._tag !== 'UsizeValue' &&
          first._tag !== 'ScalarIntegerValue')
      )
        throw new RangeError('MIR verifier allowed invalid integer-to-float callable')
      const encoded = FloatingPoint.fromNumber(
        Number(BigInt(first.value)),
        floatTarget.spelling === 'f32' ? 32 : 64,
      )
      return Object.freeze({ _tag: 'Value', value: floatValue(floatTarget.spelling, encoded.bits) })
    }
    if (Scalar.isCheckedOperation(operation)) {
      const source = Scalar.find(target.actor)
      const resultScalar = conversionTarget ?? source
      const leftValue = arguments_.at(0)
      const rightValue = arguments_.at(1)
      if (
        source?.category !== 'Integer' ||
        resultScalar?.category !== 'Integer' ||
        leftValue === undefined ||
        (leftValue._tag !== 'I32Value' &&
          leftValue._tag !== 'UsizeValue' &&
          leftValue._tag !== 'ScalarIntegerValue')
      )
        throw new RangeError('MIR verifier allowed an invalid checked callable')
      const left = BigInt(leftValue.value)
      const right =
        rightValue !== undefined &&
        (rightValue._tag === 'I32Value' ||
          rightValue._tag === 'UsizeValue' ||
          rightValue._tag === 'ScalarIntegerValue')
          ? BigInt(rightValue.value)
          : undefined
      const exact = operation.startsWith('CheckedConvertTo')
        ? left
        : operation === 'CheckedAdd' && right !== undefined
          ? left + right
          : operation === 'CheckedSubtract' && right !== undefined
            ? left - right
            : operation === 'CheckedMultiply' && right !== undefined
              ? left * right
              : operation === 'CheckedDivide' && right !== undefined && right !== 0n
                ? left / right
                : operation === 'CheckedRemainder' && right !== undefined && right !== 0n
                  ? left % right
                  : undefined
      const range = Scalar.range(resultScalar, program.layout.target.pointerSize === 4 ? 32 : 64)
      const succeeded = exact !== undefined && exact >= range.minimum && exact <= range.maximum
      const semantic = Type.option(resultScalar.spelling)
      if (!Type.isUnion(semantic))
        throw new RangeError('Canonical Option did not normalize to a structural union')
      const member = succeeded ? Type.some(resultScalar.spelling) : Type.none
      const entry = program.layout.entries.find((candidate) => Type.equals(candidate.type, member))
      if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
        throw new RangeError('Target plan omitted a canonical callable Option member')
      return Object.freeze({
        _tag: 'Value',
        value: Object.freeze({
          _tag: 'UnionValue',
          type: semantic,
          member,
          payload: Object.freeze({
            _tag: 'AggregateValue',
            type: member,
            fields: Object.freeze(
              succeeded
                ? entry.representation.fields.map((field) =>
                    Object.freeze({
                      field: field.id,
                      value: scalarIntegerValue(resultScalar.spelling, exact),
                    }),
                  )
                : [],
            ),
          }),
        }),
      })
    }
    if (conversionTarget !== undefined) {
      const subject = arguments_.at(0)
      if (
        subject === undefined ||
        (subject._tag !== 'I32Value' &&
          subject._tag !== 'UsizeValue' &&
          subject._tag !== 'ScalarIntegerValue')
      )
        throw new RangeError('MIR verifier allowed a non-integer conversion argument')
      const exact = BigInt(subject.value)
      const range = Scalar.range(
        conversionTarget,
        program.layout.target.pointerSize === 4 ? 32 : 64,
      )
      if (exact < range.minimum || exact > range.maximum)
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason: 'integer conversion out of range',
          span,
        })
      return Object.freeze({
        _tag: 'Value',
        value: scalarIntegerValue(conversionTarget.spelling, exact),
      })
    }
    if (
      operation === 'Not' ||
      operation === 'Negate' ||
      operation === 'BitNot' ||
      operation === 'WrappingNegate' ||
      operation === 'SaturatingNegate'
    ) {
      const subject = arguments_.at(0)
      if (
        subject === undefined ||
        (subject._tag !== 'I32Value' &&
          subject._tag !== 'UsizeValue' &&
          subject._tag !== 'ScalarIntegerValue')
      ) {
        throw new RangeError('MIR verifier allowed a non-scalar unary callable argument')
      }
      if (operation === 'Not')
        return Object.freeze({ _tag: 'Value', value: value(subject.value === 0 ? 1 : 0) })
      const scalar = Scalar.find(target.actor)
      if (scalar === undefined || scalar.category !== 'Integer') {
        throw new RangeError('MIR verifier allowed a non-integer negate callable')
      }
      const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
      const width = Scalar.bits(scalar, pointerBits)
      const raw = BigInt(subject.value)
      const exact =
        operation === 'BitNot'
          ? scalar.signedness === 'Signed'
            ? BigInt.asIntN(width, ~raw)
            : BigInt.asUintN(width, ~raw)
          : -raw
      const range = Scalar.range(scalar, pointerBits)
      if (operation === 'Negate' && (exact < range.minimum || exact > range.maximum)) {
        return blockedStep({ _tag: 'Trap', function: fn.id, reason: 'arithmetic overflow', span })
      }
      return Object.freeze({
        _tag: 'Value',
        value: scalarIntegerValue(
          scalar.spelling,
          operation === 'WrappingNegate'
            ? scalar.signedness === 'Signed'
              ? BigInt.asIntN(width, exact)
              : BigInt.asUintN(width, exact)
            : operation === 'SaturatingNegate'
              ? exact > range.maximum
                ? range.maximum
                : exact < range.minimum
                  ? range.minimum
                  : exact
              : exact,
        ),
      })
    }
    if (
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
    ) {
      const leftValue = arguments_.at(0)
      const rightValue = arguments_.at(1)
      if (
        leftValue === undefined ||
        rightValue === undefined ||
        (leftValue._tag !== 'I32Value' &&
          leftValue._tag !== 'UsizeValue' &&
          leftValue._tag !== 'ScalarIntegerValue') ||
        (rightValue._tag !== 'I32Value' &&
          rightValue._tag !== 'UsizeValue' &&
          rightValue._tag !== 'ScalarIntegerValue')
      ) {
        throw new RangeError('MIR verifier allowed invalid binary callable arguments')
      }
      const scalar = Scalar.find(target.actor)
      if (scalar === undefined || scalar.category !== 'Integer') {
        throw new RangeError('MIR verifier allowed a non-integer binary callable')
      }
      const left = BigInt(leftValue.value)
      const right = BigInt(rightValue.value)
      if (
        operation === 'Equals' ||
        operation === 'NotEquals' ||
        operation === 'LessThan' ||
        operation === 'LessOrEqual' ||
        operation === 'GreaterThan' ||
        operation === 'GreaterOrEqual'
      ) {
        const holds =
          operation === 'Equals'
            ? left === right
            : operation === 'NotEquals'
              ? left !== right
              : operation === 'LessThan'
                ? left < right
                : operation === 'LessOrEqual'
                  ? left <= right
                  : operation === 'GreaterThan'
                    ? left > right
                    : left >= right
        return Object.freeze({ _tag: 'Value', value: value(holds ? 1 : 0) })
      }
      if ((operation === 'Divide' || operation === 'Remainder') && right === 0n) {
        return blockedStep({ _tag: 'Trap', function: fn.id, reason: 'division by zero', span })
      }
      const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
      const width = Scalar.bits(scalar, pointerBits)
      if (
        (operation === 'ShiftLeft' || operation === 'ShiftRight') &&
        (right < 0n || right >= BigInt(width))
      ) {
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason: `invalid ${operation} count ${right}`,
          span,
        })
      }
      const fromBits = (input: bigint): bigint =>
        scalar.signedness === 'Signed' ? BigInt.asIntN(width, input) : BigInt.asUintN(width, input)
      const leftBits = BigInt.asUintN(width, left)
      const rightBits = BigInt.asUintN(width, right)
      const rotate = Number(right % BigInt(width))
      const rotatedLeft =
        rotate === 0
          ? leftBits
          : BigInt.asUintN(
              width,
              (leftBits << BigInt(rotate)) | (leftBits >> BigInt(width - rotate)),
            )
      const rotatedRight =
        rotate === 0
          ? leftBits
          : BigInt.asUintN(
              width,
              (leftBits >> BigInt(rotate)) | (leftBits << BigInt(width - rotate)),
            )
      const exact =
        operation === 'Add' || operation === 'WrappingAdd' || operation === 'SaturatingAdd'
          ? left + right
          : operation === 'Subtract' ||
              operation === 'WrappingSubtract' ||
              operation === 'SaturatingSubtract'
            ? left - right
            : operation === 'Multiply' ||
                operation === 'WrappingMultiply' ||
                operation === 'SaturatingMultiply'
              ? left * right
              : operation === 'Divide'
                ? left / right
                : operation === 'Remainder'
                  ? left % right
                  : operation === 'BitAnd'
                    ? fromBits(leftBits & rightBits)
                    : operation === 'BitOr'
                      ? fromBits(leftBits | rightBits)
                      : operation === 'BitXor'
                        ? fromBits(leftBits ^ rightBits)
                        : operation === 'ShiftLeft'
                          ? fromBits(leftBits << right)
                          : operation === 'ShiftRight'
                            ? scalar.signedness === 'Signed'
                              ? left >> right
                              : fromBits(leftBits >> right)
                            : operation === 'RotateLeft'
                              ? fromBits(rotatedLeft)
                              : fromBits(rotatedRight)
      const range = Scalar.range(scalar, pointerBits)
      const wrapping =
        operation === 'WrappingAdd' ||
        operation === 'WrappingSubtract' ||
        operation === 'WrappingMultiply'
      const saturating =
        operation === 'SaturatingAdd' ||
        operation === 'SaturatingSubtract' ||
        operation === 'SaturatingMultiply'
      if (!wrapping && !saturating && (exact < range.minimum || exact > range.maximum)) {
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason:
            scalar.signedness === 'Unsigned' && exact < 0n
              ? 'arithmetic underflow'
              : 'arithmetic overflow',
          span,
        })
      }
      const result = wrapping
        ? fromBits(exact)
        : saturating
          ? exact < range.minimum
            ? range.minimum
            : exact > range.maximum
              ? range.maximum
              : exact
          : exact
      return Object.freeze({
        _tag: 'Value',
        value: scalarIntegerValue(scalar.spelling, result),
      })
    }
    return blockedStep({
      _tag: 'Trap',
      function: fn.id,
      reason: `bootstrap callable ${target.actor}.${target.operation} is unavailable`,
      span,
    })
  }

  const cleanupMembers = (
    cleanup: Extract<Mir.Operation, { readonly _tag: 'Drop' }>['cleanup'],
    owner: Value,
  ): ReadonlyArray<Type.Nominal> => {
    if (cleanup._tag === 'NoCleanup' || cleanup._tag === 'ParameterCleanup') {
      return Object.freeze([])
    }
    if (cleanup._tag === 'AllocationCleanup') return Object.freeze([Type.allocation])
    if (cleanup._tag === 'UnionCleanup') {
      if (owner._tag !== 'UnionValue') return Object.freeze([])
      const active = cleanup.cases.find((candidate) => Type.equals(candidate.member, owner.member))
      return Object.freeze([
        owner.member,
        ...(active === undefined ? [] : cleanupMembers(active.cleanup, owner.payload)),
      ])
    }
    if (cleanup._tag === 'ArrayCleanup') {
      return owner._tag === 'ArrayValue'
        ? Object.freeze(
            owner.elements.flatMap((element) => cleanupMembers(cleanup.element, element)),
          )
        : Object.freeze([])
    }
    if (cleanup._tag === 'CallableCleanup') {
      if (owner._tag !== 'CallableValue') return Object.freeze([])
      return Object.freeze(
        cleanup.slots.flatMap((slot) => {
          const capture = owner.captures.find((candidate) => candidate.ordinal === slot.ordinal)
          return capture === undefined ? [] : cleanupMembers(slot.cleanup, capture.value)
        }),
      )
    }
    if (cleanup._tag === 'EffectCleanup') {
      if (owner._tag !== 'EffectValue') return Object.freeze([])
      return Object.freeze(
        cleanup.slots.flatMap((slot) => {
          const capture = owner.captures.at(slot.ordinal)
          return capture === undefined ? [] : cleanupMembers(slot.cleanup, capture)
        }),
      )
    }
    if (cleanup._tag === 'RawBufferCleanup') return Object.freeze([cleanup.type])
    if (cleanup._tag === 'HookCleanup') return cleanupMembers(cleanup.inner, owner)
    // Unresolved stored-executable obligations never reach evaluation: specialization resolves
    // them before lowering, while the executable fences retain every incomplete engine path.
    if (
      cleanup._tag === 'RepresentedCallableCleanup' ||
      cleanup._tag === 'RepresentedEffectCleanup'
    )
      return Object.freeze([])
    if (owner._tag !== 'AggregateValue') return Object.freeze([])
    return Object.freeze(
      cleanup.fields.flatMap((field) => {
        const value = owner.fields.find(
          (candidate) => candidate.field.ordinal === field.field.ordinal,
        )
        return value === undefined ? [] : cleanupMembers(field.cleanup, value.value)
      }),
    )
  }

  const selectFieldPath = (
    root: AggregateValue,
    path: ReadonlyArray<DeclarationIndex.FieldId>,
  ): Value => {
    let selected: Value = root
    for (const selector of path) {
      if (selected._tag !== 'AggregateValue') {
        throw new RangeError('MIR verifier allowed a match field below a non-struct value')
      }
      const field: AggregateValue['fields'][number] | undefined = selected.fields.find(
        (candidate) =>
          candidate.field.ordinal === selector.ordinal &&
          candidate.field.struct.sourceId === selector.struct.sourceId &&
          candidate.field.struct.ordinal === selector.struct.ordinal,
      )
      if (field === undefined) {
        throw new RangeError('MIR verifier allowed a missing match field')
      }
      selected = field.value
    }
    return selected
  }

  const resolvePlace = (
    root: Mir.LocalId,
    selectors: ReadonlyArray<Mir.PlaceSelector>,
  ):
    | {
        readonly _tag: 'Resolved'
        readonly selected: Value
        readonly indexes: ReadonlyArray<number>
      }
    | { readonly _tag: 'Blocked'; readonly step: Step } => {
    let selected = read(root).value
    let effectiveSelectors = selectors
    const indexes: Array<number> = []
    // A reference root reads through the borrow: the place lives on the referenced cell.
    if (selected._tag === 'ReferenceValue') {
      const target = state.cells.get(cellKey(selected.frame, selected.cell))
      if (target === undefined)
        throw new RangeError('MIR reference points at a missing evaluator cell')
      effectiveSelectors = Object.freeze([...selected.selectors, ...selectors])
      selected = target.value
    }
    for (const selector of effectiveSelectors) {
      if (selector._tag === 'FieldSelector') {
        if (selected._tag !== 'AggregateValue') {
          throw new RangeError('MIR verifier allowed a field selector on a non-struct value')
        }
        const field = selected.fields.find(
          (candidate) =>
            candidate.field.ordinal === selector.field.ordinal &&
            candidate.field.struct.sourceId === selector.field.struct.sourceId &&
            candidate.field.struct.ordinal === selector.field.struct.ordinal,
        )
        if (field === undefined)
          throw new RangeError('MIR verifier allowed a missing field selector')
        selected = field.value
        indexes.push(selector.field.ordinal)
        continue
      }
      if (selector._tag === 'SliceElementSelector') {
        if (selected._tag !== 'SliceValue' && selected._tag !== 'StaticViewValue') {
          throw new RangeError('MIR verifier allowed a slice selector on a non-slice value')
        }
        const exactIndex = readUsize(selector.index).value
        if (exactIndex >= BigInt(selected.length)) {
          return {
            _tag: 'Blocked',
            step: blockedStep({
              _tag: 'Trap',
              function: fn.id,
              reason: `slice index ${exactIndex} is outside length ${selected.length} in ${fn.id.module}.${fn.id.name}`,
              span: selector.provenance.span,
            }),
          }
        }
        const index = Number(exactIndex)
        if (selected._tag === 'StaticViewValue') {
          const byte = selected.bytes.at(index)
          if (byte === undefined) {
            throw new RangeError('MIR static view range exceeds its immutable bytes')
          }
          indexes.push(index)
          selected = scalarIntegerValue('u8', BigInt(byte))
          continue
        }
        if (selected.ticket !== undefined) {
          const allocation = state.allocations.get(selected.ticket)
          const element = allocation?.values.get(String(selected.base + index))
          if (allocation === undefined || !allocation.active || element === undefined) {
            throw new RangeError('MIR RawBuffer slice selected uninitialized storage')
          }
          indexes.push(index)
          selected = element
          continue
        }
        const backing = cell(selected).value
        if (backing._tag !== 'ArrayValue') {
          throw new RangeError('MIR slice cell does not contain an array value')
        }
        const element = backing.elements.at(selected.base + index)
        if (element === undefined) {
          throw new RangeError('MIR slice range exceeds its backing cell')
        }
        indexes.push(index)
        selected = element
        continue
      }
      if (selected._tag !== 'ArrayValue') {
        throw new RangeError('MIR verifier allowed an element selector on a non-array value')
      }
      const index =
        selector.index._tag === 'Proven'
          ? selector.index.value
          : Number(readUsize(selector.index.local).value)
      if (index < 0 || !Number.isSafeInteger(index) || index >= selector.length) {
        return {
          _tag: 'Blocked',
          step: blockedStep({
            _tag: 'Trap',
            function: fn.id,
            reason: `array index ${index} is outside length ${selector.length} in ${fn.id.module}.${fn.id.name}`,
            span: selector.provenance.span,
          }),
        }
      }
      const element = selected.elements.at(index)
      if (element === undefined)
        throw new RangeError('MIR verifier allowed an incomplete array value')
      indexes.push(index)
      selected = element
    }
    return { _tag: 'Resolved', selected, indexes: Object.freeze(indexes) }
  }

  const replacePlace = (
    current: Value,
    selectors: ReadonlyArray<Mir.PlaceSelector>,
    indexes: ReadonlyArray<number>,
    replacement: Value,
    depth = 0,
  ): Value => {
    const selector = selectors.at(depth)
    if (selector === undefined) return replacement
    const ordinal = indexes.at(depth)
    if (ordinal === undefined) throw new RangeError('Checked place omitted one selector index')
    if (selector._tag === 'FieldSelector') {
      if (current._tag !== 'AggregateValue') throw new RangeError('Invalid aggregate replacement')
      return Object.freeze({
        _tag: 'AggregateValue',
        type: current.type,
        fields: Object.freeze(
          current.fields.map((field) =>
            field.field.ordinal === selector.field.ordinal
              ? Object.freeze({
                  field: field.field,
                  value: replacePlace(field.value, selectors, indexes, replacement, depth + 1),
                })
              : field,
          ),
        ),
      })
    }
    if (selector._tag === 'SliceElementSelector') {
      if (current._tag !== 'SliceValue') throw new RangeError('Invalid slice replacement')
      if (current.ticket !== undefined) {
        const allocation = state.allocations.get(current.ticket)
        const absolute = current.base + ordinal
        const previous = allocation?.values.get(String(absolute))
        if (allocation === undefined || !allocation.active || previous === undefined) {
          throw new RangeError('Invalid RawBuffer slice replacement')
        }
        allocation.values.set(
          String(absolute),
          replacePlace(previous, selectors, indexes, replacement, depth + 1),
        )
        return current
      }
      const backing = cell(current)
      if (backing.value._tag !== 'ArrayValue') {
        throw new RangeError('Invalid slice backing cell replacement')
      }
      const absolute = current.base + ordinal
      const updated: ArrayValue = Object.freeze({
        _tag: 'ArrayValue',
        type: backing.value.type,
        elements: Object.freeze(
          backing.value.elements.map((element, index) =>
            index === absolute
              ? replacePlace(element, selectors, indexes, replacement, depth + 1)
              : element,
          ),
        ),
      })
      state.cells.set(cellKey(current.frame, current.cell), {
        value: updated,
        fromCall: backing.fromCall,
      })
      return current
    }
    if (current._tag !== 'ArrayValue') throw new RangeError('Invalid array replacement')
    return Object.freeze({
      _tag: 'ArrayValue',
      type: current.type,
      elements: Object.freeze(
        current.elements.map((element, index) =>
          index === ordinal
            ? replacePlace(element, selectors, indexes, replacement, depth + 1)
            : element,
        ),
      ),
    })
  }

  const replaceReferenced = (local: Mir.LocalId, replacement: Value): void => {
    const reference = read(local).value
    if (reference._tag !== 'ReferenceValue') {
      throw new RangeError('OS intrinsic output is not an exclusive reference')
    }
    const key = cellKey(reference.frame, reference.cell)
    const target = state.cells.get(key)
    if (target === undefined) throw new RangeError('OS intrinsic output references a missing cell')
    state.cells.set(key, {
      value: replacePlace(
        target.value,
        reference.selectors,
        Object.freeze(reference.selectors.map((selector) => selector.field.ordinal)),
        replacement,
      ),
      fromCall: target.fromCall,
    })
  }

  const bytesOfView = (viewed: SliceValue | StaticViewValue): ReadonlyArray<number> => {
    if (viewed._tag === 'StaticViewValue') return viewed.bytes
    if (viewed.ticket !== undefined) {
      const allocation = state.allocations.get(viewed.ticket)
      if (allocation === undefined || !allocation.active)
        throw new RangeError('OS intrinsic received released byte storage')
      return Object.freeze(
        Array.from({ length: viewed.length }, (_, index) => {
          const selected = allocation.values.get(String(viewed.base + index))
          if (selected?._tag !== 'ScalarIntegerValue' || selected.type !== 'u8')
            throw new RangeError('OS intrinsic received uninitialized byte storage')
          return Number(selected.value)
        }),
      )
    }
    const backing = cell(viewed).value
    if (backing._tag !== 'ArrayValue') throw new RangeError('OS byte slice lost its array')
    return Object.freeze(
      backing.elements.slice(viewed.base, viewed.base + viewed.length).map((selected) => {
        if (selected._tag !== 'ScalarIntegerValue' || selected.type !== 'u8')
          throw new RangeError('OS intrinsic received a non-byte slice')
        return Number(selected.value)
      }),
    )
  }

  const byteView = (local: Mir.LocalId): ReadonlyArray<number> => {
    const viewed = read(local).value
    if (viewed._tag !== 'SliceValue' && viewed._tag !== 'StaticViewValue')
      throw new RangeError('OS intrinsic expected a byte slice')
    return bytesOfView(viewed)
  }

  const stringView = (string: StringValue): SliceValue | StaticViewValue =>
    string.storage._tag === 'RuntimeSliceStorage'
      ? string.storage.view
      : Object.freeze({
          _tag: 'StaticViewValue',
          data: string.storage.data,
          bytes: string.storage.bytes,
          length: string.byteLength,
        })

  const stringBytes = (string: StringValue): ReadonlyArray<number> => {
    if (string.heldLoans.some((loan) => !state.activeLoans.has(loan))) {
      throw new RangeError('MIR string uses backing storage after its lexical loan ended')
    }
    const bytes = bytesOfView(stringView(string))
    if (bytes.length !== string.byteLength) {
      throw new RangeError('MIR string byte length disagrees with its backing storage')
    }
    return bytes
  }

  const writeByteView = (local: Mir.LocalId, bytes: ReadonlyArray<number>): void => {
    const viewed = read(local).value
    if (viewed._tag !== 'SliceValue' || bytes.length > viewed.length)
      throw new RangeError('OS intrinsic output exceeds its byte slice')
    if (viewed.ticket !== undefined) {
      const allocation = state.allocations.get(viewed.ticket)
      if (allocation === undefined || !allocation.active)
        throw new RangeError('OS intrinsic output uses released storage')
      for (const [index, byte] of bytes.entries()) {
        allocation.values.set(String(viewed.base + index), scalarIntegerValue('u8', BigInt(byte)))
      }
      return
    }
    const key = cellKey(viewed.frame, viewed.cell)
    const backing = state.cells.get(key)
    if (backing?.value._tag !== 'ArrayValue') throw new RangeError('OS output slice lost its array')
    const next: ArrayValue = Object.freeze({
      _tag: 'ArrayValue',
      type: backing.value.type,
      elements: Object.freeze(
        backing.value.elements.map((element, index) => {
          const byte = bytes.at(index - viewed.base)
          return byte === undefined ? element : scalarIntegerValue('u8', BigInt(byte))
        }),
      ),
    })
    state.cells.set(key, { value: next, fromCall: backing.fromCall })
  }

  const optionValue = (element: Type.Type, payload?: Value): UnionValue => {
    const semantic = Type.option(element)
    if (!Type.isUnion(semantic)) throw new RangeError('OS Option result did not normalize')
    const member = payload === undefined ? Type.none : Type.some(element)
    const entry = program.layout.entries.find((candidate) => Type.equals(candidate.type, member))
    if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
      throw new RangeError('Target plan omitted an OS Option member')
    return Object.freeze({
      _tag: 'UnionValue',
      type: semantic,
      member,
      payload: Object.freeze({
        _tag: 'AggregateValue',
        type: member,
        fields: Object.freeze(
          payload === undefined
            ? []
            : entry.representation.fields.map((field) =>
                Object.freeze({ field: field.id, value: payload }),
              ),
        ),
      }),
    })
  }

  const handleValue = (handle: OsFileSystemHost.Handle): AggregateValue => {
    const entry = program.layout.entries.find((candidate) =>
      Type.equals(candidate.type, Type.osHandle),
    )
    if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
      throw new RangeError('Target plan omitted OsHandle')
    return Object.freeze({
      _tag: 'AggregateValue',
      type: Type.osHandle,
      fields: Object.freeze(
        entry.representation.fields.map((field) =>
          Object.freeze({
            field: field.id,
            value:
              field.name === '$identity'
                ? Object.freeze({ _tag: 'UsizeValue' as const, value: BigInt(handle.identity) })
                : value(field.name === '$kind' ? (handle.kind === 'File' ? 0 : 1) : 1),
          }),
        ),
      ),
    })
  }

  const hostHandle = (local: Mir.LocalId): OsFileSystemHost.Handle => {
    const selected =
      read(local).value._tag === 'ReferenceValue' ? referenced(local).value : read(local).value
    if (selected._tag !== 'AggregateValue' || !Type.equals(selected.type, Type.osHandle))
      throw new RangeError('OS intrinsic expected OsHandle')
    const identity = selected.fields.at(0)?.value
    const kind = selected.fields.at(1)?.value
    const active = selected.fields.at(2)?.value
    if (
      identity?._tag !== 'UsizeValue' ||
      kind?._tag !== 'I32Value' ||
      active?._tag !== 'I32Value' ||
      active.value !== 1
    )
      throw new RangeError('OS intrinsic expected one live OsHandle')
    return Object.freeze({
      identity: Number(identity.value),
      kind: kind.value === 0 ? 'File' : 'Directory',
    })
  }

  let regionOrdinal = fn.entry.ordinal
  for (;;) {
    const region = regions.get(regionOrdinal)
    if (region === undefined) {
      return blockedStep({
        _tag: 'Trap',
        function: fn.id,
        reason: `missing region r${regionOrdinal}`,
        span: argumentSpanFallback(fn),
      })
    }

    const regionSpan =
      region._tag === 'ConditionalRegion' || region._tag === 'LoopRegion'
        ? region.provenance.span
        : region._tag === 'OperationRegion'
          ? (region.operations.at(0)?.provenance.span ?? region.outcome.provenance.span)
          : (region.releases.at(0)?.provenance.span ?? region.outcome.provenance.span)
    trace.push(
      Object.freeze({
        _tag: 'RegionEntry',
        function: fn.id,
        region: region.id.ordinal,
        ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
        span: regionSpan,
      }),
    )

    if (region._tag === 'ConditionalRegion') {
      const taken = readI32(region.condition).value !== 0
      trace.push(
        Object.freeze({
          _tag: 'Condition',
          function: fn.id,
          region: region.id.ordinal,
          ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
          span: region.provenance.span,
        }),
      )
      regionOrdinal = taken ? region.taken.ordinal : region.otherwise.ordinal
      continue
    }
    if (region._tag === 'LoopRegion') {
      regionOrdinal = region.condition.ordinal
      continue
    }

    const executeOperations = function* (
      operations: ReadonlyArray<Mir.Operation>,
    ): OperationsExecution {
      for (const operation of operations) {
        if (state.steps >= state.maxSteps) {
          return evaluationLimit('Steps', state.maxSteps, state.steps, operation.provenance.span)
        }
        state.steps += 1
        switch (operation._tag) {
          case 'ShortCircuit': {
            const decided = readI32(operation.left).value !== 0
            // `&&` decides on a false left operand, `||` on a true one. Only the undecided case
            // executes the nested right-operand operations at all.
            if (decided === (operation.operator === 'Or')) {
              write(operation.destination, {
                value: value(decided ? 1 : 0),
                fromCall: false,
              })
              break
            }
            const rightStep = yield* executeOperations(operation.right.operations)
            if (rightStep !== undefined) return rightStep
            write(operation.destination, read(operation.right.result))
            break
          }
          case 'Match': {
            const scrutinee = read(operation.scrutinee).value
            const activeMember =
              scrutinee._tag === 'UnionValue'
                ? scrutinee.member
                : scrutinee._tag === 'AggregateValue'
                  ? scrutinee.type
                  : undefined
            const payload =
              scrutinee._tag === 'UnionValue'
                ? scrutinee.payload
                : scrutinee._tag === 'AggregateValue'
                  ? scrutinee
                  : undefined
            if (activeMember === undefined || payload === undefined) {
              throw new RangeError('MIR verifier allowed matching a scalar value')
            }
            trace.push(
              Object.freeze({
                _tag: 'MatchDispatch',
                function: fn.id,
                match: operation.provenance.span.start,
                member: activeMember,
                access: operation.access,
                span: operation.provenance.span,
              }),
            )
            const decision = operation.decisions.find((candidate) =>
              Type.equals(candidate.member, activeMember),
            )
            if (decision === undefined) {
              throw new RangeError('MIR verifier allowed a match without its active member')
            }
            let selected = false
            for (const candidateId of decision.candidates) {
              const arm = operation.arms.find(
                (candidate) => candidate.id.ordinal === candidateId.ordinal,
              )
              if (arm === undefined) {
                throw new RangeError('MIR verifier allowed a missing match candidate')
              }
              trace.push(
                Object.freeze({
                  _tag: 'MatchCandidate',
                  function: fn.id,
                  match: operation.provenance.span.start,
                  arm: arm.id.ordinal,
                  member: activeMember,
                  access: operation.access,
                  span: arm.provenance.span,
                }),
              )
              for (const binding of arm.bindings) {
                const bound = selectFieldPath(payload, binding.path)
                write(binding.destination, { value: bound, fromCall: false })
                trace.push(
                  Object.freeze({
                    _tag: 'MatchCandidate',
                    function: fn.id,
                    match: operation.provenance.span.start,
                    arm: arm.id.ordinal,
                    member: activeMember,
                    access: operation.access,
                    binding: binding.id.ordinal,
                    path: binding.path,
                    value: bound,
                    span: binding.provenance.span,
                  }),
                )
              }
              if (arm.guard !== undefined) {
                const guardStep = yield* executeOperations(arm.guard.operations)
                if (guardStep !== undefined) return guardStep
                if (readI32(arm.guard.result).value === 0) continue
              }
              trace.push(
                Object.freeze({
                  _tag: 'MatchSelected',
                  function: fn.id,
                  match: operation.provenance.span.start,
                  arm: arm.id.ordinal,
                  member: activeMember,
                  access: operation.access,
                  span: arm.provenance.span,
                }),
              )
              const selectedStep = yield* executeOperations(arm.selected.operations)
              if (selectedStep !== undefined) return selectedStep
              for (const cleanup of arm.selected.cleanup) {
                const owner = selectFieldPath(payload, cleanup.path)
                const members = cleanupMembers(cleanup.cleanup, owner)
                trace.push(
                  Object.freeze({
                    _tag: 'MatchCleanup',
                    function: fn.id,
                    match: operation.provenance.span.start,
                    arm: arm.id.ordinal,
                    member: activeMember,
                    access: operation.access,
                    path: cleanup.path,
                    ...(members.length === 0 ? {} : { members }),
                    span: arm.provenance.span,
                  }),
                )
              }
              const result = read(arm.selected.result)
              write(operation.destination, result)
              if (arm.selected.endBorrow) {
                trace.push(
                  Object.freeze({
                    _tag: 'MatchBorrowEnd',
                    function: fn.id,
                    match: operation.provenance.span.start,
                    arm: arm.id.ordinal,
                    member: activeMember,
                    access: operation.access,
                    span: arm.provenance.span,
                  }),
                )
              }
              selected = true
              break
            }
            if (!selected) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: `exhaustive match rejected every guard in ${fn.id.module}.${fn.id.name}`,
                span: operation.provenance.span,
              })
            }
            break
          }
          case 'Literal':
            {
              const semantic = Mir.semanticType(operation.type)
              const integer = typeof semantic === 'string' && Scalar.isIntegerSpelling(semantic)
              const floating = typeof semantic === 'string' && Scalar.isFloatSpelling(semantic)
              const character = typeof semantic === 'string' && Scalar.isCharacterSpelling(semantic)
              write(operation.destination, {
                value: floating
                  ? floatValue(semantic, BigInt(operation.value))
                  : integer
                    ? scalarIntegerValue(semantic, BigInt(operation.value))
                    : character
                      ? characterValue(Number(operation.value))
                      : value(Number(operation.value)),
                fromCall: false,
              })
            }
            break
          case 'StaticView': {
            const data = program.staticData?.find((candidate) => candidate.id === operation.data)
            if (data === undefined || data.bytes.length !== operation.length) {
              throw new RangeError('MIR verifier allowed a missing or mismatched static-data view')
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'StaticViewValue',
                data: data.id,
                bytes: data.bytes,
                length: data.bytes.length,
              }),
              fromCall: false,
            })
            break
          }
          case 'StaticString': {
            const data = program.staticData?.find((candidate) => candidate.id === operation.data)
            if (data?.kind !== 'Text' || !data.utf8 || data.bytes.length !== operation.byteLength) {
              throw new RangeError('MIR verifier allowed a missing or mismatched static string')
            }
            const string: StringValue = Object.freeze({
              _tag: 'StringValue',
              storage: Object.freeze({
                _tag: 'StaticTextStorage',
                data: data.id,
                bytes: data.bytes,
              }),
              bytes: data.bytes,
              byteLength: data.bytes.length,
              heldLoans: Object.freeze([]),
            })
            write(operation.destination, { value: string, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'StringStatic',
                function: fn.id,
                storage: string.storage._tag,
                byteLength: string.byteLength,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'StringFromUtf8Unchecked': {
            const bytes = read(operation.bytes).value
            if (bytes._tag !== 'SliceValue' && bytes._tag !== 'StaticViewValue') {
              throw new RangeError('MIR verifier allowed string formation from a non-byte view')
            }
            const heldLoans = Object.freeze(operation.heldLoans.map(borrowKey))
            if (heldLoans.some((loan) => !state.activeLoans.has(loan))) {
              throw new RangeError('MIR string formation lost its active backing loan')
            }
            const storage: StringStorage =
              bytes._tag === 'StaticViewValue'
                ? Object.freeze({
                    _tag: 'StaticByteStorage',
                    data: bytes.data,
                    bytes: bytes.bytes,
                  })
                : Object.freeze({ _tag: 'RuntimeSliceStorage', view: bytes })
            for (const loan of heldLoans) state.stringLoans.add(loan)
            const string: StringValue = Object.freeze({
              _tag: 'StringValue',
              storage,
              bytes: bytesOfView(bytes),
              byteLength: bytes.length,
              heldLoans,
            })
            write(operation.destination, { value: string, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'StringRuntime',
                function: fn.id,
                storage: string.storage._tag,
                byteLength: string.byteLength,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'StringUtf8Bytes': {
            const string = read(operation.string).value
            if (string._tag !== 'StringValue')
              throw new RangeError('MIR verifier allowed byte viewing of a non-string')
            stringBytes(string)
            write(operation.destination, { value: stringView(string), fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'StringBytes',
                function: fn.id,
                storage: string.storage._tag,
                byteLength: string.byteLength,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'StringByteLength': {
            const string = read(operation.string).value
            if (string._tag !== 'StringValue')
              throw new RangeError('MIR verifier allowed byte length of a non-string')
            stringBytes(string)
            write(operation.destination, {
              value: usizeValue(BigInt(string.byteLength)),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'StringByteLength',
                function: fn.id,
                storage: string.storage._tag,
                byteLength: string.byteLength,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'StringEqualsExact': {
            const left = read(operation.left).value
            const right = read(operation.right).value
            if (left._tag !== 'StringValue' || right._tag !== 'StringValue')
              throw new RangeError('MIR verifier allowed exact equality of non-string values')
            const leftBytes = stringBytes(left)
            const rightBytes = stringBytes(right)
            const equal =
              leftBytes.length === rightBytes.length &&
              leftBytes.every((byte, index) => byte === rightBytes.at(index))
            const result = operation.negated ? !equal : equal
            write(operation.destination, { value: value(result ? 1 : 0), fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'StringEqualsExact',
                function: fn.id,
                result,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'Move':
            write(operation.destination, read(operation.source))
            break
          case 'BeginLoan': {
            state.activeLoans.add(borrowKey(operation.borrow))
            const source = read(operation.root)
            if (operation.type._tag === 'Reference') {
              const inherited =
                source.value._tag === 'ReferenceValue'
                  ? source.value
                  : Object.freeze({
                      _tag: 'ReferenceValue' as const,
                      frame,
                      cell: operation.root.ordinal,
                      selectors: Object.freeze([]),
                    })
              const key = cellKey(inherited.frame, inherited.cell)
              if (!state.cells.has(key)) state.cells.set(key, source)
              write(operation.destination, {
                value: Object.freeze({
                  _tag: 'ReferenceValue' as const,
                  frame: inherited.frame,
                  cell: inherited.cell,
                  selectors: Object.freeze([...inherited.selectors, ...operation.selectors]),
                }),
                fromCall: source.fromCall,
              })
              break
            }
            if (operation.sourceType._tag === 'Nominal') {
              throw new RangeError('MIR verifier allowed a nominal slice root')
            }
            const slice =
              operation.sourceType._tag === 'Slice'
                ? source.value
                : (() => {
                    if (operation.sourceType._tag !== 'FixedArray') {
                      throw new RangeError('MIR verifier allowed a non-array slice root')
                    }
                    if (source.value._tag !== 'ArrayValue') {
                      throw new RangeError('MIR verifier allowed borrowing a non-array value')
                    }
                    const key = cellKey(frame, operation.root.ordinal)
                    if (!state.cells.has(key)) state.cells.set(key, source)
                    return Object.freeze({
                      _tag: 'SliceValue' as const,
                      frame,
                      cell: operation.root.ordinal,
                      base: 0,
                      length: operation.sourceType.type.length,
                    })
                  })()
            if (slice._tag !== 'SliceValue') {
              throw new RangeError('MIR verifier allowed reborrowing a non-slice value')
            }
            write(operation.destination, { value: slice, fromCall: source.fromCall })
            break
          }
          case 'EndLoan': {
            const loan = borrowKey(operation.borrow)
            state.activeLoans.delete(loan)
            if (state.stringLoans.delete(loan)) {
              trace.push(
                Object.freeze({
                  _tag: 'StringLoanEnd',
                  function: fn.id,
                  loan,
                  span: operation.provenance.span,
                }),
              )
            }
            break
          }
          case 'SliceLength': {
            const slice = read(operation.slice).value
            if (slice._tag !== 'SliceValue' && slice._tag !== 'StaticViewValue') {
              throw new RangeError('MIR verifier allowed slice length on a non-slice value')
            }
            write(operation.destination, {
              value: usizeValue(BigInt(slice.length)),
              fromCall: false,
            })
            break
          }
          case 'ConvertUnion': {
            const source = read(operation.source).value
            const mapping =
              operation.conversion === 'Inject'
                ? operation.mappings.at(0)
                : source._tag === 'UnionValue'
                  ? operation.mappings.find((candidate) =>
                      Type.equals(candidate.source, source.member),
                    )
                  : undefined
            const payload =
              operation.conversion === 'Inject' && source._tag === 'AggregateValue'
                ? source
                : operation.conversion === 'Widen' && source._tag === 'UnionValue'
                  ? source.payload
                  : undefined
            if (mapping === undefined || payload === undefined) {
              throw new RangeError('MIR verifier allowed an invalid logical union conversion')
            }
            const converted: UnionValue = Object.freeze({
              _tag: 'UnionValue',
              type: operation.targetType.type,
              member: mapping.target,
              payload,
            })
            write(operation.destination, { value: converted, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'UnionConversion',
                function: fn.id,
                conversion: operation.conversion,
                source: operation.sourceType.type,
                target: operation.targetType.type,
                member: converted.member,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ValidateLayout': {
            const bytes = readInteger(operation.bytes)
            const alignment = readInteger(operation.alignment)
            if (bytes._tag !== 'UsizeValue' || alignment._tag !== 'UsizeValue') {
              throw new RangeError('MIR verifier allowed non-usize layout construction')
            }
            const valid = alignment.value > 0n && (alignment.value & (alignment.value - 1n)) === 0n
            const member = valid ? Type.layout : Type.invalidAlignment
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, member),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate') {
              throw new RangeError('Target plan omitted an intrinsic layout validation member')
            }
            const payload: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: member,
              fields: Object.freeze(
                entry.representation.fields.map((field) =>
                  Object.freeze({
                    field: field.id,
                    value: field.name === 'bytes' ? bytes : alignment,
                  }),
                ),
              ),
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'UnionValue',
                type: operation.type.type,
                member,
                payload,
              }),
              fromCall: false,
            })
            break
          }
          case 'RepeatLayout': {
            const layout = read(operation.layout).value
            const count = readInteger(operation.count)
            if (layout._tag !== 'AggregateValue' || count._tag !== 'UsizeValue') {
              throw new RangeError('MIR verifier allowed invalid repeated-layout operands')
            }
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, Type.layout),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate') {
              throw new RangeError('Target plan omitted Layout')
            }
            const representation = entry.representation
            const fieldValue = (name: string): UsizeValue | undefined => {
              const field = representation.fields.find((candidate) => candidate.name === name)
              const value = layout.fields.find(
                (candidate) => candidate.field.ordinal === field?.id.ordinal,
              )?.value
              return value?._tag === 'UsizeValue' ? value : undefined
            }
            const bytes = fieldValue('bytes')
            const alignment = fieldValue('alignment')
            if (bytes === undefined || alignment === undefined) {
              throw new RangeError('Layout payload omitted bytes or alignment')
            }
            const maximum =
              program.layout.target.pointerSize === 4 ? 4294967295n : 18446744073709551615n
            const stride =
              alignment.value === 0n
                ? 0n
                : ((bytes.value + alignment.value - 1n) / alignment.value) * alignment.value
            const overflow = count.value !== 0n && stride > maximum / count.value
            const member = overflow ? Type.layoutOverflow : Type.layout
            const memberEntry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, member),
            )
            if (
              memberEntry?._tag !== 'LayoutEntry' ||
              memberEntry.representation._tag !== 'Aggregate'
            ) {
              throw new RangeError('Target plan omitted a repeated-layout result member')
            }
            const total = usizeValue(overflow ? 0n : stride * count.value)
            const payload: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: member,
              fields: Object.freeze(
                memberEntry.representation.fields.map((field) =>
                  Object.freeze({
                    field: field.id,
                    value: field.name === 'bytes' ? total : alignment,
                  }),
                ),
              ),
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'UnionValue',
                type: operation.type.type,
                member,
                payload,
              }),
              fromCall: false,
            })
            break
          }
          case 'Allocate': {
            const layout = read(operation.layout).value
            if (layout._tag !== 'AggregateValue' || !Type.equals(layout.type, Type.layout)) {
              throw new RangeError('MIR verifier allowed allocation with a non-Layout value')
            }
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, Type.layout),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate') {
              throw new RangeError('Target plan omitted Layout')
            }
            const representation = entry.representation
            const fieldValue = (name: string): UsizeValue | undefined => {
              const field = representation.fields.find((candidate) => candidate.name === name)
              const found = layout.fields.find(
                (candidate) => candidate.field.ordinal === field?.id.ordinal,
              )?.value
              return found?._tag === 'UsizeValue' ? found : undefined
            }
            const bytes = fieldValue('bytes')
            const alignment = fieldValue('alignment')
            if (bytes === undefined || alignment === undefined)
              throw new RangeError('Layout payload omitted bytes or alignment')
            const ticket = state.nextAllocation
            state.nextAllocation += 1
            state.allocations.set(ticket, { active: true, values: new Map() })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AllocationValue',
                type: Type.allocation,
                ticket,
                bytes: bytes.value,
                alignment: alignment.value,
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'AllocationAcquire',
                function: fn.id,
                ticket,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'HostWrite': {
            const stream = readI32(operation.stream)
            const viewed = read(operation.bytes).value
            const bytes = (() => {
              if (viewed._tag === 'StaticViewValue') return viewed.bytes
              if (viewed._tag !== 'SliceValue') return undefined
              const root = cell(viewed).value
              if (root._tag !== 'ArrayValue') return undefined
              const selected = root.elements.slice(viewed.base, viewed.base + viewed.length)
              if (
                selected.some(
                  (element) => element._tag !== 'ScalarIntegerValue' || element.type !== 'u8',
                )
              )
                return undefined
              return Object.freeze(
                selected.flatMap((element) =>
                  element._tag === 'ScalarIntegerValue' ? [Number(element.value)] : [],
                ),
              )
            })()
            if (bytes === undefined) {
              throw new RangeError('MIR verifier allowed a non-byte slice standard-stream write')
            }
            const destination: StandardStreams.Destination =
              stream.value === 0 ? 'Stdout' : 'Stderr'
            const result = (() => {
              if (state.standardStreams === undefined) return undefined
              try {
                return state.standardStreams.writeAll(destination, bytes)
              } catch {
                return Object.freeze({
                  _tag: 'WriteFailure' as const,
                  message: 'standard stream provider threw',
                })
              }
            })()
            if (result === undefined) return blockedStep({ _tag: 'MissingStandardStreams' })
            trace.push(
              Object.freeze({
                _tag: 'HostWrite',
                function: fn.id,
                destination,
                bytes: Object.freeze(Array.from(bytes)),
                outcome: result._tag,
                span: operation.provenance.span,
              }),
            )
            if (result._tag === 'WriteFailure') {
              return {
                _tag: 'Value',
                value: Object.freeze({
                  _tag: 'EffectOutcomeValue',
                  type: operation.propagationType.type,
                  tag: operation.failureTag,
                  payload: Object.freeze({
                    _tag: 'AggregateValue',
                    type: operation.failure,
                    fields: Object.freeze([]),
                  }),
                }),
              }
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            break
          }
          case 'OsCall': {
            const arguments_ = operation.arguments
            const reasonOutput = arguments_.at(-2)
            const codeOutput = arguments_.at(-1)
            if (reasonOutput === undefined || codeOutput === undefined)
              throw new RangeError('OS intrinsic omitted status outputs')
            const status = (failure?: OsFileSystemHost.Failure): void => {
              replaceReferenced(
                reasonOutput,
                value(failure === undefined ? 0 : OsFileSystemHost.reasonCode(failure.reason)),
              )
              replaceReferenced(
                codeOutput,
                scalarIntegerValue('u32', BigInt(failure?.nativeCode ?? 0)),
              )
            }
            const commit = (result: Value): void =>
              write(operation.destination, { value: result, fromCall: false })
            const name = operation.operation.name
            if (name === 'osStandardInputRead') {
              const input = state.standardInput
              if (input === undefined) return blockedStep({ _tag: 'MissingStandardInput' })
              const output = arguments_.at(0)
              if (output === undefined) throw new RangeError('OS read omitted its output buffer')
              const capacity = byteView(output).length
              const result = input.read(capacity)
              if (result._tag === 'ReadFailure') {
                status({ _tag: 'Failure', reason: 'Other' })
                commit(optionValue('usize'))
                break
              }
              if (result.bytes.length > capacity)
                throw new RangeError('standard-input provider overran the caller buffer')
              writeByteView(output, result.bytes)
              status()
              commit(
                optionValue(
                  'usize',
                  Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.bytes.length) }),
                ),
              )
              break
            }
            if (name === 'osProcessExecute') {
              const child = state.childProcess
              if (child === undefined) return blockedStep({ _tag: 'MissingChildProcess' })
              const program = arguments_.at(0)
              const argumentBlock = arguments_.at(1)
              const environmentBlock = arguments_.at(2)
              const workingDirectory = arguments_.at(3)
              const processStatus = arguments_.at(4)
              const processCode = arguments_.at(5)
              const outputLength = arguments_.at(6)
              const errorLength = arguments_.at(7)
              if (
                program === undefined ||
                argumentBlock === undefined ||
                environmentBlock === undefined ||
                workingDirectory === undefined ||
                processStatus === undefined ||
                processCode === undefined ||
                outputLength === undefined ||
                errorLength === undefined
              )
                throw new RangeError('OS execute omitted arguments')
              const directory = byteView(workingDirectory)
              const entries = (
                block: ReadonlyArray<number>,
              ): ReadonlyArray<ReadonlyArray<number>> | null => {
                if (block.length === 0) return Object.freeze([])
                if (block.at(-1) !== 0) return null
                const collected: Array<ReadonlyArray<number>> = []
                let start = 0
                for (const [index, byte] of block.entries()) {
                  if (byte !== 0) continue
                  collected.push(Object.freeze(block.slice(start, index)))
                  start = index + 1
                }
                return Object.freeze(collected)
              }
              const requestArguments = entries(byteView(argumentBlock))
              const requestEnvironment = entries(byteView(environmentBlock))
              const programBytes = byteView(program)
              // The block protocol is the intrinsic's precondition, so a malformed request is a
              // typed start failure rather than an execution the host never saw.
              if (
                requestArguments === null ||
                requestEnvironment === null ||
                programBytes.length === 0 ||
                programBytes.includes(0) ||
                directory.includes(0)
              ) {
                state.processCaptures[0] = Object.freeze([])
                state.processCaptures[1] = Object.freeze([])
                status({ _tag: 'Failure', reason: 'InvalidPath' })
                commit(value(0))
                break
              }
              const result = child.execute(
                Object.freeze({
                  program: programBytes,
                  arguments: requestArguments,
                  environment: requestEnvironment,
                  ...(directory.length === 0 ? {} : { workingDirectory: directory }),
                }),
              )
              if (result._tag === 'ExecuteFailure') {
                state.processCaptures[0] = Object.freeze([])
                state.processCaptures[1] = Object.freeze([])
                status({
                  _tag: 'Failure',
                  reason: result.reason,
                  ...(result.nativeCode === undefined ? {} : { nativeCode: result.nativeCode }),
                })
                commit(value(0))
                break
              }
              state.processCaptures[0] = result.output
              state.processCaptures[1] = result.errors
              replaceReferenced(processStatus, value(result._tag === 'Exited' ? 0 : 1))
              replaceReferenced(
                processCode,
                value(result._tag === 'Exited' ? result.code : result.signal),
              )
              replaceReferenced(
                outputLength,
                Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.output.length) }),
              )
              replaceReferenced(
                errorLength,
                Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.errors.length) }),
              )
              status()
              commit(value(1))
              break
            }
            if (name === 'osProcessCapture') {
              const stream = arguments_.at(0)
              const offset = arguments_.at(1)
              const output = arguments_.at(2)
              if (stream === undefined || offset === undefined || output === undefined)
                throw new RangeError('OS capture omitted arguments')
              const selector = readI32(stream).value
              const captured = state.processCaptures.at(selector)
              const start = Number(readUsize(offset).value)
              if (selector !== 0 && selector !== 1) {
                status({ _tag: 'Failure', reason: 'WrongType' })
                commit(optionValue('usize'))
                break
              }
              if (captured === undefined || start > captured.length) {
                status({ _tag: 'Failure', reason: 'InvalidPath' })
                commit(optionValue('usize'))
                break
              }
              const transferred = captured.slice(start, start + byteView(output).length)
              writeByteView(output, transferred)
              status()
              commit(
                optionValue(
                  'usize',
                  Object.freeze({ _tag: 'UsizeValue', value: BigInt(transferred.length) }),
                ),
              )
              break
            }
            if (name.startsWith('osHost')) {
              const input = state.hostInput
              if (input === undefined) return blockedStep({ _tag: 'MissingHostInput' })
              if (name === 'osHostArgumentCount') {
                const count = arguments_.at(0)
                if (count === undefined) throw new RangeError('OS count omitted its output')
                const result = input.argumentCount()
                if (result._tag === 'LookupFailure') {
                  status({ _tag: 'Failure', reason: 'Other' })
                  commit(value(0))
                  break
                }
                replaceReferenced(
                  count,
                  Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.count) }),
                )
                status()
                commit(value(1))
                break
              }
              const output = arguments_.at(name === 'osHostWorkingDirectory' ? 0 : 1)
              if (output === undefined) throw new RangeError('OS lookup omitted its output buffer')
              const selector = arguments_.at(0)
              if (selector === undefined) throw new RangeError('OS lookup omitted its subject')
              const result =
                name === 'osHostArgument'
                  ? input.argument(Number(readUsize(selector).value))
                  : name === 'osHostVariable'
                    ? input.variable(byteView(selector))
                    : input.workingDirectory()
              if (result._tag !== 'Present') {
                // Absence is the not-found reason, which the provider reads as an ordinary answer;
                // any other reason is a host that could not answer at all.
                status({
                  _tag: 'Failure',
                  reason: result._tag === 'Absent' ? 'NotFound' : 'Other',
                })
                commit(optionValue('usize'))
                break
              }
              // The complete byte length is the result even when only a prefix fit, so the caller
              // can size an exact buffer and ask again.
              const capacity = byteView(output).length
              writeByteView(output, result.bytes.slice(0, capacity))
              status()
              commit(
                optionValue(
                  'usize',
                  Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.bytes.length) }),
                ),
              )
              break
            }
            const host = state.osFileSystem
            if (host === undefined) return blockedStep({ _tag: 'MissingOsFileSystemHost' })
            try {
              if (name === 'osFileOpen' || name === 'osDirectoryOpen') {
                const root = arguments_.at(0)
                const path = arguments_.at(1)
                if (root === undefined || path === undefined)
                  throw new RangeError('OS open omitted paths')
                const result =
                  name === 'osFileOpen'
                    ? host.fileOpen(
                        byteView(root),
                        byteView(path),
                        readI32(arguments_.at(2) ?? root).value,
                      )
                    : host.directoryOpen(byteView(root), byteView(path))
                if (result._tag === 'Failure') {
                  status(result)
                  commit(optionValue(Type.osHandle))
                } else {
                  status()
                  commit(optionValue(Type.osHandle, handleValue(result.handle)))
                }
                break
              }
              if (name === 'osFileRead') {
                const handle = arguments_.at(0)
                const output = arguments_.at(1)
                if (handle === undefined || output === undefined)
                  throw new RangeError('OS read omitted arguments')
                const capacity = byteView(output).length
                const result = host.fileRead(hostHandle(handle), capacity)
                if (result._tag === 'Failure') {
                  status(result)
                  commit(optionValue('usize'))
                } else {
                  writeByteView(output, result.bytes)
                  status()
                  commit(
                    optionValue(
                      'usize',
                      Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.bytes.length) }),
                    ),
                  )
                }
                break
              }
              if (name === 'osFileWrite') {
                const handle = arguments_.at(0)
                const input = arguments_.at(1)
                const offset = arguments_.at(2)
                if (handle === undefined || input === undefined || offset === undefined)
                  throw new RangeError('OS write omitted arguments')
                const result = host.fileWrite(
                  hostHandle(handle),
                  byteView(input).slice(Number(readUsize(offset).value)),
                )
                if (result._tag === 'Failure') {
                  status(result)
                  commit(optionValue('usize'))
                } else {
                  status()
                  commit(
                    optionValue(
                      'usize',
                      Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.count) }),
                    ),
                  )
                }
                break
              }
              if (name === 'osDirectoryNext') {
                const handle = arguments_.at(0)
                const output = arguments_.at(1)
                const kind = arguments_.at(2)
                const required = arguments_.at(3)
                if (
                  handle === undefined ||
                  output === undefined ||
                  kind === undefined ||
                  required === undefined
                )
                  throw new RangeError('OS directory next omitted arguments')
                const result = host.directoryNext(hostHandle(handle), byteView(output).length)
                if (result._tag === 'Failure' || result._tag === 'BufferTooSmall') {
                  const failure: OsFileSystemHost.Failure =
                    result._tag === 'Failure'
                      ? result
                      : { _tag: 'Failure', reason: 'BufferTooSmall' }
                  status(failure)
                  if (result._tag === 'BufferTooSmall')
                    replaceReferenced(
                      required,
                      Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.requiredCapacity) }),
                    )
                  commit(optionValue('usize'))
                } else if (result._tag === 'End') {
                  status()
                  commit(optionValue('usize', Object.freeze({ _tag: 'UsizeValue', value: 0n })))
                } else {
                  writeByteView(output, result.name)
                  replaceReferenced(kind, value(result.kind === 'File' ? 0 : 1))
                  status()
                  commit(
                    optionValue(
                      'usize',
                      Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.name.length) }),
                    ),
                  )
                }
                break
              }
              if (name === 'osDirectoryCreateUnique') {
                const root = arguments_.at(0)
                const parent = arguments_.at(1)
                const prefix = arguments_.at(2)
                const output = arguments_.at(3)
                const required = arguments_.at(4)
                if (
                  root === undefined ||
                  parent === undefined ||
                  prefix === undefined ||
                  output === undefined ||
                  required === undefined
                )
                  throw new RangeError('OS unique directory create omitted arguments')
                const result = host.directoryCreateUnique(
                  byteView(root),
                  byteView(parent),
                  byteView(prefix),
                  byteView(output).length,
                )
                if (result._tag === 'Failure' || result._tag === 'BufferTooSmall') {
                  status(
                    result._tag === 'Failure'
                      ? result
                      : { _tag: 'Failure', reason: 'BufferTooSmall' },
                  )
                  if (result._tag === 'BufferTooSmall')
                    replaceReferenced(
                      required,
                      Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.requiredCapacity) }),
                    )
                  commit(optionValue('usize'))
                } else {
                  writeByteView(output, result.name)
                  status()
                  commit(
                    optionValue(
                      'usize',
                      Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.name.length) }),
                    ),
                  )
                }
                break
              }
              if (name === 'osPathInspect') {
                const root = arguments_.at(0)
                const path = arguments_.at(1)
                const kind = arguments_.at(2)
                const length = arguments_.at(3)
                if (
                  root === undefined ||
                  path === undefined ||
                  kind === undefined ||
                  length === undefined
                )
                  throw new RangeError('OS inspect omitted arguments')
                const result = host.pathInspect(byteView(root), byteView(path))
                if (result._tag === 'Failure') {
                  status(result)
                  commit(value(0))
                } else {
                  replaceReferenced(kind, value(result.kind === 'File' ? 0 : 1))
                  replaceReferenced(
                    length,
                    Object.freeze({ _tag: 'UsizeValue', value: BigInt(result.byteLength) }),
                  )
                  status()
                  commit(value(1))
                }
                break
              }
              const command =
                name === 'osDirectoryCreate' ||
                name === 'osFileRemove' ||
                name === 'osDirectoryRemove'
                  ? (() => {
                      const root = arguments_.at(0)
                      const path = arguments_.at(1)
                      if (root === undefined || path === undefined)
                        throw new RangeError('OS command omitted paths')
                      return name === 'osDirectoryCreate'
                        ? host.directoryCreate(byteView(root), byteView(path))
                        : name === 'osFileRemove'
                          ? host.fileRemove(byteView(root), byteView(path))
                          : host.directoryRemove(byteView(root), byteView(path))
                    })()
                  : name === 'osHandleClose'
                    ? host.handleClose(hostHandle(arguments_.at(0) ?? reasonOutput))
                    : undefined
              if (command === undefined) throw new RangeError(`Unknown OS intrinsic ${name}`)
              if (command._tag === 'Failure') {
                status(command)
                commit(value(0))
              } else {
                status()
                commit(value(1))
              }
            } catch {
              const failure: OsFileSystemHost.Failure = { _tag: 'Failure', reason: 'Other' }
              status(failure)
              commit(
                operation.type._tag === 'Union'
                  ? optionValue(
                      operation.type.type.members.some((member) =>
                        Type.equals(member, Type.some(Type.osHandle)),
                      )
                        ? Type.osHandle
                        : 'usize',
                    )
                  : value(0),
              )
            }
            break
          }
          case 'RawBufferFrom': {
            const allocation = read(operation.allocation).value
            const count = readInteger(operation.count)
            if (allocation._tag !== 'AllocationValue' || count._tag !== 'UsizeValue') {
              throw new RangeError('MIR verifier allowed invalid RawBuffer construction operands')
            }
            const expectedBytes = BigInt(operation.stride) * count.value
            if (
              allocation.bytes !== expectedBytes ||
              allocation.alignment !== BigInt(operation.elementAlignment)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer allocation layout does not match its element type and count',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'RawBufferValue',
                type: operation.type.type,
                ticket: allocation.ticket,
                count: count.value,
                element: operation.element,
                stride: operation.stride,
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'RawBufferForm',
                function: fn.id,
                ticket: allocation.ticket,
                count: count.value,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'RawBufferCount': {
            const buffer = referenced(operation.buffer).value
            if (buffer._tag !== 'RawBufferValue') {
              throw new RangeError('MIR verifier allowed RawBuffer.count on another value')
            }
            write(operation.destination, { value: usizeValue(buffer.count), fromCall: false })
            break
          }
          case 'RawBufferView': {
            const buffer = referenced(operation.buffer).value
            const offset = readUsize(operation.offset).value
            const length = readUsize(operation.length).value
            if (buffer._tag !== 'RawBufferValue') {
              throw new RangeError('MIR verifier allowed RawBuffer.view on another value')
            }
            if (
              offset > buffer.count ||
              length > buffer.count - offset ||
              offset > BigInt(Number.MAX_SAFE_INTEGER) ||
              length > BigInt(Number.MAX_SAFE_INTEGER)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer view range is out of bounds',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'SliceValue',
                frame: -1,
                cell: -1,
                base: Number(offset),
                length: Number(length),
                ticket: buffer.ticket,
              }),
              fromCall: false,
            })
            break
          }
          case 'RawBufferRead': {
            const buffer = referenced(operation.buffer).value
            const index = readInteger(operation.index)
            if (buffer._tag !== 'RawBufferValue' || index._tag !== 'UsizeValue') {
              throw new RangeError('MIR verifier allowed invalid RawBuffer.read operands')
            }
            if (index.value >= buffer.count) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer read index is out of bounds',
                span: operation.provenance.span,
              })
            }
            if (!Type.equals(buffer.element, operation.element)) {
              throw new RangeError('MIR verifier allowed mismatched RawBuffer read provenance')
            }
            const allocation = state.allocations.get(buffer.ticket)
            const selected = allocation?.values.get(index.value.toString())
            if (allocation === undefined || !allocation.active || selected === undefined) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer.read requires live initialized storage',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, { value: selected, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'RawBufferRead',
                function: fn.id,
                ticket: buffer.ticket,
                index: index.value,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'RawBufferSlot': {
            const buffer = referenced(operation.buffer).value
            const index = readInteger(operation.index)
            if (buffer._tag !== 'RawBufferValue' || index._tag !== 'UsizeValue') {
              throw new RangeError('MIR verifier allowed invalid RawBuffer.slot operands')
            }
            if (index.value >= buffer.count) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer slot index is out of bounds',
                span: operation.provenance.span,
              })
            }
            if (!Type.equals(buffer.element, operation.element)) {
              throw new RangeError('MIR verifier allowed mismatched RawBuffer element provenance')
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'SlotValue',
                type: operation.type.type,
                ticket: buffer.ticket,
                index: index.value,
                element: operation.element,
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'SlotProject',
                function: fn.id,
                ticket: buffer.ticket,
                index: index.value,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'RawBufferCopy': {
            const buffer = referenced(operation.buffer).value
            const offset = readUsize(operation.offset).value
            const length = readUsize(operation.length).value
            const source = read(operation.source).value
            if (
              buffer._tag !== 'RawBufferValue' ||
              (source._tag !== 'SliceValue' && source._tag !== 'StaticViewValue')
            ) {
              throw new RangeError('MIR verifier allowed invalid RawBuffer.copy operands')
            }
            if (!Type.equals(buffer.element, operation.element)) {
              throw new RangeError('MIR verifier allowed mismatched RawBuffer copy provenance')
            }
            if (
              length > BigInt(source.length) ||
              offset > buffer.count ||
              length > buffer.count - offset ||
              offset > BigInt(Number.MAX_SAFE_INTEGER) ||
              length > BigInt(Number.MAX_SAFE_INTEGER)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer copy range is out of bounds',
                span: operation.provenance.span,
              })
            }
            const destination = state.allocations.get(buffer.ticket)
            if (destination === undefined || !destination.active) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer.copy requires live destination storage',
                span: operation.provenance.span,
              })
            }
            const count = Number(length)
            // A source range is raw storage, a borrowed array cell, or immutable static data.
            const sourceStorage =
              source._tag === 'StaticViewValue' || source.ticket === undefined
                ? undefined
                : state.allocations.get(source.ticket)
            const backing =
              source._tag === 'SliceValue' && source.ticket === undefined
                ? cell(source).value
                : undefined
            if (
              source._tag === 'SliceValue' &&
              source.ticket !== undefined &&
              (sourceStorage === undefined || !sourceStorage.active)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer.copy requires live initialized source storage',
                span: operation.provenance.span,
              })
            }
            if (backing !== undefined && backing._tag !== 'ArrayValue') {
              throw new RangeError('MIR RawBuffer.copy source slice lost its array')
            }
            // Reading the whole range before any write makes an overlapping source and
            // destination behave as if the elements travelled through an intermediate buffer.
            const moved: Array<Value> = []
            for (let index = 0; index < count; index += 1) {
              const selected =
                source._tag === 'StaticViewValue'
                  ? (() => {
                      const byte = source.bytes.at(index)
                      return byte === undefined ? undefined : scalarIntegerValue('u8', BigInt(byte))
                    })()
                  : sourceStorage === undefined
                    ? backing?._tag === 'ArrayValue'
                      ? backing.elements.at(source.base + index)
                      : undefined
                    : sourceStorage.values.get(String(source.base + index))
              if (selected === undefined) {
                return blockedStep({
                  _tag: 'Trap',
                  function: fn.id,
                  reason: 'RawBuffer.copy requires live initialized source storage',
                  span: operation.provenance.span,
                })
              }
              moved.push(selected)
            }
            // A move of a Copy element leaves the source readable, which is what the byte-level
            // backends do for every element type. Only raw storage tracks per-slot initialization,
            // so only a raw-storage-backed source range gives its slots up.
            if (
              !operation.retainsSource &&
              sourceStorage !== undefined &&
              source._tag === 'SliceValue'
            ) {
              for (let index = 0; index < count; index += 1) {
                sourceStorage.values.delete(String(source.base + index))
              }
            }
            for (const [index, selected] of moved.entries()) {
              destination.values.set(String(offset + BigInt(index)), selected)
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'RawBufferCopy',
                function: fn.id,
                ticket: buffer.ticket,
                index: offset,
                count: length,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'RawBufferFill': {
            const buffer = referenced(operation.buffer).value
            const offset = readUsize(operation.offset).value
            const length = readUsize(operation.length).value
            const value = readInteger(operation.value)
            if (buffer._tag !== 'RawBufferValue' || value._tag !== 'ScalarIntegerValue') {
              throw new RangeError('MIR verifier allowed invalid RawBuffer.fill operands')
            }
            if (
              offset > buffer.count ||
              length > buffer.count - offset ||
              offset > BigInt(Number.MAX_SAFE_INTEGER) ||
              length > BigInt(Number.MAX_SAFE_INTEGER)
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer fill range is out of bounds',
                span: operation.provenance.span,
              })
            }
            const allocation = state.allocations.get(buffer.ticket)
            if (allocation === undefined || !allocation.active) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'RawBuffer.fill requires live storage',
                span: operation.provenance.span,
              })
            }
            const byte = scalarIntegerValue('u8', value.value)
            for (let index = 0; index < Number(length); index += 1) {
              allocation.values.set(String(offset + BigInt(index)), byte)
            }
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'RawBufferFill',
                function: fn.id,
                ticket: buffer.ticket,
                index: offset,
                count: length,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SlotWrite': {
            const slot = read(operation.slot).value
            if (slot._tag !== 'SlotValue' || !Type.equals(slot.element, operation.element)) {
              throw new RangeError('MIR verifier allowed Slot.write with mismatched provenance')
            }
            const allocation = state.allocations.get(slot.ticket)
            const key = slot.index.toString()
            if (allocation === undefined || !allocation.active || allocation.values.has(key)) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Slot.write requires live uninitialized storage',
                span: operation.provenance.span,
              })
            }
            allocation.values.set(key, read(operation.value).value)
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'SlotWrite',
                function: fn.id,
                ticket: slot.ticket,
                index: slot.index,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SlotTake': {
            const slot = read(operation.slot).value
            if (slot._tag !== 'SlotValue' || !Type.equals(slot.element, operation.element)) {
              throw new RangeError('MIR verifier allowed Slot.take with mismatched provenance')
            }
            const allocation = state.allocations.get(slot.ticket)
            const key = slot.index.toString()
            const selected = allocation?.values.get(key)
            if (allocation === undefined || !allocation.active || selected === undefined) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Slot.take requires live initialized storage',
                span: operation.provenance.span,
              })
            }
            allocation.values.delete(key)
            write(operation.destination, { value: selected, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'SlotTake',
                function: fn.id,
                ticket: slot.ticket,
                index: slot.index,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SlotCopy': {
            const slot = read(operation.slot).value
            if (slot._tag !== 'SlotValue' || !Type.equals(slot.element, operation.element)) {
              throw new RangeError('MIR verifier allowed Slot.copy with mismatched provenance')
            }
            const allocation = state.allocations.get(slot.ticket)
            const key = slot.index.toString()
            const selected = allocation?.values.get(key)
            if (allocation === undefined || !allocation.active || selected === undefined) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Slot.copy requires live initialized storage',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, { value: selected, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'SlotCopy',
                function: fn.id,
                ticket: slot.ticket,
                index: slot.index,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'SlotDrop': {
            const slot = read(operation.slot).value
            if (slot._tag !== 'SlotValue' || !Type.equals(slot.element, operation.element)) {
              throw new RangeError('MIR verifier allowed Slot.drop with mismatched provenance')
            }
            const allocation = state.allocations.get(slot.ticket)
            const key = slot.index.toString()
            const selected = allocation?.values.get(key)
            if (allocation === undefined || !allocation.active || selected === undefined) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'Slot.drop requires live initialized storage',
                span: operation.provenance.span,
              })
            }
            const blocked = yield* releaseThroughPlan(
              operation.cleanup,
              selected,
              operation.provenance,
              operation.slot.ordinal,
            )
            if (blocked !== undefined) return blocked
            allocation.values.delete(key)
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'AggregateValue',
                type: Type.unit,
                fields: Object.freeze([]),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'SlotDrop',
                function: fn.id,
                ticket: slot.ticket,
                index: slot.index,
                element: operation.element,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'Binary': {
            const leftType = fn.localTypes.at(operation.left.ordinal)
            const semantic = leftType === undefined ? undefined : Mir.semanticType(leftType)
            if (Scalar.isFloatSpelling(semantic)) {
              write(operation.destination, {
                value: floatingBinary(
                  operation.operator,
                  readFloat(operation.left),
                  readFloat(operation.right),
                ),
                fromCall: false,
              })
              break
            }
            if (Scalar.isCharacterSpelling(semantic)) {
              const left = readCharacter(operation.left).value
              const right = readCharacter(operation.right).value
              const holds =
                operation.operator === 'Equals'
                  ? left === right
                  : operation.operator === 'NotEquals'
                    ? left !== right
                    : operation.operator === 'LessThan'
                      ? left < right
                      : operation.operator === 'LessOrEqual'
                        ? left <= right
                        : operation.operator === 'GreaterThan'
                          ? left > right
                          : operation.operator === 'GreaterOrEqual'
                            ? left >= right
                            : undefined
              if (holds === undefined)
                throw new RangeError('MIR verifier allowed a non-comparison char operation')
              write(operation.destination, { value: value(holds ? 1 : 0), fromCall: false })
              break
            }
            const leftValue = readInteger(operation.left)
            const rightValue = readInteger(operation.right)
            const rightType = fn.localTypes.at(operation.right.ordinal)
            const operand = leftType === undefined ? undefined : Mir.semanticType(leftType)
            if (
              rightType === undefined ||
              operand === undefined ||
              !Type.equals(Mir.semanticType(rightType), operand) ||
              !Scalar.isSpelling(operand)
            ) {
              throw new RangeError('MIR verifier allowed mixed integer operands')
            }
            const scalar = Scalar.find(operand)
            const left = BigInt(leftValue.value)
            const right = BigInt(rightValue.value)
            if (
              operation.operator === 'Equals' ||
              operation.operator === 'NotEquals' ||
              operation.operator === 'LessThan' ||
              operation.operator === 'LessOrEqual' ||
              operation.operator === 'GreaterThan' ||
              operation.operator === 'GreaterOrEqual'
            ) {
              const holds =
                operation.operator === 'Equals'
                  ? left === right
                  : operation.operator === 'NotEquals'
                    ? left !== right
                    : operation.operator === 'LessThan'
                      ? left < right
                      : operation.operator === 'LessOrEqual'
                        ? left <= right
                        : operation.operator === 'GreaterThan'
                          ? left > right
                          : left >= right
              write(operation.destination, {
                value: value(holds ? 1 : 0),
                fromCall: false,
              })
              break
            }
            if (scalar === undefined || scalar.category !== 'Integer') {
              throw new RangeError('MIR verifier allowed a non-integer binary operand')
            }
            if (
              (operation.operator === 'Divide' || operation.operator === 'Remainder') &&
              right === 0n
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'division by zero',
                span: operation.provenance.span,
              })
            }
            const pointerBits = program.layout.target.pointerSize === 4 ? 32 : 64
            const width = Scalar.bits(scalar, pointerBits)
            if (
              (operation.operator === 'ShiftLeft' || operation.operator === 'ShiftRight') &&
              (right < 0n || right >= BigInt(width))
            ) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: `invalid ${operation.operator} count ${right}`,
                span: operation.provenance.span,
              })
            }
            const fromBits = (input: bigint): bigint =>
              scalar.signedness === 'Signed'
                ? BigInt.asIntN(width, input)
                : BigInt.asUintN(width, input)
            const leftBits = BigInt.asUintN(width, left)
            const rightBits = BigInt.asUintN(width, right)
            const rotate = Number(right % BigInt(width))
            const rotatedLeft =
              rotate === 0
                ? leftBits
                : BigInt.asUintN(
                    width,
                    (leftBits << BigInt(rotate)) | (leftBits >> BigInt(width - rotate)),
                  )
            const rotatedRight =
              rotate === 0
                ? leftBits
                : BigInt.asUintN(
                    width,
                    (leftBits >> BigInt(rotate)) | (leftBits << BigInt(width - rotate)),
                  )
            const exact =
              operation.operator === 'Add' ||
              operation.operator === 'WrappingAdd' ||
              operation.operator === 'SaturatingAdd'
                ? left + right
                : operation.operator === 'Subtract' ||
                    operation.operator === 'WrappingSubtract' ||
                    operation.operator === 'SaturatingSubtract'
                  ? left - right
                  : operation.operator === 'Multiply' ||
                      operation.operator === 'WrappingMultiply' ||
                      operation.operator === 'SaturatingMultiply'
                    ? left * right
                    : operation.operator === 'Divide'
                      ? left / right
                      : operation.operator === 'Remainder'
                        ? left % right
                        : operation.operator === 'BitAnd'
                          ? fromBits(leftBits & rightBits)
                          : operation.operator === 'BitOr'
                            ? fromBits(leftBits | rightBits)
                            : operation.operator === 'BitXor'
                              ? fromBits(leftBits ^ rightBits)
                              : operation.operator === 'ShiftLeft'
                                ? fromBits(leftBits << right)
                                : operation.operator === 'ShiftRight'
                                  ? scalar.signedness === 'Signed'
                                    ? left >> right
                                    : fromBits(leftBits >> right)
                                  : operation.operator === 'RotateLeft'
                                    ? fromBits(rotatedLeft)
                                    : fromBits(rotatedRight)
            const range = Scalar.range(scalar, pointerBits)
            const wrapping =
              operation.operator === 'WrappingAdd' ||
              operation.operator === 'WrappingSubtract' ||
              operation.operator === 'WrappingMultiply'
            const saturating =
              operation.operator === 'SaturatingAdd' ||
              operation.operator === 'SaturatingSubtract' ||
              operation.operator === 'SaturatingMultiply'
            if (!wrapping && !saturating && (exact < range.minimum || exact > range.maximum)) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason:
                  scalar.signedness === 'Unsigned' && exact < 0n
                    ? 'arithmetic underflow'
                    : 'arithmetic overflow',
                span: operation.provenance.span,
              })
            }
            const result = wrapping
              ? fromBits(exact)
              : saturating
                ? exact < range.minimum
                  ? range.minimum
                  : exact > range.maximum
                    ? range.maximum
                    : exact
                : exact
            write(operation.destination, {
              value: scalarIntegerValue(scalar.spelling, result),
              fromCall: false,
            })
            break
          }
          case 'ConvertInteger': {
            const subject = readInteger(operation.source)
            const target = Scalar.find(operation.type._tag)
            if (target === undefined || target.category !== 'Integer')
              throw new RangeError('MIR verifier allowed a non-integer conversion target')
            const exact = BigInt(subject.value)
            const range = Scalar.range(target, program.layout.target.pointerSize === 4 ? 32 : 64)
            if (exact < range.minimum || exact > range.maximum)
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'integer conversion out of range',
                span: operation.provenance.span,
              })
            write(operation.destination, {
              value: scalarIntegerValue(target.spelling, exact),
              fromCall: false,
            })
            break
          }
          case 'ConvertScalar': {
            const sourceType = Scalar.find(operation.sourceType._tag)
            const targetType = Scalar.find(operation.type._tag)
            if (sourceType?.category === 'Floating' && targetType?.category === 'Floating') {
              const source = readFloat(operation.source)
              const encoded = FloatingPoint.fromNumber(
                FloatingPoint.toNumber(floatingBits(source)),
                targetType.spelling === 'f32' ? 32 : 64,
              )
              write(operation.destination, {
                value: floatValue(targetType.spelling, encoded.bits),
                fromCall: false,
              })
              break
            }
            if (sourceType?.category === 'Floating' && targetType?.category === 'Integer') {
              const number = FloatingPoint.toNumber(floatingBits(readFloat(operation.source)))
              const exact = Number.isFinite(number) ? BigInt(Math.trunc(number)) : undefined
              const range = Scalar.range(
                targetType,
                program.layout.target.pointerSize === 4 ? 32 : 64,
              )
              if (exact === undefined || exact < range.minimum || exact > range.maximum)
                return blockedStep({
                  _tag: 'Trap',
                  function: fn.id,
                  reason: 'float conversion out of range',
                  span: operation.provenance.span,
                })
              write(operation.destination, {
                value: scalarIntegerValue(targetType.spelling, exact),
                fromCall: false,
              })
              break
            }
            if (sourceType?.category === 'Integer' && targetType?.category === 'Floating') {
              const encoded = FloatingPoint.fromNumber(
                Number(BigInt(readInteger(operation.source).value)),
                targetType.spelling === 'f32' ? 32 : 64,
              )
              write(operation.destination, {
                value: floatValue(targetType.spelling, encoded.bits),
                fromCall: false,
              })
              break
            }
            throw new RangeError('MIR verifier allowed an invalid scalar conversion')
          }
          case 'ReinterpretScalar': {
            const target = Scalar.find(operation.type._tag)
            const subject = read(operation.source).value
            if (target?.category === 'Floating') {
              if (
                subject._tag !== 'I32Value' &&
                subject._tag !== 'UsizeValue' &&
                subject._tag !== 'ScalarIntegerValue'
              )
                throw new RangeError('MIR verifier allowed invalid float reinterpretation')
              write(operation.destination, {
                value: floatValue(target.spelling, BigInt(subject.value)),
                fromCall: false,
              })
            } else if (target?.category === 'Integer' && subject._tag === 'FloatValue') {
              write(operation.destination, {
                value: scalarIntegerValue(target.spelling, subject.bits),
                fromCall: false,
              })
            } else throw new RangeError('MIR verifier allowed invalid scalar reinterpretation')
            break
          }
          case 'FloatUnary':
            write(operation.destination, {
              value: floatingUnary(operation.operation, readFloat(operation.source)),
              fromCall: false,
            })
            break
          case 'FloatTranscendental': {
            const source = readFloat(operation.source)
            const result = Transcendental.evaluate(operation.operation, floatingBits(source))
            write(operation.destination, {
              value: floatValue(source.type, result.bits),
              fromCall: false,
            })
            break
          }
          case 'CheckedInteger': {
            const operands = operation.operands.map((operand) => BigInt(readInteger(operand).value))
            const left = operands.at(0)
            const right = operands.at(1)
            const source = Scalar.find(operation.sourceType._tag)
            const target = Scalar.find(operation.valueType._tag)
            if (
              left === undefined ||
              source?.category !== 'Integer' ||
              target?.category !== 'Integer'
            )
              throw new RangeError('MIR verifier allowed an invalid checked integer operation')
            const arithmetic = operation.operation.startsWith('CheckedConvertTo')
              ? left
              : operation.operation === 'CheckedAdd'
                ? left + (right ?? 0n)
                : operation.operation === 'CheckedSubtract'
                  ? left - (right ?? 0n)
                  : operation.operation === 'CheckedMultiply'
                    ? left * (right ?? 0n)
                    : operation.operation === 'CheckedDivide'
                      ? right === undefined || right === 0n
                        ? undefined
                        : left / right
                      : operation.operation === 'CheckedRemainder'
                        ? right === undefined || right === 0n
                          ? undefined
                          : left % right
                        : undefined
            const range = Scalar.range(target, program.layout.target.pointerSize === 4 ? 32 : 64)
            const success =
              arithmetic !== undefined && arithmetic >= range.minimum && arithmetic <= range.maximum
            const member = success ? operation.success : operation.failure
            const entry = program.layout.entries.find((candidate) =>
              Type.equals(candidate.type, member),
            )
            if (entry?._tag !== 'LayoutEntry' || entry.representation._tag !== 'Aggregate')
              throw new RangeError('Target plan omitted a canonical Option member')
            const payload: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: member,
              fields: Object.freeze(
                success
                  ? entry.representation.fields.map((field) =>
                      Object.freeze({
                        field: field.id,
                        value: scalarIntegerValue(target.spelling, arithmetic),
                      }),
                    )
                  : [],
              ),
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'UnionValue',
                type: operation.type.type,
                member,
                payload,
              }),
              fromCall: false,
            })
            break
          }
          case 'Construct': {
            const aggregate: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: operation.type.type,
              fields: Object.freeze(
                operation.fields.map((field) =>
                  Object.freeze({ field: field.field, value: read(field.value).value }),
                ),
              ),
            })
            write(operation.destination, { value: aggregate, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'Construct',
                function: fn.id,
                type: aggregate.type,
                fieldCount: aggregate.fields.length,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ConstructArray': {
            const array: ArrayValue = Object.freeze({
              _tag: 'ArrayValue',
              type: operation.type.type,
              elements: Object.freeze(operation.elements.map((element) => read(element).value)),
            })
            write(operation.destination, { value: array, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'ArrayConstruct',
                function: fn.id,
                type: array.type,
                elementCount: array.elements.length,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'Project': {
            const aggregate = read(operation.source).value
            if (aggregate._tag !== 'AggregateValue') {
              throw new RangeError('MIR verifier allowed projection from a scalar value')
            }
            const selected = aggregate.fields.find(
              (candidate) =>
                candidate.field.ordinal === operation.field.ordinal &&
                candidate.field.struct.sourceId === operation.field.struct.sourceId &&
                candidate.field.struct.ordinal === operation.field.struct.ordinal,
            )
            if (selected === undefined) {
              throw new RangeError('MIR verifier allowed projection of a missing aggregate field')
            }
            write(operation.destination, { value: selected.value, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'Project',
                function: fn.id,
                type: aggregate.type,
                field: operation.field,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ReadPlace': {
            let selected = read(operation.root).value
            let effectiveSelectors = operation.selectors
            const selectors: Array<PlaceReadTraceEvent['selectors'][number]> = []
            if (selected._tag === 'ReferenceValue') {
              const target = state.cells.get(cellKey(selected.frame, selected.cell))
              if (target === undefined)
                throw new RangeError('MIR reference points at a missing evaluator cell')
              effectiveSelectors = Object.freeze([...selected.selectors, ...operation.selectors])
              selected = target.value
            }
            for (const selector of effectiveSelectors) {
              if (selector._tag === 'FieldSelector') {
                if (selected._tag !== 'AggregateValue') {
                  throw new RangeError(
                    'MIR verifier allowed a field selector on a non-struct value',
                  )
                }
                const field = selected.fields.find(
                  (candidate) =>
                    candidate.field.ordinal === selector.field.ordinal &&
                    candidate.field.struct.sourceId === selector.field.struct.sourceId &&
                    candidate.field.struct.ordinal === selector.field.struct.ordinal,
                )
                if (field === undefined) {
                  throw new RangeError('MIR verifier allowed a missing field selector')
                }
                selected = field.value
                selectors.push(Object.freeze({ _tag: 'Field', field: selector.field }))
                continue
              }
              if (selector._tag === 'SliceElementSelector') {
                if (selected._tag !== 'SliceValue' && selected._tag !== 'StaticViewValue') {
                  throw new RangeError('MIR verifier allowed a slice selector on a non-slice value')
                }
                const exactIndex = readUsize(selector.index).value
                if (exactIndex >= BigInt(selected.length)) {
                  return blockedStep({
                    _tag: 'Trap',
                    function: fn.id,
                    reason: `slice index ${exactIndex} is outside length ${selected.length} in ${fn.id.module}.${fn.id.name}`,
                    span: selector.provenance.span,
                  })
                }
                const index = Number(exactIndex)
                if (selected._tag === 'StaticViewValue') {
                  const byte = selected.bytes.at(index)
                  if (byte === undefined) {
                    throw new RangeError('MIR static view range exceeds its immutable bytes')
                  }
                  selectors.push(
                    Object.freeze({
                      _tag: 'StaticElement',
                      data: selected.data,
                      index,
                      bounds: 'Checked',
                      span: selector.provenance.span,
                    }),
                  )
                  selected = scalarIntegerValue('u8', BigInt(byte))
                  continue
                }
                if (selected.ticket !== undefined) {
                  const allocation = state.allocations.get(selected.ticket)
                  const absolute = selected.base + index
                  const element = allocation?.values.get(String(absolute))
                  if (allocation === undefined || !allocation.active || element === undefined) {
                    throw new RangeError('MIR RawBuffer slice selected uninitialized storage')
                  }
                  selectors.push(
                    Object.freeze({
                      _tag: 'RawBufferElement',
                      ticket: selected.ticket,
                      index: absolute,
                      bounds: 'Checked',
                      span: selector.provenance.span,
                    }),
                  )
                  selected = element
                  continue
                }
                const backing = cell(selected).value
                if (backing._tag !== 'ArrayValue') {
                  throw new RangeError('MIR slice cell does not contain an array value')
                }
                const element = backing.elements.at(selected.base + index)
                if (element === undefined) {
                  throw new RangeError('MIR slice range exceeds its backing cell')
                }
                selectors.push(
                  Object.freeze({
                    _tag: 'Element',
                    array: backing.type,
                    index,
                    bounds: 'Checked',
                    span: selector.provenance.span,
                  }),
                )
                selected = element
                continue
              }
              if (selected._tag !== 'ArrayValue') {
                throw new RangeError(
                  'MIR verifier allowed an element selector on a non-array value',
                )
              }
              const index =
                selector.index._tag === 'Proven'
                  ? selector.index.value
                  : Number(readUsize(selector.index.local).value)
              if (index < 0 || !Number.isSafeInteger(index) || index >= selector.length) {
                return blockedStep({
                  _tag: 'Trap',
                  function: fn.id,
                  reason: `array index ${index} is outside length ${selector.length} in ${fn.id.module}.${fn.id.name}`,
                  span: selector.provenance.span,
                })
              }
              const element = selected.elements.at(index)
              if (element === undefined) {
                throw new RangeError('MIR verifier allowed an incomplete array value')
              }
              selectors.push(
                Object.freeze({
                  _tag: 'Element',
                  array: selected.type,
                  index,
                  bounds: selector.index._tag === 'Proven' ? 'Proven' : 'Checked',
                  span: selector.provenance.span,
                }),
              )
              selected = element
            }
            write(operation.destination, { value: selected, fromCall: false })
            trace.push(
              Object.freeze({
                _tag: 'PlaceRead',
                function: fn.id,
                selectors: Object.freeze(selectors),
                value: selected,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'CheckPlace': {
            const resolved = resolvePlace(operation.root, operation.selectors)
            if (resolved._tag === 'Blocked') return resolved.step
            checkedPlaces.set(operation.selectors, resolved.indexes)
            trace.push(
              Object.freeze({
                _tag: 'WriteCheck',
                function: fn.id,
                region: region.id.ordinal,
                ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'WritePlace': {
            const indexes = checkedPlaces.get(operation.selectors)
            if (indexes === undefined)
              throw new RangeError('MIR write executed without its precheck')
            const root = read(operation.root)
            const replacement = read(operation.source)
            // A reference root writes through the borrow: replace within the referenced cell
            // and store it back where the loan began, leaving the reference local untouched.
            if (root.value._tag === 'ReferenceValue' && operation.selectors.length > 0) {
              const key = cellKey(root.value.frame, root.value.cell)
              const target = state.cells.get(key)
              if (target === undefined)
                throw new RangeError('MIR reference points at a missing evaluator cell')
              const effectiveSelectors = Object.freeze([
                ...root.value.selectors,
                ...operation.selectors,
              ])
              state.cells.set(key, {
                value: replacePlace(target.value, effectiveSelectors, indexes, replacement.value),
                fromCall: replacement.fromCall,
              })
              checkedPlaces.delete(operation.selectors)
              trace.push(
                Object.freeze({
                  _tag: 'Replacement',
                  function: fn.id,
                  region: region.id.ordinal,
                  ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
                  span: operation.provenance.span,
                }),
              )
              break
            }
            if (operation.replacement === 'Owned') {
              const previous = resolvePlace(operation.root, operation.selectors)
              if (previous._tag === 'Blocked') return previous.step
              const members =
                previous.selected._tag === 'UnionValue'
                  ? Object.freeze([previous.selected.member])
                  : Object.freeze([])
              trace.push(
                Object.freeze({
                  _tag: 'ReplacementCleanup',
                  function: fn.id,
                  region: region.id.ordinal,
                  ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
                  ...(members.length === 0 ? {} : { members }),
                  span: operation.provenance.span,
                }),
              )
            }
            write(operation.root, {
              value: replacePlace(root.value, operation.selectors, indexes, replacement.value),
              fromCall: replacement.fromCall,
            })
            checkedPlaces.delete(operation.selectors)
            trace.push(
              Object.freeze({
                _tag: 'Replacement',
                function: fn.id,
                region: region.id.ordinal,
                ...(region.ownerLoop === undefined ? {} : { loop: region.ownerLoop.ordinal }),
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'Drop': {
            const dropped = read(operation.local).value
            const blocked = yield* releaseThroughPlan(
              operation.cleanup,
              dropped,
              operation.provenance,
              operation.local.ordinal,
            )
            if (blocked !== undefined) return blocked
            const members = cleanupMembers(operation.cleanup, dropped)
            trace.push(
              Object.freeze({
                _tag: 'Cleanup',
                frame: activation.frame,
                depth: activation.depth,
                function: fn.id,
                local: operation.local.ordinal,
                ...(members.length === 0 ? {} : { members }),
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'MakeCallable': {
            const ticket = state.nextCallable
            state.nextCallable += 1
            state.callables.set(ticket, { state: 'Available' })
            const captures = operation.captures.map((capture) => {
              const captured: Value =
                capture.access === 'Copy' || capture.access === 'Take'
                  ? read(capture.source).value
                  : (() => {
                      const key = cellKey(frame, capture.source.ordinal)
                      if (!state.cells.has(key)) state.cells.set(key, read(capture.source))
                      return Object.freeze({
                        _tag: 'CallableBorrowValue' as const,
                        frame,
                        cell: capture.source.ordinal,
                        access: capture.access,
                      })
                    })()
              return Object.freeze({
                ordinal: capture.ordinal,
                parameterOrdinal: capture.parameterOrdinal,
                access: capture.access,
                value: captured,
              })
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'CallableValue',
                ticket,
                type: operation.type.type,
                target: operation.target,
                typeArguments: operation.typeArguments,
                captures: Object.freeze(captures),
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'CallableConstruct',
                function: fn.id,
                ticket,
                mode: operation.type.type.mode,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'ApplyCallable': {
            const stored =
              operation.callable === undefined ? undefined : read(operation.callable).value
            if (stored !== undefined && stored._tag !== 'CallableValue') {
              throw new RangeError('MIR verifier allowed applying a non-callable evaluator value')
            }
            const target = stored?.target ?? operation.target
            if (target === undefined)
              throw new RangeError('MIR verifier allowed a callable application without identity')
            const ticket = stored?.ticket
            const callableState = ticket === undefined ? undefined : state.callables.get(ticket)
            if (ticket !== undefined && callableState === undefined)
              throw new RangeError('Callable application referenced a missing evaluator identity')
            if (ticket !== undefined && callableState !== undefined) {
              if (callableState.state !== 'Available') {
                trace.push(
                  Object.freeze({
                    _tag: 'CallableRejected',
                    function: fn.id,
                    ticket,
                    mode: stored?.type.mode ?? operation.callableType.mode,
                    span: operation.provenance.span,
                  }),
                )
                return blockedStep({
                  _tag: 'InvalidCallableReuse',
                  function: fn.id,
                  ticket,
                  state: callableState.state,
                  span: operation.provenance.span,
                })
              }
              callableState.state =
                stored?.type.mode === 'Take' || operation.access === 'Take' ? 'Consumed' : 'Running'
            }
            const captureValues = (
              stored?.captures ??
              operation.captures.map((capture) => {
                const captured: Value =
                  operation.callable === undefined ||
                  capture.access === 'Copy' ||
                  capture.access === 'Take'
                    ? read(capture.source).value
                    : Object.freeze({
                        _tag: 'CallableBorrowValue' as const,
                        frame,
                        cell: capture.source.ordinal,
                        access: capture.access,
                      })
                return Object.freeze({ ...capture, value: captured })
              })
            ).map((capture) => {
              const captured = capture.value
              const resolved =
                captured._tag === 'CallableBorrowValue'
                  ? state.cells.get(cellKey(captured.frame, captured.cell))?.value
                  : captured
              if (resolved === undefined)
                throw new RangeError('Callable capture references a missing evaluator cell')
              return Object.freeze({ parameterOrdinal: capture.parameterOrdinal, value: resolved })
            })
            const parameters = new Map<number, Value>()
            operation.arguments.forEach((argument, ordinal) => {
              parameters.set(ordinal, read(argument).value)
            })
            for (const capture of captureValues)
              parameters.set(capture.parameterOrdinal, capture.value)
            const arguments_ = Object.freeze(
              [...parameters.entries()]
                .sort(([left], [right]) => left - right)
                .map(([, argument]) => argument),
            )
            trace.push(
              Object.freeze({
                _tag: 'CallableApply',
                function: fn.id,
                ticket: ticket ?? -1,
                mode: stored?.type.mode ?? operation.callableType.mode,
                span: operation.provenance.span,
              }),
            )
            const result = yield* invokeCallableTarget(
              target,
              stored?.typeArguments ?? operation.typeArguments,
              arguments_,
              operation.provenance.span,
            )
            if (callableState?.state === 'Running') callableState.state = 'Available'
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            write(operation.destination, { value: result.value, fromCall: true })
            break
          }
          case 'MakeEffect': {
            const captures = operation.captures.map((capture, ordinal): Value => {
              const field = operation.type.environment.fields.at(ordinal)
              if (
                capture.access === 'Copy' ||
                capture.access === 'Take' ||
                field?.representation === 'Callable' ||
                field?.effectIdentity !== undefined
              )
                return read(capture.source).value
              const key = cellKey(frame, capture.source.ordinal)
              if (!state.cells.has(key)) state.cells.set(key, read(capture.source))
              return Object.freeze({
                _tag: 'EffectBorrowValue',
                frame,
                cell: capture.source.ordinal,
                access: capture.access,
              })
            })
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'EffectValue',
                type: operation.type.type,
                site: operation.type.site,
                runner: operation.runner,
                runnerTypeArguments: operation.runnerTypeArguments,
                captures: Object.freeze(captures),
              }),
              fromCall: false,
            })
            break
          }
          case 'PackEffectOutcome': {
            const payload = read(operation.source).value
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'EffectOutcomeValue',
                type: operation.type.type,
                tag: operation.tag,
                payload,
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: operation.tag === 0 ? 'EffectSuccess' : 'EffectFailure',
                function: fn.id,
                tag: operation.tag,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'PackEffectFailureUnion': {
            const source = read(operation.source).value
            if (source._tag !== 'UnionValue')
              throw new RangeError('MIR attempted to fail with a non-union value')
            const mapping = operation.mappings.find((candidate) =>
              Type.equals(
                operation.sourceType.type.members.at(candidate.source) ?? Type.unit,
                source.member,
              ),
            )
            if (mapping === undefined)
              throw new RangeError('MIR failure union has no canonical E-channel mapping')
            write(operation.destination, {
              value: Object.freeze({
                _tag: 'EffectOutcomeValue',
                type: operation.type.type,
                tag: mapping.target,
                payload: source.payload,
              }),
              fromCall: false,
            })
            trace.push(
              Object.freeze({
                _tag: 'EffectFailure',
                function: fn.id,
                tag: mapping.target,
                span: operation.provenance.span,
              }),
            )
            break
          }
          case 'UnpackEffectSuccess': {
            const outcome = read(operation.source)
            if (outcome.value._tag !== 'EffectOutcomeValue' || outcome.value.tag !== 0) {
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'attempted to unpack a failed effect outcome as success',
                span: operation.provenance.span,
              })
            }
            write(operation.destination, { value: outcome.value.payload, fromCall: true })
            break
          }
          case 'RunEffect': {
            const target = functionFor(program, operation.target, operation.typeArguments)
            if (target === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.target,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.target,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const arguments_ = operation.arguments.map((argument) => read(argument))
            arguments_.forEach((argument, ordinal) => {
              trace.push(
                Object.freeze({
                  _tag: 'Binding',
                  frame: state.nextFrame,
                  depth: activation.depth + 1,
                  target: operation.target,
                  targetInstance: target.instance,
                  callSpan: operation.provenance.span,
                  argumentOrdinal: ordinal,
                  parameterOrdinal: ordinal,
                  value: argument.value,
                  fromCall: argument.fromCall,
                  span: operation.provenance.span,
                }),
              )
            })
            const result = yield* callEffectRunner(
              target,
              arguments_.map((argument) => argument.value),
              operation,
            )
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            if (result.value._tag !== 'EffectOutcomeValue')
              throw new RangeError('MIR propagated effect returned a non-outcome value')
            const effectOutcome = result.value
            write(operation.outcome, { value: effectOutcome, fromCall: true })
            if (effectOutcome.tag === 0) {
              write(operation.destination, { value: effectOutcome.payload, fromCall: true })
              break
            }
            const mapping = operation.tagMappings.find(
              (candidate) => candidate.source === effectOutcome.tag,
            )
            if (mapping === undefined)
              throw new RangeError('MIR propagated effect has no canonical failure-tag mapping')
            const released = yield* executeOperations(operation.releases ?? [])
            if (released !== undefined) return released
            const propagated: EffectOutcomeValue = Object.freeze({
              _tag: 'EffectOutcomeValue',
              type: operation.propagationType.type,
              tag: mapping.target,
              payload: effectOutcome.payload,
            })
            trace.push(
              Object.freeze({
                _tag: 'EffectFailure',
                function: fn.id,
                tag: mapping.target,
                span: operation.provenance.span,
              }),
              Object.freeze({
                _tag: 'Return',
                frame: activation.frame,
                depth: activation.depth,
                function: fn.id,
                instance: fn.instance,
                value: propagated,
                span: operation.provenance.span,
              }),
            )
            return Object.freeze({ _tag: 'Value', value: propagated })
          }
          case 'RunEffectValue':
          case 'RunStaticEffect': {
            const captures =
              operation._tag === 'RunEffectValue'
                ? (() => {
                    const effect = read(operation.effect).value
                    if (effect._tag !== 'EffectValue')
                      throw new RangeError('MIR attempted to run a non-Effect value')
                    return effect.captures
                  })()
                : operation.captures.map((capture) => read(capture.source).value)
            const target = functionFor(program, operation.runner, operation.runnerTypeArguments)
            if (target === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.runner,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.runner,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const runnerArguments = Object.freeze([
              ...captures,
              ...operation.arguments.map((argument) => read(argument).value),
            ])
            const result =
              operation._tag === 'RunEffectValue'
                ? yield* callEffectRunner(target, runnerArguments, operation)
                : yield* callFunction(target, runnerArguments, operation.provenance.span)
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            if (result.value._tag !== 'EffectOutcomeValue')
              throw new RangeError('MIR Effect runner returned a non-outcome value')
            const effectOutcome = result.value
            write(operation.outcome, { value: effectOutcome, fromCall: true })
            if (effectOutcome.tag === 0) {
              write(operation.destination, { value: effectOutcome.payload, fromCall: true })
              break
            }
            if (operation.propagationType === undefined)
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: 'unhandled Effect failure escaped an infallible context',
                span: operation.provenance.span,
              })
            const mapping = operation.tagMappings.find(
              (candidate) => candidate.source === effectOutcome.tag,
            )
            if (mapping === undefined)
              throw new RangeError('MIR Effect runner has no failure-tag mapping')
            const released = yield* executeOperations(operation.releases ?? [])
            if (released !== undefined) return released
            const propagated: EffectOutcomeValue = Object.freeze({
              _tag: 'EffectOutcomeValue',
              type: operation.propagationType.type,
              tag: mapping.target,
              payload: effectOutcome.payload,
            })
            return Object.freeze({ _tag: 'Value', value: propagated })
          }
          case 'ReifyEffect': {
            const effect = read(operation.effect).value
            if (effect._tag !== 'EffectValue')
              throw new RangeError('MIR attempted to reify a non-Effect value')
            const target = functionFor(program, operation.runner, operation.runnerTypeArguments)
            if (target === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.runner,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.runner,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const result = yield* callEffectRunner(
              target,
              Object.freeze([
                ...effect.captures,
                ...operation.arguments.map((argument) => read(argument).value),
              ]),
              operation,
            )
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            if (result.value._tag !== 'EffectOutcomeValue')
              throw new RangeError('MIR Effect result runner returned a non-outcome value')
            const outcome = result.value
            write(operation.outcome, { value: outcome, fromCall: true })
            const branch: AggregateValue =
              outcome.tag === 0
                ? Object.freeze({
                    _tag: 'AggregateValue',
                    type: operation.successType,
                    fields: Object.freeze([
                      Object.freeze({ field: operation.successField, value: outcome.payload }),
                    ]),
                  })
                : (() => {
                    const failure = operation.outcomeType.type.failures.at(outcome.tag - 1)
                    if (failure === undefined || outcome.payload._tag !== 'AggregateValue')
                      throw new RangeError('MIR Effect result has an invalid failure tag')
                    const failureValue: Value = Type.isUnion(operation.failureValueType)
                      ? Object.freeze({
                          _tag: 'UnionValue',
                          type: operation.failureValueType,
                          member: failure,
                          payload: outcome.payload,
                        })
                      : outcome.payload
                    return Object.freeze({
                      _tag: 'AggregateValue' as const,
                      type: operation.failureType,
                      fields: Object.freeze([
                        Object.freeze({ field: operation.failureField, value: failureValue }),
                      ]),
                    })
                  })()
            const outer: UnionValue = Object.freeze({
              _tag: 'UnionValue',
              type: operation.resultUnion,
              member: outcome.tag === 0 ? operation.successType : operation.failureType,
              payload: branch,
            })
            const completed: AggregateValue = Object.freeze({
              _tag: 'AggregateValue',
              type: operation.resultType.type,
              fields: Object.freeze([
                Object.freeze({ field: operation.resultField, value: outer }),
              ]),
            })
            write(operation.destination, { value: completed, fromCall: true })
            break
          }
          case 'CloseEffectEntry': {
            const target = functionFor(program, operation.target, operation.typeArguments)
            const runner = functionFor(program, operation.runner, operation.typeArguments)
            if (target === undefined || runner === undefined)
              return blockedStep({
                _tag: 'MissingFunction',
                target: target === undefined ? operation.target : operation.runner,
                span: operation.provenance.span,
              })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.target,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const result = yield* callFunction(target, [], operation.provenance.span)
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            if (result.value._tag !== 'EffectValue')
              throw new RangeError('MIR effect entry constructor returned a non-Effect value')
            const effect = result.value
            write(operation.effect, { value: effect, fromCall: true })
            trace.push(
              Object.freeze({
                _tag: 'Call',
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.runner,
                callerInstance: fn.instance,
                targetInstance: runner.instance,
                span: operation.provenance.span,
              }),
            )
            const execution = yield* callFunction(
              runner,
              effect.captures,
              operation.provenance.span,
            )
            if (execution._tag === 'Blocked') return execution
            if (execution._tag === 'Transfer') return execution
            if (execution.value._tag !== 'EffectOutcomeValue')
              throw new RangeError('MIR effect entry runner returned a non-outcome value')
            const effectOutcome = execution.value
            write(operation.outcome, { value: effectOutcome, fromCall: true })
            if (effectOutcome.tag === 0) {
              write(operation.destination, { value: value(0), fromCall: true })
              break
            }
            const failure = operation.failures.find(
              (candidate) => candidate.tag === effectOutcome.tag,
            )
            if (failure === undefined)
              return blockedStep({
                _tag: 'Trap',
                function: fn.id,
                reason: `effect entry returned invalid failure tag ${effectOutcome.tag}`,
                span: operation.provenance.span,
              })
            write(failure.payload, { value: effectOutcome.payload, fromCall: true })
            const released = yield* releaseThroughPlan(
              failure.cleanup,
              effectOutcome.payload,
              operation.provenance,
              failure.payload.ordinal,
            )
            if (released !== undefined) return released
            trace.push(
              Object.freeze({
                _tag: 'EffectFailure',
                function: fn.id,
                tag: failure.tag,
                span: operation.provenance.span,
              }),
            )
            write(operation.destination, { value: value(failure.tag), fromCall: true })
            break
          }
          case 'Call': {
            const target = functionFor(program, operation.target, operation.typeArguments)
            if (target === undefined) {
              return blockedStep({
                _tag: 'MissingFunction',
                target: operation.target,
                span: operation.provenance.span,
              })
            }
            trace.push(
              Object.freeze({
                _tag: 'Call',
                frame: activation.frame,
                depth: activation.depth,
                caller: fn.id,
                target: operation.target,
                callerInstance: fn.instance,
                targetInstance: target.instance,
                span: operation.provenance.span,
              }),
            )
            const argumentStates = operation.arguments.map((argument) => read(argument))
            argumentStates.forEach((argumentState, ordinal) => {
              trace.push(
                Object.freeze({
                  _tag: 'Binding',
                  frame: state.nextFrame,
                  depth: activation.depth + 1,
                  target: operation.target,
                  targetInstance: target.instance,
                  callSpan: operation.provenance.span,
                  argumentOrdinal: ordinal,
                  parameterOrdinal: ordinal,
                  value: argumentState.value,
                  fromCall: argumentState.fromCall,
                  span: operation.provenance.span,
                }),
              )
            })
            const result = yield* callFunction(
              target,
              argumentStates.map((state) => state.value),
              operation.provenance.span,
            )
            if (result._tag === 'Blocked') return result
            if (result._tag === 'Transfer') return result
            write(operation.destination, { value: result.value, fromCall: true })
            break
          }
        }
      }
      return undefined
    }
    const operations = region._tag === 'OperationRegion' ? region.operations : region.releases
    const operationStep = yield* executeOperations(operations)
    if (operationStep !== undefined) return operationStep
    const outcome = region.outcome
    switch (outcome._tag) {
      case 'Return': {
        const result = read(outcome.value)
        trace.push(
          Object.freeze({
            _tag: 'Return',
            frame: activation.frame,
            depth: activation.depth,
            function: fn.id,
            instance: fn.instance,
            value: result.value,
            span: outcome.provenance.span,
          }),
        )
        return Object.freeze({ _tag: 'Value', value: result.value })
      }
      case 'Forward':
        regionOrdinal = outcome.target.ordinal
        break
      case 'Repeat': {
        const loop = loops.get(outcome.loop.ordinal)
        if (loop === undefined) throw new RangeError('MIR verifier allowed a missing repeat loop')
        trace.push(
          Object.freeze({
            _tag: 'Repeat',
            function: fn.id,
            region: region.id.ordinal,
            loop: outcome.loop.ordinal,
            span: outcome.provenance.span,
          }),
        )
        regionOrdinal = loop.id.ordinal
        break
      }
      case 'Exit': {
        const loop = loops.get(outcome.loop.ordinal)
        if (loop === undefined) throw new RangeError('MIR verifier allowed a missing exit loop')
        trace.push(
          Object.freeze({
            _tag: 'Exit',
            function: fn.id,
            region: region.id.ordinal,
            loop: outcome.loop.ordinal,
            span: outcome.provenance.span,
          }),
        )
        regionOrdinal = loop.following.ordinal
        break
      }
      case 'Yield': {
        const loop = conditionOwners.get(region.id.ordinal)
        if (loop === undefined) throw new RangeError('MIR verifier allowed an unowned yield')
        const enter = readI32(loop.conditionValue).value !== 0
        trace.push(
          Object.freeze({
            _tag: enter ? 'Iteration' : 'Condition',
            function: fn.id,
            region: region.id.ordinal,
            loop: loop.loop.ordinal,
            span: outcome.provenance.span,
          }),
        )
        regionOrdinal = enter ? loop.body.ordinal : loop.following.ordinal
        break
      }
      case 'Trap':
        return blockedStep({
          _tag: 'Trap',
          function: fn.id,
          reason: outcome.reason,
          span: outcome.provenance.span,
        })
    }
  }
}

const argumentSpanFallback = (fn: Mir.MirFunction): SourceSpan.SourceSpan => {
  const region = fn.regions.find((candidate) => candidate.id.ordinal === fn.entry.ordinal)
  const span =
    region?._tag === 'ConditionalRegion' || region?._tag === 'LoopRegion'
      ? region.provenance.span
      : region?._tag === 'OperationRegion'
        ? (region.operations.at(0)?.provenance.span ?? region.outcome.provenance.span)
        : (region?.releases.at(0)?.provenance.span ?? region?.outcome.provenance.span)
  if (span === undefined) {
    throw new RangeError(`Lowered function ${fn.id.name} has no regions`)
  }
  return span
}

const firstFunctionSpan = (program: Mir.Module): SourceSpan.SourceSpan => {
  const first = program.functions.at(0)
  return first === undefined ? raiseNoSpan() : argumentSpanFallback(first)
}

const makeActivation = (
  program: Mir.Module,
  fn: Mir.MirFunction,
  arguments_: ReadonlyArray<Value>,
  depth: number,
  trace: Array<TraceEvent>,
  state: EvaluationState,
): ActivationRecord => {
  const frame = state.nextFrame
  state.nextFrame += 1
  const locals = new Map<number, LocalState>()
  arguments_.forEach((argument, ordinal) => {
    locals.set(ordinal, { value: argument, fromCall: false })
  })
  const activation: ActivationRecord = {
    frame,
    depth,
    function: fn.id,
    instance: fn.instance,
    locals,
    cells: state.cells,
  }
  trace.push(
    Object.freeze({
      _tag: 'Entry',
      frame,
      depth,
      function: fn.id,
      instance: fn.instance,
      span: argumentSpanFallback(fn),
    }),
  )
  activation.continuation = executeFunction(program, fn, activation, trace, state)
  return activation
}

const activeFrameSnapshot = (stack: ReadonlyArray<ActivationRecord>): ReadonlyArray<ActiveFrame> =>
  Object.freeze(
    stack.map(({ frame, depth, function: function_, instance }) =>
      Object.freeze({ frame, depth, function: function_, instance }),
    ),
  )

const suspensionSpan = (program: Mir.Module, point: Mir.SuspensionPointId): SourceSpan.SourceSpan =>
  program.functions
    .flatMap((fn) => fn.suspension?.regions ?? [])
    .find(
      (region) =>
        region.point.sourceId === point.sourceId &&
        region.point.spanStart === point.spanStart &&
        region.point.spanEnd === point.spanEnd &&
        region.point.ordinal === point.ordinal,
    )?.provenance.span ?? firstFunctionSpan(program)

const executeMachine = (
  program: Mir.Module,
  entry: Mir.MirFunction,
  trace: Array<TraceEvent>,
  state: EvaluationState,
): Step => {
  const stack: Array<ActivationRecord> = [makeActivation(program, entry, [], 1, trace, state)]
  let transfer: TransferContext | undefined
  let resumed: Step | undefined

  const localValue = (activation: ActivationRecord, local: Mir.LocalId): Value | undefined =>
    state.cells.get(cellKey(activation.frame, local.ordinal))?.value ??
    activation.locals.get(local.ordinal)?.value

  const providerInEffect = (
    effect: EffectValue,
    environment: Extract<Layout.EffectEnvironment, { readonly _tag: 'EffectEnvironment' }>,
    providerType: Type.Nominal,
    visited = new Set<string>(),
  ): Value | undefined => {
    const identity = Instances.effectIdentity(environment.instance, environment.site)
    if (visited.has(identity)) return undefined
    const nextVisited = new Set(visited).add(identity)
    let selected: Value | undefined
    for (const [ordinal, field] of environment.fields.entries()) {
      const capture = effect.captures.at(ordinal)
      if (capture === undefined) continue
      if (
        Type.isReference(field.type) &&
        field.type.access === 'Exclusive' &&
        Type.isNominal(field.type.target) &&
        Type.equals(field.type.target, providerType)
      )
        selected = capture
      if (field.effectIdentity !== undefined && capture._tag === 'EffectValue') {
        const nested = program.layout.effectEnvironments.find(
          (candidate) =>
            candidate._tag === 'EffectEnvironment' &&
            Instances.effectIdentity(candidate.instance, candidate.site) === field.effectIdentity,
        )
        if (nested?._tag === 'EffectEnvironment') {
          const nestedSelected = providerInEffect(capture, nested, providerType, nextVisited)
          if (nestedSelected !== undefined) selected = nestedSelected
        }
      }
    }
    return selected
  }

  const continuationAllocatorCall = (
    relay: ContinuationTransaction.Relay,
    activation: ActivationRecord,
  ):
    | {
        readonly constructor: Mir.MirFunction
        readonly arguments: ReadonlyArray<Value>
      }
    | undefined => {
    const provider = relay.descriptor.runner.providers.find(
      (candidate) => candidate.purposes.length === 2 && candidate.argument !== undefined,
    )
    if (provider?.argument === undefined || provider.witness?._tag !== 'SourceConformanceWitness')
      return undefined
    const argumentType =
      activation.function.module === relay.function.declaration.module
        ? program.functions
            .find(
              (candidate) =>
                Instances.keyText(candidate.instance) === Instances.keyText(relay.function),
            )
            ?.localTypes.at(provider.argument.ordinal)
        : undefined
    const argument = localValue(activation, provider.argument)
    const selectedProvider =
      argumentType?._tag === 'EffectValue' && argument?._tag === 'EffectValue'
        ? providerInEffect(argument, argumentType.environment, provider.providerType)
        : argument
    if (selectedProvider === undefined) return undefined
    const implementation = provider.witness.operations.find(
      (operation) => operation.name === 'allocate',
    )?.implementation
    if (implementation === undefined) return undefined
    const layoutConstructor = program.functions.find((candidate) => {
      const parameter = candidate.localTypes.at(0)
      return (
        candidate.id.module === implementation.module &&
        candidate.id.name === implementation.name &&
        parameter?._tag === 'Reference' &&
        Type.equals(parameter.type.target, provider.providerType) &&
        candidate.result._tag === 'EffectValue'
      )
    })
    const layoutEntry = program.layout.entries.find((candidate) =>
      Type.equals(candidate.type, Type.layout),
    )
    if (
      layoutConstructor === undefined ||
      layoutEntry?._tag !== 'LayoutEntry' ||
      layoutEntry.representation._tag !== 'Aggregate'
    )
      return undefined
    const request = relay.layout.acquisition.request
    const layoutValue: AggregateValue = Object.freeze({
      _tag: 'AggregateValue',
      type: Type.layout,
      fields: Object.freeze(
        layoutEntry.representation.fields.map((field) =>
          Object.freeze({
            field: field.id,
            value: usizeValue(BigInt(field.name === 'bytes' ? request.bytes : request.alignment)),
          }),
        ),
      ),
    })
    return Object.freeze({
      constructor: layoutConstructor,
      arguments: Object.freeze([selectedProvider, layoutValue]),
    })
  }

  const beginAllocatorPreparation = (context: TransferContext): boolean => {
    const relay = context.relays.at(context.allocations.length)
    if (relay === undefined) return false
    const pending = context.pending.find((candidate) => candidate.relay === relay)?.activation
    if (pending === undefined)
      throw new RangeError('Evaluator continuation relay lost its activation')
    const call = continuationAllocatorCall(relay, pending)
    if (call === undefined)
      throw new RangeError('Evaluator continuation allocator did not resolve to a selected witness')
    const activation = makeActivation(
      program,
      call.constructor,
      call.arguments,
      pending.depth + 1,
      trace,
      state,
    )
    context.preparation = {
      index: context.allocations.length,
      phase: 'Constructor',
      relay,
      pending,
      activation,
    }
    stack.push(activation)
    return true
  }

  const reclaimContinuationAllocation = (
    allocation: AllocationValue,
    function_: DeclarationIndex.CanonicalId,
    point: Mir.SuspensionPointId,
  ): void => {
    const ticket = state.allocations.get(allocation.ticket)
    if (ticket === undefined || !ticket.active)
      throw new RangeError('Evaluator continuation allocation was reclaimed more than once')
    ticket.active = false
    trace.push(
      Object.freeze({
        _tag: 'AllocationRelease',
        function: function_,
        ticket: allocation.ticket,
        span: suspensionSpan(program, point),
      }),
    )
  }

  const releaseActivationContinuation = (activation: ActivationRecord): void => {
    const point = activation.continuationPoint
    if (point === undefined) return
    if (activation.continuationTicket !== undefined)
      trace.push(
        Object.freeze({
          _tag: 'ContinuationRelease',
          function: activation.function,
          point,
          ticket: activation.continuationTicket,
          span: suspensionSpan(program, point),
        }),
      )
    if (activation.continuationAllocation !== undefined)
      reclaimContinuationAllocation(activation.continuationAllocation, activation.function, point)
    delete activation.continuationTicket
    delete activation.continuationPoint
    delete activation.continuationAllocation
  }

  const publishPreparedTransfer = (context: TransferContext): void => {
    const prepared = ContinuationTransaction.prepare(context.relays, undefined)
    if (prepared._tag !== 'Prepared')
      throw new RangeError('Evaluator continuation preparation unexpectedly refused')
    const tickets = new Map<number, number>()
    for (const event of prepared.events) {
      if (event._tag === 'Acquire') {
        const ticket = state.nextContinuation
        state.nextContinuation += 1
        tickets.set(event.ordinal, ticket)
      }
      if (
        event._tag !== 'Request' &&
        event._tag !== 'Acquire' &&
        event._tag !== 'EndAllocatorLoan' &&
        event._tag !== 'Initialize' &&
        event._tag !== 'Publish'
      )
        continue
      const tag =
        event._tag === 'Request'
          ? 'ContinuationRequest'
          : event._tag === 'Acquire'
            ? 'ContinuationAcquire'
            : event._tag === 'EndAllocatorLoan'
              ? 'ContinuationLoanEnd'
              : event._tag === 'Initialize'
                ? 'ContinuationInitialize'
                : 'ContinuationPublish'
      const ticket = tickets.get(event.ordinal)
      const relay = context.relays.at(event.ordinal - 1)
      const continuationEvent: ContinuationTraceEvent = Object.freeze({
        _tag: tag,
        function: relay?.function.declaration ?? context.step.child.target.id,
        point: event.point,
        ordinal: event.ordinal,
        ...(ticket === undefined ? {} : { ticket }),
        ...(event._tag === 'Request' ? { bytes: event.bytes, alignment: event.alignment } : {}),
        span: suspensionSpan(program, event.point),
      })
      trace.push(continuationEvent)
    }
    let relayOrdinal = 0
    for (const pending of context.pending) {
      if (pending.relay === undefined) continue
      relayOrdinal += 1
      // A resumed activation may relay another transfer before completing. The replacement frame
      // is now fully prepared, so its prior private storage no longer owns live payload and can be
      // reclaimed before the activation adopts the new continuation identity.
      releaseActivationContinuation(pending.activation)
      const ticket = tickets.get(relayOrdinal)
      if (ticket !== undefined) pending.activation.continuationTicket = ticket
      pending.activation.continuationPoint = pending.relay.descriptor.point
      const allocation = context.allocations.find(
        (candidate) => candidate.relay === pending.relay,
      )?.allocation
      if (allocation !== undefined) pending.activation.continuationAllocation = allocation
    }
    const origin = context.pending.at(0)?.activation
    if (origin === undefined)
      throw new RangeError('Evaluator transfer lost its originating activation')
    stack.push(
      makeActivation(
        program,
        context.step.child.target,
        context.step.child.arguments,
        origin.depth + 1,
        trace,
        state,
      ),
    )
    trace.push(
      Object.freeze({
        _tag: 'SuspensionChildStart',
        function: context.step.child.target.id,
        point: context.step.origin.point,
        span: suspensionSpan(program, context.step.origin.point),
      }),
    )
  }

  while (stack.length > 0) {
    const suspended: Array<ActivationRecord> = []
    for (let current = transfer; current !== undefined; current = current.parent)
      suspended.unshift(...[...current.pending].reverse().map((pending) => pending.activation))
    state.activeFrames = activeFrameSnapshot([...suspended, ...stack])
    const activation = stack.at(-1)
    if (activation?.continuation === undefined) {
      throw new RangeError('Evaluator activation lost its continuation')
    }
    const advanced =
      resumed === undefined ? activation.continuation.next() : activation.continuation.next(resumed)
    resumed = undefined
    if (advanced.done) {
      stack.pop()
      if (advanced.value._tag === 'Blocked') return advanced.value
      const preparation = transfer?.preparation
      if (preparation?.activation === activation) {
        if (preparation.phase === 'Constructor') {
          if (advanced.value._tag !== 'Value' || advanced.value.value._tag !== 'EffectValue')
            throw new RangeError('Evaluator continuation allocator constructor returned no Effect')
          const effect = advanced.value.value
          const runner = functionFor(program, effect.runner, effect.runnerTypeArguments)
          if (
            runner === undefined ||
            (runner.suspension?.classification ?? 'Synchronous') !== 'Synchronous'
          )
            throw new RangeError(
              'Evaluator continuation allocator runner is not closed synchronous',
            )
          const runnerActivation = makeActivation(
            program,
            runner,
            effect.captures,
            preparation.pending.depth + 1,
            trace,
            state,
          )
          preparation.phase = 'Runner'
          preparation.activation = runnerActivation
          stack.push(runnerActivation)
          continue
        }
        if (advanced.value._tag !== 'Value' || advanced.value.value._tag !== 'EffectOutcomeValue')
          throw new RangeError('Evaluator continuation allocator runner returned no typed outcome')
        const allocationOutcome = advanced.value.value
        if (allocationOutcome.tag === 0) {
          if (allocationOutcome.payload._tag !== 'AllocationValue')
            throw new RangeError('Evaluator continuation allocator success returned no Allocation')
          transfer?.allocations.push(
            Object.freeze({ relay: preparation.relay, allocation: allocationOutcome.payload }),
          )
          if (transfer !== undefined) delete transfer.preparation
          if (transfer !== undefined && beginAllocatorPreparation(transfer)) continue
          if (transfer === undefined)
            throw new RangeError('Evaluator continuation allocation lost its transfer')
          publishPreparedTransfer(transfer)
          continue
        }
        const context = transfer
        if (context === undefined)
          throw new RangeError('Evaluator continuation refusal lost its transfer')
        const refusalOrdinal = preparation.index + 1
        const refused = ContinuationTransaction.prepare(
          context.relays,
          allocationOutcome.payload,
          refusalOrdinal,
        )
        if (refused._tag !== 'Refused')
          throw new RangeError('Evaluator allocator refusal prepared a continuation')
        const tickets = new Map<number, number>()
        for (const event of refused.events) {
          if (event._tag === 'Acquire') {
            const relay = context.relays.at(event.ordinal - 1)
            const pending = context.pending.find(
              (candidate) => candidate.relay === relay,
            )?.activation
            const allocation = context.allocations.find(
              (candidate) => candidate.relay === relay,
            )?.allocation
            if (relay === undefined || pending === undefined || allocation === undefined)
              throw new RangeError('Evaluator accepted continuation lost its rollback owner')
            const ticket = state.nextContinuation
            state.nextContinuation += 1
            tickets.set(event.ordinal, ticket)
            pending.continuationTicket = ticket
            pending.continuationPoint = relay.descriptor.point
            pending.continuationAllocation = allocation
          }
          if (
            event._tag !== 'Request' &&
            event._tag !== 'Reject' &&
            event._tag !== 'Acquire' &&
            event._tag !== 'EndAllocatorLoan' &&
            event._tag !== 'Initialize'
          )
            continue
          const tag =
            event._tag === 'Request'
              ? 'ContinuationRequest'
              : event._tag === 'Reject'
                ? 'ContinuationReject'
                : event._tag === 'Acquire'
                  ? 'ContinuationAcquire'
                  : event._tag === 'EndAllocatorLoan'
                    ? 'ContinuationLoanEnd'
                    : 'ContinuationInitialize'
          trace.push(
            Object.freeze({
              _tag: tag,
              function:
                context.relays.at(event.ordinal - 1)?.function.declaration ??
                preparation.pending.function,
              point: event.point,
              ordinal: event.ordinal,
              ...(event._tag === 'Request'
                ? { bytes: event.bytes, alignment: event.alignment }
                : {}),
              ...(tickets.get(event.ordinal) === undefined
                ? {}
                : { ticket: tickets.get(event.ordinal) }),
              span: suspensionSpan(program, event.point),
            }) as ContinuationTraceEvent,
          )
        }
        // The intrinsic origin has no caller frame and must not be resumed with allocator failure.
        // Every relay activation, including already-initialized inner frames and the refusing
        // current activation, does resume through its ordinary typed-failure path. That executes
        // source cleanup before releaseActivationContinuation reclaims accepted private storage.
        const rollbackPending = context.pending.slice(1)
        const innermost = rollbackPending.at(0)
        const failureOutcome =
          innermost?.relay?.descriptor.runner.outcome ?? context.step.origin.deferred.outcome
        const failureTag = failureOutcome.failures.findIndex(
          (member) => member.module === 'silk/core' && member.name === 'OutOfMemory',
        )
        if (failureTag < 0)
          throw new RangeError('Evaluator continuation refusal has no OutOfMemory channel')
        const refusal: Step = Object.freeze({
          _tag: 'Value',
          value: Object.freeze({
            _tag: 'EffectOutcomeValue',
            type: failureOutcome,
            tag: failureTag + 1,
            payload: allocationOutcome.payload,
          }),
        })
        if (!rollbackPending.some((candidate) => candidate.activation === preparation.pending))
          throw new RangeError('Evaluator continuation refusal lost its pending activation')
        const survivors = rollbackPending.map((pending) => pending.activation).reverse()
        transfer = context.parent
        stack.push(...survivors)
        resumed = refusal
        continue
      }
      releaseActivationContinuation(activation)
      if (stack.length === 0 && transfer !== undefined) {
        let next = transfer.pending.shift()
        while (next === undefined && transfer.parent !== undefined) {
          transfer = transfer.parent
          next = transfer.pending.shift()
        }
        if (next !== undefined) {
          stack.push(next.activation)
          if (
            next.relay === undefined &&
            advanced.value._tag === 'Value' &&
            advanced.value.value._tag === 'EffectOutcomeValue'
          )
            trace.push(
              Object.freeze({
                _tag: 'SuspensionChildComplete',
                function: next.activation.function,
                point: transfer.step.origin.point,
                outcome: advanced.value.value.tag === 0 ? 'Success' : 'Failure',
                span: suspensionSpan(program, transfer.step.origin.point),
              }),
            )
          if (
            next.activation.continuationTicket !== undefined &&
            next.activation.continuationPoint !== undefined &&
            advanced.value._tag === 'Value' &&
            advanced.value.value._tag === 'EffectOutcomeValue'
          )
            trace.push(
              Object.freeze({
                _tag: 'ContinuationResume',
                function: next.activation.function,
                point: next.activation.continuationPoint,
                ticket: next.activation.continuationTicket,
                outcome: advanced.value.value.tag === 0 ? 'Success' : 'Failure',
                span: suspensionSpan(program, next.activation.continuationPoint),
              }),
            )
          resumed = advanced.value
          continue
        }
        transfer = undefined
        return advanced.value
      }
      if (stack.length === 0) return advanced.value
      resumed = advanced.value
      continue
    }
    const request = advanced.value
    if (request._tag === 'OriginTransferRequest') {
      const step: TransferStep = Object.freeze({
        _tag: 'Transfer',
        origin: request.origin,
        child: request.child,
      })
      transfer = {
        step,
        relays: [],
        pending: [Object.freeze({ activation })],
        allocations: [],
        ...(transfer === undefined ? {} : { parent: transfer }),
      }
      trace.push(
        Object.freeze({
          _tag: 'SuspensionOrigin',
          function: activation.function,
          point: request.origin.point,
          span: suspensionSpan(program, request.origin.point),
        }),
      )
      stack.pop()
      if (stack.length > 0) {
        resumed = step
        continue
      }
      publishPreparedTransfer(transfer)
      continue
    }
    if (request._tag === 'RelayTransferRequest') {
      if (transfer === undefined || transfer.step !== request.transfer)
        throw new RangeError('Evaluator relay lost its originating transfer')
      transfer.pending.push(
        Object.freeze({
          activation,
          ...(request.relay === undefined ? {} : { relay: request.relay }),
        }),
      )
      if (request.relay !== undefined) transfer.relays.push(request.relay)
      stack.pop()
      if (stack.length > 0) {
        resumed = transfer.step
        continue
      }
      if (!beginAllocatorPreparation(transfer)) publishPreparedTransfer(transfer)
      continue
    }
    let suspendedDepth = 0
    for (let current = transfer; current !== undefined; current = current.parent)
      suspendedDepth += current.pending.length
    if (stack.length + suspendedDepth >= state.maxCallDepth) {
      for (;;) {
        const attemptedBinding = trace.at(-1)
        if (attemptedBinding?._tag !== 'Binding' || attemptedBinding.frame !== state.nextFrame)
          break
        trace.pop()
      }
      const attemptedCall = trace.at(-1)
      if (
        attemptedCall?._tag === 'Call' &&
        attemptedCall.frame === activation.frame &&
        attemptedCall.target.module === request.target.id.module &&
        attemptedCall.target.name === request.target.id.name &&
        attemptedCall.span.start === request.span.start &&
        attemptedCall.span.end === request.span.end
      ) {
        trace.pop()
      }
      return blockedStep({
        _tag: 'EvaluationLimit',
        kind: 'CallDepth',
        limit: state.maxCallDepth,
        count: stack.length + suspendedDepth,
        function: activation.function,
        span: request.span,
        activeFrames: state.activeFrames,
      })
    }
    stack.push(
      makeActivation(
        program,
        request.target,
        request.arguments,
        activation.depth + 1,
        trace,
        state,
      ),
    )
  }
  throw new RangeError('Evaluator activation machine stopped without a result')
}

export const defaultMaxSteps = 1_000_000
export const defaultMaxCallDepth = 1_024

const evaluationLimitOption = (
  name: string,
  value: number | undefined,
  fallback: number,
): number => {
  const selected = value ?? fallback
  if (!Number.isSafeInteger(selected) || selected <= 0) {
    throw new RangeError(`${name} must be a positive safe integer`)
  }
  return selected
}

/** Explicit host services available to one deterministic evaluation. */
export interface Options {
  readonly standardStreams?: StandardStreams.Provider
  readonly standardInput?: StandardInput.Provider
  readonly childProcess?: ChildProcess.Provider
  readonly hostInput?: HostInput.Provider
  readonly osFileSystem?: OsFileSystemHost.Provider
  readonly maxSteps?: number
  readonly maxCallDepth?: number
}

/** Executes the lowered program from the discovered entry, replaying MIR operations as a trace. */
export const evaluate = (
  discovery: Instances.Discovery,
  program: Mir.Module,
  options: Options = {},
): Outcome => {
  const maxSteps = evaluationLimitOption('maxSteps', options.maxSteps, defaultMaxSteps)
  const maxCallDepth = evaluationLimitOption(
    'maxCallDepth',
    options.maxCallDepth,
    defaultMaxCallDepth,
  )
  const availability = IntrinsicAvailability.select(program.intrinsics, 'Evaluator')
  if (availability._tag === 'Unavailable') {
    return Object.freeze({
      _tag: 'Blocked',
      entry: discovery.entry._tag === 'Resolved' ? discovery.entry.key.declaration : undefined,
      reason: Object.freeze({
        _tag: 'IntrinsicTargetUnavailable',
        diagnostics: availability.diagnostics,
      }),
      trace: Object.freeze([]),
    })
  }
  if (discovery.entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'Blocked',
      entry: undefined,
      reason: Object.freeze({ _tag: 'UnavailableEntry', reason: discovery.entry.reason }),
      trace: Object.freeze([]),
    })
  }
  const violations = Mir.verify(program)
  if (violations.length > 0) {
    return Object.freeze({
      _tag: 'Blocked',
      entry: discovery.entry._tag === 'Resolved' ? discovery.entry.key.declaration : undefined,
      reason: Object.freeze({ _tag: 'InvalidMir', violations }),
      trace: Object.freeze([]),
    })
  }
  const machine = Mir.machineEntry(program)
  const entry = machine.declaration
  const fn = functionFor(program, entry, machine.typeArguments)
  if (fn === undefined) {
    return Object.freeze({
      _tag: 'Blocked',
      entry,
      reason: Object.freeze({
        _tag: 'MissingFunction',
        target: entry,
        span: firstFunctionSpan(program),
      }),
      trace: Object.freeze([]),
    })
  }

  const trace: Array<TraceEvent> = []
  const result = executeMachine(program, fn, trace, {
    nextFrame: 0,
    nextAllocation: 0,
    nextCallable: 0,
    nextContinuation: 0,
    steps: 0,
    maxSteps,
    maxCallDepth,
    activeFrames: Object.freeze([]),
    cells: new Map(),
    allocations: new Map(),
    callables: new Map(),
    activeLoans: new Set(),
    stringLoans: new Set(),
    ...(options.standardStreams === undefined ? {} : { standardStreams: options.standardStreams }),
    ...(options.standardInput === undefined ? {} : { standardInput: options.standardInput }),
    ...(options.childProcess === undefined ? {} : { childProcess: options.childProcess }),
    processCaptures: [Object.freeze([]), Object.freeze([])],
    ...(options.hostInput === undefined ? {} : { hostInput: options.hostInput }),
    ...(options.osFileSystem === undefined ? {} : { osFileSystem: options.osFileSystem }),
  })
  if (result._tag === 'Blocked') {
    return Object.freeze({
      _tag: 'Blocked',
      entry,
      reason: result.reason,
      trace: Object.freeze([...trace]),
    })
  }
  if (result._tag === 'Transfer')
    throw new RangeError('Bootstrap evaluator returned a private suspension transfer')
  if (result.value._tag !== 'I32Value') {
    throw new RangeError('Bootstrap entry returned a non-i32 value')
  }
  const status = result.value
  if (program.entry._tag === 'EffectEntry' && status.value !== 0) {
    const failure = program.entry.failures.find((candidate) => candidate.tag === status.value)
    if (failure === undefined) {
      return Object.freeze({
        _tag: 'Blocked',
        entry,
        reason: Object.freeze({
          _tag: 'Trap',
          function: entry,
          reason: `effect entry returned invalid failure tag ${status.value}`,
          span: argumentSpanFallback(fn),
        }),
        trace: Object.freeze([...trace]),
      })
    }
    return Object.freeze({
      _tag: 'UnhandledFailure',
      entry,
      tag: failure.tag,
      report: failure.report,
      trace: Object.freeze([...trace]),
    })
  }
  return Object.freeze({
    _tag: 'Completed',
    entry,
    result: result.value,
    trace: Object.freeze([...trace]),
  })
}

const raiseNoSpan = (): never => {
  throw new RangeError('Lowered program has no functions to attach a span to')
}
