import type { IntegerValue, StringStorage, Value } from './BootstrapValue.js'
import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as Diagnostic from './Diagnostic.js'
import type * as Instances from './Instances.js'
import type * as Intrinsic from './Intrinsic.js'
import type * as Match from './Match.js'
import type * as Mir from './Mir.js'
import type * as OsFileSystemHost from './OsFileSystemHost.js'
import type * as SourceSpan from './SourceSpan.js'
import type * as StandardStreams from './StandardStreams.js'
import type * as Termination from './Termination.js'
import type * as Type from './Type.js'

export interface ActiveFrame {
  readonly frame: number
  readonly depth: number
  readonly function: DeclarationIndex.CanonicalId
  readonly instance: Instances.InstanceKey
}

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
  readonly source: Type.Type
  readonly target: Type.StructuralUnion
  readonly member: Type.Type
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
  readonly members?: ReadonlyArray<Type.Type>
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
  readonly member: Type.Type
  readonly access: Match.Access
  readonly binding?: number
  readonly path?: ReadonlyArray<DeclarationIndex.FieldId>
  readonly value?: Value
  readonly members?: ReadonlyArray<Type.Type>
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
  readonly members?: ReadonlyArray<Type.Type>
  readonly span: SourceSpan.SourceSpan
}

export interface EffectTraceEvent {
  readonly _tag: 'EffectSuccess' | 'EffectFailure'
  readonly phase: 'Produced' | 'Propagated' | 'Closed'
  readonly identity?: string
  readonly frame: number
  readonly depth: number
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

export interface CoroutineFrameTraceEvent {
  readonly _tag:
    | 'SuspensionOrigin'
    | 'CoroutineFramePush'
    | 'CoroutineFrameStateTransition'
    | 'CoroutineFrameResume'
    | 'CoroutineFrameComplete'
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
  readonly cause?: unknown
  readonly span: SourceSpan.SourceSpan
}

/** One observable native OS boundary result, including an arbitrary thrown provider cause. */
export interface OsCallTraceEvent {
  readonly _tag: 'OsCall'
  readonly function: DeclarationIndex.CanonicalId
  readonly operation: Intrinsic.OperationId
  readonly outcome: 'Completed' | 'Failure'
  readonly reason?: OsFileSystemHost.Reason
  readonly nativeCode?: number
  readonly cause?: unknown
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
  | CoroutineFrameTraceEvent
  | StandardStreamTraceEvent
  | OsCallTraceEvent
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
export type Completed = Termination.Completed<IntegerValue, TraceEvent>

/** An owned typed application failure closed by the generated effect-entry adapter. */
export type UnhandledFailure = Termination.UnhandledFailure<TraceEvent>

/** Fatal abnormal termination produced by executable MIR. */
export type Trap = Termination.Trap<TraceEvent>

/** A normal, inspectable reason bootstrap evaluation could not complete. */
export interface Blocked {
  readonly _tag: 'Blocked'
  readonly entry: DeclarationIndex.CanonicalId | undefined
  readonly reason: BlockedReason
  readonly trace: ReadonlyArray<TraceEvent>
}

/** The closed outcome of executing one lowered program. */
export type Outcome = Completed | UnhandledFailure | Trap | Blocked
