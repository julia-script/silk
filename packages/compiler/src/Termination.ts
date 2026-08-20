import type * as DeclarationIndex from './DeclarationIndex.js'
import type * as SourceSpan from './SourceSpan.js'

/** One source-level invocation retained independently from physical backend and coroutine frames. */
export interface LogicalFrame {
  readonly function: DeclarationIndex.CanonicalId
  readonly provenance: SourceSpan.SourceSpan
}

/** One typed failure observed while producing a terminal outcome. */
export interface CausalFailure {
  readonly tag: number
  readonly identity?: string
  readonly provenance: SourceSpan.SourceSpan
  readonly logicalPath: ReadonlyArray<LogicalFrame>
  readonly recovered: boolean
}

interface Common<Trace> {
  readonly entry: DeclarationIndex.CanonicalId
  readonly status: number
  readonly provenance: SourceSpan.SourceSpan
  readonly logicalPath: ReadonlyArray<LogicalFrame>
  readonly history: ReadonlyArray<CausalFailure>
  readonly trace: ReadonlyArray<Trace>
}

/** Successful target-neutral completion. */
export interface Completed<Value, Trace> extends Common<Trace> {
  readonly _tag: 'Completed'
  readonly classification: 'Success'
  readonly result: Value
}

/** An owned typed failure that escaped the entry Effect. */
export interface UnhandledFailure<Trace> extends Common<Trace> {
  readonly _tag: 'UnhandledFailure'
  readonly classification: 'TypedFailure'
  readonly status: 1
  readonly tag: number
  readonly identity: string
}

/** Fatal abnormal termination, outside typed recovery and source cleanup guarantees. */
export interface Trap<Trace> extends Common<Trace> {
  readonly _tag: 'Trap'
  readonly classification: 'Trap'
  readonly status: 2
  readonly reason: string
}

/** Closed target-neutral runtime outcome shared by evaluators, adapters, and embedding hosts. */
export type Outcome<Value, Trace = never> =
  | Completed<Value, Trace>
  | UnhandledFailure<Trace>
  | Trap<Trace>

/** Static adapter data needed to interpret one target's private scalar entry ABI. */
export interface Contract {
  readonly _tag: 'EntryTermination'
  readonly success: 'Zero' | 'ReturnedStatus'
  readonly failures: ReadonlyArray<{
    readonly tag: number
    readonly identity: string
  }>
  readonly logicalFrames: ReadonlyArray<LogicalFrame>
}
