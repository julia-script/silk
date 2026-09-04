import type * as DeclarationFacts from './DeclarationFacts.js'
import type * as SourceSpan from './SourceSpan.js'

/** One source-level invocation retained independently from physical backend and coroutine frames. */
export interface LogicalFrame {
  readonly function: DeclarationFacts.CanonicalId
  readonly provenance: SourceSpan.SourceSpan
}

/**
 * Pre-rendered host-report text a standalone adapter prints: one label per logical frame ordinal,
 * one origin per statically known failure or trap site. Empty when the target has no host report.
 */
export interface Report {
  readonly frames: ReadonlyArray<string>
  readonly failureSites: ReadonlyArray<{ readonly identity: string; readonly origin: string }>
  readonly trapSites: ReadonlyArray<{ readonly reason: string; readonly origin: string }>
}

export const emptyReport: Report = Object.freeze({
  frames: Object.freeze([]),
  failureSites: Object.freeze([]),
  trapSites: Object.freeze([]),
})

/** Static adapter data needed to interpret one target's private scalar entry ABI. */
export interface Contract {
  readonly _tag: 'EntryTermination'
  readonly success: 'Zero' | 'ReturnedStatus'
  readonly failures: ReadonlyArray<{
    readonly tag: number
    readonly identity: string
  }>
  readonly logicalFrames: ReadonlyArray<LogicalFrame>
  readonly report: Report
}
