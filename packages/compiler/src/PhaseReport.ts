/** Deterministic semantic-revision counts attached to the invalidation phase. */
export interface SemanticInvalidationCounters {
  readonly _tag: 'SemanticInvalidationCounters'
  readonly reusable: number
  readonly recomputed: number
  readonly fresh: number
  readonly localChange: number
  readonly opaqueBodyChange: number
  readonly opaqueTargetChange: number
  readonly opaqueLayoutChange: number
  readonly dependencySurfaceChange: number
  readonly cyclicPeerChange: number
  readonly environmentChange: number
  readonly surfaceChange: number
}

/** Deterministic module-work counts attached to incrementally reusable semantic phases. */
export interface ModuleReuseCounters {
  readonly _tag: 'ModuleReuseCounters'
  readonly reused: number
  readonly recomputed: number
}

export type Counters = SemanticInvalidationCounters | ModuleReuseCounters

/** One compiler or artifact-production phase's operational observation. */
export interface PhaseReport {
  readonly phase: string
  readonly elapsedMs: number
  readonly inputs: number
  readonly outputs: number
  readonly diagnostics: number
  readonly heapBytes?: number
  readonly counters?: Counters
}

/** One measured value paired with the phase observation produced while computing it. */
export interface Measured<A> {
  readonly value: A
  readonly report: PhaseReport
}

/** Constructs one immutable phase observation. */
export const make = (options: {
  readonly phase: string
  readonly elapsedMs: number
  readonly inputs: number
  readonly outputs: number
  readonly diagnostics: number
  readonly heapBytes?: number
  readonly counters?: Counters
}): PhaseReport => Object.freeze({ ...options })

/** Measures one synchronous compiler phase without making its deterministic result time-dependent. */
export const measure = <A>(
  phase: string,
  inputs: number,
  run: () => A,
  outputs: (value: A) => number,
  diagnostics: (value: A) => number = () => 0,
  heapBytes?: () => number,
): Measured<A> => {
  const startedAt = performance.now()
  const value = run()
  const observedHeap = heapBytes?.()
  return Object.freeze({
    value,
    report: make({
      phase,
      elapsedMs: performance.now() - startedAt,
      inputs,
      outputs: outputs(value),
      diagnostics: diagnostics(value),
      ...(observedHeap === undefined ? {} : { heapBytes: observedHeap }),
    }),
  })
}
