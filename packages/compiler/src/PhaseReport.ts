/** One compiler or artifact-production phase's operational observation. */
export interface PhaseReport {
  readonly phase: string
  readonly elapsedMs: number
  readonly inputs: number
  readonly outputs: number
  readonly diagnostics: number
  readonly heapBytes?: number
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
