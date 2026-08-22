import * as Context from 'effect/Context'

/** Replaceable heap telemetry used by the driver without depending on a host runtime. */
export interface Service {
  readonly heapBytes: () => number
}

/** Browser-safe default; Node applications provide `NodeHeapObservation.layer`. */
export const HeapObservation = Context.Reference<Service>('@silk-effect/compiler/HeapObservation', {
  defaultValue: () => Object.freeze({ heapBytes: () => 0 }),
})
