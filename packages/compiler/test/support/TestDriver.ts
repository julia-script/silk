import * as Effect from 'effect/Effect'
import * as Driver from '../../src/Driver.js'
import * as HeapObservation from '../../src/HeapObservation.js'

export type {
  BackendFailed,
  Compiled,
  CompileRequest,
  DriverPhaseReport,
  NoEntry,
  Outcome,
  Rejected,
  SourceResolutionFailed,
  TargetFailed,
  ToolchainFailed,
} from '../../src/Driver.js'

/** Test application edge with deterministic heap telemetry. */
export const compile = Effect.fnUntraced(function* (request: Driver.CompileRequest) {
  return yield* Driver.compile(request).pipe(Effect.provide(HeapObservation.layerTest))
})
