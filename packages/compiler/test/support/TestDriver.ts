import * as Effect from 'effect/Effect'
import * as Driver from '../../src/Driver.js'
import * as HeapObservation from '../../src/HeapObservation.js'

export type {
  BackendFailed,
  Compiled,
  DriverPhaseReport,
  NoEntry,
  Outcome,
  Rejected,
  SourceResolutionFailed,
  TargetFailed,
  ToolchainFailed,
} from '../../src/Driver.js'

export type CompileRequest = Omit<Driver.CompileRequest, 'packageName'> & {
  readonly packageName?: string
}

/** Test application edge with deterministic heap telemetry. */
export const compile = Effect.fnUntraced(function* (request: CompileRequest) {
  return yield* Driver.compile({
    ...request,
    packageName: request.packageName ?? 'compiler-test',
  }).pipe(Effect.provide(HeapObservation.layerTest))
})
