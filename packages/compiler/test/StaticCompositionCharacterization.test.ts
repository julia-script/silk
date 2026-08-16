import { NodeServices } from '@effect/platform-node'
import { assert, layer } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Path from 'effect/Path'
import * as Schema from 'effect/Schema'
import * as Process from './support/Process.js'

const Environment = Schema.Struct({
  platform: Schema.String,
  arch: Schema.String,
  node: Schema.String,
})

const Measurement = Schema.Struct({
  shape: Schema.Literals(['left', 'balanced']),
  size: Schema.Number,
  canonicalBytes: Schema.Number,
  canonicalSha256: Schema.String,
  semanticOccurrences: Schema.Number,
  representations: Schema.Number,
  instances: Schema.Number,
  layouts: Schema.Number,
  mirFunctions: Schema.Number,
  mirOperations: Schema.Number,
  peakHeapBytes: Schema.Number,
  nativeBytes: Schema.Number,
  nativeSha256: Schema.String,
  wasmBytes: Schema.Number,
  wasmSha256: Schema.String,
})
type Measurement = typeof Measurement.Type

const Report = Schema.Struct({
  schema: Schema.Literal(1),
  environment: Environment,
  elapsedMs: Schema.Number,
  reports: Schema.Array(Measurement),
})

const decodeReport = Schema.decodeUnknownEffect(Schema.fromJsonString(Report))

const deterministic = (measurement: Measurement) =>
  Object.freeze({
    shape: measurement.shape,
    size: measurement.size,
    canonicalBytes: measurement.canonicalBytes,
    canonicalSha256: measurement.canonicalSha256,
    semanticOccurrences: measurement.semanticOccurrences,
    representations: measurement.representations,
    instances: measurement.instances,
    layouts: measurement.layouts,
    mirFunctions: measurement.mirFunctions,
    mirOperations: measurement.mirOperations,
    nativeBytes: measurement.nativeBytes,
    nativeSha256: measurement.nativeSha256,
    wasmBytes: measurement.wasmBytes,
    wasmSha256: measurement.wasmSha256,
  })

layer(NodeServices.layer)('static composition characterization', (it) => {
  it.effect(
    'keeps two fresh-process static-tree artifacts deterministic and semantic growth linear',
    () =>
      Effect.gen(function* () {
        const path = yield* Path.Path
        const script = yield* path.fromFileUrl(
          new URL('./characterization/static-composition/characterize.mjs', import.meta.url),
        )
        const firstProcess = yield* Process.run(process.execPath, [script])
        const secondProcess = yield* Process.run(process.execPath, [script])
        assert.strictEqual(firstProcess.exitCode, 0, firstProcess.stderr)
        assert.strictEqual(secondProcess.exitCode, 0, secondProcess.stderr)
        const first = yield* decodeReport(firstProcess.stdout)
        const second = yield* decodeReport(secondProcess.stdout)

        assert.deepEqual(first.environment, second.environment)
        assert.strictEqual(first.reports.length, 10)
        assert.deepEqual(first.reports.map(deterministic), second.reports.map(deterministic))
        assert.deepEqual(
          first.reports.map(({ shape, size }) => `${shape}:${size}`),
          [
            'left:1',
            'left:8',
            'left:32',
            'left:64',
            'left:128',
            'balanced:1',
            'balanced:8',
            'balanced:32',
            'balanced:64',
            'balanced:128',
          ],
        )
        for (const measurement of first.reports) {
          // The first empirical threshold intentionally covers semantic facts only. Source bytes,
          // code-generation time, memory, and target size remain recorded trend data.
          assert.isAtMost(measurement.semanticOccurrences, measurement.size * 14)
          assert.isAtMost(measurement.layouts, measurement.size * 2)
          assert.isAtMost(measurement.mirOperations, measurement.size * 4 + 2)
          assert.isAbove(measurement.nativeBytes, 0)
          assert.isAbove(measurement.wasmBytes, 0)
        }
      }),
    600_000,
  )
})
