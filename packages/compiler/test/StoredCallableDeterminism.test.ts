import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Path from 'effect/Path'
import * as Schema from 'effect/Schema'
import * as Process from './support/Process.js'

/**
 * SEM0103 is part of the frontend's stable contract, so its reports must be byte-identical across
 * fresh processes: same codes, messages, spans, related provenance, and ordering. The fixture
 * covers a nested callable-bearing aggregate and one generic wrapper specialized twice, which is
 * where an unstable iteration order or presentation-dependent key would first show up.
 */

const SourceSpan = Schema.Struct({
  sourceId: Schema.String,
  start: Schema.Finite,
  end: Schema.Finite,
})
const Report = Schema.Struct({
  diagnostics: Schema.Array(
    Schema.Struct({
      code: Schema.String,
      message: Schema.String,
      span: SourceSpan,
      related: Schema.Array(
        Schema.Struct({
          label: Schema.String,
          sourceId: Schema.String,
          start: Schema.Finite,
          end: Schema.Finite,
        }),
      ),
    }),
  ),
  layout: Schema.String,
  mir: Schema.String,
})
const Result = Schema.Union([Schema.Finite, Schema.String])
const RuntimeReport = Schema.Struct({
  diagnostics: Schema.Array(Schema.String),
  evaluator: Result,
  wasm: Result,
  native: Schema.Finite,
  instanceKeys: Schema.Array(Schema.String),
  nativeLayout: Schema.String,
  wasmLayout: Schema.String,
  nativeMir: Schema.String,
  wasmMir: Schema.String,
  nativeSymbols: Schema.Array(Schema.String),
  wasmSymbols: Schema.Array(Schema.String),
  nativeArtifact: Schema.String,
  wasmArtifact: Schema.String,
  directWasm: Schema.Boolean,
})

const decodeReport = Schema.decodeUnknownEffect(Schema.fromJsonString(Report))
const decodeRuntimeReport = Schema.decodeUnknownEffect(Schema.fromJsonString(RuntimeReport))

it.effect('keeps SEM0103 reports byte-identical across fresh processes', () =>
  Effect.gen(function* () {
    const path = yield* Path.Path
    const fixture = yield* path.fromFileUrl(
      new URL('./fixtures/stored-callable-determinism.mjs', import.meta.url),
    )
    const first = yield* Process.run(process.execPath, [fixture])
    const second = yield* Process.run(process.execPath, [fixture])

    assert.strictEqual(first.exitCode, 0, first.stderr)
    assert.strictEqual(second.exitCode, 0, second.stderr)
    assert.strictEqual(first.stdout, second.stdout)

    const report = yield* decodeReport(first.stdout)
    // Two declared-field violations and two specializations of one generic wrapper.
    assert.deepEqual(
      report.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0103', 'SEM0103', 'SEM0103', 'SEM0103'],
      report.diagnostics.map((diagnostic) => diagnostic.message).join('\n'),
    )
    // Merge order is span order: report ordering is a published fact, not an accident.
    const positions = report.diagnostics.map((diagnostic) => diagnostic.span.start)
    assert.deepEqual(
      [...positions].sort((left, right) => left - right),
      positions,
    )
    // Both wrapper specializations report at their own call sites in the user source.
    for (const diagnostic of report.diagnostics) {
      assert.strictEqual(diagnostic.span.sourceId, 'fixture/StoredCallable')
    }
    const related = report.diagnostics.flatMap((diagnostic) => diagnostic.related)
    assert.deepEqual(
      related.map((span) => span.label),
      ['constructed here', 'constructed here'],
    )
    // The fence holds in the same breath: no layout or MIR is realized for the rejected program.
    assert.strictEqual(report.layout, 'Unavailable')
    assert.strictEqual(report.mir, 'Unavailable')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)

it.effect(
  'keeps accepted stored-callable facts and artifacts byte-identical across fresh processes',
  () =>
    Effect.gen(function* () {
      const path = yield* Path.Path
      const fixture = yield* path.fromFileUrl(
        new URL('./fixtures/stored-callable-runtime-determinism.mjs', import.meta.url),
      )
      const first = yield* Process.run(process.execPath, [fixture])
      const second = yield* Process.run(process.execPath, [fixture])

      assert.strictEqual(first.exitCode, 0, first.stderr)
      assert.strictEqual(second.exitCode, 0, second.stderr)
      assert.strictEqual(first.stdout, second.stdout)
      const report = yield* decodeRuntimeReport(first.stdout)
      assert.deepEqual(report.diagnostics, [])
      assert.strictEqual(report.evaluator, 42)
      assert.strictEqual(report.wasm, 42)
      assert.strictEqual(report.native, 42)
      assert.strictEqual(report.directWasm, true)
      assert.include(report.instanceKeys.join('\n'), 'target=declaration:silk/i32.add')
      assert.include(
        report.nativeSymbols.join('\n'),
        'silk_fixture_stored_callable_runtime_determinism_box__',
      )
      assert.deepEqual(report.nativeSymbols, report.wasmSymbols)
      for (const digest of [
        report.nativeLayout,
        report.wasmLayout,
        report.nativeMir,
        report.wasmMir,
        report.nativeArtifact,
        report.wasmArtifact,
      ])
        assert.strictEqual(digest.length, 64)
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
  300_000,
)
