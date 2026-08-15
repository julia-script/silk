import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Path from 'effect/Path'
import * as Schema from 'effect/Schema'
import * as Process from './support/Process.js'

const Report = Schema.Struct({
  family: Schema.String,
  publicSignature: Schema.Struct({
    bound: Schema.String,
    result: Schema.String,
    enclosingKinds: Schema.Array(Schema.String),
  }),
  publicSurface: Schema.String,
  privateDefinition: Schema.Struct({
    key: Schema.String,
    target: Schema.String,
    body: Schema.String,
    layout: Schema.String,
    captures: Schema.Array(
      Schema.Struct({ ordinal: Schema.Number, type: Schema.String, access: Schema.String }),
    ),
  }),
  invalidation: Schema.Struct({
    observations: Schema.Array(
      Schema.Union([
        Schema.Struct({
          _tag: Schema.Literal('Reusable'),
          module: Schema.String,
          surfaceChanged: Schema.Boolean,
        }),
        Schema.Struct({
          _tag: Schema.Literal('Recomputed'),
          module: Schema.String,
          reasons: Schema.Array(Schema.String),
          surfaceChanged: Schema.Boolean,
        }),
      ]),
    ),
  }),
})

const decode = Schema.decodeUnknownEffect(Schema.fromJsonString(Report))

it.effect('keeps generic opaque facts byte-identical across fresh processes', () =>
  Effect.gen(function* () {
    const path = yield* Path.Path
    const fixture = yield* path.fromFileUrl(
      new URL('./fixtures/opaque-realization-determinism.mjs', import.meta.url),
    )
    const first = yield* Process.run(process.execPath, [fixture])
    const second = yield* Process.run(process.execPath, [fixture])
    assert.strictEqual(first.exitCode, 0, first.stderr)
    assert.strictEqual(second.exitCode, 0, second.stderr)
    assert.strictEqual(first.stdout, second.stdout)
    const report = yield* decode(first.stdout)
    assert.include(report.family, 'OpaqueRepresentation')
    assert.include(report.family, 'fixture/opaque_library')
    assert.include(report.family, 'make')
    assert.deepEqual(report.publicSignature.enclosingKinds, ['Value'])
    assert.notInclude(report.publicSurface, 'exact-representation:')
    assert.notInclude(report.publicSurface, 'OpaqueCapture')
    assert.include(report.privateDefinition.target, 'exact-representation:')
    assert.strictEqual(report.privateDefinition.captures.length, 1)
    const importer = report.invalidation.observations.find(
      (observation) => observation.module === 'fixture/opaque_application',
    )
    assert.strictEqual(importer?._tag, 'Reusable')
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
