import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Path from 'effect/Path'
import * as Schema from 'effect/Schema'
import * as Process from './support/Process.js'

const Diagnostic = Schema.Struct({ code: Schema.String })
const RepresentationFieldPlan = Schema.Struct({ field: Schema.String })
const ResolvedRepresentationField = Schema.Struct({
  field: Schema.String,
  key: Schema.String,
  argument: Schema.String,
  requiredBound: Schema.String,
  admissibility: Schema.String,
})
const UnavailableRepresentationField = Schema.Struct({
  field: Schema.String,
  key: Schema.String,
  requiredBound: Schema.String,
  reason: Schema.String,
})
const ExecutableIdentityFact = Schema.Struct({
  nominal: Schema.String,
  field: Schema.String,
  argument: Schema.String,
})
const RuntimeIdentityFacts = Schema.Struct({
  callables: Schema.Array(Schema.String),
  effects: Schema.Array(Schema.String),
  runners: Schema.Array(Schema.String),
})
const RepresentationReport = Schema.Struct({
  semantic: Schema.String,
  hir: Schema.String,
  instances: Schema.Array(
    Schema.Struct({
      declaration: Schema.Struct({ name: Schema.String }),
      arguments: Schema.Array(Schema.String),
    }),
  ),
  presentation: Schema.String,
  diagnostics: Schema.Array(Diagnostic),
  fences: Schema.Struct({
    diagnostics: Schema.Array(Diagnostic),
    layout: Schema.String,
    mir: Schema.String,
  }),
  representationFields: Schema.Struct({
    plans: Schema.Array(RepresentationFieldPlan),
    resolved: Schema.Array(ResolvedRepresentationField),
    unavailable: Schema.Array(UnavailableRepresentationField),
  }),
  identityStability: Schema.Struct({
    baseline: Schema.Array(ExecutableIdentityFact),
    shifted: Schema.Array(ExecutableIdentityFact),
  }),
  runtimeIdentityStability: Schema.Struct({
    baseline: RuntimeIdentityFacts,
    shifted: RuntimeIdentityFacts,
  }),
})

const decodeReport = Schema.decodeUnknownEffect(Schema.fromJsonString(RepresentationReport))

it.effect('keeps representation facts byte-identical across fresh processes', () =>
  Effect.gen(function* () {
    const path = yield* Path.Path
    const fixture = yield* path.fromFileUrl(
      new URL('./fixtures/representation-determinism.mjs', import.meta.url),
    )
    const first = yield* Process.run(process.execPath, [fixture])
    const second = yield* Process.run(process.execPath, [fixture])
    assert.strictEqual(first.exitCode, 0, first.stderr)
    assert.strictEqual(second.exitCode, 0, second.stderr)
    assert.strictEqual(first.stdout, second.stdout)
    const encoded = yield* decodeReport(first.stdout)

    assert.include(encoded.semantic, 'exact-representation:callable-identity:')
    assert.include(encoded.hir, 'typeof(fixture/representation-determinism.decode)')
    assert.strictEqual(
      encoded.instances.some(
        (instance) =>
          instance.declaration.name === 'consume' &&
          instance.arguments.some((argument) =>
            argument.includes('exact-representation:callable-identity:'),
          ),
      ),
      true,
    )
    assert.strictEqual(
      encoded.presentation,
      'let parser: Parser<i32, typeof(fixture/representation-determinism.decode)>',
    )
    assert.deepEqual(
      encoded.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0104', 'SEM0105', 'SEM0106', 'SEM0106'],
    )
    assert.deepEqual(
      encoded.fences.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0107'],
    )
    assert.strictEqual(encoded.fences.layout, 'Unavailable')
    assert.strictEqual(encoded.fences.mir, 'Unavailable')
    assert.strictEqual(encoded.representationFields.plans.length, 10)
    assert.strictEqual(encoded.representationFields.resolved.length, 10)
    assert.strictEqual(encoded.representationFields.unavailable.length, 10)
    assert.strictEqual(
      encoded.representationFields.plans.filter((plan) =>
        plan.field.includes('MultipleOuter:field:0:'),
      ).length,
      2,
    )
    assert.strictEqual(
      encoded.representationFields.plans.filter((plan) =>
        plan.field.includes('MultipleUnion:field:0:'),
      ).length,
      2,
    )
    assert.strictEqual(
      encoded.representationFields.resolved.every(
        (field) =>
          field.key.includes(field.field) &&
          field.argument.includes('exact-representation:') &&
          field.requiredBound.length > 0 &&
          field.admissibility === 'Admitted',
      ),
      true,
    )
    assert.strictEqual(
      encoded.representationFields.unavailable.every(
        (field) =>
          field.key.includes(field.field) &&
          field.requiredBound.length > 0 &&
          field.reason === 'OpenRepresentationArgument',
      ),
      true,
    )
    assert.deepEqual(encoded.identityStability.baseline, encoded.identityStability.shifted)
    assert.strictEqual(encoded.identityStability.baseline.length, 4)
    assert.strictEqual(
      encoded.identityStability.baseline.every(
        (fact) =>
          fact.nominal.includes('exact-representation:') &&
          fact.field.includes(fact.nominal) &&
          fact.argument.includes('exact-representation:'),
      ),
      true,
    )
    assert.strictEqual(
      new Set(encoded.identityStability.baseline.map((fact) => fact.argument)).size,
      4,
    )
    assert.deepEqual(
      encoded.runtimeIdentityStability.baseline,
      encoded.runtimeIdentityStability.shifted,
    )
    assert.strictEqual(encoded.runtimeIdentityStability.baseline.callables.length, 1)
    assert.strictEqual(encoded.runtimeIdentityStability.baseline.effects.length, 1)
    assert.strictEqual(encoded.runtimeIdentityStability.baseline.runners.length, 1)
    assert.strictEqual(
      encoded.runtimeIdentityStability.baseline.runners.every(
        (runner) => !/@\d|\$\d+\$\d+/.test(runner),
      ),
      true,
    )
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
