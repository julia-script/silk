import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Path from 'effect/Path'
import * as Process from './support/Process.js'

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
    const encoded = JSON.parse(first.stdout) as {
      readonly semantic: string
      readonly hir: string
      readonly instances: ReadonlyArray<{
        readonly declaration: { readonly name: string }
        readonly arguments: ReadonlyArray<string>
      }>
      readonly presentation: string
      readonly diagnostics: ReadonlyArray<{ readonly code: string }>
      readonly fences: {
        readonly diagnostics: ReadonlyArray<{ readonly code: string }>
        readonly layout: string
        readonly mir: string
      }
      readonly representationFields: {
        readonly plans: ReadonlyArray<{ readonly field: string }>
        readonly resolved: ReadonlyArray<{
          readonly field: string
          readonly key: string
          readonly argument: string
          readonly requiredBound: string
          readonly admissibility: string
        }>
        readonly unavailable: ReadonlyArray<{
          readonly field: string
          readonly key: string
          readonly requiredBound: string
          readonly reason: string
        }>
      }
    }

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
      ['SEM0103', 'SEM0107'],
    )
    assert.strictEqual(encoded.fences.layout, 'Unavailable')
    assert.strictEqual(encoded.fences.mir, 'Unavailable')
    assert.strictEqual(encoded.representationFields.plans.length, 6)
    assert.strictEqual(encoded.representationFields.resolved.length, 6)
    assert.strictEqual(encoded.representationFields.unavailable.length, 6)
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
  }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
