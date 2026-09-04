import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Path from 'effect/Path'
import * as Schema from 'effect/Schema'
import * as Process from './support/Process.js'

const ConformanceReport = Schema.Struct({
  head: Schema.String,
  encoded: Schema.String,
  coherence: Schema.String,
  termination: Schema.String,
  requirements: Schema.Array(Schema.String),
  requirementProviders: Schema.Array(Schema.String),
  parameters: Schema.Finite,
})

const ProofReport = Schema.Struct({
  goal: Schema.String,
  encoded: Schema.String,
  proved: Schema.Boolean,
  selection: Schema.String,
  typeArguments: Schema.Array(Schema.String),
  dependencies: Schema.Array(Schema.String),
  trace: Schema.Array(Schema.String),
})
type ProofReport = typeof ProofReport.Type

const InstanceReport = Schema.Struct({
  declaration: Schema.String,
  module: Schema.String,
  arguments: Schema.Array(Schema.String),
})

const Report = Schema.Struct({
  diagnostics: Schema.Array(Schema.Struct({ code: Schema.String, message: Schema.String })),
  conformances: Schema.Array(ConformanceReport),
  normalization: Schema.Struct({
    canonical: Schema.String,
    scrambled: Schema.String,
    canonicalRequirements: Schema.Array(Schema.String),
    scrambledRequirements: Schema.Array(Schema.String),
    overlaps: Schema.Array(Schema.Struct({ overlaps: Schema.Boolean })),
  }),
  proofs: Schema.Array(ProofReport),
  reversedProofs: Schema.Array(ProofReport),
  instances: Schema.Array(InstanceReport),
  mir: Schema.String,
  native: Schema.String,
})

const decodeReport = Schema.decodeUnknownEffect(Schema.fromJsonString(Report))

const module_ = 'fixture/conditional-conformance-determinism'
const qualified = (spelling: string): string => `${module_}.${spelling}`

it.effect(
  'keeps conditional conformance proofs and instances byte-identical across fresh processes',
  () =>
    Effect.gen(function* () {
      const path = yield* Path.Path
      const fixture = yield* path.fromFileUrl(
        new URL('./fixtures/conditional-conformance-determinism.mjs', import.meta.url),
      )
      const first = yield* Process.run(process.execPath, [fixture])
      const second = yield* Process.run(process.execPath, [fixture])

      assert.strictEqual(first.exitCode, 0, first.stderr)
      assert.strictEqual(second.exitCode, 0, second.stderr)
      assert.strictEqual(first.stdout, second.stdout)
      const encoded = yield* decodeReport(first.stdout)

      // A fixture that stopped compiling, or that quietly answered nothing, would still emit a stable
      // blob, so every fact the report exists to pin is asserted rather than assumed.
      assert.deepEqual(encoded.diagnostics, [])

      assert.strictEqual(encoded.conformances.length, 4)
      const conditional = encoded.conformances.filter(
        (conformance) => conformance.requirements.length === 1,
      )
      assert.strictEqual(conditional.length, 2)
      assert.strictEqual(
        conditional.every(
          (conformance) =>
            conformance.coherence === 'Coherent' &&
            conformance.termination === 'Terminating' &&
            conformance.parameters === 1 &&
            conformance.requirements.at(0) === 'Decoder' &&
            conformance.requirementProviders.length === 1 &&
            // Both spellings of the normalized binder: identity renumbers it, prose renames it.
            conformance.head.includes('.impl:0') &&
            conformance.encoded.includes('%0'),
        ),
        true,
      )
      assert.deepEqual(
        conditional.map((conformance) => conformance.encoded),
        [
          `${qualified('Decoder')} for ${qualified('OptionalSchema')}<%0>`,
          `${qualified('Decoder')} for ${qualified('MappedSchema')}<%0>`,
        ],
      )
      assert.strictEqual(
        encoded.conformances.filter((conformance) => conformance.requirements.length === 0).length,
        2,
      )

      // Binder spelling, owner, and ordinal are provenance: two heads that differ only there are one.
      assert.strictEqual(encoded.normalization.canonical, encoded.normalization.scrambled)
      assert.deepEqual(
        encoded.normalization.canonicalRequirements,
        encoded.normalization.scrambledRequirements,
      )
      assert.strictEqual(encoded.normalization.overlaps.length, 12)
      assert.strictEqual(
        encoded.normalization.overlaps.every((pair) => pair.overlaps === false),
        true,
      )

      assert.strictEqual(encoded.proofs.length, 6)
      // The same goals asked backwards of a never-asked index: asking order is not an input to a proof.
      assert.deepEqual(encoded.proofs, encoded.reversedProofs)
      const proofOf = (spelling: string): ProofReport | undefined =>
        encoded.proofs.find((proof) => proof.encoded === `Decoder for ${spelling}`)
      const nested = proofOf(
        `${qualified('OptionalSchema')}<${qualified('MappedSchema')}<${qualified('Schema')}>>`,
      )
      assert.isDefined(nested)
      assert.strictEqual(nested?.proved, true)
      assert.strictEqual(nested?.selection, 'SourceSelection')
      assert.strictEqual(nested?.typeArguments.length, 1)
      // The depth-2 chain: the mapped wrapper, then the base schema, then the goal itself.
      assert.strictEqual(nested?.dependencies.length, 3)
      const mapped = proofOf(`${qualified('MappedSchema')}<${qualified('Schema')}>`)
      assert.strictEqual(mapped?.dependencies.length, 2)
      const tallied = proofOf(`${qualified('MappedSchema')}<${qualified('Tally')}>`)
      assert.strictEqual(tallied?.dependencies.length, 2)
      assert.notStrictEqual(mapped?.typeArguments.at(0), tallied?.typeArguments.at(0))
      const base = proofOf(qualified('Schema'))
      assert.strictEqual(base?.dependencies.length, 1)
      assert.strictEqual(base?.typeArguments.length, 0)
      const missing = proofOf(`${qualified('OptionalSchema')}<${qualified('Loose')}>`)
      assert.strictEqual(missing?.proved, false)
      assert.strictEqual(missing?.selection, 'MissingWitness')
      assert.deepEqual(missing?.trace, [
        `required by Decoder for ${qualified('OptionalSchema')}<${qualified('Loose')}>`,
        `  Decoder for ${qualified('Loose')}: no conformance declares this specialization`,
      ])
      assert.strictEqual(encoded.proofs.filter((proof) => proof.proved).length, 5)

      // Two concrete specializations of one conditional conformance, and one of the other.
      const mappedInstances = encoded.instances.filter(
        (instance) => instance.declaration === 'mappedDecode',
      )
      assert.strictEqual(mappedInstances.length, 2)
      assert.strictEqual(
        new Set(mappedInstances.map((instance) => instance.arguments.at(0))).size,
        2,
      )
      assert.strictEqual(
        encoded.instances.filter((instance) => instance.declaration === 'optionalDecode').length,
        1,
      )
      assert.strictEqual(encoded.instances.length, 8)
      assert.strictEqual(
        encoded.instances.every((instance) => instance.module === module_),
        true,
      )

      assert.isAbove(encoded.mir.length, 0)
      assert.include(encoded.mir, `mir-module ${module_}`)
      assert.include(encoded.mir, 'target wasm32-unknown-unknown')

      assert.strictEqual(encoded.native.length, 64)
    }).pipe(Effect.scoped, Effect.provide(NodeServices.layer)),
)
