import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array => Uint8Array.from(value, (unit) => unit.charCodeAt(0))

const analyze = (name: string, source: string) =>
  Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')

/**
 * One wrapper whose decoder conformance holds exactly when its source type has one.
 *
 * The wrapper adds one to whatever its source decodes, so a specialization that reached the wrong
 * witness, or that answered without following the requirement, cannot produce the expected number
 * by accident.
 */
const mappedDecoder = `interface Decoder<T> {
  fn decode(value: T) -> i32
}

struct Schema { tag: i32 }

fn schemaDecode(value: &Schema) -> i32 { return value.tag }

impl Decoder<Schema> for Schema { decode: Schema.schemaDecode }

struct MappedSchema<S> { source: S }

fn mappedDecode<S: Decoder>(value: &MappedSchema<S>) -> i32 {
  return Decoder.decode(value.source) + 1
}

impl<S: Decoder<S>> Decoder<MappedSchema<S>> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

fn decodeOf<T: Decoder>(value: T) -> i32 { return Decoder.decode(value) }`

it.effect('proves a conditional conformance through its declared requirement', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/mapped',
      `${mappedDecoder}

pub fn main() -> i32 {
  return decodeOf<MappedSchema<Schema>>(MappedSchema<Schema> { source: Schema { tag: 41 } })
}`,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [],
    )
  }),
)

it.effect('records the requirement and canonical head of a conditional conformance', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/facts',
      `${mappedDecoder}

pub fn main() -> i32 { return 0 }`,
    )
    const index = Analysis.declarationIndex(snapshot)
    const conformances = index.modules.flatMap((module) => module.conformances)
    const conditional = conformances.find((conformance) => conformance.requirements.length === 1)
    assert.isDefined(conditional)
    assert.strictEqual(conditional?.visibility, 'Public')
    assert.strictEqual(conditional?.coherence._tag, 'Coherent')
    assert.strictEqual(conditional?.termination._tag, 'Terminating')
    assert.strictEqual(conditional?.requirements.at(0)?.spelling, 'Decoder')
  }),
)

it.effect('proves one goal per concrete specialization and follows it to the base witness', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/proof',
      `${mappedDecoder}

pub fn main() -> i32 { return 0 }`,
    )
    const index = Analysis.declarationIndex(snapshot)
    const schema = Type.nominal('conditional-conformance/proof', 'Schema')
    const wrapped = Type.nominal('conditional-conformance/proof', 'MappedSchema', [schema])
    const proof = DeclarationIndex.prove(
      index,
      wrapped,
      Type.nominal('conditional-conformance/proof', 'Decoder', [wrapped]),
    )
    assert.strictEqual(proof._tag, 'Proved')
    if (proof._tag !== 'Proved') return
    assert.strictEqual(proof.selection._tag, 'SourceSelection')
    assert.strictEqual(proof.requirements.length, 1)
    assert.strictEqual(proof.typeArguments.length, 1)
    assert.strictEqual(Type.genericArgumentKey(proof.typeArguments[0] ?? 'never'), Type.key(schema))
  }),
)

it.effect('rejects a specialization whose source type has no witness', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/missing-base',
      `${mappedDecoder}

struct Loose { weight: i32 }

pub fn main() -> i32 {
  return decodeOf<MappedSchema<Loose>>(MappedSchema<Loose> { source: Loose { weight: 1 } })
}`,
    )
    assert.isAbove(Analysis.diagnostics(snapshot).length, 0)
  }),
)

it.effect('reports the requirement chain that a failed specialization broke', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/trace',
      `${mappedDecoder}

struct Loose { weight: i32 }

pub fn main() -> i32 {
  return decodeOf<MappedSchema<Loose>>(MappedSchema<Loose> { source: Loose { weight: 1 } })
}`,
    )
    const unproven = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'SEM0110',
    )
    assert.strictEqual(unproven.length, 1)
    const reported = unproven.at(0)
    assert.deepEqual(reported?.notes, [
      'required by Decoder for conditional-conformance/trace.MappedSchema<conditional-conformance/trace.Loose>',
      '  Decoder for conditional-conformance/trace.Loose: no conformance declares this specialization',
    ])
  }),
)
