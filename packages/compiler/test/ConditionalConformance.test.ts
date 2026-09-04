import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ConformanceProof from '../src/ConformanceProof.js'
import * as Hir from '../src/Hir.js'
import * as MirEncoding from '../src/MirEncoding.js'
import * as Type from '../src/Type.js'
import * as Projections from './support/projections.js'
import { raise } from './support/raise.js'

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
const mappedDecoder = `interface Decoder {
  fn decode(value: &Self) -> i32
}

struct Schema { tag: i32 }

fn schemaDecode(value: &Schema) -> i32 { return value.tag }

impl Decoder for Schema { decode: Schema.schemaDecode }

struct MappedSchema<S> { source: S }

fn mappedDecode<S: Decoder>(value: &MappedSchema<S>) -> i32 {
  return Decoder.decode(&value.source) + 1
}

impl<S: Decoder> Decoder for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

fn decodeOf<T: Decoder>(value: T) -> i32 { return Decoder.decode(&value) }`

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
    const proof = ConformanceProof.prove(
      index,
      wrapped,
      Type.nominal('conditional-conformance/proof', 'Decoder'),
    )
    assert.strictEqual(proof._tag, 'Proved')
    if (proof._tag !== 'Proved') return
    assert.strictEqual(proof.selection._tag, 'SourceSelection')
    assert.strictEqual(proof.requirements.length, 1)
    assert.strictEqual(proof.typeArguments.length, 1)
    assert.strictEqual(
      Type.genericArgumentKey(proof.typeArguments.at(0) ?? raise('expected one bound argument')),
      Type.key(schema),
    )
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
      (diagnostic) => diagnostic.code === 'SEM0121',
    )
    assert.strictEqual(unproven.length, 1)
    const reported = unproven.at(0)
    assert.deepEqual(reported?.notes, [
      'required by Decoder for conditional-conformance/trace.MappedSchema<conditional-conformance/trace.Loose>',
      '  Decoder for conditional-conformance/trace.Loose: no conformance declares this specialization',
    ])
  }),
)

/**
 * Two wrappers over one base, so a proof chain has depth two and two unrelated specializations of
 * one conditional header exist in one program.
 */
const nestedDecoder = `interface Decoder {
  fn decode(value: &Self) -> i32
}

struct Schema { tag: i32 }
struct Other { code: i32 }

fn schemaDecode(value: &Schema) -> i32 { return value.tag }
fn otherDecode(value: &Other) -> i32 { return value.code }

impl Decoder for Schema { decode: Schema.schemaDecode }
impl Decoder for Other { decode: Other.otherDecode }

struct OptionalSchema<S> { source: S }

fn optionalDecode<S: Decoder>(value: &OptionalSchema<S>) -> i32 {
  return Decoder.decode(&value.source) + 1
}

impl<S: Decoder> Decoder for OptionalSchema<S> {
  decode: OptionalSchema.optionalDecode
}

fn decodeOf<T: Decoder>(value: T) -> i32 { return Decoder.decode(&value) }`

it.effect('discovers one witness instance per concrete specialization of one header', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/specializations',
      `${nestedDecoder}

pub fn main() -> i32 {
  let outer = decodeOf<OptionalSchema<Schema>>(OptionalSchema<Schema> { source: Schema { tag: 1 } })
  let other = decodeOf<OptionalSchema<Other>>(OptionalSchema<Other> { source: Other { code: 2 } })
  return outer + other
}`,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [],
    )
    const instances = Analysis.instancesOf(snapshot).instances
    const wrappers = instances.filter(
      (instance) => instance.key.declaration.name === 'optionalDecode',
    )
    // One conditional header, two concrete providers, two witness instances — and no third for the
    // unsubstituted form.
    assert.strictEqual(wrappers.length, 2)
    assert.deepEqual(
      wrappers
        .map((instance) => instance.key.typeArguments.map(Type.genericArgumentKey).join(','))
        .toSorted(),
      [
        Type.key(Type.nominal('conditional-conformance/specializations', 'Other')),
        Type.key(Type.nominal('conditional-conformance/specializations', 'Schema')),
      ].toSorted(),
    )
    // The base witnesses are reached through the wrapper's own proof, not by being called directly.
    assert.strictEqual(
      instances.filter((instance) => instance.key.declaration.name === 'schemaDecode').length,
      1,
    )
    assert.strictEqual(
      instances.filter((instance) => instance.key.declaration.name === 'otherDecode').length,
      1,
    )
  }),
)

it.effect('discovers proved base witnesses even when the wrapper operation never calls them', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/unused-requirement',
      `interface Decoder { fn decode(value: &Self) -> i32 }

struct Schema { tag: i32 }
fn schemaDecode(value: &Schema) -> i32 { return value.tag }
impl Decoder for Schema { decode: Schema.schemaDecode }

struct Wrapper<S> { source: S }
fn wrapperDecode<S: Decoder>(value: &Wrapper<S>) -> i32 { return 7 }
impl<S: Decoder> Decoder for Wrapper<S> { decode: Wrapper.wrapperDecode }

fn decodeOf<Self: Decoder>(value: Self) -> i32 { return Decoder.decode(&value) }

pub fn main() -> i32 {
  return decodeOf<Wrapper<Schema>>(Wrapper<Schema> { source: Schema { tag: 41 } })
}`,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [],
    )
    const instances = Analysis.instancesOf(snapshot).instances
    assert.strictEqual(
      instances.filter((instance) => instance.key.declaration.name === 'wrapperDecode').length,
      1,
    )
    assert.strictEqual(
      instances.filter((instance) => instance.key.declaration.name === 'schemaDecode').length,
      1,
    )
  }),
)

it.effect('marks only proof dependencies, never the selected witness root, as descending', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/root-edge',
      `${mappedDecoder}

pub fn main() -> i32 { return 0 }`,
    )
    const schema = Type.nominal('conditional-conformance/root-edge', 'Schema')
    const wrapper = Type.nominal('conditional-conformance/root-edge', 'MappedSchema', [schema])
    const capability = Type.nominal('conditional-conformance/root-edge', 'Decoder')
    const target = ConformanceProof.interfaceWitnessTarget(
      Analysis.declarationIndex(snapshot),
      wrapper,
      capability,
      'decode',
    )
    assert.isDefined(target)
    assert.notProperty(target ?? {}, 'structurallyDescending')
    assert.strictEqual(target?.implementation.name, 'mappedDecode')
    assert.strictEqual(target?.structuralProvider === undefined, false)
  }),
)

it.effect('lowers a conditional witness to one direct static call and no runtime dispatch', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/lowering',
      `${nestedDecoder}

pub fn main() -> i32 {
  return decodeOf<OptionalSchema<Schema>>(OptionalSchema<Schema> { source: Schema { tag: 41 } })
}`,
    )
    const mir = Analysis.loweredMir(snapshot)
    const encoded = MirEncoding.encode(mir)
    // Every call names a declaration and its concrete arguments. Nothing selects a witness at run
    // time, so no dictionary, interface tag, or actor-name lookup can appear.
    for (const spelling of ['dictionary', 'vtable', 'witnessTable', 'interfaceTag', 'typeTag'])
      assert.isFalse(encoded.includes(spelling), `${spelling} reached MIR`)
    const witnessCalls = mir.functions.flatMap((fn) =>
      fn.regions.flatMap((region) =>
        region._tag === 'OperationRegion'
          ? region.operations.filter(
              (operation) =>
                operation._tag === 'Call' && operation.target.name === 'optionalDecode',
            )
          : [],
      ),
    )
    assert.strictEqual(witnessCalls.length, 1)
    const witnessCall = witnessCalls.at(0)
    assert.deepEqual(
      witnessCall?._tag === 'Call'
        ? witnessCall.typeArguments.map(Type.genericArgumentKey)
        : ['no call'],
      [Type.key(Type.nominal('conditional-conformance/lowering', 'Schema'))],
    )
  }),
)

it.effect('diagnoses unresolved and conflicting target binders deterministically', () =>
  Effect.gen(function* () {
    const unresolved = yield* analyze(
      'conditional-conformance/unresolved-target-binder',
      `interface Mixer<S, A> { fn mix(left: &S, right: &A) -> i32 }
struct Box<T> { value: T }
fn mix<T, U>(left: &Box<T>, right: &bool) -> i32 { return 0 }
impl Mixer<Box<i32>, bool> for Box<i32> { mix: Box.mix }
pub fn main() -> i32 { return 0 }`,
    )
    const conflicting = yield* analyze(
      'conditional-conformance/conflicting-target-binder',
      `interface Mixer<S, A> { fn mix(left: &S, right: &A) -> i32 }
struct Box<T> { value: T }
fn mix<T>(left: &Box<T>, right: &T) -> i32 { return 0 }
impl Mixer<Box<i32>, bool> for Box<i32> { mix: Box.mix }
pub fn main() -> i32 { return 0 }`,
    )
    const repeated = yield* analyze(
      'conditional-conformance/repeated-conflicting-target-binder',
      `interface Inspect<S> { fn inspect(value: &S) -> i32 }
struct Pair<A, B> { left: A right: B }
fn inspect<T>(value: &Pair<T, T>) -> i32 { return 0 }
impl Inspect<Pair<i32, bool>> for Pair<i32, bool> { inspect: Pair.inspect }
pub fn main() -> i32 { return 0 }`,
    )

    assert.deepEqual(
      Analysis.diagnostics(unresolved).map((diagnostic) => diagnostic.message),
      ['Invalid conformance: Box.mix: cannot infer witness target binder U'],
    )
    assert.deepEqual(
      Analysis.diagnostics(conflicting).map((diagnostic) => diagnostic.message),
      [
        'Invalid conformance: Box.mix: witness target binder T is i32 from receiver left but bool from parameter right',
      ],
    )
    assert.deepEqual(
      Analysis.diagnostics(repeated).map((diagnostic) => diagnostic.message),
      [
        'Invalid conformance: Pair.inspect: witness target binder T is i32 from receiver value (earlier occurrence) but bool from receiver value (later occurrence)',
      ],
    )
  }),
)

it.effect('keeps the conditional witness question unresolved in generic HIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/generic-hir',
      `${nestedDecoder}

pub fn main() -> i32 {
  return decodeOf<OptionalSchema<Schema>>(OptionalSchema<Schema> { source: Schema { tag: 1 } })
}`,
    )
    const hir = Projections.hirOf(snapshot, 'conditional-conformance/generic-hir')
    assert.isDefined(hir)
    if (hir === undefined) return
    const encoded = Hir.encode(hir)
    // The wrapper's body names the interface, the operation, and the bounded parameter it dispatches
    // over — and no witness. Which conformance answers it is decided per specialization, so a
    // generic body that already carried an answer would have to carry one answer for every provider.
    assert.isTrue(
      encoded.includes('interface conditional-conformance/generic-hir.Decoder.decode over S'),
    )
    for (const spelling of ['witness', 'dictionary', 'vtable'])
      assert.isFalse(encoded.includes(spelling), `${spelling} reached generic HIR`)
  }),
)

it.effect('rejects two headers whose bounds are the only difference between them', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/bound-distinguished',
      `interface Decoder { fn decode(value: &Self) -> i32 }
interface Left { fn decode(value: &Self) -> i32 }
interface Right { fn decode(value: &Self) -> i32 }

struct Wrap<S> { source: S }

fn viaLeft<S: Left>(value: &Wrap<S>) -> i32 { return Left.decode(&value.source) }
fn viaRight<S: Right>(value: &Wrap<S>) -> i32 { return Right.decode(&value.source) }

impl<S: Left> Decoder for Wrap<S> { decode: Wrap.viaLeft }
impl<S: Right> Decoder for Wrap<S> { decode: Wrap.viaRight }

pub fn main() -> i32 { return 0 }`,
    )
    // No type satisfies both bounds today, and that is deliberately not consulted: whether a bound
    // is satisfiable moves as a program grows, so a coherence answer that read it would move too.
    const overlaps = Analysis.diagnostics(snapshot).filter(
      (diagnostic) => diagnostic.code === 'SEM0119',
    )
    assert.strictEqual(overlaps.length, 1)
    assert.include(overlaps.at(0)?.message ?? '', 'may overlap')
  }),
)

it.effect('rejects a witness demanding a bound its header never promises', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/unpromised',
      `interface Decoder { fn decode(value: &Self) -> i32 }
interface Encoder { fn decode(value: &Self) -> i32 }

struct Wrap<S> { source: S }

fn viaEncoder<S: Encoder>(value: &Wrap<S>) -> i32 { return Encoder.decode(&value.source) }

impl<S: Decoder> Decoder for Wrap<S> { decode: Wrap.viaEncoder }

pub fn main() -> i32 { return 0 }`,
    )
    // The header promises a decoder for the source and the witness asks for an encoder. Admitting
    // it would leave that obligation proved nowhere, and the call would lower to nothing.
    const details = Analysis.diagnostics(snapshot)
      .filter((diagnostic) => diagnostic.code === 'SEM0083')
      .map((diagnostic) => diagnostic.message)
    assert.isTrue(
      details.some((detail) => detail.includes('which Decoder for') && detail.includes('Encoder')),
      details.join(' | '),
    )
  }),
)
