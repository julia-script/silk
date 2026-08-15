import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array => Uint8Array.from(value, (unit) => unit.charCodeAt(0))

const analyze = (name: string, source: string) =>
  Analysis.ofSourceRealized(name, ascii(source), 'wasm32-unknown-unknown')

const reported = (
  snapshot: Analysis.Snapshot,
  code: string,
): ReadonlyArray<{
  readonly message: string
  readonly notes: ReadonlyArray<string>
  readonly related: ReadonlyArray<string>
}> =>
  Analysis.diagnostics(snapshot)
    .filter((diagnostic) => diagnostic.code === code)
    .map((diagnostic) => ({
      message: diagnostic.message,
      notes: diagnostic.notes ?? [],
      related: (diagnostic.relatedSpans ?? []).map((entry) => entry.label),
    }))

/**
 * The declarations every rejection fixture below reuses.
 *
 * `Schema` carries the only base witness, so a header that is rejected at declaration time can be
 * told apart from one that was admitted and simply had nothing to prove.
 */
const decoderBase = `interface Decoder<T> {
  fn decode(value: T) -> i32
}

struct Schema { tag: i32 }

fn schemaDecode(value: &Schema) -> i32 { return value.tag }

impl Decoder<Schema> for Schema { decode: Schema.schemaDecode }

struct MappedSchema<S> { source: S }

fn mappedDecode<S: Decoder>(value: &MappedSchema<S>) -> i32 {
  return Decoder.decode(value.source) + 1
}
`

it.effect('rejects heads that may unify even when their bounds cannot both hold', () =>
  Effect.gen(function* () {
    // No type conforms to both `Left` and `Right`, and neither interface has a single
    // implementation. Coherence still reports the overlap, because whether a bound is satisfiable
    // is a property of the whole program: a later declaration could make both provable, and a
    // coherence answer that moved with the program would silently change the selected witness.
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/bound-distinguished',
      `interface Decoder<T> {
  fn decode(value: T) -> i32
}

interface Left<T> {
  fn leftward(value: T) -> i32
}

interface Right<T> {
  fn rightward(value: T) -> i32
}

struct Wrapper<S> { source: S }

struct Boxed<T> { item: T }

fn wrapperDecode<S: Left>(value: &Wrapper<S>) -> i32 { return 1 }

fn boxedDecode<T: Right>(value: &Wrapper<Boxed<T>>) -> i32 { return 2 }

impl<S: Left<S>> Decoder<Wrapper<S>> for Wrapper<S> { decode: Wrapper.wrapperDecode }

impl<T: Right<T>> Decoder<Wrapper<Boxed<T>>> for Wrapper<Boxed<T>> { decode: Wrapper.boxedDecode }

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0108').map((diagnostic) => diagnostic.message),
      [
        'conditional-conformance-rejection/bound-distinguished.Decoder<conditional-conformance-rejection/bound-distinguished.Wrapper<conditional-conformance-rejection/bound-distinguished.Boxed<%0>>> for conditional-conformance-rejection/bound-distinguished.Wrapper<conditional-conformance-rejection/bound-distinguished.Boxed<%0>> may overlap conditional-conformance-rejection/bound-distinguished.Decoder<conditional-conformance-rejection/bound-distinguished.Wrapper<%0>> for conditional-conformance-rejection/bound-distinguished.Wrapper<%0>',
      ],
    )
    assert.deepEqual(reported(snapshot, 'SEM0108').at(0)?.related, ['overlapping implementation'])
    assert.deepEqual(reported(snapshot, 'SEM0108').at(0)?.notes, [
      'Conformance overlap is decided without consulting bounds, because whether a bound is satisfiable changes as a program grows.',
    ])
  }),
)

it.effect('rejects an open failure row overlapping a compatible closed row', () =>
  Effect.gen(function* () {
    // An open row stands for every extension of itself, so a head carrying `!E` covers the closed
    // row `!Problem` and the two declarations may name one provider.
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/open-row',
      `interface Marker<T> {}

struct Problem { code: i32 }

impl Marker<Effect<i32 ! Problem>> for Effect<i32 ! Problem> {}

impl<!E> Marker<Effect<i32 ! E>> for Effect<i32 ! E> {}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0108').map((diagnostic) => diagnostic.message),
      [
        'conditional-conformance-rejection/open-row.Marker<Effect<i32 ! %0>> for Effect<i32 ! %0> may overlap conditional-conformance-rejection/open-row.Marker<Effect<i32 ! conditional-conformance-rejection/open-row.Problem>> for Effect<i32 ! conditional-conformance-rejection/open-row.Problem>',
      ],
    )
  }),
)

it.effect('accepts closed Effect heads with disjoint success, failure, and requirement rows', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/disjoint-effects',
      `interface SuccessMarker<T> {}
interface FailureMarker<T> {}
interface RequirementMarker<T> {}

struct Problem {}
struct Other {}

service Left { effect fn value() -> i32 ? &Left }
service Right { effect fn value() -> i32 ? &Right }

impl SuccessMarker<Effect<i32>> for Effect<i32> {}
impl SuccessMarker<Effect<bool>> for Effect<bool> {}

impl FailureMarker<Effect<i32 ! Problem>> for Effect<i32 ! Problem> {}
impl FailureMarker<Effect<i32 ! Other>> for Effect<i32 ! Other> {}

impl RequirementMarker<Effect<i32 ? &Left>> for Effect<i32 ? &Left> {}
impl RequirementMarker<Effect<i32 ? &Right>> for Effect<i32 ? &Right> {}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('rejects a service used as a conditional proof requirement', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/service-bound',
      `interface Mark<T> {}
service Gate<T> { effect fn read() -> i32 ? &Gate<T> }
struct Wrap<S> { value: S }

impl<S: Gate<S>> Mark<Wrap<S>> for Wrap<S> {}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0083').map((diagnostic) => diagnostic.message),
      ['Invalid conformance: requirement Gate must be an interface applied to its own provider'],
    )
    assert.deepEqual(reported(snapshot, 'SEM0108'), [])
    assert.deepEqual(reported(snapshot, 'SEM0109'), [])
    const index = Analysis.declarationIndex(snapshot)
    assert.strictEqual(index.modules.at(0)?.conformances.at(0)?.validity._tag, 'InvalidConformance')
    assert.strictEqual(
      index.modules.at(0)?.conformances.at(0)?.termination._tag,
      'UnavailableTermination',
    )
    const provider = Type.nominal('conditional-conformance-rejection/service-bound', 'Wrap', [
      'i32',
    ])
    const capability = Type.nominal('conditional-conformance-rejection/service-bound', 'Mark', [
      provider,
    ])
    assert.strictEqual(DeclarationIndex.prove(index, provider, capability)._tag, 'Unproved')
    assert.isUndefined(DeclarationIndex.witness(index, provider, capability))
    assert.isFalse(DeclarationIndex.conforms(index, provider, capability))
  }),
)

it.effect('rejects a requirement whose provider equals the header provider', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/equal-provider',
      `${decoderBase}
impl<S: Decoder<MappedSchema<S>>> Decoder<MappedSchema<S>> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(reported(snapshot, 'SEM0109'), [
      {
        message:
          'conditional-conformance-rejection/equal-provider.Decoder<conditional-conformance-rejection/equal-provider.MappedSchema<%0>> for conditional-conformance-rejection/equal-provider.MappedSchema<%0> declares a requirement that does not descend',
        notes: [
          'requirement conditional-conformance-rejection/equal-provider.Decoder<conditional-conformance-rejection/equal-provider.MappedSchema<%0>> for conditional-conformance-rejection/equal-provider.MappedSchema<%0> does not descend: conditional-conformance-rejection/equal-provider.MappedSchema<%0> is not a strict subterm of conditional-conformance-rejection/equal-provider.MappedSchema<%0>',
        ],
        related: [],
      },
    ])
  }),
)

it.effect('rejects a requirement whose provider grows around the header binder', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/growing-provider',
      `${decoderBase}
struct Wrap<T> { held: T }

impl<S: Decoder<Wrap<S>>> Decoder<MappedSchema<S>> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0109').flatMap((diagnostic) => diagnostic.notes),
      [
        'requirement conditional-conformance-rejection/growing-provider.Decoder<conditional-conformance-rejection/growing-provider.Wrap<%0>> for conditional-conformance-rejection/growing-provider.Wrap<%0> does not descend: conditional-conformance-rejection/growing-provider.Wrap<%0> is not a strict subterm of conditional-conformance-rejection/growing-provider.MappedSchema<%0>',
      ],
    )
  }),
)

it.effect('rejects a requirement naming a peer provider', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/peer-provider',
      `${decoderBase}
struct Sibling<T> { held: T }

impl<S: Decoder<Sibling<S>>> Decoder<MappedSchema<S>> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0109').flatMap((diagnostic) => diagnostic.notes),
      [
        'requirement conditional-conformance-rejection/peer-provider.Decoder<conditional-conformance-rejection/peer-provider.Sibling<%0>> for conditional-conformance-rejection/peer-provider.Sibling<%0> does not descend: conditional-conformance-rejection/peer-provider.Sibling<%0> is not a strict subterm of conditional-conformance-rejection/peer-provider.MappedSchema<%0>',
      ],
    )
  }),
)

it.effect('rejects a requirement that repeats a binder the header uses once', () =>
  Effect.gen(function* () {
    // The provider still descends here, so only the occurrence condition can reject the header:
    // a requirement that trades depth for width would let one proof step ask two questions the
    // same size as the one it answers.
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/increasing-occurrences',
      `${decoderBase}
interface Duo<T, U> {}

impl<S: Duo<S, S>> Decoder<MappedSchema<S>> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0109').flatMap((diagnostic) => diagnostic.notes),
      [
        'requirement conditional-conformance-rejection/increasing-occurrences.Duo<%0, %0> for %0 repeats %0 3 times where the header uses it 2',
      ],
    )
  }),
)

it.effect('rejects an interface application that is not applied to its own provider', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/wrong-provider',
      `${decoderBase}
impl<S: Decoder<S>> Decoder<S> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0083').map((diagnostic) => diagnostic.message),
      [
        'Invalid conformance: Decoder must be applied to its own provider conditional-conformance-rejection/wrong-provider.MappedSchema<S>',
      ],
    )
    assert.deepEqual(reported(snapshot, 'SEM0108'), [])
    assert.deepEqual(reported(snapshot, 'SEM0109'), [])
  }),
)

it.effect('reports two identical conditional headers as an overlap rather than a duplicate', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/duplicate',
      `${decoderBase}
impl<S: Decoder<S>> Decoder<MappedSchema<S>> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

impl<S: Decoder<S>> Decoder<MappedSchema<S>> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

pub fn main() -> i32 { return 0 }`,
    )
    // Two headers that both declare requirements are reported as the overlap they are even when
    // their shapes are identical: naming them duplicates would suggest the bounds were compared,
    // and coherence never reads a bound. An unbounded repeat is still named a duplicate.
    const overlaps = reported(snapshot, 'SEM0108')
    assert.strictEqual(overlaps.length, 1)
    assert.include(overlaps.at(0)?.message ?? '', 'may overlap')
    assert.deepEqual(overlaps.at(0)?.related, ['overlapping implementation'])
    assert.deepEqual(reported(snapshot, 'SEM0083'), [])
  }),
)

it.effect('withholds a rejected header from proof search rather than admitting its witness', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/withheld',
      `${decoderBase}
struct Wrap<T> { held: T }

impl<S: Decoder<Wrap<S>>> Decoder<MappedSchema<S>> for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

fn decodeOf<T: Decoder>(value: T) -> i32 { return Decoder.decode(value) }

pub fn main() -> i32 {
  return decodeOf<MappedSchema<Schema>>(MappedSchema<Schema> { source: Schema { tag: 41 } })
}`,
    )
    assert.isAbove(reported(snapshot, 'SEM0109').length, 0)
    assert.isAbove(
      Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.code !== 'SEM0109').length,
      0,
    )
  }),
)
