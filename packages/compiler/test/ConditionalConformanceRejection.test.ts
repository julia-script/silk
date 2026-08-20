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
const decoderBase = `interface Decoder {
  fn decode(value: &Self) -> i32
}

struct Schema { tag: i32 }

fn schemaDecode(value: &Schema) -> i32 { return value.tag }

impl Decoder for Schema { decode: Schema.schemaDecode }

struct MappedSchema<S> { source: S }

fn mappedDecode<S: Decoder>(value: &MappedSchema<S>) -> i32 {
  return Decoder.decode(&value.source) + 1
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
      `interface Decoder {
  fn decode(value: Self) -> i32
}

interface Left {
  fn leftward(value: Self) -> i32
}

interface Right {
  fn rightward(value: Self) -> i32
}

struct Wrapper<S> { source: S }

struct Boxed<T> { item: T }

fn wrapperDecode<S: Left>(value: &Wrapper<S>) -> i32 { return 1 }

fn boxedDecode<T: Right>(value: &Wrapper<Boxed<T>>) -> i32 { return 2 }

impl<S: Left> Decoder for Wrapper<S> { decode: Wrapper.wrapperDecode }

impl<T: Right> Decoder for Wrapper<Boxed<T>> { decode: Wrapper.boxedDecode }

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0119').map((diagnostic) => diagnostic.message),
      [
        'conditional-conformance-rejection/bound-distinguished.Decoder for conditional-conformance-rejection/bound-distinguished.Wrapper<conditional-conformance-rejection/bound-distinguished.Boxed<%0>> may overlap conditional-conformance-rejection/bound-distinguished.Decoder for conditional-conformance-rejection/bound-distinguished.Wrapper<%0>',
      ],
    )
    assert.deepEqual(reported(snapshot, 'SEM0119').at(0)?.related, ['overlapping implementation'])
    assert.deepEqual(reported(snapshot, 'SEM0119').at(0)?.notes, [
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
      `interface Marker {}

struct Problem { code: i32 }

impl Marker for Effect<i32 ! Problem> {}

impl<E> Marker for Effect<i32 ! E> {}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0119').map((diagnostic) => diagnostic.message),
      [
        'conditional-conformance-rejection/open-row.Marker for Effect<i32 ! %0> may overlap conditional-conformance-rejection/open-row.Marker for Effect<i32 ! conditional-conformance-rejection/open-row.Problem>',
      ],
    )
  }),
)

it.effect('accepts closed Effect heads with disjoint success, failure, and requirement rows', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/disjoint-effects',
      `interface SuccessMarker {}
interface FailureMarker {}
interface RequirementMarker {}

struct Problem {}
struct Other {}

service Left { effect fn value() -> i32 ? &Left }
service Right { effect fn value() -> i32 ? &Right }

impl SuccessMarker for Effect<i32> {}
impl SuccessMarker for Effect<bool> {}

impl FailureMarker for Effect<i32 ! Problem> {}
impl FailureMarker for Effect<i32 ! Other> {}

impl RequirementMarker for Effect<i32 ? &Left> {}
impl RequirementMarker for Effect<i32 ? &Right> {}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
  }),
)

it.effect('allows a service as an ordinary conditional proof requirement', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/service-bound',
      `interface Mark {}
service Gate { effect fn read() -> i32 ? &Gate }
struct Wrap<S> { value: S }

impl<S: Gate> Mark for Wrap<S> {}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.deepEqual(reported(snapshot, 'SEM0119'), [])
    assert.deepEqual(reported(snapshot, 'SEM0120'), [])
    const index = Analysis.declarationIndex(snapshot)
    assert.strictEqual(index.modules.at(0)?.conformances.at(0)?.validity._tag, 'ValidConformance')
    assert.strictEqual(index.modules.at(0)?.conformances.at(0)?.termination._tag, 'Terminating')
    const provider = Type.nominal('conditional-conformance-rejection/service-bound', 'Wrap', [
      'i32',
    ])
    const capability = Type.nominal('conditional-conformance-rejection/service-bound', 'Mark')
    assert.strictEqual(DeclarationIndex.prove(index, provider, capability)._tag, 'Unproved')
    assert.isUndefined(DeclarationIndex.witness(index, provider, capability))
    assert.isFalse(DeclarationIndex.conforms(index, provider, capability))
  }),
)

it.effect('rejects access variants that share one concrete specialization', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/access-overlap',
      `interface EffectMarker {}
interface RequirementMarker {}

service Clock { effect fn now() -> i32 ? &Clock }

impl EffectMarker for Effect<i32> {}
impl EffectMarker for mut Effect<i32> {}

impl RequirementMarker for Effect<i32 ? &Clock> {}
impl RequirementMarker for Effect<i32 ? &mut Clock> {}

pub fn main() -> i32 { return 0 }`,
    )
    const overlaps = reported(snapshot, 'SEM0119')
    for (const name of ['EffectMarker', 'RequirementMarker'])
      assert.strictEqual(
        overlaps.filter((diagnostic) => diagnostic.message.includes(name)).length,
        1,
        `${name}: ${JSON.stringify(Analysis.diagnostics(snapshot), undefined, 2)}`,
      )
    assert.strictEqual(overlaps.length, 2)
    assert.deepEqual(reported(snapshot, 'SEM0083'), [])
    assert.deepEqual(reported(snapshot, 'SEM0120'), [])
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

impl<S: Duo<S, S>> Decoder for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

pub fn main() -> i32 { return 0 }`,
    )
    assert.deepEqual(
      reported(snapshot, 'SEM0120').flatMap((diagnostic) => diagnostic.notes),
      [
        'requirement conditional-conformance-rejection/increasing-occurrences.Duo<%0, %0> for %0 repeats %0 3 times where the header uses it 1',
      ],
    )
  }),
)

it.effect('reports two identical conditional headers as an overlap rather than a duplicate', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance-rejection/duplicate',
      `${decoderBase}
impl<S: Decoder> Decoder for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

impl<S: Decoder> Decoder for MappedSchema<S> {
  decode: MappedSchema.mappedDecode
}

pub fn main() -> i32 { return 0 }`,
    )
    // Two headers that both declare requirements are reported as the overlap they are even when
    // their shapes are identical: naming them duplicates would suggest the bounds were compared,
    // and coherence never reads a bound. An unbounded repeat is still named a duplicate.
    const overlaps = reported(snapshot, 'SEM0119')
    assert.strictEqual(overlaps.length, 1)
    assert.include(overlaps.at(0)?.message ?? '', 'may overlap')
    assert.deepEqual(overlaps.at(0)?.related, ['overlapping implementation'])
    assert.deepEqual(reported(snapshot, 'SEM0083'), [])
  }),
)
