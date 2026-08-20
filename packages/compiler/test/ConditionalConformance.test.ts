import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as Hir from '../src/Hir.js'
import * as Mir from '../src/Mir.js'
import * as Type from '../src/Type.js'
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
    const proof = DeclarationIndex.prove(
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

it.effect('infers nominal failure and requirement rows through conditional witness instances', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/kinded-rows',
      `interface Decoder { fn decode(value: &Self) -> i32 }

struct Schema { tag: i32 }
fn schemaDecode(value: &Schema) -> i32 { return value.tag }
impl Decoder for Schema { decode: Schema.schemaDecode }

struct Problem {}
struct FailureBox<S, E> { source: S }
fn makeFailure<S, E>(source: S, pending: once Effect<i32 ! E>) -> FailureBox<S, E> {
  drop pending
  return FailureBox<S, E> { source: move source }
}
fn failureDecode<S: Decoder, E>(value: &FailureBox<S, E>) -> i32 { return 1 }
impl<S: Decoder, E> Decoder for FailureBox<S, E> {
  decode: FailureBox.failureDecode
}

struct RequirementBox<S, ?R> { source: S }
fn makeRequirement<S, ?R>(source: S, pending: once Effect<i32 ? R>) -> RequirementBox<S, R> {
  drop pending
  return RequirementBox<S, R> { source: move source }
}
fn requirementDecode<S: Decoder, ?R>(value: &RequirementBox<S, R>) -> i32 { return 2 }
impl<S: Decoder, ?R> Decoder for RequirementBox<S, R> {
  decode: RequirementBox.requirementDecode
}

service Clock { effect fn tick() -> i32 ? &Clock }
effect fn problem() -> i32 ! Problem { fail Problem {} }
effect fn requiringClock() -> i32 ? &Clock { return 2 }
fn decodeOf<T: Decoder>(value: T) -> i32 { return Decoder.decode(&value) }

pub fn main() -> i32 {
  let failure = makeFailure(Schema { tag: 1 }, problem())
  let requirement = makeRequirement(Schema { tag: 2 }, requiringClock())
  return decodeOf(move failure) + decodeOf(move requirement)
}`,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [],
    )
    const instances = Analysis.instancesOf(snapshot).instances
    const failure = instances.filter(
      (instance) => instance.key.declaration.name === 'failureDecode',
    )
    const requirement = instances.filter(
      (instance) => instance.key.declaration.name === 'requirementDecode',
    )
    assert.strictEqual(failure.length, 1)
    assert.strictEqual(requirement.length, 1)
    assert.deepEqual(failure.at(0)?.key.typeArguments.map(Type.encodeGenericArgument), [
      'conditional-conformance/kinded-rows.Schema',
      'conditional-conformance/kinded-rows.Problem',
    ])
    assert.deepEqual(requirement.at(0)?.key.typeArguments.map(Type.encodeGenericArgument), [
      'conditional-conformance/kinded-rows.Schema',
      '? &conditional-conformance/kinded-rows.Clock',
    ])
    const index = Analysis.declarationIndex(snapshot)
    const module = 'conditional-conformance/kinded-rows'
    const schema = Type.nominal(module, 'Schema')
    const problem = Type.nominal(module, 'Problem')
    const clock = Type.nominal(module, 'Clock')
    const failureBox = Type.nominal(module, 'FailureBox', [schema, Type.failureValue([problem])])
    const requirementBox = Type.nominal(module, 'RequirementBox', [
      schema,
      Type.requirementRowArgument([{ capability: clock, role: 'DefaultRole', access: 'Shared' }]),
    ])
    const failureProof = DeclarationIndex.prove(index, failureBox, Type.nominal(module, 'Decoder'))
    const requirementProof = DeclarationIndex.prove(
      index,
      requirementBox,
      Type.nominal(module, 'Decoder'),
    )
    assert.strictEqual(failureProof._tag, 'Proved')
    assert.strictEqual(requirementProof._tag, 'Proved')
    if (failureProof._tag === 'Proved')
      assert.deepEqual(failureProof.typeArguments.map(Type.encodeGenericArgument), [
        'conditional-conformance/kinded-rows.Schema',
        'conditional-conformance/kinded-rows.Problem',
      ])
    if (requirementProof._tag === 'Proved')
      assert.deepEqual(requirementProof.typeArguments.map(Type.encodeGenericArgument), [
        'conditional-conformance/kinded-rows.Schema',
        '? &conditional-conformance/kinded-rows.Clock',
      ])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 3n)
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

it.effect('discovers unused proof dependencies before a conditional Drop hook', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/drop-dependency',
      `import silk.core { Allocator }
import silk.core { OutOfMemoryError }
import silk.core { SystemAllocator }
import silk.effects as Effect
import silk.layout { Layout }
interface Releasable { fn release(value: &Self) -> i32 }

struct Token { code: i32 }
fn releaseToken(value: &Token) -> i32 { return value.code }
impl Releasable for Token { release: Token.releaseToken }

struct Guard<Self, U> {
  first: Self
  second: U
  storage: Allocation
}

impl<T: Releasable> Drop for Guard<T, T> {
  fn drop(self: &mut Guard<T, T>) -> () { return () }
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let guard = Guard<Token, Token> {
    first: Token { code: 1 },
    second: Token { code: 2 },
    storage: move allocation
  }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [],
    )
    const instances = Analysis.instancesOf(snapshot).instances
    const releaseOrdinal = instances.findIndex(
      (instance) => instance.key.declaration.name === 'releaseToken',
    )
    const dropOrdinal = instances.findIndex((instance) =>
      instance.key.declaration.name.startsWith('drop@impl#'),
    )
    assert.isAtLeast(releaseOrdinal, 0)
    assert.isAtLeast(dropOrdinal, 0)
    assert.isBelow(releaseOrdinal, dropOrdinal)
    assert.deepEqual(instances.at(dropOrdinal)?.key.typeArguments.map(Type.encodeGenericArgument), [
      'conditional-conformance/drop-dependency.Token',
    ])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 42n)
      assert.strictEqual(
        outcome.trace.filter(
          (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl#'),
        ).length,
        1,
      )
    }
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
    const target = DeclarationIndex.interfaceWitnessTarget(
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

it.effect('tracks structural descent by provider when witness argument positions change', () =>
  Effect.gen(function* () {
    const snapshot = yield* analyze(
      'conditional-conformance/provider-descent',
      `interface Decoder { fn decode(value: &Self) -> i32 }

struct X { tag: i32 }
struct Y {}
struct Z {}
fn decodeX(value: &X) -> i32 { return value.tag }
impl Decoder for X { decode: X.decodeX }

struct Pair<A, B> { left: A right: B }
fn decodePair<A: Decoder, B>(value: &Pair<A, B>) -> i32 {
  return Decoder.decode(&value.left) + 1
}
impl<A: Decoder, B> Decoder for Pair<A, B> {
  decode: Pair.decodePair
}

fn decodeOf<T: Decoder>(value: T) -> i32 { return Decoder.decode(&value) }
pub fn main() -> i32 {
  let inner = Pair<X, Y> { left: X { tag: 40 }, right: Y {} }
  return decodeOf<Pair<Pair<X, Y>, Z>>(Pair<Pair<X, Y>, Z> {
    left: move inner,
    right: Z {}
  })
}`,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [],
    )
    const pairInstances = Analysis.instancesOf(snapshot).instances.filter(
      (instance) => instance.key.declaration.name === 'decodePair',
    )
    assert.strictEqual(pairInstances.length, 2)
    assert.deepEqual(
      pairInstances.map((instance) => instance.key.typeArguments.map(Type.encodeGenericArgument)),
      [
        [
          'conditional-conformance/provider-descent.X',
          'conditional-conformance/provider-descent.Y',
        ],
        [
          'conditional-conformance/provider-descent.Pair<conditional-conformance/provider-descent.X, conditional-conformance/provider-descent.Y>',
          'conditional-conformance/provider-descent.Z',
        ],
      ],
    )
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
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
    const encoded = Mir.encode(mir)
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

it.effect('infers reordered target binders and discovers two exact static specializations', () =>
  Effect.gen(function* () {
    const module = 'conditional-conformance/generic-targets'
    const snapshot = yield* analyze(
      module,
      `interface Renderer { fn render(value: &Self) -> i32 }

struct Box<A, F: fn(i32) -> i32> { value: A }

fn box<F: fn(i32) -> i32>(value: i32, operation: F) -> Box<i32, F> {
  drop operation
  return Box<i32, F> { value: value }
}

fn render<F: fn(i32) -> i32, A>(value: &Box<A, F>) -> i32 { return 21 }
impl<A, F: fn(i32) -> i32> Renderer for Box<A, F> {
  render: Box.render
}

fn first(value: i32) -> i32 { return value + 1 }
fn second(value: i32) -> i32 { return value + 2 }
fn renderOf<T: Renderer>(value: T) -> i32 { return Renderer.render(&value) }

pub fn main() -> i32 {
  let left = box(1, first)
  let right = box(2, second)
  return renderOf(move left) + renderOf(move right)
}`,
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map(
        (diagnostic) => `${diagnostic.code}: ${diagnostic.message}`,
      ),
      [],
    )
    const conformance = Analysis.declarationIndex(snapshot)
      .modules.flatMap((candidate) => candidate.conformances)
      .at(0)
    assert.deepEqual(
      conformance?.operations.at(0)?.targetArguments?.map(Type.encodeGenericArgument),
      ['F', 'A'],
    )

    const hir = Analysis.hirOf(snapshot, module)
    assert.isDefined(hir)
    if (hir === undefined) return
    const encodedHir = Hir.encode(hir)
    assert.include(encodedHir, `bound ${module}.Renderer.render over T`)
    const renderQuestion = hir.functions
      .find(
        (fn) =>
          fn.declaration.name._tag === 'Present' && fn.declaration.name.spelling === 'renderOf',
      )
      ?.statements.flatMap(Hir.statementExpressions)
      .flatMap(Hir.expressionTree)
      .find((expression) => expression._tag === 'BoundOperationCall')
    assert.strictEqual(renderQuestion?._tag, 'BoundOperationCall')
    if (renderQuestion?._tag !== 'BoundOperationCall') return
    assert.strictEqual(Type.encode(renderQuestion.provider), 'T')
    assert.strictEqual(Type.encode(renderQuestion.capability), `${module}.Renderer`)
    assert.deepEqual(
      renderQuestion.contract.operands.map((operand) =>
        operand.type._tag === 'Resolved' ? Type.encode(operand.type.type) : operand.type._tag,
      ),
      ['&T'],
    )

    const renderInstances = Analysis.instancesOf(snapshot).instances.filter(
      (instance) => instance.key.declaration.name === 'render',
    )
    assert.strictEqual(renderInstances.length, 2)
    assert.isTrue(
      renderInstances.every(
        (instance) =>
          Type.isExactRepresentationArgument(instance.key.typeArguments.at(0) ?? 'never') &&
          Type.equalsGenericArgument(instance.key.typeArguments.at(1) ?? 'never', 'i32'),
      ),
    )
    assert.strictEqual(
      new Set(
        renderInstances.map((instance) =>
          instance.key.typeArguments.map(Type.genericArgumentKey).join('|'),
        ),
      ).size,
      2,
    )

    const mir = Analysis.loweredMir(snapshot)
    const witnessCalls = mir.functions.flatMap((fn) =>
      fn.regions.flatMap((region) =>
        region._tag === 'OperationRegion'
          ? region.operations.filter(
              (operation) => operation._tag === 'Call' && operation.target.name === 'render',
            )
          : [],
      ),
    )
    assert.strictEqual(witnessCalls.length, 2)
    const encodedMir = Mir.encode(mir)
    for (const spelling of ['dictionary', 'vtable', 'witnessTable', 'interfaceTag', 'typeTag'])
      assert.isFalse(encodedMir.includes(spelling), `${spelling} reached MIR`)
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
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
    const hir = Analysis.hirOf(snapshot, 'conditional-conformance/generic-hir')
    assert.isDefined(hir)
    if (hir === undefined) return
    const encoded = Hir.encode(hir)
    // The wrapper's body names the interface, the operation, and the bounded parameter it dispatches
    // over — and no witness. Which conformance answers it is decided per specialization, so a
    // generic body that already carried an answer would have to carry one answer for every provider.
    assert.isTrue(
      encoded.includes('bound conditional-conformance/generic-hir.Decoder.decode over S'),
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

it.effect('infers operand binders and propagates a failing smaller generic witness row', () =>
  Effect.gen(function* () {
    const module = 'conditional-conformance/smaller-generic-row'
    const snapshot = yield* analyze(
      module,
      `import silk.effects as Effect
import silk.result { Result, Success, Failure }

struct Problem { code: i32 }
struct Extra {}

interface Decoder {
  effect fn decode(value: &Self) -> i32 ! Problem | Extra
}

struct Box<A> { value: A }

effect fn decodeBox<A>(value: &Box<A>) -> i32 ! Problem {
  fail Problem { code: 42 }
}

impl Decoder for Box<i32> { decode: Box.decodeBox }

fn pending<T: Decoder>(value: &T) -> Effect<i32 ! Problem | Extra> {
  return Decoder.decode(value)
}

fn observe(result: Result<i32, Problem | Extra>) -> i32 {
  return match move result {
    Result<i32, Problem | Extra> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<Problem | Extra> { error } => match move error {
        Problem { code } => code
        Extra {} => 0
      }
    }
  }
}

pub fn main() -> i32 {
  let boxed = Box<i32> { value: 0 }
  return observe(run Effect.result(pending<Box<i32>>(&boxed)))
}`,
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const provider = Type.nominal(module, 'Box', ['i32'])
    const capability = Type.nominal(module, 'Decoder')
    const target = DeclarationIndex.interfaceWitnessTarget(
      Analysis.declarationIndex(snapshot),
      provider,
      capability,
      'decode',
    )
    assert.strictEqual(target?.implementation.name, 'decodeBox')
    assert.deepEqual(target?.typeArguments.map(Type.encodeGenericArgument), ['i32'])
    const instances = Analysis.instancesOf(snapshot).instances.filter(
      (instance) => instance.key.declaration.name === 'decodeBox',
    )
    assert.strictEqual(instances.length, 1)
    assert.deepEqual(instances.at(0)?.key.typeArguments.map(Type.encodeGenericArgument), ['i32'])

    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)
