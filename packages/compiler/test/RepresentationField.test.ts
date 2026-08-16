import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as ModuleClosure from '../src/ModuleClosure.js'
import * as NameResolution from '../src/NameResolution.js'
import * as RepresentationField from '../src/RepresentationField.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceOrigin from '../src/SourceOrigin.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const declarations = (id: string, source: string) =>
  Effect.map(
    ModuleClosure.load({ root: SourceFile.make(id, encoder.encode(source)) }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    ),
    (closure) => NameResolution.analyze(closure).index,
  )

const analyzeAt = (id: string, source: string, path: string) =>
  Analysis.make({
    root: SourceFile.make(id, encoder.encode(source), SourceOrigin.projectFile(path)),
  }).pipe(Effect.provide(SourceResolver.empty))

const exactCallable = (module: string, name = 'decode') =>
  Type.exactRepresentationArgument(
    Type.callableIdentityArgument(`declaration:${module}:${name}`, {
      _tag: 'Declaration',
      module,
      name,
    }),
    Type.callable(['i32'], 'i32'),
  )

const exactEffect = (identity: string) => {
  const contract = Type.effect('i32', [])
  return Type.exactRepresentationArgument(Type.effectIdentityArgument(identity), contract)
}

it.effect('keeps field and specialization keys stable across rename and span movement', () =>
  Effect.gen(function* () {
    const module = 'representation-field/stable'
    const first = yield* declarations(module, `struct Parser<F: fn(i32) -> i32> { parse: F }`)
    const moved = yield* declarations(
      module,
      `

struct Parser<F: fn(i32) -> i32> {
  renamed: F
}`,
    )
    const firstPlan = RepresentationField.plansOf(first, Type.nominal(module, 'Parser')).at(0)
    const movedPlan = RepresentationField.plansOf(moved, Type.nominal(module, 'Parser')).at(0)
    const argument = exactCallable(module)
    const instance = Type.nominal(module, 'Parser', [argument])

    assert.strictEqual(
      firstPlan === undefined ? undefined : RepresentationField.idKey(firstPlan.id),
      movedPlan === undefined ? undefined : RepresentationField.idKey(movedPlan.id),
    )
    if (firstPlan === undefined || movedPlan === undefined) return
    assert.strictEqual(
      RepresentationField.key(instance, firstPlan.id),
      RepresentationField.key(instance, movedPlan.id),
    )
    const firstResolution = RepresentationField.lookup(
      RepresentationField.resolveFields(first, [instance]),
      instance,
      firstPlan.id,
    )
    const movedResolution = RepresentationField.lookup(
      RepresentationField.resolveFields(moved, [instance]),
      instance,
      movedPlan.id,
    )
    assert.strictEqual(firstResolution?._tag, 'ResolvedRepresentationField')
    assert.strictEqual(movedResolution?._tag, 'ResolvedRepresentationField')

    const firstOpen = Type.nominal(module, 'Parser', [
      Type.representationParameterArgument(firstPlan.parameter),
    ])
    const movedOpen = Type.nominal(module, 'Parser', [
      Type.representationParameterArgument(movedPlan.parameter),
    ])
    const firstUnavailable = RepresentationField.lookup(
      RepresentationField.resolveFields(first, [firstOpen]),
      firstOpen,
      firstPlan.id,
    )
    const movedUnavailable = RepresentationField.lookup(
      RepresentationField.resolveFields(moved, [movedOpen]),
      movedOpen,
      movedPlan.id,
    )
    assert.strictEqual(firstUnavailable?._tag, 'UnavailableRepresentationField')
    assert.strictEqual(movedUnavailable?._tag, 'UnavailableRepresentationField')
    if (
      firstUnavailable?._tag === 'UnavailableRepresentationField' &&
      movedUnavailable?._tag === 'UnavailableRepresentationField'
    ) {
      assert.notDeepEqual(firstUnavailable.provenance, movedUnavailable.provenance)
      assert.strictEqual(
        RepresentationField.key(firstOpen, firstUnavailable.id),
        RepresentationField.key(movedOpen, movedUnavailable.id),
      )
    }
  }),
)

it.effect('resolves nested repeated fields once per complete nominal instance', () =>
  Effect.gen(function* () {
    const module = 'representation-field/nested'
    const index = yield* declarations(
      module,
      `struct Inner<F: fn(i32) -> i32> { operation: F }
struct Outer<F: fn(i32) -> i32> { first: Inner<F> second: Inner<F> }`,
    )
    const argument = exactCallable(module)
    const instance = Type.nominal(module, 'Outer', [argument])
    const plans = RepresentationField.plansOf(index, instance)
    const inner = Type.nominal(module, 'Inner', [argument])
    const resolutions = RepresentationField.resolveFields(index, [instance, instance, inner])

    assert.deepEqual(
      plans.map((plan) => plan.id.ordinal),
      [0, 1],
    )
    assert.strictEqual(resolutions.resolutions.length, 3)
    for (const plan of plans) {
      const resolution = RepresentationField.lookup(resolutions, instance, plan.id)
      assert.strictEqual(resolution?._tag, 'ResolvedRepresentationField')
      if (resolution?._tag === 'ResolvedRepresentationField')
        assert.strictEqual(Type.equalsGenericArgument(resolution.argument, argument), true)
    }
    const innerPlan = RepresentationField.plansOf(index, inner).at(0)
    assert.strictEqual(
      innerPlan === undefined
        ? undefined
        : RepresentationField.lookup(resolutions, inner, innerPlan.id)?._tag,
      'ResolvedRepresentationField',
    )
  }),
)

it.effect('resolves every represented use forwarded through one nested physical field', () =>
  Effect.gen(function* () {
    const module = 'representation-field/nested-multiple'
    const index = yield* declarations(
      module,
      `struct Inner<F: fn(i32) -> i32, G: Effect<i32>> { callable: F deferred: G }
struct Outer<F: fn(i32) -> i32, G: Effect<i32>> { inner: Inner<F, G> }`,
    )
    const callable = exactCallable(module, 'decode')
    const effect = exactEffect(`${module}.effect`)
    const instance = Type.nominal(module, 'Outer', [callable, effect])
    const plans = RepresentationField.plansOf(index, instance)
    const resolutions = RepresentationField.resolveFields(index, [instance])

    assert.deepEqual(
      plans.map((plan) => ({
        field: plan.id.ordinal,
        use: plan.id.useOrdinal,
        bound: plan.requiredBound._tag,
      })),
      [
        { field: 0, use: 0, bound: 'CallableType' },
        { field: 0, use: 1, bound: 'EffectType' },
      ],
    )
    assert.strictEqual(new Set(plans.map((plan) => RepresentationField.idKey(plan.id))).size, 2)
    assert.deepEqual(
      plans.map((plan) => RepresentationField.lookup(resolutions, instance, plan.id)?._tag),
      ['ResolvedRepresentationField', 'ResolvedRepresentationField'],
    )

    const open = Type.nominal(
      module,
      'Outer',
      plans.map((plan) => Type.representationParameterArgument(plan.parameter)),
    )
    const unavailable = RepresentationField.resolveFields(index, [open])
    assert.deepEqual(
      plans.map((plan) => {
        const resolution = RepresentationField.lookup(unavailable, open, plan.id)
        return resolution?._tag === 'UnavailableRepresentationField'
          ? resolution.reason._tag
          : resolution?._tag
      }),
      ['OpenRepresentationArgument', 'OpenRepresentationArgument'],
    )
  }),
)

it.effect('assigns deterministic use ordinals to every represented union member', () =>
  Effect.gen(function* () {
    const module = 'representation-field/union-multiple'
    const index = yield* declarations(
      module,
      `struct Left<F: fn(i32) -> i32> { operation: F }
struct Right<G: fn(i32) -> i32> { operation: G }
struct Choice<F: fn(i32) -> i32, G: fn(i32) -> i32> { operation: Left<F> | Right<G> }`,
    )
    const first = exactCallable(module, 'first')
    const second = exactCallable(module, 'second')
    const instance = Type.nominal(module, 'Choice', [first, second])
    const plans = RepresentationField.plansOf(index, instance)
    const resolutions = RepresentationField.resolveFields(index, [instance])

    assert.deepEqual(
      plans.map((plan) => [plan.id.ordinal, plan.id.useOrdinal]),
      [
        [0, 0],
        [0, 1],
      ],
    )
    assert.deepEqual(
      plans.map((plan) => {
        const resolution = RepresentationField.lookup(resolutions, instance, plan.id)
        return resolution?._tag === 'ResolvedRepresentationField'
          ? Type.genericArgumentKey(resolution.argument)
          : resolution?._tag
      }),
      [Type.genericArgumentKey(first), Type.genericArgumentKey(second)],
    )
  }),
)

it.effect('retains explicit open recovery facts after the source specialization gate', () =>
  Effect.gen(function* () {
    const module = 'representation-field/open'
    const index = yield* declarations(module, `struct Parser<F: fn(i32) -> i32> { parse: F }`)
    const plan = RepresentationField.plansOf(index, Type.nominal(module, 'Parser')).at(0)
    assert.notStrictEqual(plan, undefined)
    if (plan === undefined) return
    const instance = Type.nominal(module, 'Parser', [
      Type.representationParameterArgument(plan.parameter),
    ])
    const resolution = RepresentationField.lookup(
      RepresentationField.resolveFields(index, [instance]),
      instance,
      plan.id,
    )

    assert.strictEqual(resolution?._tag, 'UnavailableRepresentationField')
    if (resolution?._tag !== 'UnavailableRepresentationField') return
    assert.strictEqual(resolution.reason._tag, 'OpenRepresentationArgument')
    assert.strictEqual(resolution.provenance.field.sourceId, module)
    assert.strictEqual(resolution.provenance.parameter.sourceId, module)
  }),
)

// Source elaboration normally resolves open representation parameters before field resolution.
// These actor-level cases deliberately bypass that gate to keep every recovery reason testable.
it.effect(
  'classifies every unavailable field-resolution recovery reason behind the specialization gate',
  () =>
    Effect.gen(function* () {
      const module = 'representation-field/recovery'
      const index = yield* declarations(module, `struct Parser<F: fn(i32) -> i32> { parse: F }`)
      const plan = RepresentationField.plansOf(index, Type.nominal(module, 'Parser')).at(0)
      assert.notStrictEqual(plan, undefined)
      if (plan === undefined) return
      const incompatible = Type.exactRepresentationArgument(
        Type.callableIdentityArgument(`declaration:${module}:wrong`, {
          _tag: 'Declaration',
          module,
          name: 'wrong',
        }),
        Type.callable(['i32'], 'bool'),
      )
      const instances = [
        Type.nominal(module, 'Parser'),
        Type.nominal(module, 'Parser', ['i32']),
        Type.nominal(module, 'Parser', [Type.representationParameterArgument(plan.parameter)]),
        Type.nominal(module, 'Parser', [incompatible]),
      ]

      assert.deepEqual(
        instances.map((instance) => {
          const resolution = RepresentationField.lookup(
            RepresentationField.resolveFields(index, [instance]),
            instance,
            plan.id,
          )
          return resolution?._tag === 'UnavailableRepresentationField'
            ? resolution.reason._tag
            : resolution?._tag
        }),
        [
          'MissingRepresentationArgument',
          'WrongRepresentationArgumentKind',
          'OpenRepresentationArgument',
          'IncompatibleRepresentationArgument',
        ],
      )
    }),
)

it.effect('keeps one exact identity under shared and once field-use bounds', () =>
  Effect.gen(function* () {
    const module = 'representation-field/bounds'
    const index = yield* declarations(
      module,
      `struct Shared<F: fn(i32) -> i32> { operation: F }
struct Once<F: once fn(i32) -> i32> { operation: F }`,
    )
    const argument = exactCallable(module)
    const shared = Type.nominal(module, 'Shared', [argument])
    const once = Type.nominal(module, 'Once', [argument])
    const resolutions = RepresentationField.resolveFields(index, [shared, once]).resolutions
    const sharedResolution = resolutions.find((resolution) => resolution.instance.name === 'Shared')
    const onceResolution = resolutions.find((resolution) => resolution.instance.name === 'Once')

    assert.strictEqual(sharedResolution?._tag, 'ResolvedRepresentationField')
    assert.strictEqual(onceResolution?._tag, 'ResolvedRepresentationField')
    if (
      sharedResolution?._tag !== 'ResolvedRepresentationField' ||
      onceResolution?._tag !== 'ResolvedRepresentationField'
    )
      return
    assert.strictEqual(
      Type.equalsGenericArgument(sharedResolution.argument, onceResolution.argument),
      true,
    )
    assert.strictEqual(sharedResolution.requiredBound._tag, 'CallableType')
    assert.strictEqual(onceResolution.requiredBound._tag, 'CallableType')
    if (
      sharedResolution.requiredBound._tag === 'CallableType' &&
      onceResolution.requiredBound._tag === 'CallableType'
    ) {
      assert.strictEqual(sharedResolution.requiredBound.mode, 'Shared')
      assert.strictEqual(onceResolution.requiredBound.mode, 'Take')
    }
  }),
)

it.effect('resolves one source callable identity under shared and once source bounds', () =>
  Effect.gen(function* () {
    const module = 'representation-field/source-bounds'
    const source = `struct Shared<F: fn(i32) -> i32> { operation: F }
struct Once<F: once fn(i32) -> i32> { operation: F }
fn decode(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let shared = Shared { operation: decode }
  let onceValue = Once { operation: decode }
  return 0
}`
    const snapshot = yield* Analysis.ofSource(module, encoder.encode(source))
    const main = Analysis.rootAnalysis(snapshot).functions.find(
      (fact) =>
        fact.declaration.name._tag === 'Present' && fact.declaration.name.spelling === 'main',
    )
    const instances = (main?.statements ?? []).flatMap((statement) =>
      statement._tag === 'BindStatement' &&
      statement.binding.inferredType._tag === 'Available' &&
      Type.isNominal(statement.binding.inferredType.type)
        ? [statement.binding.inferredType.type]
        : [],
    )
    const resolutions = RepresentationField.resolveFields(snapshot.index, instances).resolutions
    const shared = resolutions.find((resolution) => resolution.instance.name === 'Shared')
    const once = resolutions.find((resolution) => resolution.instance.name === 'Once')

    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    assert.strictEqual(shared?._tag, 'ResolvedRepresentationField')
    assert.strictEqual(once?._tag, 'ResolvedRepresentationField')
    if (
      shared?._tag !== 'ResolvedRepresentationField' ||
      once?._tag !== 'ResolvedRepresentationField'
    )
      return
    assert.strictEqual(Type.equalsGenericArgument(shared.argument, once.argument), true)
    assert.strictEqual(shared.admissibility._tag, 'Admitted')
    assert.strictEqual(once.admissibility._tag, 'Admitted')
    assert.notDeepEqual(shared.requiredBound, once.requiredBound)
  }),
)

it.effect('keeps executable identities stable across edit shifts and source-path relocation', () =>
  Effect.gen(function* () {
    const module = 'representation-field/executable-sites'
    const source = `struct Mappers<F: fn(i32) -> i32, G: fn(i32) -> i32> { first: F second: G }
struct Deferred<F: Effect<i32>, G: Effect<i32>> { first: F second: G }
pub fn main() -> i32 {
  let mappers = Mappers { first: i32.add(1), second: i32.add(1) }
  let deferred = Deferred { first: effect { return 1 }, second: effect { return 1 } }
  return 0
}`
    const shifted = `// Leading comments and whitespace move every diagnostic span.

struct Mappers<F: fn(i32) -> i32, G: fn(i32) -> i32> { first: F second: G }
struct Deferred<F: Effect<i32>, G: Effect<i32>> { first: F second: G }
pub fn main() -> i32 {
  // The executable sites retain their structural ordinals.
  let mappers = Mappers { first: i32.add(1), second: i32.add(1) }
  let deferred = Deferred { first: effect { return 1 }, second: effect { return 1 } }
  return 0
}`
    const first = yield* analyzeAt(module, source, '/workspace/first/Main.silk')
    const moved = yield* analyzeAt(module, shifted, '/relocated/project/Main.silk')
    const representedInstances = (snapshot: Analysis.SingleRootFrontendSnapshot) => {
      const main = Analysis.rootAnalysis(snapshot).functions.find(
        (fact) =>
          fact.declaration.name._tag === 'Present' && fact.declaration.name.spelling === 'main',
      )
      return (main?.statements ?? []).flatMap((statement) =>
        statement._tag === 'BindStatement' &&
        statement.binding.inferredType._tag === 'Available' &&
        Type.isNominal(statement.binding.inferredType.type)
          ? [statement.binding.inferredType.type]
          : [],
      )
    }
    const facts = (snapshot: Analysis.SingleRootFrontendSnapshot) =>
      representedInstances(snapshot).flatMap((instance) => {
        const resolutions = RepresentationField.resolveFields(snapshot.index, [instance])
        return RepresentationField.plansOf(snapshot.index, instance).map((plan) => {
          const resolution = RepresentationField.lookup(resolutions, instance, plan.id)
          return {
            nominal: Type.key(instance),
            field: RepresentationField.key(instance, plan.id),
            argument:
              resolution?._tag === 'ResolvedRepresentationField'
                ? Type.genericArgumentKey(resolution.argument)
                : undefined,
          }
        })
      })

    assert.deepEqual(facts(first), facts(moved))
    assert.strictEqual(facts(first).length, 4)
    assert.strictEqual(new Set(facts(first).map((fact) => fact.argument)).size, 4)
    assert.deepEqual(
      Analysis.sources(first).get(module)?.origin,
      SourceOrigin.projectFile('/workspace/first/Main.silk'),
    )
    assert.deepEqual(
      Analysis.sources(moved).get(module)?.origin,
      SourceOrigin.projectFile('/relocated/project/Main.silk'),
    )
    const firstRealized = Analysis.realize(first, 'wasm32-unknown-unknown')
    const movedRealized = Analysis.realize(moved, 'wasm32-unknown-unknown')
    assert.deepEqual(
      Analysis.diagnostics(firstRealized).map((diagnostic) => diagnostic.code),
      [],
    )
    assert.deepEqual(
      Analysis.diagnostics(movedRealized).map((diagnostic) => diagnostic.code),
      [],
    )
    assert.strictEqual(firstRealized.mir._tag, 'Available')
    assert.strictEqual(movedRealized.mir._tag, 'Available')
  }),
)
