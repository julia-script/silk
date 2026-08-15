import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as OpaqueRealization from '../src/OpaqueRealization.js'
import * as ProjectAnalysis from '../src/ProjectAnalysis.js'
import * as SemanticInvalidation from '../src/SemanticInvalidation.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const encoder = new TextEncoder()

const sources = (entries: Readonly<Record<string, string>>): ReadonlyMap<string, Uint8Array> =>
  new Map(Object.entries(entries).map(([module, source]) => [module, encoder.encode(source)]))

const analyze = Effect.fnUntraced(function* (
  currentSources: ReadonlyMap<string, Uint8Array>,
  rootModules: ReadonlyArray<string>,
  previous?: ProjectAnalysis.ProjectAnalysis,
) {
  const roots = rootModules.map((module) =>
    SourceFile.make(module, currentSources.get(module) ?? assert.fail(`missing source ${module}`)),
  )
  const effect =
    previous === undefined ? ProjectAnalysis.make(roots) : ProjectAnalysis.revise(previous, roots)
  return yield* effect.pipe(Effect.provide(SourceResolver.memory(currentSources)))
})

const observation = (project: ProjectAnalysis.ProjectAnalysis, module: string) =>
  SemanticInvalidation.observation(project.semanticInvalidation, module) ??
  assert.fail(`missing semantic observation ${module}`)

const expectReusable = (project: ProjectAnalysis.ProjectAnalysis, module: string): void =>
  assert.strictEqual(observation(project, module)._tag, 'Reusable', `${module} should be reusable`)

const expectReasons = (
  project: ProjectAnalysis.ProjectAnalysis,
  module: string,
  reasons: ReadonlyArray<SemanticInvalidation.Reason>,
): void => {
  const found = observation(project, module)
  assert.strictEqual(found._tag, 'Recomputed', `${module} should recompute`)
  if (found._tag === 'Recomputed') assert.deepEqual(found.reasons, reasons)
}

it.effect('keeps unrelated modules and body-only importers reusable', () =>
  Effect.gen(function* () {
    const importer = 'import app.B { answer } pub fn use() -> i32 { return answer() }'
    const dependencyAfter = '\n\npub fn answer() -> i32 { return 99 }'
    const before = sources({
      'app/A': importer,
      'app/B': 'pub fn answer() -> i32 { return 1 }',
      'app/C': 'pub fn other() -> i32 { return 2 }',
    })
    const previous = yield* analyze(before, ['app/A', 'app/C'])
    const after = sources({
      'app/A': importer,
      'app/B': dependencyAfter,
      'app/C': 'pub fn other() -> i32 { return 200 }',
    })
    const current = yield* analyze(after, ['app/A', 'app/C'], previous)

    expectReusable(current, 'app/A')
    expectReasons(current, 'app/B', ['LocalChange'])
    expectReasons(current, 'app/C', ['LocalChange'])
    assert.strictEqual(current.semantics.get('app/A'), previous.semantics.get('app/A'))
    assert.notStrictEqual(current.semantics.get('app/B'), previous.semantics.get('app/B'))
    assert.notStrictEqual(current.semantics.get('app/C'), previous.semantics.get('app/C'))
    assert.strictEqual(current.toolingModules.get('app/A'), previous.toolingModules.get('app/A'))
    assert.notStrictEqual(current.toolingModules.get('app/B'), previous.toolingModules.get('app/B'))
    assert.notStrictEqual(current.toolingModules.get('app/C'), previous.toolingModules.get('app/C'))
    const view = ProjectAnalysis.view(current, 'app/A') ?? assert.fail('missing app/A view')
    const call = Analysis.semanticOccurrenceAt(view, 'app/A', importer.lastIndexOf('answer'))
    assert.strictEqual(call?.declaration?.module, 'app/B')
    assert.strictEqual(call?.declaration?.selectionSpan.start, dependencyAfter.indexOf('answer'))
    assert.deepEqual(current.semanticInvalidation.totals, {
      modules: 3,
      reusable: 1,
      recomputed: 2,
      reasons: {
        Fresh: 0,
        LocalChange: 2,
        OpaqueBodyChange: 0,
        OpaqueTargetChange: 0,
        OpaqueLayoutChange: 0,
        DependencySurfaceChange: 0,
        CyclicPeerChange: 0,
        EnvironmentChange: 0,
        SurfaceChange: 0,
      },
    })
  }),
)

it.effect('invalidates importers for signatures, visibility, and nominal struct shape', () =>
  Effect.gen(function* () {
    const cases = [
      [
        'pub fn answer() -> i32 { return 1 }',
        'pub fn answer(value: i32) -> i32 { return value }',
        'import app.Dependency { answer } pub fn use() -> i32 { return answer() }',
      ],
      [
        'pub fn answer() -> i32 { return 1 }',
        'fn answer() -> i32 { return 1 }',
        'import app.Dependency { answer } pub fn use() -> i32 { return answer() }',
      ],
      [
        'pub struct Pair { pub left: i32 }',
        'pub struct Pair { pub left: i32 pub right: i32 }',
        'import app.Dependency { Pair } pub fn use(value: Pair) -> Pair { return value }',
      ],
    ] as const

    for (const [dependencyBefore, dependencyAfter, importer] of cases) {
      const previous = yield* analyze(
        sources({ 'app/Importer': importer, 'app/Dependency': dependencyBefore }),
        ['app/Importer'],
      )
      const current = yield* analyze(
        sources({ 'app/Importer': importer, 'app/Dependency': dependencyAfter }),
        ['app/Importer'],
        previous,
      )
      expectReasons(current, 'app/Dependency', ['LocalChange'])
      const importerObservation = observation(current, 'app/Importer')
      assert.strictEqual(importerObservation._tag, 'Recomputed')
      if (importerObservation._tag === 'Recomputed')
        assert.include(importerObservation.reasons, 'DependencySurfaceChange')
      assert.notStrictEqual(
        current.semantics.get('app/Importer'),
        previous.semantics.get('app/Importer'),
      )
      assert.notStrictEqual(
        current.toolingModules.get('app/Importer'),
        previous.toolingModules.get('app/Importer'),
      )
    }
  }),
)

it.effect('stops public-surface propagation when an intermediate module stabilizes', () =>
  Effect.gen(function* () {
    const before = sources({
      'chain/A': 'import chain.B { middle } pub fn top() -> i32 { return middle() }',
      'chain/B': 'import chain.C { bottom } pub fn middle() -> i32 { return bottom() }',
      'chain/C': 'pub fn bottom() -> i32 { return 1 }',
    })
    const previous = yield* analyze(before, ['chain/A'])
    const bodyOnly = yield* analyze(
      sources({
        'chain/A': 'import chain.B { middle } pub fn top() -> i32 { return middle() }',
        'chain/B': 'import chain.C { bottom } pub fn middle() -> i32 { return bottom() }',
        'chain/C': 'pub fn bottom() -> i32 { return 2 }',
      }),
      ['chain/A'],
      previous,
    )
    expectReasons(bodyOnly, 'chain/C', ['LocalChange'])
    expectReusable(bodyOnly, 'chain/B')
    expectReusable(bodyOnly, 'chain/A')
    assert.notStrictEqual(bodyOnly.semantics.get('chain/C'), previous.semantics.get('chain/C'))
    assert.strictEqual(bodyOnly.semantics.get('chain/B'), previous.semantics.get('chain/B'))
    assert.strictEqual(bodyOnly.semantics.get('chain/A'), previous.semantics.get('chain/A'))

    const signatureChange = yield* analyze(
      sources({
        'chain/A': 'import chain.B { middle } pub fn top() -> i32 { return middle() }',
        'chain/B': 'import chain.C { bottom } pub fn middle() -> i32 { return bottom() }',
        'chain/C': 'pub fn bottom(value: i32) -> i32 { return value }',
      }),
      ['chain/A'],
      previous,
    )
    expectReasons(signatureChange, 'chain/C', ['LocalChange'])
    expectReasons(signatureChange, 'chain/B', ['DependencySurfaceChange'])
    expectReusable(signatureChange, 'chain/A')
    assert.notStrictEqual(
      signatureChange.semantics.get('chain/B'),
      previous.semantics.get('chain/B'),
    )
    assert.strictEqual(signatureChange.semantics.get('chain/A'), previous.semantics.get('chain/A'))
  }),
)

it.effect('classifies fresh, removed, changed import, and malformed revisions conservatively', () =>
  Effect.gen(function* () {
    const previous = yield* analyze(
      sources({
        'app/Main': 'import app.Dependency { answer } pub fn main() -> i32 { return answer() }',
        'app/Dependency': 'pub fn answer() -> i32 { return 1 }',
        'app/Removed': 'pub fn removed() -> i32 { return 0 }',
      }),
      ['app/Main', 'app/Removed'],
    )
    const missingDependency = yield* analyze(
      sources({
        'app/Main': 'import app.Dependency { answer } pub fn main() -> i32 { return answer() }',
        'app/Fresh': 'pub fn fresh() -> i32 { return 2 }',
      }),
      ['app/Main', 'app/Fresh'],
      previous,
    )
    expectReasons(missingDependency, 'app/Main', ['DependencySurfaceChange'])
    expectReasons(missingDependency, 'app/Fresh', ['Fresh'])
    assert.strictEqual(
      SemanticInvalidation.observation(missingDependency.semanticInvalidation, 'app/Removed'),
      undefined,
    )

    const malformed = yield* analyze(
      sources({ 'app/Main': 'pub fn main(value: ) -> { return 1 }' }),
      ['app/Main'],
      previous,
    )
    const malformedObservation = observation(malformed, 'app/Main')
    assert.strictEqual(malformedObservation._tag, 'Recomputed')
    if (malformedObservation._tag === 'Recomputed') {
      assert.include(malformedObservation.reasons, 'LocalChange')
      assert.strictEqual(malformedObservation.surfaceChanged, true)
    }
    assert.notStrictEqual(malformed.semantics.get('app/Main'), previous.semantics.get('app/Main'))
  }),
)

it.effect(
  'recomputes SCC peers and keeps dependents reusable when cycle surfaces stay stable',
  () =>
    Effect.gen(function* () {
      const before = sources({
        'cycle/A': 'import cycle.B { b } pub fn a() -> i32 { return b() }',
        'cycle/B': 'import cycle.A { a } pub fn b() -> i32 { return 1 }',
        'cycle/C': 'import cycle.A { a } pub fn c() -> i32 { return a() }',
      })
      const previous = yield* analyze(before, ['cycle/C'])
      const current = yield* analyze(
        sources({
          'cycle/A': 'import cycle.B { b } pub fn a() -> i32 { return b() }',
          'cycle/B': 'import cycle.A { a } pub fn b() -> i32 { return 99 }',
          'cycle/C': 'import cycle.A { a } pub fn c() -> i32 { return a() }',
        }),
        ['cycle/C'],
        previous,
      )

      expectReasons(current, 'cycle/B', ['LocalChange'])
      expectReasons(current, 'cycle/A', ['CyclicPeerChange'])
      expectReusable(current, 'cycle/C')
      assert.notStrictEqual(current.semantics.get('cycle/A'), previous.semantics.get('cycle/A'))
      assert.notStrictEqual(current.semantics.get('cycle/B'), previous.semantics.get('cycle/B'))
      assert.strictEqual(current.semantics.get('cycle/C'), previous.semantics.get('cycle/C'))
    }),
)

it.effect('uses the union revision graph for SCC merges and splits', () =>
  Effect.gen(function* () {
    const splitPrevious = yield* analyze(
      sources({
        'graph/A': 'import graph.B { b } pub fn a() -> i32 { return b() }',
        'graph/B': 'import graph.A { a } pub fn b() -> i32 { return 1 }',
      }),
      ['graph/A'],
    )
    const split = yield* analyze(
      sources({
        'graph/A': 'pub fn a() -> i32 { return 1 }',
        'graph/B': 'import graph.A { a } pub fn b() -> i32 { return 1 }',
      }),
      ['graph/B'],
      splitPrevious,
    )
    const splitB = observation(split, 'graph/B')
    assert.strictEqual(splitB._tag, 'Recomputed')
    if (splitB._tag === 'Recomputed') assert.include(splitB.reasons, 'CyclicPeerChange')

    const mergePrevious = yield* analyze(
      sources({
        'graph/A': 'import graph.B { b } pub fn a() -> i32 { return b() }',
        'graph/B': 'pub fn b() -> i32 { return 1 }',
      }),
      ['graph/A'],
    )
    const merge = yield* analyze(
      sources({
        'graph/A': 'import graph.B { b } pub fn a() -> i32 { return b() }',
        'graph/B': 'import graph.A { a } pub fn b() -> i32 { return 1 }',
      }),
      ['graph/A'],
      mergePrevious,
    )
    expectReasons(merge, 'graph/B', ['LocalChange', 'DependencySurfaceChange'])
    expectReasons(merge, 'graph/A', ['CyclicPeerChange'])
  }),
)

it.effect('keeps plans deterministic under root and source-map reordering', () =>
  Effect.gen(function* () {
    const beforeEntries = {
      'order/A': 'import order.B { b } pub fn a() -> i32 { return b() }',
      'order/B': 'pub fn b() -> i32 { return 1 }',
      'order/C': 'pub fn c() -> i32 { return 2 }',
    }
    const previous = yield* analyze(sources(beforeEntries), ['order/A', 'order/C'])
    const afterEntries = {
      ...beforeEntries,
      'order/B': 'pub fn b(value: i32) -> i32 { return value }',
    }
    const first = yield* analyze(sources(afterEntries), ['order/A', 'order/C'], previous)
    const reversedSources = new Map([...sources(afterEntries)].reverse())
    const second = yield* analyze(reversedSources, ['order/C', 'order/A'], previous)

    assert.deepEqual(first.semanticInvalidation, second.semanticInvalidation)
  }),
)

it.effect('bounds value-only opaque edits to the producer body', () =>
  Effect.gen(function* () {
    const importer =
      'import opaque.Dependency { make } pub fn use() -> i32 { let parser = make(1) return parser(2) }'
    const beforeDependency = `fn add(left: i32, right: i32) -> i32 { return left + right }
pub fn make(value: i32) -> some<F: fn(i32) -> i32> F { return add(value) }`
    const previous = yield* analyze(
      sources({ 'opaque/Importer': importer, 'opaque/Dependency': beforeDependency }),
      ['opaque/Importer'],
    )
    const current = yield* analyze(
      sources({
        'opaque/Importer': importer,
        'opaque/Dependency': `fn add(left: i32, right: i32) -> i32 { return left + right }
pub fn make(value: i32) -> some<F: fn(i32) -> i32> F { return add(value + 1) }`,
      }),
      ['opaque/Importer'],
      previous,
    )

    expectReasons(current, 'opaque/Dependency', ['LocalChange', 'OpaqueBodyChange'])
    expectReusable(current, 'opaque/Importer')
    const before = [...OpaqueRealization.catalogOf(previous).definitions.values()].at(0)
    const after = [...OpaqueRealization.catalogOf(current).definitions.values()].at(0)
    assert.notStrictEqual(before?.bodyFingerprint, after?.bodyFingerprint)
    assert.strictEqual(before?.targetFingerprint, after?.targetFingerprint)
    assert.strictEqual(before?.layoutFingerprint, after?.layoutFingerprint)
  }),
)

it.effect('invalidates opaque dependents for target-only edits while retaining layout', () =>
  Effect.gen(function* () {
    const importer =
      'import opaque.Dependency { make } pub fn use() -> i32 { let parser = make(1) return parser(2) }'
    const declarations = `fn add(left: i32, right: i32) -> i32 { return left + right }
fn multiply(left: i32, right: i32) -> i32 { return left * right }`
    const previous = yield* analyze(
      sources({
        'opaque/Importer': importer,
        'opaque/Dependency': `${declarations}
pub fn make(value: i32) -> some<F: fn(i32) -> i32> F { return add(value) }`,
      }),
      ['opaque/Importer'],
    )
    const current = yield* analyze(
      sources({
        'opaque/Importer': importer,
        'opaque/Dependency': `${declarations}
pub fn make(value: i32) -> some<F: fn(i32) -> i32> F { return multiply(value) }`,
      }),
      ['opaque/Importer'],
      previous,
    )

    expectReasons(current, 'opaque/Dependency', [
      'LocalChange',
      'OpaqueBodyChange',
      'OpaqueTargetChange',
    ])
    expectReasons(current, 'opaque/Importer', ['OpaqueTargetChange'])
    const before = [...OpaqueRealization.catalogOf(previous).definitions.values()].at(0)
    const after = [...OpaqueRealization.catalogOf(current).definitions.values()].at(0)
    assert.notStrictEqual(before?.targetFingerprint, after?.targetFingerprint)
    assert.strictEqual(before?.layoutFingerprint, after?.layoutFingerprint)
  }),
)

it.effect('invalidates opaque dependents for capture-shape and suspendability edits', () =>
  Effect.gen(function* () {
    const importer =
      'import opaque.Dependency { make } pub fn use() -> i32 { let pending = make(1, 2) return run pending }'
    const before = yield* analyze(
      sources({
        'opaque/Importer': importer,
        'opaque/Dependency':
          'pub fn make(left: i32, right: i32) -> some<F: Effect<i32>> F { return effect { return left } }',
      }),
      ['opaque/Importer'],
    )
    const captureShape = yield* analyze(
      sources({
        'opaque/Importer': importer,
        'opaque/Dependency':
          'pub fn make(left: i32, right: i32) -> some<F: Effect<i32>> F { return effect { return left + right } }',
      }),
      ['opaque/Importer'],
      before,
    )
    expectReasons(captureShape, 'opaque/Importer', ['OpaqueLayoutChange'])

    const suspendImporter =
      'import opaque.Suspend { make, pending } pub fn use() -> i32 { let recipe = make(pending()) return run recipe }'
    const suspendBefore = yield* analyze(
      sources({
        'opaque/SuspendImporter': suspendImporter,
        'opaque/Suspend': `pub fn pending() -> some<P: Effect<i32>> P { return effect { return 1 } }
pub fn make<F: Effect<i32>>(pending: F) -> some<G: Effect<i32>> G {
  return effect { drop pending return 1 }
}`,
      }),
      ['opaque/SuspendImporter'],
    )
    const suspendAfter = yield* analyze(
      sources({
        'opaque/SuspendImporter': suspendImporter,
        'opaque/Suspend': `pub fn pending() -> some<P: Effect<i32>> P { return effect { return 1 } }
pub fn make<F: Effect<i32>>(pending: F) -> some<G: Effect<i32>> G {
  return effect { return run pending }
}`,
      }),
      ['opaque/SuspendImporter'],
      suspendBefore,
    )
    expectReasons(suspendAfter, 'opaque/SuspendImporter', ['OpaqueLayoutChange'])
    const beforeDefinition = [
      ...OpaqueRealization.catalogOf(suspendBefore).definitions.values(),
    ].find((definition) => definition.family.producer.name === 'make')
    const afterDefinition = [
      ...OpaqueRealization.catalogOf(suspendAfter).definitions.values(),
    ].find((definition) => definition.family.producer.name === 'make')
    assert.strictEqual(beforeDefinition?.suspendable, false)
    assert.strictEqual(afterDefinition?.suspendable, true)
  }),
)

it.effect('uses public-surface invalidation for opaque bound and binder-order edits', () =>
  Effect.gen(function* () {
    const before = yield* analyze(
      sources({
        'opaque/BoundImporter':
          'import opaque.Bound { make } pub fn use() -> i32 { let parser = make() return parser(1) }',
        'opaque/Bound': `fn identity(value: i32) -> i32 { return value }
pub fn make() -> some<F: fn(i32) -> i32> F { return identity }`,
      }),
      ['opaque/BoundImporter'],
    )
    const changed = yield* analyze(
      sources({
        'opaque/BoundImporter':
          'import opaque.Bound { make } pub fn use() -> i32 { let parser = make() return parser(1) }',
        'opaque/Bound': `fn identity(value: i64) -> i64 { return value }
pub fn make<T>() -> some<F: fn(i64) -> i64> F { return identity }`,
      }),
      ['opaque/BoundImporter'],
      before,
    )
    const importer = observation(changed, 'opaque/BoundImporter')
    assert.strictEqual(importer._tag, 'Recomputed')
    if (importer._tag === 'Recomputed') assert.include(importer.reasons, 'DependencySurfaceChange')
  }),
)
