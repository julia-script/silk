import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Analysis from '../src/Analysis.js'
import * as ProjectAnalysis from '../src/ProjectAnalysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceOrigin from '../src/SourceOrigin.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const roots = Object.freeze([
  SourceFile.make('app/A', ascii('import shared.Core\npub fn a() -> i32 { return Core.answer() }')),
  SourceFile.make('app/B', ascii('import shared.Core\npub fn b() -> i32 { return Core.answer() }')),
])

const sources = new Map([['shared/Core', ascii('pub fn answer() -> i32 { return 42 }')]])

const make = (requestedRoots = roots) =>
  ProjectAnalysis.make(requestedRoots).pipe(Effect.provide(SourceResolver.memory(sources)))

const deterministicReport = (self: ProjectAnalysis.ProjectAnalysis) =>
  ProjectAnalysis.phases(self).map(({ phase, inputs, outputs, diagnostics }) => ({
    phase,
    inputs,
    outputs,
    diagnostics,
  }))

it.effect('analyzes a shared dependency once and derives structurally shared root views', () =>
  Effect.gen(function* () {
    const project = yield* make()
    const left = ProjectAnalysis.view(project, 'app/A')
    const right = ProjectAnalysis.view(project, 'app/B')

    assert.isDefined(left)
    assert.isDefined(right)
    if (left === undefined || right === undefined) return
    assert.deepEqual(project.roots, ['app/A', 'app/B'])
    assert.strictEqual(left.closure.rootModule, 'app/A')
    assert.strictEqual(right.closure.rootModule, 'app/B')
    assert.strictEqual(left.closure.modules, right.closure.modules)
    assert.strictEqual(left.closure.sources, right.closure.sources)
    assert.strictEqual(left.index, right.index)
    assert.strictEqual(left.resolution, right.resolution)
    assert.strictEqual(left.results, right.results)
    assert.strictEqual(left.ownership, right.ownership)
    assert.strictEqual(left.semanticOccurrences, right.semanticOccurrences)
    assert.strictEqual(left.anonymousExpressions, right.anonymousExpressions)
    assert.strictEqual(Analysis.phases(left), Analysis.phases(right))
    assert.deepEqual(
      Analysis.modules(left).map(({ name }) => name),
      ['app/A', 'app/B', 'shared/Core'],
    )
    assert.strictEqual(Analysis.declarationByName(left, 'app/A', 'a')._tag, 'Resolved')
    assert.strictEqual(Analysis.declarationByName(right, 'app/B', 'b')._tag, 'Resolved')
    assert.deepEqual(Analysis.diagnostics(left), [])
    assert.deepEqual(
      deterministicReport(project).map(({ phase }) => phase),
      [
        'closure',
        'declaration-collection',
        'declaration-index',
        'name-resolution',
        'elaboration',
        'ownership',
        'semantic-occurrences',
        'anonymous-expressions',
      ],
    )
    assert.deepEqual(deterministicReport(project).at(0), {
      phase: 'closure',
      inputs: 2,
      outputs: 3,
      diagnostics: 0,
    })
  }),
)

it.effect('keeps project facts deterministic when root supply order changes', () =>
  Effect.gen(function* () {
    const first = yield* make()
    const second = yield* make([...roots].reverse())

    assert.deepEqual(first.roots, second.roots)
    assert.deepEqual(
      first.closure.modules.map(({ name }) => name),
      second.closure.modules.map(({ name }) => name),
    )
    assert.deepEqual(deterministicReport(first), deterministicReport(second))
    assert.deepEqual(
      Analysis.diagnostics(ProjectAnalysis.view(first, 'app/A') ?? assert.fail('missing app/A')),
      Analysis.diagnostics(ProjectAnalysis.view(second, 'app/A') ?? assert.fail('missing app/A')),
    )
  }),
)

it.effect('rejects conflicting source bytes for one canonical root', () =>
  Effect.gen(function* () {
    const exit = yield* Effect.exit(
      make([
        SourceFile.make('app/Main', ascii('pub fn main() -> i32 { return 1 }')),
        SourceFile.make('app/Main', ascii('pub fn main() -> i32 { return 2 }')),
      ]),
    )
    assert.strictEqual(Exit.isFailure(exit), true)
  }),
)

it.effect('reuses exact unchanged syntax while rebuilding one coherent semantic frontend', () =>
  Effect.gen(function* () {
    const previous = yield* make()
    const revisedRoots = Object.freeze([
      SourceFile.make(
        'app/A',
        ascii(
          'import shared.Core\npub fn inserted() -> i32 { return 0 }\npub fn a() -> i32 { return Core.answer() }',
        ),
      ),
      roots.at(1) ?? assert.fail('missing app/B root'),
    ])
    const current = yield* ProjectAnalysis.revise(previous, revisedRoots).pipe(
      Effect.provide(SourceResolver.memory(sources)),
    )
    const previousModules = new Map(
      previous.closure.modules.map((module) => [module.name, module.syntax]),
    )
    const currentModules = new Map(
      current.closure.modules.map((module) => [module.name, module.syntax]),
    )

    assert.notStrictEqual(currentModules.get('app/A'), previousModules.get('app/A'))
    assert.strictEqual(currentModules.get('app/B'), previousModules.get('app/B'))
    assert.strictEqual(currentModules.get('shared/Core'), previousModules.get('shared/Core'))
    assert.strictEqual(current.syntaxRevisions.get('app/A')?._tag, 'Changed')
    assert.strictEqual(current.syntaxRevisions.get('app/B')?._tag, 'Reused')
    assert.strictEqual(current.syntaxRevisions.get('shared/Core')?._tag, 'Reused')
    const changed = current.syntaxRevisions.get('app/A')
    assert.strictEqual(changed?._tag, 'Changed')
    if (changed?._tag === 'Changed')
      assert.isAbove(changed.correspondence.counts.correspondingElements, 0)

    const previousView = ProjectAnalysis.view(previous, 'app/A')
    const currentView = ProjectAnalysis.view(current, 'app/A')
    assert.isDefined(previousView)
    assert.isDefined(currentView)
    if (previousView === undefined || currentView === undefined) return
    assert.notStrictEqual(currentView.index, previousView.index)
    assert.notStrictEqual(currentView.resolution, previousView.resolution)
    assert.notStrictEqual(currentView.results, previousView.results)
    assert.notStrictEqual(currentView.ownership, previousView.ownership)
    assert.notStrictEqual(currentView.semanticOccurrences, previousView.semanticOccurrences)
    assert.deepEqual(Analysis.diagnostics(currentView), [])
  }),
)

it.effect('reports fresh and removed modules and refuses reuse across source origins', () =>
  Effect.gen(function* () {
    const previousRoot = SourceFile.make(
      'app/Main',
      ascii('pub fn main() -> i32 { return 1 }'),
      SourceOrigin.memory('file:///old/Main.silk'),
    )
    const previous = yield* ProjectAnalysis.make([previousRoot]).pipe(
      Effect.provide(SourceResolver.empty),
    )
    const current = yield* ProjectAnalysis.revise(previous, [
      SourceFile.make(
        'app/Next',
        ascii('pub fn next() -> i32 { return 2 }'),
        SourceOrigin.memory('file:///new/Next.silk'),
      ),
    ]).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(current.syntaxRevisions.has('app/Main'), false)
    assert.strictEqual(current.syntaxRevisions.get('app/Next')?._tag, 'Fresh')

    const changedOrigin = yield* ProjectAnalysis.revise(previous, [
      SourceFile.make(
        'app/Main',
        ascii('pub fn main() -> i32 { return 1 }'),
        SourceOrigin.memory('file:///new/Main.silk'),
      ),
    ]).pipe(Effect.provide(SourceResolver.empty))
    assert.strictEqual(changedOrigin.syntaxRevisions.get('app/Main')?._tag, 'Changed')
    assert.notStrictEqual(
      changedOrigin.closure.modules.at(0)?.syntax,
      previous.closure.modules.at(0)?.syntax,
    )
  }),
)
