import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Analysis from '../src/Analysis.js'
import * as ProjectAnalysis from '../src/ProjectAnalysis.js'
import * as SourceFile from '../src/SourceFile.js'
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
