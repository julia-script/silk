import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as Result from 'effect/Result'
import * as Analysis from '../src/Analysis.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceOrigin from '../src/SourceOrigin.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as Tir from '../src/Tir.js'

const encoder = new TextEncoder()

const source = (module: string, text: string) =>
  SourceFile.make(
    module,
    encoder.encode(text),
    SourceOrigin.projectFile(`/project/src/${module}.silk`),
  )

const snapshot = (
  body: string,
  helper: string,
  options: { readonly prelude?: string; readonly qualifier?: 'test' | 'pub test' } = {},
) => {
  const files = [
    source('runner/Main', 'import runner.Support\npub fn main() -> () {}'),
    source('runner/Support', 'test fn runnerOnly() -> () {}'),
    source('suite/Root', 'import suite.Cases\nimport suite.Bridge\nimport dependency.Tests'),
    source('suite/Bridge', 'import suite.Cases\nimport suite.More\nimport suite.Root'),
    source(
      'suite/Cases',
      `${options.prelude ?? ''}${options.qualifier ?? 'test'} fn alpha() -> () { ${body} }
test fn beta() -> () { helper() }
static if false { test fn inactive() {} }
fn helper() -> () { ${helper} }`,
    ),
    source('suite/More', `test fn alpha() -> () { ${body} }`),
    source('dependency/Tests', 'test fn dependencyOnly() -> () {}'),
    source('suite/Neighbor', 'test fn unimported() -> () {}'),
  ]
  return Analysis.make({
    root: 'runner/Main',
    target: 'x86_64-unknown-linux-gnu',
    discovery: {
      root: 'suite/Root',
      logicalRoot: 'src',
      sources: new Map([
        [
          'dependency/Tests',
          { ownership: 'Dependency' as const, logicalPath: 'vendor/dependency/Tests.silk' },
        ],
      ]),
    },
  }).pipe(
    Effect.provide(SourceResolver.overlay(files).pipe(Layer.provideMerge(SourceResolver.empty))),
  )
}

const fingerprintSnapshot = (text: string) =>
  Analysis.make({
    root: 'Cases',
    target: 'x86_64-unknown-linux-gnu',
    discovery: { root: 'Cases' },
  }).pipe(
    Effect.provide(
      SourceResolver.overlay([source('Cases', text)]).pipe(
        Layer.provideMerge(SourceResolver.empty),
      ),
    ),
  )

it.effect('builds a deterministic project-owned catalog from only the discovery-root closure', () =>
  Effect.gen(function* () {
    const analysis = yield* snapshot('let value = 1 drop value', 'let value = 2 drop value')
    assert.deepEqual(analysis.diagnostics, [])
    const catalog = analysis.testCatalog
    assert.isDefined(catalog)
    assert.deepEqual(
      catalog?.entries.map((entry) => [entry.info.module, entry.info.name, entry.info.path]),
      [
        ['suite/Cases', 'alpha', 'src/suite/Cases.silk'],
        ['suite/Cases', 'beta', 'src/suite/Cases.silk'],
        ['suite/More', 'alpha', 'src/suite/More.silk'],
      ],
    )
    assert.match(catalog?.entries[0]?.info.fingerprint ?? '', /^silk-test-v1:[0-9a-f]{64}$/)
    assert.notStrictEqual(catalog?.entries[0]?.info.identity, catalog?.entries[2]?.info.identity)
  }),
)

it.effect(
  'fingerprints local authored test content without treating helper bodies as dependencies',
  () =>
    Effect.gen(function* () {
      const before = yield* fingerprintSnapshot(`test fn alpha() -> () { let value = 1 drop value }
test fn beta() -> () { helper() }
fn helper() -> () { let value = 2 drop value }`)
      const bodyAndHelperEdit = yield* fingerprintSnapshot(
        `test fn alpha() -> () { let value = 3 drop value }
test fn beta() -> () { helper() }
fn helper() -> () { let value = 4 drop value }`,
      )
      const movedAndTriviaEdit = yield* fingerprintSnapshot(`

fn unrelated() {}
test fn alpha() -> () { let value=1
  drop value }
test fn beta() -> () { helper() }
fn helper() -> () { let value = 2 drop value }`)
      const fingerprints = (analysis: Analysis.SingleRootFrontendSnapshot) =>
        new Map(
          analysis.testCatalog?.entries.map((entry) => [
            `${entry.info.module}:${entry.info.name}`,
            entry.info.fingerprint,
          ]),
        )
      assert.notStrictEqual(
        fingerprints(before).get('Cases:alpha'),
        fingerprints(bodyAndHelperEdit).get('Cases:alpha'),
      )
      assert.strictEqual(
        fingerprints(before).get('Cases:beta'),
        fingerprints(bodyAndHelperEdit).get('Cases:beta'),
      )
      assert.strictEqual(
        fingerprints(before).get('Cases:alpha'),
        fingerprints(movedAndTriviaEdit).get('Cases:alpha'),
      )
      assert.notStrictEqual(
        before.testCatalog?.entries[0]?.info.line,
        movedAndTriviaEdit.testCatalog?.entries[0]?.info.line,
      )
      assert.notStrictEqual(before.testCatalog?.identity, bodyAndHelperEdit.testCatalog?.identity)
      assert.notStrictEqual(before.testCatalog?.identity, movedAndTriviaEdit.testCatalog?.identity)
    }),
)

it.effect('requires the explicit discovery root', () =>
  Effect.gen(function* () {
    const attempted = yield* Effect.result(
      Analysis.make({
        root: 'runner/Main',
        discovery: { root: 'suite/Missing' },
      }).pipe(
        Effect.provide(
          SourceResolver.overlay([source('runner/Main', 'pub fn main() -> () {}')]).pipe(
            Layer.provideMerge(SourceResolver.empty),
          ),
        ),
      ),
    )
    assert.isTrue(Result.isFailure(attempted))
    if (Result.isFailure(attempted))
      assert.strictEqual(attempted.failure.reason._tag, 'MissingRoot')
  }),
)

it.effect('requires ownership and logical-path facts for reachable in-memory sources', () =>
  Effect.gen(function* () {
    const attempted = yield* Effect.result(
      Analysis.make({
        root: 'Runner',
        discovery: { root: 'MemoryTests' },
      }).pipe(
        Effect.provide(
          SourceResolver.overlay([
            source('Runner', 'pub fn main() -> () {}'),
            SourceFile.make(
              'MemoryTests',
              encoder.encode('test fn omitted() {}'),
              SourceOrigin.memory('memory://tests'),
            ),
          ]).pipe(Layer.provideMerge(SourceResolver.empty)),
        ),
      ),
    )

    assert.isTrue(Result.isFailure(attempted))
    if (Result.isFailure(attempted)) {
      assert.strictEqual(attempted.failure._tag, 'ModuleClosureError')
      assert.deepEqual(attempted.failure.reason, {
        _tag: 'MissingDiscoverySource',
        module: 'MemoryTests',
      })
    }
  }),
)

it.effect('does not retain an uncalled test in an ordinary executable', () =>
  Effect.gen(function* () {
    const analysis = yield* Analysis.makeRealized({
      root: 'Main',
      target: 'wasm32-unknown-unknown',
    }).pipe(
      Effect.provide(
        SourceResolver.overlay([
          source('Main', 'test fn unused() {}\npub fn main() -> i32 { return 0 }'),
        ]).pipe(Layer.provideMerge(SourceResolver.empty)),
      ),
    )

    assert.deepEqual(Analysis.diagnostics(analysis), [])
    assert.notInclude(
      Analysis.loweredMir(analysis).functions.map((function_) => function_.id.name),
      'unused',
    )
  }),
)

it.effect('rejects catalog iteration when realization has no discovery context', () =>
  Effect.gen(function* () {
    const analysis = yield* Analysis.makeRealized({
      root: 'Main',
      target: 'wasm32-unknown-unknown',
    }).pipe(
      Effect.provide(
        SourceResolver.overlay([
          source(
            'Main',
            `pub fn main() -> () {
  static for descriptor in Intrinsic.tests() { drop descriptor }
}`,
          ),
        ]).pipe(Layer.provideMerge(SourceResolver.empty)),
      ),
    )

    assert.include(
      Analysis.diagnostics(analysis).map((diagnostic) => diagnostic.code),
      'SEM0176',
    )
  }),
)

it.effect('specializes a private test callable without retaining discovery values', () =>
  Effect.gen(function* () {
    const analysis = yield* Analysis.makeRealized({
      root: 'Runner',
      target: 'wasm32-unknown-unknown',
      discovery: { root: 'Runner' },
    }).pipe(
      Effect.provide(
        SourceResolver.overlay([
          source(
            'Runner',
            `test fn privateTest() {}
pub fn main() -> () {
  static for descriptor in Intrinsic.tests() {
    let body = Intrinsic.testFunction(descriptor)
    body()
  }
}`,
          ),
        ]).pipe(Layer.provideMerge(SourceResolver.empty)),
      ),
    )

    assert.deepEqual(Analysis.diagnostics(analysis), [])
    const tir = Tir.encode(Analysis.rootAnalysis(analysis).tir)
    const mirStrings: string[] = []
    const pending: unknown[] = [Analysis.loweredMir(analysis)]
    while (pending.length > 0) {
      const value = pending.pop()
      if (typeof value === 'string') mirStrings.push(value)
      else if (typeof value === 'object' && value !== null)
        for (const child of Object.values(value)) pending.push(child)
    }
    const mir = mirStrings.join('\n')
    assert.include(tir, 'privateTest')
    assert.include(mir, 'privateTest')
    for (const phaseOnly of ['Intrinsic.tests', 'Intrinsic.testFunction', 'TestDescriptorValue']) {
      assert.isFalse(tir.includes(phaseOnly), tir)
      assert.isFalse(mir.includes(phaseOnly), mir)
    }
  }),
)
