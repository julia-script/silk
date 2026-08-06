import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): Effect.Effect<Analysis.Snapshot> => {
  const rootText = entries.find(([name]) => name === rootModule)?.[1]
  if (rootText === undefined) throw new RangeError(`Fixture has no root source ${rootModule}`)
  const imports = new Map(
    entries
      .filter(([name]) => name !== rootModule)
      .map(([name, text]) => [name, ascii(text)] as const),
  )
  return Analysis.make({ root: SourceFile.make(rootModule, ascii(rootText)) }).pipe(
    Effect.provide(SourceResolver.memory(imports)),
  )
}

it.effect('answers multi-module queries from one snapshot', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('root', [
      ['root', 'import lib\npub fn main() -> I32 { return 42 }'],
      ['lib', 'pub fn answer() -> I32 { return 1 }'],
    ])
    assert.deepEqual(
      Analysis.modules(self).map((module) => module.name),
      ['lib', 'root'],
    )
    assert.strictEqual(Analysis.syntaxOf(self, 'lib')?.source.id, 'lib')
    assert.strictEqual(Analysis.declarationByName(self, 'lib', 'answer')._tag, 'Resolved')
    assert.strictEqual(Analysis.declarationByName(self, 'root', 'answer')._tag, 'Missing')
    assert.strictEqual(Analysis.hirOf(self, 'root')?.functions.length, 1)
    assert.strictEqual(Analysis.moduleAnalysis(self, 'absent'), undefined)
    assert.deepEqual(Analysis.cycles(self), [])
    assert.deepEqual([...Analysis.sources(self).keys()], ['lib', 'root'])
  }),
)

it.effect('merges diagnostics while keeping unrelated facts queryable', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('root', [
      ['root', 'import lib\nimport missing\npub fn main( -> Mystery { return @ 42 }'],
      ['lib', 'pub fn answer() -> I32 { return 1 }'],
    ])
    assert.strictEqual(Analysis.declarationByName(self, 'lib', 'answer')._tag, 'Resolved')
    const libFunction = Analysis.hirOf(self, 'lib')?.functions.at(0)
    assert.strictEqual(
      libFunction === undefined ? undefined : Hir.returned(libFunction)._tag,
      'IntegerLiteral',
    )
    assert.include(
      Analysis.diagnostics(self).map((diagnostic) => diagnostic.code),
      'MOD0001',
    )
  }),
)

it.effect('answers repeated snapshots deterministically', () =>
  Effect.gen(function* () {
    const entries: ReadonlyArray<readonly [string, string]> = [
      ['root', 'import lib\npub fn main() -> I32 { return 42 }'],
      ['lib', 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }'],
    ]
    const first = yield* snapshot('root', entries)
    const second = yield* snapshot('root', [...entries].reverse())
    assert.deepEqual(first.closure, second.closure)
    assert.deepEqual(first.index, second.index)
    assert.deepEqual(Analysis.diagnostics(first), Analysis.diagnostics(second))
  }),
)

it.effect('evaluates and answers ownership through the single-source convenience', () =>
  Effect.gen(function* () {
    const evaluated = yield* Analysis.ofSource(
      'memory/facade',
      ascii('pub fn main() -> I32 { return 42 }'),
    )
    const outcome = Analysis.evaluate(evaluated)
    assert.strictEqual(outcome._tag, 'Completed')

    const owned = yield* Analysis.ofSource(
      'memory/ownership',
      ascii('pub fn identity(value: I32) -> I32 { return value }'),
    )
    const facts = Analysis.ownershipOf(owned, 'memory/ownership')
    assert.strictEqual(facts?.functions.at(0)?.verdict._tag, 'Satisfied')
    assert.strictEqual(facts?.functions.at(0)?.bindings.at(0)?.category._tag, 'Copyable')
  }),
)

it.effect('emits clean snapshots and refuses diagnosed snapshots before the backend', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'memory/codegen',
      ascii('pub fn main() -> I32 { return 42 }'),
      'aarch64-apple-darwin',
    )
    const release = yield* Analysis.codegen(self, { mode: 'release' })
    assert.strictEqual(release._tag, 'BackendArtifact')
    assert.include(release.ir, 'silk_main')

    const invalid = yield* Analysis.ofSource(
      'memory/invalid',
      ascii('pub fn main() -> Mystery { return 42 }'),
      'aarch64-apple-darwin',
    )
    const blocked = yield* Effect.result(Analysis.codegen(invalid, { mode: 'release' }))
    assert.strictEqual(blocked._tag, 'Failure')
    if (blocked._tag === 'Failure') assert.strictEqual(blocked.failure._tag, 'CodegenUnavailable')
  }),
)

it.effect('keeps target and layout availability explicit', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'memory/plan',
      ascii('pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'),
      'wasm32-unknown-unknown',
    )
    const target = Analysis.targetOf(self)
    const layout = Analysis.layoutOf(self)
    const mir = Analysis.mirOf(self)
    assert.strictEqual(target._tag, 'Resolved')
    assert.strictEqual(layout._tag, 'Available')
    assert.strictEqual(mir._tag, 'Available')

    const unsupported = yield* Analysis.ofSource(
      'memory/unsupported',
      ascii('pub fn main() -> I32 { return 42 }'),
      'mips-unknown-none',
    )
    assert.strictEqual(Analysis.targetOf(unsupported)._tag, 'Unavailable')
    assert.strictEqual(Analysis.layoutOf(unsupported)._tag, 'Unavailable')
    assert.strictEqual(Analysis.mirOf(unsupported)._tag, 'Unavailable')
  }),
)
