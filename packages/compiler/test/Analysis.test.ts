import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Hir from '../src/Hir.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): Analysis.Snapshot =>
  Analysis.make({
    rootModule,
    sources: new Map(entries.map(([name, text]) => [name, ascii(text)])),
  })

it('answers multi-module queries from one snapshot', () => {
  const self = snapshot('root', [
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
})

it('merges diagnostics across modules and phases in driver order', () => {
  const self = snapshot('root', [
    ['root', 'import missing\npub fn main() -> Mystery { return 42 }'],
    ['lib', 'pub fn unused() -> I32 { return 1 }'],
  ])

  assert.deepEqual(
    Analysis.diagnostics(self).map((diagnostic) => ({
      phase: diagnostic.phase,
      code: diagnostic.code,
    })),
    [
      { phase: 'module', code: 'MOD0001' },
      { phase: 'semantic', code: 'SEM0001' },
    ],
  )
})

it('keeps unrelated declarations queryable around damage', () => {
  const self = snapshot('root', [
    ['root', 'import lib\npub fn main( -> Mystery { return @ 42 }'],
    ['lib', 'pub fn answer() -> I32 { return 1 }'],
  ])

  assert.strictEqual(Analysis.declarationByName(self, 'lib', 'answer')._tag, 'Resolved')
  const libFunction = Analysis.hirOf(self, 'lib')?.functions.at(0)
  assert.strictEqual(
    libFunction === undefined ? undefined : Hir.returned(libFunction)._tag,
    'IntegerLiteral',
  )
  const rootMain = Analysis.rootAnalysis(self).functions.at(0)
  assert.strictEqual(rootMain?.declaration.returnType._tag, 'Unresolved')
})

it('answers every query identically across repeated snapshots', () => {
  const entries: ReadonlyArray<readonly [string, string]> = [
    ['root', 'import lib\npub fn main() -> I32 { return 42 }'],
    ['lib', 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }'],
  ]
  const first = snapshot('root', entries)
  const second = snapshot('root', [...entries].reverse())

  assert.deepEqual(first.closure, second.closure)
  assert.deepEqual(first.index, second.index)
  assert.deepEqual(Analysis.diagnostics(first), Analysis.diagnostics(second))
  assert.deepEqual(Analysis.rootAnalysis(first), Analysis.rootAnalysis(second))
})

it('evaluates the root module through the facade', () => {
  const self = Analysis.ofSource('memory/facade', ascii('pub fn main() -> I32 { return 42 }'))
  const outcome = Analysis.evaluate(self)

  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.deepEqual(outcome.result, { _tag: 'I32Value', value: 42 })
})

it('answers ownership facts and cleanup plans through the facade', () => {
  const self = Analysis.ofSource(
    'memory/ownership',
    ascii('pub fn identity(value: I32) -> I32 { return value }'),
  )
  const facts = Analysis.ownershipOf(self, 'memory/ownership')

  assert.strictEqual(facts?.functions.at(0)?.verdict._tag, 'Satisfied')
  assert.strictEqual(facts?.functions.at(0)?.bindings.at(0)?.category._tag, 'Copyable')
  assert.deepEqual(facts?.functions.at(0)?.exits.at(0)?.releases, [])
  assert.strictEqual(Analysis.ownershipOf(self, 'absent'), undefined)
})

it('answers nominal declaration, field, dependency, and layout facts through the facade', () => {
  const self = Analysis.ofSource(
    'memory/nominal-facade',
    ascii('struct Pair { left: I32 right: Bool }\npub fn main() -> I32 { return 42 }'),
    'aarch64-apple-darwin',
  )
  const lookup = Analysis.structByName(self, 'memory/nominal-facade', 'Pair')
  assert.strictEqual(Analysis.memberByName(self, 'memory/nominal-facade', 'Pair')._tag, 'Resolved')
  assert.strictEqual(lookup._tag, 'Resolved')
  if (lookup._tag !== 'Resolved') return
  assert.strictEqual(Analysis.fieldByName(lookup.declaration, 'right')._tag, 'Resolved')
  assert.strictEqual(lookup.declaration.dependency._tag, 'Available')
  const layout = Analysis.nominalLayout(self, Type.nominal('memory/nominal-facade', 'Pair'))
  assert.strictEqual(layout?._tag, 'LayoutEntry')
})

it('keeps cross-module nominal identity and recursive unavailability queryable through the facade', () => {
  const self = snapshot('app/Main', [
    [
      'app/Main',
      'import model.Tree { Node }\nstruct Root { node: Node }\nstruct Loop { next: Loop }\npub fn main() -> I32 { return 42 }',
    ],
    ['model/Tree', 'pub struct Node { value: I32 }'],
  ])
  const root = Analysis.structByName(self, 'app/Main', 'Root')
  const loop = Analysis.structByName(self, 'app/Main', 'Loop')
  assert.strictEqual(root._tag, 'Resolved')
  assert.strictEqual(loop._tag, 'Resolved')
  if (root._tag !== 'Resolved' || loop._tag !== 'Resolved') return
  const node = Analysis.fieldByName(root.declaration, 'node')
  assert.strictEqual(node._tag, 'Resolved')
  if (node._tag !== 'Resolved' || node.field.declaredType._tag !== 'Resolved') return
  assert.deepEqual(node.field.declaredType.type, {
    _tag: 'NominalType',
    module: 'model/Tree',
    name: 'Node',
  })
  assert.strictEqual(
    Analysis.nominalLayout(self, Type.nominal('model/Tree', 'Node'))?._tag,
    'LayoutEntry',
  )
  const unavailable = Analysis.nominalLayout(self, Type.nominal('app/Main', 'Loop'))
  assert.strictEqual(unavailable?._tag, 'UnavailableLayoutEntry')
  assert.strictEqual(
    unavailable?._tag === 'UnavailableLayoutEntry' ? unavailable.cause?.code : undefined,
    'SEM0020',
  )
})

it.effect('emits codegen artifacts through the facade', () =>
  Effect.gen(function* () {
    const self = Analysis.ofSource(
      'memory/codegen',
      ascii('pub fn main() -> I32 { return 42 }'),
      'aarch64-apple-darwin',
    )
    const release = yield* Analysis.codegen(self, { mode: 'release' })
    const debug = yield* Analysis.codegen(self, { mode: 'debug' })

    assert.strictEqual(release._tag, 'BackendArtifact')
    assert.include(release.ir, 'silk_main')
    assert.isAbove(release.bitcode.length, 0)
    assert.include(debug.ir, '!DICompileUnit(')
  }),
)

it('preserves one exact target and layout plan across facade queries and MIR', () => {
  const self = Analysis.ofSource(
    'memory/plan',
    ascii('pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'),
    'wasm32-unknown-unknown',
  )
  const target = Analysis.targetOf(self)
  const catalog = Analysis.layoutCatalogOf(self)
  const layout = Analysis.layoutOf(self)
  const mir = Analysis.mirOf(self)

  assert.strictEqual(target._tag, 'Resolved')
  assert.strictEqual(catalog._tag, 'Available')
  assert.strictEqual(layout._tag, 'Available')
  assert.strictEqual(mir._tag, 'Available')
  if (
    target._tag !== 'Resolved' ||
    catalog._tag !== 'Available' ||
    layout._tag !== 'Available' ||
    mir._tag !== 'Available'
  )
    return
  assert.strictEqual(catalog.value.target, target.target)
  assert.strictEqual(layout.value.target, target.target)
  assert.strictEqual(mir.value.layout, layout.value)
  assert.deepEqual(
    layout.value.entries.map((entry) => [entry.type, entry.size, entry.alignment]),
    [
      ['Bool', 4, 4],
      ['I32', 4, 4],
    ],
  )
})

it('keeps unsupported targets explicit and queryable without manufacturing MIR', () => {
  const self = Analysis.ofSource(
    'memory/unsupported',
    ascii('pub fn main() -> I32 { return 42 }'),
    'mips-unknown-none',
  )

  assert.strictEqual(Analysis.targetOf(self)._tag, 'Unavailable')
  assert.strictEqual(Analysis.layoutCatalogOf(self)._tag, 'Unavailable')
  assert.strictEqual(Analysis.layoutOf(self)._tag, 'Unavailable')
  assert.strictEqual(Analysis.mirOf(self)._tag, 'Unavailable')
})
