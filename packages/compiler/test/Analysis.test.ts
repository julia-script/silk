import { assert, it } from '@effect/vitest'
import * as Analysis from '../src/Analysis.js'

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
  assert.strictEqual(Analysis.hirOf(self, 'lib')?.functions.at(0)?.body._tag, 'IntegerLiteral')
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
  const self = Analysis.ofSource(
    'memory://facade.silk',
    ascii('pub fn main() -> I32 { return 42 }'),
  )
  const outcome = Analysis.evaluate(self)

  assert.strictEqual(outcome._tag, 'Completed')
  if (outcome._tag !== 'Completed') return
  assert.deepEqual(outcome.result, { _tag: 'I32Value', value: 42 })
})

it('answers ownership facts and cleanup plans through the facade', () => {
  const self = Analysis.ofSource(
    'memory://ownership.silk',
    ascii('pub fn identity(value: I32) -> I32 { return value }'),
  )
  const facts = Analysis.ownershipOf(self, 'memory://ownership.silk')

  assert.strictEqual(facts?.functions.at(0)?.verdict._tag, 'Satisfied')
  assert.strictEqual(facts?.functions.at(0)?.bindings.at(0)?.category._tag, 'Copyable')
  assert.deepEqual(facts?.functions.at(0)?.exits.at(0)?.releases, [])
  assert.strictEqual(Analysis.ownershipOf(self, 'absent'), undefined)
})
