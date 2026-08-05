import { assert, it } from '@effect/vitest'
import * as ModuleClosure from '../src/ModuleClosure.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const fn = 'pub fn main() -> I32 { return 42 }'

const request = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): ModuleClosure.CompilationRequest => ({
  rootModule,
  sources: new Map(entries.map(([name, text]) => [name, ascii(text)])),
})

const importNames = (module: ModuleClosure.Module): ReadonlyArray<string> =>
  module.imports.map((fact) =>
    fact.target._tag === 'Unavailable' ? '<unavailable>' : fact.target.module,
  )

it('loads a diamond closure once per module in canonical order', () => {
  const closure = ModuleClosure.load(
    request('root', [
      ['root', `import left\nimport right\n${fn}`],
      ['left', `import shared\n${fn}`],
      ['right', `import shared\n${fn}`],
      ['shared', fn],
    ]),
  )

  assert.deepEqual(
    closure.modules.map((module) => module.name),
    ['left', 'right', 'root', 'shared'],
  )
  assert.deepEqual(closure.cycles, [])
  assert.deepEqual(closure.diagnostics, [])
  assert.strictEqual(Object.isFrozen(closure), true)
})

it('excludes unreachable modules from the closure', () => {
  const closure = ModuleClosure.load(
    request('root', [
      ['root', fn],
      ['island', `import root\n${fn}`],
    ]),
  )

  assert.deepEqual(
    closure.modules.map((module) => module.name),
    ['root'],
  )
  assert.deepEqual(closure.diagnostics, [])
})

it('rejects a request whose root module is missing', () => {
  assert.throws(() => ModuleClosure.load(request('absent', [['root', fn]])), RangeError)
})

it('is independent of source supply order', () => {
  const entries: ReadonlyArray<readonly [string, string]> = [
    ['root', `import zeta\nimport alpha\n${fn}`],
    ['zeta', `import alpha\n${fn}`],
    ['alpha', fn],
  ]
  const forward = ModuleClosure.load(request('root', entries))
  const reversed = ModuleClosure.load(request('root', [...entries].reverse()))

  assert.deepEqual(forward, reversed)
})

it('diagnoses unknown targets and self-imports with caused import facts', () => {
  const closure = ModuleClosure.load(
    request('root', [['root', `import missing\nimport root\n${fn}`]]),
  )
  const root = closure.modules.at(0)

  assert.notStrictEqual(root, undefined)
  if (root === undefined) return
  assert.deepEqual(importNames(root), ['missing', 'root'])
  assert.deepEqual(
    closure.diagnostics.map((diagnostic) => ({
      phase: diagnostic.phase,
      code: diagnostic.code,
    })),
    [
      { phase: 'module', code: 'MOD0001' },
      { phase: 'module', code: 'MOD0002' },
    ],
  )
  const targets = root?.imports.map((fact) => fact.target) ?? []
  assert.strictEqual(targets.at(0)?._tag, 'Unknown')
  assert.strictEqual(targets.at(1)?._tag, 'Self')
  const unknown = targets.at(0)
  if (unknown?._tag === 'Unknown') {
    assert.strictEqual(unknown.cause.code, 'MOD0001')
  }
})

it('suppresses module diagnostics for imports recovered without a name', () => {
  const closure = ModuleClosure.load(request('root', [['root', `import\n${fn}`]]))
  const root = closure.modules.at(0)

  assert.strictEqual(root?.imports.at(0)?.target._tag, 'Unavailable')
  assert.deepEqual(closure.diagnostics, [])
  assert.deepEqual(
    root?.syntax.parserDiagnostics.map((diagnostic) => diagnostic.code),
    ['PAR0001'],
  )
})

it('records mutual import cycles as canonical facts without error diagnostics', () => {
  const closure = ModuleClosure.load(
    request('root', [
      ['root', `import beta\n${fn}`],
      ['beta', `import gamma\n${fn}`],
      ['gamma', `import beta\n${fn}`],
    ]),
  )

  assert.deepEqual(closure.cycles, [['beta', 'gamma']])
  assert.deepEqual(closure.diagnostics, [])
  assert.deepEqual(
    closure.modules.map((module) => module.name),
    ['beta', 'gamma', 'root'],
  )
})
