import { assert, it } from '@effect/vitest'
import * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as ModuleClosure from '../src/ModuleClosure.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const collect = (
  rootModule: string,
  entries: ReadonlyArray<readonly [string, string]>,
): DeclarationIndex.Index =>
  DeclarationIndex.collect(
    ModuleClosure.load({
      rootModule,
      sources: new Map(entries.map(([name, text]) => [name, ascii(text)])),
    }),
  )

it('assigns distinct canonical identities to same-named declarations across modules', () => {
  const index = collect('root', [
    ['root', 'import lib\npub fn answer() -> I32 { return 1 }'],
    ['lib', 'pub fn answer() -> I32 { return 2 }'],
  ])

  const canonicals = index.modules.flatMap((module) =>
    module.declarations.map((declaration) => declaration.canonical),
  )
  assert.deepEqual(canonicals, [
    { _tag: 'Canonical', id: { _tag: 'CanonicalDeclarationId', module: 'lib', name: 'answer' } },
    { _tag: 'Canonical', id: { _tag: 'CanonicalDeclarationId', module: 'root', name: 'answer' } },
  ])
  assert.deepEqual(index.diagnostics, [])
})

it('marks later duplicates as caused duplicates of the first occurrence', () => {
  const index = collect('root', [
    ['root', 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }'],
  ])
  const headers = index.modules.at(0)?.declarations ?? []

  assert.strictEqual(headers.at(0)?.canonical._tag, 'Canonical')
  const duplicate = headers.at(1)?.canonical
  assert.strictEqual(duplicate?._tag, 'Duplicate')
  if (duplicate?._tag !== 'Duplicate') return
  assert.deepEqual(duplicate.original, {
    _tag: 'CanonicalDeclarationId',
    module: 'root',
    name: 'same',
  })
  assert.strictEqual(duplicate.cause.code, 'SEM0003')
  assert.deepEqual(
    index.diagnostics.map((diagnostic) => diagnostic.code),
    ['SEM0003'],
  )
})

it('keeps unavailable names unidentified without extra diagnostics', () => {
  const index = collect('root', [['root', 'pub fn () -> I32 { return 0 }']])
  const header = index.modules.at(0)?.declarations.at(0)

  assert.strictEqual(header?.canonical._tag, 'Unidentified')
  assert.strictEqual(header?.name._tag, 'Unavailable')
  assert.deepEqual(index.diagnostics, [])
})

it('resolves header signatures and diagnoses unknown types at exact spans', () => {
  const index = collect('root', [
    ['root', 'pub fn choose(left: I32, right: Mystery) -> I32 { return left }'],
  ])
  const header = index.modules.at(0)?.declarations.at(0)

  assert.strictEqual(header?.parameterCount, 2)
  assert.strictEqual(header?.parameters.at(0)?.declaredType._tag, 'Resolved')
  assert.strictEqual(header?.parameters.at(1)?.declaredType._tag, 'Unresolved')
  assert.strictEqual(header?.returnType._tag, 'Resolved')
  assert.deepEqual(
    index.diagnostics.map((diagnostic) => ({
      code: diagnostic.code,
      start: diagnostic.span.start,
      end: diagnostic.span.end,
    })),
    [{ code: 'SEM0001', start: 32, end: 39 }],
  )
})

it('orders modules canonically and answers per-module lookups', () => {
  const index = collect('zeta', [
    ['zeta', 'import alpha\npub fn main() -> I32 { return 42 }'],
    ['alpha', 'pub fn helper() -> I32 { return 1 }'],
  ])

  assert.deepEqual(
    index.modules.map((module) => module.module),
    ['alpha', 'zeta'],
  )
  assert.strictEqual(DeclarationIndex.lookup(index, 'zeta', 'main')._tag, 'Resolved')
  assert.strictEqual(DeclarationIndex.lookup(index, 'zeta', 'helper')._tag, 'Missing')
  assert.strictEqual(DeclarationIndex.lookup(index, 'alpha', 'helper')._tag, 'Resolved')
})

it('collects identical indexes across repeated fresh runs', () => {
  const entries: ReadonlyArray<readonly [string, string]> = [
    ['root', 'import lib\npub fn main() -> Mystery { return lib }'],
    ['lib', 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }'],
  ]
  const first = collect('root', entries)
  const second = collect('root', [...entries].reverse())

  assert.deepEqual(first, second)
})
