import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { pathToFileURL } from 'node:url'
import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-effect/compiler/Analysis'
import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Document from '../src/Document.js'
import * as Workspace from '../src/Workspace.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const project = (): string => {
  const root = mkdtempSync(join(tmpdir(), 'silk-lsp-'))
  mkdirSync(join(root, 'src'))
  writeFileSync(join(root, 'silk.toml'), '[package]\nname = "demo"\nroot = "src/Main.silk"\n')
  writeFileSync(join(root, 'src', 'Main.silk'), 'pub fn main() -> I32 { return 42 }\n')
  writeFileSync(join(root, 'src', 'Util.silk'), 'pub fn answer() -> I32 { return 7 }\n')
  return root
}

it.effect('derives module identities from the discovered project source root', () =>
  Effect.gen(function* () {
    const root = project()
    const document = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Util.silk')).href,
      version: 1,
      bytes: encoder.encode('pub fn answer() -> I32 { return 7 }'),
    })
    assert.strictEqual(document.module, 'Util')
    assert.strictEqual(document.sourceRoot, join(root, 'src'))
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('falls back to the document directory without a manifest', () =>
  Effect.gen(function* () {
    const document = yield* Workspace.open({
      uri: 'file:///definitely/missing/standalone.silk',
      version: 1,
      bytes: encoder.encode('pub fn main() -> I32 { return 1 }'),
    })
    assert.strictEqual(document.module, 'standalone')
    assert.strictEqual(document.sourceRoot, '/definitely/missing')
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('resolves open-document overlays before rooted files', () =>
  Effect.gen(function* () {
    const root = project()
    const overlays = new Map([['Util', encoder.encode('pub fn answer() -> I32 { return 8 }')]])
    const overlaid = yield* SourceResolver.resolve('Util').pipe(
      Effect.provide(Workspace.resolver(join(root, 'src'), overlays)),
    )
    assert.isTrue(Option.isSome(overlaid))
    if (Option.isSome(overlaid)) assert.include(decoder.decode(overlaid.value), 'return 8')

    const fromDisk = yield* SourceResolver.resolve('Main').pipe(
      Effect.provide(Workspace.resolver(join(root, 'src'), overlays)),
    )
    assert.isTrue(Option.isSome(fromDisk))
    if (Option.isSome(fromDisk)) assert.include(decoder.decode(fromDisk.value), 'return 42')
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('analyzes imports against sibling files on disk', () =>
  Effect.gen(function* () {
    const root = project()
    const source = 'import Util\npub fn main() -> I32 { return Util.answer() }'
    const document = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode(source),
    })
    const snapshot = yield* Workspace.analyze(document, [])
    assert.deepEqual(
      Document.diagnostics(document, snapshot, () => undefined),
      [],
    )
    assert.deepEqual(
      Analysis.modules(snapshot).map((module) => module.name),
      ['Main', 'Util'],
    )
  }).pipe(Effect.provide(NodeServices.layer)),
)
