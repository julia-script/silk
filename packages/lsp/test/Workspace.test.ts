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

it.effect('keeps same-named modules isolated by discovered project identity', () =>
  Effect.gen(function* () {
    const left = project()
    const right = project()
    const leftDocument = yield* Workspace.open({
      uri: pathToFileURL(join(left, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode('pub fn main() -> I32 { return 1 }'),
    })
    const rightDocument = yield* Workspace.open({
      uri: pathToFileURL(join(right, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode('pub fn main() -> I32 { return 2 }'),
    })
    assert.strictEqual(leftDocument.module, rightDocument.module)
    assert.notStrictEqual(leftDocument.workspace, rightDocument.workspace)
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('assigns stable non-colliding identities to virtual documents', () =>
  Effect.gen(function* () {
    const first = yield* Workspace.open({
      uri: 'untitled:First.silk',
      version: 1,
      bytes: encoder.encode('pub fn main() -> I32 { return 1 }'),
    })
    const repeated = yield* Workspace.open({
      uri: 'untitled:First.silk',
      version: 2,
      bytes: encoder.encode('pub fn main() -> I32 { return 2 }'),
    })
    const second = yield* Workspace.open({
      uri: 'untitled:Second.silk',
      version: 1,
      bytes: encoder.encode('pub fn main() -> I32 { return 3 }'),
    })
    assert.strictEqual(first.workspace, repeated.workspace)
    assert.strictEqual(first.module, repeated.module)
    assert.notStrictEqual(first.workspace, second.workspace)
    assert.notStrictEqual(first.module, second.module)
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
