import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { pathToFileURL } from 'node:url'
import { NodeServices } from '@effect/platform-node'
import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-lang/compiler/Analysis'
import * as SourceFile from '@silk-lang/compiler/SourceFile'
import * as SourceOrigin from '@silk-lang/compiler/SourceOrigin'
import * as SourceResolver from '@silk-lang/compiler/SourceResolver'
import * as Stdlib from '@silk-lang/compiler/Stdlib'
import * as WorkspaceInventory from '@silk-lang/compiler/WorkspaceInventory'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Document from '../src/Document.js'
import * as Workspace from '../src/Workspace.js'

const encoder = new TextEncoder()
const decoder = new TextDecoder()

const project = (): string => {
  const root = mkdtempSync(join(tmpdir(), 'silk-lsp-'))
  mkdirSync(join(root, 'src'))
  writeFileSync(
    join(root, 'silk.toml'),
    '[package]\nname = "demo"\nversion = "0.1.0"\nroot = "src/Main.silk"\n',
  )
  writeFileSync(join(root, 'src', 'Main.silk'), 'pub fn main() -> i32 { return 42 }\n')
  writeFileSync(join(root, 'src', 'Util.silk'), 'pub fn answer() -> i32 { return 7 }\n')
  return root
}

it.effect('derives module identities from the discovered project source root', () =>
  Effect.gen(function* () {
    const root = project()
    const document = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Util.silk')).href,
      version: 1,
      bytes: encoder.encode('pub fn answer() -> i32 { return 7 }'),
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
      bytes: encoder.encode('pub fn main() -> i32 { return 1 }'),
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
      bytes: encoder.encode('pub fn main() -> i32 { return 1 }'),
    })
    const rightDocument = yield* Workspace.open({
      uri: pathToFileURL(join(right, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode('pub fn main() -> i32 { return 2 }'),
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
      bytes: encoder.encode('pub fn main() -> i32 { return 1 }'),
    })
    const repeated = yield* Workspace.open({
      uri: 'untitled:First.silk',
      version: 2,
      bytes: encoder.encode('pub fn main() -> i32 { return 2 }'),
    })
    const second = yield* Workspace.open({
      uri: 'untitled:Second.silk',
      version: 1,
      bytes: encoder.encode('pub fn main() -> i32 { return 3 }'),
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
    const overlays = new Map([
      [
        'Util',
        SourceResolver.resolved(
          encoder.encode('pub fn answer() -> i32 { return 8 }'),
          SourceOrigin.memory('file:///overlay/Util.silk'),
        ),
      ],
    ])
    const overlaid = yield* SourceResolver.resolve('Util').pipe(
      Effect.provide(Workspace.resolver(join(root, 'src'), overlays)),
    )
    assert.isTrue(Option.isSome(overlaid))
    if (Option.isSome(overlaid)) {
      assert.include(decoder.decode(overlaid.value.bytes), 'return 8')
      assert.deepEqual(overlaid.value.origin, {
        _tag: 'Memory',
        uri: 'file:///overlay/Util.silk',
      })
    }

    const fromDisk = yield* SourceResolver.resolve('Main').pipe(
      Effect.provide(Workspace.resolver(join(root, 'src'), overlays)),
    )
    assert.isTrue(Option.isSome(fromDisk))
    if (Option.isSome(fromDisk)) assert.include(decoder.decode(fromDisk.value.bytes), 'return 42')
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('analyzes imports against sibling files on disk', () =>
  Effect.gen(function* () {
    const root = project()
    const source = 'import Util\npub fn main() -> i32 { return Util.answer() }'
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

it.effect('keeps editor analysis on frontend phases only', () =>
  Effect.gen(function* () {
    const root = project()
    const document = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode('pub fn main() -> i32 { return 42 }'),
    })
    const snapshot = yield* Workspace.analyze(document, [])

    assert.deepEqual(
      Analysis.phases(snapshot).map(({ phase }) => phase),
      [
        'closure',
        'declaration-collection',
        'declaration-index',
        'name-resolution',
        'module-surface',
        'elaboration',
        'ownership',
        'opaque-realization',
        'semantic-occurrences',
        'anonymous-expressions',
      ],
    )
    assert.isFalse(Object.hasOwn(snapshot, 'instances'))
    assert.isFalse(Object.hasOwn(snapshot, 'layout'))
    assert.isFalse(Object.hasOwn(snapshot, 'mir'))
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('shares one project frontend across overlapping open roots', () =>
  Effect.gen(function* () {
    const root = project()
    writeFileSync(join(root, 'src', 'Shared.silk'), 'pub fn answer() -> i32 { return 40 }\n')
    const main = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode(
        'import Shared\nfn local() -> i32 { return 1 }\npub fn main() -> i32 { return Shared.answer() + local() }',
      ),
    })
    const util = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Util.silk')).href,
      version: 3,
      bytes: encoder.encode(
        'import Shared\nfn local() -> i32 { return 2 }\npub fn utility() -> i32 { return Shared.answer() + local() }',
      ),
    })
    const analyzed = yield* Workspace.analyzeProject([util, main])
    const mainSession = analyzed.get(main.uri)
    const utilSession = analyzed.get(util.uri)

    assert.isDefined(mainSession)
    assert.isDefined(utilSession)
    if (mainSession === undefined || utilSession === undefined) return
    assert.strictEqual(mainSession.project, utilSession.project)
    assert.strictEqual(mainSession.snapshot.index, utilSession.snapshot.index)
    assert.strictEqual(mainSession.snapshot.surfaces, utilSession.snapshot.surfaces)
    assert.strictEqual(mainSession.snapshot.results, utilSession.snapshot.results)
    assert.strictEqual(
      mainSession.snapshot.semanticOccurrences,
      utilSession.snapshot.semanticOccurrences,
    )
    assert.strictEqual(Analysis.phases(mainSession.snapshot), Analysis.phases(utilSession.snapshot))
    assert.strictEqual(
      mainSession.snapshot.semanticInvalidation,
      utilSession.snapshot.semanticInvalidation,
    )
    assert.strictEqual(mainSession.moduleUris, utilSession.moduleUris)
    assert.strictEqual(mainSession.snapshot.closure.rootModule, 'Main')
    assert.strictEqual(utilSession.snapshot.closure.rootModule, 'Util')
    assert.deepEqual(
      Analysis.modules(mainSession.snapshot).map(({ name }) => name),
      ['Main', 'Shared', 'Util'],
    )
    const closure = Analysis.phases(mainSession.snapshot).at(0)
    assert.deepEqual(
      closure === undefined
        ? undefined
        : { phase: closure.phase, inputs: closure.inputs, outputs: closure.outputs },
      { phase: 'closure', inputs: 2, outputs: 3 },
    )
    assert.deepEqual(
      Document.diagnostics(main, mainSession.snapshot, () => undefined),
      [],
    )
    assert.deepEqual(
      Document.diagnostics(util, utilSession.snapshot, () => undefined),
      [],
    )

    const nextMain = Document.make({
      ...main,
      version: 2,
      bytes: encoder.encode(
        'import Shared\nfn inserted() -> i32 { return 0 }\nfn local() -> i32 { return 1 }\npub fn main() -> i32 { return Shared.answer() + local() }',
      ),
    })
    const revised = yield* Workspace.analyzeProject([nextMain, util], analyzed)
    const revisedMain = revised.get(nextMain.uri)
    const revisedUtil = revised.get(util.uri)
    assert.isDefined(revisedMain)
    assert.isDefined(revisedUtil)
    if (revisedMain === undefined || revisedUtil === undefined) return
    assert.strictEqual(revisedMain.project, revisedUtil.project)
    assert.notStrictEqual(revisedMain.project, mainSession.project)
    assert.strictEqual(revisedMain.project.syntaxRevisions.get('Main')?._tag, 'Changed')
    assert.strictEqual(revisedMain.project.syntaxRevisions.get('Util')?._tag, 'Reused')
    assert.strictEqual(revisedMain.project.syntaxRevisions.get('Shared')?._tag, 'Reused')
    assert.deepEqual(revisedMain.project.semanticInvalidation.observations, [
      {
        _tag: 'Recomputed',
        module: 'Main',
        reasons: ['LocalChange'],
        surfaceChanged: true,
      },
      { _tag: 'Reusable', module: 'Shared', surfaceChanged: false },
      { _tag: 'Reusable', module: 'Util', surfaceChanged: false },
    ])
    const previousSyntax = new Map(
      mainSession.project.closure.modules.map((module) => [module.name, module.syntax]),
    )
    const currentSyntax = new Map(
      revisedMain.project.closure.modules.map((module) => [module.name, module.syntax]),
    )
    assert.notStrictEqual(currentSyntax.get('Main'), previousSyntax.get('Main'))
    assert.strictEqual(currentSyntax.get('Util'), previousSyntax.get('Util'))
    assert.strictEqual(currentSyntax.get('Shared'), previousSyntax.get('Shared'))
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('indexes closed source-root modules without widening semantic project roots', () =>
  Effect.gen(function* () {
    const root = project()
    writeFileSync(
      join(root, 'src', 'Candidate.silk'),
      'pub fn closedCandidate() -> i32 { return 7 }',
    )
    const document = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode('pub fn main() -> i32 { return 42 }'),
    })
    const sessions = yield* Workspace.analyzeProject([document])
    const session = sessions.get(document.uri)
    assert.isDefined(session)
    if (session === undefined) return

    assert.deepEqual(
      Analysis.modules(session.snapshot).map((module) => module.name),
      ['Main'],
    )
    assert.deepEqual(
      WorkspaceInventory.candidates(session.inventory, 'closedCandidate').map(
        (candidate) => candidate.module,
      ),
      ['Candidate'],
    )
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('navigates standard-library definitions to the analyzed toolchain source', () =>
  Effect.gen(function* () {
    const root = project()
    mkdirSync(join(root, 'src', 'silk'))
    writeFileSync(join(root, 'src', 'silk', 'vector.silk'), 'pub struct Hostile {}')
    const source =
      'import silk.vector { Vector }\npub fn size(value: &Vector<i32>) -> usize { return silk.vector.length(value) }'
    const document = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode(source),
    })
    const snapshot = yield* Workspace.analyze(document, [])
    const librarySource = Analysis.sources(snapshot).get('silk/vector')
    assert.isDefined(librarySource)
    if (librarySource === undefined) return
    assert.strictEqual(librarySource.origin._tag, 'ToolchainFile')
    assert.include(decoder.decode(SourceFile.toUint8Array(librarySource)), 'struct Vector<T>')
    const libraryUri = yield* Workspace.uriOf(librarySource, [document])
    assert.strictEqual(libraryUri, Stdlib.find('silk/vector')?.sourceUrl.href)

    const definition = Document.definition(
      document,
      snapshot,
      { line: 0, character: source.indexOf('Vector') },
      (module) => (module === 'silk/vector' ? libraryUri : undefined),
    )
    assert.strictEqual(definition?.targetUri, libraryUri)
    const libraryLines = decoder.decode(SourceFile.toUint8Array(librarySource)).split('\n')
    const declarationLine = libraryLines.findIndex((line) => line.includes('pub struct Vector<T>'))
    assert.isAtLeast(declarationLine, 0)
    const declaration = libraryLines[declarationLine] ?? ''
    assert.deepEqual(definition?.targetSelectionRange.start, {
      line: declarationLine,
      character: declaration.indexOf('Vector'),
    })
  }).pipe(Effect.provide(NodeServices.layer)),
)

it.effect('navigates Effect.suspend to its shipped Silk declaration', () =>
  Effect.gen(function* () {
    const root = project()
    const source = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect as Effect

pub effect fn delayed() -> i32 ! OutOfMemoryError ? &mut Allocator {
  return run Effect.suspend(effect { return 42 })
}
pub fn main() -> i32 { return 42 }`
    const document = yield* Workspace.open({
      uri: pathToFileURL(join(root, 'src', 'Main.silk')).href,
      version: 1,
      bytes: encoder.encode(source),
    })
    const snapshot = yield* Workspace.analyze(document, [])
    assert.deepEqual(Analysis.diagnostics(snapshot), [])

    const librarySource = Analysis.sources(snapshot).get('silk/effect')
    assert.isDefined(librarySource)
    if (librarySource === undefined) return
    assert.strictEqual(librarySource.origin._tag, 'ToolchainFile')
    const libraryUri = yield* Workspace.uriOf(librarySource, [document])
    assert.strictEqual(libraryUri, Stdlib.find('silk/effect')?.sourceUrl.href)

    const definition = Document.definition(
      document,
      snapshot,
      { line: 4, character: source.split('\n')[4]?.indexOf('suspend') ?? 0 },
      (module) => (module === 'silk/effect' ? libraryUri : undefined),
    )
    assert.strictEqual(definition?.targetUri, libraryUri)
    const libraryLines = decoder.decode(SourceFile.toUint8Array(librarySource)).split('\n')
    const target = definition?.targetSelectionRange.start
    assert.isDefined(target)
    assert.include(target === undefined ? '' : (libraryLines[target.line] ?? ''), 'fn suspend')
  }).pipe(Effect.provide(NodeServices.layer)),
)
